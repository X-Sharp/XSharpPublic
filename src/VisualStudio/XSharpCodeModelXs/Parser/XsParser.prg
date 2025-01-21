﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Note that the comment blocks from the various rules have been copied from XSharp.g4 inside the compiler


USING System.Collections.Generic
USING System.Collections
USING System.Text
USING System.Text.RegularExpressions
USING System.IO
USING System.Diagnostics
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING XSharp.Parser
USING LanguageService.CodeAnalysis.Text
USING XSharp.Settings

BEGIN NAMESPACE XSharpModel


CLASS XsParser IMPLEMENTS VsParser.IErrorListener
    PRIVATE  _list         AS XSharpTokenList
    PRIVATE  _file         AS XFile
    PRIVATE  _usings       AS IList<STRING>
    PRIVATE  _staticusings AS IList<STRING>
    PRIVATE  _EntityList    AS IList<XSourceEntity>
    PRIVATE  _EntityStack   AS Stack<XSourceEntity>
    PRIVATE  _BlockList     AS IList<XSourceBlock>
    PRIVATE  _BlockStack    AS Stack<XSourceBlock>
    PRIVATE  _PPBlockStack  AS Stack<XSourceBlock>
    PRIVATE  _locals        AS IList<XSourceVariableSymbol>
    PRIVATE  _collectLocals AS LOGIC
    PRIVATE  _collectBlocks AS LOGIC
    PRIVATE  _errors        AS IList<XError>
    PRIVATE  _globalType    AS XSourceTypeSymbol
    PRIVATE  _dialect       AS XDialect
    PRIVATE  _xppVisibility AS Modifiers
    PRIVATE  _commentTasks  AS IList<XCommentTask>
    PRIVATE  _modifiers     AS IList<IToken>

    PRIVATE  _attributes   AS Modifiers      // for the current entity
    PRIVATE  _start        AS IToken
    PRIVATE  _hasXmlDoc    AS LOGIC
    PRIVATE  _tokens       AS IList<IToken>
    PRIVATE  _firstTokenOnLine as IToken
    PRIVATE  _missingType  AS STRING

    PRIVATE PROPERTY CurrentEntity      AS XSourceEntity GET IIF(_EntityStack:Count > 0, _EntityStack:Peek(), NULL_OBJECT)
    PRIVATE PROPERTY CurrentType        AS XSourceTypeSymbol
    GET
        VAR aStack := _EntityStack:ToArray()
        FOREACH VAR item IN aStack
            IF item IS XSourceTypeSymbol VAR type
                RETURN type
            ENDIF
        NEXT
        RETURN NULL
    END GET
    END PROPERTY
    PRIVATE PROPERTY CurrentBlock       AS XSourceBlock   GET IIF(_BlockStack:Count > 0, _BlockStack:Peek(), NULL_OBJECT)
    PRIVATE PROPERTY CurrentEntityKind  AS Kind     GET IIF(CurrentEntity != null , CurrentEntity:Kind, Kind.Unknown)
    PRIVATE PROPERTY InFoxClass AS LOGIC GET CurrentType != NULL .AND. CurrentType:ClassType == XDialect.FoxPro
    PRIVATE PROPERTY InXppClass AS LOGIC GET CurrentType != NULL .AND. CurrentType:ClassType == XDialect.XPP
    PROPERTY EntityList AS IList<XSourceEntity>  GET _EntityList
    PROPERTY BlockList  AS IList<XSourceBlock>   GET _BlockList
    PROPERTY Locals     AS IList<XSourceVariableSymbol> GET _locals
    PROPERTY SaveToDisk AS LOGIC AUTO
    PROPERTY SupportsMemVars as LOGIC GET _file:Project:ParseOptions:SupportsMemvars

    CONSTRUCTOR(oFile AS XFile, dialect AS XDialect)
        SELF:SaveToDisk := TRUE
        _errors        := List<XError>{}
        _usings        := List<STRING>{}
        _commentTasks  := List<XCommentTask>{}
        _staticusings  := List<STRING>{}
        _EntityList    := List<XSourceEntity>{}
        _EntityStack   := Stack<XSourceEntity>{}
        _BlockList     := List<XSourceBlock>{}
        _BlockStack    := Stack<XSourceBlock>{}
        _PPBlockStack  := Stack<XSourceBlock>{}
        _file          := oFile
        _dialect       := dialect
        _locals        := List<XSourceVariableSymbol>{}
        _file:Clear()
        _globalType    := _file:GlobalType
        if (_globalType != null)
            _globalType:ClearMembers()
            _EntityStack:Push(_globalType)
        endif
        _missingType := XLiterals.ObjectType
        _modifiers     := List<IToken>{}
        IF SELF:_file:Project != NULL .AND. SELF:_file:Project:ParseOptions:Dialect != XDialect.Core
            _missingType := XLiterals.UsualType
        ENDIF

#region IErrorListener
    METHOD ReportError(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
        SELF:_errors:Add(XError{fileName, span, errorCode, message, args})

    METHOD ReportWarning(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
        SELF:_errors:Add(XWarning{fileName, span, errorCode, message, args})
#endregion

    METHOD Parse(lBlocks AS LOGIC, lLocals AS LOGIC) AS VOID
        VAR cSource  := System.IO.File.ReadAllText(_file:SourcePath)
        VAR options  := XSharpParseOptions.Default
        XSharp.Parser.VsParser.Lex(cSource, SELF:_file:SourcePath, options, SELF, OUT VAR stream, OUT VAR includeFiles)
        var bufStream := (BufferedTokenStream) stream
        SELF:Parse(bufStream:GetTokens(), lBlocks, lLocals)
        RETURN

    METHOD AddCommentLine(comment AS STRING, token AS XSharpToken, cmtToken AS XCommentToken) AS VOID
        comment := comment:Trim()
        IF comment:IndexOf(cmtToken:Text, StringComparison.OrdinalIgnoreCase) == 0 .AND. ;
                comment:Length > cmtToken:Text:Length
            VAR nextchar := comment[cmtToken:Text:Length]
            IF ! Char.IsLetterOrDigit(nextchar)
                VAR item := XCommentTask{}{ File:=_file, Line := token:Line, Column := token:Column, Priority := cmtToken:Priority, Comment := comment}
                _commentTasks:Add(item)
            ENDIF
        ENDIF


    METHOD Parse( tokens AS IList<IToken>, lBlocks AS LOGIC, lLocals AS LOGIC) AS VOID
        LOCAL aAttribs        AS IList<IToken>
        LOCAL cXmlDoc   := "" AS STRING
        VAR cmtTokens  := XSolution.CommentTokens
        Log(i"Start")
        if lLocals
            // this makes sure that locals declared in catch blocks or case blocks are also discovered
            lBlocks := true
        endif
        _collectLocals := lLocals
        _collectBlocks := lBlocks
        _tokens        := tokens
        VAR _input     := List<IToken>{}
        FOREACH token AS XSharpToken IN _tokens
            SWITCH token:Channel
            CASE TokenConstants.HiddenChannel
            CASE XSharpLexer.DEFOUTCHANNEL // Inactive code
                IF XSharpLexer.IsComment(token:Type)
                    FOREACH VAR cmtToken IN cmtTokens
                        VAR pos := token:Text:IndexOf(cmtToken:Text, StringComparison.OrdinalIgnoreCase)
                        VAR include := FALSE
                        IF pos >= 0
                            IF token:Type == XSharpLexer.SL_COMMENT
                                VAR comment := token:Text:Substring(2):Trim()
                                SELF:AddCommentLine(comment, token, cmtToken)
                            ELSEIF token:Type == XSharpLexer.ML_COMMENT
                                VAR comment := token:Text:Substring(2, token:Text:Length-4)
                                VAR lines :=comment:Split(<CHAR>{'\r','\n'}, StringSplitOptions.RemoveEmptyEntries)
                                FOREACH VAR line IN lines
                                    SELF:AddCommentLine(line, token, cmtToken)
                                NEXT
                            ENDIF
                        ENDIF
                    NEXT
                ENDIF
            CASE XSharpLexer.DefaultTokenChannel
            CASE XSharpLexer.PREPROCESSORCHANNEL
                _input:Add(token)
            CASE XSharpLexer.XMLDOCCHANNEL
                _hasXmlDoc := TRUE
            OTHERWISE
                NOP
            END SWITCH
        NEXT
        _file:CommentTasks := _commentTasks
        _list :=XSharpTokenList{_input}

        DO WHILE ! SELF:Eoi()
            VAR tokenBefore := LastToken
            _firstTokenOnLine := SELF:Lt1
            LOCAL first := (XSharpToken) SELF:Lt1  AS XSharpToken
            IF SELF:ParsePPLine()
                LOOP
            ENDIF
            IF SELF:ParseUsing( OUT VAR memUsing)
                _EntityList:Add(memUsing)
                LOOP
            ENDIF
            LOCAL firstDocCommentToken := NULL AS XSharpToken
            SELF:ParseUdcTokens()
            aAttribs := SELF:ParseAttributes()
            VAR mods := SELF:ParseVisibilityAndModifiers()
            VAR vis  := _AND(mods, Modifiers.VisibilityMask)
            IF SELF:IsStartOfEntity(OUT VAR entityKind, mods)
                IF _hasXmlDoc
                    LOCAL cDoc := first:XmlComments AS STRING
                    if first:HasTrivia
                        foreach Var triv in first:Trivia
                            if triv:Type == XSharpLexer.DOC_COMMENT
                                firstDocCommentToken := triv
                                EXIT
                            ENDIF
                        NEXT
                    ENDIF

                    cDoc := cDoc:Replace("///","")
                    IF ! String.IsNullOrEmpty(cDoc)
                        cDoc    := cDoc:Replace('\r',' ')
                        cDoc    := cDoc:Replace('\n',' ')
                        cXmlDoc := "<doc>"+cDoc+"</doc>"
                    ELSE
                        cXmlDoc := NULL
                    ENDIF
                ENDIF
                // note: do not set this before the IsStartOfEntity check to make sure that
                // single identifiers on a line are not matched with the ClassVar rule
                IF vis == Modifiers.None
                    mods |= Modifiers.Public
                ENDIF
                SELF:_attributes  := mods
                SELF:_start := first
                VAR entities := SELF:ParseEntity(entityKind)
                _modifiers:Clear()
                IF entities != NULL
                    if first:HasTrivia
                        foreach t as XSharpToken in first:Trivia
                            if firstDocCommentToken == NULL .or. t:Line < firstDocCommentToken:Line
                                tokenBefore := t
                            endif
                        next
                    elseif first:OriginalTokenIndex > 0
                        tokenBefore := (XSharpToken)_tokens[first:OriginalTokenIndex-1]
                    endif
                    FOREACH VAR entity IN entities
                        IF entity == NULL
                            LOOP
                        ENDIF
                        entity:File := _file
                        IF _hasXmlDoc
                            entity:XmlComments   := cXmlDoc
                            if firstDocCommentToken != null
                                entity:StartOfXmlComments := firstDocCommentToken:Line-1
                            endif
                            if entity:Kind == Kind.Delegate .and. entity is XSourceTypeSymbol var xtype
                                var invoke := xtype:XMembers:First()
                                invoke:XmlComments := cXmlDoc
                                if firstDocCommentToken != null
                                    invoke:StartOfXmlComments := firstDocCommentToken:Line-1
                                endif
                            endif
                        ENDIF
                        IF aAttribs?:Count > 0
                            entity:CustomAttributes := SELF:TokensAsString(aAttribs)
                        ENDIF
                        VAR lastEntity := _EntityList:LastOrDefault()
                        IF lastEntity != NULL
                            if lastEntity:Kind:IsLocal()
                                // if the last entity is a local entity then take its parent
                                lastEntity := lastEntity:Parent astype XSourceEntity
                            endif
                            IF lastEntity != NULL .and. !entity:Kind:IsLocal()
                                if tokenBefore.Channel != XSharpLexer.DefaultTokenChannel
                                    var index := _tokens:IndexOf(tokenBefore)
                                    repeat
                                        tokenBefore := (XSharpToken) _tokens[index]
                                        index -= 1
                                    until index == 0 .or. tokenBefore.Channel == Lexer.DefaultTokenChannel
                                endif
                                if lastEntity:Kind:IsLocal()
                                    lastEntity := lastEntity:Parent astype XSourceEntity
                                endif
                                lastEntity:Range       := lastEntity:Range:WithEnd(tokenBefore)
                                lastEntity:Interval    := lastEntity:Interval:WithEnd(tokenBefore)
                            ENDIF
                        ENDIF
                        _EntityList:Add(entity)
                        VAR isMember := entity IS XSourceMemberSymbol
                        VAR isType   := entity IS XSourceTypeSymbol
                        var canAddMembers  := CurrentEntityKind:HasMembers()
                        var canAddChildren := CurrentEntityKind:HasChildren()
                        LOCAL mustPop as LOGIC
                        IF _EntityStack:Count == 0
                            mustPop := FALSE
                        ELSEIF isType
                            IF canAddChildren
                                mustPop := FALSE
                            ELSE
                                mustPop := TRUE
                            ENDIF
                        ELSEIF entity:Kind:IsLocal()
                            mustPop := FALSE
                        ELSEIF isMember
                            IF canAddMembers
                                mustPop := FALSE
                            ELSE
                                mustPop := TRUE
                            ENDIF
                        ELSEIF CurrentEntityKind:HasBody()
                            mustPop := FALSE
                        ELSE
                            mustPop := TRUE
                        ENDIF
                        IF mustPop
                            _EntityStack:Pop()
                            canAddMembers  := CurrentEntityKind:HasMembers()
                            canAddChildren := CurrentEntityKind:HasChildren()
                        ENDIF

                        IF entity:Kind:IsGlobalTypeMember() .AND. entity IS XSourceMemberSymbol VAR xGlobalMember
                            // GLOBAL, DEFINE, FUNCTION, PROCEDURE
                            // also #define, #command etc
                            SELF:_globalType:AddMember(xGlobalMember)
                        ELSEIF entity:Kind:IsLocal()
                            entity:Parent := CurrentEntity
                        ELSEIF canAddMembers .AND. CurrentEntity IS XSourceTypeSymbol VAR xEnt
                            // CurrentEntity should be a type: Class, Structure, Interface, Enum, VoStruct, Union
                            IF entity IS XSourceMemberSymbol VAR xMember .AND. xMember:Parent == NULL
                                xEnt:AddMember( xMember )
                            ELSEIF canAddChildren .and. xEnt != NULL .and. entity IS XSourceTypeSymbol VAR xChild .AND.  ;
                                    ! XSourceTypeSymbol.IsGlobalType(xEnt) .and. xEnt:Kind:HasChildren()
                                // Namespace, class, structure, interface can have children (nested types)
                                xEnt:AddChild( xChild )
                                xChild:Namespace := xEnt:FullName
                            ENDIF
                        ENDIF
                        IF ! entity:SingleLine
                            _EntityStack:Push(entity)
                        ENDIF
                        _BlockStack:Clear()
                    NEXT
                ELSE
                    NOP
                ENDIF
            ELSEIF SELF:IsEndOfEntity( OUT VAR endKind)
                VAR end1 := SELF:Lt1
                VAR end2 := SELF:Lt2
                VAR type := SELF:La2
                // match la2 with current entity
                DO WHILE _EntityStack:Count > 0
                    VAR top := _EntityStack:Pop()
                    // END PROCEDURE and END FUNCTION may indicate normal procedure and local procedure
                    IF top:Kind == Kind.LocalFunc .and. endKind == Kind.Function
                        NOP // Ok
                    ELSEIF top:Kind == Kind.LocalProc .and. endKind == Kind.Procedure
                        NOP // Ok
                    ELSEIF top:Kind != endKind
                        top:Range       := top:Range:WithEnd(tokenBefore)
                        top:Interval    := top:Interval:WithEnd(tokenBefore)
                        LOOP
                    ENDIF
                    top:BlockTokens:Add(end1)
                    if (end2:Type != XSharpLexer.EOS)
                        top:BlockTokens:Add(end2)
                    endif
                    top:Range       := top:Range:WithEnd(SELF:Lt2)
                    top:Interval    := top:Interval:WithEnd(SELF:Lt2)
                    EXIT
                ENDDO
                // CLear all blocks at the end of the entity
                _BlockStack:Clear()
                SELF:ReadLine()
            ELSEIF aAttribs:Count > 0 .AND. La1 == XSharpLexer.EOS
                // Add Attribute
                VAR attribute := SELF:ParseAttribute(aAttribs)
                _EntityList:Add(attribute)
            ELSEIF SELF:_collectBlocks
                SELF:ParseBlock()
                SELF:ParseStatement()
            ELSE
                SELF:ParseStatement()
            ENDIF
        ENDDO
        VAR types := SELF:_EntityList:Where( {x => x IS XSourceTypeSymbol})
        VAR typelist := XDictionary<STRING, XSourceTypeSymbol>{System.StringComparer.InvariantCultureIgnoreCase}
        typelist:Add(_globalType:Name, _globalType)
        LOCAL last  := NULL AS XSourceTypeSymbol
        FOREACH type AS XSourceTypeSymbol IN types
            IF last != NULL .AND. last:Range:StartLine == last:Range:EndLine
                // adjust the end of the type with the start of the current line
                VAR newEndLine := type:Range:StartLine-1
                VAR newEndPos  := type:Interval:Start -1

                last:Range     := TextRange{last:Range:StartLine, last:Range:StartColumn, newEndLine, 0}
                last:Interval  := TextInterval{last:Interval:Start, newEndPos}
            ENDIF
            IF type:Parent != NULL .AND. (type:Parent:Kind:IsType() .OR. type:Parent:Kind == Kind.Namespace)
                IF type:Parent == _globalType
                    type:Namespace := ""
                ELSE
                    type:Namespace := type:Parent:FullName
                ENDIF
            ENDIF
            IF type:Kind == Kind.Namespace
                // Add to usings when needed. Not strictly necessary but makes lookup easier later
                SELF:AddNameSpaceToUsing(type:Name)
            ELSEIF type:Name:Contains(".")
                VAR pos := type:Name:LastIndexOf(".")
                VAR ns  := type:Name:Substring(0, pos)
                SELF:AddNameSpaceToUsing(ns)
                type:Name   := type:Name:Substring(pos+1)
                IF String.IsNullOrEmpty(type:Namespace)
                    type:Namespace := ns
                ELSE
                    type:Namespace += "."+ns
                ENDIF
            ENDIF

            IF ! typelist:ContainsKey(type:FullName)
                type:File := _file
                typelist:Add(type:FullName, type)
            ELSE
                var existing := typelist[type:FullName]
                existing:AddMembers(type:XMembers)
            ENDIF
            last := type
        NEXT
        VAR lasttoken := _tokens[_tokens.Count -1]
        IF last != NULL .AND. last:Range:StartLine == last:Range:EndLine .and. ! last:SingleLine
            // adjust the end of the type with the start of the current line
            // find the last token in the stream

            last:Range     := last:Range:WithEnd(lasttoken)
            last:Interval  := last:Interval:WithEnd(lasttoken)
        ENDIF
        Log(i"Completed, found {_EntityList.Count} entities and {typelist.Count} types")
        IF SELF:_EntityList:Count > 0
            LOCAL lastEntity          := SELF:_EntityList:Last() as XSourceEntity
            if lastEntity:Kind:IsClassMember(_dialect)
                // if type has no end clause then also set the end

                if lastEntity:Range:StartLine == lastEntity:Range:EndLine .or. ;
                        (lastEntity is IXSourceEntity var srcent .and. srcent:SourceCode:Contains(";"))
                    lastEntity:Range        := lastEntity:Range:WithEnd(lasttoken)
                    lastEntity:Interval     := lastEntity:Interval:WithEnd(lasttoken)
                endif

            elseif ! lastEntity:Kind:HasEndKeyword()
                lastEntity:Range        := lastEntity:Range:WithEnd(lasttoken)
                lastEntity:Interval     := lastEntity:Interval:WithEnd(lasttoken)

            ENDIF
        ELSE
            // Add at least one entity that represents the global namespace
            SELF:GetSourceInfo(_tokens[0], _tokens[_tokens:Count-1], OUT VAR range, OUT VAR interval, OUT VAR source)

            VAR xmember := XSourceMemberSymbol{_globalType:Name,Kind.Namespace, ;
                Modifiers.Export, range, interval, "", _modifiers, FALSE}
            xmember.File := _file
            xmember.SourceCode := source
            SELF:_EntityList:Add(xmember)
            SELF:_globalType:AddMember(xmember)
        ENDIF
        IF ! lLocals
            _file:SetTypes(typelist, _usings, _staticusings, SELF:_EntityList)
            IF SELF:SaveToDisk
                _file:SaveToDatabase()
            ENDIF
            _file:NofityClients()
        ENDIF
    PRIVATE METHOD AddNameSpaceToUsing(name as STRING) AS VOID
        var pos  := name:LastIndexOf(".")
        SELF:AddUniqueUsing(name)
        DO WHILE pos > 0
            name := name:Substring(0, pos)
            SELF:AddUniqueUsing(name)
            pos  := name:LastIndexOf(".")
        ENDDO
        RETURN

    PRIVATE METHOD AddUniqueUsing(strName as STRING) AS VOID
        FOREACH var u in SELF:_usings
            if String.Compare(u, strName, TRUE) == 0
                RETURN
            ENDIF
        NEXT
        _usings:Add(strName)
        RETURN

    PRIVATE METHOD ParsePPLine() AS LOGIC
        LOCAL entity as XSourceMemberSymbol
        LOCAL kind AS Kind
        VAR token := SELF:La1
        VAR start := SELF:Lt1

        SWITCH token
        CASE XSharpLexer.PP_REGION
        CASE XSharpLexer.PP_IFDEF
        CASE XSharpLexer.PP_IFNDEF
        CASE XSharpLexer.PP_IF
        CASE XSharpLexer.PP_TEXT
            VAR block := XSourceBlock{ XKeyword{SELF:La1}, SELF:Lt1}
            _BlockList:Add(block)
            _PPBlockStack:Push(block)
        CASE XSharpLexer.PP_ENDREGION
        CASE XSharpLexer.PP_ENDTEXT
        CASE XSharpLexer.PP_ENDIF
            // end
            IF _PPBlockStack:Count > 0
                _PPBlockStack:Peek():Children:Add( XBlockChild{XKeyword{SELF:La1}, SELF:Lt1})
                _PPBlockStack:Pop()
            ENDIF
        CASE XSharpLexer.PP_ELSE
            // middle
            IF _PPBlockStack:Count > 0
                _PPBlockStack:Peek():Children:Add( XBlockChild{XKeyword{SELF:La1}, SELF:Lt1})
            ENDIF
        CASE XSharpLexer.PP_INCLUDE
            var sb := StringBuilder{}
            kind   := Kind.Include
            SELF:Consume()
            VAR eol   := SELF:Lt1
            DO WHILE SELF:La1 != XSharpLexer.EOS
                eol   := SELF:Lt1
                sb:Append(eol:Text)
                SELF:Consume()
            ENDDO
            SELF:GetSourceInfo(start, eol, OUT VAR range, OUT VAR interval, OUT VAR source)
            VAR name := sb:ToString():Trim()
            if name:StartsWith("""") .and. name.EndsWith("""")
                name := name:Substring(1, name:Length-2)
            endif
            entity := XSourceMemberSymbol{name, kind, Modifiers.None, range,interval,"",_modifiers,FALSE}
            entity:SourceCode := source
        CASE XSharpLexer.PP_DEFINE
        CASE XSharpLexer.PP_UNDEF
        CASE XSharpLexer.PP_COMMAND
        CASE XSharpLexer.PP_TRANSLATE

            VAR name  := SELF:Lt2:Text
            VAR eol   := SELF:Lt2
            var hasId := SELF:IsId(SELF:La2)
            if SELF:La2 == XSharpLexer.BACKSLASH
                eol   := SELF:Lt3
                if ! hasId .and. SELF:IsId(eol:Type)
                    name  := eol:Text
                    hasId := TRUE
                endif
            ENDIF
            DO WHILE SELF:La1 != XSharpLexer.EOS
                eol := SELF:Lt1
                if ! hasId .and. SELF:IsId(eol:Type)
                    name  := eol:Text
                    hasId := TRUE
                endif
                SELF:Consume()
            ENDDO
            SELF:GetSourceInfo(start, eol, OUT VAR range, OUT VAR interval, OUT VAR source)
            SWITCH token
            CASE XSharpLexer.PP_DEFINE
                kind := Kind.Define
            CASE XSharpLexer.PP_UNDEF
                kind := Kind.Undefine
            CASE XSharpLexer.PP_COMMAND
                kind := Kind.Command
                VAR cType := start:Text[1]
                IF cType == c'x' .OR. cType == c'X'
                    kind := Kind.XCommand
                ENDIF
            CASE XSharpLexer.PP_TRANSLATE
                kind := Kind.Translate
                VAR cType := start:Text[1]
                IF cType == c'x' .OR. cType == c'X'
                    kind := Kind.XTranslate
                ENDIF
            END SWITCH
            entity := XSourceMemberSymbol{name, kind, Modifiers.None, range,interval,"",_modifiers,FALSE}
            entity:SourceCode := source
        OTHERWISE
            RETURN FALSE
        END SWITCH
        SELF:ReadLine()
        if entity != NULL
            entity:ReturnType := ""
            entity:File := _file
            entity:SingleLine := TRUE
            _EntityList.Add(entity)
            _globalType:AddMember(entity)
        ENDIF
        RETURN TRUE

    PRIVATE METHOD ParseUsing(entUsing out XSourceMemberSymbol) AS LOGIC
        /*
        using_              : USING (Static=STATIC)? (Alias=identifierName Op=assignoperator)? Name=name EOS
        ;

        */
        entUsing := NULL
        IF SELF:La1 != XSharpLexer.USING
            RETURN FALSE
        ENDIF
        IF SELF:ExpectOnThisLine(XSharpLexer.VAR)             // USING VAR
            RETURN FALSE
        ENDIF
        IF SELF:ExpectOnThisLine(XSharpLexer.IMPLIED)
            RETURN FALSE
        ENDIF
        VAR startToken := SELF:ConsumeAndGet()
        VAR isStatic := FALSE
        VAR alias := ""
        IF SELF:Expect(XSharpLexer.STATIC)
            isStatic := TRUE
        ENDIF
        IF SELF:IsId(SELF:La1) .AND. SELF:IsAssignOp(SELF:La2)
            // name :=
            alias := SELF:ConsumeAndGetText()
            SELF:Consume()   // :=
        ENDIF
        VAR name := SELF:ParseQualifiedName()
        IF isStatic
            SELF:_staticusings:Add(name)
        ELSE
            SELF:_usings:Add(name)
        ENDIF
        var endToken := Self:Lt1
        SELF:ReadLine()
        var mods := Modifiers.None
        if (isStatic)
            mods := Modifiers.Static
        endif
        SELF:GetSourceInfo(startToken, endToken , OUT VAR range, OUT VAR interval, OUT VAR source)
        VAR entity := XSourceMemberSymbol{name, Kind.Using, mods, range,interval,"",_modifiers,FALSE}
        entity:SourceCode := source
        entity:File := _file
        entity:SingleLine := TRUE
        _globalType:AddMember(entity)
        entUsing := entity
        RETURN TRUE
    PRIVATE METHOD ParseUdcTokens() AS VOID
        DO WHILE SELF:La1 == XSharpLexer.UDC_KEYWORD
            var token := (XSharpToken) SELF:Lt1
            switch token:Text:ToUpper()
            case "TEXT"
                token:Type := XSharpLexer.PP_TEXT
                return
            case "ENDTEXT"
                token:Type := XSharpLexer.PP_ENDTEXT
                return
            otherwise
                var xt := SELF:GetNextKeyword()
                if XFormattingRule.IsStartKeyword(xt)
                    RETURN
                ENDIF
                if XFormattingRule.IsMiddleKeyword(xt)
                    RETURN
                ENDIF
                if XFormattingRule.IsEndKeyword(xt)
                    RETURN
                ENDIF
                SELF:Consume()
            end switch
        ENDDO
        RETURN

    PRIVATE METHOD ParseAttributes() AS IList<IToken>
        /*
        attributes          : ( AttrBlk+=attributeBlock )+
        ;

        attributeBlock      : LBRKT Target=attributeTarget? Attributes+=attribute (COMMA Attributes+=attribute)* RBRKT
        | String=BRACKETED_STRING_CONST
        ;

        attributeTarget     : Token=(ID | CLASS | CONSTRUCTOR | DELEGATE | ENUM | EVENT | FIELD | INTERFACE | METHOD | PROPERTY  | RETURN | STRUCTURE ) COLON
        ;

        attribute           : Name=name (LPAREN (Params+=attributeParam (COMMA Params+=attributeParam)* )? RPAREN )?
        ;

        attributeParam      : Name=identifierName Op=assignoperator Expr=expression     #propertyAttributeParam
        | Expr=expression                                   #exprAttributeParam
        ;

        */
        // Please note that in the editor we do not check the contents of the attributes.
        // we simply parse LBRKT ... RBRKT groups until we find no more LBRKT

        VAR tokens := List<IToken>{}
        DO WHILE SELF:La1 == XSharpLexer.LBRKT .AND. ! SELF:Eos()
            tokens:Add(SELF:ConsumeAndGet())
            DO WHILE SELF:La1 != XSharpLexer.RBRKT .AND. ! SELF:Eos()
                tokens:Add(SELF:ConsumeAndGet())
            ENDDO
            IF SELF:La1 == XSharpLexer.RBRKT
                tokens:Add(SELF:ConsumeAndGet())
            ENDIF
        ENDDO
        RETURN tokens

    PRIVATE METHOD ParseVisibilityAndModifiers() AS   Modifiers
        VAR result := Modifiers.None
        _modifiers:Clear()
        DO WHILE ! SELF:Eos()
            VAR done := FALSE
            SWITCH SELF:La1
                // Visibility Alphabetical
            CASE XSharpLexer.EXPORT
                result |= Modifiers.Public
            CASE XSharpLexer.HIDDEN
                result |= Modifiers.Private
            CASE XSharpLexer.INTERNAL
                result |= Modifiers.Internal
            CASE XSharpLexer.PRIVATE
                result |= Modifiers.Private
            CASE XSharpLexer.PROTECTED
                result |= Modifiers.Protected
            CASE XSharpLexer.PUBLIC
                result |= Modifiers.Public

                // Real modifiers Alphabetical
            CASE XSharpLexer.ABSTRACT
                result |= Modifiers.Abstract
            CASE XSharpLexer.ASYNC
                result |= Modifiers.Async
            CASE XSharpLexer.CONST
                result |= Modifiers.Const
                result |= Modifiers.Static
            CASE XSharpLexer.EXTERN
                result |= Modifiers.External
            CASE XSharpLexer.INITONLY
                result |= Modifiers.InitOnly
            CASE XSharpLexer.INSTANCE
                result |= Modifiers.Instance
                result |= Modifiers.Protected
            CASE XSharpLexer.NEW
                result |= Modifiers.New
            CASE XSharpLexer.OVERRIDE
                result |= Modifiers.Override
            CASE XSharpLexer.PARTIAL
                result |= Modifiers.Partial
            CASE XSharpLexer.SEALED
                result |= Modifiers.Sealed
            CASE XSharpLexer.STATIC
                result |= Modifiers.Static
            CASE XSharpLexer.UNSAFE
                result |= Modifiers.Unsafe
            CASE XSharpLexer.VIRTUAL
                result |= Modifiers.Virtual
            CASE XSharpLexer.VOLATILE
                result |= Modifiers.Volatile

                // XPP modifiers
            CASE XSharpLexer.DEFERRED
                result |= Modifiers.Deferred
            CASE XSharpLexer.FINAL
                result |= Modifiers.Final
            CASE XSharpLexer.FREEZE
                result |= Modifiers.Freeze
            CASE XSharpLexer.INTRODUCE
                result |= Modifiers.Introduce
            CASE XSharpLexer.SYNC
                result |= Modifiers.Sync
            OTHERWISE
                done := TRUE
            END SWITCH
            IF ! done
                _modifiers:Add(SELF:Lt1)
                SELF:Consume()
            ELSE
                EXIT
            ENDIF
        ENDDO
        RETURN result


    PRIVATE METHOD IsStartOfEntity(entityKind OUT Kind, mods AS Modifiers) AS LOGIC
        entityKind := Kind.Unknown
        SWITCH SELF:La1
        CASE XSharpLexer.BEGIN
            // namespace ?
            IF SELF:La2 == XSharpLexer.NAMESPACE
                IF SELF:IsId(SELF:La3)
                    entityKind := Kind.Namespace
                ENDIF
            ENDIF
        CASE XSharpLexer.CLASS
            IF SELF:InXppClass
                IF SELF:La2 == XSharpLexer.METHOD  .OR. XSharpLexer.IsModifier(SELF:La2)   // XPP has CLASS METHOD = a static method
                    entityKind := Kind.Method
                ELSEIF SELF:La2 == XSharpLexer.VAR                                  // CLASS VAR for XPP
                    entityKind := Kind.Field
                ENDIF
            ELSEIF SELF:IsId(SELF:La2)
                entityKind := Kind.Class
            ENDIF
        CASE XSharpLexer.STRUCTURE
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.Structure
            ENDIF
        CASE XSharpLexer.DELEGATE
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.Delegate
            ENDIF
        CASE XSharpLexer.INTERFACE
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.Interface
            ENDIF
        CASE XSharpLexer.ENUM
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.Enum
            ENDIF
        CASE XSharpLexer.EVENT
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.Event
            ENDIF
        CASE XSharpLexer.METHOD
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.Method
            ENDIF
        CASE XSharpLexer.ACCESS
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.Access
            ENDIF
        CASE XSharpLexer.ASSIGN
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.Assign
            ENDIF
        CASE XSharpLexer.PROPERTY
            IF SELF:IsId(SELF:La2) .or. SELF:La2 == XSharpLexer.SELF
                entityKind := Kind.Property
            ENDIF
        CASE XSharpLexer.OPERATOR
            entityKind := Kind.Operator
        CASE XSharpLexer.CONSTRUCTOR
            entityKind := Kind.Constructor
        CASE XSharpLexer.DESTRUCTOR
            entityKind := Kind.Destructor
        CASE XSharpLexer.DECLARE
            // access, assign, method
            IF SELF:La2 == XSharpLexer.ASSIGN .OR. SELF:La2 == XSharpLexer.ACCESS .OR. SELF:La2 == XSharpLexer.METHOD
                entityKind := Kind.Ignore
            ENDIF
        CASE XSharpLexer.DEFINE
            // define class ?
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.VODefine
            ELSEIF _dialect == XDialect.FoxPro
                entityKind := Kind.Class
            ENDIF
        CASE XSharpLexer.VOSTRUCT
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.VOStruct
            ENDIF
        CASE XSharpLexer.UNION
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.Union
            ENDIF
        CASE XSharpLexer.MEMBER
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.EnumMember
            ENDIF
        CASE XSharpLexer.ADD
            // Todo handle Add Object clause inside Class.
            IF SELF:La2 == XSharpLexer.OBJECT
                entityKind := Kind.Field
            ENDIF
        CASE XSharpLexer.FUNCTION
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.Function
            ENDIF
        CASE XSharpLexer.PROCEDURE
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.Procedure
            ENDIF
        CASE XSharpLexer.INIT
        CASE XSharpLexer.EXIT
            IF SELF:La2 == XSharpLexer.PROCEDURE .and. ;
                SELF:IsId(SELF:La3)
                entityKind := Kind.Procedure
            ENDIF


        CASE XSharpLexer.GLOBAL
            IF SELF:IsId(SELF:La2)
                entityKind := Kind.VOGlobal
            ENDIF
        CASE XSharpLexer.DLL
            IF SELF:La2 == XSharpLexer.FUNCTION .OR. SELF:La2 == XSharpLexer.PROCEDURE
                entityKind := Kind.VODLL
            ENDIF
        CASE XSharpLexer.FIELD
            // field declaration only inside FoxPro class definition
            IF InFoxClass .AND. (CurrentEntity:Kind == Kind.Class .OR. CurrentEntity:Kind == Kind.Field)
                entityKind := Kind.Field
            ENDIF
        CASE XSharpLexer.IMPLEMENTS
            IF InFoxClass
                entityKind := Kind.Field // Not really a field but handled later
            ENDIF
        CASE XSharpLexer.DIMENSION
            // make sure that DIMENSION inside a method is not matched
            IF InFoxClass
                entityKind := Kind.Field // Not really a field but handled later
            ENDIF
            // XPP code between CLASS .. ENDCLASS
            // fail when partially parseing for locals lookup
        CASE XSharpLexer.VAR WHEN  SELF:InXppClass
            entityKind := Kind.Field // Not really a field but handled later

        CASE XSharpLexer.COLON WHEN SELF:InXppClass
            IF _AND(mods, Modifiers.VisibilityMask) != Modifiers.None
                entityKind := Kind.Field // Not really a field but handled later
            ENDIF
        CASE XSharpLexer.INLINE WHEN SELF:InXppClass
            entityKind := Kind.Method // Not really a field but handled later
        CASE XSharpLexer.LOCAL
            IF SELF:La2 == XSharpLexer.FUNCTION
                entityKind := Kind.LocalFunc
            ELSEIF SELF:La2 == XSharpLexer.PROCEDURE
                entityKind := Kind.LocalProc
            ENDIF
        CASE XSharpLexer.GET
        CASE XSharpLexer.SET
            //CASE XSharpLexer.ADD
        CASE XSharpLexer.REMOVE
            entityKind := Kind.Unknown
        OTHERWISE
            IF SELF:IsId(SELF:La1)
                IF mods != Modifiers.None
                    LOCAL parent AS XSourceTypeSymbol
                    IF CurrentEntity IS XSourceTypeSymbol
                        parent := (XSourceTypeSymbol) CurrentEntity
                    ELSEIF CurrentEntity != NULL .AND. CurrentEntity:Parent IS XSourceTypeSymbol
                        parent := (XSourceTypeSymbol) CurrentEntity:Parent
                    ENDIF
                    IF ! XSourceTypeSymbol.IsGlobalType(parent)
                        entityKind := Kind.Field
                    ENDIF
                    // PRIVATE and PUBLIC as memvar declarator
                    // when inside a method or function
                    IF mods == Modifiers.Public .OR. mods == Modifiers.Private
                        var txt := LastToken.Text.ToUpper()
                        if (txt == "PUBLIC" .or. txt == "PRIVATE")
                            IF CurrentEntity IS XSourceMemberSymbol VAR xDef .AND. ! xDef:SingleLine .AND. CurrentEntity.Kind:HasBody()
                                if SELF:SupportsMemVars
                                    entityKind := Kind.Unknown
                                endif
                            ENDIF
                        ENDIF
                    ENDIF
                ELSEIF InFoxClass .AND. (CurrentEntity:Kind == Kind.Class .OR. CurrentEntity:Kind == Kind.Field)
                    IF SELF:La1 == XSharpLexer.ID .AND. SELF:IsAssignOp(SELF:La2)
                        entityKind := Kind.Field
                    ELSEIF SELF:La1 == XSharpLexer.ID .AND. SELF:Lt1:Text:EndsWith("COMATTRIB", StringComparison.OrdinalIgnoreCase)
                        entityKind := Kind.Field
                    ENDIF
                ELSEIF CurrentType?:Kind == Kind.Enum
                    entityKind := Kind.EnumMember
                ENDIF
            ENDIF

        END SWITCH

        RETURN entityKind != Kind.Unknown


    PRIVATE METHOD ParseBlockStart(rule as XFormattingRule, xt as XKeyword) AS VOID
        // Does not process the tokens on the line !
        IF rule:Flags:HasFlag(XFormattingFlags.Member)
            IF SELF:_collectLocals
                // these are the start keywords that can introduce a local variable
                SWITCH SELF:La1
                CASE XSharpLexer.SET
                CASE XSharpLexer.REMOVE
                CASE XSharpLexer.ADD
                CASE XSharpLexer.INIT       // For Init accessors

                    // Add value token inside accessors
                    VAR id := "Value"
                    VAR strType := SELF:CurrentEntity:TypeName
                    VAR start := SELF:Lt1
                    VAR stop  := SELF:Lt2
                    SELF:GetSourceInfo(start, stop, OUT VAR range, OUT VAR interval, OUT VAR _)
                    VAR xVar := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval, strType}
                    SELF:_locals:Add(xVar)
                END SWITCH
            endif
            IF SELF:_collectBlocks
                if (_BlockStack:Count > 0)
                    var curblock := CurrentBlock
                    if xt:Kw1 == XTokenType.Set .or. xt:Kw1 == XTokenType.Get .or. xt:Kw1 == XTokenType.Init
                        if curblock:XKeyword:Kw1 == XTokenType.Set .or. ;
                                curblock:XKeyword:Kw1 == XTokenType.Get .or.;
                                curblock:XKeyword:Kw1 == XTokenType.Init
                            _BlockStack:Pop()
                        endif
                    elseif xt:Kw1 == XTokenType.Add .or. xt:Kw1 == XTokenType.Remove
                        if curblock:XKeyword:Kw1 == XTokenType.Add .or. ;
                                curblock:XKeyword:Kw1 == XTokenType.Remove
                            _BlockStack:Pop()
                        endif
                    endif
                ENDIF
                VAR block := XSourceBlock{ xt, SELF:Lt1}
                IF ! xt:IsSingle
                    block:Children:Add (XBlockChild{xt, SELF:Lt2})
                ENDIF
                _BlockList:Add(block)
                _BlockStack:Push(block)
            ENDIF
        ELSEIF rule:Flags:HasFlag(XFormattingFlags.Statement)
            // This means that the keyword is a start keyword of type statement
            IF SELF:_collectBlocks
                LOCAL block AS XSourceBlock
                block := XSourceBlock{ xt, SELF:Lt1}
                IF ! xt:IsSingle
                    block:Children:Add (XBlockChild{xt, SELF:Lt2})
                ENDIF
                // check for GET SET INIT blocks on a single line
                // or ADD/REMOVE blocks on a single line
                if XFormattingRule.IsMiddleKeyword(xt) .and. _BlockStack:Count > 0
                    CurrentBlock:Children:Add( XBlockChild{xt, SELF:Lt1})
                else
                    _BlockList:Add(block)
                    _BlockStack:Push(block)
                endif
            ENDIF
        ENDIF
        RETURN

    PRIVATE METHOD ParseJumpStatement(xt AS XKeyword) AS VOID
        SWITCH SELF:La1
        CASE XSharpLexer.EXIT
        CASE XSharpLexer.LOOP
            SELF:Consume()
        END SWITCH
        RETURN

    PRIVATE METHOD ParseBlockMiddle(xt as XKeyword) AS VOID
        // Does not process the tokens on the line !
        IF SELF:_collectLocals
            SWITCH SELF:La1
            CASE XSharpLexer.CATCH WHEN SELF:IsId(SELF:La2)
                SELF:Consume()
                VAR start   := SELF:Lt1
                VAR id      := SELF:ParseIdentifier()
                VAR strType := "System.Exception"
                IF SELF:La1 == XSharpLexer.AS
                    strType := SELF:ParseDataType(FALSE)
                ENDIF

                SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
                VAR xVar := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval, strType}
                SELF:_locals:Add(xVar)

            CASE XSharpLexer.ELSEIF // These could be followed by an IS Type AS Id
            CASE XSharpLexer.CASE
                SELF:ParseForInlineLocals()

            CASE XSharpLexer.RECOVER    // the possible ID has to be predeclared as local
            CASE XSharpLexer.ELSE       // No Id allowed
            CASE XSharpLexer.FINALLY    // No Id allowed
            OTHERWISE
                SELF:ReadLine()
            END SWITCH

        ENDIF
        RETURN

    PRIVATE METHOD GetNextKeyword() AS XKeyword
        var oLt1 := Lt1
        var oLt2 := Lt2
        if oLt1:Type == XSharpLexer.UDC_KEYWORD
            oLt1 := oLt1:Original
        ENDIF
        if oLt2:Type == XSharpLexer.UDC_KEYWORD
            oLt2 := oLt2:Original
        ENDIF
        local xt as XKeyword
        if XFormattingRule.IsSingleKeyword(oLt1:Type)
            xt := XKeyword{oLt1:Type}
        elseif XSharpLexer.IsKeyword(oLt2:Type)
            xt := XKeyword{oLt1:Type, oLt2:Type}
        else
            xt := XKeyword{oLt1:Type}
        endif
        return xt
    PRIVATE METHOD ParseBlock() AS VOID
        // Adds, updates or removes block token on the block tokens stack
        // Start of block is also added to the _BlockList
        // Does not process the tokens on the line !
        SELF:ParseUdcTokens()  // Read UDC tokens on the current line
        if !XSharpLexer.IsKeyword(La1) .and. ! XSharpLexer.IsPPKeyword(La1)
            return
        endif
        local xt as XKeyword
        LOCAL rule as XFormattingRule
        xt := SELF:GetNextKeyword()
        rule := XFormattingRule.GetFirstRuleByStart(xt)
        if rule != null
            SELF:ParseBlockStart(rule, xt)
        ELSEIF XFormattingRule.IsEndKeyword(xt) .AND. SELF:KeywordMatchesBlock(xt, CurrentBlock)
            IF SELF:_collectBlocks .AND. _BlockStack:Count > 0
                CurrentBlock:Children:Add( XBlockChild{xt, SELF:Lt1})
                IF !xt:IsSingle
                     // END DO, END CASE etc
                     CurrentBlock:Children:Add( XBlockChild{xt, SELF:Lt2})
                ENDIF
                _BlockStack:Pop()
            ENDIF
        ELSEIF XFormattingRule.IsMiddleKeyword(xt) .AND. SELF:KeywordMatchesBlock(xt, CurrentBlock)
            IF SELF:_collectBlocks .AND. _BlockStack:Count > 0
                CurrentBlock:Children:Add( XBlockChild{xt, SELF:Lt1})
            ENDIF
            SELF:ParseBlockMiddle(xt)
        ELSEIF xt.Kw1 == XSharpLexer.RETURN
            if SELF:CurrentEntity != NULL
                SELF:CurrentEntity:BlockTokens:Add(SELF:Lt1)
            endif
        ELSEIF XFormattingRule.IsJumpKeyword(xt)
            IF SELF:_collectBlocks .AND. _BlockStack:Count > 0
                IF _BlockStack:Count > 0
                    VAR temp := _BlockStack:ToArray()
                    FOREACH VAR item IN temp
                        IF item:CanJump
                            item:Children:Add(XBlockChild{xt, SELF:Lt1})
                            EXIT
                        ENDIF
                    NEXT
                ENDIF
            ENDIF
            SELF:ParseJumpStatement(xt)
        ENDIF
        RETURN
    PRIVATE METHOD KeywordMatchesBlock(kw AS XKeyword, block AS XSourceBlock) AS LOGIC
        IF block == NULL
            RETURN FALSE
        ENDIF
        VAR rules := XFormattingRule.GetStartRules(block:XKeyword)
        FOREACH rule AS XFormattingRule IN rules
            IF rule:Stop:Code == kw:Code
                RETURN TRUE
            ENDIF
        NEXT
        RETURN FALSE

    PRIVATE METHOD IsEndOfEntity(EntityKind OUT Kind) AS LOGIC
        SWITCH SELF:La1
        CASE XSharpLexer.END
            SWITCH SELF:La2
                // mandatory END <keyword> pairs
            CASE XSharpLexer.NAMESPACE
                EntityKind := Kind.Namespace
                RETURN TRUE
            CASE XSharpLexer.CLASS
            CASE XSharpLexer.DEFINE // FoxPro
                EntityKind := Kind.Class
                RETURN TRUE
            CASE XSharpLexer.STRUCTURE
                EntityKind := Kind.Structure
                RETURN TRUE
            CASE XSharpLexer.ENUM
                EntityKind := Kind.Enum
                RETURN TRUE

            CASE XSharpLexer.INTERFACE
                EntityKind := Kind.Interface
                RETURN TRUE

            CASE XSharpLexer.PROPERTY
                EntityKind := Kind.Property
                RETURN TRUE

            CASE XSharpLexer.EVENT
                EntityKind := Kind.Event
                RETURN TRUE

                // Optional

            CASE XSharpLexer.METHOD
                EntityKind := Kind.Method
                RETURN TRUE

            CASE XSharpLexer.ACCESS
                EntityKind := Kind.Access
                RETURN TRUE

            CASE XSharpLexer.ASSIGN
                EntityKind := Kind.Assign
                RETURN TRUE

            CASE XSharpLexer.OPERATOR
                EntityKind := Kind.Operator
                RETURN TRUE

            CASE XSharpLexer.CONSTRUCTOR
                EntityKind := Kind.Constructor
                RETURN TRUE

            CASE XSharpLexer.DESTRUCTOR
                EntityKind := Kind.Destructor
                RETURN TRUE

            CASE XSharpLexer.FUNCTION
                IF InFoxClass
                    EntityKind := Kind.Method
                ELSE
                    EntityKind := Kind.Function
                ENDIF
                RETURN TRUE

            CASE XSharpLexer.PROCEDURE
                IF InFoxClass
                    EntityKind := Kind.Method
                ELSE
                    EntityKind := Kind.Procedure
                ENDIF
                RETURN TRUE

            CASE XSharpLexer.VOSTRUCT
                EntityKind := Kind.VOStruct
                RETURN TRUE

            CASE XSharpLexer.UNION
                EntityKind := Kind.Union
                RETURN TRUE

                // END <nothing> is NOT allowed for entities since that conflicts with the END for blocks
            END SWITCH
        CASE XSharpLexer.ENDDEFINE
            EntityKind := Kind.Class
            RETURN TRUE

        CASE XSharpLexer.ENDCLASS
            EntityKind := Kind.Class
            RETURN TRUE

        CASE XSharpLexer.ENDPROC
            IF InFoxClass
                EntityKind := Kind.Method
            ELSE
                EntityKind := Kind.Procedure
            ENDIF
            RETURN TRUE

        CASE XSharpLexer.ENDFUNC
            IF InFoxClass
                EntityKind := Kind.Method
            ELSE
                EntityKind := Kind.Function
            ENDIF
            RETURN TRUE
        END SWITCH
        EntityKind := Kind.Unknown
        RETURN FALSE




#region Properties and Methods that are delegated to the XSharpTokenList type
    PRIVATE PROPERTY La1 AS INT => _list:La1
    PRIVATE PROPERTY La2 AS INT => _list:La2
    PRIVATE PROPERTY La3 AS INT => _list:La3
    PRIVATE PROPERTY Lt1 AS XSharpToken => (XSharpToken) _list:Lt1
    PRIVATE PROPERTY Lt2 AS XSharpToken => (XSharpToken) _list:Lt2
    PRIVATE PROPERTY Lt3 AS XSharpToken => (XSharpToken) _list:Lt3
    PRIVATE PROPERTY LastToken AS IToken => _list:LastReadToken
    PRIVATE METHOD La(nToken AS LONG) AS LONG => _list:La(nToken)
    PRIVATE METHOD Lt(nToken AS LONG) AS IToken => _list:Lt(nToken)
    PRIVATE METHOD Eoi() AS LOGIC => _list:Eoi()
    PRIVATE METHOD Eos() AS LOGIC => _list:Eos()
    PRIVATE METHOD Consume() AS VOID =>_list:Consume()
    PRIVATE METHOD ConsumeAndGet() AS IToken => _list:ConsumeAndGet()
    PRIVATE METHOD ConsumeAndGetAny(nTypes PARAMS LONG[]) AS IToken => _list:ConsumeAndGetAny(nTypes)
    PRIVATE METHOD ConsumeAndGetText() AS STRING => _list:ConsumeAndGetText()
    PRIVATE METHOD Expect(nType AS LONG) AS LOGIC => _list:Expect(nType)
    PRIVATE METHOD ExpectAny(nTypes PARAMS LONG[]) AS LOGIC => _list:ExpectAny(nTypes)
    PRIVATE METHOD ExpectAndGet(nType AS LONG, t OUT IToken) AS LOGIC => _list:ExpectAndGet(nType, OUT t)
    /// <summary>
    /// return TRUE when the token matches the type
    /// </summary>
    /// <param name="nType"></param>
    /// <returns></returns>
    PRIVATE METHOD Matches(nType AS LONG) AS LOGIC => _list:La1 == nType
    /// <summary>
    /// return TRUE when the one of the tokens matches the type
    /// </summary>
    /// <param name="nType"></param>
    /// <returns></returns>
    PRIVATE METHOD Matches(nTypes PARAMS LONG[]) AS LOGIC => _list:Matches(nTypes)
    PRIVATE METHOD PushBack() AS VOID => _list:PushBack()
    PRIVATE METHOD ReadLine() AS VOID => _list:ReadLine()
    PRIVATE METHOD ReadUntilEos() AS VOID
        DO WHILE _list:La1 != XSharpLexer.EOS .and. !_list:Eoi()
            _list:Consume()
        ENDDO
#endregion

    PRIVATE METHOD ExpectOnThisLine(nType as LONG) AS LOGIC
        return _list:ExpectOnThisLine(nType)

    /// <summary>
    /// Return TRUE when next token = ':=' or '='
    /// </summary>
    /// <returns></returns>
    PRIVATE METHOD ExpectAssignOp() AS LOGIC
        RETURN SELF:ExpectAny(XSharpLexer.ASSIGN_OP, XSharpLexer.EQ)

    PRIVATE METHOD IsId(token AS LONG) AS LOGIC
        if token == XSharpLexer.ID
            RETURN TRUE
        ENDIF
        // Soft keywords need to be
        RETURN SELF:IsKeywordXs(token) .OR. SELF:IsKeywordFox(token) .OR. SELF:IsKeywordXpp(token)


    PRIVATE METHOD ParseIdentifier() AS STRING
        IF SELF:IsId(SELF:La1)
            VAR id := SELF:ConsumeAndGetText()
            IF id:StartsWith("@@")
                id := id:Substring(2)
            ENDIF
            RETURN id
        ENDIF
        RETURN ""


    PRIVATE METHOD ParseAttribute(aAttribs AS IList<IToken>) AS XSourceEntity
        VAR name := SELF:TokensAsString(aAttribs, FALSE)
        SELF:GetSourceInfo(aAttribs[0], aAttribs[aAttribs:Count-1], OUT VAR range, OUT VAR interval, OUT VAR source)
        VAR entity := XSourceMemberSymbol{name, Kind.Attribute, Modifiers.None,;
            range,interval,"",_modifiers,FALSE}
        entity:SourceCode := source
        entity:File := _file
        entity:SingleLine := TRUE
        _globalType:AddMember(entity)
        RETURN entity

    PRIVATE METHOD ParseOptionalClassClause() AS STRING
        // parse the clause after a METHOD, ACCESS, ASSIGN, CONSTRUCTOR, DESTRUCTOR CLASS <Identifier>
        IF SELF:Expect(XSharpLexer.CLASS)
            RETURN SELF:ParseIdentifier()
        ENDIF
        RETURN ""


    PRIVATE METHOD ParseQualifiedName() AS STRING
        LOCAL result := "" AS STRING
        VAR Tokens := List<IToken>{}
        IF SELF:IsId(SELF:La1)
            Tokens:Add(SELF:ConsumeAndGet())
            DO WHILE SELF:Matches(XSharpLexer.DOT) .AND.  SELF:IsId(SELF:La2) .AND. ! SELF:Eos()
                Tokens:Add(SELF:ConsumeAndGet())
                Tokens:Add(SELF:ConsumeAndGet())
            ENDDO
        ENDIF
        RETURN SELF:TokensAsString(Tokens,FALSE)

    PRIVATE METHOD TokensAsString(tokens AS IList<IToken>, lAddTrivia := TRUE AS LOGIC) AS STRING
        LOCAL sb AS StringBuilder
        IF (tokens == NULL .or. tokens:Count == 0)
            RETURN ""
        ENDIF
        sb := StringBuilder{}

        FOREACH t AS XSharpToken IN tokens
            IF t:HasTrivia .AND. lAddTrivia
                sb:Append(t:TriviaAsText)
            ENDIF
            sb:Append(t:CleanText())
        NEXT
        RETURN sb:ToString():Trim()

    PRIVATE METHOD ParseEntity(entityKind AS Kind) AS IList<XSourceEntity>
        LOCAL result AS IList<XSourceEntity>
        SWITCH entityKind
        CASE Kind.Ignore
            SELF:ReadLine()
        CASE Kind.Namespace
            result := SELF:ParseNamespace()
        CASE Kind.Class
            IF SELF:Matches(XSharpLexer.DEFINE)
                result := SELF:ParseFoxClass()
            ELSE
                IF _dialect == XDialect.XPP .AND. SELF:HasXppEndClass()
                    result := SELF:ParseXppClass()
                ELSE
                    result := SELF:ParseTypeDef()
                ENDIF
            ENDIF
        CASE Kind.Structure
            result := SELF:ParseTypeDef()
        CASE Kind.Interface
            result := SELF:ParseTypeDef()
        CASE Kind.Delegate
            result := SELF:ParseDelegate()
        CASE Kind.Access
            IF InXppClass .AND. _dialect == XDialect.XPP
                result := SELF:ParseXppProperty()
            ELSE
                result := SELF:ParseMethod()
            ENDIF
        CASE Kind.Assign
            IF InXppClass .AND. _dialect == XDialect.XPP
                result := SELF:ParseXppProperty()
            ELSE
                result := SELF:ParseMethod()
            ENDIF
        CASE Kind.Method
            IF SELF:_dialect == XDialect.XPP
                result := SELF:ParseXppMethod()
            ELSE
                result := SELF:ParseMethod()
            ENDIF
        CASE Kind.Function
        CASE Kind.Procedure
            IF InFoxClass .AND. _dialect == XDialect.FoxPro
                result := SELF:ParseFoxMethod()
            ELSE
                result := SELF:ParseFuncProc()
            ENDIF
        CASE Kind.VODLL
            result := SELF:ParseVODLL()
        CASE Kind.VOStruct
            result := SELF:ParseVoStruct()
        CASE Kind.Union
            result := SELF:ParseVOUnion()
        CASE Kind.Enum
            result := SELF:ParseEnum()
        CASE Kind.EnumMember
            IF SELF:CurrentEntity:Kind == Kind.Enum
                result := SELF:ParseEnumMember()
            ELSE
                result := SELF:ParseVoStructMember()
            ENDIF
        CASE Kind.VODefine
            result := SELF:ParseVoDefine()
        CASE Kind.VOGlobal
            result := SELF:ParseVOGlobals()
        CASE Kind.Property
            result := SELF:ParseProperty()

        CASE Kind.Event
            result := SELF:ParseEvent()

        CASE Kind.Operator
            result := SELF:ParseOperator()

        CASE Kind.Constructor
            result := SELF:ParseConstructor()

        CASE Kind.Destructor
            result := SELF:ParseDestructor()

        CASE Kind.Field
            IF InXppClass .AND. _dialect == XDialect.XPP
                IF SELF:Matches(XSharpLexer.COLON)
                    result := SELF:ParseXppVisibility()
                ELSE
                    result := SELF:ParseXppClassVars()
                ENDIF
            ELSEIF InFoxClass .AND. _dialect == XDialect.FoxPro
                result := SELF:ParseFoxFields()
            ELSE
                result := SELF:ParseClassVars()
            ENDIF
        CASE Kind.LocalFunc
        CASE Kind.LocalProc
            result := SELF:ParseLocalFuncProc()
        END SWITCH
        RETURN result


    PRIVATE METHOD HasXppEndClass() AS LOGIC
        LOCAL iCurrent := 1 AS LONG
        LOCAL iLa AS LONG
        iLa := SELF:La(iCurrent)
        DO WHILE iLa != -1
            IF iLa == XSharpLexer.ENDCLASS
                RETURN TRUE
            ELSEIF iLa == XSharpLexer.INLINE // Does not occurs in normal class definition
                RETURN TRUE
            ELSEIF XSharpLexer.IsModifier(iLa) .AND. SELF:La(iCurrent+1) == XSharpLexer.COLON
                // visibility clause
                RETURN TRUE
            ELSEIF iLa == XSharpLexer.END .AND. SELF:La(iCurrent+1) == XSharpLexer.CLASS
                RETURN FALSE
            ENDIF
            iCurrent++
            iLa := SELF:La(iCurrent)
        ENDDO
        RETURN FALSE

#region Main entities

    PRIVATE METHOD ParseFuncProc() AS IList<XSourceEntity>
        /*
        funcproc      : (Attributes=attributes)? (Modifiers=funcprocModifiers)?
        T=funcproctype Sig=signature
        InitExit=(INIT1|INIT2|INIT3|EXIT)?
        vodummyclauses
        end=eos
        StmtBlk=statementBlock
        (END T2=funcproctype   EOS )?
        ;
        funcproctype        : Token=(FUNCTION | PROCEDURE)
        ;
        // also
        (INIT | EXIT) PROCEDURE ...

        */
        LOCAL kind AS Kind
        VAR initexit := ""
        _modifiers:Add(SELF:Lt1)
        IF SELF:Matches(XSharpLexer.INIT,XSharpLexer.EXIT)
            initexit := SELF:ConsumeAndGetText()
        ENDIF
        IF ! SELF:ParseFuncProcType (OUT kind)
            RETURN NULL
        ENDIF
        SELF:Consume()
        VAR sig := SELF:ParseSignature()

        IF initexit:Length == 0 .and. SELF:Matches(XSharpLexer.INIT1,XSharpLexer.INIT2,XSharpLexer.INIT3,XSharpLexer.EXIT)
            initexit := SELF:ConsumeAndGetText()
        ENDIF
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()

        VAR xMember := XSourceMemberSymbol{sig, kind, _attributes, range, interval, _modifiers,_attributes:HasFlag(Modifiers.Static)}
        xMember:SourceCode := source
        xMember:File := SELF:_file
        RETURN <XSourceEntity>{xMember}

    PRIVATE METHOD ParseNamespace() AS IList<XSourceEntity>
        /*
        namespace_          : BEGIN NAMESPACE Name=name e=eos
        (Entities+=entity)*
        END NAMESPACE EOS
        ;
        */
        IF SELF:La1 != XSharpLexer.BEGIN .AND. SELF:La2 != XSharpLexer.NAMESPACE
            RETURN NULL
        ENDIF
        _modifiers:Add(SELF:Lt1)
        SELF:Consume()   // BEGIN
        _modifiers:Add(SELF:Lt1)
        SELF:Consume()   // NAMESPACE
        VAR id := SELF:ParseQualifiedName()
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xType := XSourceNamespaceSymbol{id, range, interval,_file,_modifiers}
        xType:SourceCode := source
        RETURN <XSourceEntity>{xType}

    PRIVATE METHOD AddAsChild(xType as XSourceTypeSymbol) AS VOID
        IF CurrentEntity != _globalType .AND. CurrentEntityKind:HasChildren()
            xType:Parent := SELF:CurrentEntity
        ENDIF
    PRIVATE METHOD ParseTypeDef() AS IList<XSourceEntity>
        /*
        interface_          : (Attributes=attributes)? (Modifiers=classModifiers)?
        I=INTERFACE (Namespace=nameDot)? Id=identifier
        TypeParameters=typeparameters?                                      // TypeParameters indicate Generic Interface
        ((INHERIT|COLON) Parents+=datatype)? (COMMA Parents+=datatype)*
        (ConstraintsClauses+=typeparameterconstraintsclause)*              // Optional typeparameterconstraints for Generic Interface
        e=eos
        (Members+=classmember)*
        END INTERFACE EOS
        ;
        class_              : (Attributes=attributes)? (Modifiers=classModifiers)?
        C=CLASS (Namespace=nameDot)? Id=identifier
        TypeParameters=typeparameters?                                    // TypeParameters indicate Generic Class
        (INHERIT BaseType=datatype)?
        (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)?
        (ConstraintsClauses+=typeparameterconstraintsclause)*             // Optional typeparameterconstraints for Generic Class
        e=eos
        (Members+=classmember)*
        END CLASS EOS
        ;
        structure_          : (Attributes=attributes)? (Modifiers=classModifiers)?
        S=STRUCTURE (Namespace=nameDot)? Id=identifier
        TypeParameters=typeparameters?
        (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)?
        (ConstraintsClauses+=typeparameterconstraintsclause)* e=eos
        (Members+=classmember)*
        END STRUCTURE EOS
        ;


        */
        LOCAL kind AS Kind
        SWITCH SELF:La1
        CASE XSharpLexer.CLASS
            kind := Kind.Class
        CASE XSharpLexer.INTERFACE
            kind := Kind.Interface
        CASE XSharpLexer.STRUCTURE
            kind := Kind.Structure
        OTHERWISE
            RETURN NULL
        END SWITCH
        _modifiers:Add(SELF:Lt1)
        SELF:Consume()

        LOCAL constraints   AS List<STRING>
        LOCAL parentType    AS STRING
        LOCAL interfaces    AS List<STRING>
        // read Id with optional namespace prefix
        VAR id := SELF:ParseQualifiedName()
        VAR typePars := SELF:ParseTypeParameters()
        // get inherit clause
        IF SELF:ExpectAny(XSharpLexer.INHERIT, XSharpLexer.COLON)
            parentType := SELF:ParseTypeName()
        ENDIF
        // get implements clause + list of interfaces
        IF SELF:Matches(XSharpLexer.IMPLEMENTS)
            interfaces := List<STRING>{}
            DO WHILE SELF:ExpectAny(XSharpLexer.IMPLEMENTS, XSharpLexer.COMMA)
                interfaces:Add(SELF:ParseTypeName())
            ENDDO
        ENDIF
        DO WHILE SELF:Matches(XSharpLexer.WHERE) .AND. ! SELF:Eos()
            IF constraints == NULL
                constraints := List<STRING>{}
            ENDIF
            constraints:Add(SELF:ParseTypeParameterConstraints())
        ENDDO
        // read to EndOfLine
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        if typePars != null && typePars:Count > 0
            id += "`"+typePars:Count:ToString()
        endif
        VAR xType := XSourceTypeSymbol{id, kind, _attributes, range, interval, _file, _modifiers}
        xType:SourceCode := source
        IF interfaces?:Count > 0
            FOREACH VAR sInterface IN interfaces
                xType:AddInterface(sInterface)
            NEXT
        ENDIF
        IF ! String.IsNullOrEmpty(parentType)
            xType:BaseTypeName := parentType
        ENDIF
        xType:IsPartial := _attributes:HasFlag(Modifiers.Partial)
        IF constraints?:Count > 0
            FOREACH VAR constraint IN constraints
                xType:AddConstraints(constraint)
            NEXT
        ENDIF
        IF typePars?:Count > 0
            FOREACH VAR typepar IN typePars
                xType:AddTypeParameter(typepar)
            NEXT
        ENDIF
        SELF:AddAsChild(xType)
        RETURN <XSourceEntity>{xType}

    PRIVATE METHOD ParseDelegate() AS IList<XSourceEntity>
        /*
        delegate_           : (Attributes=attributes)? (Modifiers=classModifiers)?
        D=DELEGATE (Namespace=nameDot)? Id=identifier
        TypeParameters=typeparameters?
        ParamList=parameterList?
        (AS Type=datatype)?
        (ConstraintsClauses+=typeparameterconstraintsclause)*
        e=EOS
        ;


        */
        IF ! SELF:Expect(XSharpLexer.DELEGATE)
            RETURN NULL
        ENDIF

        VAR sig      := SELF:ParseSignature()
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        VAR xType    := XSourceTypeSymbol{sig:Id, Kind.Delegate,_attributes, range, interval,_file, _modifiers}{SingleLine := TRUE}
        xType:SourceCode := source
        xType:TypeName := sig:DataType
        xType:File     := _file
        SELF:AddAsChild(xType)
        VAR xMember  := XSourceMemberSymbol{sig, Kind.Delegate, _attributes, ;
            range, interval,_modifiers, _attributes:HasFlag(Modifiers.Static)} {SingleLine := TRUE}
        xMember:SourceCode := source
        xMember:Name := "Invoke"
        xType:AddMember(xMember)
        xMember:File       := _file
        RETURN <XSourceEntity>{xType}
#endregion

#region ClassMembers
    PRIVATE METHOD ParseMethod(  ) AS IList<XSourceEntity>
        /*
        // method rule used inside and outside class members rule
        method              : (Attributes=attributes)? (Modifiers=memberModifiers)?
        T=methodtype (ExplicitIface=nameDot)? Sig=signature
        (CLASS ClassId=identifier)?      // Class Clause needed when entity and allowed when class member
        vodummyclauses
        end=eos
        StmtBlk=statementBlock
        (END T2=methodtype EOS)?
        ;

        methodtype          : Token=(METHOD | ACCESS | ASSIGN )
        ;

        */
        LOCAL kind AS Kind
        SWITCH La1
        CASE XSharpLexer.ACCESS
            kind := Kind.Access
        CASE XSharpLexer.ASSIGN
            kind := Kind.Assign
        CASE XSharpLexer.METHOD
            kind := Kind.Method
        OTHERWISE
            RETURN NULL
        END SWITCH
        _modifiers:Add(SELF:Lt1)
        SELF:Consume()

        VAR sig := SELF:ParseSignature()
        VAR classClause := SELF:ParseOptionalClassClause()
        // read to EndOfLine
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()

        VAR xMember := XSourceMemberSymbol{sig, kind, _attributes, range, interval, _modifiers, _attributes:HasFlag(Modifiers.Static)}
        IF SELF:CurrentType != NULL
            SELF:CurrentType:AddMember(xMember)
        ENDIF
        xMember:SourceCode := source
        RETURN <XSourceEntity>{xMember}

    PRIVATE METHOD ParseProperty() AS IList<XSourceEntity>
        _modifiers:Add(SELF:Lt1)
        IF !  SELF:Expect(XSharpLexer.PROPERTY)
            RETURN NULL
        ENDIF
        // Note that inside the editor the SET and GET accessors are parsed as blocks and not as entities
        /*
        property            : (Attributes=attributes)? (Modifiers=memberModifiers)?
        P=PROPERTY (SELF ParamList=propertyParameterList | (ExplicitIface=nameDot)? Id=identifier)
        (ParamList=propertyParameterList)?
        (AS Type=datatype)?
        ( Auto=AUTO (AutoAccessors+=propertyAutoAccessor)* (Op=assignoperator Initializer=expression)? end=EOS	// Auto
        | (LineAccessors+=propertyLineAccessor)+ end=EOS													// Single Line
        | Multi=eos (Accessors+=propertyAccessor)+  END PROPERTY? EOS				// Multi Line
        )
        ;

        propertyParameterList
        : LBRKT  (Params+=parameter (COMMA Params+=parameter)*)? RBRKT
        | LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN		// Allow Parentheses as well
        ;

        propertyAutoAccessor: Attributes=attributes? Modifiers=accessorModifiers? Key=(GET|SET)
        ;

        propertyLineAccessor: Attributes=attributes? Modifiers=accessorModifiers?
        ( {InputStream.La(2) != SET}? Key=GET Expr=expression?
        | {InputStream.La(2) != SET}? Key=UDCSEP Expr=expression?           // New: UDCSep instead of GET
        | {InputStream.La(2) != GET}? Key=SET ExprList=expressionList?
        | Key=(GET|SET) )
        ;
        expressionList	    : Exprs+=expression (COMMA Exprs+=expression)*
        ;

        propertyAccessor    : Attributes=attributes? Modifiers=accessorModifiers?
        ( Key=GET end=eos StmtBlk=statementBlock END GET?
        | Key=GET UDCSEP ExpressionBody=expression              // New: Expression Body
        | Key=SET end=eos StmtBlk=statementBlock END SET? )
        | Key=SET UDCSEP ExpressionBody=expression              // New: Expression Body
        end=eos
        ;

        */
        VAR id := ""
        IF SELF:Matches(XSharpLexer.SELF)   // Self property
            id := SELF:ConsumeAndGetText()
        ELSE
            id := SELF:ParseQualifiedName()
        ENDIF
        VAR aParams := List<XSourceParameterSymbol>{}
        IF SELF:Matches(XSharpLexer.LPAREN)
            aParams := SELF:ParseParameterList( FALSE, OUT VAR _)
        ELSEIF SELF:Matches(XSharpLexer.LBRKT)
            aParams := SELF:ParseParameterList(TRUE, OUT VAR _)
        ENDIF
        VAR sType := SELF:ParseDataType(FALSE)
        LOCAL lSingleLine AS LOGIC
        DO WHILE ! SELF:Eos()
            IF SELF:Matches(XSharpLexer.AUTO, XSharpLexer.GET,XSharpLexer.SET, XSharpLexer.UDCSEP)
                lSingleLine := TRUE
            ENDIF
            SELF:Consume()
        ENDDO
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xMember := XSourceMemberSymbol{id, Kind.Property, _attributes, range, interval,sType,_modifiers} {SingleLine := lSingleLine}
        xMember:SourceCode := source
        IF SELF:CurrentType != NULL
            CurrentType:AddMember(xMember)
        ENDIF
        xMember:AddParameters(aParams)
        RETURN <XSourceEntity>{xMember}

     PRIVATE METHOD ParseEvent() AS IList<XSourceEntity>
        _modifiers:Add(SELF:Lt1)
        IF ! SELF:Expect(XSharpLexer.EVENT)
            RETURN NULL
        ENDIF

        // Inside the editor the ADD and REMOVE accessors are parsed as blocks and not as entities
        /*
        event_              : (Attributes=attributes)? (Modifiers=memberModifiers)?
        E=EVENT (ExplicitIface=nameDot)? Id=identifier (AS Type=datatype)?
        ( end=EOS
        | (LineAccessors += eventLineAccessor)+ end=EOS
        | Multi=eos (Accessors+=eventAccessor)+ END EVENT? EOS
        )
        ;

        eventLineAccessor   : Attributes=attributes? Modifiers=accessorModifiers?
        ( {InputStream.La(2) != REMOVE}? Key=ADD ExprList=expressionList?
        | {InputStream.La(2) != ADD}?    Key=REMOVE ExprList=expressionList?
        | Key=(ADD|REMOVE) )
        ;
        eventAccessor       : Attributes=attributes? Modifiers=accessorModifiers?
        ( Key=ADD     END=eos StmtBlk=statementBlock END ADD?
        | Key=ADD     UDCSEP ExpressionBody=expression              // New: Expression Body
        | Key=REMOVE  END=eos StmtBlk=statementBlock END REMOVE? )
        | Key=REMOVE  UDCSEP ExpressionBody=expression              // New: Expression Body
        end=eos
        ;
        */
        VAR id := SELF:ParseQualifiedName()
        VAR strType  := SELF:ParseDataType(FALSE)
        LOCAL lSingleLine AS LOGIC
        DO WHILE ! SELF:Eos()
            IF SELF:Matches( XSharpLexer.ADD,XSharpLexer.REMOVE)
                lSingleLine := TRUE
            ENDIF
            SELF:Consume()
        ENDDO
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xMember := XSourceMemberSymbol{id, Kind.Event, _attributes, range, interval,strType, _modifiers}  {SingleLine := lSingleLine}
        xMember:SourceCode := source
        IF SELF:CurrentType != NULL
            CurrentType:AddMember(xMember)
        ENDIF
        RETURN <XSourceEntity>{xMember}


    PRIVATE METHOD IsOperatorToken(token AS INT) AS LOGIC
        /*
        overloadedOps       : Token= (PLUS | MINUS | NOT | TILDE | INC | DEC | TRUE_CONST | FALSE_CONST |
        MULT | DIV | MOD | AMP | PIPE | LSHIFT | RSHIFT | EEQ | NEQ | NEQ2 |
        GT | LT | GTE | LTE |
        AND | OR )  // these two do not exist in C# and are mapped to & and |
        ;

        conversionOps		: Token=( IMPLICIT | EXPLICIT )
        ;
        */
        SWITCH token
        CASE XSharpLexer.PLUS
        CASE XSharpLexer.MINUS
        CASE XSharpLexer.NOT
        CASE XSharpLexer.TILDE
        CASE XSharpLexer.INC
        CASE XSharpLexer.DEC
        CASE XSharpLexer.TRUE_CONST
        CASE XSharpLexer.FALSE_CONST
        CASE XSharpLexer.MULT
        CASE XSharpLexer.DIV
        CASE XSharpLexer.MOD
        CASE XSharpLexer.AMP
        CASE XSharpLexer.PIPE
        CASE XSharpLexer.LSHIFT
        CASE XSharpLexer.RSHIFT
        CASE XSharpLexer.EEQ
        CASE XSharpLexer.NEQ
        CASE XSharpLexer.NEQ2
        CASE XSharpLexer.GT
        CASE XSharpLexer.LT
        CASE XSharpLexer.GTE
        CASE XSharpLexer.LTE
        CASE XSharpLexer.AND
        CASE XSharpLexer.OR
        CASE XSharpLexer.IMPLICIT
        CASE XSharpLexer.EXPLICIT
            RETURN TRUE
        END SWITCH
        RETURN FALSE


    PRIVATE METHOD ParseOperator() AS IList<XSourceEntity>
        _modifiers:Add(SELF:Lt1)
        IF ! SELF:Expect(XSharpLexer.OPERATOR)
            RETURN NULL
        ENDIF

        /*
        operator_           : Attributes=attributes? Modifiers=operatorModifiers?
        o1=OPERATOR (Operation=overloadedOps | Conversion=conversionOps) Gt=GT?
        ParamList=parameterList
        (AS Type=datatype)?
        (UDCSEP ExpressionBody=expression)?             // New: Expression Body
        end=eos
        StmtBlk=statementBlock
        (END o1=OPERATOR EOS)?
        ;

        */
        LOCAL t1 AS IToken
        LOCAL t2 AS IToken

        IF SELF:IsOperatorToken(SELF:La1)
            t1 := SELF:ConsumeAndGet()
        ENDIF
        IF SELF:Matches(XSharpLexer.GT)
            t2 := SELF:ConsumeAndGet()
        ENDIF
        VAR aParams     := SELF:ParseParameterList(FALSE, OUT VAR _)
        VAR sType       := SELF:ParseDataType(FALSE)
        SELF:ParseExpressionBody()
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        local id as string
        if t1 != null
            id := t1:GetText()+ IIF(t2 != NULL, t2:GetText(),"")
        else
            id := ""
        endif
        SELF:ReadLine()
        VAR xMember := XSourceMemberSymbol{id, Kind.Operator, _attributes, range, interval,sType, _modifiers}
        xMember:SourceCode := source
        IF SELF:CurrentType != NULL
            CurrentType:AddMember(xMember)
        ENDIF
        xMember:AddParameters(aParams)
        RETURN <XSourceEntity>{xMember}


    PRIVATE METHOD ParseConstructor() AS IList<XSourceEntity>

        /*constructor         :  (Attributes=attributes)? (Modifiers=constructorModifiers)?
        c1=CONSTRUCTOR (ParamList=parameterList)? (AS VOID)? // As Void is allowed but ignored
        (CallingConvention=callingconvention)?
        (CLASS ClassId=identifier)?
        (UDCSEP ExpressionBody=expression)?               // New: Expression Body
        end=eos
        (Chain=constructorchain)?
        StmtBlk=statementBlock
        (END c2=CONSTRUCTOR EOS)?
        ;
        */
        _modifiers:Add(SELF:Lt1)
        IF ! SELF:Expect(XSharpLexer.CONSTRUCTOR)
            RETURN NULL
        ENDIF
        VAR id  := XLiterals.ConstructorName
        VAR aParams     := SELF:ParseParameterList(FALSE, OUT VAR _)
        VAR asType      := SELF:ParseDataType(FALSE)
        VAR callconv    := SELF:ParseCallingConvention()
        VAR classClause := SELF:ParseOptionalClassClause()
        SELF:ParseExpressionBody()
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xMember := XSourceMemberSymbol{id, Kind.Constructor, _attributes, range, interval,"", _modifiers}
        xMember:SourceCode := source
        xMember:AddParameters(aParams)
        IF SELF:CurrentType != NULL
            CurrentType:AddMember(xMember)
        ENDIF

        RETURN <XSourceEntity>{xMember}

    PRIVATE METHOD ParseDestructor() AS IList<XSourceEntity>
        /*
        destructor          : (Attributes=attributes)? (Modifiers=destructorModifiers)?
        d1=DESTRUCTOR (LPAREN RPAREN)?
        (CLASS ClassId=identifier)?
        (UDCSEP ExpressionBody=expression)?               // New: Expression Body
        end=eos
        StmtBlk=statementBlock
        (END d2=DESTRUCTOR EOS)?
        ;

        */
        _modifiers:Add(SELF:Lt1)
        IF ! SELF:Expect(XSharpLexer.DESTRUCTOR)
            RETURN NULL
        ENDIF

        VAR id  := XLiterals.DestructorName
        IF SELF:La1 == XSharpLexer.LPAREN .AND. SELF:La2 == XSharpLexer.RPAREN
            SELF:Consume()
            SELF:Consume()
        ENDIF
        VAR classClause := SELF:ParseOptionalClassClause()
        SELF:ParseExpressionBody()
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xMember := XSourceMemberSymbol{id, Kind.Destructor, _attributes, range, interval,"VOID", _modifiers}
        xMember:SourceCode := source
        IF SELF:CurrentType != NULL
            SELF:CurrentType:AddMember(xMember)
        ENDIF
        RETURN <XSourceEntity>{xMember}

    PRIVATE METHOD ParseClassVars() AS IList<XSourceEntity>
        /*
        classvars           : (Attributes=attributes)? Modifiers=classvarModifiers
        Vars+=classvar (COMMA Vars+=classvar)*
        eos
        ;
        */

        VAR result := List<XSourceEntity>{}
        VAR classVars := List<XSourceMemberSymbol>{}
        VAR classVar := SELF:ParseClassVar(Kind.Field)
        classVar:SourceCode := classVar:ModVis +" "+classVar:SourceCode
        classVars:Add(classVar)
        DO WHILE SELF:Expect(XSharpLexer.COMMA)
            classVar := SELF:ParseClassVar(Kind.Field)
            classVar:SourceCode := classVar:ModVis +" "+classVar:SourceCode
            classVars:Add(classVar)
        ENDDO
        SELF:ReadLine()
        SELF:CopyClassVarTypes(classVars)
        result:AddRange(classVars)
        RETURN result
#endregion

    PRIVATE METHOD CopyClassVarTypes(classVars AS List<XSourceMemberSymbol>)  AS VOID
        IF classVars:Count > 1
            VAR type := ""
            classVars:Reverse()
            FOREACH VAR classVar IN classVars
                IF !classVar:IsTyped
                    classVar:ReturnType := type
                    classVar:SourceCode += " AS "+type
                ELSE
                    type := classVar:ReturnType
                ENDIF
            NEXT
            classVars:Reverse()
        ENDIF
        RETURN

#region Enum
    PRIVATE METHOD ParseEnum() AS IList<XSourceEntity>
        /*
        enum_               : (Attributes=attributes)? (Modifiers=classModifiers)?
        E=ENUM (Namespace=nameDot)? Id=identifier ((AS|INHERIT) Type=datatype)? e=eos
        (Members+=enummember)+
        END ENUM? EOS
        ;
        */
        _modifiers:Add(SELF:Lt1)
        IF ! SELF:Expect(XSharpLexer.ENUM)
            RETURN NULL
        ENDIF

        VAR id := SELF:ParseQualifiedName()
        VAR type := ""
        IF SELF:Matches(XSharpLexer.AS)
            type := SELF:ParseDataType(FALSE)
        ELSEIF SELF:Expect(XSharpLexer.INHERIT)
            type := SELF:ParseTypeName()
        ENDIF
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xType := XSourceTypeSymbol{id, Kind.Enum, _attributes, range, interval, _file, _modifiers} {BaseTypeName := type}
        xType:SourceCode := source
        SELF:AddAsChild(xType)
        RETURN <XSourceEntity>{xType}

    PRIVATE METHOD ParseEnumMember() AS IList<XSourceEntity>
        /*
        enummember          : (Attributes=attributes)? MEMBER? Id=identifier (Op=assignoperator Expr=expression)? eos
        ;
        */

        VAR att := SELF:ParseAttributes()
        SELF:Expect(XSharpLexer.MEMBER)    // Optional !
        VAR strValue := ""
        VAR id := SELF:ParseQualifiedName()
        IF SELF:ExpectAssignOp()
            strValue := SELF:ParseExpression()
        ENDIF
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR type := CurrentType?:Name
        VAR xMember := XSourceMemberSymbol{id, Kind.EnumMember, _attributes, range, interval, type, _modifiers} {SingleLine := TRUE, @@Value := strValue}
        xMember:File := SELF:_file
        xMember:SourceCode := source
        xMember:IsStatic := TRUE
        RETURN <XSourceEntity>{xMember}

#endregion

#region VO Global, Define, Struct, Union
    PRIVATE METHOD ParseVOGlobals() AS IList<XSourceEntity>
        /*
        // [STATIC] GLOBAL [CONST] Identifier [:= Expression] [, Identifier2 [:= Expression2] ] [AS Type]
        // STATIC          [CONST] Identifier [:= Expression] [, Identifier2 [:= Expression2] ] [AS Type]
        voglobal            : (Attributes=attributes)? (Modifiers=funcprocModifiers)? Global=GLOBAL (Const=CONST)? Vars=classVarList end=EOS
        | (Attributes=attributes)? Static=STATIC (Const=CONST)? Vars=classVarList end=EOS
        ;
        */
        IF ! SELF:Expect(XSharpLexer.GLOBAL)
            RETURN NULL
        ENDIF
        var lConst := SELF:Expect(XSharpLexer.CONST)
        VAR result := List<XSourceEntity>{}
        VAR classVars := List<XSourceMemberSymbol>{}
        VAR classVar := SELF:ParseClassVar(Kind.VOGlobal)
        classVar:SourceCode := classVar:ModVis+" GLOBAL "+ classVar:SourceCode
        classVars:Add(classVar)
        DO WHILE SELF:Expect(XSharpLexer.COMMA)
            classVar := SELF:ParseClassVar(Kind.VOGlobal)
            classVar:SourceCode := classVar:ModVis+" GLOBAL "+ classVar:SourceCode
            classVars:Add(classVar)
            if lConst
                classVar:Attributes |= Modifiers.Const
            endif
        ENDDO
        SELF:ReadLine()
        SELF:CopyClassVarTypes(classVars)
        result:AddRange(classVars)
        RETURN result

    PRIVATE METHOD ParseVoDefine() AS IList<XSourceEntity>
        /*
        vodefine            : (Modifiers=funcprocModifiers)?
        D=DEFINE Id=identifier Op=assignoperator Expr=expression (AS DataType=typeName)? end=EOS
        ;

        */
        IF ! SELF:Expect(XSharpLexer.DEFINE)
            RETURN NULL
        ENDIF

        VAR id := SELF:ParseQualifiedName()
        LOCAL strValue AS STRING
        IF SELF:ExpectAssignOp()
            strValue := SELF:ParseExpression()
        ENDIF
        VAR type := SELF:ParseDataType(FALSE)
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xMember := XSourceMemberSymbol{id, Kind.VODefine, _attributes, range,interval, type, _modifiers} {SingleLine := TRUE, @@Value := strValue}
        xMember:SourceCode := source
        xMember:File := SELF:_file
        RETURN <XSourceEntity>{xMember}

    PRIVATE METHOD ParseVoStruct() AS IList<XSourceEntity>
        /*
        vostruct            : (Modifiers=votypeModifiers)?
        V=VOSTRUCT (Namespace=nameDot)? Id=identifier (ALIGN Alignment=INT_CONST)? e=eos
        (Members+=vostructmember)+
        (END VOSTRUCT EOS)?
        ;
        */
        IF ! SELF:Expect(XSharpLexer.VOSTRUCT)
            RETURN NULL
        ENDIF

        VAR id := SELF:ParseQualifiedName()
        VAR sAlign := ""
        IF SELF:Expect(XSharpLexer.ALIGN)
            IF SELF:ExpectAndGet(XSharpLexer.INT_CONST, OUT VAR t2)
                sAlign := t2:GetText() // Align number
            ENDIF
        ENDIF
        // read to EndOfLine
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xType := XSourceTypeSymbol{id, Kind.VOStruct, _attributes, range, interval,_file, _modifiers}
        xType:SourceCode := source
        IF String.IsNullOrEmpty(sAlign)
            xType:AddInterface(sAlign)
        ENDIF
        SELF:AddAsChild(xType)
        RETURN <XSourceEntity>{xType}

    PRIVATE METHOD ParseVOUnion() AS IList<XSourceEntity>
        /*
        vounion             : (Modifiers=votypeModifiers)?
        U=UNION (Namespace=nameDot)? Id=identifier e=eos
        (Members+=vostructmember)+
        (END UNION EOS)?
        ;
        */
        IF ! SELF:Expect(XSharpLexer.UNION)
            RETURN NULL
        ENDIF

        VAR id := SELF:ParseQualifiedName()
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xType := XSourceTypeSymbol{id, Kind.Union, _attributes, range, interval,_file, _modifiers}
        xType:SourceCode := source
        SELF:AddAsChild(xType)
        RETURN <XSourceEntity>{xType}

    PRIVATE METHOD ParseVODLL() AS IList<XSourceEntity>
        /*

        vodll               : (Attributes=attributes)? (Modifiers=funcprocModifiers)? // Optional
        D=DLL T=funcproctype Id=identifier ParamList=parameterList (AS Type=datatype)?
        (CallingConvention=dllcallconv)? COLON
        Dll=identifierString (DOT Extension=identifierString)?
        (	Ordinal=REAL_CONST
        |  DOT Entrypoint=identifierString Address=ADDROF? Number=INT_CONST? (NEQ2 INT_CONST)?
        )

        ( CharSet=(AUTO | ID) )?  // ID must be either ANSI or UNICODE
        EOS
        ;

        */
        IF ! SELF:Expect(XSharpLexer.DLL)
            RETURN NULL
        ENDIF
        LOCAL kind AS Kind
        IF ! SELF:ParseFuncProcType (OUT kind)
            RETURN NULL
        ENDIF
        SELF:Consume()
        VAR sig     := SELF:ParseSignature()
        VAR colon   := SELF:ConsumeAndGetText()
        VAR dllName := SELF:ConsumeAndGetText()
        VAR dotName := ""
        IF SELF:Expect(XSharpLexer.DOT)
            dotName := SELF:ConsumeAndGetText()
        ENDIF
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xMember := XSourceMemberSymbol{sig, Kind.VODLL, _attributes, range, interval, _modifiers, _attributes:HasFlag(Modifiers.Static)} {SubType := kind }
        xMember:File := SELF:_file
        xMember:SourceCode := source
        RETURN <XSourceEntity>{xMember}


    PRIVATE METHOD ParseVoStructMember() AS IList<XSourceEntity>
        /*
        vostructmember      : MEMBER Dim=DIM Id=identifier LBRKT ArraySub=arraysub RBRKT (As=(AS | IS) DataType=datatype)? eos
        | MEMBER Id=identifier (As=(AS | IS) DataType=datatype)? eos
        ;
        */
        IF ! SELF:Expect(XSharpLexer.MEMBER)
            RETURN NULL
        ENDIF

        LOCAL sBracket AS STRING
        VAR isDim  := SELF:Expect(XSharpLexer.DIM)
        VAR id := SELF:ParseQualifiedName()

        IF isDim .or. SELF:Matches(XSharpLexer.LBRKT)
            sBracket := SELF:ParseArraySub()
        ENDIF
        VAR sType := SELF:ParseDataType(TRUE)
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        IF isDim
            sType += "[]"
        ENDIF
        VAR xMember := XSourceMemberSymbol{id, Kind.Field, _attributes, range, interval, sType, _modifiers} {SingleLine := TRUE, IsArray := isDim}
        xMember:File := SELF:_file
        xMember:SourceCode := source
        RETURN <XSourceEntity>{xMember}

#endregion



    PRIVATE METHOD ParseClassVar(eKind AS Kind) AS XSourceMemberSymbol
        /*

        classvar            : (DIM=DIM)? Id=identifier (LBRKT ArraySub=arraysub RBRKT)?
        (Op=assignoperator Initializer=expression)?
        (As=(AS | IS) DataType=datatype)?
        ;
        */
        LOCAL sDefault   AS STRING
        LOCAL startToken AS IToken
        LOCAL endToken   AS IToken
        startToken := SELF:Lt1
        VAR isDim       := SELF:Expect(XSharpLexer.DIM)
        VAR sId         := SELF:ParseIdentifier()
        VAR sBracket  := SELF:ParseArraySub()
        IF SELF:ExpectAssignOp()
            sDefault := SELF:ParseExpression()
        ENDIF
        VAR sType := SELF:ParseDataType(TRUE)
        endToken := SELF:LastToken
        SELF:GetSourceInfo(startToken, endToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        VAR xMember   := XSourceMemberSymbol{sId,eKind, SELF:_attributes, range,interval,sType, _modifiers} {SingleLine := TRUE}
        xMember:ReturnType := sType
        xMember:SourceCode := source
        xMember:Value := sDefault
        IF isDim .OR. !String.IsNullOrEmpty(sBracket)
            xMember:IsArray := TRUE
        ENDIF
        RETURN xMember


    PRIVATE METHOD ParseArraySub AS STRING
        /*
        arraysub            : ArrayIndex+=expression (RBRKT LBRKT ArrayIndex+=expression)+		// x][y
        | ArrayIndex+=expression (COMMA ArrayIndex+=expression)+			// x,y
        | ArrayIndex+=expression
        ;
        */
        LOCAL sBracket:= "" AS STRING
        DO WHILE SELF:La1 == XSharpLexer.LBRKT           // allow SomeId[1][2]
            sBracket += SELF:ConsumeAndGetText()
            DO WHILE SELF:La1 != XSharpLexer.RBRKT .AND. ! SELF:Eos()
                sBracket += SELF:ConsumeAndGetText()
            ENDDO
            IF SELF:La1 == XSharpLexer.RBRKT
                sBracket += SELF:ConsumeAndGetText()
            ENDIF
        ENDDO
        RETURN sBracket

    PRIVATE METHOD ParseArraySubFox AS STRING
        /*
        arraysub            : ArrayIndex+=expression (RPAREN LPAREN ArrayIndex+=expression)+		// x)(y
        | ArrayIndex+=expression (COMMA ArrayIndex+=expression)+			// x,y
        | ArrayIndex+=expression
        ;
        */
        LOCAL sBracket:= "" AS STRING
        DO WHILE SELF:La1 == XSharpLexer.LPAREN           // allow SomeId(1)(2)
            sBracket += SELF:ConsumeAndGetText()
            DO WHILE SELF:La1 != XSharpLexer.RPAREN .AND. ! SELF:Eos()
                sBracket += SELF:ConsumeAndGetText()
            ENDDO
            IF SELF:La1 == XSharpLexer.RPAREN
                sBracket += SELF:ConsumeAndGetText()
            ENDIF
        ENDDO
        RETURN sBracket



    PRIVATE METHOD ParseSignature() AS XMemberSignature
        /*
        signature             : Id=identifier
        TypeParameters=typeparameters?
        (ParamList=parameterList)?
        (AS Type=datatype)?
        (ConstraintsClauses+=typeparameterconstraintsclause)*
        (CallingConvention=callingconvention)?
        (UDCSEP ExpressionBody=expression)?
        ;


        */
        LOCAL oSig          AS XMemberSignature
        oSig                := XMemberSignature{}
        oSig:Id             := SELF:ParseQualifiedName()
        IF SELF:Matches(XSharpLexer.LT)
            oSig:TypeParameters := SELF:ParseTypeParameters()
        ENDIF
        IF SELF:Matches(XSharpLexer.LPAREN)
            var params        := SELF:ParseParameterList(FALSE,  OUT VAR isSelf)
            oSig:Parameters:AddRange(params)
            oSig:IsExtension  := isSelf
        ENDIF
        oSig:DataType       := SELF:ParseDataType(FALSE)
        DO WHILE SELF:La1 == XSharpLexer.WHERE .AND. ! SELF:Eos()
            oSig:TypeParameterContraints:Add(SELF:ParseTypeParameterConstraints())
        ENDDO
        oSig:CallingConvention  := SELF:ParseCallingConvention()
        IF oSig:CallingConvention == CallingConvention.Clipper
            FOREACH var parameter in oSig:Parameters
                parameter:TypeName := XLiterals.UsualType
                parameter:ParamType := ParamType.As
            NEXT
        ENDIF
        SELF:ParseExpressionBody()
        RETURN oSig


    PRIVATE METHOD ParseTypeParameters AS List<STRING>
        IF ! SELF:Expect(XSharpLexer.LT)
            RETURN NULL
        ENDIF
        VAR aTypeParams := List<STRING>{}
        DO WHILE SELF:La1 != XSharpLexer.GT .AND. !SELF:Eos()
            VAR sParam := SELF:TokensAsString(SELF:ParseAttributes())

            if self:Matches(XSharpLexer.IN, XSharpLexer.OUT)
                sParam += SELF:ConsumeAndGetText()+" "
            ENDIF
            IF SELF:IsId(SELF:La1)
                sParam += SELF:ConsumeAndGetText()
            ELSE
                EXIT
            ENDIF
            aTypeParams:Add(sParam)
            SELF:Expect(XSharpLexer.COMMA)
        ENDDO
        SELF:Expect(XSharpLexer.GT)
        RETURN aTypeParams

    PRIVATE METHOD ParseParameterList(lBracketed AS LOGIC, isSelf OUT LOGIC) AS List<XSourceParameterSymbol>
        isSelf := FALSE
        IF SELF:La1 != XSharpLexer.LPAREN .AND. ! lBracketed
            RETURN NULL
        ENDIF
        IF SELF:La1 != XSharpLexer.LBRKT .AND. lBracketed
            RETURN NULL
        ENDIF
        SELF:Consume()   // LParen
        VAR aResult  := List<XSourceParameterSymbol>{}
        LOCAL cond AS DelEndToken
        cond := { token => IIF (lBracketed, token == XSharpLexer.RBRKT, token == XSharpLexer.RPAREN ) }
        DO WHILE !cond(SELF:La1) .AND. ! SELF:Eos()
            LOCAL defaultExpr := NULL AS IList<IToken>

            VAR start := SELF:Lt1
            VAR atts := SELF:TokensAsString(SELF:ParseAttributes())
            VAR sId   := ""
            VAR sTypeName := ""
            IF SELF:Expect(XSharpLexer.SELF)
                isSelf := TRUE
            ENDIF
            IF SELF:IsId(SELF:La1)
                sId  += SELF:ConsumeAndGetText()
            ENDIF
            IF SELF:ExpectAssignOp()
                defaultExpr := SELF:ParseExpressionAsTokens()
            ENDIF

            VAR token := SELF:ConsumeAndGetAny(XSharpLexer.AS, XSharpLexer.IN, XSharpLexer.OUT, XSharpLexer.REF,XSharpLexer.PARAMS)
            IF token != NULL
                SELF:Expect(XSharpLexer.CONST)
                sTypeName := SELF:ParseTypeName()
            ELSE
                IF _file:Project:ParseOptions:VOUntypedAllowed
                    sTypeName := "USUAL"
                ENDIF
            ENDIF
            LOCAL variable AS XSourceParameterSymbol
            VAR endToken := SELF:LastToken
            SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
            variable := XSourceParameterSymbol{CurrentEntity, sId, range,interval, sTypeName}
            IF token != NULL
                variable:ParamType := (ParamType) token.Type
            ENDIF
            IF defaultExpr != NULL
                variable:Value := SELF:TokensAsString(defaultExpr)
            ENDIF
            aResult:Add(variable)
            IF ! Self:Expect(XSharpLexer.COMMA)
                EXIT
            ENDIF
        ENDDO
        IF cond(SELF:La1)
            SELF:Consume()
        ENDIF
        RETURN aResult

    PRIVATE METHOD ParseTypeParameterConstraints() AS STRING

        IF SELF:La1 != XSharpLexer.WHERE
            RETURN ""
        ENDIF
        VAR result := SELF:ConsumeAndGetText() +" "
        IF SELF:IsId(SELF:La1) .AND. SELF:La2 == XSharpLexer.IS
            result     += SELF:ParseIdentifier() +" "
            result     += SELF:ConsumeAndGetText()+" "
            result     += SELF:ParseTypeParameterConstraint()+" "
            DO WHILE SELF:La1 == XSharpLexer.COMMA .AND. ! SELF:Eos()
                result += SELF:ConsumeAndGetText()+" "
                result     += SELF:ParseTypeParameterConstraint()+" "
            ENDDO
        ENDIF
        RETURN result

    PRIVATE METHOD ParseTypeParameterConstraint() AS STRING
        IF SELF:Matches(XSharpLexer.CLASS, XSharpLexer.STRUCTURE)
            RETURN SELF:ConsumeAndGetText()
        ELSEIF SELF:La1 == XSharpLexer.NEW .AND. SELF:La2 == XSharpLexer.LPAREN .AND. La3 == XSharpLexer.RPAREN
            RETURN "NEW()"
        ENDIF
        RETURN SELF:ParseTypeName()



    PRIVATE METHOD ParseDataType(lIsAllowed AS LOGIC) AS STRING
        IF SELF:La1 == XSharpLexer.AS .OR. (SELF:La1 == XSharpLexer.IS .AND. lIsAllowed)
            SELF:Consume()
            RETURN SELF:ParseTypeName()
        ENDIF
        RETURN ""

    PRIVATE METHOD ParseExpressionBody() AS STRING
        IF !SELF:Expect(XSharpLexer.UDCSEP)
            RETURN ""
        ENDIF
        RETURN SELF:ParseExpression()


    PRIVATE METHOD ParseCallingConvention() AS CallingConvention
        /*
        callingconvention	: Convention=(CLIPPER | STRICT | PASCAL | ASPEN | WINCALL | CALLBACK | FASTCALL | THISCALL)
        ;
        */
        SWITCH SELF:La1
        CASE XSharpLexer.CLIPPER
        CASE XSharpLexer.STRICT
        CASE XSharpLexer.PASCAL
        CASE XSharpLexer.ASPEN
        CASE XSharpLexer.WINCALL
        CASE XSharpLexer.CALLBACK
        CASE XSharpLexer.FASTCALL
        CASE XSharpLexer.THISCALL
            VAR result := (CallingConvention) SELF:La1
            SELF:Consume()
            RETURN result
        END SWITCH
        RETURN CallingConvention.None


    PRIVATE METHOD ParseFuncProcType( kind OUT Kind) AS LOGIC

        SWITCH SELF:La1
        CASE XSharpLexer.FUNCTION
            kind := Kind.Function
        CASE XSharpLexer.PROCEDURE
            kind := Kind.Procedure
        OTHERWISE
            kind := Kind.Unknown
            RETURN FALSE
        END SWITCH
        RETURN TRUE

    PRIVATE METHOD ParseLocalFuncProc() AS IList<XSourceEntity>
        /*

        localfuncproc       :  (Modifiers=localfuncprocModifiers)?
        LOCAL T=funcproctype Sig=signature
        end=eos
        StmtBlk=statementBlock
        END T2=funcproctype   EOS
        ;

        */
        LOCAL kind AS Kind
        IF ! SELF:Matches(XSharpLexer.LOCAL)
            RETURN NULL
        ENDIF
        _modifiers:Add(SELF:Lt1)
        _modifiers:Add(SELF:Lt2)
        SELF:Consume()      // Local
        IF ! SELF:ParseFuncProcType (OUT kind)
            RETURN NULL
        ENDIF
        SELF:Consume()      // Function or Procedure
        IF kind == Kind.Function
            kind := Kind.LocalFunc
        ELSEIF kind == Kind.Procedure
            kind := Kind.LocalProc
        ENDIF
        VAR sig := SELF:ParseSignature()
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        _attributes |= Modifiers.Private
        _attributes &= (~Modifiers.Export)
        VAR xMember := XSourceMemberSymbol{sig, kind, _attributes, range, interval, _modifiers, FALSE}
        xMember:SourceCode := source
        xMember:File := SELF:_file
        xMember:Namespace := SELF:CurrentEntity:FullName
        RETURN <XSourceEntity>{xMember}



    PRIVATE METHOD IsAssignOp (nToken AS LONG) AS LOGIC
        RETURN nToken == XSharpLexer.ASSIGN_OP .OR. nToken == XSharpLexer.EQ

    PRIVATE METHOD ParseTypeName() AS STRING
        LOCAL result := "" AS STRING
        SWITCH SELF:La1

        CASE XSharpLexer.ID
            result := SELF:ParseQualifiedName()

        CASE XSharpLexer.ARRAY
            result :=  SELF:ConsumeAndGetText()
            IF SELF:La1 == XSharpLexer.OF
                SELF:Consume()
                var subtype := SELF:ParseTypeName()
                result := "ARRAY<"+subtype+">"
            ENDIF
        case XSharpLexer.BINARY  // Xbase Types
        case XSharpLexer.CODEBLOCK
        case XSharpLexer.CURRENCY
        case XSharpLexer.DATE
        CASE XSharpLexer.FLOAT
        CASE XSharpLexer.PSZ
        CASE XSharpLexer.SYMBOL
        CASE XSharpLexer.USUAL
        case XSharpLexer.BYTE     // Start of native Types
        CASE XSharpLexer.CHAR
        CASE XSharpLexer.DATETIME
        CASE XSharpLexer.DECIMAL
        CASE XSharpLexer.DWORD
        CASE XSharpLexer.DYNAMIC
        CASE XSharpLexer.INT
        CASE XSharpLexer.INT64
        CASE XSharpLexer.LOGIC
        CASE XSharpLexer.LONGINT
        CASE XSharpLexer.OBJECT
        case XSharpLexer.NINT
        case XSharpLexer.NUINT
        case XSharpLexer.PTR
        CASE XSharpLexer.REAL4
        CASE XSharpLexer.REAL8
        CASE XSharpLexer.SHORTINT
        CASE XSharpLexer.STRING
        CASE XSharpLexer.UINT64
        CASE XSharpLexer.VOID
        CASE XSharpLexer.WORD
            result :=  SELF:ConsumeAndGetText()
        OTHERWISE
            IF XSharpLexer.IsKeyword(SELF:La1)
                result :=  SELF:ConsumeAndGetText()
            ENDIF
        END SWITCH
        IF result:Length > 0
            VAR suffix := SELF:ParseTypeSuffix()
            IF suffix:Trim() == "?"
                result := "Nullable<"+result+">"
            ELSE
                result += suffix
            ENDIF
        ENDIF
        RETURN result:Trim()


    PRIVATE METHOD ParseTypeSuffix AS STRING
        IF SELF:La1 == XSharpLexer.PTR
            RETURN " "+SELF:ConsumeAndGetText()
        ELSEIF SELF:Matches(XSharpLexer.QMARK)
            RETURN SELF:ConsumeAndGetText()
        ELSEIF SELF:Matches(XSharpLexer.LBRKT) .and. (SELF:La2 != XSharpLexer.ID .and. SELF:La2 != XSharpLexer.UDC_KEYWORD .and. ! XSharpLexer.IsKeyword(SELF:La2))
            VAR tokens := List<IToken>{}
            tokens:Add(SELF:ConsumeAndGet())
            LOCAL openCount := 1 as LONG
            LOCAL closed := FALSE AS LOGIC
            DO WHILE ! SELF:Eos() .AND. ! closed
                SWITCH SELF:La1
                CASE XSharpLexer.RBRKT
                    tokens:Add(SELF:ConsumeAndGet())
                    openCount -= 1
                    IF openCount == 0 .and. SELF:La1 != XSharpLexer.LBRKT
                        closed := TRUE
                    ENDIF
                CASE XSharpLexer.LBRKT
                    openCount += 1
                    tokens:Add(SELF:ConsumeAndGet())
                CASE XSharpLexer.COMMA
                    tokens:Add(SELF:ConsumeAndGet())
                OTHERWISE
                    closed := TRUE
                END SWITCH
            ENDDO
            RETURN SELF:TokensAsString(tokens)
        ELSEIF SELF:La1 == XSharpLexer.LT
            VAR result := ""
            DO WHILE SELF:La1 != XSharpLexer.GT .AND. ! SELF:Eos()
                result += SELF:ConsumeAndGetText()
            ENDDO
            IF SELF:La1 == XSharpLexer.GT
                result += SELF:ConsumeAndGetText()
            ENDIF
            RETURN result

        ENDIF
        RETURN ""


    PRIVATE METHOD ParseExpressionAsTokens AS IList<IToken>
        // parse until SELF:Eos() or tokens such as AS, IS,
        LOCAL nested := 0 AS LONG
        LOCAL done  := FALSE AS LOGIC
        VAR tokens := List<IToken>{}

        DO WHILE ! SELF:Eos() .AND. ! done
            SWITCH SELF:La1
            CASE XSharpLexer.LT
            CASE XSharpLexer.LPAREN
            CASE XSharpLexer.LBRKT
            CASE XSharpLexer.LCURLY
                nested++
            CASE XSharpLexer.GT
            CASE XSharpLexer.RPAREN
            CASE XSharpLexer.RBRKT
            CASE XSharpLexer.RCURLY
                nested--

            CASE XSharpLexer.AS
            CASE XSharpLexer.IS
            CASE XSharpLexer.COMMA
                // The comma is used for a comma separated list of expression.
                // however inside a method call or constructor call we should have nested > 0
                IF (nested == 0)
                    done := TRUE
                ENDIF

            OTHERWISE
                // other keywords, operators etc.
                //
                NOP
            END SWITCH
            IF !done
                tokens:Add(SELF:ConsumeAndGet())
            ENDIF
        ENDDO
        RETURN tokens

    PRIVATE METHOD ParseExpression() AS STRING
        RETURN SELF:TokensAsString(SELF:ParseExpressionAsTokens())


    PRIVATE METHOD ParseForLocalDeclaration() AS LOGIC
        VAR tokens := List<IToken>{}
        VAR firstToken := SELF:Lt1
        SWITCH firstToken.Type
        CASE XSharpLexer.LOCAL
        CASE XSharpLexer.VAR
        CASE XSharpLexer.STATIC
            RETURN SELF:ParseDeclarationStatement()

        CASE XSharpLexer.LPARAMETERS
            RETURN SELF:ParseXBaseDeclarationStatement()

        CASE XSharpLexer.ID WHEN SELF:_firstTokenOnLine != NULL
            // The PRIVATE and PUBLIC keyword may have been parsed as Modifier
            SWITCH SELF:_firstTokenOnLine:Type
            CASE XSharpLexer.PRIVATE
            CASE XSharpLexer.PUBLIC
                IF SELF:SupportsMemVars
                    SELF:ParseMemvarAllocationStatement(SELF:_firstTokenOnLine:Type)
                ENDIF
                RETURN TRUE
            END SWITCH
         OTHERWISE
            IF SELF:SupportsMemVars
                SWITCH firstToken.Type
                CASE XSharpLexer.PUBLIC
                CASE XSharpLexer.PRIVATE
                CASE XSharpLexer.MEMVAR
                CASE XSharpLexer.PARAMETERS
                CASE XSharpLexer.DIMENSION
                CASE XSharpLexer.DECLARE
                    RETURN SELF:ParseXBaseDeclarationStatement()
                END SWITCH
            ENDIF
        END SWITCH

        DO WHILE ! SELF:Eos()
            IF SELF:Matches(XSharpLexer.IMPLIED, XSharpLexer.VAR)
                IF SELF:IsId(SELF:La2)
                    LOCAL kind := ImpliedKind.Assignment as ImpliedKind
                    VAR start := SELF:Lt2
                    SELF:Consume()      // IMPLIED or VAR
                    VAR id    := SELF:ParseIdentifier()
                    LOCAL expr AS IList<IToken>
                    IF SELF:IsAssignOp(SELF:La1) .OR. SELF:La1 == XSharpLexer.IN
                        IF SELF:La1 == XSharpLexer.IN
                            kind := ImpliedKind.InCollection
                        ELSE
                            kind := ImpliedKind.Assignment
                        ENDIF
                        SWITCH firstToken.Type
                        CASE XSharpLexer.FOR
                            kind := ImpliedKind.LoopCounter
                        CASE XSharpLexer.FOREACH
                            kind := ImpliedKind.InCollection
                        CASE XSharpLexer.BEGIN
                        CASE XSharpLexer.USING
                            kind := ImpliedKind.Using
                        CASE XSharpLexer.IF
                            kind := ImpliedKind.TypeCheck
                        END SWITCH
                        SELF:Consume()
                        // get the text after IN or the expression after the assignment operator
                        expr := SELF:ParseExpressionAsTokens()
                    ENDIF
                    SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
                    VAR xVar := XSourceImpliedVariableSymbol{SELF:CurrentEntity, id, range, interval }
                    xVar:Expression  := expr
                    xVar:ImpliedKind := kind
                    xVar:TypeName    := SELF:TokensAsString(expr):Trim()
                    SELF:_locals:Add(xVar)
                ENDIF
            ELSEIF SELF:IsId(SELF:La1) .AND. SELF:La2 == XSharpLexer.AS
                VAR start := SELF:Lt1
                VAR id    := SELF:ParseIdentifier()
                VAR type  := SELF:ParseDataType(FALSE)
                SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
                VAR xVar := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval, IIF(type == "", _missingType, type) }
                SELF:_locals:Add(xVar)

            ELSEIF SELF:La1 == XSharpLexer.LOCAL .AND. SELF:IsId(SELF:La2)
                VAR start := SELF:Lt1
                LOCAL expression := List<IToken>{} AS IList<IToken>
                SELF:Consume() // LOCAL
                VAR id    := SELF:ParseIdentifier()
                IF SELF:ExpectAssignOp()
                    expression := SELF:ParseExpressionAsTokens()
                ENDIF
                LOCAL type := "" AS STRING
                LOCAL lIsIs := SELF:La1 == XSharpLexer.IS AS LOGIC
                IF SELF:ExpectAny(XSharpLexer.AS, XSharpLexer.IS)
                    type  := SELF:ParseTypeName()
                ELSE
                    type := SELF:_missingType
                ENDIF
                SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
                VAR xVar := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval, type }
                IF lIsIs
                    xVar:LocalType := LocalType.Is
                ENDIF
                xVar:Expression := expression
                SELF:_locals:Add(xVar)
            ELSEIF SELF:Expect(XSharpLexer.IS)
                VAR start := SELF:Lt1
                LOCAL type := "" AS STRING
                type  := SELF:ParseTypeName()
                IF SELF:La1 == XSharpLexer.VAR
                    SELF:Consume()
                    VAR id    := SELF:ParseIdentifier()
                    SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
                    VAR xVar := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval, type }
                    SELF:_locals:Add(xVar)
                ENDIF
            ELSE
                SELF:Consume()
            ENDIF
        ENDDO
        RETURN TRUE

    PRIVATE METHOD ParseForInlineLocals() AS VOID
        // Check for method call with OUT VAR or OUT id AS TYPE
        // Check for <expr> IS <type> VAR <id>
        // also parses the EOS characters following the line
        DO WHILE ! SELF:Eos()
            VAR start := SELF:Lt1
            IF SELF:Expect(XSharpLexer.IS)
                VAR type := SELF:ParseTypeName()
                IF SELF:Expect(XSharpLexer.VAR)
                    VAR id            := SELF:ParseIdentifier()
                    SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
                    VAR xVar          := XSourceImpliedVariableSymbol{SELF:CurrentEntity, id, range, interval}
                    xVar:TypeName     := type:Trim()
                    xVar:ImpliedKind  := ImpliedKind.TypeCheck
                    SELF:_locals:Add(xVar)
                ENDIF

            ELSEIF SELF:Expect(XSharpLexer.OUT)
                // OUT Id AS Type
                IF SELF:IsId(SELF:La1) .AND. La2 == XSharpLexer.AS
                    VAR id   := SELF:ParseIdentifier()
                    VAR type := SELF:ParseDataType(FALSE)
                    SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
                    VAR xVar     := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval, iif(type == "", _missingType, type)}
                    SELF:_locals:Add(xVar)
                ELSEIF SELF:Expect(XSharpLexer.VAR)
                    // OUT VAR Id, when Id = '_' then discard and do not create a local
                    VAR id         := SELF:ParseIdentifier()
                    IF id          != "_"
                        SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
                        VAR xVar           := XSourceImpliedVariableSymbol{SELF:CurrentEntity, id, range, interval}
                        xVar:ImpliedKind := ImpliedKind.OutParam
                        // Todo: Get Expression for OUT variable
                        SELF:_locals:Add(xVar)
                    ENDIF
                ELSEIF SELF:Expect(XSharpLexer.NULL)
                    // OUT NULL, also discard
                    NOP
                ENDIF
            ELSEIF LastToken:Type == XSharpLexer.CASE .and. SELF:IsId(SELF:La1) .and. SELF:La2 == XSharpLexer.AS       // CASE x as SomeType
                VAR id    := SELF:ParseIdentifier()
                VAR type  := SELF:ParseDataType(FALSE)
                SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
                VAR xVar     := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval, iif(type == "", _missingType, type)}
                SELF:_locals:Add(xVar)
            ELSE
                SELF:Consume()
            ENDIF
        ENDDO
        SELF:ReadUntilEos()


    PRIVATE METHOD ParseStatement() AS VOID
        IF ! SELF:_collectLocals
            SELF:ReadLine()
            RETURN
        ENDIF
        DO WHILE SELF:Eos() .AND. !SELF:Eoi()
            SELF:Consume()
        ENDDO
        SELF:ParseUdcTokens()  // Read UDC tokens on the current line
        if SELF:ExpectOnThisLine(XSharpLexer.OUT)
            SELF:ParseForInlineLocals()
            RETURN
        ENDIF
        SWITCH SELF:La1
        CASE XSharpLexer.FIELD
            IF !SELF:ParseFieldStatement()
                SELF:ReadLine()
            ENDIF

        CASE XSharpLexer.FOREACH
        CASE XSharpLexer.FOR
            IF ! SELF:ParseForLocalDeclaration()
                SELF:ReadLine()
            ENDIF
        CASE XSharpLexer.LOCAL
        CASE XSharpLexer.VAR
            IF ! SELF:ParseDeclarationStatement()
                SELF:ReadLine()
            ENDIF
        CASE XSharpLexer.USING
            IF SELF:La2 == XSharpLexer.VAR .or. SELF:La2 == XSharpLexer.STATIC .or. SELF:La2 == XSharpLexer.LOCAL .or. SELF:La2 == XSharpLexer.IMPLIED
                SELF:ParseUsingDeclarationStatement()
            ELSE
                SELF:ReadLine()
            ENDIF
        CASE XSharpLexer.LPARAMETERS
            IF ! SELF:ParseXBaseDeclarationStatement()
                SELF:ReadLine()
            ENDIF
        CASE XSharpLexer.MEMVAR
        CASE XSharpLexer.PARAMETERS
        CASE XSharpLexer.DIMENSION
        CASE XSharpLexer.DECLARE
            IF SELF:SupportsMemVars
                IF ! SELF:ParseXBaseDeclarationStatement()
                    SELF:ReadLine()
                ENDIF
            ELSE
                // duplicate the code for the OTHERWISE clause
                SELF:ParseForInlineLocals()
                SELF:ReadLine( )
            ENDIF
        CASE XSharpLexer.ID
            // STATIC ID
            if SELF:LastToken:Type == XSharpLexer.STATIC
                SELF:ParseDeclarationStatement()
            elseIF SELF:SupportsMemVars .and. SELF:LastToken != NULL .AND. (SELF:LastToken:Type == XSharpLexer.PUBLIC .OR. SELF:LastToken:Type == XSharpLexer.PRIVATE)
                SELF:PushBack()
                IF ! SELF:ParseXBaseDeclarationStatement()
                    SELF:ReadLine()
                ENDIF
            ELSE
                SELF:ParseForInlineLocals()
                SELF:ReadLine( )
            ENDIF

        OTHERWISE
            SELF:ParseForInlineLocals()
            SELF:ReadLine( )
        END SWITCH
        RETURN

    PRIVATE METHOD ParseUsingDeclarationStatement() AS LOGIC
    //   | Using=USING Static=STATIC? VAR ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)* end=eos #varLocalDecl
    //   | Using=USING Static=STATIC? LOCAL? IMPLIED ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*  end=eos #varLocalDecl
        local lStatic := FALSE AS LOGIC
        IF ! SELF:Expect(XSharpLexer.USING)
            RETURN FALSE
        ENDIF
        IF SELF:Expect(XSharpLexer.STATIC)
            lStatic := TRUE
        ENDIF
        IF SELF:Expect(XSharpLexer.VAR)
            NOP
        ELSEIF SELF:Expect(XSharpLexer.LOCAL) .and. SELF:Expect(XSharpLexer.IMPLIED)
            NOP
        ELSEIF SELF:Expect(XSharpLexer.IMPLIED)
            NOP
        ELSE
            RETURN FALSE
        ENDIF
        VAR result := List<XSourceVariableSymbol>{}
        VAR xVar := SELF:ParseImpliedVar(lStatic)
        result:Add(xVar)
        DO WHILE SELF:Expect(XSharpLexer.COMMA)
            xVar := SELF:ParseImpliedVar(lStatic)
            result:Add(xVar)
        ENDDO
        FOREACH VAR x IN result
            SELF:_locals:Add(x)
        NEXT
        SELF:ReadLine()
        RETURN TRUE

    PRIVATE METHOD ParseDeclarationStatement() AS LOGIC
        //
        //localdecl          : LOCAL (STATIC=STATIC)? LocalVars+=localvar (COMMA LocalVars+=localvar)*			END=eos #commonLocalDecl
        //                   | STATIC=STATIC LOCAL    LocalVars+=localvar (COMMA LocalVars+=localvar)*			END=eos #commonLocalDecl
        //                   | {!XSharpLexer.IsKeyword(InputStream.La(2))}?   // STATIC Identifier , but not STATIC <Keyword>
        //                     STATIC=STATIC          LocalVars+=localvar (COMMA LocalVars+=localvar)*			END=eos #commonLocalDecl
        //                   // The following rules allow STATIC in the parser,
        //                   // but the treetransformation will produce an error 9044 for STATIC implied
        //                   | STATIC=STATIC? VAR           ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*  END=eos #varLocalDecl
        //                   | STATIC=STATIC LOCAL? IMPLIED ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*  END=eos #varLocalDecl


        LOCAL lStatic := LastToken:Type == XSharpLexer.STATIC AS LOGIC
        LOCAL lImplied := FALSE AS LOGIC
        IF SELF:Expect(XSharpLexer.LOCAL)
            lStatic  := SELF:Expect(XSharpLexer.STATIC)
            lImplied := SELF:Expect(XSharpLexer.IMPLIED)
            if !lStatic
                lStatic  := SELF:Expect(XSharpLexer.STATIC)
            ENDIF
            IF SELF:La1 == XSharpLexer.ARRAY .and. _dialect == XDialect.FoxPro
                SELF:Consume()
            ENDIF
        ELSEIF SELF:Expect(XSharpLexer.STATIC)
            lStatic := TRUE
            IF SELF:La1 == XSharpLexer.LOCAL
                SELF:Consume()
            ELSEIF SELF:Expect(XSharpLexer.VAR)
                lImplied  := TRUE
            ELSEIF XSharpLexer.IsKeyword(SELF:La1)      // STATIC Keyword should exit
                RETURN FALSE
            ENDIF
            IF SELF:Expect(XSharpLexer.IMPLIED)
                lImplied := TRUE
            ENDIF
        ELSEIF SELF:Expect(XSharpLexer.VAR)
            lImplied  := TRUE
        ELSEIF ! lStatic
            RETURN FALSE
        ENDIF
        VAR result := List<XSourceVariableSymbol>{}
        IF lImplied
            VAR xVar := SELF:ParseImpliedVar(lStatic)
            result:Add(xVar)
            DO WHILE SELF:Expect(XSharpLexer.COMMA)
                xVar := SELF:ParseImpliedVar(lStatic)
                result:Add(xVar)
            ENDDO
        ELSE
            VAR xVar := SELF:ParseLocalVar(lStatic)
            result:Add(xVar)
            DO WHILE SELF:Expect(XSharpLexer.COMMA)
                xVar := SELF:ParseLocalVar(lStatic)
                result:Add(xVar)
            ENDDO
        ENDIF
        // copy type forward
        VAR missingTypes := List<XSourceVariableSymbol>{}
        FOREACH VAR xVar IN result
            IF xVar:TypeName == XLiterals.NoType
                missingTypes:Add(xVar)
            ELSE
                IF missingTypes:Count > 0
                    FOREACH VAR xMissing IN missingTypes
                        xMissing:TypeName := xVar:TypeName
                    NEXT
                    missingTypes:Clear()
                ENDIF
            ENDIF
            SELF:_locals:Add(xVar)
        NEXT
        FOREACH VAR xMissing IN missingTypes
            xMissing:TypeName := _missingType
        NEXT
        SELF:ReadLine()
        RETURN TRUE

    PRIVATE METHOD ParseLocalVar(lStatic as logic) AS XSourceVariableSymbol
        /*
        //localvar           : (CONST=CONST)? ( DIM=DIM )? Id=identifier (LBRKT ArraySub=arraysub RBRKT)?
        //                     (Op=assignoperator Expression=expression)? (AS=(AS | IS) DataType=datatype)?
        //
        // The location of the local is linked to the Id and not to the LOCAL keyword
        */
        LOCAL expr  AS IList<IToken>
        LOCAL start := SELF:Lt1 AS IToken
        VAR lConst  := SELF:Expect(XSharpLexer.CONST)
        VAR lDim    := SELF:Expect(XSharpLexer.DIM)
        VAR id      := SELF:ParseIdentifier()
        VAR arraysub := SELF:ParseArraySub()
        IF SELF:ExpectAssignOp()
            expr       := SELF:ParseExpressionAsTokens()
        ENDIF
        VAR type     := SELF:ParseDataType(TRUE)
        SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
        IF String.IsNullOrEmpty(type)
            type := XLiterals.NoType
        ENDIF
        VAR xVar     := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval, type} {IsArray := lDim .OR. !String.IsNullOrEmpty(arraysub)}
        IF type:EndsWith("]")
            xVar:IsArray := TRUE
        ENDIF
        xVar:Expression := expr
        if lConst
            xVar:Attributes |= Modifiers.Const
        endif
        if lStatic
            xVar:Attributes |= Modifiers.Static
        endif
        IF expr != NULL
            xVar:Value := SELF:TokensAsString(expr)
        endif
        RETURN xVar

    PRIVATE METHOD ParseImpliedVar(lStatic as LOGIC) AS XSourceVariableSymbol
        // impliedvar: (Const=CONST)? Id=identifier Op=assignoperator Expression=expression
        // The location of the local is linked to the Id and not to the VAR or IMPLIED keyword

        LOCAL expr     AS IList<IToken>
        LOCAL start := SELF:Lt1 AS IToken
        VAR lConst  := SELF:Expect(XSharpLexer.CONST)
        VAR id      := SELF:ParseIdentifier()
        IF SELF:ExpectAssignOp()
            expr := SELF:ParseExpressionAsTokens()
        ENDIF
        SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
        VAR xVar     := XSourceImpliedVariableSymbol{SELF:CurrentEntity, id,  range, interval}
        xVar:Expression := expr
        if lConst
            xVar:Attributes |= Modifiers.Const
        endif
        if lStatic
            xVar:Attributes |= Modifiers.Static
        endif
        xVar:ImpliedKind := ImpliedKind.Assignment
        xVar:Value := SELF:TokensAsString(expr)
        RETURN xVar

    PRIVATE METHOD ParseXBaseDeclarationStatement() AS LOGIC
        /*
        xbasedecl           : T=(MEMVAR|PARAMETERS|LPARAMETERS)      // MEMVAR  Foo, Bar or PARAMETERS Foo, Bar
        Vars+=identifierName (COMMA Vars+=identifierName)*
        end=eos
        | T=(PRIVATE | PUBLIC)
        XVars+=xbasevar (COMMA XVars+=xbasevar)*   // PRIVATE Foo := 123,  PUBLIC Bar, PUBLIC MyArray[5,2]
        end=eos
        // FoxPro dimension statement
        | T=(DIMENSION|DECLARE) DimVars += dimensionVar (COMMA DimVars+=dimensionVar)*    end=eos
        ;

        xbasevar            : (Amp=AMP)?  Id=identifierName (LBRKT ArraySub=arraysub RBRKT)? (Op=assignoperator Expression=expression)?
        ;

        dimensionVar        : Id=identifierName  ( LBRKT ArraySub=arraysub RBRKT | LPAREN ArraySub=arraysub RPAREN ) (AS DataType=datatype)?
        ;

        */
        SWITCH SELF:La1
        CASE XSharpLexer.MEMVAR
        CASE XSharpLexer.PARAMETERS
        CASE XSharpLexer.LPARAMETERS
            SELF:ParseMemvarDeclarationStatement()
        CASE XSharpLexer.PUBLIC
        CASE XSharpLexer.PRIVATE
            SELF:ParseMemvarAllocationStatement(SELF:La1)
        CASE XSharpLexer.DIMENSION
        CASE XSharpLexer.DECLARE
            SELF:ParseFoxProDim()
        END SWITCH
        RETURN TRUE

    PRIVATE METHOD ParseMemvarDeclarationStatement() AS VOID
        /*
        xbasedecl           : T=(MEMVAR|PARAMETERS|LPARAMETERS)      // MEMVAR  Foo, Bar or PARAMETERS Foo, Bar
        Vars+=identifierName (COMMA Vars+=identifierName)*
        end=eos
        */
        VAR start := SELF:Lt1
        IF ! SELF:ExpectAny(XSharpLexer.MEMVAR, XSharpLexer.PARAMETERS, XSharpLexer.LPARAMETERS)
            RETURN
        ENDIF
        VAR Ids := List<STRING>{}
        IF ! SELF:IsId(SELF:La1)
            RETURN
        ENDIF
        Ids:Add(SELF:ParseIdentifier())
        DO WHILE SELF:Expect(XSharpLexer.COMMA)
            Ids:Add(SELF:ParseIdentifier())
        ENDDO
        SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
        SELF:ReadLine()
        FOREACH VAR id IN Ids
            VAR xVar := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval,XLiterals.UsualType}
            IF start.Type != XSharpLexer.MEMVAR
                xVar.Kind := Kind.Parameter
            ELSE
                xVar.Kind := Kind.MemVar
            ENDIF
            SELF:_locals:Add(xVar)
        NEXT
        RETURN

    PRIVATE METHOD ParseMemvarAllocationStatement(nType as LONG) AS VOID
        /*
        | T=(PRIVATE | PUBLIC)
        XVars+=xbasevar (COMMA XVars+=xbasevar)*   // PRIVATE Foo := 123,  PUBLIC Bar, PUBLIC MyArray[5,2]
        end=eos
        */
        VAR start := SELF:_firstTokenOnLine
        IF nType != XSharpLexer.PRIVATE && nType != XSharpLexer.PUBLIC
            RETURN
        ENDIF
        IF SELF:La1 == nType
            SELF:Consume()
        ENDIF
        VAR result := List<XSourceVariableSymbol>{}
        VAR xVar := SELF:ParseXBaseVar()
        result:Add(xVar)
        DO WHILE SELF:Expect(XSharpLexer.COMMA)
            xVar := SELF:ParseXBaseVar()
            result:Add(xVar)
        ENDDO
        SELF:ReadLine()
        FOREACH VAR mv IN result
            SELF:_locals:Add(mv)
        NEXT
        RETURN

    PRIVATE METHOD ParseXBaseVar AS XSourceVariableSymbol
        /*
        xbasevar            : (Amp=AMP)?  Id=identifierName (LBRKT ArraySub=arraysub RBRKT)? (Op=assignoperator Expression=expression)?
        ;
        */
        LOCAL expr     AS IList<IToken>
        LOCAL start    := SELF:Lt1 AS IToken
        VAR lAmp       := SELF:Expect(XSharpLexer.AMP)
        VAR id         := SELF:ParseIdentifier()
        VAR arraysub   := SELF:ParseArraySub()
        IF SELF:ExpectAssignOp()
            expr        := SELF:ParseExpressionAsTokens()
        ENDIF
        VAR type       := XLiterals.UsualType
        SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
        VAR xVar       := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval, type} {IsArray := !String.IsNullOrEmpty(arraysub)}
        xVar.Kind      := Kind.MemVar
        xVar:Expression := expr
        if expr != NULL
            var sb := StringBuilder{}
            foreach token as XSharpToken in expr
                sb:Append( token:TextWithTrivia)
            next
            xVar:Value := sb:ToString()
        endif

        RETURN xVar


    PRIVATE METHOD ParseFoxProDim AS VOID
        /*
        | T=(DIMENSION|DECLARE) DimVars += dimensionVar (COMMA DimVars+=dimensionVar)*    END=eos

        dimensionVar        : Id=identifierName  ( LBRKT ArraySub=arraysub RBRKT | LPAREN ArraySub=arraysub RPAREN ) (AS DataType=datatype)?
        ;
        */
        IF _dialect == XDialect.FoxPro
            IF ! SELF:ExpectAny(XSharpLexer.DIMENSION, XSharpLexer.DECLARE)
                RETURN
            ENDIF
            SELF:ParseDimensionVar()
            DO WHILE SELF:Expect(XSharpLexer.COMMA)
                SELF:ParseDimensionVar()
            ENDDO
        ENDIF
        SELF:ReadLine()
        RETURN


    PRIVATE METHOD ParseDimensionVar() AS VOID
        LOCAL start    := SELF:Lt1 AS IToken
        LOCAL arraysub AS STRING
        VAR id       := SELF:ParseIdentifier()
        IF SELF:La1 == XSharpLexer.LBRKT
            arraysub := SELF:ParseArraySub()
        ELSEIF SELF:La1 == XSharpLexer.LPAREN
            arraysub := SELF:ParseArraySubFox()
        ENDIF
        VAR type := SELF:ParseDataType(FALSE)
        SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
        IF String.IsNullOrEmpty(type)
            type := "ARRAY"
        ENDIF
        VAR xVar     := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval, type}
        xVar:Kind    := Kind.MemVar
        IF type != "ARRAY"
            xVar:IsArray := !String.IsNullOrEmpty(arraysub)
        ENDIF
        SELF:_locals:Add(xVar)


    PRIVATE METHOD ParseFieldStatement() AS LOGIC
        /*
        fielddecl          : FIELD Fields+=identifierName (COMMA Fields+=identifierName)* (IN Alias=identifierName)? end=eos
        ;

        */
        VAR start := SELF:Lt1
        IF ! SELF:Expect(XSharpLexer.FIELD)
            RETURN FALSE
        ENDIF
        VAR Ids := List<STRING>{}
        IF ! SELF:IsId(SELF:La1)
            RETURN FALSE
        ENDIF
        Ids:Add(SELF:ParseIdentifier())
        DO WHILE SELF:Expect(XSharpLexer.COMMA)
            Ids:Add(SELF:ParseIdentifier())
        ENDDO
        VAR area := ""
        IF SELF:Expect(XSharpLexer.IN)
            area := SELF:ParseIdentifier()
        ENDIF
        SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
        SELF:ReadLine()
        FOREACH VAR id IN Ids
            VAR xVar := XSourceVariableSymbol{SELF:CurrentEntity, id, range, interval,XLiterals.UsualType}
            xVar.Kind := Kind.DbField
            IF ! String.IsNullOrEmpty(area)
                xVar:Value := area
            ENDIF
            SELF:_locals:Add(xVar)
        NEXT
        RETURN TRUE

#region FoxPro class definitions
    PRIVATE METHOD ParseFoxClass() AS IList<XSourceEntity>
        /*
        foxclass            : (Attributes=attributes)?
        D=DEFINE (Modifiers=classModifiers)?
        CLASS (Namespace=nameDot)? Id=identifier
        TypeParameters=typeparameters?
        (AS BaseType=datatype)?
        (OF Classlib=identifier) ?
        (ConstraintsClauses+=typeparameterconstraintsclause)*             // Optional typeparameterconstraints for Generic Class
        (OLEPUBLIC) ?
        e=eos
        (Members+=foxclassmember)*
        (ENDDEFINE | END DEFINE) eos
        ;

        */
        var def := SELF:Lt1
        IF ! SELF:Expect(XSharpLexer.DEFINE)
            RETURN NULL
        ENDIF
        VAR mods := SELF:ParseVisibilityAndModifiers()
        _modifiers.Add(def)
        _modifiers:Add(SELF:Lt1)
        IF ! SELF:Expect(XSharpLexer.CLASS)
            RETURN NULL
        ENDIF
        VAR id := SELF:ParseQualifiedName()
        VAR typePars := SELF:ParseTypeParameters()
        VAR baseType := ""
        VAR classlib := ""
        IF SELF:Expect(XSharpLexer.AS)
            baseType := SELF:ParseTypeName()
        ENDIF
        IF SELF:Expect(XSharpLexer.OF)
            classlib := SELF:ParseIdentifier()
        ENDIF
        LOCAL constraints   AS List<STRING>
        DO WHILE SELF:La1 == XSharpLexer.WHERE
            IF constraints == NULL
                constraints := List<STRING>{}
            ENDIF
            constraints:Add(SELF:ParseTypeParameterConstraints())
        ENDDO
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xType := XSourceTypeSymbol{id, Kind.Class, mods, range, interval, _file, _modifiers}
        xType:SourceCode := source
        IF constraints?:Count > 0
            FOREACH VAR constraint IN constraints
                xType:AddConstraints(constraint)
            NEXT
        ENDIF
        IF typePars?:Count > 0
            FOREACH VAR typepar IN typePars
                xType:AddTypeParameter(typepar)
            NEXT
        ENDIF
        xType:ClassType := XDialect.FoxPro
        SELF:AddAsChild(xType)
        RETURN <XSourceEntity>{xType}

    PRIVATE METHOD ParseFoxMethod() AS IList<XSourceEntity>
        /*
        foxmethod           : (Attributes=attributes)? (Modifiers=memberModifiers)?
        T=funcproctype  Sig=signature
        (HelpString=HELPSTRING HelpText=expression)?
        (ThisAccess=THISACCESS LPAREN MemberId=identifier RPAREN)?
        end=eos
        StmtBlk=statementBlock
        (END T2=funcproctype  EOS)?
        ;
        funcproctype        : Token=(FUNCTION | PROCEDURE)
        ;

        */
        LOCAL hs := "" AS STRING
        LOCAL thisId := "" AS STRING
        LOCAL kind AS Kind
        IF ! SELF:ParseFuncProcType (OUT kind)
            RETURN NULL
        ENDIF
        _modifiers:Add(SELF:Lt1)
        VAR sig := SELF:ParseSignature()
        IF SELF:Expect(XSharpLexer.HELPSTRING)
            hs  := SELF:ParseExpression()
        ENDIF
        IF SELF:Expect(XSharpLexer.THISACCESS) .AND. SELF:Expect(XSharpLexer.LPAREN)
            thisId := SELF:ParseIdentifier()
            SELF:Expect(XSharpLexer.RPAREN)
        ENDIF
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        kind := Kind.Method
        // Check for _ACCESS or _ASSIGN
        VAR id := sig:Id
        IF id:EndsWith("_access", StringComparison.OrdinalIgnoreCase)
            sig:Id   := sig:Id:Substring(0, sig:Id:Length - "_access":Length)
            kind := Kind.Access
        ELSEIF id:EndsWith("_assign", StringComparison.OrdinalIgnoreCase)
            sig:Id   := sig:Id:Substring(0, sig:Id:Length - "_assign":Length)
            kind := Kind.Assign
        ENDIF

        VAR xMember := XSourceMemberSymbol{sig, kind, _attributes, range, interval, _modifiers, _attributes:HasFlag(Modifiers.Static)}
        xMember:SourceCode := source
        xMember:File := SELF:_file
        RETURN <XSourceEntity>{xMember}


    PRIVATE METHOD ParseFoxFields() AS IList<XSourceEntity>
        /*
        foxclassvars        : (Attributes=attributes)? (Modifiers=classvarModifiers)?
        (Fld=FIELD)? Vars += identifier (COMMA Vars += identifier )*  (AS DataType=datatype)?
        end=eos
        ;

        foxfield            : (Modifiers=classvarModifiers)? (Fld=FIELD)? F=foxfieldinitializer end=eos
        ;
        foxfieldinitializer : Name=name assignoperator Expr=expression
        ;

        foximplementsclause : IMPLEMENTS Type=datatype (Excl=EXCLUDE)? (IN Library=expression)?
        end=eos
        ;

        foxaddobjectclause  : (Attributes=attributes)? ADD OBJECT (Modifiers=classvarModifiers)?
        Id=identifier AS Type=datatype (NoInit=NOINIT)?
        (WITH FieldsInits += foxfieldinitializer (COMMA FieldsInits += foxfieldinitializer)* )?
        end=eos
        ;

        foxpemcomattrib     : DIMENSION Id=identifier LBRKT expression RBRKT end=eos
        | Id=identifier LBRKT expression RBRKT assignoperator expression end=eos
        ;
        */

        IF CurrentType == NULL .OR. CurrentType:ClassType != XDialect.FoxPro
            RETURN NULL
        ENDIF
        VAR typedef := CurrentType
        VAR result := List<XSourceEntity>{}
        // Detect which variant we need
        IF SELF:Expect(XSharpLexer.IMPLEMENTS)
            //  foximplementsclause : IMPLEMENTS Type=datatype (Excl=EXCLUDE)? (IN Library=expression)? END=eos
            VAR interf    := SELF:ParseTypeName()
            VAR exclude   := SELF:Expect(XSharpLexer.EXCLUDE)
            IF SELF:Expect(XSharpLexer.IN)
                VAR Lib := SELF:ParseExpression()
            ENDIF
            typedef:AddInterface(interf)
            SELF:ReadLine()
            RETURN NULL
        ELSEIF SELF:Expect(XSharpLexer.DIMENSION)
            // foxpemcomattrib     : DIMENSION Id=identifier LBRKT expression RBRKT end=eos
            VAR id := SELF:ParseIdentifier()
            VAR expr := ""
            IF ! SELF:Expect(XSharpLexer.LBRKT)
                RETURN NULL
            ENDIF
            expr := SELF:ParseExpression()
            IF !SELF:Expect(XSharpLexer.RBRKT)
                RETURN NULL
            ENDIF
            SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
            SELF:ReadLine()
            VAR xMember := XSourceMemberSymbol{id, Kind.Field, _attributes, range, interval,"ARRAY", _modifiers} {SingleLine := TRUE}
            xMember:SourceCode := source
            xMember:File := SELF:_file
            RETURN <XSourceEntity>{xMember}
        ELSEIF SELF:Matches(XSharpLexer.ID, XSharpLexer.FIELD)
            VAR hasField := SELF:Expect(XSharpLexer.FIELD)
            IF ! SELF:IsId(SELF:La1)
                RETURN NULL
            ENDIF
            VAR ids      := List<STRING>{}
            ids:Add(SELF:ParseIdentifier())
            IF ids:First():EndsWith("COMATTRIB", StringComparison.OrdinalIgnoreCase)
                RETURN NULL
            ENDIF
            VAR source := ""

            IF SELF:ExpectAssignOp()
                // foxfield            : (Modifiers=classvarModifiers)? (Fld=FIELD)? F=foxfieldinitializer END=eos
                //                     ;
                // foxfieldinitializer : Name=name assignoperator Expr=expression
                //                     ;
                VAR expr := SELF:ParseExpression()
                SELF:GetSourceInfo(_start, LastToken, OUT VAR r1, OUT VAR i1, OUT source)
                SELF:ReadLine()
                VAR xMember := XSourceMemberSymbol{ids:First(), Kind.Field, _attributes, r1, i1, _missingType, _modifiers} {SingleLine := TRUE}
                xMember:SourceCode := source
                xMember:Value      := expr
                xMember:File := SELF:_file
                RETURN <XSourceEntity>{xMember}
            ENDIF
            IF SELF:Matches(XSharpLexer.COMMA)
                // foxclassvars        : (Attributes=attributes)? (Modifiers=classvarModifiers)?
                //                      (Fld=FIELD)? Vars += identifier (COMMA Vars += identifier )*  (AS DataType=datatype)?
                //                      END=eos
                DO WHILE SELF:Expect(XSharpLexer.COMMA)
                    ids:Add(SELF:ParseIdentifier())
                ENDDO
            ENDIF
            VAR type := SELF:ParseDataType(FALSE)
            SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT  source)
            SELF:ReadLine()
            FOREACH VAR id IN ids
                VAR xMember := XSourceMemberSymbol{id, Kind.Field, _attributes, range, interval, type, _modifiers} {SingleLine := TRUE}
                IF ids:Count == 1
                    xMember:SourceCode := source
                ELSEIF String.IsNullOrEmpty(type)
                    xMember:SourceCode := "FIELD "+id
                ELSE
                    xMember:SourceCode := "FIELD "+id +" AS "+type
                ENDIF
                xMember:File := SELF:_file
                result:Add(xMember)
            NEXT
            RETURN result
        ELSEIF SELF:Expect(XSharpLexer.ADD)
            /*
            foxaddobjectclause  : (Attributes=attributes)? ADD OBJECT (Modifiers=classvarModifiers)?
            Id=identifier AS Type=datatype (NoInit=NOINIT)?
            (WITH FieldsInits += foxfieldinitializer (COMMA FieldsInits += foxfieldinitializer)* )?
            end=eos
            ;
            */
            IF ! SELF:Expect(XSharpLexer.OBJECT)
                RETURN NULL
            ENDIF
            VAR mods := SELF:ParseVisibilityAndModifiers()
            VAR id := SELF:ParseIdentifier()
            IF ! SELF:Expect(XSharpLexer.AS)
                RETURN NULL
            ENDIF
            VAR type   := SELF:ParseTypeName()
            VAR noinit := SELF:Expect(XSharpLexer.NOINIT)
            // no need to parse the initializers here
            SELF:ReadUntilEos()
            SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
            SELF:ReadLine()
            VAR xMember := XSourceMemberSymbol{id, Kind.Field, _attributes, range, interval, type, _modifiers} {SingleLine := TRUE}
            xMember:SourceCode := source
            xMember:File := SELF:_file
            RETURN <XSourceEntity>{xMember}
        ENDIF
        // ignore the COMATTRIB rule here
        SELF:ReadLine()
        RETURN NULL
#endregion

#region XPP classes
    PRIVATE METHOD ParseXppClass() AS IList<XSourceEntity>
        /*
        xppclass           :  (Attributes=attributes)?                                // NEW Optional Attributes
        (Modifiers=xppclassModifiers)?                          // [STATIC|FREEZE|FINAL]
        C=CLASS (Namespace=nameDot)? Id=identifier               // CLASS <ClassName>
        (
        From=(FROM| SHARING) BaseTypes+=datatype (COMMA BaseTypes+=datatype)*  // [FROM <SuperClass,...>] ;
        )?                                                                   // [SHARING <SuperClass,...>]
        (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)? // NEW Implements
        // No type parameters and type parameter constraints
        e=eos
        (Members+=xppclassMember)*
        ENDCLASS
        eos
        ;
        xppclassModifiers   : ( Tokens+=(STATIC | FREEZE | FINAL) )+
        ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs

        xppclassMember      : Member=xppmethodvis                           #xppclsvisibility
        | Member=xppclassvars                           #xppclsvars
        | Member=xppinlineMethod                        #xppclsinlinemethod
        | Member=xppdeclareMethod                       #xppclsdeclaremethod
        | Member=xppproperty                            #xppclsproperty
        | Member=pragma                                 #xpppragma
        ;
        */
        IF ! SELF:Expect(XSharpLexer.CLASS)
            RETURN NULL
        ENDIF
        _modifiers:Add(SELF:Lt1)
        VAR id         := SELF:ParseQualifiedName()
        VAR baseType   := ""
        IF SELF:ExpectAny(XSharpLexer.FROM, XSharpLexer.SHARING)
            baseType  := SELF:ParseTypeName()
            DO WHILE SELF:Expect(XSharpLexer.COMMA)
                // consume the other type declarations , ignore them because we do not support multiple inheritance
                SELF:ParseTypeName()
            ENDDO
        ENDIF
        VAR interfaces := List<STRING>{}
        IF SELF:Expect(XSharpLexer.IMPLEMENTS)
            interfaces:Add(SELF:ParseTypeName())
            DO WHILE SELF:Expect(XSharpLexer.COMMA)
                interfaces:Add(SELF:ParseTypeName())
            ENDDO
        ENDIF
        SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()

        VAR xType := XSourceTypeSymbol{id,Kind.Class, _attributes, range, interval, _file, _modifiers}
        xType:SourceCode := source

        IF interfaces?:Count > 0
            FOREACH VAR sInterface IN interfaces
                xType:AddInterface(sInterface)
            NEXT
        ENDIF
        IF ! String.IsNullOrEmpty(baseType)
            xType:BaseTypeName := baseType
        ENDIF
        xType:IsPartial := _attributes:HasFlag(Modifiers.Partial)
        xType:ClassType := XDialect.XPP
        _xppVisibility := Modifiers.Public
        SELF:AddAsChild(xType)
        RETURN <XSourceEntity>{xType}

    PRIVATE METHOD ParseXppClassVars() AS IList<XSourceEntity>
        /*

        xppclassvars        : (Modifiers=xppmemberModifiers)?                             // [CLASS]
        VAR Vars+=identifier                                        // VAR <VarName>
        (
        Is=xppisin                                                // [IS <Name>] [IN <SuperClass>]
        | ((COMMA Vars+=identifier)*                              // <,...>
        (AS DataType=datatype)?  )                                // Optional data type

        )
        (Shared=SHARED)?                                            // [SHARED]
        (ReadOnly=READONLY)?                                        // [READONLY]
        (Assignment=xppvarassignment)?                              // [ASSIGNMENT HIDDEN | PROTECTED | EXPORTED]
        (Nosave= NOSAVE)?                                           // [NOSAVE]
        eos
        ;


        xppvarassignment    : ASSIGNMENT xppvisibility                                    // [ASSIGNMENT HIDDEN | PROTECTED | EXPORTED]
        ;
        */
        VAR isStatic := SELF:ParseXPPMemberModifiers()
        IF ! SELF:Expect(XSharpLexer.VAR)
            RETURN NULL
        ENDIF
        LOCAL type AS STRING
        VAR ids := List<STRING>{}
        IF SELF:IsId(SELF:La1)
            ids:Add(SELF:ParseIdentifier())
        ELSE
            RETURN NULL
        ENDIF
        IF SELF:Matches(XSharpLexer.IN,XSharpLexer.IS)
            SELF:ParseXppIsIn()     // Parse but ignore
        ELSE
            DO WHILE SELF:Expect(XSharpLexer.COMMA)
                ids:Add(SELF:ParseIdentifier())
            ENDDO
            type := SELF:ParseDataType(FALSE)
        ENDIF
        SELF:GetSourceInfo(_start, SELF:LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
        // eat all tokens
        DO WHILE SELF:ExpectAny(XSharpLexer.SHARED, XSharpLexer.READONLY, XSharpLexer.ASSIGNMENT, ;
                XSharpLexer.HIDDEN, XSharpLexer.PROTECTED, XSharpLexer.EXPORTED, XSharpLexer.NOSAVE)
            NOP
        ENDDO
        SELF:ReadLine()
        VAR result := List<XSourceEntity>{}
        FOREACH VAR id IN ids
            VAR classvar := XSourceMemberSymbol{id, Kind.Field, _xppVisibility, range, interval, type, _modifiers, FALSE} {SingleLine := TRUE}
            classvar:SourceCode := ie"{classvar.ModVis}: \r\n{ IIF(isStatic, \"STATIC\",\"\")} VAR {classvar.Name}"
            IF SELF:CurrentType != NULL
                SELF:CurrentType:AddMember(classvar)
            ENDIF
            result:Add(classvar)
        NEXT
        RETURN result

    PRIVATE METHOD ParseXppProperty AS IList<XSourceEntity>
        /*
        xppproperty         : (Attributes=attributes)?                                    // NEW Optional Attributes
        (   Access=ACCESS Assign=ASSIGN?                            // ACCESS | ASSIGN  | ACCESS ASSIGN | ASSIGN ACCESS
        | ASSIGN=ASSIGN Access=ACCESS?
        )
        Modifiers=xppmemberModifiers?                               // [CLASS]
        M=METHOD Id=identifier                                        // METHOD <MethodName>
        (VAR VarName=identifier)?                                   // [VAR <VarName>]
        (AS Type=datatype)?                                         // NEW Optional data type
        end=eos
        ;
        */
        VAR la1 := SELF:La1
        VAR la2 := SELF:La2
        _modifiers.Add(SELF:Lt1)
        IF ! SELF:Matches(XSharpLexer.ACCESS, XSharpLexer.ASSIGN)
            RETURN NULL
        ENDIF
        _modifiers.Add(SELF:Lt1)
        DO WHILE SELF:ExpectAny(XSharpLexer.ACCESS, XSharpLexer.ASSIGN)
            NOP
        ENDDO
        VAR isStatic := SELF:ParseXPPMemberModifiers()
        IF ! SELF:Expect(XSharpLexer.METHOD)
            RETURN NULL
        ENDIF
        VAR id := SELF:ParseIdentifier()
        VAR propName := ""
        IF SELF:Expect(XSharpLexer.VAR)
            propName := SELF:ParseIdentifier()
        ENDIF
        VAR type := SELF:ParseDataType(FALSE)
        SELF:GetSourceInfo(_start, SELF:LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xmember := XSourceMemberSymbol{id+XLiterals.XppDeclaration, Kind.Property, _OR(_xppVisibility, _attributes), range, interval, type, _modifiers, isStatic}
        xmember:SourceCode := source

        IF SELF:CurrentType != NULL
            SELF:CurrentType:AddMember(xmember)
        ENDIF
        xmember:SingleLine := TRUE
        RETURN <XSourceEntity> {xmember}


    PRIVATE METHOD ParseXppMethodDeclaration() AS IList<XSourceEntity>
        /*
        xppdeclareMethod    : (Modifiers=xppdeclareModifiers)?                            // [DEFERRED |FINAL | INTRODUCE | OVERRIDE] [CLASS]
        METHOD Methods+=identifier                                   // METHOD <MethodName,...>
        (
        xppisin                                                   //  [IS <Name>] [IN <SuperClass>]
        | (COMMA Methods+=identifier)*                             // or an optional comma seperated list of other names
        )
        eos
        ;
        xppdeclareModifiers : ( Tokens+=( DEFERRED | FINAL | INTRODUCE | OVERRIDE | CLASS | SYNC ) )+
        ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs


        */
        VAR la1      := SELF:La1
        VAR isStatic :=  SELF:Expect(XSharpLexer.CLASS)
        _modifiers.Add(SELF:Lt1)
        IF ! SELF:Expect(XSharpLexer.METHOD)
            RETURN NULL
        ENDIF
        VAR ids := List<STRING>{}
        IF SELF:IsId(SELF:La1)
            ids:Add(SELF:ParseIdentifier())
        ELSE
            RETURN NULL
        ENDIF
        IF SELF:Matches(XSharpLexer.IN,XSharpLexer.IS)
            SELF:ParseXppIsIn()     // Parse but ignore
        ELSE
            DO WHILE SELF:Expect(XSharpLexer.COMMA)
                ids:Add(SELF:ParseIdentifier())
            ENDDO
        ENDIF
        SELF:GetSourceInfo(_start, SELF:LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)
        VAR result := List<XSourceEntity>{}
        FOREACH VAR id IN ids
            VAR xmethod := XSourceMemberSymbol{id+XLiterals.XppDeclaration, Kind.Method, _xppVisibility, range, interval, "", _modifiers, FALSE}
            xmethod:SourceCode := ie"{xmethod.ModVis}: \r\n{ IIF(isStatic, \"STATIC\",\"\")} METHOD {xmethod.Name}"
            IF SELF:CurrentType != NULL
                SELF:CurrentType:AddMember(xmethod)
            ENDIF
            xmethod:SingleLine := TRUE
            result:Add(xmethod)
        NEXT
        RETURN result

    PRIVATE METHOD ParseXppVisibility() AS IList<XSourceEntity>
        /*
        xppmethodvis        : Vis=xppvisibility COLON eos
        ;

        xppvisibility       : Token=(HIDDEN | PROTECTED | EXPORTED | INTERNAL | PUBLIC | PRIVATE )
        ;

        */
        // the visibility is already parsed as 'start of entity'
        //we're now at the COLON token
        VAR result := SELF:Expect(XSharpLexer.COLON)
        IF result
            SELF:ReadLine()
        ENDIF
        SELF:_xppVisibility := _attributes
        RETURN NULL

    PRIVATE METHOD ParseXppIsIn() AS VOID
        /*
        xppisin             : IS Id=identifier (IN SuperClass=identifier)?                //  IS <Name> [IN <SuperClass>]
        | IN SuperClass=identifier								             //  IN <SuperClass> without IS clause
        ;
        */
        // we parse this but the feature is not supported.
        LOCAL id, superclass AS STRING
        id := superclass := ""
        IF SELF:Expect(XSharpLexer.IS)
            id := SELF:ParseIdentifier()
        ENDIF
        IF SELF:Expect(XSharpLexer.IN)
            superclass := SELF:ParseIdentifier()
        ENDIF
        RETURN


    PRIVATE METHOD ParseXppMethod() AS IList<XSourceEntity>
        // detect if we're inside the class definition
        IF SELF:La1 == XSharpLexer.INLINE
            RETURN SELF:ParseXppMethodInLine()
        ELSEIF SELF:InXppClass
            RETURN SELF:ParseXppMethodDeclaration()
        ELSE
            RETURN SELF:ParseXppMethodImplementation()
        ENDIF



    PRIVATE METHOD ParseXppMethodInLine() AS IList<XSourceEntity>
        /*
        xppinlineMethod     : (Attributes=attributes)?                               // NEW Optional Attributes
        I=INLINE
        (Modifiers=xppmemberModifiers)?                        // [CLASS]
        METHOD  Id=identifier                                  // METHOD <MethodName>
        // no type parameters
        (ParamList=parameterList)?                            // Optional Parameters
        (AS Type=datatype)?                                   // NEW Optional return type
        // no type constraints
        (UDCSEP ExpressionBody=expression)?                   // New: Expression Body
        end=eos
        StmtBlk=statementBlock
        (END METHOD eos)?
        ;

        xppmemberModifiers  : ( Tokens+=( CLASS | STATIC) )+
        ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs
        */
        _modifiers.Add(SELF:Lt1)
        IF ! SELF:Expect(XSharpLexer.INLINE)
            RETURN NULL
        ENDIF
        VAR isStatic := SELF:ParseXPPMemberModifiers()
        _modifiers.Add(SELF:Lt1)
        IF ! SELF:Expect(XSharpLexer.METHOD)
            RETURN NULL
        ENDIF
        VAR id      := SELF:ParseIdentifier()
        VAR aParams := SELF:ParseParameterList(FALSE, OUT VAR _)
        VAR type    := ""
        IF SELF:La1 == XSharpLexer.AS
            type := SELF:ParseDataType(FALSE)
        ENDIF
        SELF:GetSourceInfo(_start, SELF:LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xmethod := XSourceMemberSymbol{id, Kind.Method, _xppVisibility, range, interval, type, _modifiers, isStatic}
        xmethod:SourceCode := source
        IF SELF:CurrentType != NULL
            SELF:CurrentType:AddMember(xmethod)
        ENDIF
        IF aParams != NULL
            FOREACH VAR par IN aParams
                xmethod:AddParameter(par)
            NEXT
        ENDIF
        RETURN <XSourceEntity>{xmethod}


    PRIVATE METHOD ParseXPPMemberModifiers() AS LOGIC
        RETURN SELF:ExpectAny(XSharpLexer.CLASS, XSharpLexer.STATIC)

    PRIVATE METHOD ParseXppMethodImplementation() AS IList<XSourceEntity>
        /*
        xppmethod           : (Attributes=attributes)?                              // NEW Optional Attributes
        (MethodType=(ACCESS|ASSIGN))?                         // Optional Access or Assign
        (Modifiers=xppmemberModifiers)?                       // [CLASS]
        M=METHOD (ClassId=identifier COLON)? Id=identifier    // [<ClassName>:] <MethodName>
        // no type parameters
        (ParamList=parameterList)?                            // Optional Parameters
        (AS Type=datatype)?                                   // NEW Optional return type
        // no type constraints
        // no calling convention
        (UDCSEP ExpressionBody=expression)?                   // New: Expression Body
        END=eos
        StmtBlk=statementBlock
        (END METHOD eos)?
        ;

        */
        VAR kind := Kind.Method
        _modifiers.Add(SELF:Lt1)
        IF SELF:ExpectAny(XSharpLexer.ACCESS)
            kind := Kind.Access
        ELSEIF SELF:ExpectAny(XSharpLexer.ASSIGN)
            kind := Kind.Assign
        ENDIF
        VAR isStatic := SELF:ParseXPPMemberModifiers()
        IF ! SELF:Expect(XSharpLexer.METHOD)
            RETURN NULL
        ENDIF
        VAR classprefix := ""
        IF SELF:La1 == XSharpLexer.ID .AND. SELF:La2 == XSharpLexer.COLON
            classprefix := SELF:ParseIdentifier()
            SELF:Expect(XSharpLexer.COLON)
        ENDIF
        VAR id      := SELF:ParseIdentifier()
        VAR aParams := SELF:ParseParameterList(FALSE, OUT VAR _)
        VAR type    := ""
        IF SELF:La1 == XSharpLexer.AS
            type := SELF:ParseDataType(FALSE)
        ENDIF
        SELF:GetSourceInfo(_start, SELF:LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)
        SELF:ReadLine()
        VAR xmethod := XSourceMemberSymbol{id, kind, Modifiers.None, range, interval, type, _modifiers, isStatic}
        // we need to lookup the visibility in the types list
        xmethod:SourceCode := source
        // find the parent in the stack. Either the last one (when the classprefix is empty) or a specific one
        // then find the declared member in this type
        VAR classdef := SELF:FindXPPClass(classprefix)
        IF classdef != NULL
            classdef:AddMember(xmethod)
            FOREACH m AS XSourceMemberSymbol IN classdef:Members
                IF m:Name == xmethod:Name+XLiterals.XppDeclaration
                    xmethod:Attributes := m:Attributes
                    EXIT
                ENDIF
            NEXT
        ELSE
            IF SELF:CurrentType != NULL
                SELF:CurrentType:AddMember(xmethod)
            ENDIF
        ENDIF


        IF aParams != NULL
            FOREACH VAR par IN aParams
                xmethod:AddParameter(par)
            NEXT
        ENDIF
        RETURN <XSourceEntity>{xmethod}

    PRIVATE METHOD FindXPPClass(sName AS STRING) AS XSourceTypeSymbol
        VAR start := SELF:_EntityList:Count-1
        FOR VAR i := start DOWNTO 0
            VAR element := _EntityList[i]
            IF element IS XSourceTypeSymbol VAR typedef
                IF typedef:ClassType == XDialect.XPP
                    IF String.IsNullOrEmpty(sName)
                        RETURN typedef
                    ELSEIF String.Compare(typedef:Name, sName, TRUE) == 0
                        RETURN typedef
                    ENDIF
                ENDIF
            ENDIF
        NEXT
        RETURN NULL

#endregion


    PRIVATE METHOD IsKeywordXs(token AS LONG) AS LOGIC
        SWITCH token
        CASE XSharpLexer.AUTO
        CASE XSharpLexer.CHAR
        CASE XSharpLexer.CONST
        CASE XSharpLexer.DEFAULT
        CASE XSharpLexer.GET
        CASE XSharpLexer.IMPLEMENTS
        CASE XSharpLexer.NEW
        CASE XSharpLexer.OUT
        CASE XSharpLexer.REF
        CASE XSharpLexer.SET
        CASE XSharpLexer.VALUE
        CASE XSharpLexer.VIRTUAL
        CASE XSharpLexer.INTERNAL
        CASE XSharpLexer.ADD
        CASE XSharpLexer.ARGLIST
        CASE XSharpLexer.ASCENDING
        CASE XSharpLexer.ASTYPE
        CASE XSharpLexer.ASYNC
        CASE XSharpLexer.AWAIT
        CASE XSharpLexer.BY
        CASE XSharpLexer.CHECKED
        CASE XSharpLexer.DESCENDING
        CASE XSharpLexer.DYNAMIC
        CASE XSharpLexer.EQUALS
        CASE XSharpLexer.EXTERN
        CASE XSharpLexer.FIXED
        CASE XSharpLexer.FROM
        CASE XSharpLexer.GROUP
        CASE XSharpLexer.INTO
        CASE XSharpLexer.JOIN
        CASE XSharpLexer.LET
        CASE XSharpLexer.NAMEOF
        CASE XSharpLexer.OF
        CASE XSharpLexer.ON
        CASE XSharpLexer.ORDERBY
        CASE XSharpLexer.OVERRIDE
        CASE XSharpLexer.PARAMS
        CASE XSharpLexer.REMOVE
        CASE XSharpLexer.SELECT
        CASE XSharpLexer.UNCHECKED
        CASE XSharpLexer.VAR
        CASE XSharpLexer.VOLATILE
        CASE XSharpLexer.WHERE
        CASE XSharpLexer.CURRENCY
        CASE XSharpLexer.DECIMAL
        CASE XSharpLexer.DATETIME
            // Added as XS keywords to allow them to be treated as IDs
            // the following entity keywords will be never used 'alone' and can therefore be safely defined as identifiers
        CASE XSharpLexer.DELEGATE
        CASE XSharpLexer.ENUM
        CASE XSharpLexer.GLOBAL
        CASE XSharpLexer.INHERIT
        CASE XSharpLexer.STRUCTURE
            // The following 'old' keywords are never used 'alone' and are harmless as identifiers
        CASE XSharpLexer.ALIGN
        CASE XSharpLexer.CALLBACK
        CASE XSharpLexer.CLIPPER
        CASE XSharpLexer.DIM
        CASE XSharpLexer.DOWNTO
        CASE XSharpLexer.DLLEXPORT
        CASE XSharpLexer.FASTCALL
        CASE XSharpLexer.IN
        CASE XSharpLexer.INIT1
        CASE XSharpLexer.INIT2
        CASE XSharpLexer.INIT3
        CASE XSharpLexer.INSTANCE
        CASE XSharpLexer.PASCAL
        CASE XSharpLexer.SEQUENCE
        CASE XSharpLexer.STEP
        CASE XSharpLexer.STRICT
        CASE XSharpLexer.TO
        CASE XSharpLexer.THISCALL
        CASE XSharpLexer.UPTO
        CASE XSharpLexer.USING
        CASE XSharpLexer.TUPLE
        CASE XSharpLexer.WINCALL
            // The following keywords are handled in the fixPositionalKeyword() method of the lexer and will only be keywords at the right place
            // but when they code event->(DoSomething()) we still need them in this rule...
            //        CASE XSharpLexer.DEFINE
            //        CASE XSharpLexer.TRY
            //        CASE XSharpLexer.SWITCH
            //        CASE XSharpLexer.EVENT
            //        CASE XSharpLexer.EXPLICIT
            //        CASE XSharpLexer.FOREACH
            //        CASE XSharpLexer.UNTIL
            //        CASE XSharpLexer.PARAMETERS
            //        CASE XSharpLexer.YIELD
            //        CASE XSharpLexer.MEMVAR
            //        CASE XSharpLexer.NOP
            //        CASE XSharpLexer.PARTIAL
            //        CASE XSharpLexer.SEALED
            //        CASE XSharpLexer.ABSTRACT
            //        CASE XSharpLexer.UNSAFE
            //        CASE XSharpLexer.SCOPE
            //        CASE XSharpLexer.NAMESPACE
            //        CASE XSharpLexer.LOCK
            //        CASE XSharpLexer.IMPLICIT
            //        CASE XSharpLexer.IMPLIED
            //        CASE XSharpLexer.INITONLY
            //        CASE XSharpLexer.PROPERTY
            //        CASE XSharpLexer.INTERFACE
            //        CASE XSharpLexer.VOSTRUCT
            //        CASE XSharpLexer.UNION
            //        CASE XSharpLexer.DECLARE
            //        CASE XSharpLexer.OPERATOR
            RETURN TRUE
        END SWITCH
        // context sensitive keywords
        // ENDCLASS, FREEZE, FINAL, INTRODUCE, SYNC, DEFERRED, INLINE

        RETURN FALSE

    PRIVATE METHOD IsKeywordXpp(token AS LONG) AS LOGIC
        if _dialect != XDialect.XPP
            RETURN FALSE
        ENDIF
        SWITCH token
        CASE XSharpLexer.SHARING
        CASE XSharpLexer.SHARED
        CASE XSharpLexer.ASSIGNMENT
        CASE XSharpLexer.EXPORTED
        CASE XSharpLexer.READONLY
        CASE XSharpLexer.NOSAVE
            RETURN TRUE
        END SWITCH
        RETURN FALSE

    PRIVATE METHOD IsKeywordFox(token AS LONG) AS LOGIC
        if _dialect != XDialect.FoxPro
            RETURN FALSE
        ENDIF
        SWITCH token
        CASE XSharpLexer.OLEPUBLIC
        CASE XSharpLexer.EXCLUDE
        CASE XSharpLexer.THISACCESS
        CASE XSharpLexer.HELPSTRING
        CASE XSharpLexer.NOINIT
        CASE XSharpLexer.FOX_AND
        CASE XSharpLexer.FOX_OR
        CASE XSharpLexer.FOX_NOT
        CASE XSharpLexer.FOX_XOR
        CASE XSharpLexer.THEN
            RETURN TRUE
        END SWITCH
        // These tokens are already marked as 'only valid in a certain context ' in the lexer
        // ENDDEFINE | TEXT| ENDTEXT | DIMENSION | LPARAMETERS | NOSHOW | TEXTMERGE | PRETEXT | FLAGS | ADDITIVE

        RETURN FALSE

    PRIVATE STATIC METHOD GetText(SELF token AS IToken) AS STRING
        VAR result := token:Text
        IF result:StartsWith("@@")
            result := result:Substring(2)
        ENDIF
        RETURN result


    PRIVATE METHOD GetSourceInfo(startToken AS IToken, endToken AS IToken, range OUT TextRange, interval OUT TextInterval, source OUT STRING) AS VOID
        range    := TextRange{startToken, endToken}
        interval := TextInterval{startToken, endToken}
        source   := SELF:GetSource(startToken, endToken)
        RETURN

    PRIVATE METHOD GetSource (startToken AS IToken, endToken AS IToken) AS STRING
        VAR startIndex := ((XSharpToken) startToken):OriginalTokenIndex
        VAR stopIndex  := ((XSharpToken)   endToken):OriginalTokenIndex
        VAR sb         := StringBuilder{}
        FOR VAR i := startIndex TO stopIndex
            sb:Append(_tokens[i].Text)
        NEXT
        RETURN sb:ToString()


    PRIVATE STATIC METHOD Log(cMessage AS STRING) AS VOID
        IF XSettings.EnableParseLog
            XSettings.Information("XParser: "+cMessage)
        ENDIF
        RETURN
END CLASS

    DELEGATE DelEndToken(iToken AS LONG) AS LOGIC



END NAMESPACE











