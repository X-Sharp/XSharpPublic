//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Note that the comment blocks from the various rules have been copied from XSharp.g4 inside the compiler
// Todo: XPP Class rules


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

BEGIN NAMESPACE XSharpModel
   
   
   CLASS XsParser IMPLEMENTS VsParser.IErrorListener
      PRIVATE  _stream       AS BufferedTokenStream 
      PRIVATE  _input        AS IList<IToken>
      PRIVATE  _index        AS LONG
      PRIVATE  _file         AS XFile
      PRIVATE  _usings       AS IList<STRING>
      PRIVATE  _staticusings AS IList<STRING>
      PRIVATE  _EntityList    AS IList<XEntityDefinition>
      PRIVATE  _EntityStack   AS Stack<XEntityDefinition>
      PRIVATE  _BlockList     AS IList<XBlock>
      PRIVATE  _BlockStack    AS Stack<XBlock>
      PRIVATE  _PPBlockStack  AS Stack<XBlock>  
      PRIVATE  _locals        AS IList<XVariable>
      PRIVATE  _lastToken     AS IToken
      PRIVATE  _collectLocals AS LOGIC 
      PRIVATE  _collectBlocks AS LOGIC
	   PRIVATE  _errors        AS IList<XError>
      PRIVATE  _globalType    AS XTypeDefinition
      PRIVATE  _dialect       AS XSharpDialect
      PRIVATE  _xppVisibility AS Modifiers

      PRIVATE  _attributes   AS Modifiers      // for the current entity
      PRIVATE  _start        AS IToken
      PRIVATE  _hasXmlDoc    AS LOGIC
      PRIVATE  _tokens       AS IList<IToken>
      
      PRIVATE PROPERTY CurrentEntity      AS XEntityDefinition GET IIF(_EntityStack:Count > 0, _EntityStack:Peek(), NULL_OBJECT)
      PRIVATE PROPERTY CurrentType        AS XTypeDefinition    
         GET 
            VAR aStack := _EntityStack:ToArray()
            FOREACH VAR item IN aStack
               IF item IS XTypeDefinition VAR type
                  RETURN type
               ENDIF
            NEXT
            RETURN NULL
         END GET
      END PROPERTY
      PRIVATE PROPERTY CurrentBlock       AS XBlock   GET IIF(_BlockStack:Count > 0, _BlockStack:Peek(), NULL_OBJECT)
      PRIVATE PROPERTY CurrentEntityKind  AS Kind     GET IIF(_EntityStack:Count > 0, CurrentEntity:Kind, Kind:Unknown)
      PRIVATE PROPERTY La1 AS INT GET SELF:La(1)
      PRIVATE PROPERTY La2 AS INT GET SELF:La(2)
      PRIVATE PROPERTY La3 AS INT GET SELF:La(3)
      PRIVATE PROPERTY Lt1 AS IToken GET SELF:Lt(1)
      PRIVATE PROPERTY Lt2 AS IToken GET SELF:Lt(2)
      PRIVATE PROPERTY LastToken AS IToken GET _lastToken
      PRIVATE PROPERTY InFoxClass AS LOGIC GET CurrentType != NULL .AND. CurrentType:ClassType == XSharpDialect.FoxPro 
      PRIVATE PROPERTY InXppClass AS LOGIC GET CurrentType != NULL .AND. CurrentType:ClassType == XSharpDialect.XPP 
      PROPERTY EntityList AS IList<XEntityDefinition>  GET _EntityList
      PROPERTY BlockList  AS IList<XBlock>    GET _BlockList
      PROPERTY Locals     AS IList<XVariable> GET _locals
      
      
      CONSTRUCTOR(oFile AS XFile, dialect AS XSharpDialect)
         _errors        := List<XError>{}
         _usings        := List<STRING>{}
         _staticusings  := List<STRING>{}
         _EntityList    := List<XEntityDefinition>{}
         _EntityStack   := Stack<XEntityDefinition>{}
         _BlockList     := List<XBlock>{}
         _BlockStack    := Stack<XBlock>{}
         _PPBlockStack  := Stack<XBlock>{}
         _file          := oFile
         _dialect       := dialect
         _locals        := List<XVariable>{}
         _file:InitTypeList()
         _globalType    := _file:GlobalType
         _globalType:ClearMembers()
         _EntityStack:Push(_globalType)

      
      #region IErrorListener
      METHOD ReportError(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
         SELF:_errors:Add(XError{fileName, span, errorCode, message, args})

      METHOD ReportWarning(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
         SELF:_errors:Add(XWarning{fileName, span, errorCode, message, args})
      #endregion

      METHOD Parse(lBlocks AS LOGIC, lLocals AS LOGIC) AS VOID
         VAR cSource  := System.IO.File.ReadAllText(_file:SourcePath) 
         VAR options  := XSharpParseOptions.Default
         XSharp.Parser.VsParser.Lex(cSource, SELF:_file:SourcePath, options, SELF, OUT VAR stream)
         SELF:Parse(stream, lBlocks, lLocals)
         RETURN 
    
      METHOD Parse( tokenStream AS ITokenStream, lBlocks AS LOGIC, lLocals AS LOGIC) AS VOID
         LOCAL aAttribs        AS IList<IToken>
         LOCAL cXmlDoc   := "" AS STRING
         Log(i"Start")
         
         _collectLocals := lLocals
         _collectBlocks := lBlocks
         _stream        := (BufferedTokenStream) tokenStream 
         _tokens        := _stream:GetTokens()
         
         _input         := _tokens:Where({ t => t:Channel == XSharpLexer.DefaultTokenChannel .OR. t:Channel == XSharpLexer.PREPROCESSORCHANNEL }):ToArray()
         _hasXmlDoc     := _tokens:FirstOrDefault({t => t:Channel == XSharpLexer.XMLDOCCHANNEL }) != NULL
         _index := 0
         _lastToken     := _tokens:FirstOrDefault()

         DO WHILE ! SELF:Eoi()
            VAR tokenBefore := LastToken
            VAR first := SELF:Lt1
            IF ParsePPLine()
               LOOP
            ENDIF
            IF SELF:ParseUsing()
               LOOP
            ENDIF
            aAttribs := SELF:ParseAttributes()
            VAR mods := SELF:ParseVisibilityAndModifiers()
            VAR vis  := _AND(mods, Modifiers.VisibilityMask)
            IF IsStartOfEntity(OUT VAR entityKind, mods)
               IF _hasXmlDoc
                  cXmlDoc := GetXmlDoc( (XSharpToken) first)
               ENDIF
               // note: do not set this before the IsStartOfEntity check to make sure that 
               // single identifiers on a line are not matched with the ClassVar rule
               IF vis == Modifiers.None
                  mods |= Modifiers.Public
               ENDIF
               SELF:_attributes  := mods
               SELF:_start := first
               VAR entities := SELF:ParseEntity(entityKind)
               IF entities != NULL
                  
                  FOREACH VAR entity IN entities
                     IF entity == NULL
                        LOOP
                     ENDIF
                     entity:File := _file
                     IF _hasXmlDoc
                        entity:XmlComments := cXmlDoc
                     ENDIF
                     IF aAttribs?:Count > 0
                        entity:CustomAttributes := TokensAsString(aAttribs)
                     ENDIF
                     VAR lastEntity := _EntityList:LastOrDefault()
                     IF lastEntity != NULL
                        lastEntity:Range       := lastEntity:Range:WithEnd(tokenBefore)
                        lastEntity:Interval    := lastEntity:Interval:WithEnd(tokenBefore)
                     ENDIF
                     _EntityList:Add(entity)
                  
                     IF _EntityStack:Count > 0 .AND. (!CurrentEntityKind:HasMembers() .OR. !(CurrentEntity IS XTypeDefinition ) )
                        _EntityStack:Pop()
                     ENDIF
                     IF entity:Kind:IsGlobalType() .AND. entity IS XMemberDefinition VAR xGlobalMember
                        SELF:_globalType:AddMember(xGlobalMember)
                     ELSE
                        IF CurrentEntityKind:HasMembers() .AND. CurrentEntity IS XTypeDefinition VAR xEnt
                           IF entity IS XMemberDefinition VAR xMember .AND. xMember:Parent == NULL
                              xEnt:AddMember( xMember )
                           ENDIF
                        ENDIF
                        IF CurrentEntityKind:HasChildren() .AND. CurrentEntity IS XTypeDefinition VAR xTypeDef
                           IF entity IS XTypeDefinition VAR xChild .AND. ! XTypeDefinition.IsGlobalType(xTypeDef)
                              xTypeDef:AddChild( xChild )
                              xChild:Namespace := xTypeDef:FullName
                           ENDIF
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
            ELSEIF IsEndOfEntity( OUT VAR endKind)
               VAR type := SELF:La2
               // match la2 with current entity
               DO WHILE _EntityStack:Count > 0
                  VAR top := _EntityStack:Pop()
                  IF top:Kind != endKind
                     top:Range       := top:Range:WithEnd(tokenBefore)
                     top:Interval    := top:Interval:WithEnd(tokenBefore)
                     LOOP
                  ENDIF
                  top:Range       := top:Range:WithEnd(SELF:Lt2)
                  top:Interval    := top:Interval:WithEnd(SELF:Lt2)
                  EXIT
               ENDDO
               SELF:ReadLine()
               
            ELSEIF ParseBlock()
               NOP               
            ELSE
               SELF:ParseStatement() 
            ENDIF
         ENDDO
         VAR types := SELF:_EntityList:Where( {x => x IS XTypeDefinition})
         VAR typelist := Dictionary<STRING, XTypeDefinition>{System.StringComparer.InvariantCultureIgnoreCase}
         typelist:Add(_globalType:Name, _globalType)
         LOCAL last  := NULL AS XTypeDefinition
         FOREACH type AS XTypeDefinition IN types
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
            IF type:Name:Contains(".")
               VAR pos := type:Name:LastIndexOf(".")
               VAR ns  := type:Name:Substring(0, pos)
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
            ENDIF
            last := type
         NEXT
         IF last != NULL .AND. last:Range:StartLine == last:Range:EndLine
            // adjust the end of the type with the start of the current line
            // find the last token in the stream
            VAR index := _tokens.Count -1
            VAR token := _tokens[index]
            last:Range     := last:Range:WithEnd(token)
            last:Interval  := last:Interval:WithEnd(token)
         ENDIF
         Log(i"Completed, found {_EntityList.Count} entities and {typelist.Count} types")
         IF SELF:_EntityList:Count > 0
            VAR lastEntity          := SELF:_EntityList:Last()
            lastEntity:Range        := lastEntity:Range:WithEnd(LastToken)
            lastEntity:Interval     := lastEntity:Interval:WithEnd(LastToken)
         ENDIF
         IF ! lLocals
            _file:SetTypes(typelist, _usings, _staticusings, SELF:_EntityList)
            _file:SaveToDatabase()
         ENDIF
         
      
      PRIVATE METHOD GetXmlDoc(startToken AS XSharpToken) AS STRING
         VAR sb         := StringBuilder{}
         VAR startindex := startToken:OriginalTokenIndex
         startindex     -= 1
         DO WHILE startindex >= 0
            VAR token := _tokens[startindex]
            SWITCH token:Channel
            CASE XSharpLexer.XMLDOCCHANNEL
               VAR text := token.Text.Substring(3)
               sb:Insert(0, text + e"\r\n")
            CASE XSharpLexer.DefaultTokenChannel
               // exit the loop
               startindex := 0
            END SWITCH
            startindex -= 1
         ENDDO
         VAR result := sb:ToString()
         IF result:Length > 0
            result := "<doc>"+result+"</doc>"
         ENDIF
         RETURN result
      
         
      PRIVATE METHOD ParsePPLine() AS LOGIC
         VAR token := SELF:La1
         SWITCH SELF:La1
         CASE XSharpLexer.PP_REGION
         CASE XSharpLexer.PP_IFDEF
         CASE XSharpLexer.PP_IFNDEF
            VAR block := XBlock{ SELF:Lt1, SELF:Lt2}
            _BlockList:Add(block)
            _PPBlockStack:Push(block)
         CASE XSharpLexer.PP_ENDREGION
         CASE XSharpLexer.PP_ENDIF
               // end
            IF _PPBlockStack:Count > 0
               _PPBlockStack:Peek():Children:Add( XBlock{SELF:Lt1,SELF:Lt2})
               _PPBlockStack:Pop()
            ENDIF
         CASE XSharpLexer.PP_ELSE
               // middle
            IF _PPBlockStack:Count > 0
               _PPBlockStack:Peek():Children:Add( XBlock{SELF:Lt1,SELF:Lt2})
            ENDIF
         OTHERWISE
            RETURN FALSE
         END SWITCH
         SELF:ReadLine()
         RETURN TRUE
         
      PRIVATE METHOD ParseUsing() AS LOGIC
/*
using_              : USING (Static=STATIC)? (Alias=identifierName Op=assignoperator)? Name=name EOS
                    ;

*/         
         
         IF SELF:La1 != XSharpLexer.USING
            RETURN FALSE
         ENDIF
         VAR startToken := SELF:ConsumeAndGet()
         VAR isStatic := FALSE
         VAR alias := ""
         IF Expect(XSharpLexer.STATIC)
            isStatic := TRUE
         ENDIF
         IF IsId(SELF:La1) .AND. SELF:IsAssignOp(SELF:La2)
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
         SELF:ReadLine()
         RETURN TRUE
         
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
         
         
      PRIVATE METHOD IsModifier(num AS LONG) AS LOGIC
            SWITCH num
               // Visibility Alphabetical
            CASE XSharpLexer.EXPORT
            CASE XSharpLexer.HIDDEN
            CASE XSharpLexer.INTERNAL
            CASE XSharpLexer.PRIVATE
            CASE XSharpLexer.PROTECTED
            CASE XSharpLexer.PUBLIC

            // Real modifiers Alphabetical
            CASE XSharpLexer.ABSTRACT
            CASE XSharpLexer.ASYNC
            CASE XSharpLexer.CONST
            CASE XSharpLexer.EXTERN
            CASE XSharpLexer.INITONLY
            CASE XSharpLexer.INSTANCE
            CASE XSharpLexer.NEW
            CASE XSharpLexer.OVERRIDE
            CASE XSharpLexer.PARTIAL
            CASE XSharpLexer.SEALED
            CASE XSharpLexer.STATIC
            CASE XSharpLexer.UNSAFE
            CASE XSharpLexer.VIRTUAL
            CASE XSharpLexer.VOLATILE
               
            // XPP modifiers
            CASE XSharpLexer.DEFERRED
            CASE XSharpLexer.FINAL
            CASE XSharpLexer.FREEZE
            CASE XSharpLexer.INTRODUCE
            CASE XSharpLexer.SYNC
               RETURN TRUE
            END SWITCH
         RETURN FALSE
      PRIVATE METHOD ParseVisibilityAndModifiers() AS   Modifiers
         VAR result := Modifiers.None
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
               entityKind := Kind.Namespace
            ENDIF
         CASE XSharpLexer.CLASS
            IF SELF:InXppClass
               IF SELF:La2 == XSharpLexer.METHOD  .OR. SELF:IsModifier(SELF:La2)   // XPP has CLASS METHOD = a static method
                  entityKind := Kind.Method
               ELSEIF SELF:La2 == XSharpLexer.VAR  
                  entityKind := Kind.Field
               ENDIF
            ELSE
               entityKind := Kind.Class
            ENDIF
         CASE XSharpLexer.STRUCTURE
            entityKind := Kind.Structure
         CASE XSharpLexer.DELEGATE
            entityKind := Kind.Delegate
         CASE XSharpLexer.INTERFACE
            entityKind := Kind.Interface
         CASE XSharpLexer.ENUM
            entityKind := Kind.Enum
         CASE XSharpLexer.EVENT
            entityKind := Kind.Event
         CASE XSharpLexer.METHOD
            entityKind := Kind.Method
         CASE XSharpLexer.ACCESS
            entityKind := Kind.Access
         CASE XSharpLexer.ASSIGN
            entityKind := Kind.Assign
         CASE XSharpLexer.PROPERTY
            entityKind := Kind.Property
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
            ELSEIF _dialect == XSharpDialect.FoxPro
               entityKind := Kind.Class
            ENDIF
         CASE XSharpLexer.VOSTRUCT
            entityKind := Kind.VOStruct
         CASE XSharpLexer.UNION
            entityKind := Kind.Union
         CASE XSharpLexer.MEMBER
            entityKind := Kind.EnumMember
         CASE XSharpLexer.ADD
            // Todo handle Add Object clause inside Class.
            IF SELF:La2 == XSharpLexer.OBJECT
               entityKind := Kind.Field
            ENDIF
         CASE XSharpLexer.FUNCTION
            entityKind := Kind.Function
         CASE XSharpLexer.PROCEDURE
            entityKind := Kind.Procedure
         CASE XSharpLexer.GLOBAL
            entityKind := Kind.VOGlobal
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
         // we do no check for InXPPClass because this will
         // fail when partially parseing for locals lookup
         CASE XSharpLexer.VAR
            IF _dialect == XSharpDialect.XPP
               entityKind := Kind.Field // Not really a field but handled later
            ENDIF
            
         CASE XSharpLexer.COLON
            IF _dialect == XSharpDialect.XPP .AND. _AND(mods, Modifiers.VisibilityMask) != Modifiers.None
               entityKind := Kind.Field // Not really a field but handled later
            ENDIF
         CASE XSharpLexer.INLINE
            IF _dialect == XSharpDialect.XPP
               entityKind := Kind.Method // Not really a field but handled later
            ENDIF
            
         OTHERWISE
            IF IsId(SELF:La1) 
               IF mods != Modifiers.None
                  LOCAL parent AS XTypeDefinition
                  IF CurrentEntity IS XTypeDefinition 
                     parent := (XTypeDefinition) CurrentEntity
                  ELSEIF CurrentEntity != NULL .AND. CurrentEntity:Parent IS XTypeDefinition 
                     parent := (XTypeDefinition) CurrentEntity:Parent 
                  ENDIF
                  IF ! XTypeDefinition.IsGlobalType(parent) 
                     entityKind := Kind.Field
                  ENDIF
                  // PRIVATE and PUBLIC as memvar declarator
                  // when inside a method or function
                  IF mods == Modifiers.Public .OR. mods == Modifiers.Private
                     IF CurrentEntity IS XMemberDefinition VAR xDef .AND. ! xDef:SingleLine .AND. CurrentEntity.Kind:HasBody()
                        entityKind := Kind.Unknown
                     ENDIF
                  ENDIF
               ELSEIF InFoxClass .AND. (CurrentEntity:Kind == Kind.Class .OR. CurrentEntity:Kind == Kind.Field)
                  IF SELF:La1 == XSharpLexer.ID .AND. IsAssignOp(SELF:La2) 
                     entityKind := Kind.Field
                  ELSEIF SELF:La1 == XSharpLexer.ID .AND. SELF:Lt1:Text:EndsWith("COMATTRIB", StringComparison.OrdinalIgnoreCase)
                     entityKind := Kind.Field
                  ENDIF
               ENDIF
            ENDIF   
            
         END SWITCH
         
         RETURN entityKind != Kind.Unknown
         
         
      PRIVATE METHOD ParseBlock() AS LOGIC
         // Adds, updates or removes block token on the block tokens stack
         // Start of block is also added to the _BlockList
         LOCAL nStart  := 0 AS LONG
         LOCAL nMiddle := 0 AS LONG
         LOCAL nEnd    := 0 AS LONG
         SWITCH SELF:La1
         CASE XSharpLexer.BEGIN
            SWITCH SELF:La2
            // tokens in the same order as the rules inside xsharp.g4                    
            CASE XSharpLexer.SEQUENCE
            CASE XSharpLexer.LOCK
            CASE XSharpLexer.SCOPE
            CASE XSharpLexer.SWITCH
            CASE XSharpLexer.USING
            CASE XSharpLexer.UNSAFE
            CASE XSharpLexer.CHECKED
            CASE XSharpLexer.UNCHECKED
            CASE XSharpLexer.FIXED
               nStart := 2
            END SWITCH
            
         CASE XSharpLexer.DO
            SWITCH SELF:La2
               CASE XSharpLexer.WHILE
               CASE XSharpLexer.CASE
               CASE XSharpLexer.SWITCH
               nStart := 2
            END SWITCH
            
            CASE XSharpLexer.IF
            CASE XSharpLexer.FOR
            CASE XSharpLexer.FOREACH
            CASE XSharpLexer.REPEAT
            CASE XSharpLexer.TRY
            CASE XSharpLexer.WITH
            CASE XSharpLexer.TEXT
            CASE XSharpLexer.SWITCH  
            CASE XSharpLexer.GET  
            CASE XSharpLexer.SET  
            CASE XSharpLexer.ADD  
            CASE XSharpLexer.REMOVE  
               nStart := 1
            // Middle of a block                    
         CASE XSharpLexer.CASE
         CASE XSharpLexer.OTHERWISE
         CASE XSharpLexer.ELSEIF
         CASE XSharpLexer.ELSE
         CASE XSharpLexer.CATCH
         CASE XSharpLexer.FINALLY
            nMiddle := 1
         CASE XSharpLexer.RECOVER 
            IF SELF:La2 == XSharpLexer.USING
               nMiddle := 2
            ENDIF
            // End of a block
         CASE XSharpLexer.ENDCASE
         CASE XSharpLexer.ENDDO
         CASE XSharpLexer.ENDIF
         CASE XSharpLexer.NEXT      // Also covers ENDFOR
         CASE XSharpLexer.UNTIL
            nEnd := 1
         CASE XSharpLexer.END
               SWITCH SELF:La2
               // tokens in the same order as the rules inside xsharp.g4                    
               CASE XSharpLexer.SEQUENCE
               CASE XSharpLexer.LOCK
               CASE XSharpLexer.SCOPE
               CASE XSharpLexer.SWITCH
               CASE XSharpLexer.USING
               CASE XSharpLexer.UNSAFE
               CASE XSharpLexer.CHECKED
               CASE XSharpLexer.UNCHECKED
               CASE XSharpLexer.FIXED
               CASE XSharpLexer.TRY
               CASE XSharpLexer.WITH
               
               CASE XSharpLexer.GET        // Accessors
               CASE XSharpLexer.SET  
               CASE XSharpLexer.ADD  
               CASE XSharpLexer.REMOVE  
                     nEnd := 2
                    
               CASE XSharpLexer.EOS        // End without following keyword is usually also allowed
                  nEnd := 1
               END SWITCH
          END SWITCH
         IF nStart > 0
            IF SELF:_collectBlocks
               VAR block := XBlock{ SELF:Lt1, IIF(nStart == 1, SELF:Lt1, SELF:Lt2)}
               _BlockList:Add(block)
               _BlockStack:Push(block)
            ENDIF
            IF SELF:_collectLocals
              IF SELF:La1 == XSharpLexer.SET .OR. SELF:La1 == XSharpLexer.REMOVE .OR. SELF:La1 == XSharpLexer.ADD
                  // Add value token inside accessors
                  VAR id := "Value"
                  VAR strType := SELF:CurrentEntity:TypeName
                  VAR start := SELF:Lt1
                  VAR stop  := SELF:Lt2
                  SELF:GetSourceInfo(start, stop, OUT VAR range, OUT VAR interval, OUT VAR _)  
                  VAR xVar := XVariable{SELF:CurrentEntity, id, range, interval, strType} 
                  
                  SELF:_locals:Add(xVar)
               ENDIF
               IF SELF:La1 == XSharpLexer.IF 
                  SELF:ParseForLocals()
               ELSE
                  SELF:ParseForBlockLocals()
               ENDIF
            ELSE
               SELF:ReadLine()
            ENDIF
            RETURN TRUE
         ELSEIF nMiddle > 0
            // Do something
            IF SELF:_collectBlocks .AND. _BlockStack:Count > 0
               CurrentBlock:Children:Add( XBlock{SELF:Lt1,IIF(nMiddle == 1, SELF:Lt1, SELF:Lt2)})
            ENDIF
            SWITCH SELF:La1
            CASE XSharpLexer.CATCH
               IF SELF:_collectLocals
                  IF SELF:IsId(SELF:La2) 
                     SELF:Consume()
                     VAR start   := SELF:Lt1
                     VAR id      := SELF:ParseIdentifier()
                     VAR strType := "System.Exception"
                     IF SELF:La1 == XSharpLexer.AS
                         strType := SELF:ParseAsIsType()
                     ENDIF
                     
                     SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
                     VAR xVar := XVariable{SELF:CurrentEntity, id, range, interval, strType} 
                     SELF:_locals:Add(xVar)
                  ENDIF
               ENDIF
            CASE XSharpLexer.ELSEIF 
            CASE XSharpLexer.CASE
                IF SELF:_collectLocals
                     SELF:ParseForLocals()
                ENDIF
            END SWITCH
            SELF:ReadLine()
            RETURN TRUE
         ELSEIF nEnd > 0
            IF SELF:_collectBlocks .AND. _BlockStack:Count > 0
               CurrentBlock:Children:Add( XBlock{SELF:Lt1,IIF(nEnd == 1, SELF:Lt1, SELF:Lt2)})
               _BlockStack:Pop()
            ENDIF
            SELF:ReadLine()
            RETURN TRUE
         ENDIF
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

         CASE XSharpLexer.UDC_KEYWORD
            IF String.Equals(SELF:Lt1.Text, "ENDFUNC", StringComparison.OrdinalIgnoreCase)
               IF InFoxClass
                  EntityKind := Kind.Method
               ELSE
                  EntityKind := Kind.Function
               ENDIF
               RETURN TRUE
            ELSEIF String.Equals(SELF:Lt1.Text, "ENDPROC",StringComparison.OrdinalIgnoreCase)
               IF InFoxClass
                  EntityKind := Kind.Method
               ELSE
                  EntityKind := Kind.Procedure
               ENDIF
               RETURN TRUE
            ENDIF
         END SWITCH
         EntityKind := Kind.Unknown
         RETURN FALSE
         
         
         
         
         #region GetTokens
      PRIVATE METHOD La(nToken AS LONG) AS LONG
         nToken += _index-1
         IF (nToken < _input:Count)
            VAR t := _input[nToken]
            RETURN t:Type
         ENDIF
         RETURN XSharpLexer.Eof
         
      PRIVATE METHOD Lt(nToken AS LONG) AS IToken
         nToken += _index-1
         IF (nToken < _input:Count)
            RETURN _input[nToken]
         ENDIF
         RETURN NULL
         
      PRIVATE METHOD Eoi() AS LOGIC
         RETURN _index >= _input:Count
         
      PRIVATE METHOD Eos() AS LOGIC
         RETURN SELF:La1 == XSharpLexer.EOS .OR. Eoi()
         
      PRIVATE METHOD SaveLastToken() AS VOID
         IF ! SELF:Eoi() .AND. SELF:La1 != XSharpLexer.EOS
            _lastToken := _input[_index]
         ENDIF
         
         
      PRIVATE METHOD PushBack() AS VOID
         _index-= 1
         RETURN


      PRIVATE METHOD Consume() AS VOID
         SELF:SaveLastToken()
         _index+= 1
         RETURN
         
      PRIVATE METHOD ConsumeAndGet() AS IToken
         VAR t := _input[_index]
         _lastToken := t
         _index+= 1
         RETURN t
         
      PRIVATE METHOD ConsumeAndGetText() AS STRING
         VAR t := _input[_index]
         _lastToken := t
         _index+= 1
         RETURN t:GetText()
         
      PRIVATE METHOD Matches(nTypes PARAMS LONG[]) AS LOGIC
         IF nTypes:Contains(SELF:La1)
            RETURN TRUE
         ENDIF
         RETURN FALSE
         
      PRIVATE METHOD ConsumeAndGetAny(nTypes PARAMS LONG[]) AS IToken
         IF nTypes:Contains(SELF:La1)
            RETURN SELF:ConsumeAndGet()
         ENDIF
         RETURN NULL
         
         
         
      PRIVATE METHOD Expect(nType AS LONG) AS LOGIC
         IF SELF:La1 == nType
            SELF:Consume()
            RETURN TRUE
         ENDIF
         RETURN FALSE
         
      PRIVATE METHOD ExpectAny(nTypes PARAMS LONG[]) AS LOGIC
         IF nTypes:Contains(SELF:La1)
            SELF:Consume()
            RETURN TRUE
         ENDIF
         RETURN FALSE
     
      PRIVATE METHOD ExpectAssign() AS LOGIC
         RETURN SELF:ExpectAny(XSharpLexer.ASSIGN_OP, XSharpLexer.EQ)
      
      PRIVATE METHOD ExpectAndGet(nType AS LONG, t OUT IToken) AS LOGIC
         IF SELF:La1 == nType
            t := SELF:ConsumeAndGet()
            RETURN TRUE
         ENDIF
         t := NULL
         RETURN FALSE
         
         
      PRIVATE METHOD ReadLine() AS VOID
         DO WHILE ! SELF:Eos()
            SELF:Consume()
         ENDDO
         DO WHILE SELF:La1 == XSharpLexer.EOS
            SELF:Consume() // Consume the EOS
         ENDDO
         RETURN
         #endregion
      
      PRIVATE METHOD IsId(token AS LONG) AS LOGIC
         IF token == XSharpLexer.ID .OR. token == XSharpLexer.KWID
            RETURN TRUE
         ENDIF
         // Soft keywords need to be 
         RETURN SELF:IsKeywordXs(token) .OR. SELF:IsKeywordFox(token) .OR. SELF:IsKeywordXpp(token)
         
         
      PRIVATE METHOD ParseIdentifier() AS STRING
         IF SELF:IsId(SELF:La1)
            IF SELF:La1 == XSharpLexer.KWID
               RETURN SELF:ConsumeAndGetText():Substring(2)
            ELSE
               RETURN SELF:ConsumeAndGetText()
            ENDIF
         ENDIF
         RETURN ""
         
         
      PRIVATE METHOD ParseOptionalClassClause() AS STRING
         IF SELF:La1 == XSharpLexer.CLASS
            SELF:Consume()
            RETURN SELF:ParseQualifiedName()
         ENDIF
         RETURN ""
         
         
      PRIVATE METHOD ParseQualifiedName() AS STRING
         LOCAL result := "" AS STRING
         VAR Tokens := List<IToken>{}
         IF SELF:La1 == XSharpLexer.ID .OR. SELF:La1 == XSharpLexer.KWID
            Tokens:Add(SELF:ConsumeAndGet())
            DO WHILE SELF:La1 == XSharpLexer.DOT .AND.  SELF:IsId(SELF:La2) .AND. ! SELF:Eos()
               Tokens:Add(SELF:ConsumeAndGet())
               Tokens:Add(SELF:ConsumeAndGet())
            ENDDO
         ENDIF
         RETURN TokensAsString(Tokens)
         
      PRIVATE METHOD TokensAsString(tokens AS IList<IToken>) AS STRING
         LOCAL sb AS StringBuilder
         LOCAL last := NULL AS IToken
         IF (tokens == NULL)
            RETURN ""
         ENDIF
         sb := StringBuilder{}
         
         FOREACH VAR t IN tokens
            IF last != NULL
               IF t:StartIndex > last:StopIndex +1
                  sb:Append(" ")
               ENDIF
            ENDIF
            sb:Append(t:GetText())
            last := t
         NEXT
         RETURN sb:ToString()
         
      PRIVATE METHOD ParseEntity(entityKind AS Kind) AS IList<XEntityDefinition>
         LOCAL result AS IList<XEntityDefinition>
         SWITCH entityKind
         CASE Kind.Ignore
            SELF:ReadLine()
         CASE Kind.Namespace
            result := SELF:ParseNamespace()                 
         CASE Kind.Class
            IF SELF:La1 == XSharpLexer.DEFINE
               result := SELF:ParseFoxClass()
            ELSE
               IF _dialect == XSharpDialect.XPP .AND. SELF:HasXppEndClass()
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
            IF InXppClass
               result := SELF:ParseXppProperty()
            ELSE
               result := SELF:ParseMethod()
            ENDIF
         CASE Kind.Assign
            IF InXppClass
               result := SELF:ParseXppProperty()
            ELSE
               result := SELF:ParseMethod()
            ENDIF
         CASE Kind.Method
            IF SELF:_dialect == XSharpDialect.XPP
               result := SELF:ParseXppMethod()
            ELSE
               result := SELF:ParseMethod()
            ENDIF
         CASE Kind.Function
         CASE Kind.Procedure
            IF InFoxClass
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
            IF InXppClass
               IF SELF:La1 == XSharpLexer.COLON
                  result := SELF:ParseXppVisibility()
               ELSE
                  result := SELF:ParseXppClassVars()
               ENDIF
            ELSEIF InFoxClass
               result := SELF:ParseFoxFields()   
            ELSE
               result := SELF:ParseClassVars()   
            ENDIF
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
               ELSEIF SELF:IsModifier(iLa) .AND. SELF:La(iCurrent+1) == XSharpLexer.COLON
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
         
         PRIVATE METHOD ParseFuncProc() AS IList<XEntityDefinition>
/*
funcproc      : (Attributes=attributes)? (Modifiers=funcprocModifiers)?   
              T=(FUNCTION|PROCEDURE) Sig=signature
              InitExit=(INIT1|INIT2|INIT3|EXIT)?                        
              vodummyclauses
              end=eos   
              StmtBlk=statementBlock
              (END T2=(FUNCTION|PROCEDURE)   EOS )?
              ;
*/
            LOCAL kind AS Kind 
            IF SELF:La1 == XSharpLexer.FUNCTION 
               kind := Kind.Function
            ELSEIF SELF:La1 == XSharpLexer.PROCEDURE
               kind := Kind.Procedure
            ELSE
               RETURN NULL
            ENDIF
            SELF:Consume()
            VAR sig := SELF:ParseSignature()
            
            VAR initexit := ""
            IF SELF:Matches(XSharpLexer.INIT1,XSharpLexer.INIT2,XSharpLexer.INIT3,XSharpLexer.EXIT)
               initexit := SELF:ConsumeAndGetText()
            ENDIF
            SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
            SELF:ReadLine()
            
            VAR xMember := XMemberDefinition{sig, kind, _attributes, range, interval, _attributes:HasFlag(Modifiers.Static)}
            xMember:SourceCode := source
            xMember:File := SELF:_file
            RETURN <XEntityDefinition>{xMember}        
            
         PRIVATE METHOD ParseNamespace() AS IList<XEntityDefinition>
/*
namespace_          : BEGIN NAMESPACE Name=name e=eos
                      (Entities+=entity)*
                      END NAMESPACE EOS
                    ;
*/         
            IF SELF:La1 != XSharpLexer.BEGIN .AND. SELF:La2 != XSharpLexer.NAMESPACE
               RETURN NULL
            ENDIF
            SELF:Consume()   // BEGIN
            SELF:Consume()   // NAMESPACE 
            VAR id := SELF:ParseQualifiedName()
            SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
            SELF:ReadLine()
            VAR xType := XTypeDefinition{id, Kind.Namespace, _attributes, range, interval,_file}
            xType:SourceCode := source
            RETURN <XEntityDefinition>{xType}
            
         PRIVATE METHOD ParseTypeDef() AS IList<XEntityDefinition>
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
            SELF:Consume()

            LOCAL constraints   AS List<STRING>
            LOCAL parentType    AS STRING
            LOCAL interfaces    AS List<STRING>
            // read Id with optional namespace prefix
            VAR id := SELF:ParseQualifiedName()
            VAR typePars := SELF:ParseTypeParameters()
            // get inherit clause
            IF ExpectAny(XSharpLexer.INHERIT, XSharpLexer.COLON)
               parentType := SELF:ParseTypeName()
            ENDIF
            // get implements clause + list of interfaces
            IF SELF:La1 == XSharpLexer.IMPLEMENTS
               interfaces := List<STRING>{}
               DO WHILE ExpectAny(XSharpLexer.IMPLEMENTS, XSharpLexer.COMMA)
                  interfaces:Add(ParseTypeName())
               ENDDO
            ENDIF
            DO WHILE SELF:La1 == XSharpLexer.WHERE .AND. ! SELF:Eos()
               IF constraints == NULL
                  constraints := List<STRING>{}
               ENDIF
               constraints:Add(SELF:ParseTypeParameterConstraints())
            ENDDO
            // read to EndOfLine
            SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
            SELF:ReadLine()
            
            VAR xType := XTypeDefinition{id, kind, _attributes, range, interval, _file}
            xType:SourceCode := source

            IF interfaces?:Count > 0
               FOREACH VAR sInterface IN interfaces
                  xType:AddInterface(sInterface)
               NEXT
            ENDIF
            IF ! String.IsNullOrEmpty(parentType)
               xType:BaseType := parentType
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
            IF CurrentEntity != _globalType .AND. CurrentEntityKind:HasChildren()
               xType:Parent := SELF:CurrentEntity
            ENDIF
            
            RETURN <XEntityDefinition>{xType}

         PRIVATE METHOD ParseDelegate() AS IList<XEntityDefinition>
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
            IF ! Expect(XSharpLexer.DELEGATE)
               RETURN NULL
            ENDIF

            VAR sig      := SELF:ParseSignature()
            SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
            VAR xType    := XTypeDefinition{sig:Id, Kind.Delegate,_attributes, range, interval,_file}{SingleLine := TRUE}
            xType:SourceCode := source
            xType:TypeName := sig:DataType
            xType:File     := _file
            VAR xMember  := XMemberDefinition{sig, Kind.Delegate, _attributes, ;
                              range, interval,_attributes:HasFlag(Modifiers.Static)} {SingleLine := TRUE}
                              xType:AddMember(xMember)
            xMember:SourceCode := source
            xType:AddMember(xMember)
            xMember:File       := _file
            RETURN <XEntityDefinition>{xType}
            #endregion
            
         #region ClassMembers
      PRIVATE METHOD ParseMethod(  ) AS IList<XEntityDefinition>
/*
// method rule used inside and outside class members rule 
method              : (Attributes=attributes)? (Modifiers=memberModifiers)?
                      T=methodtype (ExplicitIface=nameDot)? Sig=signature
                      (CLASS (Namespace=nameDot)? ClassId=identifier)?      // Class Clause needed when entity and allowed when class member
                      vodummyclauses
                      end=eos
                      StmtBlk=statementBlock
                      (END T2=methodtype EOS)?
                    ;

methodtype          : Token=(METHOD | ACCESS | ASSIGN )
                    ;

*/         
         LOCAL kind AS Kind 
         IF SELF:La1 == XSharpLexer.ACCESS 
            kind := Kind.Access
         ELSEIF SELF:La1 == XSharpLexer.ASSIGN
            kind := Kind.Assign
         ELSEIF SELF:La1 == XSharpLexer.METHOD
            kind := Kind.Method
         ELSE
            RETURN NULL
         ENDIF
         SELF:Consume()
         
         VAR sig := SELF:ParseSignature()
         VAR classClause := SELF:ParseOptionalClassClause()
         // read to EndOfLine
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
        
         VAR xMember := XMemberDefinition{sig, kind, _attributes, range, interval, _attributes:HasFlag(Modifiers.Static)}
         IF SELF:CurrentType != NULL
            SELF:CurrentType:AddMember(xMember)
         ENDIF
         xMember:SourceCode := source          
         RETURN <XEntityDefinition>{xMember}
         
      PRIVATE METHOD ParseProperty() AS IList<XEntityDefinition>
            IF ! Expect(XSharpLexer.PROPERTY)
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
                      | {InputStream.La(2) != GET}? Key=SET ExprList=expressionList?
                      | Key=(GET|SET) )
                    ;
expressionList	    : Exprs+=expression (COMMA Exprs+=expression)*
                    ;

propertyAccessor    : Attributes=attributes? Modifiers=accessorModifiers?
                      ( Key=GET end=eos StmtBlk=statementBlock END GET?
                      | Key=SET end=eos StmtBlk=statementBlock END SET? )
                      end=eos
                    ;
                    
*/         
         VAR id := ""
         IF SELF:Matches(XSharpLexer.SELF)   // Self property
            id := ConsumeAndGetText()
         ELSE
            id := SELF:ParseQualifiedName()
         ENDIF
         VAR aParams := List<IXVariable>{}
         IF Matches(XSharpLexer.LPAREN)
            aParams := SELF:ParseParameterList( FALSE, OUT VAR _)
         ELSEIF Matches(XSharpLexer.LBRKT)
            aParams := SELF:ParseParameterList(TRUE, OUT VAR _)
         ENDIF
         VAR sType := SELF:ParseAsIsType()
         LOCAL lSingleLine AS LOGIC
         DO WHILE ! SELF:Eos()
            IF Matches(XSharpLexer.AUTO, XSharpLexer.GET,XSharpLexer.SET)
               lSingleLine := TRUE
            ENDIF
            Consume()
         ENDDO
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
         VAR xMember := XMemberDefinition{id, Kind.Property, _attributes, range, interval,sType} {SingleLine := lSingleLine}
         xMember:SourceCode := source          
         IF SELF:CurrentType != NULL
            CurrentType:AddMember(xMember)
         ENDIF
         xMember:AddParameters(aParams)
         RETURN <XEntityDefinition>{xMember}
         
      PRIVATE METHOD ParseEvent() AS IList<XEntityDefinition>
            IF ! Expect(XSharpLexer.EVENT)
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
                      ( Key=ADD     end=eos StmtBlk=statementBlock END ADD?
                      | Key=REMOVE  end=eos StmtBlk=statementBlock END REMOVE? )
                      end=eos
                    ;                    
*/         
         VAR id := SELF:ParseQualifiedName()
         VAR strType  := SELF:ParseAsIsType()
         LOCAL lSingleLine AS LOGIC
         DO WHILE ! SELF:Eos()
            IF Matches( XSharpLexer.ADD,XSharpLexer.REMOVE)
               lSingleLine := TRUE
            ENDIF
            Consume()
         ENDDO
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
         VAR xMember := XMemberDefinition{id, Kind.Event, _attributes, range, interval,strType}  {SingleLine := lSingleLine}
         xMember:SourceCode := source          
         IF SELF:CurrentType != NULL
            CurrentType:AddMember(xMember)
         ENDIF
         RETURN <XEntityDefinition>{xMember}

         PRIVATE _operatortokens := <LONG> {XSharpLexer.PLUS , XSharpLexer.MINUS , XSharpLexer.NOT , XSharpLexer.TILDE , XSharpLexer.INC, ;
            XSharpLexer.DEC ,XSharpLexer.TRUE_CONST ,XSharpLexer.FALSE_CONST , XSharpLexer.MULT , ;
            XSharpLexer.DIV ,XSharpLexer.MOD ,XSharpLexer.AMP ,XSharpLexer.PIPE ,XSharpLexer.LSHIFT ,XSharpLexer.RSHIFT, ;
            XSharpLexer.EEQ , XSharpLexer.NEQ , XSharpLexer.NEQ2 ,XSharpLexer.GT , XSharpLexer.LT ,;
            XSharpLexer.GTE , XSharpLexer.LTE , XSharpLexer.AND , XSharpLexer.OR, XSharpLexer.IMPLICIT, XSharpLexer.EXPLICIT}  AS LONG[]


      PRIVATE METHOD ParseOperator() AS IList<XEntityDefinition>
            IF ! Expect(XSharpLexer.OPERATOR)
               RETURN NULL
            ENDIF
      
/*
overloadedOps       : Token= (PLUS | MINUS | NOT | TILDE | INC | DEC | TRUE_CONST | FALSE_CONST |
                              MULT | DIV | MOD | AMP | PIPE | LSHIFT | RSHIFT | EEQ | NEQ | NEQ2 |
                              GT | LT | GTE | LTE |
                              AND | OR )  // these two do not exist in C# and are mapped to & and |
                    ;

conversionOps		: Token=( IMPLICIT | EXPLICIT )
                    ;

operator_           : Attributes=attributes? Modifiers=operatorModifiers?
                      o1=OPERATOR (Operation=overloadedOps | Conversion=conversionOps) Gt=GT?
                      ParamList=parameterList
                      (AS Type=datatype)?
                      end=eos
                      StmtBlk=statementBlock
                     (END o1=OPERATOR EOS)?
                    ;

*/         
         LOCAL t1 AS IToken
         LOCAL t2 AS IToken 

         IF Matches(_operatortokens)
            t1 := SELF:ConsumeAndGet()
         ENDIF
         IF Matches (XSharpLexer.GT)
            t2 := SELF:ConsumeAndGet()
         ENDIF
         VAR aParams     := SELF:ParseParameterList(FALSE, OUT VAR _)
         VAR sType       := SELF:ParseAsIsType()
         
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         
         VAR id := t1:GetText()+ IIF(t2 != NULL, t2:GetText(),"")
         SELF:ReadLine()
         VAR xMember := XMemberDefinition{id, Kind.Operator, _attributes, range, interval,sType}
         xMember:SourceCode := source          
         IF SELF:CurrentType != NULL
            CurrentType:AddMember(xMember)
         ENDIF
         xMember:AddParameters(aParams)
         RETURN <XEntityDefinition>{xMember}
         
         
      PRIVATE METHOD ParseConstructor() AS IList<XEntityDefinition>
      
/*constructor         :  (Attributes=attributes)? (Modifiers=constructorModifiers)?
                      c1=CONSTRUCTOR (ParamList=parameterList)? (AS VOID)? // As Void is allowed but ignored
                        (CallingConvention=callingconvention)? 
                        (CLASS (Namespace=nameDot)? ClassId=identifier)?		
                        end=eos
                      (Chain=constructorchain)?
                      StmtBlk=statementBlock
                      (END c2=CONSTRUCTOR EOS)?
                    ;
*/         
         IF ! Expect(XSharpLexer.CONSTRUCTOR)
            RETURN NULL
         ENDIF
         VAR id  := ".ctor"
         VAR aParams     := SELF:ParseParameterList(FALSE, OUT VAR _)
         VAR asType      := SELF:ParseAsIsType()
         VAR callconv    := SELF:ParseCallingConvention()
         VAR classClause := SELF:ParseOptionalClassClause()
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
         VAR xMember := XMemberDefinition{id, Kind.Constructor, _attributes, range, interval,""}
         xMember:SourceCode := source          
         xMember:AddParameters(aParams)
         IF SELF:CurrentType != NULL
            CurrentType:AddMember(xMember)
         ENDIF

         RETURN <XEntityDefinition>{xMember}
         
      PRIVATE METHOD ParseDestructor() AS IList<XEntityDefinition>
/*
destructor          : (Attributes=attributes)? (Modifiers=destructorModifiers)?
                      d1=DESTRUCTOR (LPAREN RPAREN)? 
                      (CLASS (Namespace=nameDot)? ClassId=identifier)?
                      end=eos
                      StmtBlk=statementBlock
                      (END d2=DESTRUCTOR EOS)?
                    ;

*/         
         IF ! Expect(XSharpLexer.DESTRUCTOR)
            RETURN NULL
         ENDIF

         VAR id  := ".dtor"
         IF SELF:La1 == XSharpLexer.LPAREN .AND. SELF:La2 == XSharpLexer.RPAREN
            Consume()
            Consume()
         ENDIF
         VAR classClause := SELF:ParseOptionalClassClause()
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
         VAR xMember := XMemberDefinition{id, Kind.Destructor, _attributes, range, interval,"VOID"}
         xMember:SourceCode := source          
         IF SELF:CurrentType != NULL
            SELF:CurrentType:AddMember(xMember)
         ENDIF
         RETURN <XEntityDefinition>{xMember}
         
      PRIVATE METHOD ParseClassVars() AS IList<XEntityDefinition>
/*
classvars           : (Attributes=attributes)? (Modifiers=classvarModifiers)?
                      Vars=classVarList
                      eos
                    ;
*/         
         VAR classvars := SELF:ParseClassVarList(Kind.Field)
         VAR result := List<XEntityDefinition>{}
         FOREACH VAR classvar IN classvars
            classvar:SourceCode := classvar:ModVis +" "+classvar:SourceCode
            IF SELF:CurrentType != NULL
               SELF:CurrentType:AddMember(classvar)
            ENDIF
            result:Add(classvar)
         NEXT
         SELF:ReadLine()
         RETURN result  
         #endregion
         
      #region Enum
      PRIVATE METHOD ParseEnum() AS IList<XEntityDefinition>
/*
enum_               : (Attributes=attributes)? (Modifiers=classModifiers)?
                      E=ENUM (Namespace=nameDot)? Id=identifier ((AS|INHERIT) Type=datatype)? e=eos
                      (Members+=enummember)+
                      END ENUM? EOS
                    ;
*/         
         IF ! Expect(XSharpLexer.ENUM)
            RETURN NULL
         ENDIF

         VAR id := SELF:ParseQualifiedName()
         VAR type := ""
         IF Matches(XSharpLexer.AS)
            type := SELF:ParseAsIsType()
         ELSEIF Expect(XSharpLexer.INHERIT)
            type := SELF:ParseTypeName()
         ENDIF
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
         VAR xType := XTypeDefinition{id, Kind.Enum, _attributes, range, interval, _file} {BaseType := type}
         xType:SourceCode := source          
         RETURN <XEntityDefinition>{xType}
         
      PRIVATE METHOD ParseEnumMember() AS IList<XEntityDefinition>
/*
enummember          : (Attributes=attributes)? MEMBER? Id=identifier (Op=assignoperator Expr=expression)? eos
                    ;
*/         

         VAR att := SELF:ParseAttributes()
         IF ! Expect(XSharpLexer.MEMBER)
            RETURN NULL
         ENDIF
         VAR strValue := ""
         VAR id := SELF:ParseQualifiedName()
         IF ExpectAssign()
            strValue := SELF:ParseExpression()
         ENDIF
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
         VAR type := CurrentType?:Name
         VAR xMember := XMemberDefinition{id, Kind.EnumMember, _attributes, range, interval, type} {SingleLine := TRUE, @@Value := strValue}
         xMember:File := SELF:_file
         xMember:SourceCode := source          
         RETURN <XEntityDefinition>{xMember}
         
         #endregion
         
      #region VO Global, Define, Struct, Union
      PRIVATE METHOD ParseVOGlobals() AS IList<XEntityDefinition>
/*
                    // [STATIC] GLOBAL [CONST] Identifier [:= Expression] [, Identifier2 [:= Expression2] ] [AS Type]
                    // STATIC          [CONST] Identifier [:= Expression] [, Identifier2 [:= Expression2] ] [AS Type]
voglobal            : (Attributes=attributes)? (Modifiers=funcprocModifiers)? Global=GLOBAL (Const=CONST)? Vars=classVarList end=EOS 
                    | (Attributes=attributes)? Static=STATIC (Const=CONST)? Vars=classVarList end=EOS
                    ;
*/         
         IF ! Expect(XSharpLexer.GLOBAL)
            RETURN NULL
         ENDIF
         Expect(XSharpLexer.CONST)
         VAR classvars := SELF:ParseClassVarList(Kind.VOGlobal)
         VAR result    := List<XEntityDefinition>{}
         FOREACH VAR classvar IN classvars
            classvar:SourceCode := classvar:ModVis+" GLOBAL "+classvar:SourceCode
            result:Add(classvar)
         NEXT
         SELF:ReadLine()
         
         RETURN result
         
      PRIVATE METHOD ParseVoDefine() AS IList<XEntityDefinition>
/*
vodefine            : (Modifiers=funcprocModifiers)?
                      D=DEFINE Id=identifier Op=assignoperator Expr=expression (AS DataType=typeName)? end=EOS
                    ;

*/         
         IF ! Expect(XSharpLexer.DEFINE)
            RETURN NULL
         ENDIF

         VAR id := SELF:ParseQualifiedName()
         LOCAL strValue AS STRING
         IF ExpectAssign()
            strValue := SELF:ParseExpression()
         ENDIF
         VAR type := SELF:ParseAsIsType()
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
         
         VAR xMember := XMemberDefinition{id, Kind.VODefine, _attributes, range,interval, type} {SingleLine := TRUE, @@Value := strValue}
         xMember:SourceCode := source          
         xMember:File := SELF:_file
         RETURN <XEntityDefinition>{xMember}
         
      PRIVATE METHOD ParseVoStruct() AS IList<XEntityDefinition>
/*
vostruct            : (Modifiers=votypeModifiers)?
                      V=VOSTRUCT (Namespace=nameDot)? Id=identifier (ALIGN Alignment=INT_CONST)? e=eos
                      (Members+=vostructmember)+
                      (END VOSTRUCT EOS)?
                    ;
*/         
         IF ! Expect(XSharpLexer.VOSTRUCT)
            RETURN NULL
         ENDIF

         VAR id := SELF:ParseQualifiedName()
         VAR sAlign := ""
         IF Expect(XSharpLexer.ALIGN)
            IF ExpectAndGet(XSharpLexer.INT_CONST, OUT VAR t2)
               sAlign := t2:GetText() // Align number
            ENDIF
         ENDIF
         // read to EndOfLine
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
         VAR xType := XTypeDefinition{id, Kind.VOStruct, _attributes, range, interval,_file}
         xType:SourceCode := source          
         IF String.IsNullOrEmpty(sAlign)
            xType:AddInterface(sAlign)
         ENDIF
         RETURN <XEntityDefinition>{xType}
         
      PRIVATE METHOD ParseVOUnion() AS IList<XEntityDefinition>
/*
vounion             : (Modifiers=votypeModifiers)?
                      U=UNION (Namespace=nameDot)? Id=identifier e=eos
                      (Members+=vostructmember)+
                      (END UNION EOS)?
                    ;
*/         
         IF ! Expect(XSharpLexer.UNION)
            RETURN NULL
         ENDIF

         VAR id := SELF:ParseQualifiedName()
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
         VAR xType := XTypeDefinition{id, Kind.Union, _attributes, range, interval,_file}
         xType:SourceCode := source          
         RETURN <XEntityDefinition>{xType}
         
      PRIVATE METHOD ParseVODLL() AS IList<XEntityDefinition>
/*

vodll               : (Attributes=attributes)? (Modifiers=funcprocModifiers)? // Optional
                      D=DLL T=(FUNCTION|PROCEDURE) Id=identifier ParamList=parameterList (AS Type=datatype)? 
                      (CallingConvention=dllcallconv)? COLON
                      Dll=identifierString (DOT Extension=identifierString)?
                      (	Ordinal=REAL_CONST 
                       |  DOT Entrypoint=identifierString Address=ADDROF? Number=INT_CONST? (NEQ2 INT_CONST)? 
                      )
                       
                      ( CharSet=(AUTO | ID) )?  // ID must be either ANSI or UNICODE
                      EOS
                    ;

*/        
         IF ! Expect(XSharpLexer.DLL)
            RETURN NULL
         ENDIF

         VAR t := ConsumeAndGet()
         VAR sig     := SELF:ParseSignature()
         VAR colon   := SELF:ConsumeAndGetText()
         VAR dllName := SELF:ConsumeAndGetText()
         VAR dotName := ""
         IF Expect(XSharpLexer.DOT)
            dotName := SELF:ConsumeAndGetText()
         ENDIF
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
         VAR xMember := XMemberDefinition{sig, Kind.VODLL, _attributes, range, interval, _attributes:HasFlag(Modifiers.Static)} {SubType := IIF(t:Type == XSharpLexer.FUNCTION, Kind.Function, Kind.Procedure)} 
         xMember:File := SELF:_file
         xMember:SourceCode := source          
         RETURN <XEntityDefinition>{xMember}
         
         
      PRIVATE METHOD ParseVoStructMember() AS IList<XEntityDefinition>
/*
vostructmember      : MEMBER Dim=DIM Id=identifier LBRKT ArraySub=arraysub RBRKT (As=(AS | IS) DataType=datatype)? eos
                    | MEMBER Id=identifier (As=(AS | IS) DataType=datatype)? eos
                    ;
*/         
         IF ! Expect(XSharpLexer.MEMBER)
            RETURN NULL
         ENDIF
         
         LOCAL sBracket AS STRING
         VAR isArray := Expect(XSharpLexer.DIM)
         VAR id := SELF:ParseQualifiedName()
         IF isArray
            sBracket := SELF:ParseArraySub()
         ENDIF
         VAR sType := SELF:ParseAsIsType()
         SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         SELF:ReadLine()
         IF isArray
            sType += "[]"
         ENDIF
         VAR xMember := XMemberDefinition{id, Kind.Field, _attributes, range, interval, sType} {SingleLine := TRUE, IsArray := isArray}
         xMember:File := SELF:_file
         xMember:SourceCode := source          
         RETURN <XEntityDefinition>{xMember}
         
         #endregion
         
         
      PRIVATE METHOD ParseClassVarList(eKind AS Kind)   AS IList<XMemberDefinition>
/*

classVarList        : Var+=classvar (COMMA Var+=classvar)* (As=(AS | IS) DataType=datatype)?
                    ;
*/         
         LOCAL aVars  AS List<XMemberDefinition>
         LOCAL sType  AS STRING
         aVars := List<XMemberDefinition>{}
         aVars:Add(ParseClassVar(eKind))
         DO WHILE Expect(XSharpLexer.COMMA)
            aVars:Add(ParseClassVar(eKind))
         ENDDO
         sType := SELF:ParseAsIsType()
         FOREACH VAR element IN aVars
            element:TypeName := sType
            element:Attributes := _attributes
            element:SourceCode += " AS "+ sType
         NEXT
         RETURN aVars            
         
         
         
      PRIVATE METHOD ParseClassVar(eKind AS Kind) AS XMemberDefinition
/*

classvar            : (Dim=DIM)? Id=identifier (LBRKT ArraySub=arraysub RBRKT)? (Op=assignoperator Initializer=expression)?
                    ;
*/         
         LOCAL isDim  := FALSE  AS LOGIC
         LOCAL sId        AS STRING
         LOCAL sDefault   AS STRING
         LOCAL startToken AS IToken
         LOCAL endToken   AS IToken
         startToken := SELF:Lt1
         isDim := Expect(XSharpLexer.DIM)
         sId  := SELF:ParseIdentifier()
         VAR sBracket  := SELF:ParseArraySub()
         IF ExpectAssign()
            sDefault := SELF:ParseExpression()
         ENDIF
         endToken := SELF:LastToken
         SELF:GetSourceInfo(startToken, endToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
         VAR xMember   := XMemberDefinition{sId,eKind, Modifiers.None, range,interval,""}
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
                      ;


*/
         LOCAL oSig          AS XMemberSignature
         oSig                := XMemberSignature{}
         oSig:Id             := SELF:ParseQualifiedName()
         IF SELF:La1 == XSharpLexer.LT
            oSig:TypeParameters := SELF:ParseTypeParameters()
         ENDIF
         IF SELF:La1 == XSharpLexer.LPAREN
            oSig:Parameters   := SELF:ParseParameterList(FALSE,  OUT VAR isSelf)
            oSig:IsExtension  := isSelf
         ENDIF
         oSig:DataType       := SELF:ParseAsIsType()
         DO WHILE SELF:La1 == XSharpLexer.WHERE .AND. ! SELF:Eos()
            oSig:TypeParameterContraints:Add(SELF:ParseTypeParameterConstraints())
         ENDDO
         oSig:CallingConvention  := SELF:ParseCallingConvention()
         RETURN oSig
         
         
         
      PRIVATE METHOD ParseTypeParameters AS List<STRING>
         IF SELF:La1 != XSharpLexer.LT
            RETURN NULL
         ENDIF
         Consume()
         VAR aTypeParams := List<STRING>{}
         DO WHILE SELF:La1 != XSharpLexer.GT .AND. !SELF:Eos()
            VAR sParam := SELF:TokensAsString(ParseAttributes())
            
            IF SELF:La1 == XSharpLexer.IN .OR. SELF:La1 == XSharpLexer.OUT
               sParam += ConsumeAndGetText()+" "
            ENDIF
            IF SELF:IsId(SELF:La1)
               sParam += ConsumeAndGetText()
            ELSE
               EXIT
            ENDIF
            aTypeParams:Add(sParam)
            SELF:Expect(XSharpLexer.COMMA)
         ENDDO
         SELF:Expect(XSharpLexer.GT)
         RETURN aTypeParams 
         
      PRIVATE METHOD ParseParameterList(lBracketed AS LOGIC, isSelf OUT LOGIC) AS List<IXVariable>
         isSelf := FALSE 
         IF SELF:La1 != XSharpLexer.LPAREN .AND. ! lBracketed
            RETURN NULL
         ENDIF
         IF SELF:La1 != XSharpLexer.LBRKT .AND. lBracketed
            RETURN NULL
         ENDIF
         Consume()   // LParen
         VAR aResult  := List<IXVariable>{}
         LOCAL defaultExpr AS IList<IToken>
         LOCAL cond AS DelEndToken
         cond := { token => IIF (lBracketed, token == XSharpLexer.RBRKT, token == XSharpLexer.RPAREN ) }
         DO WHILE !cond(SELF:La1) .AND. ! SELF:Eos()
            VAR start := SELF:Lt1
            VAR atts := SELF:TokensAsString(ParseAttributes())
            VAR sId   := ""
            VAR sTypeName := ""
            IF SELF:La1 == XSharpLexer.SELF
               isSelf := TRUE
               Consume()
            ENDIF
            IF SELF:IsId(SELF:La1)
               sId  += SELF:ConsumeAndGetText()
            ENDIF
            IF ExpectAssign()
               defaultExpr := SELF:ParseExpressionAsTokens()
            ENDIF
            
            VAR token := SELF:ConsumeAndGetAny(XSharpLexer.AS, XSharpLexer.IN, XSharpLexer.OUT, XSharpLexer.REF,XSharpLexer.PARAMS)
            IF token != NULL
               IF SELF:La1 == XSharpLexer.CONST
                  Consume()
               ENDIF
               sTypeName := SELF:ParseTypeName()
            ENDIF
            LOCAL variable AS XVariable
            VAR endToken := SELF:LastToken
            SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
            variable := XParameter{CurrentEntity, sId, range,interval, sTypeName}
            IF token != NULL
               SWITCH token:Type
               CASE XSharpLexer.AS
                  variable:ParamType := ParamType.As
               CASE XSharpLexer.IN
                  variable:ParamType := ParamType.In
               CASE XSharpLexer.OUT
                  variable:ParamType := ParamType.Out
               CASE XSharpLexer.REF
                  variable:ParamType := ParamType.Ref
               CASE XSharpLexer.PARAMS
                  variable:ParamType := ParamType.Params
               END SWITCH
            ENDIF
            IF defaultExpr != NULL
               variable:Value := SELF:TokensAsString(defaultExpr)
            ENDIF
            aResult:Add(variable)
            IF SELF:La1 == XSharpLexer.COMMA
               Consume()
            ELSE
               EXIT
            ENDIF
         ENDDO
         IF cond(SELF:La1)
            Consume()
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
         IF SELF:La1 == XSharpLexer.CLASS .OR. SELF:La1 == XSharpLexer.STRUCTURE
            RETURN ConsumeAndGetText()
         ELSEIF SELF:La1 == XSharpLexer.NEW .AND. SELF:La2 == XSharpLexer.LPAREN .AND. La3 == XSharpLexer.RPAREN
            RETURN "NEW()"
         ENDIF
         RETURN ParseTypeName()
         
         
         
      PRIVATE METHOD ParseAsIsType() AS STRING
         IF SELF:La1 == XSharpLexer.AS .OR. SELF:La1 == XSharpLexer.IS
            SELF:Consume()
            RETURN SELF:ParseTypeName()
         ENDIF
         RETURN ""
         
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
            Consume()
            RETURN result
         END SWITCH
         RETURN CallingConvention.None
         
         
      PRIVATE METHOD IsAssignOp (nToken AS LONG) AS LOGIC
         RETURN nToken == XSharpLexer.ASSIGN_OP .OR. nToken == XSharpLexer.EQ
         
      PRIVATE METHOD ParseTypeName() AS STRING
         LOCAL result := "" AS STRING
         SWITCH SELF:La1
            
         CASE XSharpLexer.ID
            result := SELF:ParseQualifiedName() 
            
         CASE XSharpLexer.ARRAY
         CASE XSharpLexer.CODEBLOCK
         CASE XSharpLexer.DATE
         CASE XSharpLexer.FLOAT
         CASE XSharpLexer.PSZ
         CASE XSharpLexer.SYMBOL
         CASE XSharpLexer.USUAL
            result :=  ConsumeAndGet():GetText() 
            CASE XSharpLexer.BYTE
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
            CASE XSharpLexer.PTR
            CASE XSharpLexer.REAL4
            CASE XSharpLexer.REAL8
            CASE XSharpLexer.SHORTINT
            CASE XSharpLexer.STRING
            CASE XSharpLexer.UINT64
            CASE XSharpLexer.VOID
         CASE XSharpLexer.WORD
            result :=  SELF:ConsumeAndGet():GetText() 
         OTHERWISE
            IF XSharpLexer.IsKeyword(SELF:La1)
               result :=  SELF:ConsumeAndGet():GetText() 
            ENDIF
         END SWITCH
         IF result:Length > 0
            result += ParseTypeSuffix()
         ENDIF
         RETURN result
         

      PRIVATE METHOD ParseTypeSuffix AS STRING
         IF SELF:La1 == XSharpLexer.PTR
            RETURN " "+SELF:ConsumeAndGet():GetText()
         ELSEIF SELF:La1 = XSharpLexer.QMARK
            RETURN " "+SELF:ConsumeAndGet():GetText()
         ELSEIF SELF:La1 == XSharpLexer.LBRKT
            VAR tokens := List<IToken>{}
            tokens:Add(SELF:ConsumeAndGet())
            LOCAL closed := FALSE AS LOGIC
            DO WHILE ! SELF:Eos() .AND. ! closed
               SWITCH SELF:La1
               CASE XSharpLexer.RBRKT
                  tokens:Add(SELF:ConsumeAndGet())
                  closed := TRUE
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
               CASE XSharpLexer.LPAREN
               CASE XSharpLexer.LBRKT
            CASE XSharpLexer.LCURLY
               nested++
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
               tokens:Add(ConsumeAndGet())
            ENDIF
         ENDDO
         RETURN tokens   
         
      PRIVATE METHOD ParseExpression() AS STRING
         RETURN TokensAsString(SELF:ParseExpressionAsTokens())
        
         
      PRIVATE METHOD ParseForBlockLocals() AS VOID
         VAR tokens := List<IToken>{}
         DO WHILE ! SELF:Eos()
            IF SELF:Matches(XSharpLexer.IMPLIED, XSharpLexer.VAR)
               IF SELF:IsId(SELF:La2)
                  VAR start := SELF:Lt2
                  Consume()      // IMPLIED or VAR
                  VAR id    := SELF:ParseIdentifier()
                  LOCAL expr AS IList<IToken>
                  IF SELF:IsAssignOp(SELF:La1) .OR. SELF:La1 == XSharpLexer.IN
                     IF (SELF:La1 != XSharpLexer.IN)
                        Consume()   // := 
                     ENDIF
                     expr := SELF:ParseExpressionAsTokens()
                  ENDIF
                  SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
                  VAR xVar := XVariable{SELF:CurrentEntity, id, range, interval, XLiterals.VarType } {Expression := expr }
                  SELF:_locals:Add(xVar)
               ENDIF
            ELSEIF SELF:IsId(SELF:La1) .AND. SELF:La2 == XSharpLexer.AS
               VAR start := SELF:Lt1
               VAR id    := SELF:ParseIdentifier()
               VAR type  := SELF:ParseAsIsType()
               SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
               VAR xVar := XVariable{SELF:CurrentEntity, id, range, interval, type } 
               SELF:_locals:Add(xVar)

            ELSEIF SELF:La1 == XSharpLexer.LOCAL .AND. SELF:IsId(SELF:La2) .AND. La3 == XSharpLexer.AS
               VAR start := SELF:Lt1
               Consume()
               VAR id    := SELF:ParseIdentifier()
               VAR type  := SELF:ParseAsIsType()
               SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
               VAR xVar := XVariable{SELF:CurrentEntity, id, range, interval, type } 
               SELF:_locals:Add(xVar)
                     
            ENDIF
            Consume()
         ENDDO
         RETURN
      PRIVATE METHOD ParseForLocals() AS VOID
         // Check for method call with OUT VAR or OUT id AS TYPE
         // Check for <expr> IS <type> VAR <id>
         // also parses the EOS characters following the line  
         DO WHILE ! SELF:Eos()
            IF SELF:La1 == XSharpLexer.IS
               VAR start := SELF:Lt1
               Consume() // IS token
               VAR type := SELF:ParseTypeName()
               IF SELF:La1 == XSharpLexer.VAR
                  Consume()   // VAR
                  VAR id      := SELF:ParseIdentifier()
                  SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
                  VAR xVar     := XVariable{SELF:CurrentEntity, id, range, interval, type} 
                  SELF:_locals:Add(xVar)
               ENDIF
            ELSEIF SELF:La1 == XSharpLexer.OUT
               // OUT Id AS Type
               VAR start := SELF:Lt1
               Consume() // OUT
               IF SELF:IsId(SELF:La2) .AND. La3 == XSharpLexer.AS
                  VAR id   := SELF:ParseIdentifier()
                  VAR type := SELF:ParseAsIsType()
                  SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
                  VAR xVar     := XVariable{SELF:CurrentEntity, id, range, interval, type}
                  SELF:_locals:Add(xVar)
               ELSEIF SELF:La2 == XSharpLexer.VAR
                  // OUT VAR Id, when Id = '_' then discard and do not create a local
                  Consume()   // Var
                  VAR id         := SELF:ParseIdentifier()
                  IF id          == "_"
                     SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
                     VAR xVar       := XVariable{SELF:CurrentEntity, id, range, interval, XLiterals.VarType} 
                     SELF:_locals:Add(xVar)
                  ENDIF
               ELSEIF SELF:La2 == XSharpLexer.NULL
                  // OUT NULL, also discard
                  Consume()
               ENDIF
            ELSE
               Consume()
            ENDIF               
         ENDDO
         DO WHILE SELF:La1 == XSharpLexer.EOS
            Consume() // Consume the EOS
         ENDDO
         
         
      PRIVATE METHOD ParseStatement() AS VOID
         IF ! SELF:_collectLocals
            SELF:ReadLine()
            RETURN
         ENDIF
         SWITCH SELF:La1
         CASE XSharpLexer.FIELD
            IF !SELF:ParseFieldStatement()
               SELF:ReadLine()
            ENDIF
            
         CASE XSharpLexer.LOCAL
         CASE XSharpLexer.STATIC
         CASE XSharpLexer.VAR
            IF ! SELF:ParseDeclarationStatement()
               SELF:ReadLine()
            ENDIF
         CASE XSharpLexer.MEMVAR
         CASE XSharpLexer.PARAMETERS
         CASE XSharpLexer.LPARAMETERS
         CASE XSharpLexer.DIMENSION
         CASE XSharpLexer.DECLARE
            IF ! SELF:ParseXBaseDeclarationStatement()
               SELF:ReadLine()
            ENDIF
         CASE XSharpLexer.ID
            IF _lastToken != NULL .AND. (_lastToken:Type == XSharpLexer.PUBLIC .OR. _lastToken:Type == XSharpLexer.PRIVATE)
               SELF:PushBack()
               IF ! SELF:ParseXBaseDeclarationStatement()
                  SELF:ReadLine()
               ENDIF
            ELSE
               SELF:ParseForLocals()
            ENDIF
            
         OTHERWISE
            SELF:ParseForLocals()
         END SWITCH
         RETURN
         
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
//                   | LO

         LOCAL lStatic AS LOGIC
         LOCAL lImplied := FALSE AS LOGIC
         IF SELF:La1 == XSharpLexer.LOCAL
            Consume()
            IF SELF:La1 == XSharpLexer.STATIC
               lStatic := TRUE
               Consume()
            ENDIF
            IF SELF:La1 == XSharpLexer.IMPLIED
               lImplied := TRUE
               Consume()
            ENDIF
            IF SELF:La1 == XSharpLexer.STATIC
               lStatic := TRUE
               Consume()
            ENDIF
         ELSEIF SELF:La1 == XSharpLexer.STATIC
            lStatic := TRUE
            Consume()
            IF SELF:La1 == XSharpLexer.LOCAL
               Consume()
            ELSEIF SELF:La1 == XSharpLexer.VAR
               Consume()
               lImplied  := TRUE
            ELSEIF XSharpLexer.IsKeyword(SELF:La1)      // STATIC Keyword should exit
               RETURN FALSE
            ENDIF
            IF SELF:La1 == XSharpLexer.IMPLIED
               lImplied := TRUE
               Consume()
            ENDIF
         ELSEIF SELF:La1 == XSharpLexer.VAR
            Consume()
            lImplied  := TRUE
         ELSE
            RETURN FALSE
         ENDIF
         VAR result := List<XVariable>{}
         IF lImplied
            VAR xVar := SELF:ParseImpliedVar()
            result:Add(xVar)
            DO WHILE SELF:La1 == XSharpLexer.COMMA
               Consume()
               xVar := SELF:ParseImpliedVar()
               result:Add(xVar)
            ENDDO
         ELSE
            VAR xVar := SELF:ParseLocalVar()
            result:Add(xVar)
            DO WHILE SELF:La1 == XSharpLexer.COMMA
               Consume()
               xVar := SELF:ParseLocalVar()
               result:Add(xVar)
            ENDDO
         ENDIF
         // copy type forward
         VAR missingTypes := List<XVariable>{}
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
            xMissing:TypeName := XLiterals.UsualType
         NEXT
         RETURN TRUE

      PRIVATE METHOD ParseLocalVar AS XVariable
/*
//localvar           : (CONST=CONST)? ( DIM=DIM )? Id=identifier (LBRKT ArraySub=arraysub RBRKT)?
//                     (Op=assignoperator Expression=expression)? (AS=(AS | IS) DataType=datatype)?
//                   ;
*/
         LOCAL lConst   := FALSE AS LOGIC
         LOCAL lDim     := FALSE AS LOGIC
         LOCAL expr     AS IList<IToken>
         LOCAL start    := SELF:Lt1 AS IToken
         IF SELF:La1 == XSharpLexer.CONST
            lConst := TRUE
            Consume()
         ENDIF
         IF SELF:La1 == XSharpLexer.DIM
            lDim := TRUE
            Consume()
         ENDIF
         VAR id := SELF:ParseIdentifier()
         VAR arraysub := SELF:ParseArraySub()
         IF ExpectAssign()
            expr       := SELF:ParseExpressionAsTokens()
         ENDIF
         VAR type     := SELF:ParseAsIsType()
         SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
         IF String.IsNullOrEmpty(type)
            type := XLiterals.NoType
         ENDIF
         VAR xVar     := XVariable{SELF:CurrentEntity, id, range, interval, type} {IsArray := lDim .OR. !String.IsNullOrEmpty(arraysub)}
         IF type:EndsWith("[]")
            xVar:IsArray := TRUE
         ENDIF
         xVar:Expression := expr
         RETURN xVar

      PRIVATE METHOD ParseImpliedVar() AS XVariable
         // impliedvar: (Const=CONST)? Id=identifier Op=assignoperator Expression=expression         
         LOCAL lConst   := FALSE AS LOGIC
         LOCAL lDim     := FALSE AS LOGIC
         LOCAL expr     AS IList<IToken>
         LOCAL start    := SELF:Lt1 AS IToken
         IF SELF:La1 == XSharpLexer.CONST
            lConst := TRUE
            Consume()
         ENDIF
         VAR id := SELF:ParseIdentifier()
         IF ExpectAssign()
            expr:= SELF:ParseExpressionAsTokens()
         ENDIF
         SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
         VAR xVar     := XVariable{SELF:CurrentEntity, id,  range, interval, XLiterals.VarType} 
         xVar:Expression := expr
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
               ParseMemvarDeclarationStatement()            
         CASE XSharpLexer.PUBLIC
         CASE XSharpLexer.PRIVATE
            ParseMemvarAllocationStatement()            
         CASE XSharpLexer.DIMENSION
         CASE XSharpLexer.DECLARE
            ParseFoxProDim()        
         CASE XSharpLexer.FIELD
            ParseFieldStatement()
         END SWITCH
         RETURN TRUE

      PRIVATE METHOD ParseMemvarDeclarationStatement() AS VOID
            /*
            xbasedecl           : T=(MEMVAR|PARAMETERS|LPARAMETERS)      // MEMVAR  Foo, Bar or PARAMETERS Foo, Bar
                                  Vars+=identifierName (COMMA Vars+=identifierName)*
                                  end=eos
            */
            VAR start := SELF:Lt1
            IF ! ExpectAny(XSharpLexer.MEMVAR, XSharpLexer.PARAMETERS, XSharpLexer.LPARAMETERS)
               RETURN
            ENDIF
            VAR Ids := List<STRING>{}
            IF ! SELF:IsId(SELF:La1)
               RETURN
            ENDIF
            Ids:Add(SELF:ParseIdentifier())
            DO WHILE Expect(XSharpLexer.COMMA)
               Ids:Add(SELF:ParseIdentifier())
            ENDDO
            SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
            SELF:ReadLine()
            FOREACH VAR id IN Ids
               VAR xVar := XVariable{SELF:CurrentEntity, id, range, interval,XLiterals.UsualType} 
               IF start.Type != XSharpLexer.MEMVAR
                  xVar.Kind := Kind.Parameter
               ELSE
                  xVar.Kind := Kind.MemVar
               ENDIF
               SELF:_locals:Add(xVar)
            NEXT
            RETURN
            
          PRIVATE METHOD ParseMemvarAllocationStatement() AS VOID
            /*
                                | T=(PRIVATE | PUBLIC) 
                                  XVars+=xbasevar (COMMA XVars+=xbasevar)*   // PRIVATE Foo := 123,  PUBLIC Bar, PUBLIC MyArray[5,2]
                                  end=eos 
            */
            VAR start := SELF:Lt1
            IF ! ExpectAny(XSharpLexer.PRIVATE, XSharpLexer.PUBLIC)
               RETURN
            ENDIF
            VAR result := List<XVariable>{}
            VAR xVar := SELF:ParseXBaseVar()
            result:Add(xVar)
            DO WHILE SELF:La1 == XSharpLexer.COMMA
               Consume()
               xVar := SELF:ParseXBaseVar()
               result:Add(xVar)
            ENDDO
            FOREACH VAR mv IN result
               SELF:_locals:Add(mv)
            NEXT
            RETURN       

      PRIVATE METHOD ParseXBaseVar AS XVariable
         /*
         xbasevar            : (Amp=AMP)?  Id=identifierName (LBRKT ArraySub=arraysub RBRKT)? (Op=assignoperator Expression=expression)?
                              ;
         */
         LOCAL lAmp     := FALSE AS LOGIC
         LOCAL expr     AS IList<IToken>
         LOCAL start    := SELF:Lt1 AS IToken
         IF SELF:La1 == XSharpLexer.AMP
            lAmp := TRUE
            Consume()
         ENDIF
         VAR id         := SELF:ParseIdentifier()
         VAR arraysub   := SELF:ParseArraySub()
         IF ExpectAssign()
            expr        := SELF:ParseExpressionAsTokens()
         ENDIF
         VAR type       := XLiterals.UsualType
         SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
         VAR xVar       := XVariable{SELF:CurrentEntity, id, range, interval, type} {IsArray := !String.IsNullOrEmpty(arraysub)}
         xVar.Kind      := Kind.MemVar
         xVar:Expression := expr
         RETURN xVar


      PRIVATE METHOD ParseFoxProDim AS VOID
         /*
                                | T=(DIMENSION|DECLARE) DimVars += dimensionVar (COMMA DimVars+=dimensionVar)*    END=eos 

            dimensionVar        : Id=identifierName  ( LBRKT ArraySub=arraysub RBRKT | LPAREN ArraySub=arraysub RPAREN ) (AS DataType=datatype)?
                                ;
         */
         IF _dialect == XSharpDialect.FoxPro
            IF ! ExpectAny(XSharpLexer.DIMENSION, XSharpLexer.DECLARE)
               RETURN
            ENDIF
            ParseDimensionVar()
            DO WHILE Expect(XSharpLexer.COMMA)
               ParseDimensionVar()
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
         VAR type := SELF:ParseAsIsType()
         SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
         IF String.IsNullOrEmpty(type)
            type := "ARRAY"
         ENDIF
         VAR xVar     := XVariable{SELF:CurrentEntity, id, range, interval, type} 
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
            IF ! Expect(XSharpLexer.FIELD)
               RETURN FALSE
            ENDIF
            VAR Ids := List<STRING>{}
            IF ! SELF:IsId(SELF:La1)
               RETURN FALSE
            ENDIF
            Ids:Add(SELF:ParseIdentifier())
            DO WHILE Expect(XSharpLexer.COMMA)
               Ids:Add(SELF:ParseIdentifier())
            ENDDO
            VAR area := ""
            IF Expect(XSharpLexer.IN)
               area := SELF:ParseIdentifier()
            ENDIF
            SELF:GetSourceInfo(start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
            SELF:ReadLine()
            FOREACH VAR id IN Ids
               VAR xVar := XVariable{SELF:CurrentEntity, id, range, interval,XLiterals.UsualType} 
               xVar.Kind := Kind.DbField
               IF ! String.IsNullOrEmpty(area)
                  xVar:Value := area
               ENDIF
               SELF:_locals:Add(xVar)
            NEXT
            RETURN TRUE

#region FoxPro class definitions            
         PRIVATE METHOD ParseFoxClass() AS IList<XEntityDefinition>
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
            IF ! Expect(XSharpLexer.DEFINE)
               RETURN NULL
            ENDIF
            VAR mods := SELF:ParseVisibilityAndModifiers()
            IF ! Expect(XSharpLexer.CLASS)
               RETURN NULL
            ENDIF
            VAR id := SELF:ParseQualifiedName()
            VAR typePars := SELF:ParseTypeParameters()
            VAR baseType := ""
            VAR classlib := ""
            IF Expect(XSharpLexer.AS)
               baseType := SELF:ParseTypeName()
            ENDIF
            IF Expect(XSharpLexer.OF)
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
            VAR xType := XTypeDefinition{id, Kind.Class, mods, range, interval, _file}
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
            xType:ClassType := XSharpDialect.FoxPro
            RETURN <XEntityDefinition>{xType}
         
         PRIVATE METHOD ParseFoxMethod() AS IList<XEntityDefinition>
           /*
         foxmethod           : (Attributes=attributes)? (Modifiers=memberModifiers)?
                               T=foxmethodtype  Sig=signature
                               (HelpString=HELPSTRING HelpText=expression)?          	
                               (ThisAccess=THISACCESS LPAREN MemberId=identifier RPAREN)?
                               end=eos
                               StmtBlk=statementBlock
                               (END T2=foxmethodtype  EOS)?
                               ;
         foxmethodtype        : Token=(FUNCTION | PROCEDURE)
                              ;
           
            */
            LOCAL hs := "" AS STRING
            LOCAL thisId := "" AS STRING
            IF ! ExpectAny(XSharpLexer.FUNCTION, XSharpLexer.PROCEDURE)
               RETURN NULL
            ENDIF
            VAR sig := SELF:ParseSignature()
            IF Expect(XSharpLexer.HELPSTRING)
               hs  := SELF:ParseExpression()
            ENDIF
            IF Expect(XSharpLexer.THISACCESS) .AND. Expect(XSharpLexer.LPAREN)
               thisId := SELF:ParseIdentifier()
               Expect(XSharpLexer.RPAREN)
            ENDIF
            SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
            SELF:ReadLine()
            VAR kind := Kind.Method
            // Check for _ACCESS or _ASSIGN
            VAR id := sig:Id
            IF id:EndsWith("_access", StringComparison.OrdinalIgnoreCase)
               sig:Id   := sig:Id:Substring(0, sig:Id:Length - "_access":Length)
               kind := Kind.Access
            ELSEIF id:EndsWith("_assign", StringComparison.OrdinalIgnoreCase)
               sig:Id   := sig:Id:Substring(0, sig:Id:Length - "_assign":Length)
               kind := Kind.Assign
            ENDIF
             
            VAR xMember := XMemberDefinition{sig, kind, _attributes, range, interval, _attributes:HasFlag(Modifiers.Static)}
            xMember:SourceCode := source
            xMember:File := SELF:_file
            RETURN <XEntityDefinition>{xMember}        
            
         
         PRIVATE METHOD ParseFoxFields() AS IList<XEntityDefinition>
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

            IF CurrentType == NULL .OR. CurrentType:ClassType != XSharpDialect.FoxPro
               RETURN NULL
            ENDIF
            VAR typedef := CurrentType
            VAR result := List<XEntityDefinition>{}
            // Detect which variant we need
            IF Expect(XSharpLexer.IMPLEMENTS)
               //  foximplementsclause : IMPLEMENTS Type=datatype (Excl=EXCLUDE)? (IN Library=expression)? END=eos
               VAR interf    := SELF:ParseTypeName()
               VAR exclude   := Expect(XSharpLexer.EXCLUDE)
               IF Expect(XSharpLexer.IN)
                  VAR Lib := SELF:ParseExpression()
               ENDIF
               typedef:AddInterface(interf)
               SELF:ReadLine()
               RETURN NULL
            ELSEIF Expect(XSharpLexer.DIMENSION)
               // foxpemcomattrib     : DIMENSION Id=identifier LBRKT expression RBRKT end=eos
               VAR id := SELF:ParseIdentifier()
               VAR expr := ""
               IF ! Expect(XSharpLexer.LBRKT)
                  RETURN NULL
               ENDIF
               expr := SELF:ParseExpression()
               IF !Expect(XSharpLexer.RBRKT)
                  RETURN NULL
               ENDIF
               SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
               SELF:ReadLine()
               VAR xMember := XMemberDefinition{id, Kind.Field, _attributes, range, interval,"ARRAY"}
               xMember:SourceCode := source
               xMember:SingleLine := TRUE
               xMember:File := SELF:_file
               RETURN <XEntityDefinition>{xMember}        
            ELSEIF Matches(XSharpLexer.ID, XSharpLexer.FIELD)
               VAR hasField := Expect(XSharpLexer.FIELD)
               IF ! IsId(SELF:La1)
                  RETURN NULL
               ENDIF
               VAR ids      := List<STRING>{}
               ids:Add(ParseIdentifier())
               IF ids:First():EndsWith("COMATTRIB", StringComparison.OrdinalIgnoreCase)
                  RETURN NULL
               ENDIF
               VAR source := ""
               
               IF ExpectAssign()
                     // foxfield            : (Modifiers=classvarModifiers)? (Fld=FIELD)? F=foxfieldinitializer END=eos
                     //                     ;
                     // foxfieldinitializer : Name=name assignoperator Expr=expression
                     //                     ;
                     VAR expr := SELF:ParseExpression()                     
                     SELF:GetSourceInfo(_start, LastToken, OUT VAR r1, OUT VAR i1, OUT source)  
                     SELF:ReadLine()
                     VAR xMember := XMemberDefinition{ids:First(), Kind.Field, _attributes, r1, i1, "USUAL"}
                     xMember:SourceCode := source
                     xMember:Value      := expr
                     xMember:File := SELF:_file
                     xMember:SingleLine := TRUE
                     RETURN <XEntityDefinition>{xMember}        
               ENDIF                  
               IF Matches(XSharpLexer.COMMA)
                  // foxclassvars        : (Attributes=attributes)? (Modifiers=classvarModifiers)? 
                  //                      (Fld=FIELD)? Vars += identifier (COMMA Vars += identifier )*  (AS DataType=datatype)? 
                  //                      END=eos
                  DO WHILE Expect(XSharpLexer.COMMA)
                     ids:Add(ParseIdentifier())
                  ENDDO
               ENDIF
               VAR type := SELF:ParseAsIsType()
               SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT  source)  
               SELF:ReadLine()  
               FOREACH VAR id IN ids
                  VAR xMember := XMemberDefinition{id, Kind.Field, _attributes, range, interval, type}
                  IF ids:Count == 1
                     xMember:SourceCode := source
                  ELSEIF String.IsNullOrEmpty(type)
                     xMember:SourceCode := "FIELD "+id
                  ELSE
                     xMember:SourceCode := "FIELD "+id +" AS "+type
                  ENDIF
                  xMember:File := SELF:_file
                  xMember:SingleLine := TRUE
                  result:Add(xMember)
               NEXT
               RETURN result
            ELSEIF Expect(XSharpLexer.ADD)
               /*
                        foxaddobjectclause  : (Attributes=attributes)? ADD OBJECT (Modifiers=classvarModifiers)?
                                          Id=identifier AS Type=datatype (NoInit=NOINIT)?
                                          (WITH FieldsInits += foxfieldinitializer (COMMA FieldsInits += foxfieldinitializer)* )?
                                          end=eos
                                       ;
               */
               IF ! Expect(XSharpLexer.OBJECT)
                  RETURN NULL
               ENDIF                                  
               VAR mods := SELF:ParseVisibilityAndModifiers()
               VAR id := SELF:ParseIdentifier()
               IF ! Expect(XSharpLexer.AS)
                  RETURN NULL
               ENDIF
               VAR type   := SELF:ParseTypeName()
               VAR noinit := Expect(XSharpLexer.NOINIT)
               // no need to parse the initializers here
               DO WHILE SELF:La1 != XSharpLexer.EOS
                  Consume()
               ENDDO
               SELF:GetSourceInfo(_start, LastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
               SELF:ReadLine()  
               VAR xMember := XMemberDefinition{id, Kind.Field, _attributes, range, interval, type}
               xMember:SourceCode := source
               xMember:File := SELF:_file
               xMember:SingleLine := TRUE
               RETURN <XEntityDefinition>{xMember}        
           ENDIF        
            // ignore the COMATTRIB rule here
           SELF:ReadLine()  
           RETURN NULL
#endregion         

#region XPP classes
            PRIVATE METHOD ParseXppClass() AS IList<XEntityDefinition>
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
            IF ! Expect(XSharpLexer.CLASS)
               RETURN NULL
            ENDIF
            VAR id         := SELF:ParseQualifiedName()
            VAR baseType   := ""
            IF SELF:ExpectAny(XSharpLexer.FROM, XSharpLexer.SHARING)
               baseType  := SELF:ParseTypeName()
               DO WHILE SELF:Expect(XSharpLexer.COMMA)
                  // consume the other type declarations , ignore them because we do not support multiple inheritance
                  ParseTypeName()
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
            
            VAR xType := XTypeDefinition{id,Kind.Class, _attributes, range, interval, _file}
            xType:SourceCode := source

            IF interfaces?:Count > 0
               FOREACH VAR sInterface IN interfaces
                  xType:AddInterface(sInterface)
               NEXT
            ENDIF
            IF ! String.IsNullOrEmpty(baseType)
               xType:BaseType := baseType
            ENDIF
            xType:IsPartial := _attributes:HasFlag(Modifiers.Partial)
            IF CurrentEntity != _globalType .AND. CurrentEntityKind:HasChildren()
               xType:Parent := SELF:CurrentEntity
            ENDIF
            xType:ClassType := XSharpDialect.XPP
            _xppVisibility := Modifiers.Public
            RETURN <XEntityDefinition>{xType}            
            
            PRIVATE METHOD ParseXppClassVars() AS IList<XEntityDefinition>
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
                  type := SELF:ParseAsIsType()
               ENDIF
               SELF:GetSourceInfo(_start, _lastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
               // eat all tokens 
               DO WHILE ExpectAny(XSharpLexer.SHARED, XSharpLexer.READONLY, XSharpLexer.ASSIGNMENT, ;
                                  XSharpLexer.HIDDEN, XSharpLexer.PROTECTED, XSharpLexer.EXPORTED, XSharpLexer.NOSAVE)
                  NOP
               ENDDO
               SELF:ReadLine()
               VAR result := List<XEntityDefinition>{}
               FOREACH VAR id IN ids
                  VAR classvar := XMemberDefinition{id, Kind.Field, _xppVisibility, range, interval, type, FALSE}
                  classvar:SourceCode := ie"{classvar.ModVis}: \r\n{ IIF(isStatic, \"STATIC\",\"\")} VAR {classvar.Name}" 
                  IF SELF:CurrentType != NULL
                     SELF:CurrentType:AddMember(classvar)
                  ENDIF
                  classvar:SingleLine := TRUE
                  result:Add(classvar)
               NEXT
               RETURN result
               
               PRIVATE METHOD ParseXppProperty AS IList<XEntityDefinition>
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
                  IF ! Matches(XSharpLexer.ACCESS, XSharpLexer.ASSIGN)
                     RETURN NULL
                  ENDIF
                  DO WHILE ExpectAny(XSharpLexer.ACCESS, XSharpLexer.ASSIGN)
                     NOP
                  ENDDO
                  VAR isStatic := SELF:ParseXPPMemberModifiers()
                  IF ! Expect(XSharpLexer.METHOD)
                     RETURN NULL
                  ENDIF
                  VAR id := SELF:ParseIdentifier()
                  VAR propName := ""
                  IF SELF:Expect(XSharpLexer.VAR)
                     propName := SELF:ParseIdentifier()
                  ENDIF
                  VAR type := SELF:ParseAsIsType()
                  SELF:GetSourceInfo(_start, _lastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
                  SELF:ReadLine()                
                  VAR xmember := XMemberDefinition{id+XLiterals.XppDeclaration, Kind.Property, _OR(_xppVisibility, _attributes), range, interval, type, isStatic}
                  xmember:SourceCode := source
                  IF SELF:CurrentType != NULL
                     SELF:CurrentType:AddMember(xmember)
                  ENDIF
                  xmember:SingleLine := TRUE
                  RETURN <XEntityDefinition> {xmember}


      PRIVATE METHOD ParseXppMethodDeclaration() AS IList<XEntityDefinition>
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
         VAR isStatic :=  Expect(XSharpLexer.CLASS)
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
         SELF:GetSourceInfo(_start, _lastToken, OUT VAR range, OUT VAR interval, OUT VAR _)  
         VAR result := List<XEntityDefinition>{}
         FOREACH VAR id IN ids
            VAR xmethod := XMemberDefinition{id+XLiterals.XppDeclaration, Kind.Method, _xppVisibility, range, interval, "", FALSE}
            xmethod:SourceCode := ie"{xmethod.ModVis}: \r\n{ IIF(isStatic, \"STATIC\",\"\")} METHOD {xmethod.Name}" 
            IF SELF:CurrentType != NULL
               SELF:CurrentType:AddMember(xmethod)
            ENDIF
            xmethod:SingleLine := TRUE
            result:Add(xmethod)
         NEXT
         RETURN result

       PRIVATE METHOD ParseXppVisibility() AS IList<XEntityDefinition>
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
            IF Expect(XSharpLexer.IS)
               id := SELF:ParseIdentifier()
            ENDIF
            IF Expect(XSharpLexer.IN)
               superclass := SELF:ParseIdentifier()
            ENDIF
            RETURN 


       PRIVATE METHOD ParseXppMethod() AS IList<XEntityDefinition>
            // detect if we're inside the class definition
            IF SELF:La1 == XSharpLexer.INLINE
               RETURN SELF:ParseXppMethodInLine()
            ELSEIF SELF:InXppClass 
               RETURN SELF:ParseXppMethodDeclaration()
            ELSE
               RETURN SELF:ParseXppMethodImplementation()
            ENDIF
            


       PRIVATE METHOD ParseXppMethodInLine() AS IList<XEntityDefinition>
            /*
               xppinlineMethod     : (Attributes=attributes)?                               // NEW Optional Attributes
                                     I=INLINE  
                                     (Modifiers=xppmemberModifiers)?                        // [CLASS]
                                     METHOD  Id=identifier                                  // METHOD <MethodName>
                                     // no type parameters 
                                     (ParamList=parameterList)?                            // Optional Parameters
                                     (AS Type=datatype)?                                   // NEW Optional return type
                                     // no type constraints
                                     // no calling convention
                                     end=eos
                                     StmtBlk=statementBlock
                                     (END METHOD eos)?
                                   ;

               xppmemberModifiers  : ( Tokens+=( CLASS | STATIC) )+
                                   ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs
            */
            
            IF ! SELF:Expect(XSharpLexer.INLINE)
               RETURN NULL
            ENDIF
            VAR isStatic := SELF:ParseXPPMemberModifiers()
            IF ! SELF:Expect(XSharpLexer.METHOD)
               RETURN NULL
            ENDIF
            VAR id      := SELF:ParseIdentifier()
            VAR aParams := SELF:ParseParameterList(FALSE, OUT VAR _)
            VAR type    := ""
            IF Matches(XSharpLexer.AS)
               type := SELF:ParseAsIsType()
            ENDIF
            SELF:GetSourceInfo(_start, _lastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
            SELF:ReadLine()
            VAR xmethod := XMemberDefinition{id, Kind.Method, _xppVisibility, range, interval, type, isStatic}
            xmethod:SourceCode := source
            IF SELF:CurrentType != NULL
               SELF:CurrentType:AddMember(xmethod)
            ENDIF
            IF aParams != NULL
               FOREACH VAR par IN aParams
                  xmethod:AddParameter(par)
               NEXT
            ENDIF
            RETURN <XEntityDefinition>{xmethod}


       PRIVATE METHOD ParseXPPMemberModifiers() AS LOGIC
            RETURN SELF:ExpectAny(XSharpLexer.CLASS, XSharpLexer.STATIC)

      PRIVATE METHOD ParseXppMethodImplementation() AS IList<XEntityDefinition>
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
                                     END=eos
                                     StmtBlk=statementBlock
                                     (END METHOD eos)?
                                   ;
            
            */
            VAR kind := Kind.Method
            IF ExpectAny(XSharpLexer.ACCESS)
               kind := Kind.Access
            ELSEIF ExpectAny(XSharpLexer.ASSIGN)
               kind := Kind.Assign
            ENDIF
            VAR isStatic := SELF:ParseXPPMemberModifiers()
            IF ! SELF:Expect(XSharpLexer.METHOD)
               RETURN NULL
            ENDIF
            VAR classprefix := ""
            IF SELF:La1 == XSharpLexer.ID .AND. SELF:La2 == XSharpLexer.COLON
               classprefix := SELF:ParseIdentifier()
               Expect(XSharpLexer.COLON)
            ENDIF
            VAR id      := SELF:ParseIdentifier()
            VAR aParams := SELF:ParseParameterList(FALSE, OUT VAR _)
            VAR type    := ""
            IF Matches(XSharpLexer.AS)
               type := SELF:ParseAsIsType()
            ENDIF
            SELF:GetSourceInfo(_start, _lastToken, OUT VAR range, OUT VAR interval, OUT VAR source)  
            SELF:ReadLine()
            VAR xmethod := XMemberDefinition{id, kind, Modifiers.None, range, interval, type, isStatic}
            // we need to lookup the visibility in the types list
            xmethod:SourceCode := source
            // find the parent in the stack. Either the last one (when the classprefix is empty) or a specific one
            // then find the declared member in this type
            VAR classdef := SELF:FindXPPClass(classprefix)
            IF classdef != NULL
               classdef:AddMember(xmethod)
               FOREACH m AS XMemberDefinition IN classdef:Members
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
            RETURN <XEntityDefinition>{xmethod}            

         PRIVATE METHOD FindXPPClass(sName AS STRING) AS XTypeDefinition
            VAR start := SELF:_EntityList:Count-1
            FOR VAR i := start DOWNTO 0
               VAR element := _EntityList[i]
               IF element IS XTypeDefinition VAR typedef
                  IF typedef:ClassType == XSharpDialect.XPP
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
         SWITCH token
         CASE XSharpLexer.OLEPUBLIC
         CASE XSharpLexer.EACH
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
            source   := GetSource(startToken, endToken)
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
         IF XSettings.EnableParseLog .AND. XSettings.EnableLogging
            XSolution.WriteOutputMessage("XParser: "+cMessage)
         ENDIF
         RETURN         
   END CLASS
   
   DELEGATE DelEndToken(iToken AS LONG) AS LOGIC
   
   
   
END NAMESPACE











