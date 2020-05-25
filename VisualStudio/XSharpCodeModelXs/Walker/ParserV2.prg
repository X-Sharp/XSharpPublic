//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

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

BEGIN NAMESPACE XSharpModel
    DELEGATE DelEndToken(iToken AS LONG) AS LOGIC


    PARTIAL CLASS ParserV2
        
        PROTECTED _input        AS IList<IToken>
        PROTECTED _index        AS LONG
        PROTECTED _file         AS XFile
        PROTECTED _usings       AS IList<STRING>
        PROTECTED _staticusings AS IList<STRING>
        PUBLIC    EntityList    AS IList<XElement>
        PROTECTED EntityStack   AS Stack<XElement>
        PROTECTED BlockList     AS IList<XBlock>
        PROTECTED BlockStack    AS Stack<XBlock>
        PROTECTED PPBlockStack  AS Stack<XBlock>
        PROTECTED _lastToken    AS IToken
        PROTECTED _locals       AS LOGIC
        PROTECTED _blocks       AS LOGIC
        
        PROPERTY CurrentEntity      AS Xelement GET IIF(EntityStack:Count > 0, EntityStack:Peek(), NULL_OBJECT)
        PROPERTY CurrentBlock       AS XBlock   GET IIF(BlockStack:Count > 0, BlockStack:Peek(), NULL_OBJECT)
        PROPERTY CurrentEntityKind  AS Kind     GET IIF(EntityStack:Count > 0, CurrentEntity:Kind, Kind:Unknown)
        PROPERTY La1 AS INT GET SELF:La(1)
        PROPERTY La2 AS INT GET SELF:La(2)
        PROPERTY La3 AS INT GET SELF:La(3)
        PROPERTY Lt1 AS IToken GET SELF:Lt(1)
        PROPERTY Lt2 AS IToken GET SELF:Lt(2)
        PROPERTY Lt3 AS IToken GET SELF:Lt(3)
        PROPERTY lastToken AS IToken GET _lastToken
                    
        METHOD Parse( tokenStream AS ITokenStream, lIncludeLocals AS LOGIC, oFile AS XFile, lBlocks AS LOGIC, lLocals AS LOGIC) AS VOID
            LOCAL nMods  AS Modifiers
            LOCAL nVis   AS Modifiers
            LOCAL aAttribs AS IList<IToken>
            _file := oFile
            _locals := lLocals
            _blocks := lBlocks
            _usings       := List<STRING>{}
            _staticusings := List<STRING>{}
            LOCAL stream  := (BufferedTokenStream) tokenStream AS BufferedTokenStream
            VAR tokens    := stream:GetTokens()
            _input        := tokens:Where ({ t => t:Channel == 0 .OR. t:Channel == 4}):ToArray()
            EntityList    := List<XElement>{}
            EntityStack   :=     Stack<XElement>{}
            BlockList     := List<XBlock>{}
            BlockStack    := Stack<XBlock>{}
            PPBlockStack  := Stack<XBlock>{}
            EntityStack:Push(XType.CreateGlobalType(NULL))
            _index := 0
            DO WHILE ! SELF:Eoi()
                VAR last := lastToken
                VAR first := Lt1
                IF ParsePPLine()
                    LOOP
                ENDIF
                IF SELF:ParseUsing()
                    LOOP
                ENDIF
                aAttribs := ParseAttributes()
                VAR mods := ParseModifiers()
                nVis := _AND(mods, Modifiers.VisibilityMask)
                IF nVis == Modifiers.None
                    nVis := Modifiers.Public
                ENDIF
                nMods := _AND(mods, ~Modifiers.VisibilityMask)
                VAR readLine := TRUE
                IF IsStartOfEntity(OUT VAR entityKind, mods)
                    VAR entities := ParseEntity(entityKind, la1, la2, nMods, nVis, first)
                    FOREACH VAR entity IN entities
                        entity:File := _file
                        IF aAttribs?:Count > 0
                            entity:Attributes := TokensAsString(aAttribs)
                        ENDIF
                        VAR lastEntity := entityList:LastOrDefault()
                        IF lastEntity != NULL
                            lastEntity:Range       := lastEntity:Range:WithEnd(lastToken)
                            lastEntity:Interval    := lastEntity:Interval:WithEnd(lastToken)
                        ENDIF
                        entityList:Add(entity)

                        IF entityStack:Count > 0 .AND. (!CurrentEntityKind:HasChildren() .OR. !(CurrentEntity IS XType ) )
                            entityStack:Pop()
                        ENDIF
                        IF CurrentEntityKind:HasChildren() .AND. CurrentEntity IS XType VAR xEnt
                            IF entity IS XTypeMember VAR xMember
                                xEnt:AddMember( xMember )
                            ELSEIF entity IS XType VAR xChild .AND. ! XType.IsGlobalType(xEnt)
                                xEnt:AddChild( xChild )
                                xChild:NameSpace := xEnt:FullName
                            ENDIF
                        ENDIF
                        IF ! entity:SingleLine                        
                            entityStack:Push(entity)
                        ENDIF
                        BlockStack:Clear()
                    NEXT
                    readLine := FALSE
                ELSEIF IsEndOfEntity( OUT VAR endKind)
                    VAR type := La2
                    // match la2 with current entity
                    DO WHILE TRUE .AND. EntityStack:Count > 0
                        VAR top := EntityStack:Pop()
                        IF top:Kind != endKind
                           LOOP
                        ENDIF
                        top:Range       := top:Range:WithEnd(lastToken)
                        top:Interval    := top:Interval:WithEnd(lastToken)
                        EXIT
                    ENDDO
                ELSEIF ParseBlock()
                    readLine := TRUE
                
                ENDIF
                IF readLine
                    SELF:ReadLine()
                ENDIF
            ENDDO
            VAR types := SELF:EntityList:Where( {x => x IS XType})
            VAR typelist := Dictionary<STRING, XType>{System.StringComparer.InvariantCultureIgnoreCase}
            FOREACH type AS XType IN types
                IF ! typelist:ContainsKey(type:Name)
                    typelist:Add(type:Name, type)
                ENDIF
            NEXT
            _file:SetTypes(typeList, _usings, _staticusings, SELF:EntityList)
//            ? EntityList:Count, "Entities found"
//            ? BlockList:Count, "Blocks found"


        METHOD ParsePPLine() AS LOGIC
            VAR token := La1
            SWITCH La1
            CASE XSharpLexer.PP_REGION
            CASE XSharpLexer.PP_IFDEF
            CASE XSharpLexer.PP_IFNDEF
                // start
                VAR block := XBlock{ Lt1, Lt2}
                BlockList:Add(block)
                PPBlockStack:Push(block)
            CASE XSharpLexer.PP_ENDREGION
            CASE XSharpLexer.PP_ENDIF
                // end
                IF PPBlockStack:Count > 0
                    PPBlockStack:Peek():Children:Add( XBlock{Lt1,Lt2})
                    PPBlockStack:Pop()
                ENDIF
            CASE XSharpLexer.PP_ELSE
                // middle
                IF PPBlockStack:Count > 0
                    PPBlockStack:Peek():Children:Add( XBlock{Lt1,Lt2})
                ENDIF
            OTHERWISE
                RETURN FALSE
            END SWITCH
            ReadLine()
            RETURN TRUE

        METHOD ParseUsing() AS LOGIC
            IF La1 != XSharpLexer.USING
                RETURN FALSE
            ENDIF
            VAR startToken := ConsumeAndGet()
            VAR isStatic := FALSE
            VAR alias := ""
            IF La1 == XSharpLexer.STATIC
                isStatic := TRUE
                SELF:Consume()
            ENDIF
            IF IsId(La1) .AND. SELF:IsAssignOp(La2)
                // name :=
                alias := SELF:ConsumeAndGettext()
                SELF:Consume()   // := 
            ENDIF
            VAR name := SELF:ParseQualifiedName()
            IF isStatic
                SELF:_staticusings:Add(name)
            ELSE
                SELF:_usings:Add(name)
            ENDIF
            ReadLine()
            RETURN TRUE
            
        METHOD ParseAttributes() AS IList<IToken>
            VAR tokens := List<IToken>{}
            DO WHILE La1 == XSharpLexer.LBRKT .AND. ! EoS() 
                tokens:Add(ConsumeAndGet())
                DO WHILE La1 != XSharpLexer.RBRKT .AND. ! EoS() 
                    tokens:Add(ConsumeAndGet())
                ENDDO
                IF La1 == XSharpLexer.RBRKT
                    tokens:Add(ConsumeAndGet())
                ENDIF
            ENDDO
            RETURN tokens

        METHOD ParseModifiers() AS   Modifiers
            VAR result := Modifiers.None
            DO WHILE ! EoS()
                VAR done := FALSE
                SWITCH la1
                    // Visibility
                    CASE XSharpLexer.PUBLIC
                         result |= Modifiers.Public   
                    CASE XSharpLexer.EXPORT
                         result |= Modifiers.Public   
                    CASE XSharpLexer.HIDDEN
                         result |= Modifiers.Private
                    CASE XSharpLexer.PRIVATE
                         result |= Modifiers.Private
                    CASE XSharpLexer.PROTECTED
                         result |= Modifiers.Protected
                    CASE XSharpLexer.INTERNAL
                        result |= Modifiers.Internal

                    // Real modifiers

                    CASE XSharpLexer.ABSTRACT
                        result |= Modifiers.Abstract
                    CASE XSharpLexer.EXTERN
                        result |= Modifiers.External

                    CASE XSharpLexer.UNSAFE
                        result |= Modifiers.Unsafe
                    CASE XSharpLexer.STATIC
                        result |= Modifiers.Static
                    CASE XSharpLexer.PARTIAL
                        result |= Modifiers.Partial
                    CASE XSharpLexer.NEW
                        result |= Modifiers.New
                    CASE XSharpLexer.OVERRIDE
                        result |= Modifiers.Override
                    CASE XSharpLexer.VIRTUAL
                        result |= Modifiers.Virtual
                    CASE XSharpLexer.SEALED
                        result |= Modifiers.Sealed
                    CASE XSharpLexer.CONST
                        result |= Modifiers.Const
                    CASE XSharpLexer.INSTANCE
                        result |= Modifiers.Instance
                        result |= Modifiers.Protected
                    CASE XSharpLexer.INITONLY
                        result |= Modifiers.InitOnly
                    OTHERWISE
                        done := TRUE
                    END SWITCH
                    IF ! done
                        Consume()
                    ELSE
                        EXIT
                    ENDIF
                ENDDO
                RETURN result
            
            
        METHOD IsStartOfEntity(entityKind OUT Kind, mods AS Modifiers) AS LOGIC
            entityKind := Kind.Unknown
            SWITCH La1
            CASE XSharpLexer.BEGIN
                // namespace ?
                IF La2 == XSharpLexer.NAMESPACE
                    entityKind := Kind.Namespace
                ENDIF
            CASE XSharpLexer.CLASS
                entityKind := Kind.Class
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
                entityKind := Kind.Ignore
            CASE XSharpLexer.DEFINE
                    // define class ?
                IF La2 == XSharpLexer.CLASS
                    entityKind := Kind.Class
                ELSEIF SELF:IsId(La2)
                    entityKind := Kind.VODefine
                ENDIF
            CASE XSharpLexer.VOSTRUCT
                entityKind := Kind.VOStruct
            CASE XSharpLexer.UNION
                entityKind := Kind.Union
            CASE XSharpLexer.MEMBER
                entityKind := Kind.EnumMember
            CASE XSharpLexer.FIELD
                // field declaration only inside FoxPro class definition
            CASE XSharpLexer.ADD
                    // Todo handle Add Object clause inside Class.
                    // Add Accessor only inside Event
             CASE XSharpLexer.FUNCTION
                entityKind := Kind.Function
            CASE XSharpLexer.PROCEDURE
                entityKind := Kind.Procedure
            CASE XSharpLexer.GLOBAL
                entityKind := Kind.VOGlobal
            CASE XSharpLexer.DLL
                IF La2 == XSharpLexer.FUNCTION .OR. La2 == XSharpLexer.PROCEDURE
                    entityKind := Kind.VODLL
                ENDIF
            OTHERWISE
                IF IsId(La1) .AND. mods != Modifiers.None
                    LOCAL parent AS XType
                    IF CurrentEntity IS XType 
                        parent := (XType) CurrentEntity
                    ELSEIF CurrentEntity != NULL .AND. CurrentEntity:Parent IS XType 
                        parent := (XType) CurrentEntity:Parent 
                    ENDIF
                    IF ! XType.IsGlobalType(parent) 
                        entityKind := Kind.Field
                    ENDIF
                ENDIF               
            END SWITCH
            
            RETURN entityKind != Kind.Unknown


        METHOD ParseBlock() AS LOGIC
            // Adds, updates or removes block token on the block tokens stack
            // Start of block is also added to the blockLIst
            LOCAL nStart  := 0 AS LONG
            LOCAL nMiddle := 0 AS LONG
            LOCAL nEnd    := 0 AS LONG
            SWITCH La1
            CASE XSharpLexer.BEGIN
                SWITCH La2
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
                SWITCH La2
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
                IF La2 == XSharpLexer.USING
                    nMiddle := 2
                ENDIF
            // End of a block
            CASE XSharpLexer.ENDCASE
            CASE XSharpLexer.ENDDO
            CASE XSharpLexer.ENDIF
            CASE XSharpLexer.NEXT
            CASE XSharpLexer.UNTIL
                nEnd := 1
            CASE XSharpLexer.END
                SWITCH La2
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
                IF SELF:_blocks
                    VAR block := XBlock{ Lt1, IIF(nStart == 1, Lt1, Lt2)}
                    BlockList:Add(block)
                    BlockStack:Push(block)
                ENDIF
                IF SELF:_locals
                    VAR tokens := List<IToken>{}
                    DO WHILE ! Eos()
                        IF SELF:Matches(XSharpLexer.IMPLIED, XSharpLexer.VAR)
                            IF SELF:IsId(La2)
                                tokens:Add(SELF:Lt2)
                            ENDIF
                        ENDIF
                        Consume()
                    ENDDO
                    
                ENDIF
                RETURN TRUE
            ENDIF
            IF nMiddle > 0
                // Do something
                IF SELF:_blocks .AND. BlockStack:Count > 0
                    CurrentBlock:Children:Add( XBlock{Lt1,IIF(nMiddle == 1, Lt1, Lt2)})
                ENDIF
                RETURN TRUE
            ENDIF
            IF nEnd > 0
                IF SELF:_blocks .AND. BlockStack:Count > 0
                    CurrentBlock:Children:Add( XBlock{Lt1,IIF(nEnd == 1, Lt1, Lt2)})
                    BlockStack:Pop()
                ENDIF
                RETURN TRUE
            ENDIF
            RETURN FALSE


        METHOD IsEndOfEntity(EntityKind OUT Kind) AS LOGIC
            IF La1 == XSHarpLexer.END
                SWITCH La2
                // mandatory END <keyword> pairs
                CASE XSharpLexer.NAMESPACE
                    EntityKind := Kind.Namespace
                    RETURN TRUE
                CASE XSharpLexer.CLASS
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
                    EntityKind := Kind.Function
                    RETURN TRUE
                    
                CASE XSharpLexer.PROCEDURE
                    EntityKind := Kind.Procedure
                    RETURN TRUE
                
                CASE XSharpLexer.VOSTRUCT
                    EntityKind := Kind.VoStruct
                    RETURN TRUE
                    
                CASE XSharpLexer.UNION
                    EntityKind := Kind.Union
                    RETURN TRUE
                
                // END <nothing> is NOT allowed for entities since that conflicts with the END for blocks
                END SWITCH
            ENDIF                
            EntityKind := Kind.Unknown
            RETURN FALSE
            
   
            

        #region GetTokens
        METHOD La(nToken AS LONG) AS LONG
            nToken += _index-1
            IF (nToken < _input:Count)
                VAR t := _input[nToken]
                RETURN t:Type
            ENDIF
            RETURN XSharpLexer.Eof
            
        METHOD Lt(nToken AS LONG) AS IToken
            nToken += _index-1
            IF (nToken < _input:Count)
                RETURN _input[nToken]
            ENDIF
            RETURN NULL

        METHOD Eoi() AS LOGIC
            RETURN _index >= _input:Count
            
        METHOD Eos() AS LOGIC
            RETURN La1 == XSharpLexer.EOS .OR. Eoi()

        PRIVATE METHOD SaveLastToken() AS VOID
            IF ! SELF:Eoi() .AND. La1 != XSharpLexer.EOS
                _lastToken := _input[_index]
            ENDIF


        METHOD PushBack() AS VOID
            _index-= 1
            RETURN

        METHOD Consume() AS VOID
            SELF:SaveLastToken()
            _index+= 1
            RETURN

        METHOD ConsumeAndGet() AS IToken
            VAR t := _input[_index]
            _lastToken := t
            _index+= 1
            RETURN t

        METHOD ConsumeAndGetText() AS STRING
            VAR t := _input[_index]
            _lastToken := t
            _index+= 1
            RETURN t:GetText()

        METHOD Matches(nTypes PARAMS LONG[]) AS LOGIC
            IF nTypes:Contains(La1)
                RETURN TRUE
            ENDIF
            RETURN FALSE

        METHOD ConsumeAndGetAny(nTypes PARAMS LONG[]) AS IToken
            IF nTypes:Contains(La1)
                RETURN ConsumeAndGet()
            ENDIF
            RETURN NULL
            


        METHOD Expect(nType AS LONG) AS LOGIC
            IF La1 == nType
                Consume()
                RETURN TRUE
            ENDIF
            RETURN FALSE

        METHOD ExpectAny(nTypes PARAMS LONG[]) AS LOGIC
            IF nTypes:Contains(La1)
                Consume()
                RETURN TRUE
            ENDIF
            RETURN FALSE

        METHOD ExpectAndGet(nType AS LONG, t OUT IToken) AS LOGIC
            IF La1 == nType
                t := ConsumeAndGet()
                RETURN TRUE
            ENDIF
            t := NULL
            RETURN FALSE


        METHOD ReadLine() AS VOID
            DO WHILE ! Eos()
                Consume()
            ENDDO
            DO WHILE La1 == XSharpLexer.EOS
                Consume() // Consume the EOS
            ENDDO
            RETURN
        #endregion
        
        METHOD IsId(token AS LONG) AS LOGIC
            IF token == XSharpLexer.ID .OR. token == XSharpLexer.KWID
                RETURN TRUE
            ENDIF
            // Soft keywords need to be 
            RETURN SELF:IsKeywordXs(token) .OR. SELF:IsKeywordFox(token) .OR. SELF:IsKeywordXpp(token)
            

        METHOD ParseIdentifier() AS STRING
            IF SELF:IsId(La1)
                IF La1 == XSharpLexer.KWID
                    RETURN SELF:ConsumeAndGetText():Substring(2)
                ELSE
                    RETURN SELF:ConsumeAndGetText()
                ENDIF
            ENDIF
            RETURN ""
                

        METHOD ParseOptionalClassClause() AS STRING
            IF La1 == XSharpLexer.CLASS
                SELF:Consume()
                RETURN SELF:ParseQualifiedName()
            ENDIF
            RETURN ""

            
        METHOD ParseQualifiedName() AS STRING
            LOCAL result := "" AS STRING
            VAR Tokens := List<IToken>{}
            IF La1 == XSharpLexer.ID .OR. La1 == XSharpLexer.KWID
                Tokens:Add(ConsumeAndGet())
                DO WHILE La1 == XSharpLexer.DOT .AND.  SELF:IsId(La2)
                    Tokens:Add(SELF:ConsumeAndGet())
                    Tokens:Add(SELF:ConsumeAndGet())
                ENDDO
            ENDIF
            RETURN TokensAsString(tokens)

        METHOD TokensAsString(tokens AS IList<IToken>) AS STRING
            LOCAL sb AS StringBuilder
            sb := StringBuilder{}
            FOREACH VAR t IN tokens
                sb:Append(t:GetText())
            NEXT
            RETURN sb:ToString()

        METHOD ParseEntity(entityKind AS Kind, iToken AS LONG, iToken1 AS LONG, nMods AS Modifiers, nVis AS Modifiers, start AS IToken) AS IList<XElement>
            LOCAL id AS STRING
            LOCAL sig    AS XSignature
            LOCAL range AS TextRange
            LOCAL interval AS TextInterval
            VAR  result := List<XElement>{}
            SELF:Consume()
            SWITCH entityKind
            CASE Kind.Ignore
                ReadLine()
            CASE Kind.NameSpace
                    // Consume 2 tokens and read Id with optional namespace prefix
                    Consume()   // Namespace token
                    id := ParseQualifiedName()
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    result:Add(XType{id, entityKind, nMods, nVis, range, interval,_file})
                    
                    
                CASE Kind.Class
                CASE Kind.Structure
                CASE Kind.Interface
                    // read Id with optional namespace prefix
                    id := ParseQualifiedName()
                    LOCAL typePars      AS List<STRING>
                    LOCAL constraints   AS List<STRING>
                    LOCAL parentType    AS STRING
                    LOCAL interfaces    AS List<STRING>
                    IF La1 == XSharpLexer.LT
                        typePars := SELF:ParseTypeParameters()
                    ENDIF
                    // get inherit clause
                    IF La1 == XSharpLexer.INHERIT .OR. La1 == XSharpLexer.COLON
                        Consume()
                        parentType := ParseTypeName()
                    ENDIF
                    // get implements clause + list of interfaces
                    IF La1 == XSharpLexer.IMPLEMENTS
                        interfaces := List<STRING>{}
                        DO WHILE La1 == XSharpLexer.IMPLEMENTS .OR. La1 == XSharpLexer.COMMA
                            Consume()
                            interfaces:Add(parseTypeName())
                        ENDDO
                    ENDIF
                    DO WHILE La1 == XSharpLexer.WHERE
                        IF constraints == NULL
                            constraints := List<STRING>{}
                        ENDIF
                        constraints:Add(SELF:ParseTypeParameterConstraints())
                    ENDDO
                    // read to EndOfLine
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    
                    VAR xType := XType{id, entityKind, nMods, nVis, range, interval, _file}
                    IF interfaces?:Count > 0
                        FOREACH VAR sInterface IN interfaces
                            xType:AddInterface(sInterface)
                        NEXT
                    ENDIF
                    IF ! String.IsNullOrEmpty(parentType)
                        xType:ParentName := parentType
                    ENDIF
                    xType:IsPartial := nMods:HasFlag(Modifiers.Partial)
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
                    result:Add(xType)
                    
                CASE Kind.Delegate
                    sig := SELF:ParseSignature()
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 

                    readLine()

                    VAR xtype := XType{sig:Id, entityKind, nMods, nVis, range, interval,_file}
                    VAR xmember := XTypeMember{sig:Id, entityKind, nMods, nVis, ;
                        range, interval,sig:DataType, nMods:HasFlag(Modifiers.Static)}
                    xmember:SingleLine := TRUE
                    xtype:AddMember(xmember)
                    xtype:SingleLine := TRUE
                    SELF:CopySignatureValues(xmember, sig)
                    result:Add(xType)

                CASE Kind.Access
                CASE Kind.Assign
                CASE Kind.Method
                    sig := SELF:ParseSignature()
                    VAR classClause := ParseOptionalClassClause()
                    // read to EndOfLine
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    
                    VAR xmember := XTypeMember{sig:Id, entityKind, nMods, nVis, range, interval,sig:DataType, nMods:HasFlag(Modifiers.Static)}
                    SELF:CopySignatureValues(xmember, sig)
                    result:Add(xmember)
                    
                CASE Kind.Function
                CASE Kind.Procedure
                    sig := SELF:ParseSignature()

                    VAR initexit := ""
                    IF SELF:Matches(XSharpLexer.INIT1,XSharpLexer.INIT2,XSharpLexer.INIT3,XSharpLexer.EXIT)
                        initexit := SELF:ConsumeAndGetText()
                    ENDIF
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    
                    VAR xmember := XTypeMember{sig:Id, entityKind, nMods, nVis, range, interval,sig:DataType, nMods:HasFlag(Modifiers.Static)}
                    SELF:CopySignatureValues(xmember, sig)
                    result:Add(xmember)
            CASE Kind.VODLL
                    VAR t := ConsumeAndGet()
                    id := ParseQualifiedName()
                    VAR aParams := SELF:ParseParameterList()
                    VAR sType   := SELF:ParseAsIsType()
                    VAR cc      := SELF:ParseCallingConvention()
                    VAR colon   := SELF:ConsumeAndGetText()
                    VAR dllName := SELF:ConsumeAndGetText()
                    VAR dotName := ""
                    IF Matches(XSharpLexer.DOT)
                        Consume()
                        dotName := SELF:ConsumeAndGetText()
                    ENDIF
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    VAR xMember := XTypeMember{id, entityKind, nMods, nVis, range, interval,sType, nMods:HasFlag(Modifiers.Static)}
                    xMember:AddParameters(aParams)
                    xMember:SubType := t:GetText()
                    xMember:CallingConvention := cc
                    result:Add(xmember)
                
            CASE Kind.VOStruct
                    id := ParseQualifiedName()
                    VAR sAlign := ""
                    IF La1 == XSharpLexer.ALIGN .AND. La2 == XSharpLexer.INT_CONST
                        sAlign := SELF:Lt2:GetText() // Align number
                        Consume()
                        Consume()
                    ENDIF
                    // read to EndOfLine
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    VAR xType := XType{id, entityKind, nMods, nVis, range, interval,_file}
                    IF String.IsNullOrEmpty(sAlign)
                        xType:AddInterface(sAlign)
                    ENDIF
                    result:Add(xType)
            CASE Kind.Union
                    id := ParseQualifiedName()
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    result:Add(XType{id, entityKind, nMods, nVis, range, interval,_file})

            CASE Kind.Enum
                    id := ParseIdentifier()
                    VAR type := ""
                    IF Matches(XSharpLexer.AS)
                        type := SELF:ParseAsIsType()
                    ELSEIF Matches(XSharpLexer.INHERIT)
                        Consume() // inherit clause
                        type := SELF:ParseTypeName()
                    ENDIF
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    VAR xType := XType{id, entityKind, nMods, nVis, range, interval, _file}
                    xType:ParentName := type
                    xType:SingleLine := FALSE
                    result:Add(xType)   

            CASE Kind.EnumMember
                    IF SELF:CurrentEntity:Kind == Kind.Enum
                        VAR att := SELF:ParseAttributes()
                        VAR strValue := ""
                        id := ParseQualifiedName()
                        IF SELF:IsAssignOp(La1)
                            Consume() // :=
                            strValue := ParseExpression()
                        ENDIF
                        range    := textRange{start, lastToken}
                        interval := textInterVal{start, lastToken} 
                        VAR xMember := XTypeMember{id, entityKind, nMods, nVis, range, interval, ""}
                        xMember:Value := strValue
                        xMember:SingleLine := TRUE
                        result:Add(xMember)
                    ELSE

                        LOCAL isArray AS LOGIC
                        LOCAL sBracket AS STRING
                        LOCAL sType    AS STRING
                        IF Matches(XSharpLexer.DIM)
                            // read optional DIM clause
                            isArray := TRUE
                            Consume()
                        ENDIF
                        id := ParseQualifiedName()
                        IF isArray
                            DO WHILE La1 == XSharpLexer.LBRKT           // allow SomeId[1][2]
                                sBracket += SELF:ConsumeAndGetText()
                                DO WHILE La1 != XSharpLexer.RBRKT .AND. ! SELF:Eos()        
                                    sBracket += SELF:ConsumeAndGetText()
                                ENDDO
                                IF La1 == XSharpLexer.RBRKT
                                    sBracket += SELF:ConsumeAndGetText()
                                ENDIF
                            ENDDO
                        ENDIF
                        sType := SELF:ParseAsIsType()
                        range    := textRange{start, lastToken}
                        interval := textInterVal{start, lastToken} 
                        readLine()
                        IF isArray
                            sType += "[]"
                        ENDIF
                        VAR xMember := XTypeMember{id, Kind.Field, nMods, nVis, range, interval, sType}
                        xMember:SingleLine := TRUE
                        xMember:IsArray := isArray
                        result:Add(xMember)
                ENDIF                    
            CASE Kind.VODefine
                    id := ParseQualifiedName()
                    LOCAL strValue AS STRING
                    IF SELF:IsAssignOp(La1)
                        Consume() // :=
                        strValue := ParseExpression()
                    ENDIF
                    VAR type := SELF:ParseAsIsType()
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    
                    VAR xmember := XTypeMember{id, entityKind, nMods, nVis, range,interval, type}
                    xmember:Value := strValue
                    xmember:SingleLine := TRUE
                    result:Add(xmember)
                    
            CASE Kind.VOGlobal
                    IF La1 == XSharpLexer.CONST
                        Consume()
                    ENDIF
                    VAR classvars := ParseClassVarList(entityKind)
                    LOCAL first := TRUE AS LOGIC
                    FOREACH classvar AS XElement IN classvars
                        VAR element := XTypeMember{classVar:Name, entityKind, nMods, nVis, classvar:Range, classvar:Interval, classvar:Typename, FALSE}
                        element:Range       := element:Range:WithEnd(lastToken)
                        element:Interval    := element:Interval:WithEnd(lastToken)
                        element:File := _file
                        element:SingleLine := TRUE
                        result:Add(element)
                    NEXT
                    readLine()
                    
            CASE Kind.Property
                    IF SELF:Matches(XSharpLexer.SELF)   // Self property
                        id := ConsumeAndGetText()
                    ELSE
                        id := ParseQualifiedName()
                    ENDIF
                    VAR aParams := List<XVariable>{}
                    IF Matches(XSharpLexer.LPAREN)
                        aParams := ParseParameterList()
                    ELSEIF Matches(XSharpLexer.LBRKT)
                        aParams := ParseParameterList(TRUE)
                    ENDIF
                    VAR sType := ParseAsIsType()
                    LOCAL lSingleLine AS LOGIC
                    DO WHILE ! Eos()
                        IF Matches(XSharpLexer.AUTO, XSharpLexer.GET,XSharpLexer.SET)
                            lSingleLine := TRUE
                        ENDIF
                        Consume()
                    ENDDO
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    VAR xmember := XTypeMember{id, entityKind, nMods, nVis, range, interval,""}
                    IF (lSingleLine)
                        xMember:SingleLine := TRUE
                    ENDIF
                    xmember:AddParameters(aParams)
                    result:Add(xMember)
                
            CASE Kind.Event
                    id := ParseQualifiedName()
                    LOCAL lSingleLine AS LOGIC
                    DO WHILE ! Eos()
                        IF Matches( XSharpLexer.ADD,XSharpLexer.REMOVE)
                            lSingleLine := TRUE
                        ENDIF
                        Consume()
                    ENDDO
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    VAR xmember := XTypeMember{id, entityKind, nMods, nVis, range, interval,""}
                    IF (lSingleLine)
                        xMember:SingleLine := TRUE
                    ENDIF
                    result:Add(xMember)
                                
            CASE Kind.Operator
                    LOCAL t1 AS IToken
                    LOCAL t2 AS IToken 
                    IF Matches(XSharpLexer.PLUS , XSharpLexer.MINUS , XSharpLexer.NOT , XSharpLexer.TILDE , XSharpLexer.INC, ;
                            XSharpLexer.DEC ,XSharpLexer.TRUE_CONST ,XSharpLexer.FALSE_CONST , XSharpLexer.MULT , ;
                            XSharpLexer.DIV ,XSharpLexer.MOD ,XSharpLexer.AMP ,XSharpLexer.PIPE ,XSharpLexer.LSHIFT ,XSharpLexer.RSHIFT, ;
                            XSharpLexer.EEQ , XSharpLexer.NEQ , XSharpLexer.NEQ2 ,XSharpLexer.GT , XSharpLexer.LT ,;
                            XSharpLexer.GTE , XSharpLexer.LTE , XSharpLexer.AND , XSharpLexer.OR, XSharpLexer.IMPLICIT, XSharpLexer.EXPLICIT)
                            t1 := SELF:ConsumeAndGet()
                    ENDIF
                    IF Matches (XSharpLexer.GT)
                         t2 := SELF:ConsumeAndGet()
                    ENDIF
                    VAR aParams     := SELF:ParseParameterList()
                    VAR sType       := SELF:ParseAsIsType()
                    
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 

                    id := t1:GetText()+ IIF(t2 != NULL, t2:GetText(),"")
                    readLine()
                    VAR xmember := XTypeMember{id, entityKind, nMods, nVis, range, interval,sType}
                    xmember:AddParameters(aParams)
                    result:Add(xmember)
                
            CASE Kind.Constructor
                    id  := ".ctor"
                    VAR aParams     := SELF:ParseParameterList()
                    VAR asType      := SELF:ParseAsisType()
                    VAR callconv    := SELF:ParseCallingConvention()
                    VAR classClause := SELF:ParseOptionalClassClause()
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    VAR xmember := XTypeMember{id, entityKind, nMods, nVis,range, interval,""}
                    xmember:AddParameters(aParams)
                    result:Add(xmember)
                    
                
            CASE Kind.Destructor
                    id  := ".dtor"
                    IF La1 == XSharpLexer.LPAREN .AND. La2 == XSharpLexer.RPAREN
                        Consume()
                        Consume()
                    ENDIF
                    VAR classClause := SELF:ParseOptionalClassClause()
                    range    := textRange{start, lastToken}
                    interval := textInterVal{start, lastToken} 
                    readLine()
                    result:Add(XTypeMember{id, entityKind, nMods, nVis, range, interval,"VOID"})
                    
            CASE Kind.Field
                    SELF:PushBack()
                    VAR classvars := ParseClassVarList(entityKind)
                    LOCAL first := TRUE AS LOGIC
                    FOREACH classvar AS XElement IN classvars
                        VAR element := XTypeMember{classVar:Name, entityKind, nMods, nVis, classvar:Range, classvar:Interval, classvar:Typename, FALSE}
                        element:Range       := textRange{start, lastToken}
                        element:Interval    := TextInterval {start, lastToken}
                        element:SingleLine := TRUE
                        result:Add(element)
                    NEXT
                    readLine()
            END SWITCH            
            RETURN result



    METHOD CopySignatureValues(xmember AS XTypeMember, sig AS XSignature) AS VOID
        IF sig:Parameters?:Count > 0
            xmember:AddParameters(sig:Parameters)
        ENDIF
        IF sig:TypeParameters?:Count > 0
            FOREACH VAR par IN sig:TypeParameters
                xmember:AddTypeParameter(par)
            NEXT
        ENDIF
        IF sig:TypeParameterContraints:Count > 0
            FOREACH VAR constr IN sig:TypeParameterContraints
                xmember:AddConstraints(constr)
            NEXT
        ENDIF
        xmember:CallingConvention := sig:CallingConvention
        // typename was already set when creating xMember
        

    METHOD ParseClassVarList(eKind AS Kind)   AS List<Xelement>
        LOCAL aVars  AS List<Xelement>
        LOCAL sType  AS STRING
        aVars := List<Xelement>{}
        aVars:Add(ParseClassVar(eKind))
        DO WHILE La1 == XSharpLexer.COMMA
            SELF:Consume()
            aVars:Add(ParseClassVar(eKind))
        ENDDO
        sType := SELF:ParseAsisType()
        FOREACH VAR element IN aVars
            element:TypeName := sType
        NEXT
        RETURN aVars            



    METHOD ParseClassVar(eKind AS Kind) AS XeLement
        LOCAL IsDim  := FALSE  AS LOGIC
        LOCAL sId        AS STRING
        LOCAL sBracket   AS STRING
        LOCAL sDefault   AS STRING
        LOCAL startToken AS IToken
        LOCAL endToken   AS IToken
        startToken := Lt1
        IF La1 == XSharpLexer.DIM
            isDim := TRUE
            SELF:Consume()
        ENDIF
        sId  := SELF:ParseIdentifier()
        DO WHILE La1 == XSharpLexer.LBRKT           // allow SomeId[1][2]
            sBracket += SELF:ConsumeAndGetText()
            DO WHILE La1 != XSharpLexer.RBRKT .AND. ! SELF:Eos()        
                sBracket += SELF:ConsumeAndGetText()
            ENDDO
            
        ENDDO
        IF IsAssignOp(La1)
            Consume()   // :=
            sDefault := SELF:ParseExpression()
        ENDIF
        endToken := SELF:lastToken
        VAR range := TextRange{startToken, endToken}
        VAR interval := TextInterVal{startToken, endToken}
        VAR result := XElement{sId,eKind, Modifiers.None, Modifiers.None, range,interval}
        result:Value := sDefault
        IF IsDim .OR. !String.IsNullOrEmpty(sBracket)
            result:IsArray := TRUE
        ENDIF
        RETURN result
        
            

    METHOD ParseSignature() AS XSignature
        LOCAL oSig          AS XSignature
        oSig                := XSignature{}
        oSig:Id             := SELF:ParseQualifiedName()
        IF La1 == XSharpLexer.LT
            oSig:TypeParameters := SELF:ParseTypeParameters()
        ENDIF
        IF La1 == XSharpLexer.LPAREN
            oSig:Parameters := SELF:ParseParameterList()
        ENDIF
        oSig:DataType       := SELF:ParseAsIsType()
        DO WHILE La1 == XSharpLexer.WHERE .AND. ! Eos()
            oSig:TypeParameterContraints:Add(SELF:ParseTypeParameterConstraints())
        ENDDO
        oSig:CallingConvention  := SELF:ParseCallingConvention()
        RETURN oSig
        


    METHOD ParseTypeParameters AS List<STRING>
        IF La1 != XSharpLexer.LT
            RETURN NULL
        ENDIF
        Consume()
        VAR aTypeParams := List<STRING>{}
        DO WHILE La1 != XSharpLexer.GT .AND. !Eos()
            VAR sParam := SELF:TokensAsString(ParseAttributes())
            
            IF La1 == XSharpLexer.IN .OR. La1 == XSharpLexer.OUT
                sParam += ConsumeAndGetText()+" "
            ENDIF
            IF SELF:IsId(La1)
               sParam += ConsumeAndGetText()
            ENDIF
            aTypeParams:Add(sParam)
            IF La1 == XSharpLexer.COMMA
                Consume()
            ENDIF
        ENDDO
        IF La1 == XSharpLexer.GT
            Consume()
        ENDIF
        RETURN aTypeParams 
        


    METHOD ParseParameterList(lBracketed := FALSE AS LOGIC) AS List<XVariable>
        IF La1 != XSharpLexer.LPAREN .AND. ! lBracketed
            RETURN NULL
        ENDIF
        IF La1 != XSharpLexer.LBRKT .AND. lBracketed
            RETURN NULL
        ENDIF
        Consume()   // LParen
        VAR aResult  := List<XVariable>{}
        VAR start := Lt1
        LOCAL cond AS DelEndToken
        cond := { token => IIF (lBracketed, token == XSharpLexer.RBRKT, token == XSharpLexer.RPAREN ) }
        DO WHILE !cond(La1) .AND. ! Eos()
            VAR atts := SELF:TokensAsString(ParseAttributes())
            VAR isSelf := FALSE 
            VAR sId   := ""
            VAR sTypeName := ""
            IF La1 == XSharpLexer.SELF
                isSelf := TRUE
                Consume()
            ENDIF
            IF SELF:IsId(La1)
                sId  += SELF:ConsumeAndGetText()
            ENDIF
            IF IsAssignOp(La1)
                // parse default value
            ENDIF
            
            VAR token := SELF:ConsumeAndGetAny(XSharpLexer.AS, XSharpLexer.IN, XSharpLexer.OUT, XSharpLexer.REF,XSharpLexer.PARAMS)
            IF token != NULL
                IF La1 == XSharpLexer.CONST
                    Consume()
                ENDIF
                sTypeName := SELF:ParseTypeName()
            ENDIF
            LOCAL variable AS XVariable
            VAR endToken := SELF:lastToken
            VAR range    := TextRange{start, lastToken}
            VAR interval := textInterVal{start, lastToken}
            
            variable := XVariable{NULL, sId, Kind.Parameter,range,interval, sTypeName, TRUE}
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
            aResult:Add(variable)
            IF La1 == XSharpLexer.COMMA
                Consume()
            ELSE
                EXIT
            ENDIF
        ENDDO
        IF Cond(La1)
            Consume()
        ENDIF
        RETURN aResult


    METHOD ParseTypeParameterConstraints() AS STRING

        IF La1 != XSharpLexer.WHERE
            RETURN ""
        ENDIF
        VAR result := SELF:ConsumeAndGetText() +" "
        IF SELF:IsId(La1) .AND. La2 == XSharpLexer.IS
            result     += SELF:ParseIdentifier() +" "
            result     += SELF:ConsumeAndGetText()+" "
            result     += SELF:ParseTypeParameterConstraint()+" "
            DO WHILE La1 == XSharpLexer.COMMA .AND. ! Eos()
                result += SELF:ConsumeAndGetText()+" "
                result     += SELF:ParseTypeParameterConstraint()+" "
            ENDDO
        ENDIF
        RETURN result

    METHOD ParseTypeParameterConstraint() AS STRING
        IF La1 == XSharpLexer.CLASS .OR. La1 == XSharpLexer.STRUCTURE
            RETURN ConsumeAndGetText()
        ELSEIF La1 == XSharpLexer.NEW .AND. La2 == XSharpLexer.LPAREN .AND. La3 == XSharpLexer.RPAREN
            RETURN "NEW()"
        ENDIF
        RETURN ParseTypeName()
        


    METHOD ParseAsIsType() AS STRING
        IF La1 == XSharpLexer.AS .OR. La1 == XSharpLexer.IS
            SELF:Consume()
            RETURN SELF:ParseTypeName()
        ENDIF
        RETURN ""

    METHOD ParseCallingConvention() AS STRING
        SWITCH La1
        CASE XSharpLexer.CLIPPER
        CASE XSharpLexer.STRICT
        CASE XSharpLexer.PASCAL
        CASE XSharpLexer.ASPEN
        CASE XSharpLexer.WINCALL
        CASE XSharpLexer.CALLBACK
        CASE XSharpLexer.FASTCALL
        CASE XSharpLexer.THISCALL
            RETURN SELF:ConsumeAndGetText()
        END SWITCH
        RETURN ""


    METHOD IsAssignOp (nToken AS LONG) AS LOGIC
        RETURN nToken == XSharpLexer.ASSIGN_OP .OR. nToken == XSharpLexer.EQ

    METHOD ParseTypeName() AS STRING
        SWITCH La1
            
        CASE XSharpLexer.ID
            RETURN SELF:ParseQualifiedName() + SELF:ParseTypeSuffix()
        CASE XSharpLexer.ARRAY
        CASE XSharpLexer.CODEBLOCK
        CASE XSharpLexer.DATE
        CASE XSharpLexer.FLOAT
        CASE XSharpLexer.PSZ
        CASE XSharpLexer.SYMBOL
        CASE XSharpLexer.USUAL
            RETURN ConsumeAndGet():GetText() + SELF:ParseTypeSuffix()
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
            RETURN SELF:ConsumeAndGet():GetText() + SELF:ParseTypeSuffix()
        OTHERWISE
            IF XSharpLexer.IsKeyword(La1)
                RETURN SELF:ConsumeAndGet():GetText() + SELF:ParseTypeSuffix()
            ENDIF
        END SWITCH
        RETURN ""

       INTERNAL METHOD ParseTypeSuffix AS STRING
            IF La1 == XSharpLexer.PTR
                RETURN " "+SELF:ConsumeAndGet():GetText()
            ELSEIF La1 = XSharpLexer.QMARK
                RETURN " "+SELF:ConsumeAndGet():GetText()
            ELSEIF La1 == XSharpLexer.LBRKT
                VAR tokens := List<IToken>{}
                tokens:Add(SELF:ConsumeAndGet())
                DO WHILE (La1 == XSharpLexer.COMMA || La1 == XSharpLexer.RBRKT) .AND. ! Eos()
                    tokens:Add(SELF:ConsumeAndGet())
                ENDDO
                RETURN SELF:TokensAsString(tokens)
            ENDIF
            RETURN ""


    METHOD ParseExpression() AS STRING
        // parse until Eos() or tokens such as AS, IS, 
        LOCAL nested := 0 AS LONG
        LOCAL done  := FALSE AS LOGIC
        VAR tokens  := List<IToken>{} 
        DO WHILE ! eos() .AND. ! done
            SWITCH La1
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
        RETURN SELF: TokensAsString(tokens)


    METHOD IsKeywordXs(token AS LONG) AS LOGIC
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

    METHOD IsKeywordXpp(token AS LONG) AS LOGIC
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

    METHOD IsKeywordFox(token AS LONG) AS LOGIC
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

        STATIC METHOD GetText(SELF token AS IToken) AS STRING
            VAR result := token:Text
            IF result:StartsWith("@@")
                result := result:Substring(2)
            ENDIF
            RETURN result
       
    END CLASS
    

  
    
END NAMESPACE


        
    






