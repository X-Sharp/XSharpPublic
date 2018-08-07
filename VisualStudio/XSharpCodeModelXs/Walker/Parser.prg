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

//
//function Start() as void
//local cFileName as string
////	cFileName := "C:\Test\test.prg"
////	cFileName := "C:\VIDE\Projects\XIDE\VIDE\PIDE.prg"
//cFileName := "c:\temp\InventoryRepILst.prg"
//Parser.ParseAndDisplay(File.ReadAllLines(cFileName))
//return	
//
BEGIN NAMESPACE XSharpModel
	CLASS Parser
		STATIC PROTECT oEntityMarkers AS Dictionary<STRING , STRING>
		STATIC PROTECT oEntityVisibility AS Dictionary<STRING , STRING>
		STATIC PROTECT oDirectives AS Dictionary<STRING , STRING>
		STATIC PROTECT hBrk AS Dictionary<CHAR,CHAR>
		STATIC PROTECT aTokenIn AS STRING[]
		STATIC PROTECT aTokenInOut AS STRING[]
		STATIC PROTECT aTokenOut AS STRING[]
		STATIC PROTECT aEntityWords AS STRING[]
		STATIC PROTECT aOperators AS CHAR[]
		STATIC PROTECT aEndKeywords AS STRING[]
		STATIC PROTECT aTypes AS STRING[]
		
		// instance fields
		PRIVATE aEntities AS List<EntityObject> 
		PRIVATE aLocals   AS List<EntityObject> 
		PRIVATE aResult   AS List<EntityObject> 
		PRIVATE aLinesWithSpecialStuff AS List<LineObject> 
		PRIVATE aLines    AS List<LineObject> 
		PRIVATE aSourceLines    AS IList<STRING>
		PRIVATE aTypeStack AS Stack<EntityObject>
		PRIVATE oGlobalObject AS EntityObject
		PRIVATE _nLength  AS INT
        PRIVATE iClassOrStruct AS INT
				
		CONSTRUCTOR()
			SELF:aEntities				:= List<EntityObject>{}
			SELF:aLocals				:= List<EntityObject>{}
			SELF:aLinesWithSpecialStuff := List<LineObject>{}
			SELF:aLines					:= List<LineObject>{}

			SELF:aTypeStack					:= Stack<EntityObject>{}
			SELF:oGlobalObject				:= EntityObject{EntityType._Class}
			SELF:oGlobalObject:cName		:= XELement.GlobalName
			SELF:oGlobalObject:lStatic		:= true
			SELF:oGlobalObject:lPartial		:= true
			SELF:oGlobalObject:nStartLine	:= 1
			SELF:oGlobalObject:nCol			:= 1
            SELF:iClassOrStruct             := 0
		
		
		STATIC CONSTRUCTOR()
			oEntityMarkers := Dictionary<STRING , STRING>{}
			oEntityVisibility := Dictionary<STRING , STRING>{}
			oDirectives := Dictionary<STRING , STRING>{}
			hBrk := Dictionary<CHAR,CHAR>{}
			aTokenIn := <STRING>{"IF", "DO", "WHILE", "FOR", "FOREACH", "TRY", "REPEAT", "SWITCH"} 
			aTokenInOut := <STRING>{"ELSE", "ELSEIF", "CASE", "OTHERWISE", "CATCH", "FINALLY", "RECOVER" } 
			aTokenOut := <STRING>{"ENDIF", "ENDDO", "ENDCASE", "NEXT", "ENDTRY", "UNTIL"}
			aEntityWords := <STRING>{"EVENT" , "PROTECT" , "PROTECTED", "INSTANCE" , "EXPORT" , "PUBLIC" , "PRIVATE" , "HIDDEN" , "INTERNAL" , "MEMBER" , "GLOBAL"} 
			aOperators := <CHAR>{'+','-','*','/','%','&','|','>','<','=','!','~'}
			aEndKeyWords := <STRING>{"CLASS","STRUCTURE","STRUCT","INTERFACE","ENUM"}
			aTypes := <STRING>{"CLASS","STRUCTURE","STRUCT","INTERFACE","DELEGATE","ENUM","VOSTRUCT","UNION"}
			LOCAL aWords AS STRING[]
			
			aWords := <STRING>{;
								"CLASS","METHOD","FUNCTION","PROCEDURE","FUNC","PROC","ACCESS","ASSIGN","OPERATOR","DELEGATE",;
								"GLOBAL","CONSTRUCTOR","DESTRUCTOR","STRUCTURE","STRUCT","VOSTRUCT","UNION","ENUM","INTERFACE","PROPERTY","DEFINE"}
			FOREACH cWord AS STRING IN aWords
				oEntityMarkers:Add(cWord:ToUpper() , cWord)
			NEXT
			
			
			aWords := <STRING>{;
								"VIRTUAL", "PARTIAL", "_DLL", "ABSTRACT", "SEALED", ;
								"INTERNAL", "HIDDEN", "STATIC", "PROTECTED", "INSTANCE", ;
								"PROTECT", "PRIVATE", "PUBLIC", "EXPORT", "CONST", ;
								"INITONLY", "MEMBER", "NEW", "ASYNC", "EXTERN", "UNSAFE", "OVERRIDE"}
			FOREACH cWord AS STRING IN aWords
				oEntityVisibility:Add(cWord:ToUpper() , cWord)
			NEXT
			
			aWords := <STRING>{;
								"ifdef","ifndef","endif","else","define","undef","command","translate",;
								"region","endregion","using","include","line","pragma","error","warning"}
			FOREACH cWord AS STRING IN aWords
				oDirectives:Add(cWord:ToUpper() , cWord)
			NEXT
			
			LOCAL cBreak AS STRING
			LOCAL n AS INT
			cBreak := e",./;:[]<>?{}`~!@#$%^&*()-=\\+|'\""
			hBrk := Dictionary<CHAR,CHAR>{50}
			FOR n := 0 UPTO cBreak:Length - 1
				hBrk:Add(cBreak[n] , cBreak[n])
			NEXT
			
			RETURN
		
		INTERNAL METHOD _SetLineType(oLine AS LineObject, eLineType AS LineType) AS VOID
			oLine:eType := eLineType
			IF ! SELF:aLinesWithSpecialStuff:Contains(oLine)
				SELF:aLinesWithSpecialStuff:Add(oLine)
			ENDIF
		
		PROPERTY Entities		AS IList<EntityObject>	GET aEntities				
		PROPERTY Types		    AS IList<EntityObject>	GET aResult
		PROPERTY Locals		    AS IList<EntityObject>	GET aLocals
		PROPERTY SpecialLines	AS IList<LineObject>	GET aLinesWithSpecialStuff 
		PROPERTY SourceLength   AS INT					GET _nLength -2 // exclude last CRLF
		PROPERTY LineCount      AS INT					GET aSourceLines:Count

		METHOD AddEntity(oInfo AS EntityObject, oLine AS LineObject) AS VOID
			LOCAL oLast AS EntityObject
			IF aEntities:Count > 0
				oLast := aEntities[aEntities:Count-1]
			ENDIF
			aEntities:Add(oInfo)
			IF oLast != NULL_OBJECT
				oLast:oNext := oInfo
			ENDIF
			RETURN

        METHOD getParentType() AS EntityObject
            LOCAL oParent := NULL AS EntityObject
            IF ( aTypeStack:Count > 0 )
                // Per default, it is the last type in Stack
                oParent := aTypeStack:Peek()
                // but we may found a DELEGATE, a VOSTRUCT, ... that we MUST skip (and so, we may support Nested Types ?)
                IF ( !oParent:eType:SupportNestedType() )
                    // Walk down the stack to the right type
                    VAR nPos := 1
                    WHILE ( nPos < aTypeStack:Count )
                        oParent := aTypeStack:ElementAt<EntityObject>( nPos )
                        IF ( !oParent:eType:SupportNestedType() )
                            oParent := NULL
                        ELSE
                            EXIT
                        ENDIF
                        nPos++
                    ENDDO
                ENDIF
            ENDIF
            RETURN oParent
			


		METHOD Parse(aLineCollection AS IList<STRING>, lIncludeLocals AS LOGIC) AS ParseResult
			LOCAL oLine AS LineObject
			LOCAL oStatementLine AS LineObject
			LOCAL nLine,nLineLen AS INT
			LOCAL cChar,cOldChar,cOldOldChar AS CHAR
			LOCAL cRealChar AS CHAR
			LOCAL cTestChar AS CHAR
			LOCAL sWord AS StringBuilder
			LOCAL cWord,cUpperWord AS STRING
			LOCAL state AS ParseState
			LOCAL lIsBreakChar AS LOGIC
			LOCAL lIsSpaceChar AS LOGIC
			LOCAL lEscapedWord AS LOGIC
			LOCAL lEscapedChar AS LOGIC
			LOCAL lEscapedString AS LOGIC
			LOCAL lInEnum AS LOGIC
			LOCAL lFindingType AS LOGIC
			LOCAL lFindingName AS LOGIC
			LOCAL sFoundType AS StringBuilder
			LOCAL lNewCommandInLine AS LOGIC
			LOCAL lContinueNextLine AS LOGIC
			LOCAL lBeforeLexerChange AS LOGIC
			LOCAL cEnumType AS STRING
			LOCAL nChar AS INT
			LOCAL oInfo AS EntityObject
			LOCAL oCurrentMethod AS EntityObject
			LOCAL lStatic AS LOGIC
			LOCAL eAccessLevel AS AccessLevel
			LOCAL eModifiers AS EntityModifiers
			LOCAL eLexer AS LexerStep
			LOCAL eStep AS ParseStep
			LOCAL nParamType AS ParamType
			LOCAL aLineFields AS List<EntityObject>
			LOCAL aLineLocals AS List<EntityObject>
			LOCAL nBracketCount AS INT
			LOCAL cBracketOpen , cBracketClose AS CHAR
			LOCAL lPartial AS LOGIC
			LOCAL n,n1,n2 AS INT
			
			LOCAL lMustLoop AS LOGIC
			LOCAL cCharBeforeWord AS CHAR
			LOCAL lInAttribute AS LOGIC
			LOCAL lAllowAttribute AS LOGIC
			LOCAL nEntityStartLine AS INT
			LOCAL nEntityStartOffset AS INT
			LOCAL nEntityStartCol AS INT
			LOCAL nAt AS INT
			
			LOCAL eWordStatus AS WordStatus
			LOCAL eWordSubStatus AS WordSubStatus
			LOCAL eCharStatus AS WordStatus
			LOCAL eCharSubStatus AS WordSubStatus
			
			LOCAL cShortClassName AS STRING
			LOCAL cTypedClassName AS STRING
			LOCAL cClassNameSpace AS STRING
			LOCAL cClassType AS STRING
			LOCAL cBaseNameSpace AS STRING
			LOCAL aNameSpaces AS List<STRING>
			
			LOCAL hVis , hEnt AS Dictionary<STRING,STRING>
			
			LOCAL lInProperty AS LOGIC
			LOCAL nAfterColonEquals AS INT
			aSourceLines := aLineCollection
			aLineFields := List<EntityObject>{}
			aLineLocals := List<EntityObject>{}
			cShortClassName := ""
			cTypedClassName := ""
			cClassNameSpace := ""
			cClassType := ""
			cBaseNameSpace := ""
			aNameSpaces := List<STRING>{}
			
			hEnt := oEntityMarkers
			hVis := oEntityVisibility
			
			sWord := StringBuilder{20}
			sFoundType := StringBuilder{20}
			
			state:Reset()
			
			nLine := 0
			_nLength := 0
			FOREACH cLine AS STRING IN aLineCollection
				nLine ++
				oLine := LineObject{nLine, _nLength}
				aLines:Add(oLine)
				_nLength += cLine:Length + 2
				
				// Line parsing
				nChar := 0
				cChar := ' '
				cOldChar := ' '
				cOldOldChar := ' '
				cCharBeforeWord := ' '
				cRealChar := ' '
				nLineLen := cLine:Length
				sWord:Length := 0
				lEscapedWord := false
				
				IF lContinueNextLine
					lContinueNextLine := false
				ELSE
					state:Reset()
					nBracketCount := 0
					lPartial := false
					lStatic := false
					lFindingType := false
					lFindingName := false
					sFoundType:Length := 0
					eAccessLevel := AccessLevel.Public
					eModifiers := EntityModifiers._None
					eStep := ParseStep.None
					aLineFields:Clear()
					aLineLocals:Clear()
					sWord:Length := 0
					cRealChar := ' '
					lEscapedWord := false
					lInAttribute := false
					lAllowAttribute := true
					oInfo := null
					nEntityStartLine := 0
					nEntityStartOffset := 0
					nEntityStartCol := 0
					oStatementLine := oLine
				ENDIF
				IF eLexer != LexerStep.BlockComment
					eLexer := LexerStep.None
				END IF
				
				DO WHILE nChar <= nLineLen // one more than chars in line
					
					// Lexing
					IF sWord:Length == 0
						cCharBeforeWord := cRealChar
					END IF
					cOldOldChar := cOldChar
					cOldChar := cChar
					IF cOldOldChar == '@' .and. cOldChar == '@'
						lEscapedWord := true
					ELSE
						IF sWord:Length == 0
							lEscapedWord := false
						END IF
					END IF
					IF cOldChar == ';' .and. eLexer == LexerStep.None .and. (.not. state:lDirective .or. eStep == ParseStep.AfterUsing)
						lNewCommandInLine := false
						lContinueNextLine := true
						IF nChar < nLineLen
							FOR n := nChar UPTO nLineLen - 1
								cTestChar := cLine[n]
								IF cTestChar != ' ' .and. cTestChar != '\t'
									IF cTestChar != '/'  // non-commented code follows
										lNewCommandInLine := true
										lContinueNextLine := false
									END IF
									EXIT
								END IF
							NEXT
						ENDIF
						IF lNewCommandInLine
							lNewCommandInLine := false
							state:Reset()
							nBracketCount := 0
							lPartial := false
							lStatic := false
							lFindingType := false
							lFindingName := false
							sFoundType:Length := 0
							eAccessLevel := AccessLevel.Public
							eModifiers := EntityModifiers._None
							eLexer := LexerStep.None
							eStep := ParseStep.None
							aLineFields:Clear()
							aLineLocals:Clear()
							sWord:Length := 0
							cRealChar := ' '
							lEscapedWord := false
							lInAttribute := false
							lAllowAttribute := true
							oInfo := null
							nEntityStartLine := 0
							nEntityStartOffset := 0
							nEntityStartCol := 0
							oStatementLine := oLine:AddSubLine(nChar)
							cOldChar := ' '
						END IF
					ENDIF
					
					IF nBracketCount == 0 .and. .not. lFindingType
						IF cOldChar == '{' .or. cOldChar == '['
							
							// indexed PROPERTY
							IF .not. (cOldChar == '[' .and. state:lEntityFound .and. oInfo != null .and. oInfo:eType == EntityType._Property)
								SWITCH cOldChar
									CASE '{'
										cBracketOpen := '{'
										cBracketClose := '}'
									CASE '['
										cBracketOpen := '['
										cBracketClose := ']'
								END SWITCH
								nBracketCount := 1
							END IF
						END IF
					END IF
					
					
					IF nChar == nLineLen
						cChar := ' '
					ELSE
						cChar := cLine[nChar]
					ENDIF
					cRealChar := cChar
					nChar ++
					
					IF state:lFirstChar
						IF cOldChar != ' ' .and. cOldChar != '\t'
							state:lFirstChar := false
						END IF
					END IF
					
					lBeforeLexerChange := false
					lMustLoop := false
					eWordStatus := WordStatus.Text
					eWordSubStatus := WordSubStatus.Text
					eCharStatus := WordStatus.Text
					eCharSubStatus := WordSubStatus.Text
					DO CASE
						CASE state:lDirective .and. eLexer == LexerStep.None .and. .not. (cRealChar == '\'' .or. cRealChar == '"' .or. (cRealChar == '*' .and. cOldChar == '/') .or. (cRealChar == '/' .and. cOldChar == '/') )
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.Text
							IF cChar == '/' .and. nChar < cLine:Length .and. cLine[nChar] == '*'
								eCharStatus := WordStatus.Comment
								eCharSubStatus := WordSubStatus.CommentBlock
							ELSEIF cChar == '/' .and. nChar < cLine:Length .and. cLine[nChar] == '/'
								eCharStatus := WordStatus.Comment
								eCharSubStatus := WordSubStatus.CommentLine
							ELSE
								eCharStatus := WordStatus.Text
								eCharSubStatus := WordSubStatus.Text
							END IF
						CASE eLexer == LexerStep.Comment
							eWordStatus := WordStatus.Comment
							eWordSubStatus := WordSubStatus.CommentLine
							eCharStatus := WordStatus.Comment
							eCharSubStatus := WordSubStatus.CommentLine
						CASE eLexer == LexerStep.BlockComment
							IF cChar == '/' .and. cOldChar == '*'
								eLexer := LexerStep.None
								cChar := ' ' // if next char is '*', we shouldn't go to BlockComment mode again
							ENDIF
							eWordStatus := WordStatus.Comment
							eWordSubStatus := WordSubStatus.CommentBlock
							eCharStatus := WordStatus.Comment
							eCharSubStatus := WordSubStatus.CommentBlock
							lMustLoop := true
						CASE cChar == '"' .and. eLexer != LexerStep.Quote .and. .not. (eLexer == LexerStep.DoubleQuote .and. lEscapedString .and. lEscapedChar)
							IF eLexer == LexerStep.DoubleQuote
								eLexer := LexerStep.None
								eWordStatus := WordStatus.Literal
								eWordSubStatus := WordSubStatus.LiteralString
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralString
								lMustLoop := true
							ELSE
								eLexer := LexerStep.DoubleQuote
								lEscapedChar := false
								lEscapedString := cOldChar == 'e' .and. sWord:Length == 1
								IF .not. lEscapedString
									lEscapedString := (cOldChar == 'e' .or. cOldChar == 'i') .and. sWord:Length == 2 .and. (sWord:ToString() == "ei" .or. sWord:ToString() == "ie")
								END IF
								IF lEscapedString
									eWordStatus := WordStatus.Literal
									eWordSubStatus := WordSubStatus.LiteralString
								ELSE
									eWordStatus := WordStatus.Text
									eWordSubStatus := WordSubStatus.Text
									IF cOldChar == 'i' .and. sWord:Length == 1
										eWordStatus := WordStatus.Literal
										eWordSubStatus := WordSubStatus.LiteralString
									END IF
								END IF
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralString
								lBeforeLexerChange := true
								cChar := ' '
							END IF
						CASE cChar == '\'' .and. eLexer != LexerStep.DoubleQuote .and. .not. (eLexer == LexerStep.Quote .and. cOldChar == '\\' .and. lEscapedChar)
							IF eLexer == LexerStep.Quote
								eLexer := LexerStep.None
								eWordStatus := WordStatus.Literal
								eWordSubStatus := WordSubStatus.LiteralChar
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralChar
								lMustLoop := true
							ELSE
								eLexer := LexerStep.Quote
								eWordStatus := WordStatus.Text
								eWordSubStatus := WordSubStatus.Text
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralChar
								lBeforeLexerChange := true
								cChar := ' '
							END IF
						CASE eLexer == LexerStep.DoubleQuote
							eWordStatus := WordStatus.Literal
							eWordSubStatus := WordSubStatus.LiteralString
							eCharStatus := WordStatus.Literal
							eCharSubStatus := WordSubStatus.LiteralString
							IF lEscapedChar
								lEscapedChar := false
							ELSE
								lEscapedChar := lEscapedString .and. cChar == '\\'
							END IF
							lMustLoop := true
						CASE eLexer == LexerStep.Quote
							eWordStatus := WordStatus.Literal
							eWordSubStatus := WordSubStatus.LiteralChar
							eCharStatus := WordStatus.Literal
							eCharSubStatus := WordSubStatus.LiteralChar
							IF lEscapedChar
								lEscapedChar := false
							ELSE
								lEscapedChar := cChar == '\\'
							END IF
							lMustLoop := true
						CASE (cChar == '/' .and. cOldChar == '/') .or. (state:lFirstChar .and. cChar = '*')
							eLexer := LexerStep.Comment
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.Text
							eCharStatus := WordStatus.Comment
							eCharSubStatus := WordSubStatus.CommentLine
							IF .not. lContinueNextLine
								state:lIgnore := true
							END IF
						CASE cChar == '*' .and. cOldChar == '/'
							eLexer := LexerStep.BlockComment
							cChar := ' ' // if next char is '/', we shouldn't go out of BlockComment mode again
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.Text
							eCharStatus := WordStatus.Comment
							eCharSubStatus := WordSubStatus.CommentBlock
							lMustLoop := true
						CASE cChar == '/'
							IF nChar < nLineLen .and. (cLine[nChar] == '/' .or. cLine[nChar] == '*') // about to go into comment mode
								lBeforeLexerChange := true
								IF cLine[nChar] == '/'
									eCharStatus := WordStatus.Comment
									eCharSubStatus := WordSubStatus.CommentLine
								ELSE
									state:lDirective := false
									eCharStatus := WordStatus.Comment
									eCharSubStatus := WordSubStatus.CommentBlock
								END IF
								eWordStatus := WordStatus.Text
								eWordSubStatus := WordSubStatus.Text
							END IF
					END CASE
					
					IF nBracketCount == 0
						 lInAttribute := lAllowAttribute .and. cRealChar == '[' .and. sWord:Length == 0
					ELSE
						IF cChar == cBracketOpen
							nBracketCount ++
						ELSEIF cChar == cBracketClose
							nBracketCount --
						END IF
					ENDIF
					
					#warning need TO check this
									/*IF lMustLoop
					LOOP
					END IF*/
					
					lIsSpaceChar := cChar == ' ' .or. cChar == '\t'
					IF .not. lIsSpaceChar
						lAllowAttribute := cRealChar == '(' .or. cRealChar == ',' .or. (state:lFirstWord .and. (cRealChar == ']' .or. cRealChar == ';') )
					END IF
					IF .not. lIsSpaceChar .and. nEntityStartLine == 0
						nEntityStartLine := nLine
						nEntityStartOffset := oLine:OffSet
						nEntityStartCol := nChar - 1
					END IF
					IF lIsSpaceChar
						IF sWord:Length == 0
							IF .not. (cRealChar == ' ' .or. cRealChar == '\t') 
								LOOP
							END IF
						END IF
						lIsBreakChar := false
					ELSE
						lIsBreakChar := hBrk:ContainsKey(cChar)
						IF lIsBreakChar .and. state:lEntityFound
							IF oInfo != null .and. oInfo:eType == EntityType._Operator
								IF System.Array.IndexOf(aOperators , cRealChar) != -1
									sWord:Append(cRealChar)
									LOOP
								END IF
							END IF
						END IF
					END IF
					
					IF eLexer == LexerStep.None
						IF cRealChar == '#'
							eLexer := LexerStep.Sharp
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.Text
							eCharStatus := WordStatus.Literal
							eCharSubStatus := WordSubStatus.LiteralSymbol
						END IF
					ELSEIF eLexer == LexerStep.Sharp
						IF lIsBreakChar .or. lIsSpaceChar
							eLexer := LexerStep.None
						ENDIF
						eWordStatus := WordStatus.Literal
						eWordSubStatus := WordSubStatus.LiteralSymbol
						eCharStatus := WordStatus.Text
						eCharSubStatus := WordSubStatus.Text
					ENDIF
					
					IF cChar == '=' .and. cOldChar == ':'
						nAfterColonEquals := nChar
					END IF
					
					IF .not. (lIsBreakChar .or. lIsSpaceChar)
						sWord:Append(cRealChar)
						LOOP
					ENDIF
					// End of lexing
					
					
					// Parsing
					IF sWord:Length == 0
						cWord := ""
						cUpperWord := ""
					ELSE
						cWord := sWord:ToString()
						cUpperWord := cWord:ToUpper()
					END IF
					
					IF eWordSubStatus == WordSubStatus.LiteralSymbol .and. sWord:Length != 0
						IF state:lFirstWord .and. oDirectives:ContainsKey(cUpperWord)
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.TextDirective
							state:lDirective := true
							state:lIgnore := true
							oStatementLine:cArgument :=cWord
							SWITCH cUpperWord
								CASE "REGION"
									_SetLineType(oStatementLine, LineType.RegionIn)
								CASE "ENDREGION"
									_SetLineType(oStatementLine, LineType.RegionOut)
								CASE "IFDEF" 
								CASE "IFNDEF" 
								CASE "ELSE"
									_SetLineType(oStatementLine, LineType.IfdefIn)
								CASE "ENDIF"
									_SetLineType(oStatementLine, LineType.IfdefOut)
								CASE "DEFINE"
									_SetLineType(oStatementLine, LineType.Define)
								CASE "INCLUDE"
									_SetLineType(oStatementLine, LineType.Include)
									eStep := ParseStep.AfterInclude
									oStatementLine:cArgument := ""
								CASE "USING"
									_SetLineType(oStatementLine, LineType.Using)
									eStep := ParseStep.AfterUsing
									oStatementLine:cArgument := ""
								OTHERWISE
									_SetLineType(oStatementLine, LineType.OtherDirective)
							END SWITCH
						ELSE
							eWordStatus := WordStatus.Literal
							eWordSubStatus := WordSubStatus.LiteralSymbol
						ENDIF
					END IF
					
					
					LOCAL lAllowEntityParse AS LOGIC
					lAllowEntityParse := .not. state:lIgnore
					
					DO CASE
						CASE eLexer == LexerStep.BlockComment
							sWord:Length := 0
							lEscapedWord := false
							LOOP
						
						CASE eStep == ParseStep.WaitImplements .and. .not. (cUpperWord == "IMPLEMENTS" .and. .not. lEscapedWord)
							eStep := ParseStep.None
							state:lIgnore := true
						CASE eStep == ParseStep.WaitCommaImplements
							IF cWord:Trim():Length == 0
								IF cRealChar == ','
									eStep := ParseStep.AfterImplements
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								ELSE
									eStep := ParseStep.None
									state:lIgnore := true
								ENDIF
							ELSE
								eStep := ParseStep.None
								state:lIgnore := true
							END IF
						
						CASE state:lLocal .and. state:lImpliedLocal .and. .not. lEscapedWord .and. cUpperWord == "IN"
							nAfterColonEquals := -nChar // FOREACH
						
						CASE state:lLocal .and. state:lImpliedLocal .and. nAfterColonEquals != 0
							// LOCAL IMPLIED type recognition code here
							state:lLocal := false
							state:lIgnore := true
						
						CASE eStep == ParseStep.AfterUsing
							IF eWordStatus == WordStatus.Text .and. .not. eWordSubStatus == WordSubStatus.TextDirective
								oStatementLine:cArgument += cWord
							END IF
							IF .not. lIsSpaceChar .and. cRealChar != ';' .and. eCharSubStatus == WordSubStatus.Text
								oStatementLine:cArgument += cRealChar:ToString()
							ELSEIF cRealChar == '.' .and. eCharSubStatus == WordSubStatus.TextOperator
								oStatementLine:cArgument += cRealChar:ToString()
							END IF
						
						CASE eStep == ParseStep.AfterInclude
							IF eWordSubStatus == WordSubStatus.LiteralString
								oStatementLine:cArgument += cWord
							END IF
							IF eCharSubStatus == WordSubStatus.LiteralString .and. cRealChar != '"'
								oStatementLine:cArgument += cRealChar:ToString()
							END IF
						
						CASE eWordStatus == WordStatus.Literal .or. eWordStatus == WordStatus.Comment .or. eWordSubStatus == WordSubStatus.TextDirective
						
						CASE (lIsSpaceChar .or. cChar == ';') .and. sWord:Length == 0
							lEscapedWord := false
							LOOP
						CASE (state:lFirstChar .and. eWordStatus == WordStatus.Text .and. eCharStatus == WordStatus.Comment) .and. sWord:Length == 0
							lEscapedWord := false
							LOOP
						CASE lInAttribute
							sWord:Length := 0
							lEscapedWord := false
							LOOP
						CASE state:lFirstWord .and. cUpperWord == "RETURN" .and. .not. lEscapedWord
							_SetLineType(oStatementLine, LineType.Return)
							oStatementLine:cArgument := null
							state:lIgnore := true
						CASE lAllowEntityParse .and. cChar == ',' .and. (state:lField .or. state:lLocal) .and. state:lNameFound .and. eStep != ParseStep.AfterAs .and. eStep != ParseStep.AfterRef
							IF nBracketCount == 0
								state:lNameFound := false
								state:lEntityFound := false
							END IF
						CASE lAllowEntityParse .and. .not. state:lFirstWord .and. sWord:Length == 0 .and. .not. state:lInParams .and. .not. lFindingType .and. .not. lFindingName
							IF cChar == '('
								IF state:lNameFound .and. oInfo:aParams == null
									state:lInParams := true
								END IF
							END IF
						CASE lAllowEntityParse .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. ;
							((hVis:ContainsKey(cUpperWord) .and. eStep != ParseStep.AfterBegin) .or. cUpperWord == "EVENT" .or. ;
							((cUpperWord == "LOCAL" .or. cUpperWord == "CATCH" .or. cUpperWord == "FOREACH")) )
							// Checking eStep beause UNSAFE could be UNSAFE FUNCTION or BEGIN UNSAFE
							state:lVisFound := true
							SWITCH cUpperWord
								CASE "PROTECT" 
								CASE "PROTECTED" 
								CASE "INSTANCE"
									eAccessLevel := AccessLevel.Protected
									eModifiers := eModifiers | EntityModifiers._Protected
								CASE "HIDDEN" 
								CASE "PRIVATE"
									eAccessLevel := AccessLevel.Hidden
									eModifiers := eModifiers | EntityModifiers._Private
								CASE "INTERNAL"
									eAccessLevel := AccessLevel.Internal
									eModifiers := eModifiers | EntityModifiers._Internal
								CASE "PUBLIC"
									eAccessLevel := AccessLevel.Public
								CASE "STATIC"
									lStatic := true
									eModifiers := eModifiers | EntityModifiers._Static
								CASE "CONST"
									lStatic := true
								CASE "PARTIAL"
									eModifiers := eModifiers | EntityModifiers._Partial
									lPartial := true
								CASE "VIRTUAL"
									eModifiers := eModifiers | EntityModifiers._Virtual
								CASE "NEW"
									eModifiers := eModifiers | EntityModifiers._New
								CASE "SEALED"
									eModifiers := eModifiers | EntityModifiers._Sealed
								CASE "ABSTRACT"
									eModifiers := eModifiers | EntityModifiers._Abstract
							END SWITCH
							IF System.Array.IndexOf(aEntityWords , cUpperWord) != -1
								// Allow multiple names in same line
								state:lField := true
								IF cUpperWord == "EVENT"
									state:lEvent := true
								END IF
							END IF
							IF cUpperWord == "LOCAL" .or. cUpperWord == "CATCH" .or. cUpperWord == "FOREACH"
								state:lLocal := true
								state:lImpliedLocal := false
							
							    IF cUpperWord == "FOREACH"
    								_SetLineType(oStatementLine, LineType.TokenIn)
                                ELSEIF cUpperWord == "CATCH"
                                    _SetLineType(oStatementLine, LineType.TokenInOut)
							    ENDIF
                            END IF
						CASE lAllowEntityParse .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. (cUpperWord == "VAR" .and. state:lFirstWord)
							state:lVisFound := true
							IF cUpperWord == "VAR"
								state:lLocal := true
								state:lImpliedLocal := true
								nAfterColonEquals := 0
							END IF
						CASE lAllowEntityParse .and. .not. lEscapedWord .and. state:lFirstWord .and. cUpperWord == "USING"
							_SetLineType(oStatementLine, LineType.Using)
							eStep := ParseStep.AfterUsing
							oStatementLine:cArgument := ""
						CASE state:lVisFound .and. lInProperty .and. (cUpperWord == "SET" .or. cUpperWord == "GET") .and. .not. lEscapedWord
							_SetLineType(oStatementLine, LineType.TokenIn)
							oStatementLine:cArgument := cUpperWord
							state:lIgnore := true
						CASE lAllowEntityParse .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. hEnt:ContainsKey(cUpperWord)
							lInEnum := FALSE
							state:lField := false
							state:lLocal := false
							state:lEvent := false
							IF state:lEntityFound
								state:lIgnore := true
							ENDIF
							state:lEntityFound := true
							state:lEntityIsType := System.Array.IndexOf(aTypes , cUpperWord) != -1
							IF eStep == ParseStep.AfterEnd .and. state:lEntityIsType
								_SetLineType(oStatementLine, LineType.EndClass)
								IF aTypeStack:Count > 0
									VAR oPrevInfo := aTypeStack:Pop()
                                    IF ( oPrevInfo:eType:SupportNestedType() )
                                        iClassOrStruct --
                                    ENDIF
								ENDIF
								state:lEntityFound := false
								state:lEntityIsType := false
								state:lIgnore := true
								lInEnum := FALSE
								cShortClassName := ""
								cTypedClassName := ""
								cClassNameSpace := ""
								cClassType := ""
								lInProperty := false
							ELSEIF eStep == ParseStep.AfterEnd .and. cUpperWord == "PROPERTY"
								_SetLineType(oStatementLine, LineType.EndProperty)
								state:lEntityFound := false
								state:lEntityIsType := false
								state:lIgnore := true
								lInEnum := FALSE
								lInProperty := false
							ELSE
								lInEnum := cUpperWord == "ENUM"
								oInfo := EntityObject{GetEntityType(cUpperWord)}
								IF oInfo:eType:IsType()
									IF !oInfo:eType:SupportNestedType() 
										DO WHILE aTypeStack:Count > 0
											aTypeStack:Pop()
										ENDDO
									ENDIF
									aTypeStack:Push(oInfo)
                                    IF ( iClassOrStruct > 0 )
                                        // We are already in Class Or Struct
                                        // So, this is a nested Type
                                        oInfo:oParent := getParentType()
                                    ENDIF
                                    IF oInfo:eType:SupportNestedType()
                                        iClassOrStruct ++
                                    ENDIF
								ELSEIF oInfo:eType:IsClassMember()
								    IF aTypeStack:Count > 0
                                        VAR oParent := getParentType()
                                        IF ( oParent != null )
										    oParent:AddChild(oInfo)
                                        ENDIF
									ENDIF
								ELSEIF oInfo:eType:IsGlobalMember()
									SELF:oGlobalObject:AddChild(oInfo)
								ENDIF
								oInfo:SetNamespaces(aNameSpaces)
								IF (oInfo:eType:HasBody())
									oCurrentMethod := oInfo
								ELSE
									oCurrentMethod := NULL_OBJECT
								ENDIF
								lInProperty := oInfo:eType == EntityType._Property
								IF state:lEntityIsType
									cClassType := cUpperWord
									SWITCH cUpperWord
										CASE "CLASS"
											oInfo:cInherit := ""
											oInfo:cImplements := ""
										CASE "STRUCTURE" 
										CASE "STRUCT"
											oInfo:cInherit := ""
											oInfo:cImplements := ""
										CASE "DELEGATE"
											oInfo:cInherit := ""
											oInfo:cImplements := ""
											oInfo:cRetType := "" // Default
										CASE "ENUM"
											oInfo:cInherit := ""
											oInfo:cImplements := ""
											oInfo:cRetType := ""
											cEnumType := "INT"
										OTHERWISE
											oInfo:cInherit := ""
											oInfo:cImplements := ""
									END SWITCH
								ELSE
									SWITCH oInfo:eType
										CASE EntityType._Method 
										CASE EntityType._Access 
										CASE EntityType._Property 
										CASE EntityType._Function 
										CASE EntityType._Global
											oInfo:cRetType := "USUAL" // Default
										OTHERWISE
											oInfo:cRetType := ""
									END SWITCH
								END IF
								oInfo:lStatic := lStatic
								oInfo:eAccessLevel := eAccessLevel
								oInfo:eModifiers := eModifiers
								IF oInfo:eType == EntityType._Constructor .or. oInfo:eType == EntityType._Destructor
									state:lNameFound := true // Dont't wait for a name, add it to the list now
									oInfo:nStartLine := nEntityStartLine
									oInfo:nOffSet	:= nEntityStartOffSet
									oInfo:nCol		:= nEntityStartCol
									oInfo:cName := IIF(oInfo:eType == EntityType._Constructor , ".ctor" , ".dtor")
									oInfo:cShortClassName := cShortClassName
									oInfo:cTypedClassName := cTypedClassName
									oInfo:cClassNameSpace := cClassNameSpace
									oInfo:cClassType := cClassType
									AddEntity(oInfo, oLine)
									IF cChar == '('
										state:lInParams := true
									END IF
								END IF
							END IF
						CASE lAllowEntityParse .and. (state:lEntityFound .or. eStep == ParseStep.AfterBeginNamespace .or. eStep == ParseStep.WaitImplements)
							DO CASE
								CASE cUpperWord == "INHERIT" .and. .not. lEscapedWord
									eStep := ParseStep.AfterInherit
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								CASE cUpperWord == "IMPLEMENTS" .and. .not. lEscapedWord
									eStep := ParseStep.AfterImplements
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								CASE cUpperWord == "AS" .and. .not. lEscapedWord
									eStep := ParseStep.AfterAs
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								CASE state:lInParams .and. (cUpperWord == "REF" .or. cUpperWord == "OUT" .or. cUpperWord == "PARAMS") .and. .not. lEscapedWord
									eStep := ParseStep.AfterRef
									SWITCH cUpperWord
										CASE "REF"
											nParamType := ParamType.Ref
										CASE "OUT"
											nParamType := ParamType.Out
										CASE "PARAMS"
											nParamType := ParamType.Params
										OTHERWISE
											nParamType := ParamType.As
									END SWITCH

									sFoundType:Length := 0
									n1 := 0;n2 := 0
								CASE eStep == ParseStep.AfterInherit .or. eStep == ParseStep.AfterImplements .or. ;
									eStep == ParseStep.AfterAs .or. eStep == ParseStep.AfterRef .or. ;
									eStep == ParseStep.AfterBeginNamespace .or. ;
									.not. state:lNameFound
									
									IF eStep == ParseStep.AfterInherit .or. eStep == ParseStep.AfterImplements .or. eStep == ParseStep.AfterAs .or. eStep == ParseStep.AfterRef
										// Waiting for type that may be generic, array
										lFindingType := true
										sFoundType:Append(sWord:ToString())
										sWord:Length := 0
										
										DO WHILE nChar < nLineLen .and. (cChar == ' ' .or. cChar == '\t')
											cOldOldChar := cOldChar
											cOldChar := cChar
											cChar := cLine[nChar]
											IF cChar != ' ' .and. cChar != '\t' .and. cChar != '[' .and. cChar != '<'
												cChar := ' '
												EXIT
											END IF
											nChar ++
										END DO
										
										SWITCH cChar
											CASE '.'
												sFoundType:Append('.')
											CASE '['
												sFoundType:Append('[')
												n1 ++
											CASE ']'
												IF n1 > 0
													sFoundType:Append(']')
													n1 --
												END IF
											CASE '<'
												sFoundType:Append('<')
												n2 ++
											CASE '>'
												IF n2 > 0
													sFoundType:Append('>')
													n2 --
												END IF
											CASE ' '
											CASE '\t'
											CASE ';'
												NOP
											OTHERWISE
												IF (n1 != 0 .or. n2 != 0) 
													sFoundType:Append(cChar)
												ENDIF
										END SWITCH
										IF cChar == '.' .or. n1 != 0 .or. n2 != 0 .or. (cChar == ']' .and. nChar < nLineLen .and. cLine[nChar] == '[') // more dims
											LOOP
										END IF
										cWord := sFoundType:ToString()
										sFoundType:Length := 0
										
										lFindingType := false
										
									ELSE // eStep == ParseStep.AfterBeginNamespace .or. .not. state:lNameFound
										
										// Waiting for simple type or method name that may contain dots
										sFoundType:Append(sWord:ToString())
										IF cChar == '.'
											sFoundType:Append('.')
											sWord:Length := 0
											lFindingName := true
											LOOP
										END IF
										cWord := sFoundType:ToString()
										sFoundType:Length := 0
										lFindingName := false
										
									END IF
									
									IF state:lEntityIsType .and. .not. state:lNameFound
										cTypedClassName := cWord
										nAt := cWord:LastIndexOf('.')
										IF nAt <= 0 .or. nAt >= cWord:Length - 1
											cShortClassName := cWord
											cClassNameSpace := ""
											//								cClassType := cWord
										ELSE
											cShortClassName := cWord:Substring(nAt + 1)
											cClassNameSpace := cWord:Substring(0 , nAt)
											//								cClassType := cWord:Substring(nAt + 1)
										END IF
										IF .not. String.IsNullOrEmpty(cBaseNameSpace)
											IF cClassNameSpace == ""
												cClassNameSpace := cBaseNameSpace
											ELSE
												cClassNameSpace := cBaseNameSpace + "." + cClassNameSpace
											END IF
										END IF
									ENDIF
									
									DO CASE
										CASE state:lInParams
											oInfo:SetParamType(cWord, nParamType)
											nParamType := ParamType.AS
											eStep := ParseStep.None
										CASE eStep == ParseStep.AfterInherit
											oInfo:cInherit := cWord
											eStep := ParseStep.WaitImplements
										//							state:lIgnore := TRUE
										CASE eStep == ParseStep.AfterImplements
											IF oInfo:cImplements:Trim():Length == 0
												oInfo:cImplements := cWord
											ELSE
												oInfo:cImplements += ", " + cWord
											END IF
											SWITCH cRealChar
												CASE ','
													eStep := ParseStep.AfterImplements
													sFoundType:Length := 0
													n1 := 0;n2 := 0
												CASE ' ' 
												CASE '\t'
													eStep := ParseStep.WaitCommaImplements
												OTHERWISE
													eStep := ParseStep.None
													state:lIgnore := true
											END SWITCH
										CASE eStep == ParseStep.AfterAs
											LOCAL cRetType AS STRING
											IF state:lDimDeclaration
												cRetType := cWord + "[]"
											ELSE
												cRetType := cWord
											END IF
											oInfo:cRetType := cRetType
											IF lInEnum
												cEnumType := cWord
											ELSEIF state:lField
												FOR n := 0 UPTO aLineFields:Count - 1
													((EntityObject)aLineFields[n]):cRetType := cRetType
												NEXT
											ELSEIF state:lLocal
												FOR n := 0 UPTO aLineLocals:Count - 1
													((EntityObject)aLineLocals[n]):cRetType := cRetType
												NEXT
											END IF
											eStep := ParseStep.None
											state:lIgnore := true
										CASE eStep == ParseStep.AfterBeginNamespace
											eStep := ParseStep.None
											aNameSpaces:Add(cWord)
											cBaseNameSpace := GetNameSpace(aNameSpaces)
											_SetLineType(oStatementLine, LineType.BeginNamespace)
											oStatementLine:cArgument := cWord
											state:lIgnore := true
										CASE .not. state:lNameFound
											state:lNameFound := true
											oInfo:nStartLine := nEntityStartLine
											oInfo:nCol	:= nEntityStartCol
											oInfo:nOffSet := nEntityStartOffSet
											oInfo:cName := cWord
											IF oInfo:IsFuncProcGlobal
												oInfo:cShortClassName := ""
												oInfo:cTypedClassName := ""
												oInfo:cClassNameSpace := ""
												oInfo:cClassType := ""
											ELSE
												oInfo:cShortClassName := cShortClassName
												oInfo:cTypedClassName := cTypedClassName
												oInfo:cClassNameSpace := cClassNameSpace
												oInfo:cClassType := cClassType
											END IF
											IF oInfo:eType == EntityType._Class .and. lPartial
												oInfo:lPartial := true
											END IF
											AddEntity(oInfo, oLine)
											
											lPartial := false
									END CASE
								
							END CASE
							IF state:lInParams
								IF cChar == ','
									IF .not. state:lParam
										VAR oParam := oInfo:AddParam(cWord)
										oParam:nCol := nChar - cWord:Length
										
									END IF
									state:lParam := false
								ELSEIF .not. state:lParam .and. sWord:Length != 0
									IF .not. cWord == "SELF"
										VAR oParam := oInfo:AddParam(cWord)
										oParam:nCol := nChar - cWord:Length
										state:lParam := true
									ELSE
										oCurrentMethod:lExtension := true
									END IF
								END IF
							ENDIF
							
							IF cChar == '('
								state:lInParams := true
							ELSEIF cChar == '[' .and. oInfo != null .and. oInfo:eType == EntityType._Property .and. .not. state:lInParams
								state:lInParams := true
							ELSEIF state:lInParams .and. cChar == ')'
								state:lInParams := false
							ELSEIF state:lInParams .and. cChar == ']' .and. oInfo:eType == EntityType._Property
								state:lInParams := false
							END IF
						//					lIgnore := TRUE
						CASE state:lFirstWord .and. cUpperWord == "BEGIN"
							lInEnum := false
							eStep := ParseStep.AfterBegin
						CASE eStep == ParseStep.AfterBegin
							lInEnum := false
							IF cUpperWord == "NAMESPACE"
								eStep := ParseStep.AfterBeginNamespace
							ELSE
								SWITCH cUpperWord
									CASE "LOCK"
										_SetLineType(oStatementLine, LineType.TokenIn)
										oStatementLine:cArgument := "BEGIN"
									CASE "SEQUENCE"
										_SetLineType(oStatementLine, LineType.TokenIn)
										oStatementLine:cArgument := "BEGIN"
									CASE "SCOPE"
										_SetLineType(oStatementLine, LineType.TokenIn)
										oStatementLine:cArgument := "BEGIN"
									CASE "CHECKED" 
									CASE  "UNCHECKED" 
									CASE "UNSAFE" 
									CASE "USING" 
									CASE "FIXED"
										_SetLineType(oStatementLine, LineType.TokenIn)
										oStatementLine:cArgument := "BEGIN"
								END SWITCH
							END IF
						CASE state:lFirstWord .and. cUpperWord == "END"
							lInEnum := false
							eStep := ParseStep.AfterEnd
							_SetLineType(oStatementLine, LineType.TokenOut)
							oStatementLine:cArgument := cUpperWord
						CASE eStep == ParseStep.AfterEnd
							state:lIgnore := true
							lInEnum := false
							IF System.Array.IndexOf(aEndKeywords , cUpperWord) != -1
								_SetLineType(oStatementLine, LineType.EndClass)
								cShortClassName := ""
								cTypedClassName := ""
								cClassNameSpace := ""
								cClassType := ""
							ELSEIF cUpperWord == "NAMESPACE"
								_SetLineType(oStatementLine, LineType.EndNamespace)
								IF lAllowEntityParse
									IF aNameSpaces:Count != 0
										aNameSpaces:RemoveAt(aNameSpaces:Count - 1)
										cBaseNameSpace := GetNameSpace(aNameSpaces)
									END IF
								END IF
							ENDIF
						
						CASE state:lFirstWord .and. cUpperWord == "USING"
							lInEnum := false
							eStep := ParseStep.AfterUsing
							_SetLineType(oStatementLine, LineType.Using)
						
						CASE lAllowEntityParse .and. ;  // 2nd .not. is for IF(logic) CASE(something) etc syntax (paren after IF/CASE etc)
							.not. (.not. lEscapedWord .and. cRealChar == '(' .and. System.Array.IndexOf(<STRING>{"IF", "ELSEIF", "WHILE", "CASE", "FOR"} , cUpperWord) != -1) .and. ;
							.not. (lIsSpaceChar .or. cRealChar == ';' .or. lBeforeLexerChange .or. lContinueNextLine) .and. .not. (state:lField .or. state:lLocal)
							state:lIgnore := true
						CASE state:lLocal .and. .not. lEscapedWord .and. cUpperWord == "IMPLIED"
							state:lImpliedLocal := true
							nAfterColonEquals := 0
						CASE lAllowEntityParse .and. (state:lField .or. state:lLocal .or. lInEnum) .and. .not. state:lNameFound .and. cWord == "DIM" .and. .not. lEscapedWord
							state:lDimDeclaration := true
						CASE lAllowEntityParse .and. (state:lField .or. state:lLocal .or. lInEnum) .and. .not. state:lNameFound
							state:lNameFound := true
							state:lEntityFound := true
							oInfo := EntityObject{}
							oInfo:SetNamespaces(aNameSpaces)
							oInfo:nCol := nChar - cWord:Length
							IF state:lLocal
								oInfo:eType := EntityType._Local
							ELSEIF state:lEvent
								oInfo:eType := EntityType._Event                            
							ELSE
                                IF lInEnum
								    oInfo:eType := EntityType._EnumMember
                                ELSE
								    oInfo:eType := EntityType._Field
                                ENDIF
								IF aTypeStack:Count > 0
                                    VAR oParent := getParentType()
                                    IF ( oParent != null )
										oParent:AddChild(oInfo)
                                    ENDIF
								ENDIF
							END IF
							oInfo:lStatic := lStatic
							oInfo:eAccessLevel := eAccessLevel
							oInfo:eModifiers := eModifiers
							oInfo:cRetType := "USUAL"
							IF state:lDimDeclaration
								oInfo:cRetType := "USUAL[]"
							END IF
							IF lInEnum
								oInfo:cRetType := cEnumType
							ELSE
								IF state:lLocal
									aLineLocals:Add(oInfo)
								ELSE
									aLineFields:Add(oInfo)
								END IF
								IF cChar == ','
									state:lNameFound := false
									state:lEntityFound := false
								END IF
							ENDIF
							oInfo:nStartLine := nLine
							oInfo:cName		 := cWord
							oInfo:cShortClassName := cShortClassName
							oInfo:cTypedClassName := cTypedClassName
							oInfo:cClassNameSpace := cClassNameSpace
							oInfo:cClassType := cClassType
							IF state:lField
								AddEntity(oInfo, oLine)
							END IF 
							IF state:lLocal .and. lIncludeLocals
								aLocals:Add(oInfo)
							END IF
						CASE state:lFirstWord .or. state:lFirstChar
							IF System.Array.IndexOf(aTokenInOut, cUpperWord) != -1
								_SetLineType(oStatementLine, LineType.TokenInOut)
								oStatementLine:cArgument := cUpperWord
							ELSEIF System.Array.IndexOf(aTokenIn, cUpperWord) != -1
								_SetLineType(oStatementLine, LineType.TokenIn)
								oStatementLine:cArgument := cUpperWord
							ELSEIF System.Array.IndexOf(aTokenOut , cUpperWord) != -1
								_SetLineType(oStatementLine, LineType.TokenOut)
								oStatementLine:cArgument := cUpperWord
							ELSEIF cUpperWord == "GET" .or. cUpperWord == "SET"
								IF lInProperty
									_SetLineType(oStatementLine, LineType.TokenIn)
									oStatementLine:cArgument := cUpperWord
								ENDIF
								ELSE
								IF .not. lContinueNextLine
									IF .not. cUpperWord == "FOR" // Allow LOCAL after FOR
										state:lIgnore := true
									ENDIF
								END IF
							END IF
						
					END CASE
					
					IF cUpperWord == "FROM"
						state:lLinqSelect := true
					ENDIF
					
					
					IF (cChar != '#' .or. sWord:Length != 0) .and. .not. lInAttribute
						state:lFirstWord := false
					END IF
					sWord:Length := 0
					lEscapedWord := false
					// End of parsing
					
				END DO
			NEXT
			// Finish and add types to aTypes
			aResult := List<EntityObject>{}
			aResult:Add(SELF:oGlobalObject)
			FOREACH oEnt AS EntityObject IN aEntities
				IF oEnt:eType:IsType()
					aResult:Add(oEnt)
				ELSEIF oEnt:oParent == null
					aResult:Add(oEnt)
				ENDIF
			NEXT
			RETURN ParseResult{SELF}
		
		
		PROTECTED STATIC METHOD GetNameSpace(aNameSpaces AS List<STRING>) AS STRING
			LOCAL oEnumerator AS IEnumerator
			LOCAL cRet AS STRING
			cRet := ""
			IF aNameSpaces == null
				RETURN cRet
			END IF
			oEnumerator := aNameSpaces:GetEnumerator()
			DO WHILE oEnumerator:MoveNext()
				IF cRet == String.Empty
					cRet := (STRING)oEnumerator:Current
				ELSE
					cRet := (STRING)oEnumerator:Current + "." + cRet
				END IF
			END DO
			RETURN cRet
		
		PROTECTED STATIC METHOD GetEntityType(cWord AS STRING) AS EntityType
			LOCAL eType AS EntityType
			SWITCH cWord
				CASE "METHOD"
					eType := EntityType._Method
				CASE "CONSTRUCTOR"
					eType := EntityType._Constructor
				CASE "CLASS"
					eType := EntityType._Class
				CASE "DESTRUCTOR"
					eType := EntityType._Destructor
				CASE "PROPERTY"
					eType := EntityType._Property
				CASE "ACCESS"
					eType := EntityType._Access
				CASE "ASSIGN"
					eType := EntityType._Assign
				CASE "FUNCTION" 
				CASE "FUNC"
					eType := EntityType._Function
				CASE "PROCEDURE" 
				CASE "PROC"
					eType := EntityType._Procedure
				CASE  "ENUM"
					eType := EntityType._Enum
				CASE "STRUCTURE" 
				CASE "STRUCT"
					eType := EntityType._Structure
				CASE  "VOSTRUCT"
					eType := EntityType._VOStruct
				CASE  "UNION"
					eType := EntityType._Union
				CASE  "GLOBAL"
					eType := EntityType._Global
				CASE  "DELEGATE"
					eType := EntityType._Delegate
				CASE  "EVENT"
					eType := EntityType._Event
				CASE  "INTERFACE"
					eType := EntityType._Interface
				CASE  "OPERATOR"
					eType := EntityType._Operator
				CASE  "DEFINE"
					eType := EntityType._Define
			END SWITCH
			RETURN eType
		
		STATIC METHOD IsType(SELF e AS ENtityType) AS LOGIC
			SWITCH e
				CASE EntityType._Class
				CASE EntityType._Structure
				CASE EntityType._Interface
				CASE EntityType._Delegate
				CASE EntityType._Enum
				CASE EntityType._Union
				CASE EntityType._VOStruct
					RETURN true
			END SWITCH
			RETURN FALSE

		STATIC METHOD SupportNestedType(SELF e AS ENtityType) AS LOGIC
			SWITCH e
				CASE EntityType._Class
				CASE EntityType._Structure
					RETURN true
			END SWITCH
			RETURN FALSE

		STATIC METHOD IsGlobalMember(SELF e AS ENtityType) AS LOGIC
			SWITCH e
				CASE EntityType._Function
				CASE EntityType._Procedure
				CASE EntityType._Define
				CASE EntityType._Global
				CASE EntityType._Resource
				CASE EntityType._Structure
					RETURN true
			END SWITCH
			RETURN FALSE

		STATIC METHOD IsClassMember(SELF e AS ENtityType) AS LOGIC
			SWITCH e
				CASE EntityType._Field
				CASE EntityType._Property
				CASE EntityType._Access
				CASE EntityType._Assign
				CASE EntityType._Method
				CASE EntityType._Constructor
				CASE EntityType._Destructor
				CASE EntityType._Operator
				CASE EntityType._Event
				CASE EntityType._EnumMember
					RETURN true
			END SWITCH
			RETURN false
		STATIC METHOD HasBody(SELF e AS EntityType) AS LOGIC
			SWITCH e
				CASE EntityType._Property
				CASE EntityType._Access
				CASE EntityType._Assign
				CASE EntityType._Method
				CASE EntityType._Constructor
				CASE EntityType._Destructor
				CASE EntityType._Operator
				CASE EntityType._Event
				CASE EntityType._Function
				CASE EntityType._Procedure
					RETURN true
				END SWITCH
				RETURN false

		STATIC METHOD NeedsEndKeyword(SELF e AS EntityType) AS LOGIC
			SWITCH e
				CASE EntityType._Class
				CASE EntityType._Structure
				CASE EntityType._Interface
				CASE EntityType._Enum
					RETURN true
				END SWITCH
				RETURN false
		
	END CLASS
	
	[DebuggerDIsplay("{Line} {eType}")];
		CLASS LineObject
		PROTECT _nLine AS INT
		PROTECT _nOffSet AS INT
		PROTECT _nCol AS INT
		PROTECT _eType AS LineType
		PROTECT _cArgument AS STRING
		
		
		
		INTERNAL CONSTRUCTOR(nLine AS INT)
			SELF(nLine , 0)
			RETURN
		
		INTERNAL CONSTRUCTOR(nLine AS INT, nOffSet AS INT)
			SELF:_nLine := nLine
			SELF:_nOffSet := nOffSet
			RETURN
		
		PRIVATE CONSTRUCTOR(nLine AS INT, nOffSet AS INT, nCol AS INT)
			SELF:_nLine := nLine
			SELF:_nOffSet := nOffSet
			SELF:_nCol  := nCol
			RETURN		
		
		INTERNAL METHOD AddSubLine(nColStart AS INT) AS LineObject
			RETURN LineObject{SELF:_nLine , SELF:_nOffSet+nColStart, nColStart}
		
		PROPERTY Line AS INT GET SELF:_nLine
		PROPERTY OffSet AS INT GET SELF:_nOffSet
		PROPERTY eType AS LineType GET SELF:_eType SET SELF:_eType := VALUE
		
		PROPERTY cArgument AS STRING GET SELF:_cArgument SET SELF:_cArgument := VALUE
		
		
	END CLASS
	
	[DebuggerDIsplay("{eType} {cName,nq}")];
	CLASS EntityObject
		PROPERTY eType AS EntityType AUTO
		PROPERTY cName AS STRING AUTO
		PROPERTY cInherit AS STRING AUTO
		PROPERTY cRetType AS STRING AUTO
		PROPERTY cImplements AS STRING AUTO
		PROPERTY eModifiers AS EntityModifiers AUTO
		PROPERTY eAccessLevel AS AccessLevel AUTO
		PROPERTY cShortClassName AS STRING AUTO
		PROPERTY cTypedClassName AS STRING AUTO
		PROPERTY cClassNamespace AS STRING AUTO
		PROPERTY aParams AS List<EntityParamsObject> AUTO
		PROPERTY nStartLine AS INT  AUTO
		PROPERTY nOffSet AS INT AUTO
		PROPERTY nCol AS INT  AUTO
		PROPERTY aNameSpaces AS List<STRING> AUTO 
		PROPERTY lStatic AS LOGIC AUTO
		PROPERTY lPartial AS LOGIC AUTO
		PROPERTY cClassType AS STRING AUTO
		PROPERTY lExtension AS LOGIC AUTO
		PROPERTY oParent AS EntityObject AUTO
		PROPERTY aChildren AS IList<EntityObject> AUTO
		PROPERTY oNext AS EntityObject AUTO
		PROPERTY oCargo AS OBJECT AUTO
		
		INTERNAL CONSTRUCTOR()
			SUPER()
			SELF:cInherit := ""
			SELF:cImplements := ""
			SELF:cRetType := ""
			SELF:cClassType := ""
			SELF:aChildren := List<EntityObject>{}
			SELF:oParent := null
			RETURN
		INTERNAL CONSTRUCTOR(nType AS EntityType)
			SELF()
			SELF:eType := nType
		
		INTERNAL METHOD AddChild(oChild AS EntityObject) AS VOID
			SELF:aChildren:Add(oChild)
			oChild:oParent := SELF
			RETURN
		
		INTERNAL ACCESS IsFuncProcGlobal AS LOGIC
			RETURN SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure .or. SELF:eType == EntityType._Global
		
		INTERNAL METHOD NamespacesEqual(_aNameSpaces AS List<STRING>) AS LOGIC
			LOCAL n AS INT
			IF SELF:aNameSpaces == null .or. SELF:aNameSpaces:Count != _aNameSpaces:Count
				RETURN false
			END IF
			FOR n := 0 UPTO SELF:aNameSpaces:Count - 1
				IF SELF:aNameSpaces[n] != _aNameSpaces[n]
					RETURN false
				END IF
			NEXT
			RETURN true
		INTERNAL METHOD SetNamespaces(_aNameSpaces AS List<STRING>) AS VOID
			LOCAL n AS INT
			IF SELF:NamespacesEqual(_aNameSpaces)
				RETURN
			END IF 
			IF SELF:aNameSpaces == null
				SELF:aNameSpaces := List<STRING>{_aNameSpaces:Count}
			END IF
			FOR n := 0 UPTO _aNameSpaces:Count - 1
				SELF:aNameSpaces:Add(_aNameSpaces[n])
			NEXT
			RETURN

		INTERNAL METHOD AddParam(cParam AS STRING) AS EntityParamsObject
			RETURN SELF:AddParam(cParam, "")
		
		INTERNAL METHOD AddParam(cParam AS STRING , cType AS STRING) AS EntityParamsObject
			IF SELF:aParams == null
				SELF:aParams := List<EntityParamsObject>{}
			END IF
			LOCAL oParam AS EntityParamsObject
			oParam := EntityParamsObject{cParam , cType}
			SELF:aParams:Add(oParam)
			RETURN oParam
		
		INTERNAL METHOD SetParamType(cType AS STRING, nParamType AS ParamType) AS VOID
			LOCAL oParam AS EntityParamsObject
			IF SELF:aParams == null .or. SELF:aParams:Count == 0 .or. String.IsNullOrEmpty(cType)
				RETURN
			END IF
			oParam := SELF:aParams[SELF:aParams:Count - 1]
			oParam:cType	  := cType
			oParam:nParamType := nParamType
			RETURN
	END CLASS
	
	[DebuggerDIsplay("{cName,nq} {cType,nq}")];
		CLASS EntityParamsObject
		PROPERTY cName AS STRING AUTO
		PROPERTY cType AS STRING AUTO
		PROPERTY nParamType AS ParamType AUTO
		PROPERTY nCol AS INT AUTO
		INTERNAL CONSTRUCTOR(_cName AS STRING , _cType AS STRING)
			SUPER()
			SELF:cName := _cName
			SELF:cType := _cType
			RETURN
	END CLASS
	
	INTERNAL ENUM LexerStep
		MEMBER None
		MEMBER Quote
		MEMBER DoubleQuote
		MEMBER BracketQuote
		MEMBER Comment
		MEMBER BlockComment
		MEMBER Sharp
	END ENUM
	
	INTERNAL ENUM ParseStep
		MEMBER None
		MEMBER AfterAs
		MEMBER AfterRef
		MEMBER AfterInherit
		MEMBER AfterEnd
		MEMBER AfterBegin
		MEMBER AfterBeginNamespace
		MEMBER AfterSharp
		MEMBER AfterDefine
		MEMBER AfterUsing
		MEMBER AfterInclude
		MEMBER AfterImplements
		MEMBER WaitImplements
		MEMBER WaitCommaImplements
		
		// for VO only:
		//	MEMBER WaitClassClause
		MEMBER AfterClassClause
		MEMBER AfterExportClause
	END ENUM
	
	[Flags];
	ENUM EntityModifiers AS Int32
		// roslyn values in the comments, should we keep in sync ?
		MEMBER _None := 0			// 0x0000
		MEMBER _Protected := 1		// 0x0004
		MEMBER _Private := 2		// 0x0008
		MEMBER _Internal := 4		// 0x0002
		MEMBER _Virtual := 8		// 0x0080
		MEMBER _Abstract := 16		// 0x0020
		MEMBER _Sealed := 32		// 0x0010
		MEMBER _Static := 64		// 0x0040
		MEMBER _Partial := 128		// 0x4000
		MEMBER _New := 256			// 0x0200
		// roslyn also has these. Should we add these ?
		// public			0x0001
		// extern			0x0100
		// override			0x0400
		// readonly / initonly 0x0800
		// volatile			0x1000
		// unsafe			0x2000
		// async			0x8000
	END ENUM
	
	ENUM EntityType AS Int32 
		MEMBER _None
		MEMBER _Constructor
		MEMBER _Destructor	
		MEMBER _Method		
		MEMBER _Access		
		MEMBER _Assign		
		MEMBER _Class		
		MEMBER _Function	
		MEMBER _Procedure	
		MEMBER _Enum		
		MEMBER _VOStruct	
		MEMBER _Global		
		MEMBER _Structure	
		MEMBER _Interface	
		MEMBER _Delegate	
		MEMBER _Event		
		MEMBER @@_Field		
		MEMBER _Union		
		MEMBER _Operator	
		MEMBER _Local		
		MEMBER _Property	
		MEMBER _Define		
		MEMBER _Resource	
		MEMBER _TextBlock	
        MEMBER _EnumMember
	END ENUM
	
	
	[Flags];
	ENUM AccessLevel
		MEMBER @@Public := 0
		MEMBER @@Protected := 1
		MEMBER @@Hidden := 2
		MEMBER @@Internal := 4
	END ENUM
	
	ENUM WordStatus
		MEMBER Text
		MEMBER Literal
		MEMBER Comment
	END ENUM
	
	ENUM WordSubStatus
		MEMBER Text
		MEMBER TextReserved
		MEMBER TextFunction
		MEMBER TextDirective
		MEMBER TextOperator
		MEMBER LiteralInt
		MEMBER LiteralUInt
		MEMBER LiteralFloat
		MEMBER LiteralDecimal
		MEMBER LiteralSingle
		MEMBER LiteralDouble
		MEMBER LiteralSymbol
		MEMBER LiteralString
		MEMBER LiteralChar
		MEMBER CommentBlock
		MEMBER CommentLine
		MEMBER CommentRegion
	END ENUM
	
	[Flags];
		ENUM WordStyle AS INT
		MEMBER None := 0
		MEMBER EscapedLiteral := 1
		MEMBER InAttribute := 2
	END ENUM
	
	ENUM LineType AS INT
		//ENUM LineType AS BYTE
		MEMBER None
		MEMBER RegionIn
		MEMBER RegionOut
		MEMBER TokenIn
		MEMBER TokenInOut
		MEMBER TokenOut
		MEMBER BeginNamespace
		MEMBER EndNamespace
		MEMBER EndClass
		MEMBER @@Using
		MEMBER Include
		MEMBER @@Define
		MEMBER @@Return
		MEMBER IfdefIn
		MEMBER IfdefOut
		MEMBER EndProperty
		//	MEMBER BeginProperty
		MEMBER OtherDirective
	END ENUM
	
	INTERNAL STRUCTURE ParseState				   
		INTERNAL PROPERTY  lVisFound AS LOGIC			   AUTO
		INTERNAL PROPERTY  lEntityFound AS LOGIC			   AUTO
		INTERNAL PROPERTY  lEntityIsType AS LOGIC		   AUTO
		INTERNAL PROPERTY  lFirstChar AS LOGIC			   AUTO
		INTERNAL PROPERTY  lFirstWord AS LOGIC			   AUTO
		INTERNAL PROPERTY  lInParams AS LOGIC			   AUTO
		INTERNAL PROPERTY  lNameFound AS LOGIC			   AUTO
		INTERNAL PROPERTY  lField AS LOGIC				   AUTO
		INTERNAL PROPERTY  lLocal AS LOGIC				   AUTO
		INTERNAL PROPERTY  lImpliedLocal AS LOGIC		   AUTO
		INTERNAL PROPERTY  lEvent AS LOGIC				   AUTO
		INTERNAL PROPERTY  lParam AS LOGIC				   AUTO
		INTERNAL PROPERTY  lDirective AS LOGIC			   AUTO
		INTERNAL PROPERTY  lIgnore AS LOGIC				   AUTO
		INTERNAL PROPERTY  lExpectName AS LOGIC			   AUTO
		INTERNAL PROPERTY  lLinqSelect AS LOGIC			   AUTO
		INTERNAL PROPERTY  lDimDeclaration AS LOGIC		   AUTO
		METHOD Reset() AS VOID
			lVisFound := false
			lEntityFound := false
			lEntityIsType := false
			lFirstChar := true
			lFirstWord := true
			lInParams := false
			lNameFound := false
			lField := false
			lLocal := false
			lEvent := false
			lParam := false
			lDirective := false
			lIgnore := false
			lExpectName := false
			lLinqSelect := false
			lDimDeclaration := false
			RETURN
	END STRUCTURE



	
END NAMESPACE


