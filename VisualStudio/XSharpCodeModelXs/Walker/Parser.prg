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
			SELF:oGlobalObject:lStatic		:= TRUE
			SELF:oGlobalObject:lPartial		:= TRUE
			SELF:oGlobalObject:nStartLine	:= 1
			SELF:oGlobalObject:nCol			:= 1
			SELF:iClassOrStruct             := 0

			SELF:StartPosition				:= 0


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
		PROPERTY StartPosition  AS INT					AUTO

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
			LOCAL context AS ParseContext

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
			context := ParseContext{}

			nLine := 0
			_nLength := 0
			FOREACH cLine AS STRING IN aLineCollection
				nLine ++
				oLine := LineObject{nLine, _nLength}
				aLines:Add(oLine)
				_nLength += cLine:Length + 2

				// Before starting the new line/Entity, let's check for the previous one
				IF ( oInfo != NULL ) .AND. state:lImpliedLocal .AND. ( oInfo:eType == EntityType._Local )
					BuildEntityContext( oInfo, context, cWord)
					// Create a new context
					context := ParseContext{}
				ENDIF

				// Line parsing
				nChar := 0
				cChar := ' '
				cOldChar := ' '
				cOldOldChar := ' '
				cCharBeforeWord := ' '
				cRealChar := ' '
				nLineLen := cLine:Length
				sWord:Length := 0
				lEscapedWord := FALSE

				IF lContinueNextLine
					lContinueNextLine := FALSE
				ELSE
					state:Reset()
					nBracketCount := 0
					lPartial := FALSE
					lStatic := FALSE
					lFindingType := FALSE
					lFindingName := FALSE
					sFoundType:Length := 0
					eAccessLevel := AccessLevel.Public
					eModifiers := EntityModifiers._None
					eStep := ParseStep.None
					aLineFields:Clear()
					aLineLocals:Clear()
					sWord:Length := 0
					cRealChar := ' '
					lEscapedWord := FALSE
					lInAttribute := FALSE
					lAllowAttribute := TRUE
					oInfo := NULL
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
					IF cOldOldChar == '@' .AND. cOldChar == '@'
						lEscapedWord := TRUE
					ELSE
						IF sWord:Length == 0
							lEscapedWord := FALSE
						END IF
					END IF
					IF cOldChar == ';' .AND. eLexer == LexerStep.None .AND. (.NOT. state:lDirective .OR. eStep == ParseStep.AfterUsing)
						lNewCommandInLine := FALSE
						lContinueNextLine := TRUE
						IF nChar < nLineLen
							FOR n := nChar UPTO nLineLen - 1
								cTestChar := cLine[n]
								IF cTestChar != ' ' .AND. cTestChar != '\t'
									IF cTestChar != '/'  // non-commented code follows
										lNewCommandInLine := TRUE
										lContinueNextLine := FALSE
									END IF
									EXIT
								END IF
							NEXT
						ENDIF
						IF lNewCommandInLine
							lNewCommandInLine := FALSE
							state:Reset()
							nBracketCount := 0
							lPartial := FALSE
							lStatic := FALSE
							lFindingType := FALSE
							lFindingName := FALSE
							sFoundType:Length := 0
							eAccessLevel := AccessLevel.Public
							eModifiers := EntityModifiers._None
							eLexer := LexerStep.None
							eStep := ParseStep.None
							aLineFields:Clear()
							aLineLocals:Clear()
							sWord:Length := 0
							cRealChar := ' '
							lEscapedWord := FALSE
							lInAttribute := FALSE
							lAllowAttribute := TRUE
							oInfo := NULL
							nEntityStartLine := 0
							nEntityStartOffset := 0
							nEntityStartCol := 0
							oStatementLine := oLine:AddSubLine(nChar)
							cOldChar := ' '
						END IF
					ENDIF

					IF nBracketCount == 0 .AND. .NOT. lFindingType
						IF cOldChar == '{' .OR. cOldChar == '['

							// indexed PROPERTY
							IF .NOT. (cOldChar == '[' .AND. state:lEntityFound .AND. oInfo != NULL .AND. oInfo:eType == EntityType._Property)
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
						IF cOldChar != ' ' .AND. cOldChar != '\t'
							state:lFirstChar := FALSE
						END IF
					END IF

					lBeforeLexerChange := FALSE
					lMustLoop := FALSE
					eWordStatus := WordStatus.Text
					eWordSubStatus := WordSubStatus.Text
					eCharStatus := WordStatus.Text
					eCharSubStatus := WordSubStatus.Text
					DO CASE
						CASE state:lDirective .AND. eLexer == LexerStep.None .AND. .NOT. (cRealChar == '\'' .OR. cRealChar == '"' .OR. (cRealChar == '*' .AND. cOldChar == '/') .OR. (cRealChar == '/' .AND. cOldChar == '/') )
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.Text
							IF cChar == '/' .AND. nChar < cLine:Length .AND. cLine[nChar] == '*'
								eCharStatus := WordStatus.Comment
								eCharSubStatus := WordSubStatus.CommentBlock
							ELSEIF cChar == '/' .AND. nChar < cLine:Length .AND. cLine[nChar] == '/'
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
							IF cChar == '/' .AND. cOldChar == '*'
								eLexer := LexerStep.None
								cChar := ' ' // if next char is '*', we shouldn't go to BlockComment mode again
							ENDIF
							eWordStatus := WordStatus.Comment
							eWordSubStatus := WordSubStatus.CommentBlock
							eCharStatus := WordStatus.Comment
							eCharSubStatus := WordSubStatus.CommentBlock
							lMustLoop := TRUE
						CASE cChar == '"' .AND. eLexer != LexerStep.Quote .AND. .NOT. (eLexer == LexerStep.DoubleQuote .AND. lEscapedString .AND. lEscapedChar)
							IF eLexer == LexerStep.DoubleQuote
								eLexer := LexerStep.None
								eWordStatus := WordStatus.Literal
								eWordSubStatus := WordSubStatus.LiteralString
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralString
								lMustLoop := TRUE
							ELSE
								eLexer := LexerStep.DoubleQuote
								lEscapedChar := FALSE
								lEscapedString := cOldChar == 'e' .AND. sWord:Length == 1
								IF .NOT. lEscapedString
									lEscapedString := (cOldChar == 'e' .OR. cOldChar == 'i') .AND. sWord:Length == 2 .AND. (sWord:ToString() == "ei" .OR. sWord:ToString() == "ie")
								END IF
								IF lEscapedString
									eWordStatus := WordStatus.Literal
									eWordSubStatus := WordSubStatus.LiteralString
								ELSE
									eWordStatus := WordStatus.Text
									eWordSubStatus := WordSubStatus.Text
									IF cOldChar == 'i' .AND. sWord:Length == 1
										eWordStatus := WordStatus.Literal
										eWordSubStatus := WordSubStatus.LiteralString
									END IF
								END IF
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralString
								lBeforeLexerChange := TRUE
								cChar := ' '
							END IF
						CASE cChar == '\'' .AND. eLexer != LexerStep.DoubleQuote .AND. .NOT. (eLexer == LexerStep.Quote .AND. cOldChar == '\\' .AND. lEscapedChar)
							IF eLexer == LexerStep.Quote
								eLexer := LexerStep.None
								eWordStatus := WordStatus.Literal
								eWordSubStatus := WordSubStatus.LiteralChar
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralChar
								lMustLoop := TRUE
							ELSE
								eLexer := LexerStep.Quote
								eWordStatus := WordStatus.Text
								eWordSubStatus := WordSubStatus.Text
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralChar
								lBeforeLexerChange := TRUE
								cChar := ' '
							END IF
						CASE eLexer == LexerStep.DoubleQuote
							eWordStatus := WordStatus.Literal
							eWordSubStatus := WordSubStatus.LiteralString
							eCharStatus := WordStatus.Literal
							eCharSubStatus := WordSubStatus.LiteralString
							IF lEscapedChar
								lEscapedChar := FALSE
							ELSE
								lEscapedChar := lEscapedString .AND. cChar == '\\'
							END IF
							lMustLoop := TRUE
						CASE eLexer == LexerStep.Quote
							eWordStatus := WordStatus.Literal
							eWordSubStatus := WordSubStatus.LiteralChar
							eCharStatus := WordStatus.Literal
							eCharSubStatus := WordSubStatus.LiteralChar
							IF lEscapedChar
								lEscapedChar := FALSE
							ELSE
								lEscapedChar := cChar == '\\'
							END IF
							lMustLoop := TRUE
						CASE (cChar == '/' .AND. cOldChar == '/') .OR. (state:lFirstChar .AND. cChar = '*')
							eLexer := LexerStep.Comment
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.Text
							eCharStatus := WordStatus.Comment
							eCharSubStatus := WordSubStatus.CommentLine
							IF .NOT. lContinueNextLine
								state:lIgnore := TRUE
							END IF
						CASE cChar == '*' .AND. cOldChar == '/'
							eLexer := LexerStep.BlockComment
							cChar := ' ' // if next char is '/', we shouldn't go out of BlockComment mode again
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.Text
							eCharStatus := WordStatus.Comment
							eCharSubStatus := WordSubStatus.CommentBlock
							lMustLoop := TRUE
						CASE cChar == '/'
							IF nChar < nLineLen .AND. (cLine[nChar] == '/' .OR. cLine[nChar] == '*') // about to go into comment mode
								lBeforeLexerChange := TRUE
								IF cLine[nChar] == '/'
									eCharStatus := WordStatus.Comment
									eCharSubStatus := WordSubStatus.CommentLine
								ELSE
									state:lDirective := FALSE
									eCharStatus := WordStatus.Comment
									eCharSubStatus := WordSubStatus.CommentBlock
								END IF
								eWordStatus := WordStatus.Text
								eWordSubStatus := WordSubStatus.Text
							END IF
					END CASE

					IF nBracketCount == 0
						lInAttribute := lAllowAttribute .AND. cRealChar == '[' .AND. sWord:Length == 0
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

					lIsSpaceChar := cChar == ' ' .OR. cChar == '\t'
					IF .NOT. lIsSpaceChar
						lAllowAttribute := cRealChar == '(' .OR. cRealChar == ',' .OR. (state:lFirstWord .AND. (cRealChar == ']' .OR. cRealChar == ';') )
					END IF
					IF .NOT. lIsSpaceChar .AND. nEntityStartLine == 0
						nEntityStartLine := nLine
						nEntityStartOffset := oLine:OffSet
						nEntityStartCol := nChar - 1
					END IF
					IF lIsSpaceChar
						IF sWord:Length == 0
							IF .NOT. (cRealChar == ' ' .OR. cRealChar == '\t')
								LOOP
							END IF
						END IF
						lIsBreakChar := FALSE
					ELSE
						//
						IF eCharStatus != WordStatus.Comment
							context:LastMeaningPos := oLine:OffSet + nChar
							context:Status := eWordStatus
							context:SubStatus := eWordSubStatus
						ENDIF
						//
						lIsBreakChar := hBrk:ContainsKey(cChar)
						IF lIsBreakChar .AND. state:lEntityFound
							IF oInfo != NULL .AND. oInfo:eType == EntityType._Operator
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
						IF lIsBreakChar .OR. lIsSpaceChar
							eLexer := LexerStep.None
						ENDIF
						eWordStatus := WordStatus.Literal
						eWordSubStatus := WordSubStatus.LiteralSymbol
						eCharStatus := WordStatus.Text
						eCharSubStatus := WordSubStatus.Text
					ENDIF

					IF cChar == '=' .AND. cOldChar == ':'
						nAfterColonEquals := nChar
					END IF

					IF .NOT. (lIsBreakChar .OR. lIsSpaceChar)
						sWord:Append(cRealChar)
						LOOP
					ENDIF
					IF cChar == '.' .AND. Char.IsDigit(cOldChar)
						sWord:Append(cRealChar)
						LOOP
					END IF
					// End of lexing


					// Parsing
					IF sWord:Length == 0
						cWord := ""
						cUpperWord := ""
					ELSE
						cWord := sWord:ToString()
						cUpperWord := cWord:ToUpper()
					END IF

					IF eWordSubStatus == WordSubStatus.LiteralSymbol .AND. sWord:Length != 0
						IF state:lFirstWord .AND. oDirectives:ContainsKey(cUpperWord)
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.TextDirective
							state:lDirective := TRUE
							state:lIgnore := TRUE
							oStatementLine:cArgument :=cWord
							SWITCH cUpperWord
								CASE "REGION"
									_SetLineType(oStatementLine, LineType.RegionIn)
								CASE "ENDREGION"
									_SetLineType(oStatementLine, LineType.RegionOut)
								CASE "IFDEF"
								CASE "IFNDEF"
									_SetLineType(oStatementLine, LineType.TokenIn)
								CASE "ELSE"
									_SetLineType(oStatementLine, LineType.TokenInOut)
								CASE "ENDIF"
									_SetLineType(oStatementLine, LineType.TokenOut)
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
					lAllowEntityParse := .NOT. state:lIgnore

					DO CASE
						CASE eLexer == LexerStep.BlockComment
							sWord:Length := 0
							lEscapedWord := FALSE
							LOOP

						CASE eStep == ParseStep.WaitImplements .AND. .NOT. (cUpperWord == "IMPLEMENTS" .AND. .NOT. lEscapedWord)
							eStep := ParseStep.None
							state:lIgnore := TRUE
						CASE eStep == ParseStep.WaitCommaImplements
							IF cWord:Trim():Length == 0
								IF cRealChar == ','
									eStep := ParseStep.AfterImplements
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								ELSE
									eStep := ParseStep.None
									state:lIgnore := TRUE
								ENDIF
							ELSE
								eStep := ParseStep.None
								state:lIgnore := TRUE
							END IF

						CASE state:lLocal .AND. state:lImpliedLocal .AND. .NOT. lEscapedWord .AND. cUpperWord == "IN"
							nAfterColonEquals := -nChar // FOREACH
							context:AfterIn := TRUE

						CASE state:lLocal .AND. state:lImpliedLocal .AND. nAfterColonEquals != 0
							// LOCAL IMPLIED type recognition code here
							state:lLocal := FALSE
							state:lIgnore := TRUE

						CASE eStep == ParseStep.AfterUsing
							IF eWordStatus == WordStatus.Text .AND. .NOT. eWordSubStatus == WordSubStatus.TextDirective
								oStatementLine:cArgument += cWord
							END IF
							IF .NOT. lIsSpaceChar .AND. cRealChar != ';' .AND. eCharSubStatus == WordSubStatus.Text
								oStatementLine:cArgument += cRealChar:ToString()
							ELSEIF cRealChar == '.' .AND. eCharSubStatus == WordSubStatus.TextOperator
								oStatementLine:cArgument += cRealChar:ToString()
							END IF

						CASE eStep == ParseStep.AfterInclude
							IF eWordSubStatus == WordSubStatus.LiteralString
								oStatementLine:cArgument += cWord
							END IF
							IF eCharSubStatus == WordSubStatus.LiteralString .AND. cRealChar != '"'
								oStatementLine:cArgument += cRealChar:ToString()
							END IF

						CASE eWordStatus == WordStatus.Literal .OR. eWordStatus == WordStatus.Comment .OR. eWordSubStatus == WordSubStatus.TextDirective

						CASE (lIsSpaceChar .OR. cChar == ';') .AND. sWord:Length == 0
							lEscapedWord := FALSE
							LOOP
						CASE (state:lFirstChar .AND. eWordStatus == WordStatus.Text .AND. eCharStatus == WordStatus.Comment) .AND. sWord:Length == 0
							lEscapedWord := FALSE
							LOOP
						CASE lInAttribute
							sWord:Length := 0
							lEscapedWord := FALSE
							LOOP
						CASE state:lFirstWord .AND. cUpperWord == "RETURN" .AND. .NOT. lEscapedWord
							_SetLineType(oStatementLine, LineType.Return)
							oStatementLine:cArgument := NULL
							state:lIgnore := TRUE
						CASE lAllowEntityParse .AND. cChar == ',' .AND. (state:lField .OR. state:lLocal) .AND. state:lNameFound .AND. .NOT. state:lInParams .AND. eStep != ParseStep.AfterAs .AND. eStep != ParseStep.AfterRef
							IF nBracketCount == 0
								state:lNameFound := FALSE
								state:lEntityFound := FALSE
							END IF
						CASE lAllowEntityParse .AND. .NOT. state:lFirstWord .AND. sWord:Length == 0 .AND. .NOT. state:lInParams .AND. .NOT. lFindingType .AND. .NOT. lFindingName
							IF cChar == '('
								IF state:lNameFound .AND. oInfo:aParams == NULL
									state:lInParams := TRUE
								END IF
							END IF
						CASE lAllowEntityParse .AND. .NOT. lEscapedWord .AND. cChar != '.' .AND. cCharBeforeWord != '.' .AND. ;
						((hVis:ContainsKey(cUpperWord) .AND. eStep != ParseStep.AfterBegin) .OR. cUpperWord == "EVENT" .OR. ;
						((cUpperWord == "LOCAL" .OR. cUpperWord == "CATCH" .OR. cUpperWord == "FOREACH")) )
							// Checking eStep beause UNSAFE could be UNSAFE FUNCTION or BEGIN UNSAFE
							state:lVisFound := TRUE
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
									lStatic := TRUE
									eModifiers := eModifiers | EntityModifiers._Static
								CASE "CONST"
									lStatic := TRUE
								CASE "PARTIAL"
									eModifiers := eModifiers | EntityModifiers._Partial
									lPartial := TRUE
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
								state:lField := TRUE
								IF cUpperWord == "EVENT"
									state:lEvent := TRUE
								END IF
							END IF
							IF cUpperWord == "LOCAL" .OR. cUpperWord == "CATCH" .OR. cUpperWord == "FOREACH"
								state:lLocal := TRUE
								state:lImpliedLocal := FALSE

								IF cUpperWord == "FOREACH"
									_SetLineType(oStatementLine, LineType.TokenIn)
								ELSEIF cUpperWord == "CATCH"
									_SetLineType(oStatementLine, LineType.TokenInOut)
								ENDIF
							END IF
						CASE lAllowEntityParse .AND. .NOT. lEscapedWord .AND. cChar != '.' .AND. cCharBeforeWord != '.' .AND. (cUpperWord == "VAR" .AND. state:lFirstWord)
							state:lVisFound := TRUE
							IF cUpperWord == "VAR"
								state:lLocal := TRUE
								state:lImpliedLocal := TRUE
								nAfterColonEquals := 0
							END IF
						CASE lAllowEntityParse .AND. .NOT. lEscapedWord .AND. state:lFirstWord .AND. cUpperWord == "USING"
							_SetLineType(oStatementLine, LineType.Using)
							eStep := ParseStep.AfterUsing
							oStatementLine:cArgument := ""
						CASE state:lVisFound .AND. lInProperty .AND. (cUpperWord == "SET" .OR. cUpperWord == "GET") .AND. .NOT. lEscapedWord
							_SetLineType(oStatementLine, LineType.TokenIn)
							oStatementLine:cArgument := cUpperWord
							state:lIgnore := TRUE
						CASE lAllowEntityParse .AND. .NOT. lEscapedWord .AND. cChar != '.' .AND. cCharBeforeWord != '.' .AND. hEnt:ContainsKey(cUpperWord)
							lInEnum := FALSE
							state:lField := FALSE
							state:lLocal := FALSE
							state:lEvent := FALSE
							IF state:lEntityFound
								state:lIgnore := TRUE
							ENDIF
							state:lEntityFound := TRUE
							state:lEntityIsType := System.Array.IndexOf(aTypes , cUpperWord) != -1
							IF eStep == ParseStep.AfterEnd .AND. state:lEntityIsType
								_SetLineType(oStatementLine, LineType.EndClass)
								IF aTypeStack:Count > 0
									VAR oPrevInfo := aTypeStack:Pop()
									IF ( oPrevInfo:eType:SupportNestedType() )
										iClassOrStruct --
									ENDIF
								ENDIF
								state:lEntityFound := FALSE
								state:lEntityIsType := FALSE
								state:lIgnore := TRUE
								lInEnum := FALSE
								cShortClassName := ""
								cTypedClassName := ""
								cClassNameSpace := ""
								cClassType := ""
								lInProperty := FALSE
							ELSEIF eStep == ParseStep.AfterEnd .AND. cUpperWord == "PROPERTY"
								_SetLineType(oStatementLine, LineType.EndProperty)
								state:lEntityFound := FALSE
								state:lEntityIsType := FALSE
								state:lIgnore := TRUE
								lInEnum := FALSE
								lInProperty := FALSE
							ELSE
								lInEnum := cUpperWord == "ENUM"
								oInfo := EntityObject{GetEntityType(cUpperWord)}
								oInfo:nPosition := nEntityStartOffset + SELF:StartPosition + nEntityStartCol
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
										IF ( oParent != NULL )
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
								IF oInfo:eType == EntityType._Constructor .OR. oInfo:eType == EntityType._Destructor
									state:lNameFound := TRUE // Dont't wait for a name, add it to the list now
									oInfo:nStartLine := nEntityStartLine
									oInfo:nCol		:= nEntityStartCol
									oInfo:nOffSet	:= nEntityStartOffSet + oInfo:nCol
									oInfo:cName := IIF(oInfo:eType == EntityType._Constructor , ".ctor" , ".dtor")
									oInfo:cShortClassName := cShortClassName
									oInfo:cTypedClassName := cTypedClassName
									oInfo:cClassNameSpace := cClassNameSpace
									oInfo:cClassType := cClassType
									AddEntity(oInfo, oLine)
									IF cChar == '('
										state:lInParams := TRUE
									END IF
								END IF
							END IF
						CASE lAllowEntityParse .AND. (state:lEntityFound .OR. eStep == ParseStep.AfterBeginNamespace .OR. eStep == ParseStep.WaitImplements)
							DO CASE
								CASE cUpperWord == "INHERIT" .AND. .NOT. lEscapedWord
									eStep := ParseStep.AfterInherit
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								CASE cUpperWord == "IMPLEMENTS" .AND. .NOT. lEscapedWord
									eStep := ParseStep.AfterImplements
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								CASE cUpperWord == "AS" .AND. .NOT. lEscapedWord
									eStep := ParseStep.AfterAs
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								CASE state:lInParams .AND. (cUpperWord == "REF" .OR. cUpperWord == "OUT" .OR. cUpperWord == "PARAMS") .AND. .NOT. lEscapedWord
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
								CASE eStep == ParseStep.AfterInherit .OR. eStep == ParseStep.AfterImplements .OR. ;
								eStep == ParseStep.AfterAs .OR. eStep == ParseStep.AfterRef .OR. ;
								eStep == ParseStep.AfterBeginNamespace .OR. ;
								.NOT. state:lNameFound

									IF eStep == ParseStep.AfterInherit .OR. eStep == ParseStep.AfterImplements .OR. eStep == ParseStep.AfterAs .OR. eStep == ParseStep.AfterRef
										// Waiting for type that may be generic, array
										lFindingType := TRUE
										sFoundType:Append(sWord:ToString())
										sWord:Length := 0

										DO WHILE nChar < nLineLen .AND. (cChar == ' ' .OR. cChar == '\t')
											cOldOldChar := cOldChar
											cOldChar := cChar
											cChar := cLine[nChar]
											IF cChar != ' ' .AND. cChar != '\t' .AND. cChar != '[' .AND. cChar != '<'
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
												IF (n1 != 0 .OR. n2 != 0)
													sFoundType:Append(cChar)
												ENDIF
										END SWITCH
										IF cChar == '.' .OR. n1 != 0 .OR. n2 != 0 .OR. (cChar == ']' .AND. nChar < nLineLen .AND. cLine[nChar] == '[') // more dims
											LOOP
										END IF
										cWord := sFoundType:ToString()
										sFoundType:Length := 0

										lFindingType := FALSE

									ELSE // eStep == ParseStep.AfterBeginNamespace .or. .not. state:lNameFound

										// Waiting for simple type or method name that may contain dots
										sFoundType:Append(sWord:ToString())
										IF cChar == '.'
											sFoundType:Append('.')
											sWord:Length := 0
											lFindingName := TRUE
											LOOP
										END IF
										cWord := sFoundType:ToString()
										sFoundType:Length := 0
										lFindingName := FALSE

									END IF

									IF state:lEntityIsType .AND. .NOT. state:lNameFound
										cTypedClassName := cWord
										nAt := cWord:LastIndexOf('.')
										IF nAt <= 0 .OR. nAt >= cWord:Length - 1
											cShortClassName := cWord
											cClassNameSpace := ""
											//								cClassType := cWord
										ELSE
											cShortClassName := cWord:Substring(nAt + 1)
											cClassNameSpace := cWord:Substring(0 , nAt)
											//								cClassType := cWord:Substring(nAt + 1)
										END IF
										IF .NOT. String.IsNullOrEmpty(cBaseNameSpace)
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
													state:lIgnore := TRUE
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
											state:lIgnore := TRUE
										CASE eStep == ParseStep.AfterBeginNamespace
											eStep := ParseStep.None
											aNameSpaces:Add(cWord)
											cBaseNameSpace := GetNameSpace(aNameSpaces)
											_SetLineType(oStatementLine, LineType.BeginNamespace)
											oStatementLine:cArgument := cWord
											state:lIgnore := TRUE
										CASE .NOT. state:lNameFound
											state:lNameFound := TRUE
											oInfo:nStartLine := nEntityStartLine
											oInfo:nCol	:= nEntityStartCol
											oInfo:nOffSet := nEntityStartOffSet + oInfo:nCol
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
											IF oInfo:eType == EntityType._Class .AND. lPartial
												oInfo:lPartial := TRUE
											END IF
											AddEntity(oInfo, oLine)

											lPartial := FALSE
									END CASE

							END CASE
							IF state:lInParams
								IF cChar == ','
									IF .NOT. state:lParam
										VAR oParam := oInfo:AddParam(cWord)
										oParam:nCol := nChar - cWord:Length

									END IF
									state:lParam := FALSE
								ELSEIF .NOT. state:lParam .AND. sWord:Length != 0
									IF .NOT. cWord == "SELF"
										VAR oParam := oInfo:AddParam(cWord)
										oParam:nCol := nChar - cWord:Length
										state:lParam := TRUE
									ELSE
										oCurrentMethod:lExtension := TRUE
									END IF
								END IF
							ENDIF

							IF cChar == '('
								state:lInParams := TRUE
							ELSEIF cChar == '[' .AND. oInfo != NULL .AND. oInfo:eType == EntityType._Property .AND. .NOT. state:lInParams
								state:lInParams := TRUE
							ELSEIF state:lInParams .AND. cChar == ')'
								state:lInParams := FALSE
							ELSEIF state:lInParams .AND. cChar == ']' .AND. oInfo:eType == EntityType._Property
								state:lInParams := FALSE
							END IF
							//					lIgnore := TRUE
						CASE state:lFirstWord .AND. cUpperWord == "BEGIN"
							lInEnum := FALSE
							eStep := ParseStep.AfterBegin
						CASE eStep == ParseStep.AfterBegin
							lInEnum := FALSE
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
						CASE state:lFirstWord .AND. cUpperWord == "END"
							lInEnum := FALSE
							eStep := ParseStep.AfterEnd
							_SetLineType(oStatementLine, LineType.TokenOut)
							oStatementLine:cArgument := cUpperWord
						CASE eStep == ParseStep.AfterEnd
							state:lIgnore := TRUE
							lInEnum := FALSE
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

						CASE state:lFirstWord .AND. cUpperWord == "USING"
							lInEnum := FALSE
							eStep := ParseStep.AfterUsing
							_SetLineType(oStatementLine, LineType.Using)

						CASE lAllowEntityParse .AND. ;  // 2nd .not. is for IF(logic) CASE(something) etc syntax (paren after IF/CASE etc)
							.NOT. (.NOT. lEscapedWord .AND. cRealChar == '(' .AND. System.Array.IndexOf(<STRING>{"IF", "ELSEIF", "WHILE", "CASE", "FOR"} , cUpperWord) != -1) .AND. ;
							.NOT. (lIsSpaceChar .OR. cRealChar == ';' .OR. lBeforeLexerChange .OR. lContinueNextLine) .AND. .NOT. (state:lField .OR. state:lLocal)
							state:lIgnore := TRUE
						CASE state:lLocal .AND. .NOT. lEscapedWord .AND. ( cUpperWord == "IMPLIED" .OR. cUpperWord == "VAR" )
							state:lImpliedLocal := TRUE
							nAfterColonEquals := 0
						CASE lAllowEntityParse .AND. (state:lField .OR. state:lLocal .OR. lInEnum) .AND. .NOT. state:lNameFound .AND. cWord == "DIM" .AND. .NOT. lEscapedWord
							state:lDimDeclaration := TRUE
						CASE lAllowEntityParse .AND. (state:lField .OR. state:lLocal .OR. lInEnum) .AND. .NOT. state:lNameFound
							state:lNameFound := TRUE
							state:lEntityFound := TRUE
							oInfo := EntityObject{}
							oInfo:SetNamespaces(aNameSpaces)
							oInfo:nCol := nChar - cWord:Length
							oInfo:nPosition := nEntityStartOffset + oInfo:nCol + SELF:StartPosition
							oInfo:lStatic := lStatic
							oInfo:eAccessLevel := eAccessLevel
							oInfo:eModifiers := eModifiers
							oInfo:cRetType := "USUAL"
							//
							IF state:lLocal
								oInfo:eType := EntityType._Local
								//
                                // only switch to implied local when the var name is followed by ':=' or '='
                                IF state:lImpliedLocal
                                    LOCAL followedByAssign := FALSE AS LOGIC
                                    LOCAL nNext := nChar AS INT
                                    LOCAL cTemp := cChar AS CHAR
                                    DO WHILE nNext <=nLineLen
                                        IF cTemp == ' ' .OR. cTemp == '\t'
                                            cTemp := cLine[nNext]
                                            nNext++
                                        ELSE
                                            IF cTemp == ':'
                                                IF nNext <= nLineLen .AND. cLine[nNext] == '='
                                                    followedByAssign := TRUE
                                                ENDIF
                                            ELSEIF cTemp == '='
                                                followedByAssign := TRUE
                                            ENDIF
                                            EXIT
                                        ENDIF
                                    ENDDO
                                    IF ! followedByAssign
                                        state:lImpliedLocal := FALSE
                                    ENDIF
                                ENDIF
								IF state:lImpliedLocal
									oInfo:cRetType := XVariable.VarType
								ENDIF
								//
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
									IF ( oParent != NULL )
										oParent:AddChild(oInfo)
									ENDIF
								ENDIF
							END IF
							//
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
									state:lNameFound := FALSE
									state:lEntityFound := FALSE
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
							IF state:lLocal .AND. lIncludeLocals
								aLocals:Add(oInfo)
							END IF
						CASE state:lFirstWord .OR. state:lFirstChar
							IF System.Array.IndexOf(aTokenInOut, cUpperWord) != -1
								_SetLineType(oStatementLine, LineType.TokenInOut)
								oStatementLine:cArgument := cUpperWord
							ELSEIF System.Array.IndexOf(aTokenIn, cUpperWord) != -1
								_SetLineType(oStatementLine, LineType.TokenIn)
								oStatementLine:cArgument := cUpperWord
							ELSEIF System.Array.IndexOf(aTokenOut , cUpperWord) != -1
								_SetLineType(oStatementLine, LineType.TokenOut)
								oStatementLine:cArgument := cUpperWord
							ELSEIF cUpperWord == "GET" .OR. cUpperWord == "SET"
								IF lInProperty
									_SetLineType(oStatementLine, LineType.TokenIn)
									oStatementLine:cArgument := cUpperWord
								ENDIF
							ELSE
								IF .NOT. lContinueNextLine
									IF .NOT. cUpperWord == "FOR" // Allow LOCAL after FOR
										state:lIgnore := TRUE
									ENDIF
								END IF
							END IF

					END CASE

					IF cUpperWord == "FROM"
						state:lLinqSelect := TRUE
					ENDIF


					IF (cChar != '#' .OR. sWord:Length != 0) .AND. .NOT. lInAttribute
						state:lFirstWord := FALSE
					END IF
					sWord:Length := 0
					lEscapedWord := FALSE
					// End of parsing

				END DO
			NEXT
			// Check on exit
			IF ( oInfo != NULL ) .AND. state:lImpliedLocal .AND. ( oInfo:eType == EntityType._Local )
				BuildEntityContext( oInfo, context, cWord)
				//
			ENDIF
			// Finish and add types to aTypes
			aResult := List<EntityObject>{}
			aResult:Add(SELF:oGlobalObject)
			FOREACH oEnt AS EntityObject IN aEntities
				IF oEnt:eType:IsType()
					aResult:Add(oEnt)
				ELSEIF oEnt:oParent == NULL
					aResult:Add(oEnt)
				ENDIF
			NEXT
			RETURN ParseResult{SELF}



		PROTECTED METHOD BuildEntityContext( oInfo AS EntityObject, context AS ParseContext, cWord AS STRING ) AS VOID
			// This is after the VAR definition, afaik
			context:LastMeaningPos := SELF:StartPosition + context:LastMeaningPos
			// this is the last pos after the name
			LOCAL endOfName := oInfo:nPosition + oInfo:cName:Length AS INT
			// +2 for ":=" or "IN" unless we may have a trouble of unTerminated VAR definition
			IF ( context:LastMeaningPos <= endOfName + 2 )
				// incorrect pos
				context:LastMeaningPos := -1
			ENDIF
			// Try to recognize Literal Numeric
			IF ( context:LastMeaningPos > - 1 ) .AND. ( context:Status == WordStatus.Text )
				IF !String.IsNullOrEmpty(cWord )
					LOCAL dummy AS INT64
					LOCAL dummy2 AS DOUBLE
					IF int64.TryParse( cWord, OUT dummy )
						context:Status := WordStatus.Literal
						context:SubStatus := WordSubStatus.LiteralInt
					ELSE
						IF double.TryParse( cWord, OUT dummy2 )
							context:Status := WordStatus.Literal
							context:SubStatus := WordSubStatus.LiteralDouble
						ENDIF
					ENDIF
				ENDIF
			ENDIF
			oInfo:Context := context





		PROTECTED STATIC METHOD GetNameSpace(aNameSpaces AS List<STRING>) AS STRING
			LOCAL oEnumerator AS IEnumerator
			LOCAL cRet AS STRING
			cRet := ""
			IF aNameSpaces == NULL
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
					RETURN TRUE
				END SWITCH
			RETURN FALSE

		STATIC METHOD SupportNestedType(SELF e AS ENtityType) AS LOGIC
			SWITCH e
			CASE EntityType._Class
				CASE EntityType._Structure
					RETURN TRUE
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
					RETURN TRUE
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
					RETURN TRUE
				END SWITCH
			RETURN FALSE
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
					RETURN TRUE
				END SWITCH
			RETURN FALSE

		STATIC METHOD NeedsEndKeyword(SELF e AS EntityType) AS LOGIC
			SWITCH e
				CASE EntityType._Class
				CASE EntityType._Structure
				CASE EntityType._Interface
				CASE EntityType._Enum
					RETURN TRUE
			END SWITCH
			RETURN FALSE

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
		PROPERTY nPosition AS INT AUTO
		PROPERTY Context AS ParseContext AUTO

		INTERNAL CONSTRUCTOR()
			SUPER()
			SELF:cInherit := ""
			SELF:cImplements := ""
			SELF:cRetType := ""
			SELF:cClassType := ""
			SELF:aChildren := List<EntityObject>{}
			SELF:oParent := NULL
			RETURN
		INTERNAL CONSTRUCTOR(nType AS EntityType)
			SELF()
			SELF:eType := nType

		INTERNAL METHOD AddChild(oChild AS EntityObject) AS VOID
			SELF:aChildren:Add(oChild)
			oChild:oParent := SELF
			RETURN

		INTERNAL ACCESS IsFuncProcGlobal AS LOGIC
			RETURN SELF:eType == EntityType._Function .OR. SELF:eType == EntityType._Procedure .OR. SELF:eType == EntityType._Global

		INTERNAL METHOD NamespacesEqual(_aNameSpaces AS List<STRING>) AS LOGIC
			LOCAL n AS INT
			IF SELF:aNameSpaces == NULL .OR. SELF:aNameSpaces:Count != _aNameSpaces:Count
				RETURN FALSE
			END IF
			FOR n := 0 UPTO SELF:aNameSpaces:Count - 1
				IF SELF:aNameSpaces[n] != _aNameSpaces[n]
					RETURN FALSE
				END IF
			NEXT
			RETURN TRUE
		INTERNAL METHOD SetNamespaces(_aNameSpaces AS List<STRING>) AS VOID
			LOCAL n AS INT
			IF SELF:NamespacesEqual(_aNameSpaces)
				RETURN
			END IF
			IF SELF:aNameSpaces == NULL
				SELF:aNameSpaces := List<STRING>{_aNameSpaces:Count}
			END IF
			FOR n := 0 UPTO _aNameSpaces:Count - 1
				SELF:aNameSpaces:Add(_aNameSpaces[n])
			NEXT
			RETURN

		INTERNAL METHOD AddParam(cParam AS STRING) AS EntityParamsObject
			RETURN SELF:AddParam(cParam, "")

		INTERNAL METHOD AddParam(cParam AS STRING , cType AS STRING) AS EntityParamsObject
			IF SELF:aParams == NULL
				SELF:aParams := List<EntityParamsObject>{}
			END IF
			LOCAL oParam AS EntityParamsObject
			oParam := EntityParamsObject{cParam , cType}
			SELF:aParams:Add(oParam)
			RETURN oParam

		INTERNAL METHOD SetParamType(cType AS STRING, nParamType AS ParamType) AS VOID
			LOCAL oParam AS EntityParamsObject
			IF SELF:aParams == NULL .OR. SELF:aParams:Count == 0 .OR. String.IsNullOrEmpty(cType)
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
		MEMBER IfDefInOut
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
			lVisFound := FALSE
			lEntityFound := FALSE
			lEntityIsType := FALSE
			lFirstChar := TRUE
			lFirstWord := TRUE
			lInParams := FALSE
			lNameFound := FALSE
			lField := FALSE
			lLocal := FALSE
			lEvent := FALSE
			lParam := FALSE
			lDirective := FALSE
			lIgnore := FALSE
			lExpectName := FALSE
			lLinqSelect := FALSE
			lDimDeclaration := FALSE
			RETURN
	END STRUCTURE

	CLASS ParseContext
		PROPERTY LastMeaningPos AS INT AUTO
		PROPERTY Status AS WordStatus AUTO
		PROPERTY SubStatus AS WordSubStatus AUTO
		PROPERTY AfterIn AS LOGIC AUTO
	END CLASS

END NAMESPACE


