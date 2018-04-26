#using System.Collections.Generic
#using System.Collections
#using System.Reflection

BEGIN NAMESPACE Xide

CLASS BaseBuffer INHERIT LanguageBuffer
	
	CONSTRUCTOR(eFileType AS FileType, _aLines AS List<LineObject>)
		SUPER(eFileType , _aLines)
	RETURN

	VIRTUAL PROTECTED METHOD Parse(eItems AS BufferParseItems) AS ArrayList
	RETURN SELF:Parse(eItems , 0 , 0 , 0)
	VIRTUAL PROTECTED METHOD Parse(eItems AS BufferParseItems , nStartLine AS INT , nEndLine AS INT , nExceptLine AS INT) AS ArrayList
#region locals
		LOCAL oLine AS LineObject
		LOCAL oStatementLine AS LineObject
		LOCAL cLine AS STRING
		LOCAL nLine,nLineLen AS INT
		LOCAL cChar,cOldChar,cOldOldChar AS Char
		LOCAL cRealChar AS Char
		LOCAL cTestChar AS Char
		LOCAL sWord AS System.Text.StringBuilder
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
		LOCAL sFoundType AS System.Text.StringBuilder
		LOCAL lNewCommandInLine AS LOGIC
		LOCAL lContinueNextLine AS LOGIC
		LOCAL lBeforeLexerChange AS LOGIC
		LOCAL cEnumType AS STRING
		LOCAL nChar AS INT
		LOCAL oInfo AS EntityObject
		LOCAL lStatic AS LOGIC
		LOCAL eAccessLevel AS AccessLevel
		LOCAL eModifiers AS EntityModifiers
		LOCAL eLexer AS LexerStep
		LOCAL eStep AS ParseStep
		LOCAL aFields AS ArrayList
		LOCAL aLocals AS ArrayList
		LOCAL nBracketCount AS INT
		LOCAL cBracketOpen , cBracketClose AS Char
		LOCAL lPartial AS LOGIC
		LOCAL n,n1,n2 AS INT
		
		LOCAL aRet AS ArrayList
		LOCAL oWord AS WordObject
		LOCAL lMustLoop AS LOGIC
		LOCAL cCharBeforeWord AS Char
		LOCAL cWordBeforeSpace AS STRING
		LOCAL lFirstLineAmpersand AS LOGIC
		LOCAL lInAttribute AS LOGIC
		LOCAL lAllowAttribute AS LOGIC
		LOCAL nEntityStartLine AS INT
		LOCAL nEntityStartCol AS INT
		LOCAL cUseChar AS STRING
		LOCAL lMoreModified AS LOGIC
		LOCAL lLineNumsVerified AS LOGIC
		LOCAL nAt AS INT
		LOCAL m AS INT

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
		LOCAL nCaseReserved AS INT
		LOCAL nCaseFunctions AS INT
		
		LOCAL lInProperty AS LOGIC
		LOCAL nAfterColonEquals AS INT
		
		LOCAL _lLinesModified AS LOGIC
		LOCAL _lEntities AS LOGIC
		LOCAL _lFields AS LOGIC
		LOCAL _lTokens AS LOGIC
		LOCAL _lWords AS LOGIC
		
		LOCAL lXSharp := SELF:eFileType == FileType.XSharp AS LOGIC
#endregion

state:Reset()

//PyrgasIdeBase.Ide:Text += "|" + nStartLine:ToString() + "," + nEndLine:ToString()
		_lLinesModified := TRUE
		_lEntities := TRUE
		_lTokens := TRUE
		_lWords := FALSE
		IF _lEntities
			_lFields := TRUE
		END IF
		
#region init
		aFields := ArrayList{}
		aLocals := ArrayList{}
		cShortClassName := ""
		cTypedClassName := ""
		cClassNameSpace := ""
		cClassType := ""
		cBaseNameSpace := ""
		aNameSpaces := List<STRING>{}
		
		hEnt := SELF:oParsingOptions:oEntityMarkers
		hVis := SELF:oParsingOptions:oEntityVisibility
		nCaseReserved := 0
		nCaseFunctions := 0
		
		cWordBeforeSpace := NULL
		IF _lWords
			aRet := ArrayList{}
		END IF
		
		IF _lLinesModified .and. nStartLine != 0
			DO WHILE nStartLine > 1 .and. SELF:aLines[nStartLine - 2]:lOutAmpersand
				lMoreModified := TRUE
				nStartLine --
			END DO
			IF nStartLine > 1
				nLine := nStartLine - 1
				DO WHILE nLine > 0
					oLine := SELF:aLines[nLine - 1]
					IF oLine:ContainsEntity
						cShortClassName := oLine:LastEntity:cShortClassName
						cTypedClassName := oLine:LastEntity:cTypedClassName
						cClassNameSpace := oLine:LastEntity:cClassNameSpace
						cClassType := oLine:LastEntity:cClassType
						IF oLine:LastEntity:aNameSpaces != NULL .and. oLine:LastEntity:aNameSpaces:Count != 0
							FOR n := 0 UPTO oLine:LastEntity:aNameSpaces:Count - 1
								aNameSpaces:Add(oLine:LastEntity:aNameSpaces[n])
							NEXT
							cBaseNameSpace := GetNameSpace(aNameSpaces)
						END IF
						EXIT
					END IF
					nLine --
				END DO
			END IF
		END IF
#endregion		
		IF nStartLine == 0
			nStartLine := 1
			nEndLine := SELF:aLines:Count
		ELSE
			IF nStartLine > 1
				oLine := SELF:aLines[nStartLine - 2]
				IF oLine:lOutBlockComment
					eLexer := LexerStep.BlockComment
				END IF
				IF oLine:lOutAmpersand
					lFirstLineAmpersand := TRUE
				END IF
			END IF
			IF nEndLine > SELF:aLines:Count
				nEndLine := SELF:aLines:Count
			END IF
		END IF
		
		IF nStartLine > 1
			nLine := nStartLine - 1
			DO WHILE nLine > 0
				oLine := SELF:aLines[nLine - 1]
				IF oLine:oEntity != NULL
					lInProperty := oLine:oEntity:eType == EntityType._Property
					EXIT
				ELSEIF oLine:IsBeginNamespace .or. oLine:IsEndClass .or. oLine:IsEndProperty
					EXIT
				ELSE
					nLine --
				END IF
			END DO
		END IF
		IF .not. _lEntities
			oLine := SELF:aLines[nStartLine - 1]
			IF oLine:oEntity != NULL .and. oLine:oEntity:eType == EntityType._Property
				lInProperty := TRUE
			END IF
			IF oLine:oEntity != NULL
				lInProperty := oLine:oEntity:eType == EntityType._Property
			END IF
		END IF
		
		sWord := System.Text.StringBuilder{20}
		sFoundType := System.Text.StringBuilder{20}

//		nLine := 0
		nLine := nStartLine
		DO WHILE nLine <= nEndLine
		
			// Line parsing
			oLine := SELF:aLines[nLine - 1]
			oLine:lInAmpersand := lContinueNextLine
			oLine:lInBlockComment := eLexer == LexerStep.BlockComment
			cLine := oLine:LineText

			IF _lEntities
				oLine:oEntity := NULL
				IF _lFields
					oLine:ClearFields()
				ENDIF
			END IF
			IF _lTokens
				oLine:eType := LineType.None
				oLine:cArgument := NULL
			END IF
			IF _lEntities .or. _lTokens
				oLine:oSubLine := NULL
			END IF
			IF lFirstLineAmpersand
				lFirstLineAmpersand := FALSE
				oLine:lInAmpersand := TRUE
			END IF

//			nLine ++
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
				aFields:Clear()
				aLocals:Clear()
				sWord:Length := 0
				cRealChar := ' '
				lEscapedWord := FALSE
				lInAttribute := FALSE
				lAllowAttribute := TRUE
				cWordBeforeSpace := NULL
				oInfo := NULL
				nEntityStartLine := 0
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
					lEscapedWord := TRUE
				ELSE
					IF sWord:Length == 0
						lEscapedWord := FALSE
					END IF
				END IF
				IF cOldChar == ';' .and. eLexer == LexerStep.None .and. (.not. state:lDirective .or. eStep == ParseStep.AfterUsing)
					lNewCommandInLine := FALSE
					lContinueNextLine := TRUE
					IF nChar < nLineLen
						FOR n := nChar UPTO nLineLen - 1
							cTestChar := cLine[n]
							IF cTestChar != ' ' .and. cTestChar != '\t'
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
						aFields:Clear()
						aLocals:Clear()
						sWord:Length := 0
						cRealChar := ' '
						lEscapedWord := FALSE
						lInAttribute := FALSE
						lAllowAttribute := TRUE
						cWordBeforeSpace := NULL
						oInfo := NULL
						nEntityStartLine := 0
						nEntityStartCol := 0
						IF _lEntities .or. _lTokens
							oStatementLine := oLine:AddSubLine(nChar)
						ELSE
							oStatementLine := oLine
						END IF
						cOldChar := ' '
					END IF
				ENDIF

				IF nBracketCount == 0 .and. .not. lFindingType
					IF cOldChar == '{' .or. cOldChar == '['

						// indexed PROPERTY
						IF .not. (cOldChar == '[' .and. state:lEntityFound .and. oInfo != NULL .and. oInfo:eType == EntityType._Property)
							DO CASE
							CASE cOldChar == '{'
								cBracketOpen := '{'
								cBracketClose := '}'
							CASE cOldChar == '['
								cBracketOpen := '['
								cBracketClose := ']'
	//							lInAttribute := TRUE
							END CASE
							nBracketCount := 1
						END IF
					END IF
				END IF

				
/*				// performance hack
				IF state:lIgnore
					DO WHILE nChar < nLineLen
						cChar := cLine[nChar]
						IF cChar != '/' .and. cChar != '"' .and. cChar != '*' .and. cChar != ';'
							cRealChar := cChar
							nChar ++
						ELSE
							EXIT
						END IF
					END DO
					IF nChar >= nLineLen
						EXIT
					END IF
				END IF*/

				IF nChar == nLineLen
					cChar := ' '
				ELSE
					cChar := cLine[nChar]
				ENDIF
				cRealChar := cChar
				nChar ++
				
				IF state:lFirstChar
					IF cOldChar != ' ' .and. cOldChar != '\t'
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
//				CASE state:lDirective .and. eLexer != LexerStep.BlockComment .and. .not. (cRealChar == '*' .and. cOldChar == '/')
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
					lMustLoop := TRUE
				CASE cChar == '"' .and. eLexer != LexerStep.Quote .and. .not. (eLexer == LexerStep.DoubleQuote .and. lEscapedString .and. lEscapedChar)
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
						lBeforeLexerChange := TRUE
						cChar := ' '
					END IF
				CASE cChar == '\'' .and. eLexer != LexerStep.DoubleQuote .and. .not. (eLexer == LexerStep.Quote .and. cOldChar == '\\' .and. lEscapedChar)
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
						lEscapedChar := lEscapedString .and. cChar == '\\'
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
				CASE (cChar == '/' .and. cOldChar == '/') .or. (state:lFirstChar .and. cChar = '*')
					eLexer := LexerStep.Comment
					eWordStatus := WordStatus.Text
					eWordSubStatus := WordSubStatus.Text
					eCharStatus := WordStatus.Comment
					eCharSubStatus := WordSubStatus.CommentLine
					IF _lWords .or. _lTokens
						IF .not. lContinueNextLine
							state:lIgnore := TRUE
						END IF
					ELSE
						EXIT
					END IF
				CASE cChar == '*' .and. cOldChar == '/'
					eLexer := LexerStep.BlockComment
					cChar := ' ' // if next char is '/', we shouldn't go out of BlockComment mode again
					eWordStatus := WordStatus.Text
					eWordSubStatus := WordSubStatus.Text
					eCharStatus := WordStatus.Comment
					eCharSubStatus := WordSubStatus.CommentBlock
					lMustLoop := TRUE
				CASE cChar == '/'
					IF nChar < nLineLen .and. (cLine[nChar] == '/' .or. cLine[nChar] == '*') // about to go into comment mode
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
//						cChar := ' '
					END IF
				END CASE

				IF nBracketCount == 0
//					lInAttribute := state:lFirstWord .and. cRealChar == '[' .and. sWord:Length == 0
					lInAttribute := lAllowAttribute .and. cRealChar == '[' .and. sWord:Length == 0
				ELSE
					IF cChar == cBracketOpen
						nBracketCount ++
					ELSEIF cChar == cBracketClose
						nBracketCount --
					END IF
				ENDIF
				
				IF .not. (_lWords .or. _lTokens)
					IF lMustLoop
						LOOP
					END IF
					IF state:lIgnore .or. lContinueNextLine
						LOOP
					ENDIF
					
					// Ignore code inside {..} , [..]
					IF nBracketCount != 0
						LOOP
					END IF
					
					IF cChar == ';' .and. sWord:Length == 0
						LOOP
					END IF
				END IF
				
				lIsSpaceChar := cChar == ' ' .or. cChar == '\t'
				IF .not. lIsSpaceChar
					lAllowAttribute := cRealChar == '(' .or. cRealChar == ',' .or. (state:lFirstWord .and. (cRealChar == ']' .or. cRealChar == ';') )
				END IF
				IF .not. lIsSpaceChar .and. nEntityStartLine == 0
					nEntityStartLine := nLine
					nEntityStartCol := nChar - 1
				END IF
				IF lIsSpaceChar
					IF sWord:Length == 0
						IF .not. _lWords .and. .not. (_lTokens .and. (cRealChar == ' ' .or. cRealChar == '\t') )
							LOOP
						END IF
					END IF
					lIsBreakChar := FALSE
				ELSE
					lIsBreakChar := hBrk:ContainsKey(cChar)
					IF lIsBreakChar .and. state:lEntityFound
						IF oInfo != NULL .and. oInfo:eType == EntityType._Operator
							IF System.Array.IndexOf(<Char>{'+','-','*','/','%','&','|','>','<','=','!','~'} , cRealChar) != -1
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
					IF state:lFirstWord .and. SELF:oParsingOptions:oDirectives:ContainsKey(cUpperWord)
						eWordStatus := WordStatus.Text
						eWordSubStatus := WordSubStatus.TextDirective
						IF _lWords .and. aRet:Count != 0
							((WordObject)aRet[aRet:Count - 1]):eStatus := WordStatus.Text
							((WordObject)aRet[aRet:Count - 1]):eSubStatus := WordSubStatus.TextDirective
						END IF
						state:lDirective := TRUE
						state:lIgnore := TRUE
							DO CASE
							CASE cUpperWord == "REGION"
								oStatementLine:eType := LineType.RegionIn
							CASE cUpperWord == "ENDREGION"
								oStatementLine:eType := LineType.RegionOut
							CASE cUpperWord == "IFDEF" .or. cUpperWord == "IFNDEF" .or. cUpperWord == "ELSE"
								oStatementLine:eType := LineType.IfdefIn
							CASE cUpperWord == "ENDIF"
								oStatementLine:eType := LineType.IfdefOut
							CASE cUpperWord == "DEFINE"
								oStatementLine:eType := LineType.Define
								eStep := ParseStep.AfterDefine
							CASE cUpperWord == "INCLUDE"
								oStatementLine:eType := LineType.Include
								eStep := ParseStep.AfterInclude
								oStatementLine:cArgument := ""
							CASE cUpperWord == "USING"
								oStatementLine:eType := LineType.Using
								eStep := ParseStep.AfterUsing
								oStatementLine:cArgument := ""
							OTHERWISE
								oStatementLine:eType := LineType.OtherDirective
							END CASE
					ELSE
						eWordStatus := WordStatus.Literal
						eWordSubStatus := WordSubStatus.LiteralSymbol
					ENDIF
				END IF

				IF _lWords
					IF cWord:Length != 0
						IF eWordStatus == WordStatus.Text
							DO CASE
							CASE .not. lEscapedWord .and. .not. state:lDirective .and. cWord:Length > 1 .and. SELF:oParsingOptions:oReserved:ContainsKey(cUpperWord)
								IF .not. (cCharBeforeWord == '.' .or. /*cCharBeforeWord == ':' .or. */cChar == '.')
									LOCAL lReserved AS LOGIC
									lReserved := TRUE
									DO CASE
									CASE lXSharp .and. System.Array.IndexOf(XSharpBuffer.oLinqKeywords, cUpperWord) != -1 .and. .not. state:lLinqSelect
										lReserved := FALSE
									CASE lXSharp .and. (cUpperWord == "CHECKED" .or. cUpperWord == "UNCHECKED" .or. cUpperWord == "FIXED" /*.or. cUpperWord == "UNSAFE"*/)
										IF cWordBeforeSpace == NULL .or. (cWordBeforeSpace:ToUpper() != "BEGIN" .and. cWordBeforeSpace:ToUpper() != "END")
											lReserved := FALSE
										END IF
									CASE cUpperWord == "NAMESPACE" .or. cUpperWord == "LOCK" .or. cUpperWord == "SCOPE"
										IF cWordBeforeSpace == NULL .or. (cWordBeforeSpace:ToUpper() != "BEGIN" .and. cWordBeforeSpace:ToUpper() != "END")
											lReserved := FALSE
										END IF
									CASE cUpperWord == "EXPLICIT" .or. cUpperWord == "IMPLICIT"
										IF cWordBeforeSpace == NULL .or. cWordBeforeSpace:ToUpper() != "OPERATOR"
											lReserved := FALSE
										END IF
									CASE cUpperWord == "CONSTRUCTOR" .or. cUpperWord == "EVENT"
										IF cWordBeforeSpace != NULL
											IF cWordBeforeSpace != "]" .and. .not. hVis:ContainsKey(cWordBeforeSpace:ToUpper())
												lReserved := FALSE
											END IF
										END IF
									CASE cUpperWord == "ENUM" .or. cUpperWord == "PROPERTY"
										IF cWordBeforeSpace != NULL
											IF cWordBeforeSpace != "]" .and. .not. hVis:ContainsKey(cWordBeforeSpace:ToUpper()) .and. .not. cWordBeforeSpace:ToUpper() == "END"
												lReserved := FALSE
											END IF
										END IF
									CASE cUpperWord == "SET" .or. cUpperWord == "GET" .or. cUpperWord == "VALUE" .or. cUpperWord == "AUTO"
										IF .not. lInProperty
											lReserved := FALSE
										END IF
									CASE cUpperWord == "DESTRUCTOR" .or. cUpperWord == "FOREACH" .or. cUpperWord == "REPEAT" .or. cUpperWord == "UNTIL" //.or. cUpperWord == "SWITCH"
										IF cWordBeforeSpace != NULL
											lReserved := FALSE
										END IF

									END CASE
									IF lReserved
										eWordSubStatus := WordSubStatus.TextReserved
										DO CASE
										CASE nCaseReserved==1
											cWord := cUpperWord
										CASE nCaseReserved==2
											cWord := cWord:ToLower()
										CASE nCaseReserved==3
											cWord := SELF:oParsingOptions:oReserved[cUpperWord]
										END CASE
									END IF
								END IF
							CASE cWord:Length > 2 .and. .not. state:lDirective .and. (lIsSpaceChar .or. cChar == '(') .and. ;
							nCaseFunctions != 0 .and. SELF:oParsingOptions:oFunctions:ContainsKey(cUpperWord) .and. ;
							(cWordBeforeSpace == NULL .or. .not. (hEnt:ContainsKey(cWordBeforeSpace) .or. cWordBeforeSpace == ":" .or. cWordBeforeSpace == "."))
								LOCAL lFunction AS LOGIC
								lFunction := FALSE
								IF cChar == '('
									lFunction := TRUE
								ELSE
									n := nChar
									DO WHILE n < cLine:Length
										DO CASE
										CASE cLine[n] == '('
											lFunction := TRUE
										CASE cLine[n] != ' ' .and. cLine[n] != '\t'
											EXIT
										END CASE
										n ++
									END DO
								ENDIF
								IF lFunction
									eWordSubStatus := WordSubStatus.TextFunction
									DO CASE
									CASE nCaseFunctions == 1
										cWord := cUpperWord
									CASE nCaseFunctions == 2
										cWord := cWord:ToLower()
									CASE nCaseFunctions == 3
										cWord := SELF:oParsingOptions:oFunctions[cUpperWord]
									END CASE
								ENDIF
							CASE cUpperWord[0] >= 48 .and. cWord[0] <= 57 .and. _lWords
								LOCAL cNumChar AS Char
								LOCAL lNumber AS LOGIC
								LOCAL lPostFix AS LOGIC
								LOCAL lHex AS LOGIC
								lNumber := TRUE
								lHex := FALSE
								lPostFix := FALSE
								eWordSubStatus := WordSubStatus.LiteralInt
								FOR m := 1 UPTO cUpperWord:Length - 1
									cNumChar := cUpperWord[m]
									DO CASE
									CASE cNumChar >= 48 .and. cNumChar <= 57
									CASE lHex .and. cNumChar >= 65 .and. cNumChar <= 70
									CASE m == 1 .and. (cNumChar == 'X' .or. cNumChar == 'B') .and. cUpperWord[0] == '0'
										lHex := TRUE
										eWordSubStatus := WordSubStatus.LiteralUInt
									CASE m == cUpperWord:Length - 1 .and. (cNumChar == 'U' .or. cNumChar == 'L' .or. cNumChar == 'M' .or. cNumChar == 'S' .or. cNumChar == 'D')
										lPostFix := TRUE
										DO CASE
										CASE cNumChar == 'U'
											eWordSubStatus := WordSubStatus.LiteralUInt
										CASE cNumChar == 'L'
											eWordSubStatus := WordSubStatus.LiteralInt
										CASE cNumChar == 'M'
											eWordSubStatus := WordSubStatus.LiteralDecimal
										CASE cNumChar == 'S'
											eWordSubStatus := WordSubStatus.LiteralSingle
										CASE cNumChar == 'D'
											eWordSubStatus := WordSubStatus.LiteralDouble
										END CASE
									OTHERWISE
										lNumber := FALSE
										EXIT
									END CASE
								NEXT
								IF lNumber
									eWordStatus := WordStatus.Literal
									IF cCharBeforeWord == '.'
										IF .not. lPostFix // eWordSubStatus already set above
											// TODO based on compiler option
//											eWordSubStatus := WordSubStatus.LiteralFloat
											eWordSubStatus := WordSubStatus.LiteralDouble
										END IF
										IF _lWords .and. aRet:Count != 0
											((WordObject)aRet[aRet:Count - 1]):eStatus := eWordStatus
											((WordObject)aRet[aRet:Count - 1]):eSubStatus := eWordSubStatus
										END IF
									END IF
									IF cRealChar == '.'
										eCharStatus := WordStatus.Literal
										// TODO based on compiler option
//										eCharSubStatus := WordSubStatus.LiteralFloat
										eCharSubStatus := WordSubStatus.LiteralDouble
									END IF
								ELSE
									eWordSubStatus := WordSubStatus.Text
								END IF
							END CASE
						END IF
						IF _lWords
							oWord := WordObject{cWord}
							oWord:nStart := nChar - cWord:Length
							oWord:nEnd := nChar - 1
							IF eWordSubStatus == WordSubStatus.LiteralString .and. lEscapedString
								oWord:SetStyle(WordStyle.EscapedLiteral)
							ENDIF
							IF lInAttribute
								oWord:SetStyle(WordStyle.InAttribute)
							END IF
							oWord:eStatus :=  eWordStatus
							oWord:eSubStatus :=  eWordSubStatus
							aRet:Add(oWord)
						END IF
						cWordBeforeSpace := cWord
					END IF
					IF nChar != nLineLen + 1
						IF eCharStatus == WordStatus.Text .and. .not. state:lDirective
							eCharSubStatus := WordSubStatus.TextOperator
						ENDIF
						cUseChar := cRealChar:ToString()
						IF _lWords
							oWord := WordObject{cUseChar}
							oWord:nStart := nChar
							oWord:nEnd := nChar
							oWord:eStatus :=  eCharStatus
							oWord:eSubStatus :=  eCharSubStatus
							IF eCharSubStatus == WordSubStatus.LiteralString .and. lEscapedString
								oWord:SetStyle(WordStyle.EscapedLiteral)
							ENDIF
							IF lInAttribute
								oWord:SetStyle(WordStyle.InAttribute)
							END IF
							aRet:Add(oWord)
						END IF
						IF .not. lIsSpaceChar
							cWordBeforeSpace := cUseChar
						END IF
					ENDIF
				ENDIF
				
				LOCAL lAllowEntityParse AS LOGIC
				lAllowEntityParse := _lEntities .and. .not. state:lIgnore

				DO CASE
				CASE eLexer == LexerStep.BlockComment
					sWord:Length := 0
					lEscapedWord := FALSE
					LOOP
				
				CASE eStep == ParseStep.WaitImplements .and. .not. (cUpperWord == "IMPLEMENTS" .and. .not. lEscapedWord)
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

				CASE state:lLocal .and. state:lImpliedLocal .and. .not. lEscapedWord .and. cUpperWord == "IN"
					nAfterColonEquals := -nChar // FOREACH

				CASE eStep == ParseStep.AfterDefine
					IF .not. eWordSubStatus == WordSubStatus.TextDirective
						oStatementLine:cArgument := cWord
						eStep := ParseStep.None
						state:lIgnore := TRUE
					END IF

				CASE eStep == ParseStep.AfterUsing
					IF _lTokens
						IF eWordStatus == WordStatus.Text .and. .not. eWordSubStatus == WordSubStatus.TextDirective
							oStatementLine:cArgument += cWord
						END IF
						IF .not. lIsSpaceChar .and. cRealChar != ';' .and. eCharSubStatus == WordSubStatus.Text
							oStatementLine:cArgument += cRealChar:ToString()
						ELSEIF lXSharp .and. cRealChar == '.' .and. eCharSubStatus == WordSubStatus.TextOperator
							oStatementLine:cArgument += cRealChar:ToString()
						END IF
					END IF

				CASE eStep == ParseStep.AfterInclude
					IF _lTokens
						IF eWordSubStatus == WordSubStatus.LiteralString
							oStatementLine:cArgument += cWord
						END IF
						IF eCharSubStatus == WordSubStatus.LiteralString .and. cRealChar != '"'
							oStatementLine:cArgument += cRealChar:ToString()
						END IF
					END IF

				CASE eWordStatus == WordStatus.Literal .or. eWordStatus == WordStatus.Comment .or. eWordSubStatus == WordSubStatus.TextDirective

				CASE (lIsSpaceChar .or. cChar == ';') .and. sWord:Length == 0
					lEscapedWord := FALSE
					LOOP
				CASE (state:lFirstChar .and. eWordStatus == WordStatus.Text .and. eCharStatus == WordStatus.Comment) .and. sWord:Length == 0
					lEscapedWord := FALSE
					LOOP
				CASE lInAttribute
					sWord:Length := 0
					lEscapedWord := FALSE
					LOOP
				CASE .not. (_lEntities) .and. .not. _lTokens
				CASE state:lFirstWord .and. cUpperWord == "RETURN" .and. .not. lEscapedWord
					oStatementLine:eType := LineType.Return
					oStatementLine:cArgument := NULL
					state:lIgnore := TRUE
				CASE lAllowEntityParse .and. cChar == ',' .and. (state:lField .or. state:lLocal) .and. state:lNameFound .and. eStep != ParseStep.AfterAs .and. eStep != ParseStep.AfterRef
					IF nBracketCount == 0
						state:lNameFound := FALSE
						state:lEntityFound := FALSE
					END IF
				CASE lAllowEntityParse .and. .not. state:lFirstWord .and. sWord:Length == 0 .and. .not. state:lInParams .and. .not. lFindingType .and. .not. lFindingName
					IF cChar == '('
						IF state:lNameFound .and. oInfo:aParams == NULL
							state:lInParams := TRUE
						END IF
					END IF
				CASE lAllowEntityParse .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. ;
((hVis:ContainsKey(cUpperWord) .and. eStep != ParseStep.AfterBegin) .or. cUpperWord == "EVENT" .or. (;
	(cUpperWord == "LOCAL" .or. cUpperWord == "CATCH" .or. cUpperWord == "FOREACH")) )
// o elegxos gia eStep einai giati to UNSAFE mporei na einai UNSAFE FUNCTION alla kai BEGIN UNSAFE
					state:lVisFound := TRUE
					DO CASE
					CASE cUpperWord == "PROTECT" .or. cUpperWord == "PROTECTED" .or. cUpperWord == "INSTANCE"
						eAccessLevel := AccessLevel.Protected
						eModifiers := eModifiers | EntityModifiers._Protected
					CASE cUpperWord == "HIDDEN" .or. cUpperWord == "PRIVATE"
						eAccessLevel := AccessLevel.Hidden
						eModifiers := eModifiers | EntityModifiers._Private
					CASE cUpperWord == "INTERNAL"
						eAccessLevel := AccessLevel.Internal
						eModifiers := eModifiers | EntityModifiers._Internal
					CASE cUpperWord == "PUBLIC"
						eAccessLevel := AccessLevel.Public
					CASE cUpperWord == "STATIC"
						lStatic := TRUE
						eModifiers := eModifiers | EntityModifiers._Static
					CASE cUpperWord == "CONST"
						lStatic := TRUE
					CASE cUpperWord == "PARTIAL"
						eModifiers := eModifiers | EntityModifiers._Partial
						lPartial := TRUE
					CASE cUpperWord == "VIRTUAL"
						eModifiers := eModifiers | EntityModifiers._Virtual
					CASE cUpperWord == "NEW"
						eModifiers := eModifiers | EntityModifiers._New
					CASE cUpperWord == "SEALED"
						eModifiers := eModifiers | EntityModifiers._Sealed
					CASE cUpperWord == "ABSTRACT"
						eModifiers := eModifiers | EntityModifiers._Abstract
					END CASE
					IF System.Array.IndexOf(<STRING>{"EVENT" , "PROTECT" , "PROTECTED", "INSTANCE" , "EXPORT" , "PUBLIC" , "PRIVATE" , "HIDDEN" , "INTERNAL" , "MEMBER" , "GLOBAL"} , cUpperWord) != -1
						// Allow multiple names in same line
						state:lField := TRUE
						IF cUpperWord == "EVENT"
							state:lEvent := TRUE
						END IF
					END IF
					IF cUpperWord == "LOCAL" .or. cUpperWord == "CATCH" .or. cUpperWord == "FOREACH"
						state:lLocal := TRUE
						state:lImpliedLocal := FALSE
					END IF
				CASE lXSharp .and. lAllowEntityParse .and. .not. lEscapedWord .and. state:lFirstWord .and. cUpperWord == "USING"
					oStatementLine:eType := LineType.Using
					eStep := ParseStep.AfterUsing
					oStatementLine:cArgument := ""
				CASE state:lVisFound .and. lInProperty .and. (cUpperWord == "SET" .or. cUpperWord == "GET") .and. .not. lEscapedWord
					oStatementLine:eType := LineType.TokenIn
					oStatementLine:cArgument := cUpperWord
					state:lIgnore := TRUE
				CASE lAllowEntityParse .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. hEnt:ContainsKey(cUpperWord)
					lInEnum := FALSE
					state:lField := FALSE
					state:lLocal := FALSE
					state:lEvent := FALSE
					IF state:lEntityFound
						state:lIgnore := TRUE
					ENDIF
					state:lEntityFound := TRUE
					state:lEntityIsClass := System.Array.IndexOf(<STRING>{"CLASS","STRUCTURE","STRUCT","INTERFACE","DELEGATE","ENUM","VOSTRUCT","UNION"} , cUpperWord) != -1
					IF eStep == ParseStep.AfterEnd .and. state:lEntityIsClass
						oStatementLine:eType := LineType.EndClass
						state:lEntityFound := FALSE
						state:lEntityIsClass := FALSE
						state:lIgnore := TRUE
						lInEnum := FALSE
						cShortClassName := ""
						cTypedClassName := ""
						cClassNameSpace := ""
						cClassType := ""
						lInProperty := FALSE
					ELSEIF eStep == ParseStep.AfterEnd .and. cUpperWord == "PROPERTY"
						oStatementLine:eType := LineType.EndProperty
						state:lEntityFound := FALSE
						state:lEntityIsClass := FALSE
						state:lIgnore := TRUE
						lInEnum := FALSE
						lInProperty := FALSE
					ELSE
						lInEnum := cUpperWord == "ENUM"
						oInfo := EntityObject{}
						IF _lEntities
							oInfo:SetNamespaces(aNameSpaces)
						END IF
						oInfo:eType := GetEntityType(cUpperWord)
						lInProperty := oInfo:eType == EntityType._Property
						IF state:lEntityIsClass
							cClassType := cUpperWord
							DO CASE
							CASE cUpperWord == "CLASS"
//								oInfo:cInherit := "System.Object"
								oInfo:cInherit := ""
								oInfo:cImplements := ""
							CASE cUpperWord == "STRUCTURE" .or. cUpperWord == "STRUCT"
//								oInfo:cInherit := "System.ValueType"
								oInfo:cInherit := ""
								oInfo:cImplements := ""
							CASE cUpperWord == "DELEGATE"
//								oInfo:cInherit := "System.MultiCastDelegate"
								oInfo:cInherit := ""
								oInfo:cImplements := ""
//								oInfo:cRetType := "USUAL" // Default
								oInfo:cRetType := "" // Default
							CASE cUpperWord == "ENUM"
//								oInfo:cInherit := "System.Enum"
								oInfo:cInherit := ""
								oInfo:cImplements := ""
//								oInfo:cRetType := "INT"
								oInfo:cRetType := ""
								cEnumType := "INT"
							OTHERWISE
								oInfo:cInherit := ""
								oInfo:cImplements := ""
							END CASE
						ELSE
							IF oInfo:eType == EntityType._Method .or. oInfo:eType == EntityType._Access .or. oInfo:eType == EntityType._Property .or. ;
								oInfo:eType == EntityType._Function .or. oInfo:eType == EntityType._Global
								oInfo:cRetType := "USUAL" // Default
							ELSE
								oInfo:cRetType := ""
							END IF
						END IF
						oInfo:lStatic := lStatic
						oInfo:eAccessLevel := eAccessLevel
						oInfo:eModifiers := eModifiers
						IF oInfo:eType == EntityType._Constructor .or. oInfo:eType == EntityType._Destructor
							state:lNameFound := TRUE // Dont't wait for a name, add it to the list now
							oInfo:nLine := nEntityStartLine
							oInfo:nCol := nEntityStartCol
//							oInfo:cName := cWord
//							oInfo:cName := cClass // since there is no other name available
							oInfo:cName := iif(oInfo:eType == EntityType._Constructor , ".ctor" , ".dtor")
							oInfo:cShortClassName := cShortClassName
							oInfo:cTypedClassName := cTypedClassName
							oInfo:cClassNameSpace := cClassNameSpace
							oInfo:cClassType := cClassType
							IF _lEntities
								oStatementLine:oEntity := oInfo
//								oStatementLine:lEntity := TRUE
							END IF
							IF cChar == '('
								state:lInParams := TRUE
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
					CASE state:lInParams .and. (cUpperWord == "REF" .or. cUpperWord == "OUT") .and. .not. lEscapedWord
						eStep := ParseStep.AfterRef
						sFoundType:Length := 0
						n1 := 0;n2 := 0
					CASE eStep == ParseStep.AfterInherit .or. eStep == ParseStep.AfterImplements .or. ;
						eStep == ParseStep.AfterAs .or. eStep == ParseStep.AfterRef .or. ;
						eStep == ParseStep.AfterBeginNamespace .or. ;
						.not. state:lNameFound
						
						IF eStep == ParseStep.AfterInherit .or. eStep == ParseStep.AfterImplements .or. eStep == ParseStep.AfterAs .or. eStep == ParseStep.AfterRef
							// Waiting for type that may be generic, array
							lFindingType := TRUE
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
									IF _lWords
										cUseChar := cChar:ToString()
										IF _lWords
											oWord := WordObject{cUseChar}
											oWord:nStart := nChar
											oWord:nEnd := nChar
											oWord:eStatus := WordStatus.Text
											oWord:eSubStatus := WordSubStatus.Text
											aRet:Add(oWord)
										END IF
									END IF
								END DO
							DO CASE
							CASE cChar == '.'
								sFoundType:Append('.')
							CASE cChar == '['
								sFoundType:Append('[')
								n1 ++
							CASE cChar == ']'
								IF n1 > 0
									sFoundType:Append(']')
									n1 --
								END IF
							CASE cChar == '<'
								sFoundType:Append('<')
								n2 ++
							CASE cChar == '>'
								IF n2 > 0
									sFoundType:Append('>')
									n2 --
								END IF
							CASE (n1 != 0 .or. n2 != 0) .and. cChar != ' ' .and. cChar != '\t' .and. cChar != ';'
								sFoundType:Append(cChar)
							END CASE
							IF cChar == '.' .or. n1 != 0 .or. n2 != 0 .or. (cChar == ']' .and. nChar < nLineLen .and. cLine[nChar] == '[') // more dims
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

						IF state:lEntityIsClass .and. .not. state:lNameFound
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
							IF eStep == ParseStep.AfterRef
								oInfo:SetParamType("&" + cWord)
							ELSE
								oInfo:SetParamType(cWord)
							ENDIF
							eStep := ParseStep.None
						CASE eStep == ParseStep.AfterInherit
							oInfo:cInherit := cWord
//							eStep := ParseStep.None
							eStep := ParseStep.WaitImplements
//							state:lIgnore := TRUE
						CASE eStep == ParseStep.AfterImplements
							IF oInfo:cImplements:Trim():Length == 0
								oInfo:cImplements := cWord
							ELSE
								oInfo:cImplements += ", " + cWord
							END IF
							DO CASE
							CASE cRealChar == ','
								eStep := ParseStep.AfterImplements
								sFoundType:Length := 0
								n1 := 0;n2 := 0
							CASE cRealChar == ' ' .or. cRealChar == '\t'
								eStep := ParseStep.WaitCommaImplements
							OTHERWISE
								eStep := ParseStep.None
								state:lIgnore := TRUE
							END CASE
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
								FOR n := 0 UPTO aFields:Count - 1
									((EntityObject)aFields[n]):cRetType := cRetType
								NEXT
							ELSEIF state:lLocal
								FOR n := 0 UPTO aLocals:Count - 1
									((EntityObject)aLocals[n]):cRetType := cRetType
								NEXT
							END IF
							eStep := ParseStep.None
							state:lIgnore := TRUE
						CASE eStep == ParseStep.AfterBeginNamespace
							eStep := ParseStep.None
							aNameSpaces:Add(cWord)
							cBaseNameSpace := GetNameSpace(aNameSpaces)
							oStatementLine:eType := LineType.BeginNamespace
//							oStatementLine:cBeginNamespace := cWord
							oStatementLine:cArgument := cWord
							state:lIgnore := TRUE
						CASE .not. state:lNameFound
							IF (.not. state:lField .or. _lFields) .and. (.not. state:lLocal)
								state:lNameFound := TRUE
								oInfo:nLine := nEntityStartLine
								oInfo:nCol := nEntityStartCol
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
									oInfo:lPartial := TRUE
								END IF
								IF _lEntities
									oStatementLine:oEntity := oInfo
//									oStatementLine:lEntity := TRUE
								END IF
								lPartial := FALSE
							ELSE
								state:lIgnore := TRUE
							END IF
						END CASE
					
					END CASE
					IF state:lInParams
						IF cChar == ','
							IF .not. state:lParam
								oInfo:AddParam(cWord)
							END IF
							state:lParam := FALSE
						ELSEIF .not. state:lParam .and. sWord:Length != 0
							IF .not. cWord == "SELF"
								oInfo:AddParam(cWord)
								state:lParam := TRUE
							END IF
						END IF
					ENDIF

					IF cChar == '('
						state:lInParams := TRUE
					ELSEIF cChar == '[' .and. oInfo != NULL .and. oInfo:eType == EntityType._Property .and. .not. state:lInParams
						state:lInParams := TRUE
					ELSEIF state:lInParams .and. cChar == ')'
						state:lInParams := FALSE
					ELSEIF state:lInParams .and. cChar == ']' .and. oInfo:eType == EntityType._Property
						state:lInParams := FALSE
					END IF
//					lIgnore := TRUE
				CASE state:lFirstWord .and. cUpperWord == "BEGIN"
					lInEnum := FALSE
					eStep := ParseStep.AfterBegin
				CASE eStep == ParseStep.AfterBegin
					lInEnum := FALSE
					IF cUpperWord == "NAMESPACE"
						eStep := ParseStep.AfterBeginNamespace
					ELSEIF _lTokens
						DO CASE
						CASE cUpperWord == "LOCK"
							oStatementLine:eType := LineType.TokenIn
//							oStatementLine:cArgument := "BEGIN_LOCK"
							oStatementLine:cArgument := "BEGIN"
						CASE cUpperWord == "SEQUENCE"
							oStatementLine:eType := LineType.TokenIn
//							oStatementLine:cArgument := "BEGIN_SEQUENCE"
							oStatementLine:cArgument := "BEGIN"
						CASE cUpperWord == "SCOPE"
							oStatementLine:eType := LineType.TokenIn
//							oStatementLine:cArgument := "BEGIN_SCOPE"
							oStatementLine:cArgument := "BEGIN"
						CASE lXSharp .and. (cUpperWord == "CHECKED" .or. cUpperWord == "UNCHECKED" .or. ;
											cUpperWord == "UNSAFE" .or. cUpperWord == "USING" .or. ;
											cUpperWord == "FIXED")
							oStatementLine:eType := LineType.TokenIn
							oStatementLine:cArgument := "BEGIN"
						END CASE
					ELSE
						state:lIgnore := TRUE
					END IF
				CASE state:lFirstWord .and. cUpperWord == "END"
					lInEnum := FALSE
					eStep := ParseStep.AfterEnd
					IF _lTokens
						oStatementLine:eType := LineType.TokenOut
						oStatementLine:cArgument := "END"
					END IF
				CASE eStep == ParseStep.AfterEnd
					state:lIgnore := TRUE
					lInEnum := FALSE
					IF System.Array.IndexOf(<STRING>{"CLASS","STRUCTURE","STRUCT","INTERFACE","ENUM"} , cUpperWord) != -1
						// TODO: erxetai pote edw???
						oStatementLine:eType := LineType.EndClass
						IF lAllowEntityParse
							cShortClassName := ""
							cTypedClassName := ""
							cClassNameSpace := ""
							cClassType := ""
						END IF
					ELSEIF cUpperWord == "NAMESPACE"
						oStatementLine:eType := LineType.EndNamespace
						IF lAllowEntityParse
							IF aNameSpaces:Count != 0
								aNameSpaces:RemoveAt(aNameSpaces:Count - 1)
								cBaseNameSpace := GetNameSpace(aNameSpaces)
							END IF
						END IF
					ENDIF
				
				CASE _lTokens .and. state:lFirstWord .and. cUpperWord == "USING"
					lInEnum := FALSE
					eStep := ParseStep.AfterUsing
					oStatementLine:eType := LineType.Using

//				CASE .not. lIsSpaceChar .and. .not. (cChar == ',' .and. state:lField)
				CASE lAllowEntityParse .and. ;  // 2nd .not. is for IF(logic) CASE(something) etc syntax (paren after IF/CASE etc)
					.not. (.not. lEscapedWord .and. cRealChar == '(' .and. System.Array.IndexOf(<STRING>{"IF", "ELSEIF", "WHILE", "CASE", "FOR"} , cUpperWord) != -1) .and. ;
					.not. (lIsSpaceChar .or. cRealChar == ';' .or. lBeforeLexerChange .or. lContinueNextLine) .and. .not. (state:lField .or. state:lLocal)
					state:lIgnore := TRUE
				CASE state:lLocal .and. .not. lEscapedWord .and. cUpperWord == "IMPLIED"
					state:lImpliedLocal := TRUE
					nAfterColonEquals := 0
				CASE lAllowEntityParse .and. (state:lField .or. state:lLocal .or. lInEnum) .and. .not. state:lNameFound .and. cWord == "DIM" .and. .not. lEscapedWord
					state:lDimDeclaration := TRUE
				CASE lAllowEntityParse .and. (state:lField .or. state:lLocal .or. lInEnum) .and. .not. state:lNameFound
					state:lNameFound := TRUE
					state:lEntityFound := TRUE
					IF _lFields
						oInfo := EntityObject{}
						IF _lEntities
							oInfo:SetNamespaces(aNameSpaces)
						END IF
						IF state:lLocal
							oInfo:eType := EntityType._Local
						ELSEIF state:lEvent
							oInfo:eType := EntityType._Event
						ELSE
							oInfo:eType := EntityType._Field
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
								aLocals:Add(oInfo)
							ELSE
								aFields:Add(oInfo)
							END IF
							IF cChar == ','
								state:lNameFound := FALSE
								state:lEntityFound := FALSE
							END IF
						ENDIF
						oInfo:nLine := nLine
						oInfo:cName := cWord
						oInfo:cShortClassName := cShortClassName
						oInfo:cTypedClassName := cTypedClassName
						oInfo:cClassNameSpace := cClassNameSpace
						oInfo:cClassType := cClassType
						IF _lFields .and. state:lField
							IF _lEntities
								oStatementLine:AddField(oInfo)
							END IF
						END IF
					ELSE
						state:lIgnore := TRUE
						lInEnum := FALSE
						state:lField := FALSE
						state:lLocal := FALSE
						state:lEvent := FALSE
					END IF
				CASE state:lFirstWord .or. state:lFirstChar
					IF _lTokens
						IF System.Array.IndexOf(<STRING>{"IF", "ELSE", "ELSEIF", "DO", "WHILE", "CASE", "OTHERWISE", "FOR", "FOREACH", "TRY", "CATCH", "FINALLY", "RECOVER", "REPEAT", "SWITCH"} , cUpperWord) != -1
							oStatementLine:eType := LineType.TokenIn
							oStatementLine:cArgument := cUpperWord
						ELSEIF System.Array.IndexOf(<STRING>{"ENDIF", "ENDDO", "ENDCASE", "NEXT", "ENDTRY", "UNTIL"} , cUpperWord) != -1
							oStatementLine:eType := LineType.TokenOut
							oStatementLine:cArgument := cUpperWord
						ELSEIF cUpperWord == "GET" .or. cUpperWord == "SET"
							IF lInProperty
								oStatementLine:eType := LineType.TokenIn
								oStatementLine:cArgument := cUpperWord
							ENDIF
						ELSE
							IF .not. lContinueNextLine
								state:lIgnore := TRUE
							END IF
						END IF
					ELSE
						IF .not. lContinueNextLine
							IF .not. cUpperWord == "FOR" // Allow LOCAL after FOR
								state:lIgnore := TRUE
							ENDIF
						END IF
					END IF
				END CASE

				IF lXSharp .and. cUpperWord == "FROM"
					state:lLinqSelect := TRUE
				ENDIF


				IF (cChar != '#' .or. sWord:Length != 0) .and. .not. lInAttribute
					state:lFirstWord := FALSE
				END IF
				sWord:Length := 0
				lEscapedWord := FALSE
				// End of parsing
				
			END DO

			IF oLine != NULL
				oLine:lOutAmpersand := lContinueNextLine
				oLine:lOutBlockComment := eLexer == LexerStep.BlockComment
			END IF
			
			nLine ++
			
			IF _lLinesModified .and. nLine > nEndLine .and. nLine <= SELF:aLines:Count
				oLine := SELF:aLines[nLine - 1]
				IF (oLine:lInBlockComment .and. eLexer != LexerStep.BlockComment) .or. ;
					(.not. oLine:lInBlockComment .and. eLexer == LexerStep.BlockComment) .or. ;
					oLine:lInAmpersand .or. lContinueNextLine
					nEndLine ++
					lMoreModified := TRUE
				ELSE
					LOCAL nTestLine AS INT
					LOCAL lMustExit AS LOGIC
					nTestLine := nLine
					lMustExit := FALSE
					DO WHILE nTestLine <= SELF:aLines:Count
						nLine ++ // gia to teliko VerifyLines. H trexousa nLine (nEndLine + 1) ginetai Verify edw
						oLine := SELF:aLines[nTestLine - 1]
						DO WHILE oLine != NULL
							DO CASE
							CASE oLine:IsBeginNamespace
								aNameSpaces:Add(oLine:cArgument)
								cBaseNameSpace := GetNameSpace(aNameSpaces)
							CASE oLine:IsEndNamespace
								IF aNameSpaces:Count != 0
									aNameSpaces:RemoveAt(aNameSpaces:Count - 1)
									cBaseNameSpace := GetNameSpace(aNameSpaces)
								END IF
							CASE oLine:IsEndClass
								cShortClassName := ""
								cTypedClassName := ""
								cClassNamespace := ""
								cClassType := ""

							CASE oLine:aFields != NULL .and. oLine:aFields:Count != 0
								lLineNumsVerified := oLine:VerifyEntitiesLineNum(nTestLine , FALSE)
								IF oLine:aFields[0]:cTypedClassName == cTypedClassName .and. oLine:aFields[0]:NamespacesEqual(aNamespaces)
									lMustExit := TRUE
									EXIT
								ELSE
									FOR n := 0 UPTO oLine:aFields:Count - 1
										oLine:aFields[n]:cShortClassName := cShortClassName
										oLine:aFields[n]:cTypedClassName := cTypedClassName
										oLine:aFields[n]:cClassNamespace := cClassNamespace
										oLine:aFields[n]:SetNamespaces(aNameSpaces)
										oLine:aFields[n]:cClassType := cClassType
									NEXT
								END IF
								
							CASE oLine:lEntity
								lLineNumsVerified := oLine:VerifyEntitiesLineNum(nTestLine , FALSE)
								IF oLine:oEntity:IsType
									cShortClassName := oLine:oEntity:cShortClassName
									cTypedClassName := oLine:oEntity:cTypedClassName
									cClassNamespace := oLine:oEntity:cClassNamespace
									cClassType := oLine:oEntity:cClassType
									nAt := cTypedClassName:LastIndexOf('.')
									IF nAt <= 0 .or. nAt >= cTypedClassName:Length - 1
										cClassNameSpace := ""
									ELSE
										cClassNameSpace := cTypedClassName:Substring(0 , nAt)
									END IF
									IF .not. String.IsNullOrEmpty(cBaseNameSpace)
										IF cClassNameSpace == ""
											cClassNameSpace := cBaseNameSpace
										ELSE
											cClassNameSpace := cBaseNameSpace + "." + cClassNameSpace
										END IF
									END IF
								END IF
								IF ( (oLine:oEntity:IsFuncProcGlobal .and. oLine:oEntity:NamespacesEqual(aNamespaces)) .or. ;
									(.not. oLine:oEntity:IsFuncProcGlobal .and. oLine:oEntity:cTypedClassName == cTypedClassName .and. oLine:oEntity:NamespacesEqual(aNamespaces)) )
									lMustExit := TRUE
									EXIT
								ELSE
									IF oLine:oEntity:IsFuncProcGlobal
										oLine:oEntity:cShortClassName := ""
										oLine:oEntity:cTypedClassName := ""
										oLine:oEntity:cClassNamespace := ""
										oLine:oEntity:cClassType := ""
									ELSE
										oLine:oEntity:cShortClassName := cShortClassName
										oLine:oEntity:cTypedClassName := cTypedClassName
										oLine:oEntity:cClassNamespace := cClassNamespace
										oLine:oEntity:cClassType := cClassType
									END IF
									oLine:oEntity:SetNamespaces(aNameSpaces)
//									lMoreModified := TRUE
/*									PyrgasIdeBase.Ide:Text += "|C|"
									IF PyrgasIdeBase.Ide:Text:Length > 256
										PyrgasIdeBase.Ide:Text := ""
									ENDIF*/
								END IF
							END CASE
							
							oLine := oLine:oSubLine
							
						ENDDO

						IF lMustExit
							EXIT
						END IF

						nTestLine ++

					END DO
				END IF

			END IF
			
		ENDDO

		IF _lLinesModified .and. nLine > nEndLine .and. .not. lLineNumsVerified
			DO WHILE nLine < SELF:aLines:Count
				oLine := SELF:aLines[nLine - 1]
				IF oLine:VerifyEntitiesLineNum(nLine , TRUE)
					EXIT
				END IF
				nLine ++
			END DO
		ENDIF
		
		IF _lWords
			aRet:Insert(0 , WordObject{""})
			aRet:Add(WordObject{""})
		END IF
		
		IF _lLinesModified
			IF lMoreModified
				aRet := ArrayList{}
			ELSE
				aRet := NULL
			END IF
		END IF
		
	RETURN aRet

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
		case "FUNCTION" 
        CASE "FUNC"
			eType := EntityType._Function
		case "PROCEDURE" 
        CASE "PROC"
			eType := EntityType._Procedure
		CASE "ENUM"
			eType := EntityType._Enum
		case "STRUCTURE" 
        CASE "STRUCT"
			eType := EntityType._Structure
		CASE "VOSTRUCT"
			eType := EntityType._VOStruct
		CASE "UNION"
			eType := EntityType._Union
		CASE "GLOBAL"
			eType := EntityType._Global
		CASE "DELEGATE"
			eType := EntityType._Delegate
		CASE "EVENT"
			eType := EntityType._Event
		CASE "INTERFACE"
			eType := EntityType._Interface
		CASE "OPERATOR"
			eType := EntityType._Operator
		CASE "DEFINE"
			eType := EntityType._Define
		END SWITCH
	RETURN eType
	
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

END CLASS

END NAMESPACE
