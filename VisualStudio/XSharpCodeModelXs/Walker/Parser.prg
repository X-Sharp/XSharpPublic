//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Collections.Generic
using System.Collections
using System.Text
using System.IO
//
//function Start() as void
//local cFileName as string
////	cFileName := "C:\Test\test.prg"
////	cFileName := "C:\VIDE\Projects\XIDE\VIDE\PIDE.prg"
//cFileName := "c:\temp\InventoryRepILst.prg"
//Parser.ParseAndDisplay(File.ReadAllLines(cFileName))
//return	
//
begin namespace XSharpModel
	class Parser
		static protect oEntityMarkers as Dictionary<string , string>
		static protect oEntityVisibility as Dictionary<string , string>
		static protect oDirectives as Dictionary<string , string>
		static protect hBrk as Dictionary<char,char>
		static protect aTokenIn as string[]
		static protect aTokenOut as string[]
		static protect aEntityWords as string[]
		static protect aOperators as char[]
		static protect aEndKeywords as string[]
		//STATIC PROTECT nLineCount AS INT
		
		static constructor()
			oEntityMarkers := Dictionary<string , string>{}
			oEntityVisibility := Dictionary<string , string>{}
			oDirectives := Dictionary<string , string>{}
			hBrk := Dictionary<char,char>{}
			aTokenIn := <string>{"IF", "ELSE", "ELSEIF", "DO", "WHILE", "CASE", "OTHERWISE", "FOR", "FOREACH", "TRY", "CATCH", "FINALLY", "RECOVER", "REPEAT", "SWITCH"} 
			aTokenOut := <string>{"ENDIF", "ENDDO", "ENDCASE", "NEXT", "ENDTRY", "UNTIL"}
			aEntityWords := <string>{"EVENT" , "PROTECT" , "PROTECTED", "INSTANCE" , "EXPORT" , "PUBLIC" , "PRIVATE" , "HIDDEN" , "INTERNAL" , "MEMBER" , "GLOBAL"} 
			aOperators := <char>{'+','-','*','/','%','&','|','>','<','=','!','~'}
			aEndKeyWords := <string>{"CLASS","STRUCTURE","STRUCT","INTERFACE","ENUM"}
			local aWords as string[]
			
			aWords := <string>{;
								"CLASS","METHOD","FUNCTION","PROCEDURE","FUNC","PROC","ACCESS","ASSIGN","OPERATOR","DELEGATE",;
								"GLOBAL","CONSTRUCTOR","DESTRUCTOR","STRUCTURE","STRUCT","VOSTRUCT","UNION","ENUM","INTERFACE","PROPERTY","DEFINE"}
			foreach cWord as string in aWords
				oEntityMarkers:Add(cWord:ToUpper() , cWord)
			next
			

			aWords := <string>{;
								"VIRTUAL", "PARTIAL", "_DLL", "ABSTRACT", "SEALED", ;
								"INTERNAL", "HIDDEN", "STATIC", "PROTECTED", "INSTANCE", ;
								"PROTECT", "PRIVATE", "PUBLIC", "EXPORT", "CONST", ;
								"INITONLY", "MEMBER", "NEW", "ASYNC", "EXTERN", "UNSAFE", "OVERRIDE"}
			foreach cWord as string in aWords
				oEntityVisibility:Add(cWord:ToUpper() , cWord)
			next
			
			aWords := <string>{;
								"ifdef","ifndef","endif","else","define","undef","command","translate",;
								"region","endregion","using","include","line","pragma","error","warning"}
			foreach cWord as string in aWords
				oDirectives:Add(cWord:ToUpper() , cWord)
			next
			
			local cBreak as string
			local n as int
			cBreak := e",./;:[]<>?{}`~!@#$%^&*()-=\\+|'\""
			hBrk := Dictionary<char,char>{50}
			for n := 0 upto cBreak:Length - 1
				hBrk:Add(cBreak[n] , cBreak[n])
			next
			
			return
		
			/*	static method ParseAndDisplay(aLineCollection as IEnumerable) as void
		local aLocals := List<EntityObject>{} as List<EntityObject>
		local aEntities := List<EntityObject>{} as List<EntityObject>
		local nLineCount as int
		local d as DateTime
		
		? "Starting parsing..."
		d := DateTime.Now
		LineObject.LinesWithSpecialStuff:Clear()
		nLineCount := Parse(aLineCollection , aLocals , aEntities)
		? "Parsing completed!"
		?
		? "Time elapsed:" , DateTime.Now - d
		?
		? "Total Lines:" , nLineCount
		? "Locals:" , aLocals:Count
		? "Entities:" , aEntities:Count
		? "Directives, block commands etc:" , LineObject.LinesWithSpecialStuff:Count
		?
		? "Press enter to list info"
		Console.ReadLine()
		?
		? "Locals:"
		foreach oLocal as EntityObject in aLocals
		? "Line:" , oLocal:nLine , "Name:" , oLocal:cName , "Type =", oLocal:cRetType
		next
		?
		? "Entities:"
		foreach oEntity as EntityObject in aEntities
		? "Line:" , oEntity:nLine , "Name:" , oEntity:cName , "Type:" , oEntity:eType , "Return Type =", oEntity:cRetType
		next
		?
		? "Directives, Block commands etc:"
		foreach oLine as LineObject in LineObject.LinesWithSpecialStuff
		if oLine:eType == LineType.EndClass .or. oLine:eType == LineType.Return .or. oLine:eType == LineType.Define
		loop
		end if
		? "Line:" , oLine:Line , "Type:" , oLine:eType , ":" , oLine:cArgument
		next
		
		
		return
		*/
		static method Parse(aLineCollection as IEnumerable<string>, aLocals as List<EntityObject>,aEntities as List<EntityObject>) as int
			local oLine as LineObject
			local oStatementLine as LineObject
			local nLine,nLineLen as int
			local cChar,cOldChar,cOldOldChar as char
			local cRealChar as char
			local cTestChar as char
			local sWord as StringBuilder
			local cWord,cUpperWord as string
			local state as ParseState
			local lIsBreakChar as logic
			local lIsSpaceChar as logic
			local lEscapedWord as logic
			local lEscapedChar as logic
			local lEscapedString as logic
			local lInEnum as logic
			local lFindingType as logic
			local lFindingName as logic
			local sFoundType as StringBuilder
			local lNewCommandInLine as logic
			local lContinueNextLine as logic
			local lBeforeLexerChange as logic
			local cEnumType as string
			local nChar as int
			local oInfo as EntityObject
			local lStatic as logic
			local eAccessLevel as AccessLevel
			local eModifiers as EntityModifiers
			local eLexer as LexerStep
			local eStep as ParseStep
			local aLineFields as List<EntityObject>
			local aLineLocals as List<EntityObject>
			local nBracketCount as int
			local cBracketOpen , cBracketClose as char
			local lPartial as logic
			local n,n1,n2 as int
			
			local lMustLoop as logic
			local cCharBeforeWord as char
			local lInAttribute as logic
			local lAllowAttribute as logic
			local nEntityStartLine as int
			local nEntityStartCol as int
			local nAt as int
			
			local eWordStatus as WordStatus
			local eWordSubStatus as WordSubStatus
			local eCharStatus as WordStatus
			local eCharSubStatus as WordSubStatus
			
			local cShortClassName as string
			local cTypedClassName as string
			local cClassNameSpace as string
			local cClassType as string
			local cBaseNameSpace as string
			local aNameSpaces as List<string>
			
			local hVis , hEnt as Dictionary<string,string>
			
			local lInProperty as logic
			local nAfterColonEquals as int
			
			aLineFields := List<EntityObject>{}
			aLineLocals := List<EntityObject>{}
			cShortClassName := ""
			cTypedClassName := ""
			cClassNameSpace := ""
			cClassType := ""
			cBaseNameSpace := ""
			aNameSpaces := List<string>{}
			
			hEnt := oEntityMarkers
			hVis := oEntityVisibility
			
			sWord := StringBuilder{20}
			sFoundType := StringBuilder{20}
			
			state:Reset()
			
			nLine := 0
			foreach cLine as string in aLineCollection
				nLine ++
				oLine := LineObject{nLine}
				
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
				
				if lContinueNextLine
					lContinueNextLine := false
				else
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
					nEntityStartCol := 0
					oStatementLine := oLine
				endif
				if eLexer != LexerStep.BlockComment
					eLexer := LexerStep.None
				end if
				
				do while nChar <= nLineLen // one more than chars in line
					
					// Lexing
					if sWord:Length == 0
						cCharBeforeWord := cRealChar
					end if
					cOldOldChar := cOldChar
					cOldChar := cChar
					if cOldOldChar == '@' .and. cOldChar == '@'
						lEscapedWord := true
					else
						if sWord:Length == 0
							lEscapedWord := false
						end if
					end if
					if cOldChar == ';' .and. eLexer == LexerStep.None .and. (.not. state:lDirective .or. eStep == ParseStep.AfterUsing)
						lNewCommandInLine := false
						lContinueNextLine := true
						if nChar < nLineLen
							for n := nChar upto nLineLen - 1
								cTestChar := cLine[n]
								if cTestChar != ' ' .and. cTestChar != '\t'
									if cTestChar != '/'  // non-commented code follows
										lNewCommandInLine := true
										lContinueNextLine := false
									end if
									exit
								end if
							next
						endif
						if lNewCommandInLine
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
							nEntityStartCol := 0
							oStatementLine := oLine:AddSubLine(nChar)
							cOldChar := ' '
						end if
					endif
					
					if nBracketCount == 0 .and. .not. lFindingType
						if cOldChar == '{' .or. cOldChar == '['
							
							// indexed PROPERTY
							if .not. (cOldChar == '[' .and. state:lEntityFound .and. oInfo != null .and. oInfo:eType == EntityType._Property)
								switch cOldChar
									case '{'
										cBracketOpen := '{'
										cBracketClose := '}'
									case '['
										cBracketOpen := '['
										cBracketClose := ']'
									//							lInAttribute := TRUE
								end switch
								nBracketCount := 1
							end if
						end if
					end if
					
					
					if nChar == nLineLen
						cChar := ' '
					else
						cChar := cLine[nChar]
					endif
					cRealChar := cChar
					nChar ++
					
					if state:lFirstChar
						if cOldChar != ' ' .and. cOldChar != '\t'
							state:lFirstChar := false
						end if
					end if
					
					lBeforeLexerChange := false
					lMustLoop := false
					eWordStatus := WordStatus.Text
					eWordSubStatus := WordSubStatus.Text
					eCharStatus := WordStatus.Text
					eCharSubStatus := WordSubStatus.Text
					do case
						case state:lDirective .and. eLexer == LexerStep.None .and. .not. (cRealChar == '\'' .or. cRealChar == '"' .or. (cRealChar == '*' .and. cOldChar == '/') .or. (cRealChar == '/' .and. cOldChar == '/') )
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.Text
							if cChar == '/' .and. nChar < cLine:Length .and. cLine[nChar] == '*'
								eCharStatus := WordStatus.Comment
								eCharSubStatus := WordSubStatus.CommentBlock
							elseif cChar == '/' .and. nChar < cLine:Length .and. cLine[nChar] == '/'
								eCharStatus := WordStatus.Comment
								eCharSubStatus := WordSubStatus.CommentLine
							else
								eCharStatus := WordStatus.Text
								eCharSubStatus := WordSubStatus.Text
							end if
						case eLexer == LexerStep.Comment
							eWordStatus := WordStatus.Comment
							eWordSubStatus := WordSubStatus.CommentLine
							eCharStatus := WordStatus.Comment
							eCharSubStatus := WordSubStatus.CommentLine
						case eLexer == LexerStep.BlockComment
							if cChar == '/' .and. cOldChar == '*'
								eLexer := LexerStep.None
								cChar := ' ' // if next char is '*', we shouldn't go to BlockComment mode again
							endif
							eWordStatus := WordStatus.Comment
							eWordSubStatus := WordSubStatus.CommentBlock
							eCharStatus := WordStatus.Comment
							eCharSubStatus := WordSubStatus.CommentBlock
							lMustLoop := true
						case cChar == '"' .and. eLexer != LexerStep.Quote .and. .not. (eLexer == LexerStep.DoubleQuote .and. lEscapedString .and. lEscapedChar)
							if eLexer == LexerStep.DoubleQuote
								eLexer := LexerStep.None
								eWordStatus := WordStatus.Literal
								eWordSubStatus := WordSubStatus.LiteralString
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralString
								lMustLoop := true
							else
								eLexer := LexerStep.DoubleQuote
								lEscapedChar := false
								lEscapedString := cOldChar == 'e' .and. sWord:Length == 1
								if .not. lEscapedString
									lEscapedString := (cOldChar == 'e' .or. cOldChar == 'i') .and. sWord:Length == 2 .and. (sWord:ToString() == "ei" .or. sWord:ToString() == "ie")
								end if
								if lEscapedString
									eWordStatus := WordStatus.Literal
									eWordSubStatus := WordSubStatus.LiteralString
								else
									eWordStatus := WordStatus.Text
									eWordSubStatus := WordSubStatus.Text
									if cOldChar == 'i' .and. sWord:Length == 1
										eWordStatus := WordStatus.Literal
										eWordSubStatus := WordSubStatus.LiteralString
									end if
								end if
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralString
								lBeforeLexerChange := true
								cChar := ' '
							end if
						case cChar == '\'' .and. eLexer != LexerStep.DoubleQuote .and. .not. (eLexer == LexerStep.Quote .and. cOldChar == '\\' .and. lEscapedChar)
							if eLexer == LexerStep.Quote
								eLexer := LexerStep.None
								eWordStatus := WordStatus.Literal
								eWordSubStatus := WordSubStatus.LiteralChar
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralChar
								lMustLoop := true
							else
								eLexer := LexerStep.Quote
								eWordStatus := WordStatus.Text
								eWordSubStatus := WordSubStatus.Text
								eCharStatus := WordStatus.Literal
								eCharSubStatus := WordSubStatus.LiteralChar
								lBeforeLexerChange := true
								cChar := ' '
							end if
						case eLexer == LexerStep.DoubleQuote
							eWordStatus := WordStatus.Literal
							eWordSubStatus := WordSubStatus.LiteralString
							eCharStatus := WordStatus.Literal
							eCharSubStatus := WordSubStatus.LiteralString
							if lEscapedChar
								lEscapedChar := false
							else
								lEscapedChar := lEscapedString .and. cChar == '\\'
							end if
							lMustLoop := true
						case eLexer == LexerStep.Quote
							eWordStatus := WordStatus.Literal
							eWordSubStatus := WordSubStatus.LiteralChar
							eCharStatus := WordStatus.Literal
							eCharSubStatus := WordSubStatus.LiteralChar
							if lEscapedChar
								lEscapedChar := false
							else
								lEscapedChar := cChar == '\\'
							end if
							lMustLoop := true
						case (cChar == '/' .and. cOldChar == '/') .or. (state:lFirstChar .and. cChar = '*')
							eLexer := LexerStep.Comment
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.Text
							eCharStatus := WordStatus.Comment
							eCharSubStatus := WordSubStatus.CommentLine
							if .not. lContinueNextLine
								state:lIgnore := true
							end if
						case cChar == '*' .and. cOldChar == '/'
							eLexer := LexerStep.BlockComment
							cChar := ' ' // if next char is '/', we shouldn't go out of BlockComment mode again
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.Text
							eCharStatus := WordStatus.Comment
							eCharSubStatus := WordSubStatus.CommentBlock
							lMustLoop := true
						case cChar == '/'
							if nChar < nLineLen .and. (cLine[nChar] == '/' .or. cLine[nChar] == '*') // about to go into comment mode
								lBeforeLexerChange := true
								if cLine[nChar] == '/'
									eCharStatus := WordStatus.Comment
									eCharSubStatus := WordSubStatus.CommentLine
								else
									state:lDirective := false
									eCharStatus := WordStatus.Comment
									eCharSubStatus := WordSubStatus.CommentBlock
								end if
								eWordStatus := WordStatus.Text
								eWordSubStatus := WordSubStatus.Text
							end if
					end case
					
					if nBracketCount == 0
						lInAttribute := lAllowAttribute .and. cRealChar == '[' .and. sWord:Length == 0
					else
						if cChar == cBracketOpen
							nBracketCount ++
						elseif cChar == cBracketClose
							nBracketCount --
						end if
					endif
					
					#warning need to check this
					/*IF lMustLoop
					LOOP
					END IF*/
					
					lIsSpaceChar := cChar == ' ' .or. cChar == '\t'
					if .not. lIsSpaceChar
						lAllowAttribute := cRealChar == '(' .or. cRealChar == ',' .or. (state:lFirstWord .and. (cRealChar == ']' .or. cRealChar == ';') )
					end if
					if .not. lIsSpaceChar .and. nEntityStartLine == 0
						nEntityStartLine := nLine
						nEntityStartCol := nChar - 1
					end if
					if lIsSpaceChar
						if sWord:Length == 0
							if .not. (cRealChar == ' ' .or. cRealChar == '\t') 
								loop
							end if
						end if
						lIsBreakChar := false
					else
						lIsBreakChar := hBrk:ContainsKey(cChar)
						if lIsBreakChar .and. state:lEntityFound
							if oInfo != null .and. oInfo:eType == EntityType._Operator
								if System.Array.IndexOf(aOperators , cRealChar) != -1
									sWord:Append(cRealChar)
									loop
								end if
							end if
						end if
					end if
					
					if eLexer == LexerStep.None
						if cRealChar == '#'
							eLexer := LexerStep.Sharp
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.Text
							eCharStatus := WordStatus.Literal
							eCharSubStatus := WordSubStatus.LiteralSymbol
						end if
					elseif eLexer == LexerStep.Sharp
						if lIsBreakChar .or. lIsSpaceChar
							eLexer := LexerStep.None
						endif
						eWordStatus := WordStatus.Literal
						eWordSubStatus := WordSubStatus.LiteralSymbol
						eCharStatus := WordStatus.Text
						eCharSubStatus := WordSubStatus.Text
					endif
					
					if cChar == '=' .and. cOldChar == ':'
						nAfterColonEquals := nChar
					end if
					
					if .not. (lIsBreakChar .or. lIsSpaceChar)
						sWord:Append(cRealChar)
						loop
					endif
					// End of lexing
					
					
					// Parsing
					if sWord:Length == 0
						cWord := ""
						cUpperWord := ""
					else
						cWord := sWord:ToString()
						cUpperWord := cWord:ToUpper()
					end if
					
					if eWordSubStatus == WordSubStatus.LiteralSymbol .and. sWord:Length != 0
						if state:lFirstWord .and. oDirectives:ContainsKey(cUpperWord)
							eWordStatus := WordStatus.Text
							eWordSubStatus := WordSubStatus.TextDirective
							state:lDirective := true
							state:lIgnore := true
							switch cUpperWord
								case "REGION"
									oStatementLine:eType := LineType.RegionIn
								case "ENDREGION"
									oStatementLine:eType := LineType.RegionOut
								case "IFDEF" 
								case "IFNDEF" 
								case "ELSE"
									oStatementLine:eType := LineType.IfdefIn
								case "ENDIF"
									oStatementLine:eType := LineType.IfdefOut
								case "DEFINE"
									oStatementLine:eType := LineType.Define
								case "INCLUDE"
									oStatementLine:eType := LineType.Include
									eStep := ParseStep.AfterInclude
									oStatementLine:cArgument := ""
								case "USING"
									oStatementLine:eType := LineType.Using
									eStep := ParseStep.AfterUsing
									oStatementLine:cArgument := ""
								otherwise
									oStatementLine:eType := LineType.OtherDirective
							end switch
						else
							eWordStatus := WordStatus.Literal
							eWordSubStatus := WordSubStatus.LiteralSymbol
						endif
					end if
					
					
					local lAllowEntityParse as logic
					lAllowEntityParse := .not. state:lIgnore
					
					do case
						case eLexer == LexerStep.BlockComment
							sWord:Length := 0
							lEscapedWord := false
							loop
						
						case eStep == ParseStep.WaitImplements .and. .not. (cUpperWord == "IMPLEMENTS" .and. .not. lEscapedWord)
							eStep := ParseStep.None
							state:lIgnore := true
						case eStep == ParseStep.WaitCommaImplements
							if cWord:Trim():Length == 0
								if cRealChar == ','
									eStep := ParseStep.AfterImplements
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								else
									eStep := ParseStep.None
									state:lIgnore := true
								endif
							else
								eStep := ParseStep.None
								state:lIgnore := true
							end if
						
						case state:lLocal .and. state:lImpliedLocal .and. .not. lEscapedWord .and. cUpperWord == "IN"
							nAfterColonEquals := -nChar // FOREACH
						
						case state:lLocal .and. state:lImpliedLocal .and. nAfterColonEquals != 0
							// LOCAL IMPLIED type recognition code here
							state:lLocal := false
							state:lIgnore := true
						
						case eStep == ParseStep.AfterUsing
							if eWordStatus == WordStatus.Text .and. .not. eWordSubStatus == WordSubStatus.TextDirective
								oStatementLine:cArgument += cWord
							end if
							if .not. lIsSpaceChar .and. cRealChar != ';' .and. eCharSubStatus == WordSubStatus.Text
								oStatementLine:cArgument += cRealChar:ToString()
							elseif cRealChar == '.' .and. eCharSubStatus == WordSubStatus.TextOperator
								oStatementLine:cArgument += cRealChar:ToString()
							end if
						
						case eStep == ParseStep.AfterInclude
							if eWordSubStatus == WordSubStatus.LiteralString
								oStatementLine:cArgument += cWord
							end if
							if eCharSubStatus == WordSubStatus.LiteralString .and. cRealChar != '"'
								oStatementLine:cArgument += cRealChar:ToString()
							end if
						
						case eWordStatus == WordStatus.Literal .or. eWordStatus == WordStatus.Comment .or. eWordSubStatus == WordSubStatus.TextDirective
						
						case (lIsSpaceChar .or. cChar == ';') .and. sWord:Length == 0
							lEscapedWord := false
							loop
						case (state:lFirstChar .and. eWordStatus == WordStatus.Text .and. eCharStatus == WordStatus.Comment) .and. sWord:Length == 0
							lEscapedWord := false
							loop
						case lInAttribute
							sWord:Length := 0
							lEscapedWord := false
							loop
						case state:lFirstWord .and. cUpperWord == "RETURN" .and. .not. lEscapedWord
							oStatementLine:eType := LineType.Return
							oStatementLine:cArgument := null
							state:lIgnore := true
						case lAllowEntityParse .and. cChar == ',' .and. (state:lField .or. state:lLocal) .and. state:lNameFound .and. eStep != ParseStep.AfterAs .and. eStep != ParseStep.AfterRef
							if nBracketCount == 0
								state:lNameFound := false
								state:lEntityFound := false
							end if
						case lAllowEntityParse .and. .not. state:lFirstWord .and. sWord:Length == 0 .and. .not. state:lInParams .and. .not. lFindingType .and. .not. lFindingName
							if cChar == '('
								if state:lNameFound .and. oInfo:aParams == null
									state:lInParams := true
								end if
							end if
						case lAllowEntityParse .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. ;
							((hVis:ContainsKey(cUpperWord) .and. eStep != ParseStep.AfterBegin) .or. cUpperWord == "EVENT" .or. ;
							((cUpperWord == "LOCAL" .or. cUpperWord == "CATCH" .or. cUpperWord == "FOREACH")) )
							// Checking eStep beause UNSAFE could be UNSAFE FUNCTION or BEGIN UNSAFE
							state:lVisFound := true
							switch cUpperWord
								case "PROTECT" 
								case "PROTECTED" 
								case "INSTANCE"
									eAccessLevel := AccessLevel.Protected
									eModifiers := eModifiers | EntityModifiers._Protected
								case "HIDDEN" 
								case "PRIVATE"
									eAccessLevel := AccessLevel.Hidden
									eModifiers := eModifiers | EntityModifiers._Private
								case  "INTERNAL"
									eAccessLevel := AccessLevel.Internal
									eModifiers := eModifiers | EntityModifiers._Internal
								case "PUBLIC"
									eAccessLevel := AccessLevel.Public
								case "STATIC"
									lStatic := true
									eModifiers := eModifiers | EntityModifiers._Static
								case "CONST"
									lStatic := true
								case "PARTIAL"
									eModifiers := eModifiers | EntityModifiers._Partial
									lPartial := true
								case "VIRTUAL"
									eModifiers := eModifiers | EntityModifiers._Virtual
								case "NEW"
									eModifiers := eModifiers | EntityModifiers._New
								case "SEALED"
									eModifiers := eModifiers | EntityModifiers._Sealed
								case "ABSTRACT"
									eModifiers := eModifiers | EntityModifiers._Abstract
							end switch
							if System.Array.IndexOf(aEntityWords , cUpperWord) != -1
								// Allow multiple names in same line
								state:lField := true
								if cUpperWord == "EVENT"
									state:lEvent := true
								end if
							end if
							if cUpperWord == "LOCAL" .or. cUpperWord == "CATCH" .or. cUpperWord == "FOREACH"
								state:lLocal := true
								state:lImpliedLocal := false
							end if
						case lAllowEntityParse .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. (cUpperWord == "VAR" .and. state:lFirstWord)
							state:lVisFound := true
							if cUpperWord == "VAR"
								state:lLocal := true
								state:lImpliedLocal := true
								nAfterColonEquals := 0
							end if
						case lAllowEntityParse .and. .not. lEscapedWord .and. state:lFirstWord .and. cUpperWord == "USING"
							oStatementLine:eType := LineType.Using
							eStep := ParseStep.AfterUsing
							oStatementLine:cArgument := ""
						case state:lVisFound .and. lInProperty .and. (cUpperWord == "SET" .or. cUpperWord == "GET") .and. .not. lEscapedWord
							oStatementLine:eType := LineType.TokenIn
							oStatementLine:cArgument := cUpperWord
							state:lIgnore := true
						case lAllowEntityParse .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. hEnt:ContainsKey(cUpperWord)
							lInEnum := false
							state:lField := false
							state:lLocal := false
							state:lEvent := false
							if state:lEntityFound
								state:lIgnore := true
							endif
							state:lEntityFound := true
							state:lEntityIsClass := System.Array.IndexOf(<string>{"CLASS","STRUCTURE","STRUCT","INTERFACE","DELEGATE","ENUM","VOSTRUCT","UNION"} , cUpperWord) != -1
							if eStep == ParseStep.AfterEnd .and. state:lEntityIsClass
								oStatementLine:eType := LineType.EndClass
								state:lEntityFound := false
								state:lEntityIsClass := false
								state:lIgnore := true
								lInEnum := false
								cShortClassName := ""
								cTypedClassName := ""
								cClassNameSpace := ""
								cClassType := ""
								lInProperty := false
							elseif eStep == ParseStep.AfterEnd .and. cUpperWord == "PROPERTY"
								oStatementLine:eType := LineType.EndProperty
								state:lEntityFound := false
								state:lEntityIsClass := false
								state:lIgnore := true
								lInEnum := false
								lInProperty := false
							else
								lInEnum := cUpperWord == "ENUM"
								oInfo := EntityObject{}
								oInfo:SetNamespaces(aNameSpaces)
								oInfo:eType := GetEntityType(cUpperWord)
								lInProperty := oInfo:eType == EntityType._Property
								if state:lEntityIsClass
									cClassType := cUpperWord
									switch cUpperWord
										case "CLASS"
											oInfo:cInherit := ""
											oInfo:cImplements := ""
										case "STRUCTURE" 
										case "STRUCT"
											oInfo:cInherit := ""
											oInfo:cImplements := ""
										case "DELEGATE"
											oInfo:cInherit := ""
											oInfo:cImplements := ""
											oInfo:cRetType := "" // Default
										case "ENUM"
											oInfo:cInherit := ""
											oInfo:cImplements := ""
											oInfo:cRetType := ""
											cEnumType := "INT"
										otherwise
											oInfo:cInherit := ""
											oInfo:cImplements := ""
									end switch
								else
									if oInfo:eType == EntityType._Method .or. oInfo:eType == EntityType._Access .or. oInfo:eType == EntityType._Property .or. ;
										oInfo:eType == EntityType._Function .or. oInfo:eType == EntityType._Global
										oInfo:cRetType := "USUAL" // Default
									else
										oInfo:cRetType := ""
									end if
								end if
								oInfo:lStatic := lStatic
								oInfo:eAccessLevel := eAccessLevel
								oInfo:eModifiers := eModifiers
								if oInfo:eType == EntityType._Constructor .or. oInfo:eType == EntityType._Destructor
									state:lNameFound := true // Dont't wait for a name, add it to the list now
									oInfo:nLine := nEntityStartLine
									oInfo:nCol := nEntityStartCol
									oInfo:cName := iif(oInfo:eType == EntityType._Constructor , ".ctor" , ".dtor")
									oInfo:cShortClassName := cShortClassName
									oInfo:cTypedClassName := cTypedClassName
									oInfo:cClassNameSpace := cClassNameSpace
									oInfo:cClassType := cClassType
									aEntities:Add(oInfo)
									if cChar == '('
										state:lInParams := true
									end if
								end if
							end if
						case lAllowEntityParse .and. (state:lEntityFound .or. eStep == ParseStep.AfterBeginNamespace .or. eStep == ParseStep.WaitImplements)
							do case
								case cUpperWord == "INHERIT" .and. .not. lEscapedWord
									eStep := ParseStep.AfterInherit
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								case cUpperWord == "IMPLEMENTS" .and. .not. lEscapedWord
									eStep := ParseStep.AfterImplements
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								case cUpperWord == "AS" .and. .not. lEscapedWord
									eStep := ParseStep.AfterAs
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								case state:lInParams .and. (cUpperWord == "REF" .or. cUpperWord == "OUT") .and. .not. lEscapedWord
									eStep := ParseStep.AfterRef
									sFoundType:Length := 0
									n1 := 0;n2 := 0
								case eStep == ParseStep.AfterInherit .or. eStep == ParseStep.AfterImplements .or. ;
									eStep == ParseStep.AfterAs .or. eStep == ParseStep.AfterRef .or. ;
									eStep == ParseStep.AfterBeginNamespace .or. ;
									.not. state:lNameFound
									
									if eStep == ParseStep.AfterInherit .or. eStep == ParseStep.AfterImplements .or. eStep == ParseStep.AfterAs .or. eStep == ParseStep.AfterRef
										// Waiting for type that may be generic, array
										lFindingType := true
										sFoundType:Append(sWord:ToString())
										sWord:Length := 0
										
										do while nChar < nLineLen .and. (cChar == ' ' .or. cChar == '\t')
											cOldOldChar := cOldChar
											cOldChar := cChar
											cChar := cLine[nChar]
											if cChar != ' ' .and. cChar != '\t' .and. cChar != '[' .and. cChar != '<'
												cChar := ' '
												exit
											end if
											nChar ++
										end do
										
										switch cChar
											case '.'
												sFoundType:Append('.')
											case '['
												sFoundType:Append('[')
												n1 ++
											case ']'
												if n1 > 0
													sFoundType:Append(']')
													n1 --
												end if
											case '<'
												sFoundType:Append('<')
												n2 ++
											case '>'
												if n2 > 0
													sFoundType:Append('>')
													n2 --
												end if
											case ' '
											case '\t'
											case ';'
												nop
											otherwise
												if (n1 != 0 .or. n2 != 0) 
													sFoundType:Append(cChar)
												endif
										end switch
										if cChar == '.' .or. n1 != 0 .or. n2 != 0 .or. (cChar == ']' .and. nChar < nLineLen .and. cLine[nChar] == '[') // more dims
											loop
										end if
										cWord := sFoundType:ToString()
										sFoundType:Length := 0
										
										lFindingType := false
										
									else // eStep == ParseStep.AfterBeginNamespace .or. .not. state:lNameFound
										
										// Waiting for simple type or method name that may contain dots
										sFoundType:Append(sWord:ToString())
										if cChar == '.'
											sFoundType:Append('.')
											sWord:Length := 0
											lFindingName := true
											loop
										end if
										cWord := sFoundType:ToString()
										sFoundType:Length := 0
										lFindingName := false
										
									end if
									
									if state:lEntityIsClass .and. .not. state:lNameFound
										cTypedClassName := cWord
										nAt := cWord:LastIndexOf('.')
										if nAt <= 0 .or. nAt >= cWord:Length - 1
											cShortClassName := cWord
											cClassNameSpace := ""
											//								cClassType := cWord
										else
											cShortClassName := cWord:Substring(nAt + 1)
											cClassNameSpace := cWord:Substring(0 , nAt)
											//								cClassType := cWord:Substring(nAt + 1)
										end if
										if .not. String.IsNullOrEmpty(cBaseNameSpace)
											if cClassNameSpace == ""
												cClassNameSpace := cBaseNameSpace
											else
												cClassNameSpace := cBaseNameSpace + "." + cClassNameSpace
											end if
										end if
									endif
									
									do case
										case state:lInParams
											if eStep == ParseStep.AfterRef
												oInfo:SetParamType("&" + cWord)
											else
												oInfo:SetParamType(cWord)
											endif
											eStep := ParseStep.None
										case eStep == ParseStep.AfterInherit
											oInfo:cInherit := cWord
											eStep := ParseStep.WaitImplements
										//							state:lIgnore := TRUE
										case eStep == ParseStep.AfterImplements
											if oInfo:cImplements:Trim():Length == 0
												oInfo:cImplements := cWord
											else
												oInfo:cImplements += ", " + cWord
											end if
											switch cRealChar
												case ','
													eStep := ParseStep.AfterImplements
													sFoundType:Length := 0
													n1 := 0;n2 := 0
												case ' ' 
												case '\t'
													eStep := ParseStep.WaitCommaImplements
												otherwise
													eStep := ParseStep.None
													state:lIgnore := true
											end switch
										case eStep == ParseStep.AfterAs
											local cRetType as string
											if state:lDimDeclaration
												cRetType := cWord + "[]"
											else
												cRetType := cWord
											end if
											oInfo:cRetType := cRetType
											if lInEnum
												cEnumType := cWord
											elseif state:lField
												for n := 0 upto aLineFields:Count - 1
													((EntityObject)aLineFields[n]):cRetType := cRetType
												next
											elseif state:lLocal
												for n := 0 upto aLineLocals:Count - 1
													((EntityObject)aLineLocals[n]):cRetType := cRetType
												next
											end if
											eStep := ParseStep.None
											state:lIgnore := true
										case eStep == ParseStep.AfterBeginNamespace
											eStep := ParseStep.None
											aNameSpaces:Add(cWord)
											cBaseNameSpace := GetNameSpace(aNameSpaces)
											oStatementLine:eType := LineType.BeginNamespace
											//							oStatementLine:cBeginNamespace := cWord
											oStatementLine:cArgument := cWord
											state:lIgnore := true
										case .not. state:lNameFound
											state:lNameFound := true
											oInfo:nLine := nEntityStartLine
											oInfo:nCol := nEntityStartCol
											oInfo:cName := cWord
											if oInfo:IsFuncProcGlobal
												oInfo:cShortClassName := ""
												oInfo:cTypedClassName := ""
												oInfo:cClassNameSpace := ""
												oInfo:cClassType := ""
											else
												oInfo:cShortClassName := cShortClassName
												oInfo:cTypedClassName := cTypedClassName
												oInfo:cClassNameSpace := cClassNameSpace
												oInfo:cClassType := cClassType
											end if
											if oInfo:eType == EntityType._Class .and. lPartial
												oInfo:lPartial := true
											end if
											aEntities:Add(oInfo)
											lPartial := false
									end case
								
							end case
							if state:lInParams
								if cChar == ','
									if .not. state:lParam
										oInfo:AddParam(cWord)
									end if
									state:lParam := false
								elseif .not. state:lParam .and. sWord:Length != 0
									if .not. cWord == "SELF"
										oInfo:AddParam(cWord)
										state:lParam := true
									end if
								end if
							endif
							
							if cChar == '('
								state:lInParams := true
							elseif cChar == '[' .and. oInfo != null .and. oInfo:eType == EntityType._Property .and. .not. state:lInParams
								state:lInParams := true
							elseif state:lInParams .and. cChar == ')'
								state:lInParams := false
							elseif state:lInParams .and. cChar == ']' .and. oInfo:eType == EntityType._Property
								state:lInParams := false
							end if
						//					lIgnore := TRUE
						case state:lFirstWord .and. cUpperWord == "BEGIN"
							lInEnum := false
							eStep := ParseStep.AfterBegin
						case eStep == ParseStep.AfterBegin
							lInEnum := false
							if cUpperWord == "NAMESPACE"
								eStep := ParseStep.AfterBeginNamespace
							else
								switch cUpperWord
									case "LOCK"
										oStatementLine:eType := LineType.TokenIn
										oStatementLine:cArgument := "BEGIN"
									case "SEQUENCE"
										oStatementLine:eType := LineType.TokenIn
										oStatementLine:cArgument := "BEGIN"
									case "SCOPE"
										oStatementLine:eType := LineType.TokenIn
										oStatementLine:cArgument := "BEGIN"
									case "CHECKED" 
									case  "UNCHECKED" 
									case "UNSAFE" 
									case "USING" 
									case "FIXED"
										oStatementLine:eType := LineType.TokenIn
										oStatementLine:cArgument := "BEGIN"
								end switch
							end if
						case state:lFirstWord .and. cUpperWord == "END"
							lInEnum := false
							eStep := ParseStep.AfterEnd
							oStatementLine:eType := LineType.TokenOut
							oStatementLine:cArgument := "END"
						case eStep == ParseStep.AfterEnd
							state:lIgnore := true
							lInEnum := false
							if System.Array.IndexOf(aEndKeywords , cUpperWord) != -1
								oStatementLine:eType := LineType.EndClass
								cShortClassName := ""
								cTypedClassName := ""
								cClassNameSpace := ""
								cClassType := ""
							elseif cUpperWord == "NAMESPACE"
								oStatementLine:eType := LineType.EndNamespace
								if lAllowEntityParse
									if aNameSpaces:Count != 0
										aNameSpaces:RemoveAt(aNameSpaces:Count - 1)
										cBaseNameSpace := GetNameSpace(aNameSpaces)
									end if
								end if
							endif
						
						case state:lFirstWord .and. cUpperWord == "USING"
							lInEnum := false
							eStep := ParseStep.AfterUsing
							oStatementLine:eType := LineType.Using
						
						case lAllowEntityParse .and. ;  // 2nd .not. is for IF(logic) CASE(something) etc syntax (paren after IF/CASE etc)
							.not. (.not. lEscapedWord .and. cRealChar == '(' .and. System.Array.IndexOf(<string>{"IF", "ELSEIF", "WHILE", "CASE", "FOR"} , cUpperWord) != -1) .and. ;
							.not. (lIsSpaceChar .or. cRealChar == ';' .or. lBeforeLexerChange .or. lContinueNextLine) .and. .not. (state:lField .or. state:lLocal)
							state:lIgnore := true
						case state:lLocal .and. .not. lEscapedWord .and. cUpperWord == "IMPLIED"
							state:lImpliedLocal := true
							nAfterColonEquals := 0
						case lAllowEntityParse .and. (state:lField .or. state:lLocal .or. lInEnum) .and. .not. state:lNameFound .and. cWord == "DIM" .and. .not. lEscapedWord
							state:lDimDeclaration := true
						case lAllowEntityParse .and. (state:lField .or. state:lLocal .or. lInEnum) .and. .not. state:lNameFound
							state:lNameFound := true
							state:lEntityFound := true
							oInfo := EntityObject{}
							oInfo:SetNamespaces(aNameSpaces)
							if state:lLocal
								oInfo:eType := EntityType._Local
							elseif state:lEvent
								oInfo:eType := EntityType._Event
							else
								oInfo:eType := EntityType._Field
							end if
							oInfo:lStatic := lStatic
							oInfo:eAccessLevel := eAccessLevel
							oInfo:eModifiers := eModifiers
							oInfo:cRetType := "USUAL"
							if state:lDimDeclaration
								oInfo:cRetType := "USUAL[]"
							end if
							if lInEnum
								oInfo:cRetType := cEnumType
							else
								if state:lLocal
									aLineLocals:Add(oInfo)
								else
									aLineFields:Add(oInfo)
								end if
								if cChar == ','
									state:lNameFound := false
									state:lEntityFound := false
								end if
							endif
							oInfo:nLine := nLine
							oInfo:cName := cWord
							oInfo:cShortClassName := cShortClassName
							oInfo:cTypedClassName := cTypedClassName
							oInfo:cClassNameSpace := cClassNameSpace
							oInfo:cClassType := cClassType
							if state:lField
								aEntities:Add(oInfo)
							end if
							if state:lLocal
								aLocals:Add(oInfo)
							end if
						case state:lFirstWord .or. state:lFirstChar
							if System.Array.IndexOf(aTokenIn, cUpperWord) != -1
								oStatementLine:eType := LineType.TokenIn
								oStatementLine:cArgument := cUpperWord
							elseif System.Array.IndexOf(aTokenOut , cUpperWord) != -1
								oStatementLine:eType := LineType.TokenOut
								oStatementLine:cArgument := cUpperWord
							elseif cUpperWord == "GET" .or. cUpperWord == "SET"
								if lInProperty
									oStatementLine:eType := LineType.TokenIn
									oStatementLine:cArgument := cUpperWord
								endif
							else
								if .not. lContinueNextLine
									if .not. cUpperWord == "FOR" // Allow LOCAL after FOR
										state:lIgnore := true
									endif
								end if
							end if
						
					end case
					
					if cUpperWord == "FROM"
						state:lLinqSelect := true
					endif
					
					
					if (cChar != '#' .or. sWord:Length != 0) .and. .not. lInAttribute
						state:lFirstWord := false
					end if
					sWord:Length := 0
					lEscapedWord := false
					// End of parsing
					
				end do
			next
			
			
			return nLine
		
		
		protected static method GetNameSpace(aNameSpaces as List<string>) as string
			local oEnumerator as IEnumerator
			local cRet as string
			cRet := ""
			if aNameSpaces == null
				return cRet
			end if
			oEnumerator := aNameSpaces:GetEnumerator()
			do while oEnumerator:MoveNext()
				if cRet == String.Empty
					cRet := (string)oEnumerator:Current
				else
					cRet := (string)oEnumerator:Current + "." + cRet
				end if
			end do
			return cRet
		
		protected static method GetEntityType(cWord as string) as EntityType
			local eType as EntityType
			switch cWord
				case "METHOD"
					eType := EntityType._Method
				case "CONSTRUCTOR"
					eType := EntityType._Constructor
				case "CLASS"
					eType := EntityType._Class
				case "DESTRUCTOR"
					eType := EntityType._Destructor
				case "PROPERTY"
					eType := EntityType._Property
				case "ACCESS"
					eType := EntityType._Access
				case "ASSIGN"
					eType := EntityType._Assign
				case "FUNCTION" 
				case "FUNC"
					eType := EntityType._Function
				case "PROCEDURE" 
				case "PROC"
					eType := EntityType._Procedure
				case  "ENUM"
					eType := EntityType._Enum
				case "STRUCTURE" 
				case "STRUCT"
					eType := EntityType._Structure
				case  "VOSTRUCT"
					eType := EntityType._VOStruct
				case  "UNION"
					eType := EntityType._Union
				case  "GLOBAL"
					eType := EntityType._Global
				case  "DELEGATE"
					eType := EntityType._Delegate
				case  "EVENT"
					eType := EntityType._Event
				case  "INTERFACE"
					eType := EntityType._Interface
				case  "OPERATOR"
					eType := EntityType._Operator
				case  "DEFINE"
					eType := EntityType._Define
			end switch
			return eType
		
	end class
	
	
	internal class LineObject
		protect _nLine as int
		protect _nCol as int
		
		protect _eType as LineType
		protect _cArgument as string
		
		protect _aLinesWithSpecialStuff as List<LineObject>
		
		internal constructor(nLine as int)
			self(nLine , 0)
			return

		internal constructor(nLine as int, nCol as int)
			self:_nLine := nLine
			self:_nCol := nCol
			self:_aLinesWithSpecialStuff := List<LineObject>{}
			return
		
		
		internal method AddSubLine(nColStart as int) as LineObject
			return LineObject{self:_nLine , nColStart}
		
		internal property LinesWithSpecialStuff as List<LineObject> get _aLinesWithSpecialStuff
		
		internal property Line as int get self:_nLine
		internal property eType as LineType
			get
				return self:_eType
			end get
			set
				self:_eType := VALUE
				_aLinesWithSpecialStuff:Add(self)
			end set
		end property

		internal property cArgument as string get self:_cArgument set self:_cArgument := value
		
	end class
	
	class EntityObject
		property eType as EntityType auto
		property cName as string auto
		property cInherit as string auto
		property cRetType as string auto
		property cImplements as string auto
		property eModifiers as EntityModifiers auto
		property eAccessLevel as AccessLevel auto
		property cShortClassName as string auto
		property cTypedClassName as string auto
		property cClassNamespace as string auto
		property aParams as List<EntityParamsObject> auto
		property nLine as int  auto
		property nCol as int  auto
		property aNameSpaces as List<string> auto // prosoxh, kai se functions etc
		property lStatic as logic auto
		property lPartial as logic auto
		property cClassType as string auto
		property lExtension as logic auto
		
		internal constructor()
			super()
			self:cInherit := ""
			self:cImplements := ""
			self:cRetType := ""
			self:cClassType := ""
			return

		internal access IsFuncProcGlobal as logic
			return self:eType == EntityType._Function .or. self:eType == EntityType._Procedure .or. self:eType == EntityType._Global
		
		internal method NamespacesEqual(_aNameSpaces as List<string>) as logic
			local n as int
			if self:aNameSpaces == null .or. self:aNameSpaces:Count != _aNameSpaces:Count
				return false
			end if
			for n := 0 upto self:aNameSpaces:Count - 1
				if self:aNameSpaces[n] != _aNameSpaces[n]
					return false
				end if
			next
			return true
		internal method SetNamespaces(_aNameSpaces as List<string>) as void
			local n as int
			if self:NamespacesEqual(_aNameSpaces)
				return
			end if 
			if self:aNameSpaces == null
				self:aNameSpaces := List<string>{_aNameSpaces:Count}
			end if
			for n := 0 upto _aNameSpaces:Count - 1
				self:aNameSpaces:Add(_aNameSpaces[n])
			next
			return
		internal method AddParam(cParam as string) as void
			self:AddParam(cParam, "USUAL")
			return

		internal method AddParam(cParam as string , cType as string) as void
			if self:aParams == null
				self:aParams := List<EntityParamsObject>{}
			end if
			self:aParams:Add(EntityParamsObject{cParam , cType})
			return

		internal method SetParamType(cType as string) as void
			local oParam as EntityParamsObject
			if self:aParams == null .or. self:aParams:Count == 0 .or. String.IsNullOrEmpty(cType)
				return
			end if
			oParam := self:aParams[self:aParams:Count - 1]
			if cType:Contains("&")
				cType := cType:Replace("&" , "")
				oParam:lReference := true
			end if
			oParam:cType := cType
			return
	end class
	
	class EntityParamsObject
		property cName as string auto
		property cType as string auto
		property lReference as logic auto
		internal constructor(_cName as string , _cType as string)
			super()
			self:cName := _cName
			self:cType := _cType
			return
	end class
	
	internal enum LexerStep
		member None
		member Quote
		member DoubleQuote
		member BracketQuote
		member Comment
		member BlockComment
		member Sharp
	end enum
	
	internal enum ParseStep
		member None
		member AfterAs
		member AfterRef
		member AfterInherit
		member AfterEnd
		member AfterBegin
		member AfterBeginNamespace
		member AfterSharp
		member AfterDefine
		member AfterUsing
		member AfterInclude
		member AfterImplements
		member WaitImplements
		member WaitCommaImplements
		
		// for VO only:
		//	MEMBER WaitClassClause
		member AfterClassClause
		member AfterExportClause
	end enum
	
	[Flags];
		enum   EntityModifiers as Int32
		member _None := 0
		member _Protected := 1
		member _Private := 2
		member _Internal := 4
		member _Virtual := 8
		member _Abstract := 16
		member _Sealed := 32
		member _Static := 64
		member _Partial := 128
		member _New := 256
	end enum
	
	enum EntityType as Int32 
		member _None
		member _Constructor	:= Kind.Constructor
		member _Destructor	:= Kind.Destructor
		member _Method		:= Kind.Method
		member _Access		:= Kind.Access
		member _Assign		:= Kind.Assign
		member _Class		:= Kind.Class
		member _Function	:= Kind.Function
		member _Procedure	:= Kind.Procedure
		member _Enum		:= Kind.Enum
		member _VOStruct	:= Kind.VOStruct
		member _Global		:= Kind.VOGlobal
		member _Structure	:= Kind.Structure
		member _Interface	:= Kind.Interface
		member _Delegate	:= Kind.Delegate
		member _Event		:= Kind.Event
		member @@_Field		:= Kind.Field
		member _Union		:= Kind.Union
		member _Operator	:= Kind.Operator
		member _Local		:= Kind.Local
		member _Property	:= Kind.Property
		member _Define		:= Kind.VODefine
		member _Resource	
		member _TextBlock	
	end enum
	
	
	[Flags];
	enum AccessLevel
		member @@Public 			:= Modifiers.Public
		member @@Protected			:= Modifiers.Protected
		member @@Hidden				:= Modifiers.Hidden
		member @@Internal			:= Modifiers.Internal
	end enum
	
	enum WordStatus
		member Text
		member Literal
		member Comment
	end enum
	
	enum WordSubStatus
		member Text
		member TextReserved
		member TextFunction
		member TextDirective
		member TextOperator
		member LiteralInt
		member LiteralUInt
		member LiteralFloat
		member LiteralDecimal
		member LiteralSingle
		member LiteralDouble
		member LiteralSymbol
		member LiteralString
		member LiteralChar
		member CommentBlock
		member CommentLine
		member CommentRegion
	end enum
	
	[Flags];
	enum	 WordStyle as int
		member None := 0
		member EscapedLiteral := 1
		member InAttribute := 2
	end enum
	
	enum LineType as int
		//ENUM LineType AS BYTE
		member None
		member RegionIn
		member RegionOut
		member TokenIn
		member TokenOut
		member BeginNamespace
		member EndNamespace
		member EndClass
		member @@Using
		member Include
		member @@Define
		member @@Return
		member IfdefIn
		member IfdefOut
		member EndProperty
		//	MEMBER BeginProperty
		member OtherDirective
	end enum
	
	internal structure ParseState				   
		internal property  lVisFound as logic			   auto
		internal property  lEntityFound as logic			   auto
		internal property  lEntityIsClass as logic		   auto
		internal property  lFirstChar as logic			   auto
		internal property  lFirstWord as logic			   auto
		internal property  lInParams as logic			   auto
		internal property  lNameFound as logic			   auto
		internal property  lField as logic				   auto
		internal property  lLocal as logic				   auto
		internal property  lImpliedLocal as logic		   auto
		internal property  lEvent as logic				   auto
		internal property  lParam as logic				   auto
		internal property  lDirective as logic			   auto
		internal property  lIgnore as logic				   auto
		internal property  lExpectName as logic			   auto
		internal property  lLinqSelect as logic			   auto
		internal property  lDimDeclaration as logic		   auto
		method Reset() as void
			lVisFound := false
			lEntityFound := false
			lEntityIsClass := false
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
			return
	end structure
	
end namespace