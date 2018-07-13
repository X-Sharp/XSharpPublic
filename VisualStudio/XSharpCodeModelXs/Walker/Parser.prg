//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Collections.Generic
using System.Collections
using System.Text
using System.Text.RegularExpressions
using System.IO
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
begin namespace XSharpModel
	class Parser
		static protect oEntityMarkers as Dictionary<string , string>
		static protect oEntityVisibility as Dictionary<string , string>
		static protect oDirectives as Dictionary<string , string>
		static protect hBrk as Dictionary<char,char>
		static protect aTokenIn as string[]
		static protect aTokenInOut as string[]
		static protect aTokenOut as string[]
		static protect aEntityWords as string[]
		static protect aOperators as char[]
		static protect aEndKeywords as string[]
		static protect aTypes as string[]
		
		// instance fields
		private aEntities as List<EntityObject> 
		private aLocals   as List<EntityObject> 
		private aResult   as List<EntityObject> 
		private aLinesWithSpecialStuff as List<LineObject> 
		private aLines    as List<LineObject> 
		private aSourceLines    as IList<String>
		PRIVATE aTypeStack AS Stack<EntityObject>
		private oGlobalObject as EntityObject
		PRIVATE _nLength  AS INT
        PRIVATE iClassOrStruct AS INT
				
		constructor()
			self:aEntities				:= List<EntityObject>{}
			self:aLocals				:= List<EntityObject>{}
			self:aLinesWithSpecialStuff := List<LineObject>{}
			self:aLines					:= List<LineObject>{}

			SELF:aTypeStack					:= Stack<EntityObject>{}
			self:oGlobalObject				:= EntityObject{EntityType._Class}
			self:oGlobalObject:cName		:= XELement.GlobalName
			self:oGlobalObject:lStatic		:= true
			self:oGlobalObject:lPartial		:= true
			self:oGlobalObject:nStartLine	:= 1
			SELF:oGlobalObject:nCol			:= 1
            SELF:iClassOrStruct             := 0
		
		
		static constructor()
			oEntityMarkers := Dictionary<string , string>{}
			oEntityVisibility := Dictionary<string , string>{}
			oDirectives := Dictionary<string , string>{}
			hBrk := Dictionary<char,char>{}
			aTokenIn := <string>{"IF", "DO", "WHILE", "FOR", "FOREACH", "TRY", "REPEAT", "SWITCH"} 
			aTokenInOut := <string>{"ELSE", "ELSEIF", "CASE", "OTHERWISE", "CATCH", "FINALLY", "RECOVER" } 
			aTokenOut := <string>{"ENDIF", "ENDDO", "ENDCASE", "NEXT", "ENDTRY", "UNTIL"}
			aEntityWords := <string>{"EVENT" , "PROTECT" , "PROTECTED", "INSTANCE" , "EXPORT" , "PUBLIC" , "PRIVATE" , "HIDDEN" , "INTERNAL" , "MEMBER" , "GLOBAL"} 
			aOperators := <char>{'+','-','*','/','%','&','|','>','<','=','!','~'}
			aEndKeyWords := <string>{"CLASS","STRUCTURE","STRUCT","INTERFACE","ENUM"}
			aTypes := <string>{"CLASS","STRUCTURE","STRUCT","INTERFACE","DELEGATE","ENUM","VOSTRUCT","UNION"}
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
		
		internal method _SetLineType(oLine as LineObject, eLineType as LineType) as void
			oLine:eType := eLineType
			if ! self:aLinesWithSpecialStuff:Contains(oLine)
				self:aLinesWithSpecialStuff:Add(oLine)
			endif
		
		property Entities		as IList<EntityObject>	get aEntities				
		property Types		    as IList<EntityObject>	get aResult
		property Locals		    as IList<EntityObject>	get aLocals
		property SpecialLines	as IList<LineObject>	get aLinesWithSpecialStuff 
		property SourceLength   as int					get _nLength -2 // exclude last CRLF
		property LineCount      as int					get aSourceLines:Count

		method AddEntity(oInfo as EntityObject, oLine as LineObject) as void
			local oLast as EntityObject
			if aEntities:Count > 0
				oLast := aEntities[aEntities:Count-1]
			endif
			aEntities:Add(oInfo)
			if oLast != NULL_OBJECT
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
            return oParent
			


		method Parse(aLineCollection as IList<string>, lIncludeLocals as logic) as ParseResult
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
			LOCAL lInEnum AS LOGIC
			local lFindingType as logic
			local lFindingName as logic
			local sFoundType as StringBuilder
			local lNewCommandInLine as logic
			local lContinueNextLine as logic
			local lBeforeLexerChange as logic
			local cEnumType as string
			local nChar as int
			local oInfo as EntityObject
			local oCurrentMethod as EntityObject
			local lStatic as logic
			local eAccessLevel as AccessLevel
			local eModifiers as EntityModifiers
			local eLexer as LexerStep
			LOCAL eStep AS ParseStep
			local nParamType as ParamType
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
			local nEntityStartOffset as int
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
			aSourceLines := aLineCollection
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
			_nLength := 0
			foreach cLine as string in aLineCollection
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
					nEntityStartOffset := 0
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
							nEntityStartOffset := 0
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
						nEntityStartOffset := oLine:OffSet
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
							oStatementLine:cArgument :=cWord
							switch cUpperWord
								case "REGION"
									_SetLineType(oStatementLine, LineType.RegionIn)
								case "ENDREGION"
									_SetLineType(oStatementLine, LineType.RegionOut)
								case "IFDEF" 
								case "IFNDEF" 
								case "ELSE"
									_SetLineType(oStatementLine, LineType.IfdefIn)
								case "ENDIF"
									_SetLineType(oStatementLine, LineType.IfdefOut)
								case "DEFINE"
									_SetLineType(oStatementLine, LineType.Define)
								case "INCLUDE"
									_SetLineType(oStatementLine, LineType.Include)
									eStep := ParseStep.AfterInclude
									oStatementLine:cArgument := ""
								case "USING"
									_SetLineType(oStatementLine, LineType.Using)
									eStep := ParseStep.AfterUsing
									oStatementLine:cArgument := ""
								otherwise
									_SetLineType(oStatementLine, LineType.OtherDirective)
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
							_SetLineType(oStatementLine, LineType.Return)
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
								case "INTERNAL"
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
							
							    if cUpperWord == "FOREACH"
    								_SetLineType(oStatementLine, LineType.TokenIn)
                                ELSEIF cUpperWord == "CATCH"
                                    _SetLineType(oStatementLine, LineType.TokenInOut)
							    ENDIF
                            end if
						case lAllowEntityParse .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. (cUpperWord == "VAR" .and. state:lFirstWord)
							state:lVisFound := true
							if cUpperWord == "VAR"
								state:lLocal := true
								state:lImpliedLocal := true
								nAfterColonEquals := 0
							end if
						case lAllowEntityParse .and. .not. lEscapedWord .and. state:lFirstWord .and. cUpperWord == "USING"
							_SetLineType(oStatementLine, LineType.Using)
							eStep := ParseStep.AfterUsing
							oStatementLine:cArgument := ""
						case state:lVisFound .and. lInProperty .and. (cUpperWord == "SET" .or. cUpperWord == "GET") .and. .not. lEscapedWord
							_SetLineType(oStatementLine, LineType.TokenIn)
							oStatementLine:cArgument := cUpperWord
							state:lIgnore := true
						case lAllowEntityParse .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. hEnt:ContainsKey(cUpperWord)
							lInEnum := FALSE
							state:lField := false
							state:lLocal := false
							state:lEvent := false
							if state:lEntityFound
								state:lIgnore := true
							endif
							state:lEntityFound := true
							state:lEntityIsType := System.Array.IndexOf(aTypes , cUpperWord) != -1
							if eStep == ParseStep.AfterEnd .and. state:lEntityIsType
								_SetLineType(oStatementLine, LineType.EndClass)
								if aTypeStack:Count > 0
									VAR oPrevInfo := aTypeStack:Pop()
                                    IF ( oPrevInfo:eType:SupportNestedType() )
                                        iClassOrStruct --
                                    ENDIF
								endif
								state:lEntityFound := false
								state:lEntityIsType := false
								state:lIgnore := true
								lInEnum := FALSE
								cShortClassName := ""
								cTypedClassName := ""
								cClassNameSpace := ""
								cClassType := ""
								lInProperty := false
							elseif eStep == ParseStep.AfterEnd .and. cUpperWord == "PROPERTY"
								_SetLineType(oStatementLine, LineType.EndProperty)
								state:lEntityFound := false
								state:lEntityIsType := false
								state:lIgnore := true
								lInEnum := FALSE
								lInProperty := false
							else
								lInEnum := cUpperWord == "ENUM"
								oInfo := EntityObject{GetEntityType(cUpperWord)}
								IF oInfo:eType:IsType()
									IF !oInfo:eType:SupportNestedType() 
										do while aTypeStack:Count > 0
											aTypeStack:Pop()
										ENDDO
									endif
									aTypeStack:Push(oInfo)
                                    IF ( iClassOrStruct > 0 )
                                        // We are already in Class Or Struct
                                        // So, this is a nested Type
                                        oInfo:oParent := getParentType()
                                    ENDIF
                                    IF oInfo:eType:SupportNestedType()
                                        iClassOrStruct ++
                                    ENDIF
								elseif oInfo:eType:IsClassMember()
								    IF aTypeStack:Count > 0
                                        VAR oParent := getParentType()
                                        if ( oParent != null )
										    oParent:AddChild(oInfo)
                                        endif
									endif
								elseif oInfo:eType:IsGlobalMember()
									self:oGlobalObject:AddChild(oInfo)
								endif
								oInfo:SetNamespaces(aNameSpaces)
								if (oInfo:eType:HasBody())
									oCurrentMethod := oInfo
								else
									oCurrentMethod := NULL_OBJECT
								endif
								lInProperty := oInfo:eType == EntityType._Property
								if state:lEntityIsType
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
									switch oInfo:eType
										case EntityType._Method 
										case EntityType._Access 
										case EntityType._Property 
										case EntityType._Function 
										case EntityType._Global
											oInfo:cRetType := "USUAL" // Default
										otherwise
											oInfo:cRetType := ""
									end switch
								end if
								oInfo:lStatic := lStatic
								oInfo:eAccessLevel := eAccessLevel
								oInfo:eModifiers := eModifiers
								if oInfo:eType == EntityType._Constructor .or. oInfo:eType == EntityType._Destructor
									state:lNameFound := true // Dont't wait for a name, add it to the list now
									oInfo:nStartLine := nEntityStartLine
									oInfo:nOffSet	:= nEntityStartOffSet
									oInfo:nCol		:= nEntityStartCol
									oInfo:cName := iif(oInfo:eType == EntityType._Constructor , ".ctor" , ".dtor")
									oInfo:cShortClassName := cShortClassName
									oInfo:cTypedClassName := cTypedClassName
									oInfo:cClassNameSpace := cClassNameSpace
									oInfo:cClassType := cClassType
									AddEntity(oInfo, oLine)
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
								case state:lInParams .and. (cUpperWord == "REF" .or. cUpperWord == "OUT" .or. cUpperWord == "PARAMS") .and. .not. lEscapedWord
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
									end switch

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
									
									if state:lEntityIsType .and. .not. state:lNameFound
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
											oInfo:SetParamType(cWord, nParamType)
											nParamType := ParamType.AS
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
											_SetLineType(oStatementLine, LineType.BeginNamespace)
											oStatementLine:cArgument := cWord
											state:lIgnore := true
										case .not. state:lNameFound
											state:lNameFound := true
											oInfo:nStartLine := nEntityStartLine
											oInfo:nCol	:= nEntityStartCol
											oInfo:nOffSet := nEntityStartOffSet
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
											AddEntity(oInfo, oLine)
											
											lPartial := false
									end case
								
							end case
							if state:lInParams
								if cChar == ','
									if .not. state:lParam
										var oParam := oInfo:AddParam(cWord)
										oParam:nCol := nChar - cWord:Length
										
									end if
									state:lParam := false
								elseif .not. state:lParam .and. sWord:Length != 0
									if .not. cWord == "SELF"
										var oParam := oInfo:AddParam(cWord)
										oParam:nCol := nChar - cWord:Length
										state:lParam := true
									else
										oCurrentMethod:lExtension := true
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
										_SetLineType(oStatementLine, LineType.TokenIn)
										oStatementLine:cArgument := "BEGIN"
									case "SEQUENCE"
										_SetLineType(oStatementLine, LineType.TokenIn)
										oStatementLine:cArgument := "BEGIN"
									case "SCOPE"
										_SetLineType(oStatementLine, LineType.TokenIn)
										oStatementLine:cArgument := "BEGIN"
									case "CHECKED" 
									case  "UNCHECKED" 
									case "UNSAFE" 
									case "USING" 
									case "FIXED"
										_SetLineType(oStatementLine, LineType.TokenIn)
										oStatementLine:cArgument := "BEGIN"
								end switch
							end if
						case state:lFirstWord .and. cUpperWord == "END"
							lInEnum := false
							eStep := ParseStep.AfterEnd
							_SetLineType(oStatementLine, LineType.TokenOut)
							oStatementLine:cArgument := cUpperWord
						case eStep == ParseStep.AfterEnd
							state:lIgnore := true
							lInEnum := false
							if System.Array.IndexOf(aEndKeywords , cUpperWord) != -1
								_SetLineType(oStatementLine, LineType.EndClass)
								cShortClassName := ""
								cTypedClassName := ""
								cClassNameSpace := ""
								cClassType := ""
							elseif cUpperWord == "NAMESPACE"
								_SetLineType(oStatementLine, LineType.EndNamespace)
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
							_SetLineType(oStatementLine, LineType.Using)
						
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
							oInfo:nCol := nChar - cWord:Length
							if state:lLocal
								oInfo:eType := EntityType._Local
							elseif state:lEvent
								oInfo:eType := EntityType._Event                            
							ELSE
                                if lInEnum
								    oInfo:eType := EntityType._EnumMember
                                else
								    oInfo:eType := EntityType._Field
                                ENDIF
								IF aTypeStack:Count > 0
                                    VAR oParent := getParentType()
                                    if ( oParent != null )
										oParent:AddChild(oInfo)
                                    ENDIF
								endif
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
							oInfo:nStartLine := nLine
							oInfo:cName		 := cWord
							oInfo:cShortClassName := cShortClassName
							oInfo:cTypedClassName := cTypedClassName
							oInfo:cClassNameSpace := cClassNameSpace
							oInfo:cClassType := cClassType
							if state:lField
								AddEntity(oInfo, oLine)
							end if 
							if state:lLocal .and. lIncludeLocals
								aLocals:Add(oInfo)
							end if
						case state:lFirstWord .or. state:lFirstChar
							if System.Array.IndexOf(aTokenInOut, cUpperWord) != -1
								_SetLineType(oStatementLine, LineType.TokenInOut)
								oStatementLine:cArgument := cUpperWord
							elseif System.Array.IndexOf(aTokenIn, cUpperWord) != -1
								_SetLineType(oStatementLine, LineType.TokenIn)
								oStatementLine:cArgument := cUpperWord
							elseif System.Array.IndexOf(aTokenOut , cUpperWord) != -1
								_SetLineType(oStatementLine, LineType.TokenOut)
								oStatementLine:cArgument := cUpperWord
							elseif cUpperWord == "GET" .or. cUpperWord == "SET"
								if lInProperty
									_SetLineType(oStatementLine, LineType.TokenIn)
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
			// Finish and add types to aTypes
			aResult := List<EntityObject>{}
			aResult:Add(self:oGlobalObject)
			foreach oEnt as EntityObject in aEntities
				if oEnt:eType:IsType()
					aResult:Add(oEnt)
				elseif oEnt:oParent == null
					aResult:Add(oEnt)
				endif
			next
			return ParseResult{self}
		
		
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
		
		static method IsType(self e as ENtityType) as logic
			switch e
				case EntityType._Class
				case EntityType._Structure
				case EntityType._Interface
				case EntityType._Delegate
				case EntityType._Enum
				case EntityType._Union
				case EntityType._VOStruct
					return true
			end switch
			RETURN FALSE

		static method SupportNestedType(self e as ENtityType) as logic
			switch e
				case EntityType._Class
				case EntityType._Structure
					return true
			end switch
			RETURN FALSE

		static method IsGlobalMember(self e as ENtityType) as logic
			switch e
				case EntityType._Function
				case EntityType._Procedure
				case EntityType._Define
				case EntityType._Global
				case EntityType._Resource
				case EntityType._Structure
					return true
			end switch
			RETURN FALSE

		static method IsClassMember(self e as ENtityType) as logic
			switch e
				case EntityType._Field
				case EntityType._Property
				case EntityType._Access
				case EntityType._Assign
				case EntityType._Method
				case EntityType._Constructor
				case EntityType._Destructor
				case EntityType._Operator
				CASE EntityType._Event
				case EntityType._EnumMember
					return true
			end switch
			return false
		static method HasBody(self e as EntityType) as logic
			switch e
				case EntityType._Property
				case EntityType._Access
				case EntityType._Assign
				case EntityType._Method
				case EntityType._Constructor
				case EntityType._Destructor
				case EntityType._Operator
				case EntityType._Event
				case EntityType._Function
				case EntityType._Procedure
					return true
				end switch
				return false

		static method NeedsEndKeyword(self e as EntityType) as logic
			switch e
				case EntityType._Class
				case EntityType._Structure
				case EntityType._Interface
				case EntityType._Enum
					return true
				end switch
				return false
		
	end class
	
	[DebuggerDIsplay("{Line} {eType}")];
		class LineObject
		protect _nLine as int
		protect _nOffSet as int
		protect _nCol as int
		protect _eType as LineType
		protect _cArgument as string
		
		
		
		internal constructor(nLine as int)
			self(nLine , 0)
			return
		
		internal constructor(nLine as int, nOffSet as int)
			self:_nLine := nLine
			self:_nOffSet := nOffSet
			return
		
		private constructor(nLine as int, nOffSet as int, nCol as int)
			self:_nLine := nLine
			self:_nOffSet := nOffSet
			self:_nCol  := nCol
			return		
		
		internal method AddSubLine(nColStart as int) as LineObject
			return LineObject{self:_nLine , self:_nOffSet+nColStart, nColStart}
		
		property Line as int get self:_nLine
		property OffSet as int get self:_nOffSet
		property eType as LineType get self:_eType set self:_eType := VALUE
		
		property cArgument as string get self:_cArgument set self:_cArgument := value
		
		
	end class
	
	[DebuggerDIsplay("{eType} {cName,nq}")];
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
		property nStartLine as int  auto
		property nOffSet as int auto
		property nCol as int  auto
		property aNameSpaces as List<string> auto 
		property lStatic as logic auto
		property lPartial as logic auto
		property cClassType as string auto
		property lExtension as logic auto
		property oParent as EntityObject auto
		property aChildren as IList<EntityObject> auto
		property oNext as EntityObject auto
		property oCargo as object auto
		
		internal constructor()
			super()
			self:cInherit := ""
			self:cImplements := ""
			self:cRetType := ""
			self:cClassType := ""
			self:aChildren := List<EntityObject>{}
			self:oParent := null
			return
		internal constructor(nType as EntityType)
			self()
			self:eType := nType
		
		internal method AddChild(oChild as EntityObject) as void
			self:aChildren:Add(oChild)
			oChild:oParent := self
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

		internal method AddParam(cParam as string) as EntityParamsObject
			return self:AddParam(cParam, "")
		
		internal method AddParam(cParam as string , cType as string) as EntityParamsObject
			if self:aParams == null
				self:aParams := List<EntityParamsObject>{}
			end if
			local oParam as EntityParamsObject
			oParam := EntityParamsObject{cParam , cType}
			self:aParams:Add(oParam)
			return oParam
		
		internal method SetParamType(cType as string, nParamType as ParamType) as void
			local oParam as EntityParamsObject
			if self:aParams == null .or. self:aParams:Count == 0 .or. String.IsNullOrEmpty(cType)
				return
			end if
			oParam := self:aParams[self:aParams:Count - 1]
			oParam:cType	  := cType
			oParam:nParamType := nParamType
			return
	end class
	
	[DebuggerDIsplay("{cName,nq} {cType,nq}")];
		class EntityParamsObject
		property cName as string auto
		property cType as string auto
		property nParamType as ParamType auto
		property nCol as int auto
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
	ENUM EntityModifiers AS Int32
		// roslyn values in the comments, should we keep in sync ?
		member _None := 0			// 0x0000
		member _Protected := 1		// 0x0004
		member _Private := 2		// 0x0008
		member _Internal := 4		// 0x0002
		member _Virtual := 8		// 0x0080
		member _Abstract := 16		// 0x0020
		member _Sealed := 32		// 0x0010
		member _Static := 64		// 0x0040
		member _Partial := 128		// 0x4000
		MEMBER _New := 256			// 0x0200
		// roslyn also has these. Should we add these ?
		// public			0x0001
		// extern			0x0100
		// override			0x0400
		// readonly / initonly 0x0800
		// volatile			0x1000
		// unsafe			0x2000
		// async			0x8000
	end enum
	
	enum EntityType as Int32 
		member _None
		member _Constructor
		member _Destructor	
		member _Method		
		member _Access		
		member _Assign		
		member _Class		
		member _Function	
		member _Procedure	
		member _Enum		
		member _VOStruct	
		member _Global		
		member _Structure	
		member _Interface	
		member _Delegate	
		member _Event		
		member @@_Field		
		member _Union		
		member _Operator	
		member _Local		
		member _Property	
		member _Define		
		member _Resource	
		MEMBER _TextBlock	
        MEMBER _EnumMember
	end enum
	
	
	[Flags];
	enum AccessLevel
		member @@Public := 0
		member @@Protected := 1
		member @@Hidden := 2
		member @@Internal := 4
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
		enum WordStyle as int
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
		member TokenInOut
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
		internal property  lEntityIsType as logic		   auto
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
			return
	END STRUCTURE



	
end namespace


