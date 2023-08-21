#using System.Collections.Generic
#using System.Collections
#using System.IO

BEGIN NAMESPACE Xide

ENUM FileType
	MEMBER VulcanNet
	MEMBER CSharp
	MEMBER XSharp
	MEMBER VBNet
	MEMBER Harbour
	MEMBER TextFile
	MEMBER VO
	MEMBER None
END ENUM

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

CLASS ParsingOptions
	EXPORT oFunctions AS Dictionary<STRING , STRING>
	EXPORT oReserved AS Dictionary<STRING , STRING>
	EXPORT oEntityMarkers AS Dictionary<STRING , STRING>
	EXPORT oEntityVisibility AS Dictionary<STRING , STRING>
	EXPORT oIndent AS Dictionary<STRING , STRING>
	EXPORT oOutdent AS Dictionary<STRING , STRING>
	EXPORT oDirectives AS Dictionary<STRING , STRING>
	CONSTRUCTOR()
		SELF:oFunctions := Dictionary<STRING , STRING>{}
		SELF:oReserved := Dictionary<STRING , STRING>{}
		SELF:oEntityMarkers := Dictionary<STRING , STRING>{}
		SELF:oEntityVisibility := Dictionary<STRING , STRING>{}
		SELF:oIndent := Dictionary<STRING , STRING>{}
		SELF:oOutdent := Dictionary<STRING , STRING>{}
		SELF:oDirectives := Dictionary<STRING , STRING>{}
	RETURN
END CLASS

INTERNAL STRUCTURE ParseState
	EXPORT lVisFound AS LOGIC
	EXPORT lEntityFound AS LOGIC
	EXPORT lEntityIsClass AS LOGIC
	EXPORT lFirstChar AS LOGIC
	EXPORT lFirstWord AS LOGIC
	EXPORT lInParams AS LOGIC
	EXPORT lNameFound AS LOGIC
	EXPORT lField AS LOGIC
	EXPORT lLocal AS LOGIC
	EXPORT lImpliedLocal AS LOGIC
	EXPORT lEvent AS LOGIC
	EXPORT lParam AS LOGIC
	EXPORT lDirective AS LOGIC
	EXPORT lIgnore AS LOGIC
	EXPORT lExpectName AS LOGIC
	EXPORT lLinqSelect AS LOGIC
	EXPORT lDimDeclaration AS LOGIC
	METHOD Reset() AS VOID
		lVisFound := FALSE
		lEntityFound := FALSE
		lEntityIsClass := FALSE
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

[Flags];
ENUM BufferParseItems AS INT
	MEMBER Entities := 1
//	MEMBER Fields := 2
	MEMBER Locals := 4
	MEMBER Tokens := 8
	MEMBER Usings := 16
	MEMBER Words := 32
	MEMBER FinalizeLine := 64
	MEMBER AddTyped := 128
	MEMBER RetrieveEntities := 256
	MEMBER RetrieveLocals := 512
	MEMBER LinesModified := 1024
END ENUM

PARTIAL CLASS EditorBuffer
	PROTECT eFileType AS FileType
	EXPORT aLines AS List<LineObject>
	PROTECT oParsingOptions AS ParsingOptions
	EXPORT cFileName AS STRING
	EXPORT cGuid AS STRING
//	PROTECT oIntelParams AS IntellisenseParams
//	EXPORT oHiddenBuffer_File AS FileClass
	
	PROTECT nActionStart AS INT
	PROTECT nActionEnd AS INT
	PROTECT nActionDepth AS INT
	
	STATIC PROTECT hBrk AS Dictionary<Char,Char>
	STATIC CONSTRUCTOR()
		InitHashTables()
	RETURN
	PROTECTED STATIC METHOD InitHashTables() AS VOID
		LOCAL cBreak AS STRING
		LOCAL n AS INT
		cBreak := e",./;:[]<>?{}`~!@#$%^&*()-=\\+|'\""
		hBrk := Dictionary<Char,Char>{50}
		FOR n := 0 UPTO cBreak:Length - 1
			hBrk:Add(cBreak[n] , cBreak[n])
		NEXT
	RETURN

	CONSTRUCTOR(_eFileType AS FileType , _aLines AS List<LineObject>)
		SELF:eFileType := _eFileType
		SELF:aLines := _aLines
		SELF:oParsingOptions := XSharpBuffer.XSharpParsingOptions
	RETURN

	VIRTUAL METHOD Dispose() AS VOID
		IF SELF:aLines == NULL
			RETURN
		END IF
		SELF:aLines:Clear()
		SELF:aLines := NULL
		SELF:oParsingOptions := NULL
	RETURN

	PROPERTY FileType AS FileType GET SELF:eFileType
	
	PROPERTY ParsingOptions AS ParsingOptions GET SELF:oParsingOptions

	PROPERTY Count AS INT GET SELF:aLines:Count
	
	VIRTUAL METHOD GetLine(nLine AS INT) AS LineObject
		IF nLine <= 0 .or. nLine > SELF:aLines:Count
			RETURN NULL
		END IF
	RETURN SELF:aLines[nLine - 1]
		
	VIRTUAL METHOD BeginAction() AS VOID
		IF SELF:nActionDepth == 0
			SELF:nActionStart := 0
			SELF:nActionEnd := 0
		END IF
		SELF:nActionDepth ++
	RETURN
	VIRTUAL METHOD EndAction() AS VOID
		SELF:nActionDepth --
		IF SELF:nActionDepth == 0 .and. SELF:nActionStart != 0
			IF SELF:nActionStart > SELF:aLines:Count
				SELF:nActionStart := SELF:aLines:Count
			END IF
			IF SELF:nActionEnd > SELF:aLines:Count
				SELF:nActionEnd := SELF:aLines:Count
			END IF
			IF SELF:nActionStart > 1
				SELF:nActionStart --
			END IF
			IF SELF:nActionEnd < SELF:aLines:Count
				SELF:nActionEnd ++
			END IF
			IF SELF:nActionStart < 1 .or. SELF:nActionEnd < 1
				RETURN
			END IF
			SELF:Parse(BufferParseItems.Entities + BufferParseItems.Tokens + BufferParseItems.LinesModified , SELF:nActionStart , SELF:nActionEnd , 0)
		END IF
	RETURN
	VIRTUAL PROTECTED METHOD LineModified(nLine AS INT) AS VOID
		IF nLine <= 0 .or. nLine > SELF:aLines:Count
			RETURN
		END IF
		IF SELF:nActionStart == 0 .or. SELF:nActionStart > nLine
			SELF:nActionStart := nLine
		END IF
		IF SELF:nActionEnd == 0 .or. SELF:nActionEnd < nLine
			SELF:nActionEnd := nLine
		END IF
	RETURN
	
	VIRTUAL METHOD Clear() AS VOID
		SELF:aLines:Clear()
		SELF:aLines:Add(LineObject{""})
		SELF:LineModified(1)
	RETURN
	
	VIRTUAL METHOD DeleteAllLines() AS VOID
		SELF:aLines:Clear()
		SELF:aLines:Add(LineObject{""})
		SELF:LineModified(1)
	RETURN

	VIRTUAL METHOD DeleteLine(nLine AS INT) AS VOID
		IF nLine >= 1 .and. nLine <= SELF:aLines:Count
			SELF:aLines:RemoveAt(nLine - 1)
			SELF:LineModified(nLine)
		END IF
	RETURN
	VIRTUAL METHOD InsertLine(nLine AS INT , cLine AS STRING) AS VOID
		IF nLine >= 1 .and. nLine <= SELF:aLines:Count
			SELF:aLines:Insert(nLine - 1 , LineObject{cLine})
			SELF:LineModified(nLine)
		ELSEIF nLine > SELF:aLines:Count
			SELF:aLines:Add(LineObject{cLine})
			SELF:LineModified(nLine)
		END IF
	RETURN
	VIRTUAL METHOD AddLine(cLine AS STRING) AS VOID
		SELF:aLines:Add(LineObject{cLine})
		SELF:LineModified(SELF:aLines:Count)
	RETURN
	VIRTUAL METHOD ChangeLine(nLine AS INT , cLine AS STRING) AS VOID
		IF nLine >= 1 .and. nLine <= SELF:aLines:Count
			SELF:aLines[nLine - 1]:LineText := cLine
			SELF:LineModified(nLine)
		END IF
	RETURN
			
	VIRTUAL METHOD ParseEntities() AS VOID
		SELF:Parse(BufferParseItems.Entities)
	RETURN
	
	VIRTUAL METHOD CheckLineTokens(nLine AS INT) AS VOID
		SELF:Parse(BufferParseItems.Tokens , nLine , nLine , 0)
	RETURN
	VIRTUAL METHOD LinesModified(nStartLine AS INT , nEndLine AS INT , nCaret AS INT) AS LOGIC
		LOCAL lMoreModified AS LOGIC
		lMoreModified := SELF:Parse(BufferParseItems.Entities + BufferParseItems.Tokens + ;
									BufferParseItems.FinalizeLine + BufferParseItems.LinesModified +;
									BufferParseItems.AddTyped , ;
									nStartLine , nEndLine , nCaret) != NULL
	RETURN lMoreModified
	
	VIRTUAL METHOD FullParse() AS LOGIC
		SELF:Parse(BufferParseItems.Entities + BufferParseItems.Tokens)
	RETURN TRUE
	
	STATIC METHOD ParseLine(eFileType AS FileType , cLine AS STRING) AS ArrayList
		LOCAL oBuffer AS EditorBuffer
		LOCAL aLines AS List<LineObject>
		aLines := List<LineObject>{1}
		aLines:Add(LineObject{cLine})
		oBuffer := XSharpBuffer{aLines}
	RETURN oBuffer:ParseLine(1)
	VIRTUAL METHOD ParseLine(nLine AS INT) AS ArrayList
	RETURN SELF:Parse(BufferParseItems.Words , nLine , nLine , 0)

	VIRTUAL PROTECTED METHOD Parse(eItems AS BufferParseItems) AS ArrayList
	RETURN SELF:Parse(eItems , 0 , 0 , 0)
	VIRTUAL PROTECTED METHOD Parse(eItems AS BufferParseItems , nStartLine AS INT , nEndLine AS INT , nExceptLine AS INT) AS ArrayList
	RETURN NULL
	
END CLASS


CLASS LanguageBuffer INHERIT EditorBuffer
	
	INTERNAL CONSTRUCTOR(_eFileType AS FileType , _aLines AS List<LineObject>)
		SUPER(_eFileType , _aLines)
	RETURN
	
END CLASS

END NAMESPACE
