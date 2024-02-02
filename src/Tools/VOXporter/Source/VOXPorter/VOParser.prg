// Parser for VO code, taken from XIDE

USING System.Collections.Generic
USING System.Collections

BEGIN NAMESPACE VOParser

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

CLASS WordObject
	EXPORT cWord AS STRING
	EXPORT nStart,nEnd AS INT
	EXPORT eStatus AS WordStatus
	EXPORT eSubStatus AS WordSubStatus
	PROTECT eStyle AS WordStyle
	CONSTRUCTOR(_cWord AS STRING)
		SELF:cWord := _cWord
	RETURN
	METHOD SetStyle(_eStyle AS WordStyle) AS VOID
		SELF:eStyle := SELF:eStyle | _eStyle
	RETURN
	METHOD HasStyle(_eStyle AS WordStyle) AS LOGIC
	RETURN (SELF:eStyle & _eStyle) == _eStyle
	ACCESS IsOneChar AS LOGIC
	RETURN SELF:cWord:Length==1 .and. !SELF:cWord==" " .and. !SELF:cWord == e"\t" .and. SELF:eStatus == WordStatus.Text
	ACCESS IsWhiteSpace AS LOGIC
	RETURN (SELF:cWord==" " .or. SELF:cWord == e"\t") .and. SELF:eStatus == WordStatus.Text
	VIRTUAL METHOD ToString() AS STRING
	RETURN "Word:" + iif(SELF:cWord != NULL , SELF:cWord , "")

END CLASS




ENUM WordType AS INT
	MEMBER Unknown := 0
	MEMBER Type := 1
	MEMBER Variable := 2
	MEMBER @@Member := 4
	MEMBER @@Method := 8
	MEMBER @@Operator := 16
END ENUM
ENUM WordSubType
	MEMBER Unknown

	MEMBER @@Local
	MEMBER @@Param
	MEMBER @@Global

	MEMBER @@Function
	MEMBER @@Method

	MEMBER @@Field
	MEMBER @@Property
	MEMBER @@Event

	MEMBER @@Self
	MEMBER @@Super
	MEMBER @@Type
END ENUM

ENUM OperatorType
	MEMBER None
	MEMBER @@Array
	MEMBER IndexedProperty
	MEMBER MethodCall
	MEMBER ConstructorCall
END ENUM

ENUM LineType AS INT
//ENUM LineType AS BYTE
	MEMBER None
	MEMBER RegionIn
	MEMBER RegionOut
	MEMBER TokenIn
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

STRUCTURE Range
	EXPORT Begins AS INT
	EXPORT Ends AS INT
	PROPERTY Empty AS LOGIC GET SELF:Begins == 0 .and. SELF:Ends == 0
	PROPERTY NotEmpty AS LOGIC GET .not. Empty
	PROPERTY HasBeginAndEnd AS LOGIC GET SELF:Begins != 0 .and. SELF:Ends != 0
	METHOD ToString() AS STRING
	RETURN SELF:Begins:ToString() + "-" + SELF:Ends:ToString()
END STRUCTURE

CLASS LineInfo
	EXPORT ClassClause AS Range
	EXPORT PascalClause AS Range
	EXPORT ExportLocalClause AS Range
END CLASS

PARTIAL CLASS LineObject
	EXPORT lInBlockComment , lOutBlockComment AS LOGIC
	EXPORT lInAmpersand , lOutAmpersand AS LOGIC

	EXPORT lCollapsed AS LOGIC
	EXPORT lBreakPoint AS LOGIC
	EXPORT lBreakPointDisabled AS LOGIC
	PROTECT _nBookmark AS BYTE

	EXPORT eType AS LineType

	PROTECT cLineText , cLineCased , cLineSaved AS STRING

	PROTECT nStartCol AS INT
	EXPORT nFirstWordCol AS INT

	EXPORT cArgument AS STRING
	EXPORT cExtra AS STRING
		

	EXPORT oSubLine AS LineObject
	EXPORT oEntity AS EntityObject
	EXPORT oInline AS EntityObject
	EXPORT aFields AS List<EntityObject>
	EXPORT nCollapsed AS INT
	EXPORT nCollapsedSubline AS INT
	EXPORT cBookmarkDescr AS STRING
	
	EXPORT oMoreInfo AS LineInfo

	CONST PROTECT _internlimit := 0 AS INT
		
	ACCESS LineText AS STRING
	RETURN SELF:cLineText
	ASSIGN LineText(cLine AS STRING)
		IF cLine != NULL .and. cLine:Length < _internlimit
			cLine := String.Intern(cLine)
		END IF
		SELF:cLineText := cLine
	RETURN
	ACCESS LineCased AS STRING
	RETURN SELF:cLineCased
	ASSIGN LineCased(cLine AS STRING)
		IF cLine != NULL .and. cLine:Length < _internlimit
			cLine := String.Intern(cLine)
		END IF
		SELF:cLineCased := cLine
	RETURN

	CONSTRUCTOR(cLine AS STRING)
		SELF:LineText := cLine
		SELF:LineCased := cLine
		SELF:cArgument := NULL
	RETURN
	CONSTRUCTOR()
		SELF:LineText := NULL
		SELF:LineCased := NULL
		SELF:cArgument := NULL
		SELF:cBookmarkDescr := NULL
	RETURN
		
	ACCESS StartCol AS INT
	RETURN SELF:nStartCol
	
	METHOD AddSubLine(nCol AS INT) AS LineObject
		IF SELF:oSubLine == NULL
			SELF:oSubLine := LineObject{""}
			SELF:oSubLine:nStartCol := nCol
			RETURN SELF:oSubLine
		END IF
	RETURN SELF:oSubLine:AddSubLine(nCol)
	ACCESS LastSubLine AS LineObject
		IF SELF:oSubLine == NULL
			RETURN SELF
		END IF
	RETURN SELF:oSubLine:LastSubLine

	METHOD ClearFields() AS VOID
		IF SELF:aFields != NULL
			SELF:aFields:Clear()
		END IF
	RETURN
	METHOD AddField(oEntity AS EntityObject) AS VOID
		IF SELF:aFields == NULL
			SELF:aFields := List<EntityObject>{}
		END IF
		SELF:aFields:Add(oEntity)
	RETURN
	ACCESS ContainsField AS LOGIC
		IF SELF:aFields == NULL .or. SELF:aFields:Count == 0
			RETURN FALSE
		END IF
	RETURN SELF:aFields[0]:eType == EntityType._Field
	ACCESS ContainsEvent AS LOGIC
		IF SELF:aFields == NULL .or. SELF:aFields:Count == 0
			RETURN FALSE
		END IF
	RETURN SELF:aFields[0]:eType == EntityType._Event
	
	
	ACCESS ContainsEntity AS LOGIC
		IF SELF:oEntity != NULL
			RETURN TRUE
		END IF
		IF SELF:oSubLine != NULL
			RETURN SELF:oSubLine:ContainsEntity
		END IF
	RETURN FALSE
	ACCESS FirstEntity AS EntityObject
		IF SELF:oEntity != NULL
			RETURN SELF:oEntity
		END IF
		IF SELF:oSubLine != NULL
			RETURN SELF:oSubLine:FirstEntity
		END IF
	RETURN NULL
	ACCESS LastEntity AS EntityObject
		LOCAL oEntity AS EntityObject
		IF SELF:oSubLine != NULL
			oEntity := SELF:oSubLine:LastEntity
			IF oEntity != NULL
				RETURN oEntity
			END IF
		END IF
	RETURN SELF:oEntity
	METHOD GetEntityAt(nCol AS INT) AS EntityObject
		LOCAL oEntity AS EntityObject
		IF SELF:oSubLine != NULL
			oEntity := SELF:oSubLine:GetEntityAt(nCol)
			IF oEntity != NULL
				RETURN oEntity
			END IF
		END IF
		IF SELF:oEntity != NULL .and. SELF:oEntity:nCol <= nCol
			RETURN SELF:oEntity
		END IF
	RETURN NULL
	METHOD VerifyEntitiesLineNum(nLine AS INT , lWithSubLines AS LOGIC) AS LOGIC
		LOCAL oEntity AS EntityObject
		LOCAL lAllOk AS LOGIC
		LOCAL n AS INT
		IF SELF:oEntity != NULL .or. (SELF:aFields != NULL .and. SELF:aFields:Count != 0)
			lAllOk := TRUE
		END IF
		IF SELF:oEntity != NULL
			IF SELF:oEntity:nLine != nLine
				lAllOk := FALSE
				SELF:oEntity:nLine := nLine
			END IF
		END IF
		IF SELF:aFields != NULL .and. SELF:aFields:Count != 0
			FOR n := 0 UPTO aFields:Count - 1
				oEntity := SELF:aFields[n]
				IF oEntity:nLine != nLine
					lAllOk := FALSE
					oEntity:nLine := nLine
				END IF
			NEXT
		END IF
		IF lWithSubLines .and. SELF:oSubLine != NULL
			lAllOk := SELF:oSubLine:VerifyEntitiesLineNum(nLine , TRUE) .and. lAllOk // prwta to call se VerifyEntitiesLineNum()
		END IF
	RETURN lAllOk

	ACCESS lEntity AS LOGIC
	RETURN SELF:oEntity != NULL

	ACCESS IsReturn AS LOGIC
	RETURN SELF:eType == LineType.Return
	ACCESS ContainsReturn AS LOGIC
		IF SELF:eType == LineType.Return
			RETURN TRUE
		END IF
		IF SELF:oSubLine != NULL
			RETURN SELF:oSubLine:ContainsReturn
		END IF
	RETURN FALSE

	ACCESS IsRegionIn AS LOGIC
	RETURN SELF:eType == LineType.RegionIn
	ACCESS IsRegionOut AS LOGIC
	RETURN SELF:eType == LineType.RegionOut
	
	ACCESS IsIfdefIn AS LOGIC
	RETURN SELF:eType == LineType.IfdefIn
	ACCESS IsIfdefOut AS LOGIC
	RETURN SELF:eType == LineType.IfdefOut
	
	ACCESS IsBeginProperty AS LOGIC
//	RETURN SELF:eType == LineType.BeginProperty
	RETURN SELF:oEntity != NULL .and. SELF:oEntity:eType == EntityType._Property
	ACCESS IsEndProperty AS LOGIC
	RETURN SELF:eType == LineType.EndProperty

	ACCESS IsEndClass AS LOGIC
	RETURN SELF:eType == LineType.EndClass
	ACCESS IsEndNamespace AS LOGIC
	RETURN SELF:eType == LineType.EndNameSpace
	ACCESS IsBeginNamespace AS LOGIC
	RETURN SELF:eType == LineType.BeginNamespace
	
	ACCESS IsUsing AS LOGIC
	RETURN SELF:eType == LineType.Using
	ACCESS IsInclude AS LOGIC
	RETURN SELF:eType == LineType.Include
	
/*	ACCESS IsEntity AS LOGIC
	RETURN SELF:oEntity != NULL*/

	ACCESS IsDirective AS LOGIC
	RETURN SELF:eType == LineType.Define .or. SELF:eType == LineType.IfdefIn .or. SELF:eType == LineType.IfdefOut .or. ;
			SELF:eType == LineType.Include .or. SELF:eType == LineType.RegionIn .or. SELF:eType == LineType.RegionOut .or. ;
			SELF:eType == LineType.Using .or. SELF:eType == LineType.OtherDirective
			
ACCESS IsStartClass AS LOGIC
// prepei na exei prohghthei CheckBlockComments()
RETURN SELF:lEntity .and. (SELF:oEntity:eType == EntityType._Class .or. SELF:oEntity:eType == EntityType._Structure .or. SELF:oEntity:eType == EntityType._Interface)

	ACCESS IsDesignerUserStart AS LOGIC
		LOCAL cLine AS STRING
		cLine := SELF:LineText:ToUpper()
	RETURN cLine:Contains("##USER##") .or. cLine:Contains("{{%UC%}}") .or. cLine:Contains("USER CODE STARTS")
			
	METHOD SetClassClauseStart(nStart AS INT) AS VOID
		IF SELF:oMoreInfo == NULL
			SELF:oMoreInfo := LineInfo{}
		END IF
		SELF:oMoreInfo:ClassClause:Begins := nStart
	RETURN
	METHOD SetClassClauseEnd(nEnd AS INT) AS VOID
		IF SELF:oMoreInfo == NULL
			SELF:oMoreInfo := LineInfo{}
		END IF
		SELF:oMoreInfo:ClassClause:Ends := nEnd
	RETURN

	METHOD SetPascalClauseStart(nStart AS INT) AS VOID
		IF SELF:oMoreInfo == NULL
			SELF:oMoreInfo := LineInfo{}
		END IF
		SELF:oMoreInfo:PascalClause:Begins := nStart
		SELF:oMoreInfo:PascalClause:Ends := nStart + 6
	RETURN

	METHOD SetExportLocalClauseStart(nStart AS INT) AS VOID
		IF SELF:oMoreInfo == NULL
			SELF:oMoreInfo := LineInfo{}
		END IF
		SELF:oMoreInfo:ExportLocalClause:Begins := nStart
	RETURN
	METHOD SetExportLocalClauseEnd(nEnd AS INT) AS VOID
		IF SELF:oMoreInfo == NULL
			SELF:oMoreInfo := LineInfo{}
		END IF
		SELF:oMoreInfo:ExportLocalClause:Ends := nEnd
	RETURN

	PROPERTY HasClassClause AS LOGIC GET SELF:oMoreInfo != NULL .and. SELF:oMoreInfo:ClassClause:NotEmpty
	PROPERTY HasPascalClause AS LOGIC GET SELF:oMoreInfo != NULL .and. SELF:oMoreInfo:PascalClause:NotEmpty
	PROPERTY HasExportLocalClause AS LOGIC GET SELF:oMoreInfo != NULL .and. SELF:oMoreInfo:ExportLocalClause:HasBeginAndEnd

	PROPERTY IsXmlComment AS LOGIC
		GET
			RETURN .not. SELF:lInBlockComment .and. SELF:cLineCased:TrimStart():StartsWith("///") .and. .not. SELF:cLineCased:TrimStart():StartsWith("////")
		END GET
	END PROPERTY	

	VIRTUAL METHOD ToString() AS STRING
	RETURN "Line: " + SELF:LineText

END CLASS

ENUM EntityType AS Int32 // todo need to add delegate, operator
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
END ENUM

[Flags];
ENUM EntityModifiers AS Int32
	MEMBER _None := 0
	MEMBER _Protected := 1
	MEMBER _Private := 2
	MEMBER _Internal := 4
	MEMBER _Virtual := 8
	MEMBER _Abstract := 16
	MEMBER _Sealed := 32
	MEMBER _Static := 64
	MEMBER _Partial := 128
	MEMBER _New := 256
END ENUM

ENUM AccessLevel
	MEMBER @@Public := 0
	MEMBER @@Protected := 1
	MEMBER @@Hidden := 2
	MEMBER @@Internal := 4
END ENUM

CLASS EntityParamsObject
	EXPORT cName AS STRING
	EXPORT cType AS STRING
	EXPORT lReference AS LOGIC
	CONSTRUCTOR(_cName AS STRING , _cType AS STRING)
		SUPER()
		SELF:cName := _cName
		SELF:cType := _cType
	RETURN
	ACCESS IntelText AS STRING
		LOCAL cRet AS STRING
		cRet := SELF:cName
		IF .not. String.IsNullOrWhiteSpace(SELF:cType)
			cRet += iif(SELF:lReference , " REF " , " AS ") + SELF:cType
		END IF
	RETURN cRet
	METHOD Clone() AS EntityParamsObject
	RETURN (EntityParamsObject)SELF:MemberwiseClone()
END CLASS

CLASS EntityObject
	EXPORT eType AS EntityType
	EXPORT cName,cInherit,cRetType,cImplements AS STRING
	EXPORT eModifiers AS EntityModifiers
	EXPORT eAccessLevel AS AccessLevel
	EXPORT cShortClassName AS STRING
	EXPORT cTypedClassName AS STRING
	EXPORT cClassNamespace AS STRING
	EXPORT aParams AS List<EntityParamsObject>
	EXPORT nLine , nCol AS INT
	EXPORT aNameSpaces AS List<STRING> // prosoxh, kai se functions etc
	EXPORT lStatic AS LOGIC
	EXPORT lPartial AS LOGIC
	EXPORT cClassType AS STRING
	EXPORT lExtension AS LOGIC

	CONSTRUCTOR()
		SUPER()
		SELF:cInherit := ""
		SELF:cImplements := ""
		SELF:cRetType := ""
		SELF:cClassType := ""
	RETURN
	
	VIRTUAL METHOD Clone() AS EntityObject
		LOCAL oEntity AS EntityObject
		oEntity := (EntityObject)SELF:MemberwiseClone()
		IF oEntity:aParams != NULL
			oEntity:aParams := List<EntityParamsObject>{}
			FOREACH oParam AS EntityParamsObject IN SELF:aParams
				oEntity:aParams:Add(oParam:Clone())
			NEXT
		END IF
		IF oEntity:aNameSpaces != NULL
			oEntity:aNameSpaces := List<STRING>{}
			FOREACH cNameSpace AS STRING IN oEntity:aNameSpaces
				oEntity:aNameSpaces:Add(cNameSpace)
			NEXT
		END IF
	RETURN oEntity
	
	PROPERTY HasParams AS LOGIC GET aParams != NULL .and. aParams:Count != 0
	
	METHOD NamespacesEqual(_aNameSpaces AS List<STRING>) AS LOGIC
		LOCAL n AS INT
		IF SELF:aNameSpaces == NULL .or. SELF:aNameSpaces:Count != _aNameSpaces:Count
			RETURN FALSE
		END IF
		FOR n := 0 UPTO SELF:aNameSpaces:Count - 1
			IF SELF:aNameSpaces[n] != _aNameSpaces[n]
				RETURN FALSE
			END IF
		NEXT
	RETURN TRUE
	METHOD SetNamespaces(_aNameSpaces AS List<STRING>) AS VOID
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
	
	ACCESS FullClassName AS STRING
		LOCAL cRet AS STRING
		cRet := SELF:cShortClassName
		IF .not. String.IsNullOrEmpty(SELF:cClassNamespace)
			cRet := SELF:cClassNamespace + "." + cRet
		END IF
	RETURN cRet
	ACCESS FullName AS STRING
		LOCAL cRet AS STRING
		DO CASE
		CASE SELF:eType == EntityType._Class .or. SELF:eType == EntityType._Interface .or. SELF:eType == EntityType._Structure
			cRet := SELF:FullClassName
		CASE SELF:eType == EntityType._Access .or. SELF:eType == EntityType._Assign .or. SELF:eType == EntityType._Property .or. ;
				SELF:eType == EntityType._Method .or. SELF:eType == EntityType._Field .or. SELF:eType == EntityType._Event
			cRet := SELF:FullClassName + "." + SELF:cName
		CASE SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure .or. SELF:eType == EntityType._Global
			cRet := SELF:cName
		CASE SELF:eType == EntityType._Enum
			cRet := SELF:FullClassName
		CASE SELF:eType == EntityType._Delegate
			cRet := SELF:FullClassName
		CASE SELF:eType == EntityType._VOStruct .or. SELF:eType == EntityType._Union
			cRet := SELF:FullClassName
		OTHERWISE
			cRet := ""
		END CASE
	RETURN cRet
	
	ACCESS IsType AS LOGIC
	RETURN SELF:eType == EntityType._Class .or. SELF:eType == EntityType._Structure .or. ;
			SELF:eType == EntityType._VOStruct .or. SELF:eType == EntityType._Union .or. ;
			SELF:eType == EntityType._Interface .or. SELF:eType == EntityType._Delegate .or. ;
			SELF:eType == EntityType._Enum

	ACCESS IsClassOrMember AS LOGIC
	RETURN SELF:eType == EntityType._Class .or. SELF:eType == EntityType._Method .or. ;
			SELF:eType == EntityType._Access .or. SELF:eType == EntityType._Assign .or. SELF:eType == EntityType._Property .or. ;
			SELF:eType == EntityType._Constructor .or. SELF:eType == EntityType._Destructor

	ACCESS NonClass AS LOGIC
	RETURN SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure .or. ;
			SELF:eType == EntityType._Global

	ACCESS IsCode AS LOGIC
	RETURN SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure .or. ;
			SELF:eType == EntityType._Access .or. SELF:eType == EntityType._Assign .or. ;
			SELF:eType == EntityType._Constructor .or. SELF:eType == EntityType._Destructor .or. ;
			SELF:eType == EntityType._Method .or. SELF:eType == EntityType._Operator .or. SELF:eType == EntityType._Property

	ACCESS IsVO AS LOGIC
	RETURN SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure .or. ;
			SELF:eType == EntityType._VOStruct .or. SELF:eType == EntityType._Union .or. ;
			SELF:eType == EntityType._Global

	ACCESS IsFuncProc AS LOGIC
	RETURN SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure
	ACCESS IsFuncProcGlobal AS LOGIC
	RETURN SELF:eType == EntityType._Function .or. SELF:eType == EntityType._Procedure .or. SELF:eType == EntityType._Global

	ACCESS StringEntityType AS STRING
	RETURN SELF:eType:ToString():Substring(1):ToUpper()

	METHOD AddParam(cParam AS STRING) AS VOID
		IF SELF:aParams == NULL
			SELF:aParams := List<EntityParamsObject>{}
		END IF
		SELF:aParams:Add(EntityParamsObject{cParam , "USUAL"})
	RETURN
	METHOD AddParam(cParam AS STRING , cType AS STRING) AS VOID
		IF SELF:aParams == NULL
			SELF:aParams := List<EntityParamsObject>{}
		END IF
		SELF:aParams:Add(EntityParamsObject{cParam , cType})
	RETURN
	METHOD SetParamType(cType AS STRING) AS VOID
		LOCAL oParam AS EntityParamsObject
		IF SELF:aParams == NULL .or. SELF:aParams:Count == 0 .or. String.IsNullOrEmpty(cType)
			RETURN
		END IF
		oParam := SELF:aParams[SELF:aParams:Count - 1]
		IF cType:Contains("&")
			cType := cType:Replace("&" , "")
			oParam:lReference := TRUE
		END IF
		oParam:cType := cType
	RETURN

	METHOD IsMatchForParams(nParams AS INT) AS LOGIC
		IF nParams == -1
			RETURN TRUE
		END IF
		IF nParams == 0 .and. (SELF:aParams == NULL .or. SELF:aParams:Count == 0)
			RETURN TRUE
		END IF
		IF nParams > 0 .and. SELF:aParams != NULL .and. SELF:aParams:Count == nParams
			RETURN TRUE
		END IF
	RETURN FALSE

	VIRTUAL METHOD ToString() AS STRING
		LOCAL cRet AS STRING
//		LOCAL n AS INT
		IF SELF:eType == EntityType._Property .or. SELF:eType == EntityType._Access .or. SELF:eType == EntityType._Assign .or. ;
			SELF:eType == EntityType._Constructor .or. SELF:eType == EntityType._Destructor .or. SELF:eType == EntityType._Event .or. ;
			SELF:eType == EntityType._Field .or. SELF:eType == EntityType._Method
			cRet := "    "
		ELSE
			cRet := ""
		END IF
		cRet += SELF:eType:ToString() + " "
		cRet += SELF:cName
		IF .not. String.IsNullOrEmpty(SELF:cInherit)
			cRet += " (" + SELF:cInherit + ") "
		END IF
	RETURN cRet
	
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
	MEMBER AfterBeginUsing
	MEMBER AfterSharp
	MEMBER AfterDefine
	MEMBER AfterUsing
	MEMBER AfterInclude
	MEMBER AfterWith
	MEMBER AfterImplements
	MEMBER WaitImplements
	MEMBER WaitCommaImplements

	// for VO only:
//	MEMBER WaitClassClause
	MEMBER AfterClassClause
	MEMBER AfterExportClause
END ENUM

CLASS ParsingOptions
	EXPORT oTyped AS Dictionary<STRING , STRING>
	EXPORT oFunctions AS Dictionary<STRING , STRING>
	EXPORT oReserved AS Dictionary<STRING , STRING>
	EXPORT oIgnoredReserved AS Dictionary<STRING , STRING>
	EXPORT oEntityMarkers AS Dictionary<STRING , STRING>
	EXPORT oEntityVisibility AS Dictionary<STRING , STRING>
	EXPORT oIndent AS Dictionary<STRING , STRING>
	EXPORT oOutdent AS Dictionary<STRING , STRING>
	EXPORT oDirectives AS Dictionary<STRING , STRING>
	CONSTRUCTOR()
		SELF:oTyped := Dictionary<STRING , STRING>{}
		SELF:oFunctions := Dictionary<STRING , STRING>{}
		SELF:oReserved := Dictionary<STRING , STRING>{}
		SELF:oIgnoredReserved := Dictionary<STRING , STRING>{}
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

CLASS VoBuffer
	PROTECT aLines AS List<LineObject>
	PROTECT oParsingOptions AS ParsingOptions
	STATIC PROTECT hBrk AS Dictionary<Char,Char>

	STATIC CONSTRUCTOR()
		LOCAL aWords AS STRING[]
		LOCAL n AS INT
		
		VoBuffer.oVoParsingOptions := ParsingOptions{}
		InitHashTables()

		aWords := <STRING>{;
		"Byte","Short","ShortInt","Word","Int","Long","LongInt","DWord",;
		"Real4","Real8","Float","Logic","String","Psz","Usual","Array","Date",;
		"Object","Symbol","CodeBlock","Ptr","Void","Nil","Null","True","False",;
		;
		"Clas","Class",;
		"Unio","Union",;
		"Stru","Struc","Struct","Structu","Structur","Structure",;
		"Meth","Metho","Method",;
		"Acce","Acces","Access",;
		"Assi","Assig","Assign",;
		"Func","Funct","Functi","Functio","Function",;
		"Proc","Proce","Proced","Procedu","Procedur","Procedure",;
		"Defi","Defin","Define",;
		"Glob","Globa","Global",;
		;
		"VOStruct","Align","Member","Enum",;
		"Inherit","Hidden","Self",;
		"Super","Return",;
		"Local","Static","Protect",;
		"Instance","Export","As","Is","_Cast","Ref","Dim","Clipper","Pascal","CDecl",;
		"Strict","WinApi","For","Next","To","UpTo","DownTo","Step","Loop","Exit",;
		"If","Else","ElseIf","EndIf","Do","Case","Otherwise","While","EndDo","EndCase",;
		"Begin","Sequence","End","Recover","Finally","Break",;
		"Private","Protected","Field","_Dll",;
		"_Init1","_Init2","_Init3",;
		"Resource","Textblock"}
		FOR n := 1 UPTO aWords:Length
			VoBuffer.oVoParsingOptions:oReserved:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT
		
/*		aWords := <STRING>{;
		"CLASS","CLAS","METHOD","FUNCTION","PROCEDURE","FUNC","PROC","ACCESS","ASSIGN",;
		"GLOBAL","STRUCTURE","STRUCT","VOSTRUCT","UNION","DEFINE","RESOURCE","TEXTBLOCK"}*/
		aWords := <STRING>{"RESOURCE","TEXTBLOCK","VOSTRUCT",;
		"CLAS","CLASS",;
		"UNIO","UNION",;
		"STRU","STRUC","STRUCT","STRUCTU","STRUCTUR","STRUCTURE",;
		"METH","METHO","METHOD",;
		"ACCE","ACCES","ACCESS",;
		"ASSI","ASSIG","ASSIGN",;
		"FUNC","FUNCT","FUNCTI","FUNCTIO","FUNCTION",;
		"PROC","PROCE","PROCED","PROCEDU","PROCEDUR","PROCEDURE",;
		"DEFI","DEFIN","DEFINE",;
		"GLOB","GLOBA","GLOBAL"}
		FOR n := 1 UPTO aWords:Length
			VoBuffer.oVoParsingOptions:oEntityMarkers:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT

		aWords := <STRING>{;
		"_DLL", ;
		"HIDDEN", "STATIC", "PROTECTED", "INSTANCE", ;
		"PROTECT", "PRIVATE", "PUBLIC", "EXPORT", ;
		"MEMBER"}
		FOR n := 1 UPTO aWords:Length
			VoBuffer.oVoParsingOptions:oEntityVisibility:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT

		aWords := <STRING>{;
		"ifdef","ifndef","endif","else","define","undef","command","translate",;
		"region","endregion","using","line","pragma","error","warning"}
		FOR n := 1 UPTO aWords:Length
			VoBuffer.oVoParsingOptions:oDirectives:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT

		aWords := <STRING>{;
		"DO","CASE","OTHERWISE","FOR","WHILE","IF","ELSE","ELSEIF",;
		"BEGIN","RECOVER","FINALLY"}
		FOR n := 1 UPTO aWords:Length
			VoBuffer.oVoParsingOptions:oIndent:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT

		aWords := <STRING>{;
		"END","ENDIF","ENDCASE","ENDDO","NEXT","ELSE","ELSEIF",;
		"OTHERWISE","BEGIN","RECOVER","FINALLY"}
		FOR n := 1 UPTO aWords:Length
			VoBuffer.oVoParsingOptions:oOutdent:Add(aWords[n]:ToUpper() , aWords[n])
		NEXT

		VoBuffer.oVoParsingOptions:oFunctions:Add("SLEN" , "SLen")

	PROTECTED STATIC METHOD InitHashTables() AS VOID
		LOCAL cBreak AS STRING
		LOCAL n AS INT
		cBreak := e",./;:[]<>?{}`~!@#$%^&*()-=\\+|'\""
		hBrk := Dictionary<Char,Char>{50}
		FOR n := 0 UPTO cBreak:Length - 1
			hBrk:Add(cBreak[n] , cBreak[n])
		NEXT
	RETURN

	PROTECTED CONSTRUCTOR(_aLines AS List<LineObject>)
		SELF:aLines := _aLines
		SELF:oParsingOptions := VoBuffer.VoParsingOptions
	RETURN

	STATIC METHOD CreateBuffer(aLines AS List<LineObject>) AS VOBuffer
	RETURN VoBuffer{aLines}
	
	ACCESS Count AS INT
	RETURN SELF:aLines:Count
						
	METHOD FullParse() AS LOGIC
		SELF:Parse(BufferParseItems.Entities + BufferParseItems.Tokens)
	RETURN TRUE

	STATIC METHOD ParseLine(cLine AS STRING) AS ArrayList
		LOCAL oBuffer AS VOBuffer
		LOCAL aLines AS List<LineObject>
		aLines := List<LineObject>{1}
		aLines:Add(LineObject{cLine})
		oBuffer := VOBuffer.CreateBuffer(aLines)
	RETURN oBuffer:ParseLine(1)
	METHOD ParseLine(nLine AS INT) AS ArrayList
	RETURN SELF:Parse(BufferParseItems.Words , nLine , nLine , 0)

	PROTECTED METHOD Parse(eItems AS BufferParseItems) AS ArrayList
	RETURN SELF:Parse(eItems , 0 , 0 , 0)
	PROTECTED METHOD Parse(eItems AS BufferParseItems , nStartLine AS INT , nEndLine AS INT , nExceptLine AS INT) AS ArrayList
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
		LOCAL lFindingType AS LOGIC
		LOCAL lFindingName AS LOGIC
		LOCAL sFoundType AS System.Text.StringBuilder
		LOCAL lNewCommandInLine AS LOGIC
		LOCAL lContinueNextLine AS LOGIC
		LOCAL lBeforeLexerChange AS LOGIC
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
		LOCAL n,n1 AS INT
		
		LOCAL aRet AS ArrayList
		LOCAL oWord AS WordObject
		LOCAL lMustLoop AS LOGIC
		LOCAL cCharBeforeWord AS Char
		LOCAL cWordBeforeSpace AS STRING
		LOCAL sLine AS System.Text.StringBuilder
		LOCAL lFirstLineAmpersand AS LOGIC
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
		LOCAL cClassType AS STRING

		LOCAL hVis , hEnt AS Dictionary<STRING,STRING>
		LOCAL nCaseReserved AS INT
		LOCAL nCaseFunctions AS INT
		
		LOCAL lCanStartBracketString AS LOGIC
		
		LOCAL _lLinesModified AS LOGIC
		LOCAL _lEntities AS LOGIC
		LOCAL _lRetrieveEntities AS LOGIC
		LOCAL _lRetrieveLocals AS LOGIC
		LOCAL _lFields AS LOGIC
		LOCAL _lTokens AS LOGIC
		LOCAL _lWords AS LOGIC
		LOCAL _lFinalize AS LOGIC
		LOCAL _lAddTyped AS LOGIC
		
#endregion
		_lLinesModified := _And(eItems , BufferParseItems.LinesModified) != 0
		_lEntities := _And(eItems , BufferParseItems.Entities) != 0
		_lRetrieveEntities := _And(eItems , BufferParseItems.RetrieveEntities) != 0
		_lRetrieveLocals := _And(eItems , BufferParseItems.RetrieveLocals) != 0
//		_lFields := _And(eItems , BufferParseItems.Fields) != 0
		_lTokens := _And(eItems , BufferParseItems.Tokens) != 0
		_lWords := _And(eItems , BufferParseItems.Words) != 0
		_lFinalize := _And(eItems , BufferParseItems.FinalizeLine) != 0
		_lAddTyped := _And(eItems , BufferParseItems.AddTyped) != 0
		IF _lRetrieveEntities .or. _lEntities
			_lFields := TRUE
		END IF
		IF _lRetrieveLocals
			_lRetrieveLocals := _lRetrieveLocals
		END IF
		
#region init
		aFields := ArrayList{}
		aLocals := ArrayList{}
		cShortClassName := ""
		cTypedClassName := ""
		cClassType := ""
		
		hEnt := SELF:oParsingOptions:oEntityMarkers
		hVis := SELF:oParsingOptions:oEntityVisibility
		nCaseReserved := 0
		nCaseFunctions := 0
		
		cWordBeforeSpace := NULL
		IF _lWords .or. _lRetrieveEntities .or. _lRetrieveLocals
			aRet := ArrayList{}
		END IF
		IF _lFinalize
			sLine := System.Text.StringBuilder{}
		ENDIF
		
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
						cClassType := oLine:LastEntity:cClassType
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
		
		sWord := System.Text.StringBuilder{20}
		sFoundType := System.Text.StringBuilder{20}

//		nLine := 0
		nLine := nStartLine
		DO WHILE nLine <= nEndLine
		
			// Line parsing
			oLine := SELF:aLines[nLine - 1]
			oLine:lInAmpersand := lContinueNextLine
			oLine:lInBlockComment := eLexer == LexerStep.BlockComment
			oLine:nFirstWordCol := 0
			cLine := oLine:LineText

			IF _lEntities
				oLine:oEntity := NULL
				oLine:oMoreInfo := NULL
				IF _lFields
					oLine:ClearFields()
				ENDIF
			END IF
			IF _lTokens
				oLine:eType := LineType.None
				oLine:cArgument := NULL
			END IF
			IF _lFinalize
				sLine:Length := 0
			END IF
			IF _lEntities .or. _lRetrieveEntities .or. _lTokens //.or. _lRetrieveLocals
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
				lCanStartBracketString := TRUE
				nBracketCount := 0
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
				IF cOldChar == ';' .and. eLexer == LexerStep.None .and. (.not. state:lDirective)
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
						lCanStartBracketString := TRUE
						state:Reset()
						nBracketCount := 0
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
						cWordBeforeSpace := NULL
						oInfo := NULL
						nEntityStartLine := 0
						nEntityStartCol := 0
						IF _lEntities .or. _lRetrieveEntities .or. _lTokens //.or. _lRetrieveLocals
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

				CASE cChar == '"' .and. eLexer != LexerStep.Quote .and. eLexer != LexerStep.BracketQuote
					IF eLexer == LexerStep.DoubleQuote
						eLexer := LexerStep.None
						eWordStatus := WordStatus.Literal
						eWordSubStatus := WordSubStatus.LiteralString
						eCharStatus := WordStatus.Literal
						eCharSubStatus := WordSubStatus.LiteralString
						lMustLoop := TRUE
					ELSE
						eLexer := LexerStep.DoubleQuote
						eWordStatus := WordStatus.Text
						eWordSubStatus := WordSubStatus.Text
						eCharStatus := WordStatus.Literal
						eCharSubStatus := WordSubStatus.LiteralString
						lBeforeLexerChange := TRUE
						cChar := ' '
					END IF
				CASE cChar == '\'' .and. eLexer != LexerStep.DoubleQuote .and. eLexer != LexerStep.BracketQuote
					IF eLexer == LexerStep.Quote
						eLexer := LexerStep.None
						eWordStatus := WordStatus.Literal
						eWordSubStatus := WordSubStatus.LiteralString
						eCharStatus := WordStatus.Literal
						eCharSubStatus := WordSubStatus.LiteralString
						lMustLoop := TRUE
					ELSE
						eLexer := LexerStep.Quote
						eWordStatus := WordStatus.Text
						eWordSubStatus := WordSubStatus.Text
						eCharStatus := WordStatus.Literal
						eCharSubStatus := WordSubStatus.LiteralString
						lBeforeLexerChange := TRUE
						cChar := ' '
					END IF

				CASE cChar == '[' .and. lCanStartBracketString .and. eLexer != LexerStep.Quote .and. eLexer != LexerStep.DoubleQuote .and. eLexer != LexerStep.BracketQuote
					eLexer := LexerStep.BracketQuote
					eWordStatus := WordStatus.Text
					eWordSubStatus := WordSubStatus.Text
					eCharStatus := WordStatus.Literal
					eCharSubStatus := WordSubStatus.LiteralString
					lBeforeLexerChange := TRUE
					cChar := ' '
				CASE cChar == ']' .and. eLexer == LexerStep.BracketQuote
					eLexer := LexerStep.None
					eWordStatus := WordStatus.Literal
					eWordSubStatus := WordSubStatus.LiteralString
					eCharStatus := WordStatus.Literal
					eCharSubStatus := WordSubStatus.LiteralString
					lMustLoop := TRUE

				CASE eLexer == LexerStep.DoubleQuote .or. eLexer == LexerStep.BracketQuote
					eWordStatus := WordStatus.Literal
					eWordSubStatus := WordSubStatus.LiteralString
					eCharStatus := WordStatus.Literal
					eCharSubStatus := WordSubStatus.LiteralString
					lMustLoop := TRUE
				CASE eLexer == LexerStep.Quote
					eWordStatus := WordStatus.Literal
					eWordSubStatus := WordSubStatus.LiteralString
					eCharStatus := WordStatus.Literal
					eCharSubStatus := WordSubStatus.LiteralString
					lMustLoop := TRUE
				CASE (cChar == '/' .and. cOldChar == '/') .or. (state:lFirstChar .and. cChar = '*')
					eLexer := LexerStep.Comment
					eWordStatus := WordStatus.Text
					eWordSubStatus := WordSubStatus.Text
					eCharStatus := WordStatus.Comment
					eCharSubStatus := WordSubStatus.CommentLine
					IF _lWords .or. _lTokens .or. _lFinalize // no _lAddTyped
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
				
				IF eLexer == LexerStep.None
					IF cChar != ' ' .and. cChar != '\t'
						lCanStartBracketString := "({+-=,?":IndexOf(cChar) != -1
					END IF
				ELSE
					lCanStartBracketString := FALSE
				END IF

				IF nBracketCount == 0
					NOP
				ELSE
					IF cChar == cBracketOpen
						nBracketCount ++
					ELSEIF cChar == cBracketClose
						nBracketCount --
					END IF
				ENDIF
				
				IF .not. (_lWords .or. _lTokens .or. _lFinalize .or. _lAddTyped)
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
				IF .not. lIsSpaceChar .and. nEntityStartLine == 0
					nEntityStartLine := nLine
					nEntityStartCol := nChar - 1
				END IF
				IF lIsSpaceChar
					IF sWord:Length == 0
						IF .not. _lWords .and. .not. _lFinalize .and. .not. (_lTokens .and. (cRealChar == ' ' .or. cRealChar == '\t') )
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
					IF oStatementLine:nFirstWordCol == 0
						oStatementLine:nFirstWordCol := nChar - sWord:Length
					END IF
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
						IF _lTokens
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
							OTHERWISE
								oStatementLine:eType := LineType.OtherDirective
							END CASE
						END IF
					ELSE
						eWordStatus := WordStatus.Literal
						eWordSubStatus := WordSubStatus.LiteralSymbol
					ENDIF
				END IF

				IF _lWords .or. _lFinalize .or. _lAddTyped
					IF cWord:Length != 0
						IF eWordStatus == WordStatus.Text
							DO CASE
							CASE .not. lEscapedWord .and. .not. state:lDirective .and. cWord:Length > 1 .and. SELF:oParsingOptions:oReserved:ContainsKey(cUpperWord)
								IF .not. (cCharBeforeWord == '.' .or. /*cCharBeforeWord == ':' .or. */cChar == '.')
									LOCAL lReserved AS LOGIC
									lReserved := TRUE
									IF lReserved
										eWordSubStatus := WordSubStatus.TextReserved
										DO CASE
										CASE nCaseReserved==1
											cWord := cUpperWord
										CASE nCaseReserved==2
											cWord := cWord:ToLower()
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
							oWord:eStatus :=  eWordStatus
							oWord:eSubStatus :=  eWordSubStatus
							aRet:Add(oWord)
						END IF
						IF _lFinalize
							sLine:Append(cWord)
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
							aRet:Add(oWord)
						END IF
						IF _lFinalize
							sLine:Append(cUseChar)
						END IF
						IF .not. lIsSpaceChar
							cWordBeforeSpace := cUseChar
						END IF
					ENDIF
				ENDIF
				
				LOCAL lAllowEntityParse AS LOGIC
				lAllowEntityParse := (_lEntities .or. _lRetrieveEntities .or. _lRetrieveLocals) .and. .not. state:lIgnore

				DO CASE
				CASE eLexer == LexerStep.BlockComment
					sWord:Length := 0
					lEscapedWord := FALSE
					LOOP

				CASE eWordStatus == WordStatus.Literal .or. eWordStatus == WordStatus.Comment .or. eWordSubStatus == WordSubStatus.TextDirective

				CASE (lIsSpaceChar .or. cChar == ';') .and. sWord:Length == 0
					lEscapedWord := FALSE
					LOOP
				CASE (state:lFirstChar .and. eWordStatus == WordStatus.Text .and. eCharStatus == WordStatus.Comment) .and. sWord:Length == 0
					lEscapedWord := FALSE
					LOOP
				CASE .not. (_lEntities .or. _lRetrieveEntities .or. _lRetrieveLocals) .and. .not. _lTokens

				CASE lAllowEntityParse .and. state:lEntityFound .and. state:lNameFound .and. cUpperWord == "PASCAL" .and. .not. lEscapedWord .and. ;
					(oInfo:eType == EntityType._Method .or. oInfo:eType == EntityType._Access .or. oInfo:eType == EntityType._Assign .or. ;
					oInfo:eType == EntityType._Function .or. oInfo:eType == EntityType._Procedure)
					oLine:SetPascalClauseStart(nChar - 6)

				CASE lAllowEntityParse .and. state:lEntityFound .and. VoBuffer.IsVOKeyword(cUpperWord , "CLASS") .and. .not. lEscapedWord
					eStep := ParseStep.AfterClassClause
					sFoundType:Length := 0
					n1 := 0
					oLine:SetClassClauseStart(nChar - 5)

				CASE lAllowEntityParse .and. eStep == ParseStep.AfterExportClause
					IF cUpperWord == "LOCAL"
						oLine:SetExportLocalClauseEnd(nChar)
					END IF
					eStep := ParseStep.None
					state:lIgnore := TRUE

				CASE lAllowEntityParse .and. state:lEntityFound .and. cUpperWord == "EXPORT" .and. .not. lEscapedWord
					eStep := ParseStep.AfterExportClause
					oLine:SetExportLocalClauseStart(nChar - 6)

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
((hVis:ContainsKey(cUpperWord) .and. eStep != ParseStep.AfterBegin) .or. (_lRetrieveLocals .and. ;
	(cUpperWord == "LOCAL" .or. cUpperWord == "CATCH")) )
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
					END CASE
					IF System.Array.IndexOf(<STRING>{"PROTECT" , "PROTECTED", "INSTANCE" , "EXPORT" , "PUBLIC" , "PRIVATE" , "HIDDEN" , "INTERNAL" , "MEMBER" , "GLOBAL"} , cUpperWord) != -1
						// Allow multiple names in same line
						state:lField := TRUE
					END IF
					IF cUpperWord == "LOCAL"
						state:lLocal := TRUE
					END IF
				CASE lAllowEntityParse .and. state:lFirstWord .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. cUpperWord == "TEXTBLOCK"
					state:lField := FALSE
					state:lLocal := FALSE
					state:lEntityFound := TRUE
					state:lEntityIsClass := FALSE
					oInfo := EntityObject{}
					oInfo:eType := EntityType._TextBlock
					state:lNameFound := TRUE
					state:lIgnore := TRUE
					oInfo:nLine := nEntityStartLine
					oInfo:nCol := nEntityStartCol
					LOCAL cTextBlock AS STRING
					cTextBlock := "<Textblock>"
					IF nChar < cLine:Length
						cTextBlock := cLine:Substring(nChar):Trim()
					END IF
					oInfo:cName := cTextBlock
					oInfo:cShortClassName := ""
					oInfo:cTypedClassName := ""
					oInfo:cClassNameSpace := ""
					oInfo:cClassType := ""
					IF _lEntities
						oStatementLine:oEntity := oInfo
					ELSEIF _lRetrieveEntities
						aRet:Add(oInfo)
					END IF

				CASE lAllowEntityParse .and. .not. lEscapedWord .and. cChar != '.' .and. cCharBeforeWord != '.' .and. hEnt:ContainsKey(cUpperWord) .and. cUpperWord != "TEXTBLOCK"
					state:lField := FALSE
					state:lLocal := FALSE
					IF state:lEntityFound
						state:lIgnore := TRUE
					ENDIF
					state:lEntityFound := TRUE
//					state:lEntityIsClass := System.Array.IndexOf(<STRING>{"CLASS","STRUCTURE","STRUCT","VOSTRUCT","UNION"} , cUpperWord) != -1
					state:lEntityIsClass := VoBuffer.IsVOKeyword(cUpperWord , "CLASS") .or. ;
											VoBuffer.IsVOKeyword(cUpperWord , "STRUCTURE") .or. ;
											VoBuffer.IsVOKeyword(cUpperWord , "VOSTRUCT") .or. ;
											VoBuffer.IsVOKeyword(cUpperWord , "UNION")
					IF eStep == ParseStep.AfterEnd .and. state:lEntityIsClass
						oStatementLine:eType := LineType.EndClass
						state:lEntityFound := FALSE
						state:lEntityIsClass := FALSE
						state:lIgnore := TRUE
						cShortClassName := ""
						cTypedClassName := ""
						cClassType := ""
					ELSE
						oInfo := EntityObject{}
						oInfo:eType := GetEntityType(cUpperWord)
						IF state:lEntityIsClass
							cClassType := cUpperWord
							DO CASE
							CASE VoBuffer.IsVOKeyword(cUpperWord , "CLASS")
//								oInfo:cInherit := "System.Object"
								oInfo:cInherit := ""
								oInfo:cImplements := ""
							CASE VoBuffer.IsVOKeyword(cUpperWord , "STRUCTURE")
//								oInfo:cInherit := "System.ValueType"
								oInfo:cInherit := ""
								oInfo:cImplements := ""
							OTHERWISE
								oInfo:cInherit := ""
								oInfo:cImplements := ""
							END CASE
						ELSE
							IF oInfo:eType == EntityType._Method .or. oInfo:eType == EntityType._Access .or. ;
								oInfo:eType == EntityType._Function .or. oInfo:eType == EntityType._Global .or. oInfo:eType == EntityType._Define
								oInfo:cRetType := "USUAL" // Default
							ELSE
								oInfo:cRetType := ""
							END IF
							IF oInfo:eType != EntityType._Method .and. oInfo:eType != EntityType._Access .and. oInfo:eType != EntityType._Assign
								cShortClassName := ""
								cTypedClassName := ""
								cClassType := ""
								oInfo:cShortClassName := ""
								oInfo:cTypedClassName := ""
							ENDIF
						END IF
						oInfo:lStatic := lStatic
						oInfo:eAccessLevel := eAccessLevel
						oInfo:eModifiers := eModifiers
					END IF
				CASE lAllowEntityParse .and. (state:lEntityFound)
					DO CASE
/*					CASE cUpperWord == "CLASS" .and. .not. lEscapedWord
						eStep := ParseStep.AfterClassClause
						sFoundType:Length := 0
						n1 := 0*/
					CASE cUpperWord == "INHERIT" .and. .not. lEscapedWord
						eStep := ParseStep.AfterInherit
						sFoundType:Length := 0
						n1 := 0
					CASE cUpperWord == "AS" .and. .not. lEscapedWord
						eStep := ParseStep.AfterAs
						sFoundType:Length := 0
						n1 := 0
					CASE state:lInParams .and. (cUpperWord == "REF" .or. cUpperWord == "OUT") .and. .not. lEscapedWord
						eStep := ParseStep.AfterRef
						sFoundType:Length := 0
						n1 := 0
					CASE eStep == ParseStep.AfterInherit .or. eStep == ParseStep.AfterClassClause .or. ;
						eStep == ParseStep.AfterAs .or. eStep == ParseStep.AfterRef .or. ;
						.not. state:lNameFound
						
						IF eStep == ParseStep.AfterInherit .or. eStep == ParseStep.AfterClassClause .or. ;
							eStep == ParseStep.AfterAs .or. eStep == ParseStep.AfterRef
							// Waiting for type that may be generic, array
							lFindingType := TRUE
							sFoundType:Append(sWord:ToString())
							sWord:Length := 0
							IF FALSE//lBeforeLexerChange
								LOOP
							ELSE
								DO WHILE nChar < nLineLen .and. (cChar == ' ' .or. cChar == '\t')
									cOldOldChar := cOldChar
									cOldChar := cChar
									cChar := cLine[nChar]
									IF cChar != ' ' .and. cChar != '\t' .and. cChar != '['
										cChar := ' '
										EXIT
									END IF
									nChar ++
									IF _lWords .or. _lFinalize
										cUseChar := cChar:ToString()
										IF _lWords
											oWord := WordObject{cUseChar}
											oWord:nStart := nChar
											oWord:nEnd := nChar
											oWord:eStatus := WordStatus.Text
											oWord:eSubStatus := WordSubStatus.Text
											aRet:Add(oWord)
										END IF
										IF _lFinalize
											sLine:Append(cUseChar)
										END IF
									END IF
								END DO
							END IF
							DO CASE
							CASE cChar == '['
								sFoundType:Append('[')
								n1 ++
							CASE cChar == ']'
								IF n1 > 0
									sFoundType:Append(']')
									n1 --
								END IF
							CASE n1 != 0 .and. cChar != ' ' .and. cChar != '\t' .and. cChar != ';'
								sFoundType:Append(cChar)
							END CASE
							IF cChar == '.' .or. n1 != 0 .or. (cChar == ']' .and. nChar < nLineLen .and. cLine[nChar] == '[') // more dims
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
							cShortClassName := cWord
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
							eStep := ParseStep.None
							state:lIgnore := TRUE
						CASE eStep == ParseStep.AfterClassClause
							oLine:SetClassClauseEnd(nChar)
							cTypedClassName := cWord
							cShortClassName := cWord
							oInfo:cShortClassName := cWord
							oInfo:cTypedClassName := cWord
							eStep := ParseStep.None
//							state:lIgnore := TRUE // oxi, gia na brei kai ta EXPORT LOCAL
						CASE eStep == ParseStep.AfterAs
							LOCAL cRetType AS STRING
							IF state:lDimDeclaration
								cRetType := cWord + "[]"
							ELSE
								cRetType := cWord
							END IF
							oInfo:cRetType := cRetType
							IF state:lField
								FOR n := 0 UPTO aFields:Count - 1
									((EntityObject)aFields[n]):cRetType := cRetType
								NEXT
							ELSEIF state:lLocal
								FOR n := 0 UPTO aLocals:Count - 1
									((EntityObject)aLocals[n]):cRetType := cRetType
								NEXT
							END IF
							IF oInfo:eType == EntityType._Method .or. oInfo:eType == EntityType._Access .or. oInfo:eType == EntityType._Assign
//								eStep := ParseStep.WaitClassClause
								eStep := ParseStep.None
							ELSEIF oInfo:eType == EntityType._Procedure .or. oInfo:eType == EntityType._Function
								// Wste na pianei to AS <type> PASCAL
								eStep := ParseStep.None
							ELSE
								eStep := ParseStep.None
								state:lIgnore := TRUE
							END IF
						CASE .not. state:lNameFound
							IF (.not. state:lField .or. _lFields) .and. (.not. state:lLocal .or. _lRetrieveLocals)
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
									oInfo:cClassNameSpace := ""
									oInfo:cClassType := cClassType
								END IF
								IF _lEntities
									oStatementLine:oEntity := oInfo
//									oStatementLine:lEntity := TRUE
								ELSEIF _lRetrieveEntities
									aRet:Add(oInfo)
								END IF
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
					ELSEIF state:lInParams .and. cChar == ')'
						state:lInParams := FALSE
					END IF
//					lIgnore := TRUE
				CASE state:lFirstWord .and. cUpperWord == "BEGIN"
					eStep := ParseStep.AfterBegin
				CASE eStep == ParseStep.AfterBegin
					IF _lTokens
						DO CASE
						CASE cUpperWord == "SEQUENCE"
							oStatementLine:eType := LineType.TokenIn
//							oStatementLine:cArgument := "BEGIN_SEQUENCE"
							oStatementLine:cArgument := "BEGIN"
						END CASE
					ELSE
						state:lIgnore := TRUE
					END IF
				CASE state:lFirstWord .and. cUpperWord == "END"
					eStep := ParseStep.AfterEnd
					IF _lTokens
						oStatementLine:eType := LineType.TokenOut
						oStatementLine:cArgument := "END"
					END IF
				CASE eStep == ParseStep.AfterEnd
					state:lIgnore := TRUE
				
//				CASE .not. lIsSpaceChar .and. .not. (cChar == ',' .and. state:lField)
				CASE lAllowEntityParse .and. ;  // 2nd .not. is for IF(logic) CASE(something) etc syntax (paren after IF/CASE etc)
					.not. (.not. lEscapedWord .and. cRealChar == '(' .and. System.Array.IndexOf(<STRING>{"IF", "ELSEIF", "WHILE", "CASE", "FOR"} , cUpperWord) != -1) .and. ;
					.not. (lIsSpaceChar .or. cRealChar == ';' .or. lBeforeLexerChange .or. lContinueNextLine) .and. .not. (state:lField .or. state:lLocal)
					state:lIgnore := TRUE
				CASE lAllowEntityParse .and. (state:lField .or. state:lLocal) .and. .not. state:lNameFound .and. cWord == "DIM" .and. .not. lEscapedWord
					state:lDimDeclaration := TRUE
				CASE lAllowEntityParse .and. (state:lField .or. state:lLocal) .and. .not. state:lNameFound
					state:lNameFound := TRUE
					state:lEntityFound := TRUE
					IF _lFields .or. _lRetrieveLocals
						oInfo := EntityObject{}
						IF state:lLocal
							oInfo:eType := EntityType._Local
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
						IF state:lLocal
							aLocals:Add(oInfo)
						ELSE
							aFields:Add(oInfo)
						END IF
						IF cChar == ','
							state:lNameFound := FALSE
							state:lEntityFound := FALSE
						END IF
						oInfo:nLine := nLine
						oInfo:cName := cWord
						oInfo:cShortClassName := cShortClassName
						oInfo:cTypedClassName := cTypedClassName
						oInfo:cClassNameSpace := ""
						oInfo:cClassType := cClassType
						IF _lFields .and. state:lField
							IF _lEntities
								oStatementLine:AddField(oInfo)
							ELSEIF _lRetrieveEntities
								aRet:Add(oInfo)
							END IF
						END IF
						IF _lRetrieveLocals .and. state:lLocal
							aRet:Add(oInfo)
						END IF
					ELSE
						state:lIgnore := TRUE
						state:lField := FALSE
						state:lLocal := FALSE
					END IF
				CASE state:lFirstWord .or. state:lFirstChar
					IF _lTokens
						IF System.Array.IndexOf(<STRING>{"IF", "ELSE", "ELSEIF", "DO", "WHILE", "CASE", "OTHERWISE", "FOR", "FINALLY", "RECOVER"} , cUpperWord) != -1
							oStatementLine:eType := LineType.TokenIn
							oStatementLine:cArgument := cUpperWord
						ELSEIF System.Array.IndexOf(<STRING>{"ENDIF", "ENDDO", "ENDCASE", "NEXT"} , cUpperWord) != -1
							oStatementLine:eType := LineType.TokenOut
							oStatementLine:cArgument := cUpperWord
						ELSE
							IF .not. lContinueNextLine
								state:lIgnore := TRUE
							END IF
						END IF
					END IF
				END CASE


				IF (cChar != '#' .or. sWord:Length != 0)
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
			IF _lFinalize// .and. nLine != nExceptLine
				oLine:LineCased := sLine:ToString()
//				PyrgasIdeBase.Ide:Text := oLine:LineCased
			END IF
			
			nLine ++
			
			IF _lLinesModified .and. nLine > nEndLine .and. nLine <= SELF:aLines:Count
				oLine := SELF:aLines[nLine - 1]
				IF (oLine:lInBlockComment .and. eLexer != LexerStep.BlockComment) .or. ;
					(.not. oLine:lInBlockComment .and. eLexer == LexerStep.BlockComment) .or. ;
					oLine:lInAmpersand .or. lContinueNextLine
					nEndLine ++
					lMoreModified := TRUE
//					PyrgasIdeBase.Ide:Text += "|B|"
/*					IF PyrgasIdeBase.Ide:Text:Length > 256
						PyrgasIdeBase.Ide:Text := ""
					ENDIF*/
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
							CASE oLine:IsEndClass
								cShortClassName := ""
								cTypedClassName := ""
								cClassType := ""

							CASE oLine:aFields != NULL .and. oLine:aFields:Count != 0
								lLineNumsVerified := oLine:VerifyEntitiesLineNum(nTestLine , FALSE)
								IF oLine:aFields[0]:cTypedClassName == cTypedClassName //.and. oLine:aFields[0]:NamespacesEqual(aNamespaces)
									lMustExit := TRUE
									EXIT
								ELSE
									FOR n := 0 UPTO oLine:aFields:Count - 1
										oLine:aFields[n]:cShortClassName := cShortClassName
										oLine:aFields[n]:cTypedClassName := cTypedClassName
										oLine:aFields[n]:cClassNamespace := ""
										oLine:aFields[n]:cClassType := cClassType
									NEXT
								END IF
								
							CASE oLine:lEntity
								lLineNumsVerified := oLine:VerifyEntitiesLineNum(nTestLine , FALSE)
								IF oLine:oEntity:IsType
									cShortClassName := oLine:oEntity:cShortClassName
									cTypedClassName := oLine:oEntity:cTypedClassName
									cClassType := oLine:oEntity:cClassType
									nAt := cTypedClassName:LastIndexOf('.')
								END IF
								IF ( (oLine:oEntity:IsFuncProcGlobal) .or. ;
									(.not. oLine:oEntity:IsFuncProcGlobal .and. oLine:oEntity:cTypedClassName == cTypedClassName) )
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
										oLine:oEntity:cClassNamespace := ""
										oLine:oEntity:cClassType := cClassType
									END IF
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
		DO CASE
		CASE VoBuffer.IsVOKeyword(cWord , "METHOD")
			eType := EntityType._Method
		CASE VoBuffer.IsVOKeyword(cWord , "CLASS")
			eType := EntityType._Class
		CASE VoBuffer.IsVOKeyword(cWord , "ACCESS")
			eType := EntityType._Access
		CASE VoBuffer.IsVOKeyword(cWord , "ASSIGN")
			eType := EntityType._Assign
		CASE VoBuffer.IsVOKeyword(cWord , "FUNCTION")
			eType := EntityType._Function
		CASE VoBuffer.IsVOKeyword(cWord , "PROCEDURE")
			eType := EntityType._Procedure
		CASE VoBuffer.IsVOKeyword(cWord , "STRUCTURE")
			eType := EntityType._Structure
		CASE VoBuffer.IsVOKeyword(cWord , "VOSTRUCT")
			eType := EntityType._VOStruct
		CASE VoBuffer.IsVOKeyword(cWord , "UNION")
			eType := EntityType._Union
		CASE VoBuffer.IsVOKeyword(cWord , "GLOBAL")
			eType := EntityType._Global
		CASE VoBuffer.IsVOKeyword(cWord , "DEFINE")
			eType := EntityType._Define
		CASE cWord == "RESOURCE"
			eType := EntityType._Resource
		CASE cWord == "TEXTBLOCK"
			eType := EntityType._TextBlock
		END CASE
	RETURN eType
	
	STATIC PROTECT oVoParsingOptions AS ParsingOptions
	STATIC ACCESS VoParsingOptions AS ParsingOptions
	RETURN VoBuffer.oVoParsingOptions
	
	STATIC METHOD IsVOKeyword(cWord AS STRING, cKeyword AS STRING) AS LOGIC
		IF cWord == cKeyword
			RETURN TRUE
		END IF
		DO WHILE cKeyword:Length > 4
			cKeyword := cKeyword:Substring(0 , cKeyword:Length - 1)
			IF cWord == cKeyword
				RETURN TRUE
			END IF
		END DO
	RETURN FALSE
	
END CLASS

END NAMESPACE
