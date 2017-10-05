#using System.Collections.Generic
#using System.Collections.Generic
#using System.Collections

USING Xide

BEGIN NAMESPACE Xide

CLASS AbstractEditor
	EXPORT aLines AS List<LineObject>
	PROTECT nStart,nEnd AS INT
	PROTECT oCaretPos AS System.Drawing.Point
	PROTECT oBuffer AS EditorBuffer
	CONSTRUCTOR(_oBuffer AS EditorBuffer)
		SELF:oBuffer := _oBuffer
		SELF:oBuffer:FullParse()
		SELF:aLines := SELF:oBuffer:aLines
	RETURN
	
	METHOD BeginCode() AS VOID
		SELF:nStart := 0
		SELF:nEnd := 0
		IF SELF:oBuffer != NULL
			SELF:oBuffer:BeginAction()
		END IF
	RETURN

	METHOD EndCode() AS VOID
		IF SELF:oBuffer != NULL
			SELF:oBuffer:EndAction()
		END IF
	RETURN

	METHOD ModifiedLine(nLine AS INT) AS VOID
		IF SELF:nStart == 0 .or. SELF:nStart > nLine
			SELF:nStart := nLine
		ENDIF
		IF SELF:nEnd == 0 .or. SELF:nEnd < nLine
			SELF:nEnd := nLine
		ENDIF
	RETURN

	METHOD CheckCreatedCode() AS VOID
		IF SELF:nStart != 0
			SELF:oBuffer:LinesModified(0,0,0)
		ENDIF
		SELF:nStart := 0
		SELF:nEnd := 0
	RETURN
	
	METHOD AddLine(cLine AS STRING) AS VOID
		SELF:oBuffer:AddLine(cLine)
	RETURN

	METHOD InsertLine(cLine AS STRING , nLine AS INT) AS VOID
		SELF:oBuffer:InsertLine(nLine , cLine)
	RETURN

	METHOD ReplaceLine(cLine AS STRING , nLine AS INT) AS VOID
		SELF:oBuffer:ChangeLine(nLine , cLine)
	RETURN

	METHOD RemoveLine(nLine AS INT) AS VOID
		SELF:oBuffer:DeleteLine(nLine)
	RETURN

	METHOD DeleteAllLines() AS VOID
		SELF:oBuffer:DeleteAllLines()
	RETURN
	
END CLASS


PARTIAL CLASS CodeGenerator
	PROTECT aLines AS List<LineObject>
	PROTECT nStart,nEnd AS INT
	PROTECT oCaretPos AS System.Drawing.Point
	PROTECT oEditor , oBaseEditor , oDesEditor AS AbstractEditor

	CONSTRUCTOR(oBuffer AS EditorBuffer)
		SELF:aLines := oBuffer:aLines
		SELF:oBaseEditor := AbstractEditor{oBuffer}
	RETURN


	METHOD BeginCode() AS LOGIC
		SELF:oDesEditor := SELF:oBaseEditor
		SELF:oEditor := SELF:oBaseEditor
		SELF:oEditor:BeginCode()
	RETURN FALSE

	METHOD EndCode() AS VOID
		SELF:oEditor:EndCode()
	RETURN

METHOD ModifiedLine(nLine AS INT) AS VOID
	SELF:oEditor:ModifiedLine(nLine)
RETURN

METHOD CheckCreatedCode() AS VOID
	SELF:oEditor:CheckCreatedCode()
RETURN
METHOD GetLine(nLine AS INT) AS LineObject
	RETURN SELF:aLines[nLine - 1]



METHOD EntityExists(eType AS EntityType,cName AS STRING,cClass AS STRING) AS LOGIC
LOCAL nDummy AS INT
SELF:FindEntity(eType,cName,cClass,nDummy)
RETURN nDummy!=0

METHOD EntityExistsAnywhere(eType AS EntityType , cName AS STRING , cClass AS STRING) AS LOGIC
RETURN FALSE

METHOD FindEntity(eType AS EntityType,cName AS STRING,cClass AS STRING,nLine REF Int32) AS LOGIC
LOCAL oLine AS LineObject
LOCAL oEntity AS EntityObject
cName := cName:ToUpper():Trim()
cClass := cClass:ToUpper():Trim()
FOR nLine:=1 UPTO SELF:aLines:Count
	oLine:=SELF:GetLine(nLine)
	IF oLine:lEntity
		oEntity:=oLine:oEntity
		IF oEntity:eType == eType .and. oEntity:cTypedClassName:ToUpper() == cClass
			IF eType == EntityType._Constructor .or. oEntity:cName:ToUpper() == cName
				RETURN FALSE
			ENDIF
		END IF
	END IF
NEXT
nLine:=0
RETURN FALSE

METHOD DeleteEntity(eType AS EntityType,cName AS STRING,cClass AS STRING) AS LOGIC
	LOCAL nLine AS INT
RETURN SELF:DeleteEntity(eType , cName ,cClass , nLine)

METHOD DeleteEntity(eType AS EntityType,cName AS STRING,cClass AS STRING,nLine REF Int32) AS LOGIC
LOCAL oLine AS LineObject
LOCAL lExit AS LOGIC
LOCAL nOldLine AS INT
LOCAL cLine AS STRING
nOldLine := nLine
SELF:FindEntity(eType,cName,cClass,nLine)
IF nLine == 0
	nLine := nOldLine
	RETURN FALSE
ENDIF

DO WHILE nLine!=0
	SELF:RemoveWEDLine(nLine)
	SELF:ModifiedLine(nLine)
	IF nLine<=SELF:aLines:Count
		oLine:=SELF:GetLine(nLine)
		IF oLine:lEntity .or. lExit .or. oLine:IsEndClass
			EXIT
		END IF
		cLine := oLine:LineText:ToUpper()
		IF cLine:Contains("##USER##") .or. cLine:Contains("{{%UC%}}") .or. cLine:Contains("USER CODE STARTS")
			lExit := TRUE
		END IF
	ELSE
		EXIT
	END IF
END DO
SELF:CheckCreatedCode()
RETURN FALSE

METHOD AddInclude(cInclude AS STRING) AS VOID
	LOCAL oLine AS LineObject
	LOCAL nLine AS INT
	LOCAL n AS INT
	
	nLine := 1
	
	FOR n := 1 UPTO SELF:aLines:Count
		oLine := SELF:aLines[n - 1]
		DO CASE
		CASE oLine:lEntity
			nLine := n - 1
			EXIT
		CASE oLine:IsInclude .and. oLine:cArgument:ToUpper() == cInclude:ToUpper()
			RETURN
		END CASE
	NEXT
	IF nLine <= 0
		nLine := 1
	ENDIF
	
	SELF:AddWEDLine("#include " + e"\"" + cInclude + e"\"" , nLine)
	SELF:ModifiedLine(nLine)
RETURN

METHOD RemoveInclude(cInclude AS STRING) AS VOID
	FOR LOCAL nLine := 1 UPTO SELF:aLines:Count
		LOCAL oLine AS LineObject
		oLine := SELF:aLines[nLine - 1]
		DO CASE
		CASE oLine:lEntity
			RETURN
		CASE oLine:IsInclude .and. oLine:cArgument:ToUpper() == cInclude:ToUpper()
			SELF:RemoveWEDLine(nLine)
			SELF:ModifiedLine(nLine)
			RETURN
		END CASE
	NEXT
RETURN

METHOD AddLine(cLine AS STRING) AS VOID
	SELF:oEditor:AddLine(cLine)
	SELF:ModifiedLine(SELF:aLines:Count)
RETURN
METHOD InsertLine(nLine AS INT , cLine AS STRING) AS VOID
	SELF:oEditor:InsertLine(cLine , nLine)
	SELF:ModifiedLine(SELF:aLines:Count)
RETURN

METHOD AddWEDLine(cLine AS STRING,nLine REF Int32) AS LineObject
LOCAL oLine AS LineObject
IF nLine<=SELF:aLines:Count .and. nLine!=0
	SELF:oEditor:InsertLine(cLine , nLine)
	SELF:ModifiedLine(nLine)
ELSE
	SELF:oEditor:AddLine(cLine)
	nLine:=SELF:aLines:Count
	SELF:ModifiedLine(nLine)
END IF
oLine := SELF:GetLine(nLine)
nLine++
RETURN oLine
METHOD ReplaceWEDLine(cLine AS STRING,nLine AS Int32) AS LineObject
	SELF:oEditor:ReplaceLine(cLine , nLine)
	SELF:ModifiedLine(nLine)
RETURN SELF:GetLine(nLine)
METHOD RemoveWEDLine(nLine AS Int32) AS VOID
	SELF:oEditor:RemoveLine(nLine)
RETURN
METHOD Clear() AS VOID
	SELF:oEditor:DeleteAllLines()
RETURN

	STATIC PROTECT cUser := "// {{%UC%}} User code starts here (DO NOT remove this line)  " AS STRING
	STATIC PROTECT cTagU := "" AS STRING

	METHOD GetEntity(eType AS EntityType , cName AS STRING , cClass AS STRING) AS EntityObject
		LOCAL nLine AS INT
		nLine := SELF:FindEntity(eType , cName , cClass)
		IF nLine != 0
			RETURN SELF:GetLine(nLine):oEntity
		END IF
	RETURN NULL
	
	METHOD FindEntity(eType AS EntityType , cName AS STRING , cClass AS STRING) AS INT
		LOCAL nLine AS INT
		SELF:FindEntity(eType , cName , cClass , nLine)
	RETURN nLine

	METHOD WriteEntity(eType AS EntityType , cName AS STRING , cClass AS STRING , eOptions AS EntityOptions , aEntity AS List<STRING>) AS INT
		LOCAL oLine AS LineObject
		LOCAL nLine AS INT
		LOCAL nTemp AS INT
		LOCAL nFirst AS INT
		LOCAL lIsUser AS LOGIC
		LOCAL lRemoveFields AS LOGIC
		LOCAL n AS INT

		LOCAL cTab , ceTab AS STRING
		cTab := e"\t"
		ceTab := e"\t"
		
		IF aEntity:Count == 0
			RETURN 0
		END IF
		
		cClass := cClass:ToUpper()
		nLine := SELF:FindEntity(eType , cName , cClass)
		
		IF nLine == 0
			IF eType == EntityType._Class
				nLine := SELF:aLines:Count + 1
			ELSEIF eType == EntityType._Constructor
				nLine := SELF:FindEntity(EntityType._Class , cClass , cClass)
				IF nLine == 0
					RETURN 0
				END IF
				DO WHILE TRUE
					nLine ++
					IF nLine > SELF:aLines:Count
						EXIT
					END IF
					oLine := SELF:GetLine(nLine)
					IF oLine:lEntity .or. oLine:IsEndClass
						EXIT
					END IF
				END DO
			ELSE
				nLine := SELF:FindEntity(EntityType._Class , cClass , cClass)
				IF nLine == 0
					RETURN 0
				END IF
				DO WHILE TRUE
					nLine ++
					IF nLine > SELF:aLines:Count
						EXIT
					END IF
					oLine := SELF:GetLine(nLine)
					IF oLine:IsEndClass
						EXIT
					END IF
					IF oLine:lEntity .and. oLine:oEntity:cTypedClassName:ToUpper() != cClass
						EXIT
					END IF
				END DO
			END IF
			nFirst := nLine
		ELSE
			nFirst := nLine
			IF (eOptions & EntityOptions.NotOverwrite) == EntityOptions.NotOverwrite
				RETURN nFirst
			END IF
			oLine := SELF:GetLine(nLine)
			
			DO WHILE oLine:lOutAmpersand .and. (oLine:LineText:Contains("[") .or. oLine:LineText:Contains("]"))
				nLine ++
				oLine := SELF:GetLine(nLine)
			END DO
			
			IF (eOptions & EntityOptions.ClearOldFields) == EntityOptions.ClearOldFields
				lRemoveFields := FALSE
				nTemp := nLine
				nTemp ++
				DO WHILE nTemp <= SELF:aLines:Count
					oLine := SELF:GetLine(nTemp)
					IF oLine:lEntity
						EXIT
					END IF
					IF oLine:IsDesignerUserStart
						lRemoveFields := TRUE
						EXIT
					ENDIF
					nTemp ++
				END DO
			ELSE
				lRemoveFields := TRUE
			END IF

			IF lRemoveFields
				nTemp := nLine
				nTemp ++
				DO WHILE nTemp <= SELF:aLines:Count
					oLine := SELF:GetLine(nTemp)
					IF oLine:lEntity .or. oLine:IsEndClass
						EXIT
					END IF
					lIsUser := oLine:IsDesignerUserStart
					SELF:RemoveWEDLine(nTemp)
					IF lIsUser
						EXIT
					ENDIF
				END DO
			END IF

			IF SELF:GetLine(nLine):LineText:ToUpper() == aEntity[0]:ToUpper() .or. (eOptions & EntityOptions.KeepFirstLine) == EntityOptions.KeepFirstLine
				nLine ++
			ELSE
				SELF:ReplaceWEDLine(aEntity[0] , nLine)
				nLine ++
			END IF
			n := 1
		END IF

		DO WHILE n < aEntity:Count
			oLine := SELF:AddWEDLine(aEntity[n] , nLine)
			n ++
		END DO
		IF (eOptions & EntityOptions.AddUser) == EntityOptions.AddUser
			IF eType == EntityType._Class
				oLine := SELF:AddWEDLine(ceTab + cUser + cTagU , nLine)
			ELSE
				oLine := SELF:AddWEDLine(cTab + cUser + cTagU , nLine)
			END IF
		END IF
		
		IF eType != EntityType._Class
			IF nLine <= SELF:aLines:Count
				oLine := SELF:GetLine(nLine)
				IF oLine:LineText:Trim() != ""
					oLine := SELF:AddWEDLine("" , nLine)
				END IF
			END IF
		END IF
		
		SELF:CheckCreatedCode()
	RETURN nFirst

	METHOD WriteEndClass(cClass AS STRING) AS VOID
		LOCAL oLine AS LineObject
		LOCAL lFound AS LOGIC
		LOCAL nLine AS INT
		LOCAL n AS INT
		
		cClass := cClass:ToUpper()
		FOR n := 1 UPTO SELF:aLines:Count
			oLine := SELF:GetLine(n)
			IF lFound
				IF oLine:IsEndClass
					RETURN
				END IF
				IF oLine:lEntity .and. oLine:oEntity:cTypedClassName:ToUpper() != cClass
					EXIT
				END IF
				nLine := n + 1
			ELSE
				IF oLine:lEntity .and. oLine:oEntity:cTypedClassName:ToUpper() == cClass
					lFound := TRUE
					nLine := n + 1
				END IF
			END IF
		NEXT
		
		IF nLine != 0
			SELF:AddWEDLine("END CLASS" , nLine)
		END IF
		
	RETURN
	
	METHOD DeleteLine(nLine AS INT) AS VOID
		SELF:RemoveWEDLine(nLine)
		SELF:ModifiedLine(nLine)
	RETURN

	METHOD DeleteDefines(aDefines AS List<STRING>) AS VOID
		LOCAL oLine AS LineObject
		LOCAL nLine AS INT
		nLine := 1
		DO WHILE nLine <= SELF:aLines:Count
			LOCAL lDeleted := FALSE AS LOGIC
			oLine := SELF:aLines[nLine - 1]
			IF oLine:eType == LineType.Define .and. .not. String.IsNullOrEmpty(oLine:cArgument)
				FOREACH cDefine AS STRING IN aDefines
					IF cDefine:ToUpper() == oLine:cArgument:ToUpper()
						SELF:DeleteLine(nLine)
						lDeleted := TRUE
						EXIT
					END IF
				NEXT
			ELSEIF oLine:oEntity != NULL .and. oLine:oEntity:eType == EntityType._Define
				FOREACH cDefine AS STRING IN aDefines
					IF cDefine:ToUpper() == oLine:oEntity:cName:ToUpper()
						SELF:DeleteLine(nLine)
						lDeleted := TRUE
						EXIT
					END IF
				NEXT
			END IF
			IF .not. lDeleted
				nLine ++
			END IF
		END DO

		SELF:CheckCreatedCode()
		
	RETURN


	METHOD AddDefine(cDefine AS STRING , cValue AS STRING) AS VOID
		SELF:AddDefine(cDefine , cValue , TRUE)
	RETURN

	METHOD AddDefine(cDefine AS STRING , cValue AS STRING , lAdd AS LOGIC) AS VOID
		LOCAL cLine AS STRING
		cLine := String.Format("#define {0} {1}" , cDefine , cValue)
//		cLine := String.Format("DEFINE {0} := {1}" , cDefine , cValue)
		IF lAdd
			SELF:AddLine(cLine)
		ELSE
			SELF:InsertLine(1 , cLine)
		END IF
	RETURN
	
	METHOD AddDefines(aDefines AS List<STRING> , aDefineValues AS List<STRING>) AS VOID
		SELF:AddDefines(aDefines , aDefineValues , TRUE)
	RETURN

	METHOD AddDefines(aDefines AS List<STRING> , aDefineValues AS List<STRING> , lAdd AS LOGIC) AS VOID
		FOR LOCAL n := 0 AS INT UPTO aDefines:Count - 1
			SELF:AddDefine(aDefines[n] , aDefineValues[n] , lAdd)
		NEXT
	RETURN

	METHOD RemoveDefines(cWindowName as STRING) AS VOID
		LOCAL oLine AS LineObject
		LOCAL nLine AS INT
		nLine := 1
		cWindowName := cWindowName:ToUpper()+"_"
		DO WHILE nLine <= SELF:aLines:Count
			LOCAL lDeleted := FALSE AS LOGIC
			oLine := SELF:aLines[nLine - 1]
			IF oLine:eType == LineType.Define .and. .not. String.IsNullOrEmpty(oLine:cArgument)
				IF oLine:cArgument:ToUpper():StartsWith(cWindowName)
					SELF:DeleteLine(nLine)
					lDeleted := TRUE
				END IF
			ELSEIF oLine:oEntity != NULL .and. oLine:oEntity:eType == EntityType._Define
				IF oLine:oEntity:cName:ToUpper():StartsWith(cWindowName)
					SELF:DeleteLine(nLine)
					lDeleted := TRUE
				END IF
			END IF
			IF .not. lDeleted
				nLine ++
			END IF
		END DO
		SELF:CheckCreatedCode()
		RETURN

	METHOD AddXSharpDefines(aDefines AS List<STRING> , aDefineValues AS List<STRING>) AS VOID
		SELF:AddXSharpDefines(aDefines, aDefineValues, FALSE)

	METHOD AddXSharpDefines(aDefines AS List<STRING> , aDefineValues AS List<STRING>, lStatic AS LOGIC) AS VOID
		LOCAL cLine AS STRING
		LOCAL nLine AS INT
		FOR LOCAL n := 1 AS INT UPTO SELF:aLines:Count
			LOCAL oLine AS LineObject
			oLine := SELF:aLines[n-1]
			nLine := n
			DO CASE
			CASE oLine:oEntity != NULL
				EXIT
//			CASE oLine:IsRegionIn .and. oLine:cArgument:ToUpper() == "DEFINES"
			// grr, I am not setting cArgument for regions
			CASE oLine:IsRegionIn .and. oLine:LineText:ToUpper():Contains("DEFINES")
				nLine ++
				EXIT
			END CASE
		NEXT
		LOCAL cMask AS STRING
		cMask := "DEFINE {0} := {1}"
		IF lStatic
			cMask := "STATIC "+cMask			
		ENDIF
		FOR LOCAL n := 0 AS INT UPTO aDefines:Count - 1
			cLine := String.Format(cMask , aDefines[n] , aDefineValues[n])
			SELF:InsertLine(nLine , cLine)
			nLine ++
		NEXT
	RETURN

END CLASS

[Flags];
ENUM EntityOptions
	MEMBER None := 0
	MEMBER AddUser := 1
	MEMBER NotOverwrite := 2
	MEMBER ClearOldFields := 4
	MEMBER KeepFirstLine := 8
END ENUM

END NAMESPACE

