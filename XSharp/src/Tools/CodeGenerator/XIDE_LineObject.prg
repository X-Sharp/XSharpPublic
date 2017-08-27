#using System.Collections.Generic
#using System.Collections

BEGIN NAMESPACE Xide

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

PARTIAL CLASS LineObject
	EXPORT lInBlockComment , lOutBlockComment AS LOGIC
	EXPORT lInAmpersand , lOutAmpersand AS LOGIC

	EXPORT eType AS LineType
//	PROTECT _dummy AS BYTE

	PROTECT cLineText AS STRING
//	PROTECT oDescriptor AS LineDescriptor

	PROTECT nStartCol AS INT

	EXPORT cArgument AS STRING

	EXPORT oSubLine AS LineObject
	EXPORT oEntity AS EntityObject
	EXPORT oInline AS EntityObject
	EXPORT aFields AS List<EntityObject>
	
	CONST PROTECT _internlimit := 0 AS INT
		
	ACCESS LineText AS STRING
	RETURN SELF:cLineText
	ASSIGN LineText(cLine AS STRING)
		IF cLine != NULL .and. cLine:Length < _internlimit
			cLine := String.Intern(cLine)
		END IF
		SELF:cLineText := cLine
	RETURN

	CONSTRUCTOR(cLine AS STRING)
		SELF:LineText := cLine
		SELF:cArgument := NULL
	RETURN
	CONSTRUCTOR()
		SELF:LineText := NULL
		SELF:cArgument := NULL
	RETURN
	
	METHOD Dispose() AS VOID
		SELF:LineText := NULL
		SELF:oEntity := NULL
		SELF:cArgument := NULL
		IF SELF:oSubLine != NULL
			SELF:oSubLine:Dispose()
			SELF:oSubLine := NULL
		END IF
		IF SELF:aFields != NULL
			SELF:aFields:Clear()
			SELF:aFields := NULL
		END IF
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
	
	ACCESS IsInclude AS LOGIC
	RETURN SELF:eType == LineType.Include
	
/*	ACCESS IsEntity AS LOGIC
	RETURN SELF:oEntity != NULL*/

	ACCESS IsDesignerUserStart AS LOGIC
		LOCAL cLine AS STRING
		cLine := SELF:LineText:ToUpper()
	RETURN cLine:Contains("##USER##") .or. cLine:Contains("{{%UC%}}") .or. cLine:Contains("USER CODE STARTS")


	VIRTUAL METHOD ToString() AS STRING
	RETURN "Line: " + SELF:LineText

END CLASS

END NAMESPACE
