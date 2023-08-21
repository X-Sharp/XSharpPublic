#define FALSE .F.
#define TRUE .T.
SET PROCEDURE TO XsFoxArea
DEFINE CLASS XsFoxManager AS Custom OLEPUBLIC
	
	FUNCTION Init() AS Boolean
	ENDFUNC


FUNCTION CloseAll() AS Boolean
	CLOSE TABLES

RETURN .T.


FUNCTION OpenTable(cFile as String, cAlias as String) AS XsFoxArea
LOCAL oArea as XsFoxArea
oArea = CREATEOBJECT("XsFoxArea")
oArea.Open(cFile, cAlias)
RETURN oArea


FUNCTION OpenSql(cStmt as String, cAlias as String) AS XsFoxArea
Application.DoCmd(cStmt)
RETURN this.GetArea(cAlias)

FUNCTION GetArea(cAlias as String) AS XsFoxArea
LOCAL oArea as XsFoxArea
oArea = CREATEOBJECT("XsFoxArea")
oArea.LinkToCursor(cAlias)
RETURN oArea

FUNCTION ExecSql(cStmt as String) AS Boolean
LOCAL result as Boolean
TRY
	Application.DoCmd(cStmt)
	result = .T.
CATCH 
	result = .F.
ENDTRY	
RETURN result

FUNCTION GET(cSetting as String) AS Variant
	RETURN SET(cSetting)

FUNCTION SET(cSetting as String, newValue as Variant) AS Variant
	LOCAL oldSetting 
	cSetting = UPPER(cSetting)
	oldSetting = SET(cSetting)
	IF cSetting == "SOFTSEEK"
		cSetting = "NEAR"
	ENDIF
	IF PCOUNT() > 1
		DO CASE
		CASE cSetting == "COLLATE"
			SET COLLATE TO &newValue
		CASE cSetting == "DATABASE"
			SET DATABASE TO &newValue
		CASE cSetting == "DATE"
			SET DATE &newValue
		CASE cSetting == "DELETED"
			SET DELETED &newValue
		CASE cSetting == "EXACT"
			SET EXACT &newValue
		CASE cSetting == "EXCLUSIVE"
			SET EXCLUSIVE &newValue
		CASE cSetting == "LOCK"
			SET LOCK &newValue
		CASE cSetting == "LOGERRORS"
			SET LOGERRORS &newValue
		CASE cSetting == "NEAR"
			SET NEAR &newValue
		CASE cSetting == "UNIQUE"
			SET UNIQUE &newValue
		ENDCASE
	ENDIF
	RETURN oldSetting

*
* Error Handling
*
FUNCTION ShowError
CLEAR


   ? "********************************"
   ? 'Error number: ' + PADR(this.LastError ,20)
   ? 'Error message: ' + MESSAGE()
   ? 'Line of code with error: ' + MESSAGE(1)
   ? 'Program with error: ' + PROGRAM(0)
   ? 'Line number of error: ' + PADR(LINENO(0),20)
   ? "********************************"
   wait
RETURN .F.

ENDDEFINE


