#INCLUDE foxpro.h

DEFINE CLASS XsFoxArea AS Custom OLEPUBLIC
	PROTECTED nArea AS Long
	PROTECTED nRecord AS Long
	PROTECTED aRecordBuffer[1]
	PROTECTED HotBuffer AS Boolean
	PROTECTED LastError AS Long


FUNCTION Init() AS Boolean
	this.nArea = 0
    this.nRecord = 0
    this.HotBuffer = .f.
	ENDFUNC


PROTECTED FUNCTION GoCold() AS Boolean
IF this.HotBuffer
	SELECT (this.nArea)
	GOTO this.nRecord
	GATHER FROM this.aRecordBuffer
	this.HotBuffer = .F.
ENDIF
RETURN .T.
*
* Properties
*

FUNCTION Alias_Access  as String
LOCAL result as String
TRY
	result = ALIAS(this.nArea)
CATCH
	this.LastError = ERROR()
	this.ShowError()
	result = ""
ENDTRY
RETURN result

FUNCTION Recno as Long
LOCAL result as long
TRY
	result = RECNO(this.nArea)
CATCH
	this.LastError = ERROR()
	this.ShowError()
	result = -1
ENDTRY
RETURN result

FUNCTION Eof as Boolean
LOCAL result as boolean
TRY
	result = Eof(this.nArea)
CATCH
	this.LastError = ERROR()
	this.ShowError()
	result = .t.
ENDTRY
RETURN result

FUNCTION Bof as Boolean
LOCAL result as boolean
TRY
	result = Bof(this.nArea)
CATCH
	this.LastError = ERROR()
	this.ShowError()
	result = .T.
ENDTRY
RETURN result

FUNCTION RecCount as Long
LOCAL result as boolean
TRY
	result = RecCount(this.nArea)
CATCH
	this.LastError = ERROR()
	this.ShowError()
	result = -1
ENDTRY
RETURN result

FUNCTION Used AS Boolean
RETURN USED(this.nArea)

FUNCTION DbStruct  AS STRING
TRY

	LOCAL ARRAY aStruct(1,1)
	LOCAL nFld as Long
	LOCAL result as String
	SELECT (this.nArea)
	AFIELDS(aStruct)
	result = ""
	FOR nFld = 1 TO ALEN(aStruct,1)
		result = result + aStruct(nFld,1)+";" && Name
		result = result + aStruct(nFld,2)+";" && Type
		result = result + STR(aStruct(nFld,3))+";" && Len
		result = result + STR(aStruct(nFld,4))+";" && Width
		result = result + IIF(aStruct(nFld,5), "T", "F")+";" && Null
		result = result + STR(aStruct(nFld,17))+";" && Next AutoInc
		result = result + STR(aStruct(nFld,18))+";" && Step AutoInc
		result = result + CHR(13)
	NEXT
CATCH TO Ex
	this.LastError = ERROR()
	this.ShowError()
	result = ""
ENDTRY
RETURN result

FUNCTION FCount AS Long
LOCAL result as Long
TRY
	result = FCOUNT(this.nArea)
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = -1
ENDTRY	
RETURN result

FUNCTION FieldName(nFld as Long) AS STRING
LOCAL result as string
TRY
	result = FIELD(nFld, this.nArea,0)
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = ""
ENDTRY	
RETURN result
	
FUNCTION FieldCaption(nFld as Long) AS STRING
LOCAL result as string
TRY
	result = FIELD(nFld, this.nArea,1)
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = ""
ENDTRY	
RETURN result


*
* Open and Close 
*

FUNCTION LinkToCursor(cAlias as String) AS Boolean
LOCAL result AS Boolean
LOCAL nCurrent as Long
TRY
	this.GoCold()
	this.LastError = .null.
	this.nArea = SELECT(cAlias)
	result = .t.
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = .f.
ENDTRY
RETURN result


FUNCTION Open(cFile as String, cAlias as String) AS Boolean
LOCAL result AS Boolean
LOCAL nCurrent as Long
TRY
	this.GoCold()
	nCurrent = SELECT(cAlias)
	this.LastError = .null.
	USE &cFile IN (nCurrent) ALIAS &cAlias 
	this.nArea = SELECT()
	result = .t.
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = .f.
ENDTRY
RETURN result


FUNCTION Close() AS Boolean
LOCAL result AS boolean
TRY
	this.GoCold()
	this.LastError = .null.
	USE IN (this.nArea)
	result = .T.
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = .F.
ENDTRY
RETURN result

*
* Navigation
*

FUNCTION GoTop() as Boolean
TRY
	this.LastError = .null.
	= this.GoCold()
	GO TOP IN (this.nArea)
	result = .T.
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = .F.
ENDTRY
RETURN result

FUNCTION GoBottom() as Boolean
LOCAL result AS boolean
TRY
	this.LastError = .null.
	= this.GoCold()
	GO BOTTOM IN (this.nArea)
	result = .T.
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = .F.
ENDTRY
RETURN result

FUNCTION Skip(nRecords as Long) as Boolean
LOCAL result AS boolean
TRY
	this.LastError = .null.
	= this.GoCold()
	SKIP nRecords IN (this.nArea)
	result = .T.
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = .F.
ENDTRY
RETURN result

FUNCTION Goto(nRecord as Long) as Boolean
LOCAL result AS boolean
TRY
	this.LastError = .null.
	= this.GoCold()
	GOTO nRecord IN (this.nArea)
	result = .T.
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = .F.
ENDTRY

RETURN result


*
* Read /Write
*

FUNCTION Scatter() AS Boolean
IF this.nRecord != RECNO(this.nArea)
	this.nRecord = RECNO(this.nArea)
	SCATTER TO this.aRecordBuffer MEMO
ENDIF
RETURN .T.


FUNCTION FieldGet(nFld as Long) AS Variant
LOCAL result as Variant
TRY
	this.Scatter()
	result = this.aRecordBuffer[nFld]
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = .null.
ENDTRY
RETURN result

FUNCTION FieldPut(nFld as Long, uNewValue as Variant) AS Boolean
LOCAL result as Boolean
TRY
	this.Scatter()
	this.aRecordBuffer[nFld] = uNewValue
	this.HotBuffer = .T.
	result = .T.
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = .f.
ENDTRY

RETURN result

FUNCTION Append()  AS Boolean
LOCAL result as Boolean
TRY
	this.LastError = .null.
	= this.GoCold()
	APPEND BLANK IN (this.nArea)
	this.Scatter()
	result = .T.
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = .F.
ENDTRY
RETURN result

FUNCTION Delete()  AS Boolean
LOCAL result as Boolean
TRY
	this.LastError = .null.
	= this.GoCold()
	DELETE IN (this.nArea)
	result = .T.
CATCH 
	this.LastError = ERROR()
	this.ShowError()
	result = .F.
ENDTRY

RETURN result

*
* Index Operations
*
FUNCTION SetOrder(cTag as String, cIndex as STRING) AS Boolean
LOCAL result as Boolean
TRY
	this.LastError = .null.
	SET ORDER TO (cTag) OF (cIndex) IN (this.nArea)
	result = .t.
CATCH TO Ex
	this.LastError = ERROR()
	this.ShowError()
	result = .f.
ENDTRY
RETURN result

*
* Error Handling
*
PROTECTED FUNCTION ShowError

   ? "********************************"
   ? 'Error number: ' + PADR(this.LastError ,20)
   ? 'Error message: ' + MESSAGE()
   ? 'Line of code with error: ' + MESSAGE(1)
   ? 'Program with error: ' + PROGRAM(0)
   ? 'Line number of error: ' + PADR(LINENO(0),20)
   ? "********************************"
   
RETURN .F.


ENDDEFINE