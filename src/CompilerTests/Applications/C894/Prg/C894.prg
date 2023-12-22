// 894. Compiler crash with missing reference to XSharp.VDP
// https://github.com/X-Sharp/XSharpPublic/issues/1405
#command DELETE FILE <(filename)> => System.IO.File.Delete(<(filename)>)
#command RENAME <(filename1)> TO <(filename2)> => FRENAME(<(filename1)>, <(filename2)>) 
FUNCTION Start( ) AS VOID
xAssert( OtherFunction("asd") == "C")
xAssert( OtherFunction(1)     == "N")
xAssert( OtherFunction(FALSE) == "L")
RETURN
	            
FUNCTION OtherFunction
PARAMETERS test
RETURN VARTYPE(test)

FUNCTION ADD_TABLE_COLUMN
PARAMETERS TableAlias, fldName, fldType, fldLen, fldDec
IF VARTYPE(fldName) <> "C"
	messagebox("Error on call to ADD_TABLE_COLUMN. Field Name must be a Character String")
	RETURN 1
ENDIF
IF Len(fldName) < 1 .or. Len(fldName) > 10
	messagebox("Error on call to ADD_TABLE_COLUMN. Field Name length must be bewteen 1 and 10")
	RETURN 1
ENDIF
CurrWorkArea = Alias()
select (TableAlias)
cDBF = DBF()
cDBFPath = JUSTPATH(cDBF) 
TempFldsFile = cDBFPath+"\temp_flds"
copy STRUCTURE extended TO (TempFldsFile)
Select 0
use (TempFldsFile) alias tempflds
Append Blank
Replace Field_name WITH fldName
Replace Field_type WITH fldType
Replace Field_len WITH fldLen
Replace Field_dec WITH fldDec 
tempdbf = cDBFPath+"\tempdbf"     
Use
Select (TableAlias)
Create (tempdbf) FROM TempFldsFile
Use (tempdbf) Alias (TableAlias)
Append FROM (cDBF)   
*Erase (cDBF) 
DELETE File (cDBF)
Use      
Rename (tempdbf) TO (cDBF)
Select 0
Use (cDBF) Alias (TableAlias)
select (CurrWorkArea)
RETURN 0

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN


