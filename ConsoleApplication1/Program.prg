//
// Start.prg
//
#include "dbcmds.vh"
FUNCTION Start() AS VOID
    //Start1()
    //Start2()
    Start3()
    wait
RETURN


FUNCTION Start1() AS VOID
LOCAL aStruct AS ARRAY
LOCAL i AS DWORD
aStruct := {{"CHARFIELD","C",10,0},{"NUMFIELD","N",3,0},{"DATEFIELD","D", 8,0}}
SetAnsi(TRUE)
SetCollation(#Windows)
SetNatDLL("german2.dll")
DBCREATE("Test1Ansi",aStruct, "DBFNTX")
DBCLOSEAREA()
USE Test1Ansi       
FOR i := 1 TO 255
	DBAPPEND()
	_FIELD->CHARFIELD := Replicate(CHR(i),10)
	_FIELD->NUMFIELD  := i
	_FIELD->DATEFIELD := ConDate(1800 + i, 1 + i % 12, 1 + i % 28)
NEXT
DBCREATEINDEX("test1Ansi1","CHARFIELD")
DBCREATEINDEX("test1Ansi2","NUMFIELD")
DBCREATEINDEX("test1Ansi3","DATEFIELD")

DBCLOSEAREA()
SetAnsi(FALSE)
DBCREATE("Test1OEM",aStruct, "DBFNTX")
DBCLOSEAREA()
USE Test1OEM       
FOR i := 1 TO 255
	DBAPPEND()
	_FIELD->CHARFIELD := Replicate(CHR(i),10)
	_FIELD->NUMFIELD  := i
	_FIELD->DATEFIELD := ConDate(1800 + i, 1 + i % 12, 1 + i % 28)
NEXT                  
SetCollation(#Clipper)
DBCREATEINDEX("test1Oem1","CHARFIELD")
DBCREATEINDEX("test1Oem2","NUMFIELD")
DBCREATEINDEX("test1Oem3","DATEFIELD")
DBCLOSEAREA()
RETURN

FUNCTION Start2() AS VOID
LOCAL f AS FLOAT
f := seconds()
USE "c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\TEST10K.DBF"
DbCreateIndex("10kName.xxx", "upper(Last+First)")
DbCreateIndex("10kState", "State")
DbCreateIndex("10kSalary", "-Salary")
DbCloseArea()
? Seconds() - f
WAIT
RETURN


FUNCTION Start3() AS VOID
LOCAL cFileName AS STRING
cFileName := "C:\Test\teest.dbf"
? DBCreate(cFileName, {{"FLD1","C",10,0}})

? DBUseArea ( TRUE , , cFileName , "a1")
? DBGetSelect() // 1
? DBCloseArea()

? DBUseArea ( TRUE , , cFileName , "a2")
? DBGetSelect() // 2
? DBCloseArea()

? DBUseArea ( TRUE , , cFileName , "a3")
? DBGetSelect() // 3
? DBCloseArea()
RETURN


