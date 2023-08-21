FUNCTION Start() AS VOID

LOCAL cFileName AS STRING

LOCAL aStruct AS ARRAY

cFileName := "memotest"

RddSetDefault("DBFCDX")

aStruct := { ;
{ "MEMO1","M",0,0 } ,;
{ "MEMO2","M",1,0 } ,;
{ "MEMO3","M",10,0 } ,;
{ "MEMO4","M",111,0 } ;
}

DbCreate(cFileName,aStruct)

DbUseArea(,,cFileName)

DbAppend()

FieldPut(1,"memo1")

FieldPut(2,"memo2")

FieldPut(3,"memo3")

FieldPut(4,"memo4")

XAssert(FieldGet(1) == "memo1")
XAssert(FieldGet(2) == "memo2")
XAssert(FieldGet(3) == "memo3")
XAssert(FieldGet(4) == "memo4")

DbCloseArea()
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

