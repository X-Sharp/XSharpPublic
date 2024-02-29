// 901.  Harbour dialect FIELD statement not supported #1436 
// https://github.com/X-Sharp/XSharpPublic/issues/1436

FIELD FLDCHAR2
FIELD FLDNUM2

FUNCTION Start() AS VOID
	LOCAL cDbf AS STRING
	cDbf := "C901.dbf"
	DbCreate(cDbf, {{"FLDCHAR1" , "C" , 10 , 0} , {"FLDCHAR2" , "C" , 10 , 0} , {"FLDNUM1" , "N" , 8 , 0}, {"FLDNUM2" , "N" , 8 , 0} })
	DbUseArea(,,cDbf,"alias")

	DbAppend()
	FieldPut(1, "TEST1")
	FieldPut(2, "TEST2")
	FieldPut(3, 123)
	FieldPut(4, 456)
	
	FIELD FLDCHAR1
	FIELD FLDNUM1
	? FLDCHAR1
	? FLDNUM1
	xAssert(AllTrim(FLDCHAR1) == "TEST1")
	xAssert(AllTrim(FLDCHAR2) == "TEST2")

	xAssert(FLDNUM1 == 123)
	xAssert(FLDNUM2 == 456)
	
	DbCloseArea()

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
