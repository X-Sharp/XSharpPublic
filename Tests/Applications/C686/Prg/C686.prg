FUNCTION Start() AS VOID
	xAssert( DbCreate("Test686",{{"LASTNAME","C",10,0}}) )
	xAssert( DbUseArea(TRUE, ,"Test686") )
	xAssert( DbAppend() )
	FieldPut(1, "ASD")
	XAssert(AllTrim(FieldGet(1)) == "ASD")
	xAssert( DbCloseArea() )
	
	xAssert( DbUseArea(TRUE,NIL ,"Test686",NIL) )
	xAssert( DbCloseArea() )

	xAssert( DbUseArea(TRUE, ,"Test686","FoxAlias") )
	FoxAlias  -> FieldPut(1, "ZXC")
	XAssert(FoxAlias  -> AllTrim(FieldGet(1)) == "ZXC")
	xAssert( DbCloseArea() )
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

