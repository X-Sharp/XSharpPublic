// 971. Macro compiler does not support the dbfname.fieldname expression #2018
// https://github.com/X-Sharp/XSharpPublic/issues/2018

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
FUNCTION Start( ) AS VOID
	LOCAL cDbf AS STRING
	cDbf := "macrotest"
	
	DbCreate( cDbf, {{"FLD1","C",4,0}},"DBFVFP")
	
	DbUseArea(TRUE,"DBFVFP",cDbf, "macrotest")
	DbAppend()
	FieldPut(1, "test")

	? macrotest.FLD1 // test, OK
	
	xAssert( macrotest.FLD1 == "test" )
	
	? &("macrotest->FLD1") // test, OK

	xAssert( &("macrotest->FLD1") == "test" )

	? &("macrotest.FLD1") // XSharp.Error: Variable does not exist: macrotest
	
	xAssert( &("macrotest.FLD1") == "test" )
	

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

