// 978. Problems with the /fox3 option - codeblocks #2054
// https://github.com/X-Sharp/XSharpPublic/issues/2054

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("lb", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)

CLASS TestClass
	EXPORT expfld AS INT
	METHOD Test() AS VOID
	cDbf := "c:\test\testcb"
	DbCreate( cDbf, {{"FLD","N",10,0}} )
	DbUseArea( , , cdbf )
	DbAppend( ); FieldPut(1, 0)
	DbAppend( ); FieldPut(1, 1)
	DbAppend( ); FieldPut(1, 0)
	DbGoTop()

	LOCAL n,n2 AS INT
	COUNT TO n FOR this.expfld == FLD
	? n
	xAssert(n==2)

	COUNT TO n2 FOR this:expfld == 0
	? n2
    
	xAssert(n2==3)
	
END CLASS

FUNCTION Start() AS VOID
	TestClass{}:Test()



PROC xAssert(l AS LOGIC) AS VOID
IF .not. l
//	? "FAILED!"
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
	? "Assertion passed"
RETURN
