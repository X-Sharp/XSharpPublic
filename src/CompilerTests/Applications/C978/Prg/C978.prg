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
	
	LOCAL nCount AS INT
	
	LOCAL cb AS Codeblock
	cb := { || this.expfld == 0 }
	nCount := __VfpCount( cb )
	? nCount
	xAssert(nCount == 3) // OK

	cb := { || this.expfld == FLD }
	nCount := __VfpCount( cb )
	? nCount
	xAssert(nCount == 2) // OK
	
	COUNT TO nCount FOR this:expfld == 0
	? nCount
	xAssert(nCount == 3) // OK

	COUNT TO nCount FOR this:expfld == FLD
	? nCount
	xAssert(nCount == 2) // OK

	COUNT TO nCount FOR TRUE
	? nCount
	xAssert(nCount == 3) // OK

	// these result to zero:
	COUNT TO nCount FOR this.expfld == 0
	? nCount // 0
	xAssert(nCount == 3)
	COUNT TO nCount FOR this.expfld == FLD
	? nCount // 0
	xAssert(nCount == 2)
	
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
