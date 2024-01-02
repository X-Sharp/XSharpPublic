// 898. The global MEMVARs hide local variables
// https://github.com/X-Sharp/XSharpPublic/issues/1294

MEMVAR myArg

FUNCTION Start() AS VOID STRICT
	myArg := 10
	TestFunc(1, 2, 3)
	RETURN

FUNCTION TestFunc(a, myArg, b)
	? a
	? myArg
	? b
	? m->myArg
	
	xAssert(myArg == 2)
	xAssert(m->myArg == 10)
	
	RETURN NIL
	
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN	
