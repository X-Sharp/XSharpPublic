// 812. Problems with ARRAY OF <type> and assigning/comparing to NULL_ARRAY
// https://github.com/X-Sharp/XSharpPublic/issues/833
#pragma warnings(165, off) // unasigned local
FUNCTION Start() AS VOID STRICT
	LOCAL oIntArray AS ARRAY OF INT
	xAssert(oIntArray == NULL_ARRAY)
	xAssert(NULL_ARRAY == oIntArray)
	TestFunc( OUT oIntArray )
	xAssert(oIntArray == NULL_ARRAY)
	xAssert(NULL_ARRAY == oIntArray)

	oIntArray := {1,2,3}
	xAssertFalse(oIntArray == NULL_ARRAY)
	xAssertFalse(NULL_ARRAY == oIntArray)
	oIntArray := NULL_ARRAY
	xAssert(oIntArray == NULL_ARRAY)
	xAssert(NULL_ARRAY == oIntArray)
	RETURN

FUNCTION TestFunc(oIntArray OUT ARRAY OF INT) AS VOID STRICT
	oIntArray := NULL_ARRAY
	RETURN

PROC xAssert(l AS LOGIC)
IF .NOT. l
//	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
	? "FAILED"
ELSE
	? "Assertion passed"
END IF
RETURN
PROC xAssertFalse(l AS LOGIC)
l := .not. l
IF .NOT. l
//	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
	? "FAILED"
ELSE
	? "Assertion passed"
END IF
RETURN

