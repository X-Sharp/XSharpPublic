// 810. _PrivateCount() throws an InvalidateOperationException
// https://github.com/X-Sharp/XSharpPublic/issues/801
FUNCTION Start() AS VOID
_MClear()   // clear all variables. This is needed when running in the test suite
? _PublicCount()  // ok , 0
? _PrivateCount() 
xAssert(_PublicCount() == 0)
xAssert(_PrivateCount() == 0)

PRIVATE x	

xAssert(_PublicCount() == 0)
xAssert(_PrivateCount() == 1)

PUBLIC pub

xAssert(_PublicCount() == 1)
xAssert(_PrivateCount() == 1)

AnotherProc()

xAssert(_PublicCount() == 1)
xAssert(_PrivateCount() == 1)

RETURN

PROCEDURE AnotherProc()
xAssert(_PublicCount() == 1)
xAssert(_PrivateCount() == 1)

PRIVATE p2
xAssert(_PrivateCount() == 2)
xAssert(_PrivateCount(FALSE) == 2)
xAssert(_PrivateCount(TRUE) == 1)

PRIVATE p3
PRIVATE p4
xAssert(_PrivateCount(FALSE) == 4)
xAssert(_PrivateCount(TRUE) == 3)


PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN 		

