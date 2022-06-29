// 497. error XS9010: The function 'pcount' is only supported in functions or methods with CLIPPER calling convention
// also vulcan has a problem with that
#pragma warnings(9032, off) // return value ignored
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	xAssert( o:TestMethod(1,2) == 2)
	xAssert( o:TestMethod() == 0)

	xAssert( o:TestAccess1[1] == 1)
	xAssert( o:TestAccess2[1] == 1)
    o:TestAssign  := 10
	xAssert( o:TestAccess1["A"] > 0)
RETURN

CLASS TestClass
	ACCESS TestAccess1(a)
		? pcount(),a
	RETURN pcount()
	ACCESS TestAccess2(a) CLIPPER
		? pcount(),a
	RETURN pcount()
	Assign TestAccess2(b,a) CLIPPER
		? pcount(),a,b
	RETURN pcount()
	ASSIGN TestAssign(n) CLIPPER
		? pcount(),n
	RETURN

	METHOD TestMethod(a,b,c) CLIPPER
		? pcount()
	RETURN pcount()
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

