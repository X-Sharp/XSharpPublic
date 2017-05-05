// 497. error XS9010: The function 'pcount' is only supported in functions or methods with CLIPPER calling convention
// also vulcan has a problem with that
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	xAssert( o:TestMethod(1,2) == 2)
	xAssert( o:TestMethod() == 0)

	xAssert( o:TestAccess1 == 0)
	xAssert( o:TestAccess2 == 0)

	xAssert( o:TestAccess1["A"] > 0)
RETURN

CLASS TestClass
	ACCESS TestAccess1(a)
		? pcount()
	RETURN pcount()
	ACCESS TestAccess2(a) CLIPPER
		? pcount()
	RETURN pcount()
	ASSIGN TestAssign(n) CLIPPER
		? pcount()
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

