// 880. Incorrect ambiguous call error with OBJECT/IntPtr parameters
// https://github.com/X-Sharp/XSharpPublic/issues/1197
// error XS0121: The call is ambiguous between the following methods or properties: 'TestClass.Test(object, object)' and 'TestClass.Test(System.IntPtr, System.IntPtr)'
// VO dialect, /vo7+ enabled

#pragma options ("vo7" , on)

CLASS TestClass
	METHOD InstanceTest(o1 AS OBJECT, o2 AS OBJECT) AS VOID
		? "object overload"
		xAssert(TRUE)
	METHOD InstanceTest(o1 AS IntPtr, o2 AS IntPtr) AS VOID
		? "IntPtr overload"
		xAssert(FALSE)

	STATIC METHOD StaticTest(o1 AS OBJECT, o2 AS OBJECT) AS VOID
		? "object overload"
		xAssert(TRUE)
	STATIC METHOD StaticTest(o1 AS IntPtr, o2 AS IntPtr) AS VOID
		? "IntPtr overload"
		xAssert(FALSE)
END CLASS

FUNCTION FuncTest(o1 AS OBJECT, o2 AS OBJECT) AS VOID
	? "object overload"
		xAssert(TRUE)
FUNCTION FuncTest(o1 AS IntPtr, o2 AS IntPtr) AS VOID
	? "IntPtr overload"
		xAssert(FALSE)

FUNCTION Start() AS VOID
	LOCAL oObject := NULL AS OBJECT

	FuncTest(NULL, oObject) // object overload, ok
	TestClass{}:InstanceTest(NULL, oObject) // error XS0121
	TestClass.StaticTest(NULL, oObject) // error XS0121

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
