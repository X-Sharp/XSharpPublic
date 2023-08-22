// 880. Incorrect ambiguous call error with OBJECT/IntPtr parameters
// https://github.com/X-Sharp/XSharpPublic/issues/1197
// error XS0121: The call is ambiguous between the following methods or properties: 'TestClass.Test(object, object)' and 'TestClass.Test(System.IntPtr, System.IntPtr)'
// VO dialect, /vo7+ enabled

#pragma options ("vo7" , on)

CLASS TestClass
	METHOD InstanceTest(o1 AS OBJECT, o2 AS OBJECT) AS VOID
		? "object overload"
	METHOD InstanceTest(o1 AS IntPtr, o2 AS IntPtr) AS VOID
		? "IntPtr overload"

	STATIC METHOD StaticTest(o1 AS OBJECT, o2 AS OBJECT) AS VOID
		? "object overload"
	STATIC METHOD StaticTest(o1 AS IntPtr, o2 AS IntPtr) AS VOID
		? "IntPtr overload"
END CLASS

FUNCTION FuncTest(o1 AS OBJECT, o2 AS OBJECT) AS VOID
	? "object overload"
FUNCTION FuncTest(o1 AS IntPtr, o2 AS IntPtr) AS VOID
	? "IntPtr overload"

FUNCTION Start() AS VOID
	LOCAL oObject := NULL AS OBJECT

	FuncTest(NULL, oObject) // object overload, ok
	TestClass{}:InstanceTest(NULL, oObject) // error XS0121
	TestClass.StaticTest(NULL, oObject) // error XS0121
