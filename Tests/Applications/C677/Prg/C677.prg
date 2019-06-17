// 677. Problem calling a constructor with PARAMS parameter with a USUAL
/*
calling a method with a [ParamArray] parameter by
passing it a (string inside a) USUAL works fine at runtime. But when
using the same exact code for calling a constructor with the exact same
parameters causes a runtime exception.

Also when using the [ParamArray] attribute, a warning is reported:

warning XS0674: Do not use 'System.ParamArrayAttribute'. Use the 'params' keyword instead.

but the PARAMS sytnax does not seem to be supported
*/
FUNCTION Start() AS VOID
	LOCAL u AS USUAL
	u := "asd"
	DoTest(u)
RETURN

PROCEDURE DoTest(u AS USUAL)
	TestClass.Test(u) // ok
	
	LOCAL o AS TestClass
	o := TestClass{u} // runtime exception
	
CLASS TestClass
	CONSTRUCTOR([ParamArray] s AS STRING[])
		? s:Length
		? s[1]
	RETURN
	STATIC METHOD Test([ParamArray] s AS STRING[]) AS VOID
		? s:Length
		? s[1]
	RETURN

	METHOD TestParams(PARAMS ooo AS INT[]) AS VOID

END CLASS
