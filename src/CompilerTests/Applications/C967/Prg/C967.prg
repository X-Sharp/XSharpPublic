// 967. No compiler error when using NIL and CLIPPER calling convention in the Core dialect #1907
// https://github.com/X-Sharp/XSharpPublic/issues/1907

// Core dialect
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o:TestClipper()

CLASS TestClass
	// no errors!
	METHOD TestClipper() AS OBJECT CLIPPER
	RETURN NIL
END CLASS
