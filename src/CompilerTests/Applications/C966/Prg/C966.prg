// 966. Bogus Type Expected compiler error with CLIPPER method with no args/return type #1907
// https://github.com/X-Sharp/XSharpPublic/issues/1907

// VO dialect
#pragma options (vo15, off)
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o:TestClipper()

CLASS TestClass
	// error XS1031: Type expected
	METHOD TestClipper() CLIPPER
	RETURN 123
END CLASS
