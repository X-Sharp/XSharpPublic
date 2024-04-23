// 904. Problems with memvar pragma option
// https://github.com/X-Sharp/XSharpPublic/issues/1454

// xBase dialect
FUNCTION Start() AS VOID
TestClass{}:TestPublics()

// both are accepted:
#pragma options("memvars",on) // no warning
#pragma options("memvar",on)  // no warning

#pragma options("memvarsnot",on) // warning XS9096: Unrecognized #pragma OK

CLASS TestClass
	METHOD TestPublics() AS VOID
		PUBLIC MyPublic
		MyPublic := 123 // error XS9002: Parser: unexpected input 'MyPublic'
		? MyPublic
	RETURN
END CLASS

