// 972. Compiler problem with incorrect default parameters in the VO dialect
// https://github.com/X-Sharp/XSharpPublic/issues/2014

// VO dialect
CLASS TestClass
	CONSTRUCTOR( n AS INT, l := FALSE AS STRING) AS VOID // no compiler error
	METHOD Test( n := "asd" AS LOGIC) AS VOID // no compiler error
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{1} // ICE
	o:Test() // ICE
