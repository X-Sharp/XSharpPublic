// 973. Problems with the /fox3 option - instance vars
// https://github.com/X-Sharp/XSharpPublic/issues/2025#issuecomment-5150615555

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
CLASS TestClass
	EXPORT TestField := "abc" AS USUAL
	EXPORT TestFieldTyped := "abc" AS STRING

	PROPERTY TestProp AS USUAL GET "abc"
	PROPERTY TestPropTyped AS strin GET "abc"

	CONSTRUCTOR()
		TestFieldTyped := "test"
		? TestFieldTyped.Length // XSharp.Error: Variable does not exist: TESTFIELDTYPED
		? TestPropTyped.Length  // XSharp.Error: Variable does not exist: TESTPROPTYPED

		TestField := "test"
		? TestField.Length // XSharp.Error: Variable does not exist: TESTFIELD
		? TestProp.Length  // XSharp.Error: Variable does not exist: TESTPROP
END CLASS

FUNCTION Start() AS VOID
	LOCAL t AS TestClass
	t := TestClass{}
	? t.TestField // XSharp.Error: Variable does not exist: T
	? t.TestProp // XSharp.Error: Variable does not exist: T

	LOCAL u AS USUAL
	u := TestClass{}
	? u.TestField // XSharp.Error: Variable does not exist: U
	? u.TestProp // XSharp.Error: Variable does not exist: U
RETURN

