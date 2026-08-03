// 973. Problems with the /fox3 option - instance vars
// https://github.com/X-Sharp/XSharpPublic/issues/2025#issuecomment-5150615555

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)
CLASS TestClass
	EXPORT TestField := "abc" AS USUAL
	EXPORT TestFieldTyped := "abc" AS STRING
	
	METHOD TestUntypedMethod( n ) CLIPPER
		IF IsNil( n )
			RETURN 1000
		END IF
	RETURN n + 1
	METHOD TestMethod( n AS INT ) AS INT
	RETURN n + 1

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
	? t.TestMethod( 1 ) + 10  // XSharp.Error: Variable does not exist: T
	? t.TestUntypedMethod( 1,2,3 ) + 10
	? t.TestUntypedMethod( 1 ) + 10
	? t.TestUntypedMethod(  ) + 10

	LOCAL u AS USUAL
	u := TestClass{}
	? u.TestField // XSharp.Error: Variable does not exist: U
	? u.TestProp // XSharp.Error: Variable does not exist: U
	? u.TestMethod( 1 ) + 10
	? u.TestUntypedMethod( 1,2,3 )
	? u.TestUntypedMethod( 1 )
	? u.TestUntypedMethod( )
RETURN

