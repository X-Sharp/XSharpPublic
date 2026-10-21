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
		xAssert(TestFieldTyped.Length==4)
		xAssert(TestFieldTyped:Length==4)
		
		? TestPropTyped.Length  // XSharp.Error: Variable does not exist: TESTPROPTYPED
		xAssert(TestPropTyped.Length==3)
		xAssert(TestPropTyped:Length==3)

		TestField := "test"
		? TestField.Length // XSharp.Error: Variable does not exist: TESTFIELD
		xAssert(TestField.Length==4)
		xAssert(TestField:Length==4)

		? TestProp.Length  // XSharp.Error: Variable does not exist: TESTPROP
		xAssert(TestProp.Length==3)
		xAssert(TestProp:Length==3)
END CLASS

FUNCTION Start() AS VOID
	LOCAL t AS TestClass
	t := TestClass{}
	? t.TestField // XSharp.Error: Variable does not exist: T
	xAssert(t.TestField == "test")

	? t.TestProp // XSharp.Error: Variable does not exist: T
	xAssert(t.TestProp == "abc")

	? t.TestMethod( 1 ) + 10  // XSharp.Error: Variable does not exist: T
	xAssert(t.TestMethod( 1 ) == 2)

	? t.TestUntypedMethod( 1,2,3 ) + 10
	? t.TestUntypedMethod( 1 ) + 10
	? t.TestUntypedMethod(  ) + 10

	xAssert(t.TestUntypedMethod( 1,2,3 ) == 2)
	xAssert(t.TestUntypedMethod( 1 ) == 2)
	xAssert(t.TestUntypedMethod(  ) == 1000)

	xAssert(t:TestUntypedMethod( 1,2,3 ) == 2)
	xAssert(t:TestUntypedMethod( 1 ) == 2)
	xAssert(t:TestUntypedMethod(  ) == 1000)


	LOCAL u AS USUAL
	u := TestClass{}
	? u.TestField // XSharp.Error: Variable does not exist: U
	xAssert(u.TestField == "test")
	xAssert(u:TestField == "test")

	? u.TestProp // XSharp.Error: Variable does not exist: U
	xAssert(u.TestProp == "abc")
	xAssert(u:TestProp == "abc")

	? u.TestMethod( 1 ) + 10
	xAssert(u.TestMethod( 1 ) == 2)
	xAssert(u:TestMethod( 1 ) == 2)

	? u.TestUntypedMethod( 1,2,3 )
	? u.TestUntypedMethod( 1 )
	? u.TestUntypedMethod( )

	xAssert(u.TestUntypedMethod( 1,2,3 ) == 2)
	xAssert(u.TestUntypedMethod( 1 ) == 2)
	xAssert(u.TestUntypedMethod(  ) == 1000)

	xAssert(u:TestUntypedMethod( 1,2,3 ) == 2)
	xAssert(u:TestUntypedMethod( 1 ) == 2)
	xAssert(u:TestUntypedMethod(  ) == 1000)
RETURN

PROC xAssert(l AS LOGIC) AS VOID
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

