// 991. Problems with the /fox3 option - using colon for members access #2141
// https://github.com/X-Sharp/XSharpPublic/issues/2141

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("lb", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)

CLASS TestDotsAndColons
	EXPORT TestField := "abc" AS USUAL
	EXPORT TestFieldTyped := "abc" AS STRING
	
	PROPERTY TestProp AS USUAL GET "abc"
	PROPERTY TestPropTyped AS STRING GET "abc"

	METHOD DoTest() AS VOID
		TestFieldTyped := "test"

		? TestFieldTyped.Length // OK
		? TestFieldTyped:Length // runtime error

		? TestPropTyped.Length // OK
		? TestPropTyped:Length // runtime error

		TestField := "test"
		? TestField.Length // OK
		? TestField:Length // runtime error

		? TestProp.Length // OK
		? TestProp:Length // runtime error
END CLASS

FUNCTION Start() AS VOID
	TestDotsAndColons{}:DoTest()

