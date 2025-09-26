// 941. Late bound problem with PARAMS parameters
// https://github.com/X-Sharp/XSharpPublic/issues/1746
PUBLIC CLASS TestClass
	PUBLIC METHOD TestMethod(args PARAMS USUAL[]) AS VOID
		Console.WriteLine("TestMethod called with {0} arguments.", args:Length)
		?
		xAssert(args:Length == 3)
		xAssert(args[1] == 1)
		xAssert(args[2] == 2)
		xAssert(args[3] == 3)

	PUBLIC METHOD TestMethod2(X AS STRING, ARGS PARAMS USUAL[]) AS VOID
		?
		xAssert(args:Length == 3)
		xAssert(args[1] == 1)
		xAssert(args[2] == 2)
		xAssert(args[3] == 3)
END CLASS

FUNCTION Start() AS VOID STRICT
	LOCAL test := TestClass{} AS TestClass
	LOCAL testUntyped := test AS USUAL

	test:TestMethod(1, 2, 3)
	testUntyped:TestMethod(1, 2, 3)

	test:TestMethod2("asd", 1, 2, 3)
	testUntyped:TestMethod2("asd", 1, 2, 3)

PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

