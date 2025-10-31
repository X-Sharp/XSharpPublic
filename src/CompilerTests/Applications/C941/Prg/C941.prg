// 941. Late bound problem with PARAMS parameters
// https://github.com/X-Sharp/XSharpPublic/issues/1746
PUBLIC CLASS TestClass
	PUBLIC METHOD TestMethod(args PARAMS USUAL[]) AS VOID
		? __FUNCTION__, i"called with {args.Length} arguments."
		xAssert(args:Length == 3)
		xAssert(args[1] == 1)
		xAssert(args[2] == 2)
		xAssert(args[3] == 3)

	PUBLIC METHOD TestMethod2(X AS STRING, ARGS PARAMS USUAL[]) AS VOID
		? __FUNCTION__, i"called with {args.Length+1} arguments."
		xAssert(args:Length == 3)
		xAssert(args[1] == 1)
		xAssert(args[2] == 2)
		xAssert(args[3] == 3)
    PUBLIC METHOD TestMethod2Obj(X AS STRING, ARGS PARAMS OBJECT[]) AS VOID
		? __FUNCTION__, i"called with {args.Length+1} arguments."
		xAssert(args:Length == 3)
		xAssert((int) args[1] == 1)
		xAssert((int) args[2] == 2)
		xAssert((int) args[3] == 3)
    PUBLIC METHOD TestMethod2Long(X AS STRING, ARGS PARAMS Long[]) AS VOID
		? __FUNCTION__, i"called with {args.Length+1} arguments."
		xAssert(args:Length == 3)
		xAssert((int) args[1] == 1)
		xAssert((int) args[2] == 2)
		xAssert((int) args[3] == 3)
END CLASS

FUNCTION Start() AS VOID STRICT
	LOCAL test := TestClass{} AS TestClass
	LOCAL testUntyped := test AS USUAL

	test:TestMethod(1, 2, 3)
	testUntyped:TestMethod(1, 2, 3)

	test:TestMethod2("asd", 1, 2, 3)
	test:TestMethod2Obj("asd", 1, 2, 3)
	test:TestMethod2Long("asd", 1, 2, 3)
	testUntyped:TestMethod2("asd", 1, 2, 3)
	testUntyped:TestMethod2Obj("asd", 1, 2, 3)
	testUntyped:TestMethod2Long("asd", 1, 2, 3)

PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

