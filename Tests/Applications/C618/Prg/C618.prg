// 618. In Core dialect, function calls resolve to same named methods
// (in VO dialect the call is resolved to the function as expected)

FUNCTION DoTest(c AS STRING) AS STRING
RETURN c
FUNCTION AnotherTest() AS VOID

FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	
	xAssert(o:DoTest(1) == "1")
	
	// results to stack overflow due to recursive call
	o:AnotherTest()
RETURN

CLASS TestClass
	METHOD DoTest(n AS INT) AS STRING
	RETURN DoTest(n:ToString()) // functions
	METHOD AnotherTest() AS VOID
	AnotherTest() // function
END CLASS



PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

