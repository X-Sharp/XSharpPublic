// 748. error XS1628: Cannot use ref, out, or in parameter 'n' inside an anonymous method, lambda expression, query expression, or local function
FUNCTION Start() AS VOID
LOCAL n := 1 AS INT
TestClass.TestMethod(REF n)
? n
xAssert(n == 2)

PUBLIC CLASS TestClass
// error XS1628: Cannot use ref, out, or in parameter 'n' inside an anonymous method, lambda expression, query expression, or local function
STATIC METHOD TestMethod(n REF INT) AS VOID
n := 2
END CLASS


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

