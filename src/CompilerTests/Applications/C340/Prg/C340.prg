// 340. error XS0149: Method name expected
FUNCTION Start() AS VOID
Test("12345")

FUNCTION Test(val AS STRING) AS VOID
IF SLen(val) != 0
	? Val(val)
END IF

FUNCTION TestClipper(val)
? Val(val) // OK
RETURN NIL

CLASS TestClass
	CONSTRUCTOR(val AS STRING)
	? Val(val)
END CLASS
FUNCTION SLen(c AS STRING) AS DWORD
RETURN (DWORD)c:Length
