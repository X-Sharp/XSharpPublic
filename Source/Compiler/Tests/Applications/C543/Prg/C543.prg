// 543. error XS0236: A field initializer cannot reference the non-static field, method, or property 'TestClass.aHints'
CLASS TestClass
	EXPORT aHints := {1,2,3} AS ARRAY
	EXPORT nHintLen := (INT)ALen(aHints) AS INT
	EXPORT nHintLen0 := (INT)ALen(SELF:aHints) AS INT
	
	EXPORT n := 1 AS INT
	EXPORT m := n AS INT
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	xAssert(o:nHintLen == 3)
	xAssert(o:nHintLen0 == 3)

	xAssert(o:n == 1)
	xAssert(o:m == 1)
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

