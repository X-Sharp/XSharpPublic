// 697. Cannot pass PUBLIC/PRIVATE by reference
FUNCTION Start() AS VOID
	PRIVATE pr
	PUBLIC pu
	pr := 123
	pu := "abc"

	testproc(@pr,pu) // error XS9079: Cannot take the address of an aliased expression.
	? pr,pu
	xAssert(pr == -1)
	xAssert(pu == "abc")

	testproc(pr,REF pu) // error XS0206: A property or indexer may not be passed as an out or ref parameter
	? pr,pu
	xAssert(pr == -1)
	xAssert(pu == 2)
RETURN

PROCEDURE testproc(a,b)
	a := -1
	b := 2
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

