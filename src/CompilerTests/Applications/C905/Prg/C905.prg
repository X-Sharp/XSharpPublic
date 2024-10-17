// 905. Problem using symbol literals #NULL_STRING, #NULL_DATE etc
// https://github.com/X-Sharp/XSharpPublic/issues/1456

// error XS9002: Parser: unexpected input '#'
FUNCTION Start() AS VOID
	LOCAL s AS SYMBOL
	s := #ABC
	xAssert(s == #ABC)
	s := #NULL
	xAssert(s == #NULL)
	s := #NULL_DATE
	xAssert(s == #NULL_DATE)
	s := #NULL_STRING
	xAssert(s == #NULL_STRING)
	s := #NULL_ARRAY
	xAssert(s == #NULL_ARRAY)
	? #NULL_DATE
	
	? Test(#NULL_PTR)

FUNCTION Test(u)
	? u
	xAssert(u == #NULL_PTR)
RETURN u

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
