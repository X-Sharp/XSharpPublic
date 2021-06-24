// C783 - Error with Real numbers
// https://github.com/X-Sharp/XSharpPublic/issues/704
FUNCTION Start( ) AS VOID
? 2/.1 // 20.0, OK
xAssert(2/.1 == 20.0)

// for all the following: error XS9002: Parser: unexpected input '/'
? 2./1
xAssert(2./1 == 2.0)
? 2./.1
xAssert(2./.1 == 20.0)
? 2.*.1
xAssert(2.*.1 == 0.2)
? 2.+.1
xAssert(2.+.1 == 2.1)


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

