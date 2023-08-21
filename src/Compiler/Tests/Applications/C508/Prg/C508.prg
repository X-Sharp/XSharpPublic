// 508. error XS0034: Operator '==' is ambiguous on operands of type 'byte*' and 'byte*'
// error only with with /vo7+

// without /vo7, it compiles without errors

FUNCTION Start( ) AS VOID
	LOCAL p1,p2 AS BYTE PTR
	LOCAL b AS BYTE
	p1 := @b
	p2 := @b
	? p1,p2
	? p1 == p2
	? p1 != p2
	IF p1 == p2
		? "equal"
	END IF
	xAssert(p1 == p2)
	xAssert(.not. (p1 != p2) )
	
	test(p1 , REF p2)

	LOCAL b2 AS BYTE
	
	p2 := @b2
	? p1 == p2
	? p1 != p2
	xAssert(.not. (p1 == p2) )
	xAssert(p1 != p2)
RETURN

FUNCTION Test(p1 AS BYTE PTR , p2 REF BYTE PTR) AS VOID
	? p1 == p2
	? p1 != p2
	xAssert(p1 == p2)
	xAssert(.not. (p1 != p2) )
RETURN 


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

