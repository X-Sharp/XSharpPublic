// 772. Runtime error with logic expression between USUALs in literal array #597
// https://github.com/X-Sharp/XSharpPublic/issues/597
/*
XSharp.Error
Value does not fall within the expected range.

Callstack : 
static __Usual XSharp.__Usual.op_BitwiseAnd(XSharp.__Usual lhs, XSharp.__Usual rhs)()   :  C:\xSharp\DevRt\Runtime\XSharp.RT\Types\Usual.prg  :  1912
*/
FUNCTION Start( ) AS VOID
	LOCAL uFalse := FALSE AS USUAL
	LOCAL uTrue := TRUE AS USUAL
	LOCAL uResult AS USUAL

	uResult := uFalse .or. uTrue
	? uResult
	xAssert(uResult)

	uResult := uTrue .and. uTrue
	? uResult
	xAssert(uResult)
	
    // compiler here incorrectly used bitwise or/and
	uResult := {uFalse .or. uTrue}
	xAssert(uResult[1])

	uResult := {uTrue .and. uTrue}
	xAssert(uResult[1])
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

