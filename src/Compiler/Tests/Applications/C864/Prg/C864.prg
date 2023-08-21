// https://github.com/X-Sharp/XSharpPublic/issues/1075
// 864. AccessViolationException with System.Decimal default parameter value
/*
System.AccessViolationException
Attempted to read or write protected memory. This is often an indication that other memory is corrupt.
*/
FUNCTION TestDecimalDefault(nDec := 123.5m AS DECIMAL) AS Decimal STRICT
	? nDec
	LOCAL d AS Decimal
	d := nDec
	? d
RETURN nDec

FUNCTION Start() AS VOID STRICT
	? TestDecimalDefault() // access violation
	xAssert(TestDecimalDefault() == 123.5m)
RETURN


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
