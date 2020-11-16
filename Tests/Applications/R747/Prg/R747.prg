// R747. System.ExecutionEngineException
// Problem happens only with /vo11+ enabled and when the target is set to x86   
// https://github.com/X-Sharp/XSharpPublic/issues/511
FUNCTION Start( ) AS VOID
	LOCAL dNullable AS Nullable<Decimal>
	LOCAL dValue AS Decimal

	dNullable := 1m
//
	dValue := dNullable:Value // ok
	? dValue
	xAssert(dValue == 1m)

	dValue := (Decimal)dNullable // ok
	? dValue
	xAssert(dValue == 1m)

	// Note that c# does not allow this syntax at all, requires a cast or using :Value
	dValue := dNullable // System.ExecutionEngineException
	? dValue
	xAssert(dValue == 1m)
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

