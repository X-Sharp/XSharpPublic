// 590. error XS1503: Argument 2: cannot convert from '<null>' to 'ref int'
/*
Surprisingly vulcan allows NULL to be passed for REF params and additionally REF params 
can be checked with @param == NULL to see if NULL was passed for them.

The IL code that vulcan generates for the call simply passes null (with ldnull) to the param

	IL_0020: ldc.i4.s 10
	IL_0022: ldnull
	IL_0023: call int32 C590.Exe.Functions::TempRef(int32, int32&)

Similarly, for the equality check, it compares to null again

	IL_0002: ldarg.1
	IL_0003: ldnull
	IL_0004: ceq
	IL_0006: brfalse IL_0017
*/
FUNCTION Start() AS VOID
	LOCAL n AS INT
	n := 1
	? TempRef(10 , n)
	? TempRef(10 , NULL)
	
	xAssert(TempRef(10 , n) == 11)
	xAssert(TempRef(10 , NULL) == 1976)
RETURN

FUNCTION TempRef(a AS INT, r REF INT) AS INT
	IF @r == NULL
		RETURN 1976
	END IF
RETURN a + r

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

