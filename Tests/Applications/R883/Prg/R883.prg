FUNCTION Start() AS VOID STRICT
    VAR x := StackAlloc <dword>{1,2,3,4,5,6,7,8,9,10}
    VAR y := StackAlloc dword[]{10}
    //var z := STACKALLOC 10
    local STACKALLOC := "sometext" as string
    ? STACKALLOC
    xAssert(x[1] == 1)
    xAssert(x[10] == 10)
    ? x[1], x[2], x[3], x[4]
    ? y[1], y[2], y[3], y[4]
    y[1] := x[1] := 42
    y[2] := x[2] := 42*42
    x[3] := 42 * x[2]
    y[4] := x[4] := 42 * x[3]
    ? x[1], x[2], x[3], x[4]
    ? y[1], y[2], y[3], y[4]
    xAssert(x[1] == 42)
    xAssert(y[1] == 42)
    xAssert(x[4] == 42*42*42*42)
    xAssert(y[4] == 42*42*42*42)
    ? @x
    for var i := 1 to 10
        ? i, @x[i], x[i]
    next
    ? " outside of boundaries, no exception, this may be dangerous"

    for var i := 11 to 20
        ? i, @x[i], x[i]
    next
    return


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
