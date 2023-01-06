FUNCTION Start() AS VOID STRICT
    VAR x := StackAlloc<DWORD>(10)
    LOCAL dim y[10] as dword
    ? @x
    ? @y
    y[1] := x[1] := 42
    y[2] := x[2] := 42*42
    x[3] := 42 * x[2]
    y[4] := x[4] := 42 * x[3]
    ? x[1], x[2], x[3], x[4]
    ? y[1], y[2], y[3], y[4]
    Console.Read()
    return

