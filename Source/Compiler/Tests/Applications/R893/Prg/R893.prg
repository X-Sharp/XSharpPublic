// Reported for 2.16
// missing AS TYPE clause caused internal error
FUNCTION Start() AS VOID
    ? Test(42,42)
    RETURN

FUNCTION Test (a := NIL,b := NIL as USUAL)
    return a * b
