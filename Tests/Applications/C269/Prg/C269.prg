// compiler crash
FUNCTION Start() AS VOID
LOCAL r8 := 1.1 AS REAL8
LOCAL r4 := 1.1 AS REAL4
LOCAL d := 1.1m AS System.Decimal
? r8 < 10000000000
? r4 < 10000000000
? d < 10000000000

