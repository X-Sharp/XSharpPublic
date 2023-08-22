// 651. Runtime error when omitting default param to function call
// Parameters after the default one get their defalt values, instead of what is passed to them
GLOBAL ggg AS PTR
FUNCTION Start() AS VOID

Test1(,"asd")

Test2(111,"111")

ggg := @ggg
Test3(,222,ggg)
RETURN

FUNCTION Test1(n := 123 AS INT, s AS STRING) AS VOID
? n
? s
xAssert(n == 123)
xAssert(s == "asd")

FUNCTION Test2(n := 123 AS INT, s AS STRING) AS VOID
? n
? s
xAssert(n == 111)
xAssert(s == "111")

FUNCTION Test3(s := "" AS STRING, d AS DWORD, p AS PTR) AS VOID
? s,d,p
xAssert(s == "")
xAssert(d == 222)
xAssert(p == ggg)
xAssert(ggg == p)

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

