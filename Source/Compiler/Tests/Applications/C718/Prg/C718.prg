// 718. Problems with using  "- <DWORD>"

// Assigning "u := -d" as below, in VO assigns the value of -1, while in X# it assigns MAXDWORD
// If /ovf is enabled, then an overflow exception is thrown (it's not in VO)
// The following code did run with no erros in builds previous to 2.3b
FUNCTION Start() AS VOID
	LOCAL u AS USUAL
	LOCAL d AS DWORD
	d := 1
	u := - d
	? u
	xAssert(u == -1)


	LOCAL i64 AS INT64
	LOCAL dw AS DWORD
	dw := 1
	? Test(-dw)
	xAssert(Test(-dw) == "INT64-1")

	i64 := -dw
	? i64
	xAssert(i64 == -1)
RETURN

FUNCTION Test(dw AS DWORD) AS STRING
? "DWORD", dw
RETURN "DWORD" + dw:ToString()
FUNCTION Test(i64 AS INT64) AS STRING
? "INT64", i64
RETURN "INT64" + i64:ToString()

PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF   
RETURN
