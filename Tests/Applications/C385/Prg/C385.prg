// 385. Various compatibility issues with conversions and casts
FUNCTION Start() AS VOID

LOCAL b1 := 0xF1 AS BYTE
LOCAL b2 := 0x01 AS BYTE
LOCAL b3 := 0xF3 AS BYTE

? b1,b3 // warning killer

? INT64(b2)
? INT64(_CAST , b2)
xAssert(INT64(b2) == 0x01 , "INT64(b2)")
xAssert(INT64(_CAST , b2) == 0x01 , "INT64(_CAST , b2)")


? AsHexString(UINT64(b2))
? AsHexString(UINT64(_CAST , b2))
xAssert(AsHexString(UINT64(b2)) == "00000001" , "UINT64(b2)")
xAssert(AsHexString(UINT64(_CAST , b2)) == "00000001" , "UINT64(_CAST , b2)")


? AsHexString(INT64(b2))
? AsHexString(INT64(_CAST , b2))
xAssert(AsHexString(INT64(b2)) == "00000001" , "INT64(b2)")
xAssert(AsHexString(INT64(_CAST , b2)) == "00000001" , "INT64(_CAST , b2)")

LOCAL d := ConDate(1975,7,5) AS DATE // or SYMBOL, PSZ etc
? DWORD(_CAST , d)
? DWORD(_CAST , d + 1)
xAssert(DWORD(_CAST , d) == 2442599 , "DWORD(_CAST , d)")
xAssert(DWORD(_CAST , d + 1) == 2442600 , "DWORD(_CAST , d + 1)")
xAssert(DWORD(_CAST , d - 1) == 2442598 , "DWORD(_CAST , d - 1)")

PROC xAssert(l AS LOGIC , cExpression AS STRING)
IF .not. l
	THROW Exception{"Failed result: " + cExpression}
END IF
