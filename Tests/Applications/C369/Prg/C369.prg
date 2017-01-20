// 369. BOGUS warning XS0675: Bitwise-or operator used on a sign-extended operand; consider casting to a smaller unsigned type first
///warnaserrors
// with /vo4+ only, no warnign without it
FUNCTION Start() AS VOID
LOCAL ch0 := 0 AS BYTE
LOCAL val1 := 0 AS DWORD
? _Or( (0 + DWORD(_CAST, ch0) ), val1) // 0
? sndAlias(1,2) // 513

IF _Or( (0 + DWORD(_CAST, ch0) ), val1) != 0
	THROW Exception{"Incorrect result"}
END IF
IF sndAlias(1,2) != 513
	THROW Exception{"Incorrect result"}
END IF

#define SND_ALIAS_START 0
FUNCTION sndAlias(ch0 AS BYTE, ch1 AS BYTE) AS DWORD
	LOCAL val1 AS DWORD
	val1 := DWORD(_CAST,ch1) <<8
	RETURN(_Or((SND_ALIAS_START + DWORD(_CAST, ch0)), val1))

