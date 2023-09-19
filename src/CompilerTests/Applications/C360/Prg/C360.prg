// 360. (runtime) Unhandled Exception: System.InvalidOperationException: Nullable object must have a value.
// /vo2+, so p1 := NULL_STRING should default to a ""
FUNCTION DBFDebug( p1 := NULL_STRING AS STRING) AS VOID
? p1 == NULL // .T., should be false
? p1 == NULL_STRING // .F., should be true
? SLen(p1)
? p1:IndexOf('a')

IF p1 == NULL
	THROW Exception{"p1 == NULL"}
END IF
IF .not. (p1 == NULL_STRING)
	THROW Exception{"p1 == NULL_STRING"}
END IF
IF SLen(p1) != 0
	THROW Exception{"SLen(p1) != 0"}
END IF
IF p1:IndexOf('a') != -1
	THROW Exception{"p1:IndexOf('a') != -1"}
END IF

FUNCTION Start() AS VOID
DBFDebug()

FUNCTION SLen(c AS STRING) AS DWORD
RETURN (DWORD)c:Length
