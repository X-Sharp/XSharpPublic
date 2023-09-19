// See https://github.com/X-Sharp/XSharpPublic/issues/1339
USING System
#pragma warnings(219, off) // cTmpDir is assigned but not used
GLOBAL callCount := 0 AS LONG

FUNCTION Start() AS VOID STRICT

    LOCAL cTmpDir AS STRING

    IF SLen((cTmpDir := JoinFiles())) == 0
        xAssert(CallCount == 1)
		RETURN
    ENDIF
    xAssert(CallCount == 1)
    RETURN

FUNCTION JoinFiles() AS STRING STRICT
    ? ++CallCount

    RETURN ""

FUNCTION SLen(c as STRING) AS DWORD
    RETURN (DWORD) c:Length

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
