// 382. Incorrect _Or() results
#define SWP_NOZORDER 0x0004
#define SWP_NOSIZE 0x0001
#define SWP_NOREDRAW 0x0008

FUNCTION Start() AS VOID
LOCAL n AS INT
LOCAL d AS DWORD

n := _OR(SWP_NOZORDER, SWP_NOSIZE, SWP_NOREDRAW)
d := _OR(SWP_NOZORDER, SWP_NOSIZE, SWP_NOREDRAW)
? n ,d
CheckResult(n,13)
CheckResult(d,13)

n := _OR(1,4,8)
d := _OR(1,4,8)
? n ,d
CheckResult(n,13)
CheckResult(d,13)

n := _OR(1,4,8,16)
d := _OR(1,4,8,16)
? n ,d
CheckResult(n,29)
CheckResult(d,29)

n := _OR(1,2,3,4)
d := _OR(1,2,3,4)
? n ,d
CheckResult(n,7)
CheckResult(d,7)

n := _OR(2,2,2)
d := _OR(2,2,2)
? n ,d
CheckResult(n,2)
CheckResult(d,2)

n := _OR(1,2)
d := _OR(1,2)
? n ,d
CheckResult(n,3)
CheckResult(d,3)

FUNCTION CheckResult(nResult AS INT , nCorrect AS INT) AS VOID
IF nResult != nCorrect
	ThrowException(Exception{String.Format("Incorrect result: {0} , {1}" ,nResult , nCorrect)})
END IF
FUNCTION CheckResult(nResult AS DWORD , nCorrect AS DWORD) AS VOID
IF nResult != nCorrect
	ThrowException(Exception{String.Format("Incorrect result: {0} , {1}" ,nResult , nCorrect)})
END IF
FUNCTION ThrowException(oException AS Exception) AS VOID
//TRY // uncomment to track with handled exceptions
	THROW oException
//END TRY
