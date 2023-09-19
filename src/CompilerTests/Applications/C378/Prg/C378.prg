// 378. Runtime problem calculating INT(_CAST, SLen())
#pragma warnings(219, off) // assigned but not used
FUNCTION Start() AS VOID
LOCAL c := "abcde" AS STRING
LOCAL n AS INT
n := INT(_CAST, SLen(c))
? n
IF n != 5
	THROW Exception{"Incorrect value " + n:ToString()}
END IF
n := INT(_CAST, SLen(c)+1)
? n
IF n != 6
	THROW Exception{"Incorrect value " + n:ToString()}
END IF

LOCAL o AS StandardFileDialog
o := StandardFileDialog{}

FUNCTION SLen(c AS STRING) AS DWORD
RETURN (DWORD)c:Length


CLASS StandardFileDialog
	PROTECT iFilterLen AS INT
CONSTRUCTOR(uOwner, cInitPath)
	LOCAL pszFilters AS PSZ
	LOCAL sFilters AS STRING

	sFilters := "filtershere"
	iFilterLen := INT(_CAST, SLen(sFilters)+1)
	? SLen(sFilters)+1
	? iFilterLen
	IF 12 != iFilterLen
		THROW Exception{"Incorrect iFilterLen value " + iFilterLen:ToString()}
	END IF
	pszFilters := MemAlloc(DWORD(iFilterLen))
	MemSet(pszFilters, 0, DWORD(iFilterLen))
	MemCopy(pszFilters, String2Psz(sFilters), DWORD(iFilterLen-1) )

	? pszFilters , sFilters
END CLASS

