VOSTRUCT ADS_MGMT_TABLE_INFO
   MEMBER DIM aucTableName[300] AS BYTE

FUNCTION Start() AS INT
LOCAL nTableCount := 10000 AS WORD
LOCAL n := 300 AS INT
LOCAL d AS DWORD
d := _SIZEOF(ADS_MGMT_TABLE_INFO) * nTableCount
? "assignment", d
xAssert(d == 3000000)

? "sizeof * WORD", DwFunc(_SIZEOF(ADS_MGMT_TABLE_INFO) * nTableCount) // the actual case in Trevor's code
xAssert(DwFunc(_SIZEOF(ADS_MGMT_TABLE_INFO) * nTableCount) == 3000000)
xAssert(_SIZEOF(ADS_MGMT_TABLE_INFO) * nTableCount == 3000000)

? "word * sizeof",DwFunc(nTableCount * _SIZEOF(ADS_MGMT_TABLE_INFO))
xAssert(DwFunc(nTableCount * _SIZEOF(ADS_MGMT_TABLE_INFO)) == 3000000)
xAssert(nTableCount * _SIZEOF(ADS_MGMT_TABLE_INFO) == 3000000)

? "constant * word", DwFunc(300 * nTableCount)  // this returns 50880 also in VO
xAssert(DwFunc(300 * nTableCount) == 3000000)
xAssert(300 * nTableCount == 3000000)

? "word * constant",DwFunc(nTableCount * 300)
xAssert(DwFunc(nTableCount * 300) == 3000000)
xAssert(nTableCount * 300 == 3000000)

? "DWORD(word*constant)",DwFunc(DWORD(n * nTableCount)) // this is the only one that does return 300000 in X#
xAssert(DwFunc(DWORD(n * nTableCount)) == 3000000)
xAssert(DWORD(n * nTableCount) == 3000000)

? "DWORD(constant*word)",DwFunc(DWORD(nTableCount * n))
xAssert(DwFunc(DWORD(nTableCount * n)) == 3000000)
xAssert(DWORD(nTableCount * n) == 3000000)

nTableCount ++
? "DWORD(sizeof * expression)",DwFunc(_SIZEOF(ADS_MGMT_TABLE_INFO) * (nTableCount-1))
xAssert(DwFunc(_SIZEOF(ADS_MGMT_TABLE_INFO) * (nTableCount-1)) == 3000000)
xAssert(_SIZEOF(ADS_MGMT_TABLE_INFO) * (nTableCount-1) == 3000000)

RETURN 0

FUNCTION DwFunc(d AS DWORD) AS DWORD
//? d
RETURN d 

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

