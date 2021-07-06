VOSTRUCT ADS_MGMT_TABLE_INFO
   MEMBER DIM aucTableName[300] AS BYTE

FUNCTION Start() AS INT
LOCAL nTableCount := 10000 AS WORD
LOCAL n := 300 AS INT
LOCAL d AS DWORD
d := _SIZEOF(ADS_MGMT_TABLE_INFO) * nTableCount
? "assignment", d
? "sizeof * WORD", DwFunc(_SIZEOF(ADS_MGMT_TABLE_INFO) * nTableCount) // the actual case in Trevor's code
? "word * sizeof",DwFunc(nTableCount * _SIZEOF(ADS_MGMT_TABLE_INFO))
? "constant * word", DwFunc(300 * nTableCount)  // this returns 50880 also in VO
? "word * constant",DwFunc(nTableCount * 300)
? "DWORD(word*constant)",DwFunc(DWORD(n * nTableCount)) // this is the only one that does return 300000 in X#
? "DWORD(constant*word)",DwFunc(DWORD(nTableCount * n))
nTableCount ++
? "DWORD(sizeof * expression)",DwFunc(_SIZEOF(ADS_MGMT_TABLE_INFO) * (nTableCount-1))
wait
RETURN 0

FUNCTION DwFunc(d AS DWORD) AS DWORD
//? d
RETURN d 
