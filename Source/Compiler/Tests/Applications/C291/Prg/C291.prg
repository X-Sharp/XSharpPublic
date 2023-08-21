// 291. error XS0211: Cannot take the address of the given expression
VOSTRUCT _WINWIN32_FIND_DATA
	MEMBER DIM cFileName[ 10 ] AS BYTE

FUNCTION Start() AS VOID
	LOCAL pData IS _WINWIN32_FIND_DATA
	pData:cFileName[1] := 65
	pData:cFileName[2] := 66
	pData:cFileName[3] := 67
	
    LOCAL cTemp AS STRING
    cTemp := Psz2String(@pData:cFileName)
    ? cTemp
    // vulcan prints correctly ABC
