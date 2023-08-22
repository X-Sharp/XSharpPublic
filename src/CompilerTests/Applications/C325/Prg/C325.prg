// 291. error XS0211: Cannot take the address of the given expression
// when reading the VOStruct from a dll
#pragma warnings(219, off) // assigned but not used
#pragma warnings(165, off) // unassigned local
FUNCTION Start() AS VOID
	LOCAL pData IS _WINWIN32_FIND_DATA
	pData:cFileName[1] := 65
	pData:cFileName[2] := 66
	pData:cFileName[3] := 67

    LOCAL cTemp AS STRING
    cTemp := Psz2String(@pData:cFileName)
    ? @pData:ftCreationTime
    ? cTemp

    IF TRUE
    	LOCAL pAs AS _WINWIN32_FIND_DATA
    	LOCAL p AS PTR
    	p := @pAs:ftCreationTime
    	p := @pAs:cFileName
    END IF

// redefining the structure here works ok
/*
VOSTRUCT _WINWIN32_FIND_DATA
	MEMBER dwFileAttributes AS DWORD
	MEMBER ftCreationTime IS _WINFILETIME
	MEMBER ftLastAccessTime IS _WINFILETIME
	MEMBER ftLastWriteTime IS _WINFILETIME
	MEMBER nFileSizeHigh AS DWORD
	MEMBER nFileSizeLow AS DWORD
	MEMBER dwReserved0 AS DWORD
	MEMBER dwReserved1 AS DWORD
	MEMBER DIM cFileName[ 255 ] AS BYTE
	MEMBER DIM cAlternateFileName[ 14 ] AS BYTE

VOSTRUCT _WINFILETIME
	MEMBER dwLowDateTime AS  DWORD
	MEMBER dwHighDateTime AS DWORD
*/
