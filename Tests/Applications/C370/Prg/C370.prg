// 360. error XS0029: Cannot implicitly convert type '_WinHostEnt' to '_WinHostEnt*'
_DLL FUNCTION GetHostByName( name AS PSZ) AS _WinHostEnt PASCAL:WSOCK32.GetHostByName

VOSTRUCT  _WinHostEnt
	MEMBER  h_name AS PTR
	MEMBER  h_aliases AS PTR
	MEMBER  h_addrtype AS SHORTINT
	MEMBER  h_length AS SHORTINT
	MEMBER  h_addr_list AS PTR

FUNCTION Start() AS VOID
	LOCAL cHost     AS STRING
	LOCAL pHostEnt  AS _WinHostEnt

	cHost := "test"
	pHostEnt := GetHostByName(String2Psz(cHost))
	? pHostEnt

