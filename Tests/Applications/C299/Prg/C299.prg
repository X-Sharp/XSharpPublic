// 299. error XS0029: Cannot implicitly convert type 'System.IntPtr' to '_winCTIME'
VOSTRUCT _winCTIME
	MEMBER   byHour AS BYTE
	MEMBER     byMin AS BYTE
	MEMBER     bySec AS BYTE

FUNCTION Test() AS _winCTIME STRICT
RETURN NULL_PTR

FUNCTION Start() AS VOID
