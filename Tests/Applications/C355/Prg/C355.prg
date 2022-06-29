// 355. Assertion failed and compiler crash with DWORD(_CAST , value)
#pragma warnings(9032, off) // unassigned local
FUNCTION Start() AS VOID
	? DWORD (_CAST , 0x00000102L) // compiler crash
	? DWORD (_CAST ,0) // compiler crash


	Test()
RETURN

// original code:
#define STATUS_TIMEOUT DWORD (_CAST, 0x00000102L)
#define WSA_WAIT_TIMEOUT STATUS_TIMEOUT
CLASS CSocket
	PROTECT nLastError AS DWORD
	ACCESS Error AS DWORD STRICT
	RETURN SELF:nLastError
	ASSIGN Error(n AS DWORD)  STRICT
	RETURN SELF:nLastError := n
	METHOD __ConnectThread() AS LOGIC STRICT
		IF SELF:Error = 0
			SELF:Error := WSA_WAIT_TIMEOUT // compiler crash here
		ENDIF
	RETURN FALSE
END CLASS

FUNCTION Test() AS VOID
	LOCAL socket AS CSocket
	socket := CSocket{}
	? socket:__ConnectThread()
RETURN

