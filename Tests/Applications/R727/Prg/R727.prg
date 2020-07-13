// Github ticket 392 
// Special default argument for PTR type leads to XS7038 error (Failed to emit module) #392
DEFINE HWND_DESKTOP := PTR(_CAST,0)
FUNCTION ScrBits(hDevice := HWND_DESKTOP AS PTR) AS DWORD PASCAL
RETURN 0


FUNCTION Start AS VOID
	RETURN
