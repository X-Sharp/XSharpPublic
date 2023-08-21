// error XS0029: Cannot implicitly convert type 'string' to 'Vulcan.__Psz'
#pragma warnings(9068, off) // automatic psz
_DLL FUNCTION MessageBox(hwnd AS PTR, lpText AS PSZ, lpCaption AS PSZ, uType AS DWORD);
	AS INT PASCAL:USER32.MessageBoxA

FUNCTION Start( ) AS VOID
	LOCAL p AS PSZ
	p := "psz"
	? p

	MessageBox(NULL_PTR , "message box text" , "caption" , 1)
RETURN
