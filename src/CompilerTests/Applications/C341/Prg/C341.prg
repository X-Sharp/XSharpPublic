// error XS0029: Cannot implicitly convert type 'string' to 'Vulcan.__Psz'
#pragma warnings(9068, off) // automatic psz
_DLL FUNCTION MessageBox(hwnd AS PTR, lpText AS PSZ, lpCaption AS PSZ, uType AS DWORD);
	AS INT PASCAL:USER32.MessageBoxA

_DLL FUNCTION MessageBox_NoCoonventionClause(hwnd AS PTR, lpText AS PSZ, lpCaption AS PSZ, uType AS DWORD);
	AS INT:USER32.MessageBoxA

FUNCTION Start( ) AS VOID
	LOCAL p AS PSZ
	p := "psz"
	? p

	? MessageBox(NULL_PTR , "message box text" , "caption" , 1)
	? MessageBox_NoCoonventionClause(NULL_PTR , "message box text" , "caption" , 2)
RETURN
