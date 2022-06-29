// error XS1503: Argument 2: cannot convert from 'string' to 'System.IntPtr'
// similar to C341, but this one uses the function as defined in the vulcan-compiled Win32APILibrary dll file
// the problem is that Vulcan does not mark the parameters in the DLL as __Psz, but marks them as IntPtr
// and added an ActualTypeAttribute that says that the expected parameters should be PSZ
#pragma warnings(9068, off) // automatic psz
FUNCTION Start( ) AS VOID
	LOCAL cText := "message box text" AS STRING
	MessageBox(NULL_PTR , cText , "caption" , 1)
RETURN

// redefining it here works ok
/*_DLL FUNCTION MessageBox(hwnd AS PTR, lpText AS PSZ, lpCaption AS PSZ, uType AS DWORD);
	AS INT PASCAL:USER32.MessageBoxA*/

