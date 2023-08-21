// 409. error XS0601: The DllImport attribute must be specified on a method marked 'static' and 'extern'
// warning XS9025: Missing RETURN statement. A statement with a default 'empty' return value is returned.

// note: happens only with /vo9+

#using System.Runtime.InteropServices
#pragma warnings (9068, off) // psz
[DllImport("user32.dll", CharSet := CharSet.Ansi)];
FUNCTION MessageBox(hwnd AS PTR, lpText AS PSZ, lpCaption AS PSZ, uType AS DWORD);
	AS INT PASCAL

_DLL FUNCTION _DLL_MessageBox(hwnd AS PTR, lpText AS PSZ, lpCaption AS PSZ, uType AS DWORD);
	AS INT PASCAL:USER32.MessageBoxA

FUNCTION Start( ) AS VOID
	_DLL_MessageBox(NULL_PTR , "message box text" , "from _DLL FUNC" , 1)
	MessageBox(NULL_PTR , "message box text" , "from DllImport" , 1)
	Foo.MessageBox1(NULL_PTR , "message box text" , "from Static method" , 1)
RETURN


STATIC CLASS Foo
	[DllImport("user32.dll", CharSet := CharSet.Ansi, EntryPoint := "MessageBox")];
	STATIC EXTERN METHOD MessageBox1(hwnd AS PTR, lpText AS PSZ, lpCaption AS PSZ, uType AS DWORD);
		AS INT PASCAL


END CLASS
