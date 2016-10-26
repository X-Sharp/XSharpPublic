#using System.Runtime.InteropServices
CLASS Test2
	[DllImport("gdi32.dll",  EntryPoint:="CreateSolidBrush")];
	STATIC EXTERN METHOD CreateSolidBrush(hDC AS DWORD) AS IntPtr
END CLASS

FUNCTION Start() AS VOID

RETURN
