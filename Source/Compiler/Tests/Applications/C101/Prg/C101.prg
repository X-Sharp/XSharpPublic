#using System.Runtime.InteropServices
// vulcan does not require EXTERN
CLASS Test1
	[DllImport("gdi32.dll",  EntryPoint:="CreateSolidBrush")];
	STATIC METHOD CreateSolidBrush(hDC AS DWORD) AS IntPtr
END CLASS

