// error XS9002: Parser: unexpected input 'RETURN'

USING System.Runtime.InteropServices

CLASS TestClass

[RETURN: MarshalAs(UnmanagedType.Bool)];
[DllImport("user32", CharSet := CharSet.Ansi, SetLastError := TRUE, ExactSpelling := TRUE)];
STATIC METHOD SetForegroundWindow(hWnd AS System.IntPtr) AS LOGIC

END CLASS


