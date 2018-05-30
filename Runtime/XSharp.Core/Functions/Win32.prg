using System.Runtime.InteropServices
CLASS Win32
	    [DllImport("kernel32.dll", SetLastError := false, EntryPoint := "GetOEMCP")];
        extern static method GetDosCodePage() AS LONG
	    [DllImport("kernel32.dll", SetLastError := false, EntryPoint := "GetACP")];
        extern static method GetWinCodePage() as LONG

END CLASS