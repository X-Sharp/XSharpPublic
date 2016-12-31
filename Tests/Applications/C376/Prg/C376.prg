// 376. Runtime Unhandled Exception: System.InvalidProgramException: Common Language Runtime detected an invalid program.
// at Test.Dispatch(__Usual[] Xs$Args)
GLOBAL gpfnDragFinish AS TDragFinish PTR
FUNCTION TDragFinish(hDrop AS PTR) AS VOID STRICT
RETURN

CLASS Test
	METHOD Dispatch() AS VOID
		? "entered dispatch" // exception happens before this line
		IF __LoadShellDll()
			PCALL(gpfnDragFinish, PTR(_CAST, 0)) // commenting this no more runtime error
			? "PCALLed"
		ENDIF
	RETURN
END CLASS

_DLL FUNC GetProcAddress(hModule AS PTR, lpProcName AS PSZ) AS PTR PASCAL:KERNEL32.GetProcAddress
_DLL FUNC LoadLibrary(lpLibFileName AS PSZ) AS PTR PASCAL:KERNEL32.LoadLibraryA	
FUNCTION __LoadShellDll() AS LOGIC
	LOCAL hDll AS PTR
	hDll := LoadLibrary(String2Psz( "SHELL32.DLL"))
	gpfnDragFinish  := GetProcAddress(hDll, String2Psz("DragFinish"))
	? hDll, gpfnDragFinish
RETURN TRUE

FUNCTION Start() AS VOID
LOCAL o AS Test
o := Test{}
? "before dispatch"
o:Dispatch() // exception here

