// 251. error XS0103: The name 'PCallNative' does not exist in the current context
_DLL FUNCTION LoadLibrary( lpLibFileName AS STRING ) AS PTR PASCAL:KERNEL32.LoadLibraryA ANSI
_DLL FUNCTION FreeLibrary( hModule AS PTR ) AS VOID PASCAL:KERNEL32.FreeLibrary
_DLL FUNCTION GetProcAddress( hModule AS PTR, lpProcName AS STRING ) AS PTR PASCAL:KERNEL32.GetProcAddress ANSI
GLOBAL hFuncTyped    AS MessageBox PTR


FUNCTION Start() AS VOID
   LOCAL hModule AS PTR
   LOCAL hStr1   AS IntPtr
   LOCAL hStr2   AS IntPtr
   LOCAL hFuncUnTyped   AS PTR
   hModule := LoadLibrary( "user32.dll" )   
   hFuncTyped    := GetProcAddress( hModule, "MessageBoxW" )   
   hFuncUnTyped  := hFuncTyped
   hStr1   := System.Runtime.InteropServices.Marshal.StringToHGlobalUni( "Hello from XSharp")
   hStr2   := System.Runtime.InteropServices.Marshal.StringToHGlobalUni( "pcall() From XSharp" )
   
   ? "MessageBox() returns with PCall()", PCall( hFuncTyped, IntPtr.Zero, hStr1, hStr2, 3 )
   System.Runtime.InteropServices.Marshal.FreeHGlobal( hStr2 )               
   
   hStr2   := System.Runtime.InteropServices.Marshal.StringToHGlobalUni( "pcallnative() From XSharp" )
   
   ? "MessageBox() returns with PCallNative()", PCallNative<INT>(hFuncUntyped, IntPtr.Zero, hStr1, hStr2, 3 )	
   System.Runtime.InteropServices.Marshal.FreeHGlobal( hStr1 )
   System.Runtime.InteropServices.Marshal.FreeHGlobal( hStr2 )
   FreeLibrary( hModule )
   RETURN
   
FUNCTION MessageBox(pParent AS PTR, pCaption AS PTR, pText AS PTR, nType AS INT) AS INT PASCAL   
	RETURN 0

CLASS FOo
	
	METHOD Bar AS VOID    
	   LOCAL hModule AS PTR
	   LOCAL hStr1   AS IntPtr
	   LOCAL hStr2   AS IntPtr
	   LOCAL hFuncUnTyped   AS PTR
	   hModule := LoadLibrary( "user32.dll" )   
	   hFuncTyped    := GetProcAddress( hModule, "MessageBoxW" )   
	   hFuncUnTyped  := hFuncTyped
	   hStr1   := System.Runtime.InteropServices.Marshal.StringToHGlobalUni( "Hello from XSharp")
	   hStr2   := System.Runtime.InteropServices.Marshal.StringToHGlobalUni( "pcall() From XSharp" )
	   
	   ? "MessageBox() returns with PCall()", PCall( hFuncTyped, IntPtr.Zero, hStr1, hStr2, 3 )
	   System.Runtime.InteropServices.Marshal.FreeHGlobal( hStr2 )               
	   
	   hStr2   := System.Runtime.InteropServices.Marshal.StringToHGlobalUni( "pcallnative() From XSharp" )
	   
	   ? "MessageBox() returns with PCallNative()", PCallNative<INT>(hFuncUntyped, IntPtr.Zero, hStr1, hStr2, 3 )	
	   System.Runtime.InteropServices.Marshal.FreeHGlobal( hStr1 )
	   System.Runtime.InteropServices.Marshal.FreeHGlobal( hStr2 )
	   FreeLibrary( hModule )
		RETURN
END CLASS	
