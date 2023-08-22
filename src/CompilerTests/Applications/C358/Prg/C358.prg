USING System.Runtime.InteropServices

_DLL FUNCTION LoadLibrary( lpLibFileName AS STRING ) AS PTR PASCAL:KERNEL32.LoadLibraryA ANSI
_DLL FUNCTION FreeLibrary( hModule AS PTR ) AS VOID PASCAL:KERNEL32.FreeLibrary
_DLL FUNCTION GetProcAddress( hModule AS PTR, lpProcName AS STRING ) AS PTR PASCAL:KERNEL32.GetProcAddress ANSI

FUNCTION Start() AS VOID
   LOCAL hModule AS PTR
   LOCAL hFunc   AS PTR
   LOCAL hStr1   AS PTR
   LOCAL hStr2   AS PTR
   
   hModule := LoadLibrary( "user32.dll" )   
   hFunc   := GetProcAddress( hModule, "MessageBoxW" )   
   hStr1   := Marshal.StringToHGlobalUni( "Hello from XSharp")
   hStr2   := Marshal.StringToHGlobalUni( "__pcallnative() From XSharp" )
   ? "MessageBox() returns", PCallNative<INT>( hFunc, NULL, hStr1, hStr2, 3 )
   Marshal.FreeHGlobal( hStr1 )
   Marshal.FreeHGlobal( hStr2 )
   FreeLibrary( hModule )
   RETURN

