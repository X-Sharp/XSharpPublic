// error XS1503: Argument 2: cannot convert from 'dword' to 'System.IntPtr'
// using the function directly form the VOSDK dll

FUNCTION Start() AS VOID
LOCAL hWindow AS PTR
LOCAL dw := 0 AS DWORD
hWindow := @dw
RemoveProp(hWindow, dw)

// defining it here works ok
//_DLL FUNCTION RemoveProp( hwnd AS PTR, lpString AS PSZ) AS PTR PASCAL:USER32.RemovePropA
