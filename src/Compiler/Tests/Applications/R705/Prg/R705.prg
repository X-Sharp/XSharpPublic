VOSTRUCT SomeStruct
	MEMBER pMessageBox AS MyMessageBox PTR
GLOBAL gMessageBox as MyMessageBox PTR	
	
FUNCTION MyMessageBox(hwnd AS PTR, lpText AS PSZ, lpCaption AS PSZ, uType AS DWORD) AS INT 
	RETURN 0

	
	
FUNCTION Start as void
	local sSomeStruct IS SomeStruct		
	local hUser as PTR
	hUser := GetModuleHandle(String2Psz("User32.DLL"))
    sSomeStruct.pMessageBox := GetProcAddress(hUser, String2Psz("MessageBoxA"))     
    PCall(sSomeStruct.pMessageBox,NULL_PTR, String2Psz("Text"), String2psz("Caption"),0)
    RETURN
	


_DLL FUNC GetProcAddress(hModule AS PTR, lpProcName AS PSZ) AS PTR PASCAL:KERNEL32.GetProcAddress
_DLL FUNC GetModuleHandle(lpModuleName AS PSZ) AS PTR PASCAL:KERNEL32.GetModuleHandleA

