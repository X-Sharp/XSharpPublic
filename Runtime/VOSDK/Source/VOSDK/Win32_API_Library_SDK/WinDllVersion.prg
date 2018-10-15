PARTIAL CLASS WinDLLVersion
	//PP-030909
	INSTANCE cDLLName AS STRING
	EXPORT MajorVersion AS DWORD
	EXPORT MinorVersion AS DWORD
	EXPORT Build AS DWORD
	EXPORT PlatformId AS DWORD

ACCESS DLL  AS STRING 
	RETURN SELF:cDLLName

ASSIGN DLL(cDLL AS STRING) 
	LOCAL hDLL AS PTR
	LOCAL hFunc AS PTR
	LOCAL pVersionInfo IS _WINDLLVERSIONINFO	//RvdH 060413 changed from AS to IS
	LOCAL hResult AS INT

	SELF:cDLLName := cDLL

	IF ! (hDLL := GetModuleHandle(String2Psz(cDLL))) == NULL_PTR
		IF ! (hFunc := GetProcAddress(hDLL,String2Psz("DllGetVersion"))) == NULL_PTR
#ifdef __VULCAN__			
			pVersionInfo:cbSize := _sizeof(_winDLLVERSIONINFO)
			hResult := PCallNative<INT>(hFunc,@pVersionInfo)
#else			
			pVersionInfo.cbSize := _sizeof(_winDLLVERSIONINFO)
			hResult := PCALL(hFunc,@pVersionInfo)
#endif			
         IF hResult == 0
				SELF:MajorVersion := pVersionInfo:dwMajorVersion
				SELF:MinorVersion := pVersionInfo:dwMinorVersion
				SELF:Build := pVersionInfo:dwBuildNumber
				SELF:PlatformId := pVersionInfo:dwPlatformId
			ENDIF
		ENDIF
	ENDIF
	RETURN 

CONSTRUCTOR(cDLL AS STRING) 
	SELF:DLL := cDLL
	RETURN 

ACCESS IsWinNT() AS LOGIC
	RETURN SELF:PlatformId == DLLVER_PLATFORM_NT

END CLASS

VOSTRUCT _winDLLVERSIONINFO
	//PP-030902
	MEMBER cbSize AS DWORD
	MEMBER dwMajorVersion AS DWORD
	MEMBER dwMinorVersion AS DWORD
	MEMBER dwBuildNumber AS DWORD
	MEMBER dwPlatformId AS DWORD



#region defines
DEFINE SYS_WIN32     := .T.
DEFINE DLLVER_PLATFORM_NT              := 0x00000002      // Windows NT
DEFINE DLLVER_PLATFORM_WINDOWS         := 0x00000001      // Windows 9x
#endregion
