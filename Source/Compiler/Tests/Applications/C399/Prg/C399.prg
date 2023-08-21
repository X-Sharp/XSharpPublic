// 399. error XS9035: The first argument to PCallNative<LONGINT> must be a 'pointer'.
// ok, I really need to get a good sleep to understand what he's doing here...
// NOTE: (Most of) the comments in the code are not mine!
FUNCTION Start() AS VOID
InitComCtl32()
InitCommonControls()
InitComCtl32()

_DLL FUNC InitCommonControls() AS VOID PASCAL:COMCTL32.InitCommonControls#50
_DLL FUNC GetModuleHandle(lpModuleName AS PSZ) AS PTR PASCAL:KERNEL32.GetModuleHandleA
_DLL FUNC GetProcAddress(hModule AS PTR, lpProcName AS PSZ) AS PTR PASCAL:KERNEL32.GetProcAddress

VOSTRUCT _WINDllVersionInfo
	MEMBER cbSize 			AS DWORD
	MEMBER dwMajorVersion	AS DWORD
	MEMBER dwMinorVersion	AS DWORD
	MEMBER dwBuildNumber	AS DWORD
	MEMBER dwPlatformID		AS DWORD

DELEGATE _ComCtl32Version(sDLLVersion AS _WINDLLVERSIONINFO) AS LONGINT
DELEGATE _InitCommonControls() AS VOID


FUNCTION InitComCtl32() AS VOID
STATIC LOCAL nPass := 0
nPass ++
? 
? "Pass" , nPass
IF nPass == 2
	? "(after calling InitCommonControls())"
END IF
	// this function is called automatically in Start() to initialize the COMCTL32 DLL.
	
	LOCAL hLib					    AS PTR
	LOCAL pDLLGetVersion			AS _ComCtl32Version PTR
    LOCAL pInitCommonControls	    AS _InitCommonControls PTR
	LOCAL sDLLVersion			    IS _winDllVersionInfo

	// this will always work because there is a reference to InitCommonControls() so the DLL is preloaded.
	hLib:=GetModuleHandle(String2Psz("COMCTL32"))
	? "hLib", hLib

	// this should never fail...but just incase
	IF (pDLLGetVersion:=GetProcAddress(hLib,String2Psz("DllGetVersion")))==NULL_PTR
		? "GetProcAddress() failed (ok in pass1)"
	// get the common control DLL version
	ELSE 
		sDLLVersion:cbSize	:=_SIZEOF(_WINDllVersionInfo)
	    PCallNative<LONGINT>(pDLLGetVersion,@sDLLVersion)
		? "Version:" , sDLLVersion:dwMajorVersion , sDLLVersion:dwMinorVersion
	ENDIF
	
	// initialize the controls common to all versions
	IF !(pInitCommonControls:=GetProcAddress(hLib,String2Psz("InitCommonControls")))==NULL_PTR
        PCallNative<VOID>(pInitCommonControls)
    ENDIF

RETURN
