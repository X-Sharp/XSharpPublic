// 312. PCALL sample
// Mote that this is copy & paste from Arne's code, so PCALL does not
// appear in the SDK only, but people have been uisng this as well!
// Note that this also needs /vo6
#define ICC_DATE_CLASSES 0x00000100
#define ICC_USEREX_CLASSES 0x00000200
#define ICC_COOL_CLASSES 0x00000400
#define ICC_INTERNET_CLASSES 0x00000800
#define ICC_LINK_CLASS 0x00008000

GLOBAL gpfnInitCommonControlsEx AS InitCommonControlsEx PTR
FUNCTION InitCommonControlsEx(lpicex AS PTR) AS LOGIC STRICT
RETURN FALSE

FUNCTION Start() AS VOID
	LOCAL hModule AS PTR
	LOCAL icex IS _winINITCOMMONCONTROLSEX
	LOCAL lpfnInitCommonControlsEx AS InitCommonControlsEx PTR

	hModule := GetModuleHandle(String2Psz("COMCTL32.DLL"))
	IF hModule == NULL_PTR
		hModule := LoadLibrary(String2Psz("COMCTL32.DLL"))
	ENDIF
	gpfnInitCommonControlsEx := GetProcAddress(hModule, String2Psz("InitCommonControlsEx"))
	lpfnInitCommonControlsEx := gpfnInitCommonControlsEx
	
	? gpfnInitCommonControlsEx

	IF (gpfnInitCommonControlsEx != NULL_PTR)
		icex:dwSize := _SIZEOF(_winINITCOMMONCONTROLSEX)
		icex:dwICC := _OR(ICC_DATE_CLASSES, ICC_USEREX_CLASSES, ICC_COOL_CLASSES, ICC_INTERNET_CLASSES, ICC_LINK_CLASS)
		? !PCALL(gpfnInitCommonControlsEx, @icex)
		? !PCALL(lpfnInitCommonControlsEx, @icex)
		// prints TRUE in vulcan
	ENDIF


