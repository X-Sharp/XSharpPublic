// 312. PCALL sample
// Mote that this is copy & paste from Arne's code, so PCALL does not
// appear in the SDK only, but people have been uisng this as well!
// Note that this also needs /vo6
#include "VOWin32APILibrary.vh"
GLOBAL gpfnInitCommonControlsEx AS InitCommonControlsEx PTR
FUNCTION InitCommonControlsEx(lpicex AS PTR) AS LOGIC STRICT
RETURN FALSE

FUNCTION Start() AS VOID
	LOCAL hModule AS PTR
	LOCAL icex IS _winINITCOMMONCONTROLSEX

	hModule := GetModuleHandle(String2Psz("COMCTL32.DLL"))
	IF hModule == NULL_PTR
		hModule := LoadLibrary(String2Psz("COMCTL32.DLL"))
	ENDIF
	gpfnInitCommonControlsEx := GetProcAddress(hModule, String2Psz("InitCommonControlsEx"))
	
	? gpfnInitCommonControlsEx

	IF (gpfnInitCommonControlsEx != NULL_PTR)
		icex:dwSize := _SIZEOF(_winINITCOMMONCONTROLSEX)
		icex:dwICC := _OR(ICC_DATE_CLASSES, ICC_USEREX_CLASSES, ICC_COOL_CLASSES, ICC_INTERNET_CLASSES, ICC_LINK_CLASS)
		? !PCALL(gpfnInitCommonControlsEx, @icex)
		// prints TRUE in vulcan
	ENDIF


