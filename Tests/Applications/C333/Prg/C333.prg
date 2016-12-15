// 312. PCALL with class field 
// note: vulcan does not require /vo6 for this to compile. But it's not a big deal if we do require it in x#

#include "VOWin32APILibrary.vh"

FUNCTION Start() AS VOID
	TestClass{}:DoTest()

FUNCTION InitCommonControlsEx(lpicex AS PTR) AS LOGIC STRICT
RETURN FALSE

CLASS TestClass
	PROTECT gpfnInitCommonControlsEx AS InitCommonControlsEx PTR

	METHOD DoTest() AS VOID
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
	RETURN
END CLASS



