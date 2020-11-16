// 312. PCALL with class field 
// note: vulcan does not require /vo6 for this to compile. But it's not a big deal if we do require it in x#

#define ICC_DATE_CLASSES 0x00000100
#define ICC_USEREX_CLASSES 0x00000200
#define ICC_COOL_CLASSES 0x00000400
#define ICC_INTERNET_CLASSES 0x00000800
#define ICC_LINK_CLASS 0x00008000


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



