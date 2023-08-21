// 549. Compiler crash with PSZ _CAST and PCALL/PCallNative

_DLL FUNC GetModuleHandle(lpModuleName AS PSZ) AS PTR PASCAL:KERNEL32.GetModuleHandleA
_DLL FUNC GetProcAddress(hModule AS PTR, lpProcName AS PSZ) AS PTR PASCAL:KERNEL32.GetProcAddress

_DLL FUNCTION CharUpper( lpsz AS PSZ) AS PSZ PASCAL:USER32.CharUpperA

FUNCTION Start() AS VOID
	LOCAL hModule, pFunc AS PTR
	hModule := GetModuleHandle(String2Psz("USER32.DLL"))
	pFunc := GetProcAddress(hModule, String2Psz("CharUpperA"))

	LOCAL p1,p2 AS PSZ
	
	p1 := String2Psz("AaBb")
	// ok:
	p2 := CharUpper(p1)
	? p1,p2

	p1 := String2Psz("AaBb")
	// ok:
	p2 := PCALLNative<PSZ>(pFunc , p1)
	? p1,p2

	p1 := String2Psz("AaBb")
	// ok:
	p2 := PCALLNative<PSZ>(pFunc , p1)
	? p1,p2

	p1 := String2Psz("AaBb")
	// compiler crash, should be error about untyped pFunc instead:
	p2 := PSZ(_CAST , PCALL(pFunc , p1))
	// compiler crash also here:
	p2 := PSZ(_CAST , PCallNative<PSZ>(pFunc , p1))
	? p1,p2
RETURN
