// 337. error XS0029: Cannot implicitly convert type '_winFIELDINFO*' to '_winFIELDINFO'
#pragma warnings(219, off) // variable is never used
#pragma warnings(649, off) // field never assigned
CLASS DataColumn INHERIT VObject
	PROTECT strucFI AS _WinFieldInfo
	ACCESS __FieldInfo AS _WINFieldInfo STRICT
	RETURN strucFI
END CLASS

FUNCTION Start() AS VOID
	LOCAL strucFI AS _WinFieldInfo
	LOCAL oDC AS DataColumn
	oDC := DataColumn{}
	strucFI := oDC:__FieldInfo

	StrucFI := PCALL(gpfnCntFocusFldGet, NULL_PTR)

VOSTRUCT _winFIELDINFO
	MEMBER lpNext AS _winFIELDINFO
	MEMBER lpPrev AS _winFIELDINFO
	MEMBER dwFieldInfoLen AS DWORD
//	......

STATIC GLOBAL gpfnCntFocusFldGet AS TCntFocusFldGet PTR
STATIC FUNCTION TCntFocusFldGet(hCntWnd AS PTR) AS _winFIELDINFO STRICT
RETURN NULL_PTR

