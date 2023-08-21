// 425. No error should be reported when compiling with /vo7+ 
// See also R731 in the "Must Report Error" group

FUNCTION Start( ) AS VOID
	LOCAL p AS _winDRAWITEMSTRUCT
	p := MemAlloc(SizeOf(_winDRAWITEMSTRUCT))
	? p
RETURN

VOSTRUCT _winDRAWITEMSTRUCT
	MEMBER  CtlType AS DWORD
	MEMBER  CtlID AS DWORD

