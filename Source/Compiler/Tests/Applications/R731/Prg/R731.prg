// R731. An error should be reported when compiling without /vo7+ that there is no conversion from PTR to the VOSTRUCT
// See also C425 which compiles without errors.
FUNCTION Start( ) AS VOID
	LOCAL p AS _winDRAWITEMSTRUCT
	p := MemAlloc(SizeOf(_winDRAWITEMSTRUCT))
	? p
RETURN

VOSTRUCT _winDRAWITEMSTRUCT
	MEMBER  CtlType AS DWORD
	MEMBER  CtlID AS DWORD

