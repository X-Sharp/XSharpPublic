// error XS1503: Argument 1: cannot convert from '_WINFILETIME**' to '_WINFILETIME*'
#pragma warnings(165, off) // uniassigned local
VOSTRUCT _WINFILETIME
	MEMBER dwLowDateTime AS  DWORD
	MEMBER dwHighDateTime AS DWORD

FUNCTION Start() AS VOID
LOCAL p AS _WINFILETIME
LOCAL q IS _WINFILETIME
//TestFunc(@p)	// The original C326 had this line of code but that does not compile in Vulcan as well
TestFunc(p)
TestFunc(@q)

FUNCTION TestFunc(p AS _WINFILETIME) AS VOID
? p
