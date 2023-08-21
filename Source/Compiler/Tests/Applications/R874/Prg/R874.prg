
FUNCTION Start( ) AS VOID
	local uTest as USUAL
	uTest := 10
	? Test(uTest)
RETURN



Function Test(cValue as string) AS  DWORD
    return 42
Function Test(dValue as DWORD) AS  DWORD
    return dValue
