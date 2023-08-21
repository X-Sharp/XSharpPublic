// 153. error XS0121: The call is ambiguous between the following methods or properties: 'VulcanRTFuncs.Functions.Left(string, uint)' and 'Xs$Globals.Left(string, uint)'

// /dialect:vulcan
FUNCTION Left(c AS STRING, n AS DWORD) AS STRING
RETURN "local function correctly called"

CLASS Test
	METHOD Abc() AS VOID
	? Left("Asd",1) // error here
END CLASS

FUNCTION Start() AS VOID
	? Left("Asd",1) // ok here

