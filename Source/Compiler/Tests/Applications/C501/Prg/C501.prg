// 501. error XS0121: The call is ambiguous between the following methods or properties: 'VulcanVOInternetClasses.Functions.WSAGetLastError()' and 'VulcanVOWin32APILibrary.Functions.WSAGetLastError()'
// This one is new, build of 2017/05/04
// in previous builds, this was only a warning
#pragma warnings(9021, off) // ambiguous

FUNCTION Start( ) AS VOID
	? WSAGetLastError()
RETURN

