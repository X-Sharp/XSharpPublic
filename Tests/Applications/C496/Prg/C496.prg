// 496. error XS0199: A static readonly field cannot be passed ref or out (except in a static constructor)
// Logged this because I found dozens of such occurences in VO library TE_DevLibV15
// Not sure if we should really "fix" it in x#, but it is a lot of work getting rid 
// of all those error messages so at least let's be aware of this.

_DLL FUNCTION GetTerBuffer1(HWND AS PTR,  SIZE REF LONG) AS PTR STRICT:TER15.GetTerBuffer#30
_DLL FUNCTION GetTerBuffer2(HWND AS PTR,  SIZE REF DWORD) AS PTR STRICT:TER15.GetTerBuffer#30

FUNCTION Start() AS VOID
	// dozens of function calls like that:
	LOCAL hwnd AS PTR
	GetTerBuffer1(hwnd , NULL_PTR)
	GetTerBuffer2(hwnd , NULL_PTR)

	// this one is my own test, it also does compile in VO (not sure about the runtime behavior..)
	LOCAL p AS PTR
	GetTerBuffer1(hwnd , p)
	GetTerBuffer1(hwnd , @p)
RETURN

// Made the following adjustemnts so that the code compiles in x#
// (changed the definition from REF LONG to REF PTR and defined a PTR local to be passed to the function)
/*
_DLL FUNCTION GetTerBuffer1(HWND AS PTR,  SIZE REF PTR) AS PTR STRICT:TER15.GetTerBuffer#30
_DLL FUNCTION GetTerBuffer2(HWND AS PTR,  SIZE REF PTR) AS PTR STRICT:TER15.GetTerBuffer#30

FUNCTION Start() AS VOID
	LOCAL hwnd AS PTR
	LOCAL pNULL_PTR
	GetTerBuffer1(hwnd , pNULL_PTR)
	GetTerBuffer2(hwnd , pNULL_PTR)
RETURN
*/

