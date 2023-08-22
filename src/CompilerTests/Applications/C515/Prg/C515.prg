// error XS0037: Cannot convert null to 'IntPtr' because it is a non-nullable value type
#pragma warnings (9043,off)
FUNCTION Start() AS VOID
	LOCAL l:= FALSE AS LOGIC
	IF l
		// no error for _non_ _DLL functions
		TestNonDll(NULL)
		// error XS0037
		TestDll(NULL)
	END IF

	? GetSystemMessage(0)
	? GetSystemMessage(1)
	xAssert(GetSystemMessage(0) = "The operation completed successfully.")
	xAssert(GetSystemMessage(1) = "Incorrect function.")
RETURN

_DLL FUNCTION TestDll(p AS PSZ) AS VOID PASCAL:KERNEL32.Nothing
FUNCTION TestNonDll(p AS PSZ) AS VOID
	? p
RETURN

DEFINE LANG_NEUTRAL                   :=  0x00
DEFINE FORMAT_MESSAGE_FROM_SYSTEM := 0x00001000
DEFINE SUBLANG_DEFAULT                :=  0x01    // user default
FUNCTION MAKELANGID(p, s) AS WORD
RETURN ( _OR((WORD(_CAST, S) << 10), WORD(P)))
_DLL FUNCTION FormatMessage(dwFlags AS DWORD, lpSource AS PTR, dwMessageId AS DWORD, dwLanguageId AS DWORD, lpBuffer AS PTR, nSize AS DWORD, Arguments AS PSZ) AS DWORD PASCAL:KERNEL32.FormatMessageA

FUNCTION GetSystemMessage(dwId AS DWORD) AS STRING
	LOCAL pMsg AS PSZ PTR    // dcaton 070405 changed from PTR to PSZ PTR
	LOCAL cReturn AS STRING

	pMsg := MemAlloc(512)
	FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM, NULL, dwId, ;
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),  ; // Default language
		pMsg, 512, NULL )
	cReturn := Psz2String(PSZ(_CAST,PTR(_CAST,pMsg)))
	MemFree(pMsg)

	RETURN cReturn

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

