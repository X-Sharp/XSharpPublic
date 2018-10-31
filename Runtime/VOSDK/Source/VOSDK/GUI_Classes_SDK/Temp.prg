FUNCTION __DBCSEnabled() AS LOGIC
	RETURN LOGIC(_CAST, GetSystemMetrics(SM_DBCSENABLED))

FUNCTION __ShowLastError()
	LOCAL dwErr AS DWORD
	LOCAL lpMsgBuf AS PSZ

	dwErr := GetLastError()

	FormatMessage(_OR(FORMAT_MESSAGE_ALLOCATE_BUFFER, FORMAT_MESSAGE_FROM_SYSTEM), ;
		NULL_PTR, dwErr, 0, @lpMsgBuf, 0, NULL_PTR)

	MessageBox(0, String2Psz( "Error #: "+NTrim(dwErr)+", Text: "+AsString(lpMsgBuf)), String2Psz("GetLastError"), _OR(MB_OK,MB_ICONINFORMATION))

	LocalFree(lpMsgBuf)
	RETURN NIL

_DLL FUNC DragObject(hDesktop AS PTR, hWnd AS PTR, uFlags AS DWORD, dw AS DWORD, hCursor AS PTR);
	AS DWORD PASCAL:USER32.DragObject

// FUNCTION MyMakeLong(wLow AS WORD, wHigh AS WORD) AS LONG
// 	RETURN LONG(_CAST, _or((DWORD(wHigh) << 16), DWORD(wLow)))




FUNCTION IsShiftPressed()
	//PP-030319 added function
	RETURN LOGIC(_CAST, _AND(GetKeyState(VK_SHIFT), SHORTINT(_CAST, 0x8000)))

FUNCTION IsControlPressed()
	//PP-030319 added function
	RETURN LOGIC(_CAST, _AND(GetKeyState(VK_CONTROL), SHORTINT(_CAST, 0x8000)))

FUNCTION IsAltPressed()
	//PP-030319 added function
	RETURN LOGIC(_CAST, _AND(GetKeyState(VK_MENU), SHORTINT(_CAST, 0x8000)))

// STRUCT _WINDEVMODE ALIGN 1
// 	MEMBER	 DIM dmDeviceName[CCHDEVICENAME] AS BYTE
// 	MEMBER	 dmSpecVersion AS WORD
// 	MEMBER	 dmDriverVersion AS WORD
// 	MEMBER	 dmSize AS WORD
// 	MEMBER	 dmDriverExtra AS WORD
// 	MEMBER	 dmFields AS DWORD
// 	MEMBER	 dmOrientation AS SHORT
// 	MEMBER	 dmPaperSize AS SHORT
// 	MEMBER	 dmPaperLength AS SHORT
// 	MEMBER	 dmPaperWidth AS SHORT
// 	MEMBER	 dmScale AS SHORT
// 	MEMBER	 dmCopies AS SHORT
// 	MEMBER	 dmDefaultSource AS SHORT
// 	MEMBER	 dmPrintQuality AS SHORT
// 	MEMBER	 dmColor AS SHORT
// 	MEMBER	 dmDuplex AS SHORT
// 	MEMBER	 dmYResolution AS SHORT
// 	MEMBER	 dmTTOption AS SHORT
// 	MEMBER	 dmCollate AS SHORT
// 	MEMBER	 DIM dmFormName[CCHFORMNAME] AS BYTE
// 	MEMBER	 dmLogPixels AS WORD
// 	MEMBER	 dmBitsPerPel AS DWORD
// 	MEMBER	 dmPelsWidth AS DWORD
// 	MEMBER	 dmPelsHeight AS DWORD
// 	MEMBER	 dmDisplayFlags AS DWORD
// 	MEMBER	 dmDisplayFrequency AS DWORD
// 	MEMBER	 dmICMMethod AS DWORD
// 	MEMBER	 dmICMIntent AS DWORD
// 	MEMBER	 dmMediaType AS DWORD
// 	MEMBER	 dmDitherType AS DWORD
// 	MEMBER	 dmReserved1 AS DWORD
// 	MEMBER	 dmReserved2 AS DWORD


