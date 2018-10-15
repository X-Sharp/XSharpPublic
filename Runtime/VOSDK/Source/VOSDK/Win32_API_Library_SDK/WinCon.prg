VOSTRUCT _winCHAR_INFO ALIGN 2   
	MEMBER char IS Char_win
	MEMBER Attributes AS WORD  



VOSTRUCT _winCOORD
	MEMBER X AS SHORTINT
	MEMBER Y AS SHORTINT

VOSTRUCT _winSMALL_RECT
	MEMBER Left AS SHORTINT
	MEMBER Top AS SHORTINT
	MEMBER Right AS SHORTINT
	MEMBER Bottom AS SHORTINT


VOSTRUCT _winKEY_EVENT_RECORD
	MEMBER bKeyDown AS LOGIC
	MEMBER wRepeatCount AS WORD
	MEMBER wVirtualKeyCode AS WORD
	MEMBER wVirtualScanCode AS WORD

	MEMBER uChar IS uChar_win
	MEMBER dwControlKeyState AS DWORD


VOSTRUCT _winMOUSE_EVENT_RECORD
	MEMBER dwMousePosition IS _winCOORD
	MEMBER dwButtonState AS DWORD
	MEMBER dwControlKeyState AS DWORD
	MEMBER dwEventFlags AS DWORD


VOSTRUCT _winWINDOW_BUFFER_SIZE_RECORD
	MEMBER dwSize IS _winCOORD

VOSTRUCT _winMENU_EVENT_RECORD
	MEMBER dwCommandId AS DWORD


VOSTRUCT _winFOCUS_EVENT_RECORD
	MEMBER  bSetFocus AS LOGIC

VOSTRUCT _winINPUT_RECORD
	MEMBER EventType AS WORD

	MEMBER @@Event IS Event_win



VOSTRUCT _winCONSOLE_SCREEN_BUFFER_INFO
	MEMBER  dwSize IS _winCOORD
	MEMBER  dwCursorPosition IS _winCOORD
	MEMBER   wAttributes AS WORD
	MEMBER  srWindow IS _winSMALL_RECT
	MEMBER  dwMaximumWindowSize IS _winCOORD

VOSTRUCT _winCONSOLE_CURSOR_INFO
	MEMBER  dwSize AS DWORD
	MEMBER  bVisible AS LOGIC



//
// Selection flags
//

UNION uCHAR_win
	MEMBER UnicodecHAR AS BYTE
	MEMBER AsciiChar AS BYTE

UNION Event_win
	MEMBER  KeyEvent IS _winKEY_EVENT_RECORD
	MEMBER  MouseEvent IS _winMOUSE_EVENT_RECORD
	MEMBER  WindowBufferSizeEvent IS _winWINDOW_BUFFER_SIZE_RECORD
	MEMBER  MenuEvent IS _winMENU_EVENT_RECORD
	MEMBER  FocusEvent IS _winFOCUS_EVENT_RECORD

UNION Char_win      //
	MEMBER UnicodeChar AS WORD     	// RvdH 070411 changed from BYTE to WORD
	MEMBER aSCIIcHAR AS BYTE

_DLL FUNC PeekConsoleInput (hConsoleInput AS PTR, lpBuffer AS _winINPUT_RECORD,;
	nLenght AS DWORD, lpNumberOfEventsRead AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.PeekConsoleInputA


_DLL FUNC ReadConsoleInput( hConsoleInput AS PTR, lpBuffer AS _winINPUT_RECORD,;
	nLenght AS DWORD, lpNumberOfEventsRead AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.ReadConsoleInputA




_DLL FUNC WriteConsoleInput(hConsoleInput AS PTR, lpBuffer AS _winINPUT_RECORD,;
	nLength AS DWORD, lpNumberOfEventsWritten AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.WriteConsoleInputA




_DLL FUNC ReadConsoleOutput(hConsoleOutput AS PTR, lpBuffer AS _winCHAR_INFO,;
	dwBufferSize AS DWORD, dwBufferCoord AS DWORD,;
	lpReadRegion AS _winSMALL_RECT);
	AS LOGIC PASCAL:KERNEL32.ReadConsoleOutputA





_DLL FUNC WriteConsoleOutput( hConsolePutput AS PTR, lpBUffer AS _winCHAR_INFO,;
	dwBufferSize AS DWORD, dwBufferCoord AS DWORD,;
	lpwriteRegion AS _winSMALL_RECT);
	AS LOGIC PASCAL:KERNEL32.WriteConsoleOutputA



_DLL FUNC ReadConsoleOutputCharacter(hConsoleOutput AS PTR, lpCharacter AS PSZ,;
	nLenght AS DWORD, dwReadCoord AS DWORD,;
	lpNumberOfCharsRead AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.ReadConsoleOutputCharacterA



_DLL FUNC ReadConsoleOutputAttribute( hConsoleOutput AS PTR, lpAttribute AS WORD PTR,;
	nLenght AS DWORD, dwReadCoord AS DWORD,;
	lpNumberOfAttrisRead AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.ReadConsoleOutputAttribute


_DLL FUNC WriteConsoleOutputCharacter(HcONSOLEoUTPUT AS PTR, lpCharacter AS PSZ,;
	nLenght AS DWORD, dwWriteCoord AS DWORD,;
	lpNumberOfCharsWritten AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.WriteConsoleOutputCharacterA




_DLL FUNC WriteConsoleOutputAttribute(hConsoleOutput AS PTR, lpAttribute AS WORD PTR,;
	nLenght AS DWORD, dwWriteCoord AS DWORD,;
	lpNumberOfAttrsWritten AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.WriteConsoleOutputAttribute

_DLL FUNC FillConsoleOutputCharacter(hConsoleOutput AS PTR, cCharacter AS BYTE,;
	nLenght AS DWORD, dwWriteCoord AS DWORD,;
	lpNumberOfCharWritten AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.FillConsoleOutputCharacterA



_DLL FUNC FillConsoleOutputAttribute(hConsoleOutput AS PTR, wAttribute AS WORD,;
	nLenght AS DWORD, dwWriteCoord AS DWORD,;
	lpNumberOfAttrsWritten AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.FillConsoleOutputAttribute


_DLL FUNC GetConsoleMode(hConsoleHandle AS PTR, lpMode AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetConsoleMode


_DLL FUNC GetNumberOfConsoleInputEvents(hConsoleInput AS PTR, lpNumberOfEvents AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetNumberOfConsoleInputEvents

_DLL FUNC GetConsoleScreenBufferInfo( hConsoleOutput AS PTR,;
	lpCondoleScreenBufferInfo AS _winCONSOLE_SCREEN_BUFFER_INFO);
	AS LOGIC PASCAL:KERNEL32.GetConsoleScreenBufferInfo


_DLL FUNC GetLargestConsoleWindowSize(hConsoleOutput AS PTR);
	AS DWORD PASCAL:KERNEL32.GetLargestConsoleWindowSize


_DLL FUNC GetConsoleCursorInfo( hConsoleOutput AS PTR, lpConsoleCurrsorInfo AS _winCONSOLE_CURSOR_INFO);
	AS LOGIC PASCAL:KERNEL32.GetConsoleCursorInfo


_DLL FUNC GetNumberOfConsoleMouseButtons( lpNumberOfMouseButtons AS DWORD PTR);
	AS LOGIC PASCAL:KERNEL32.GetNumberOfConsoleMouseButtons


_DLL FUNC SetConsoleMode(hConsoleHandle AS PTR, dwMode AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetConsoleMode


_DLL FUNC SetConsoleActiveScreenBuffer(hConsoleOutput AS PTR);
	AS LOGIC PASCAL:KERNEL32.SetConsoleActiveScreenBuffer


_DLL FUNC FlushConsoleInputBuffer(hConsoleInput AS PTR);
	AS LOGIC PASCAL:KERNEL32.FlushConsoleInputBuffer


_DLL FUNC SetConsoleScreenBufferSize( hConsoleOutput AS PTR, dwSize AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetConsoleScreenBufferSize


_DLL FUNC SetConsoleCursorPosition(hConsoleOutput AS PTR, dwCursouPosition AS DWORD);
	AS LOGIC PASCAL:KERNEL32.SetConsoleCursorPosition


_DLL FUNC SetConsoleCursorInfo(hConsoleOutput AS PTR,;
	lpConsoleCursorInfo AS _winCONSOLE_CURSOR_INFO);
	AS LOGIC PASCAL:KERNEL32.SetConsoleCursorInfo



_DLL FUNC ScrollConsoleScreenBuffer( hConsoleOutput AS PTR, lpSrcollRectAngle AS _winSMALL_RECT,;
	lpClipRectangle AS _winSMALL_RECT,;
	dwDestinationationOrigin AS DWORD,;
	lpFill AS _winCHAR_INFO);
	AS LOGIC PASCAL:KERNEL32.ScrollConsoleScreenBufferA



_DLL FUNC SetConsoleWindowInfo( hConsoleOutput AS PTR, bAbsolute AS LOGIC,;
	lpConsoleWindow AS _winSMALL_RECT);
	AS LOGIC PASCAL:KERNEL32.SetConsoleWindowInfo


_DLL FUNC SetConsoleTextAttribute( hConsoleOutput AS PTR, wAttributes AS WORD);
	AS LOGIC PASCAL:KERNEL32.SetConsoleTextAttribute


_DLL FUNC SetConsoleCtrlHandler( HandleRoutine  AS PTR, Add AS LOGIC );
	AS LOGIC PASCAL:KERNEL32.SetConsoleCtrlHandler


_DLL FUNC GenerateConsoleCtrlEvent(dwCtrlEvent AS DWORD, dwPecessgroupId AS DWORD);
	AS LOGIC PASCAL:KERNEL32.GenerateConsoleCtrlEvent


_DLL FUNC AllocConsole() AS LOGIC PASCAL:KERNEL32.AllocConsole


_DLL FUNC FreeConsole() AS LOGIC PASCAL:KERNEL32.FreeConsole


_DLL FUNC GetConsoleTitle( lpComsoleTitle AS PSZ, nSize AS DWORD);
	AS DWORD PASCAL:KERNEL32.GetConsoleTitleA



_DLL FUNC SetConsoleTitle( lpComsoleTile AS PSZ) AS LOGIC PASCAL:KERNEL32.SetConsoleTitleA





_DLL FUNC ReadConsole(hConsoleInput AS PTR, lpBUffer AS PTR,;
	nNumberOfCharsToRead AS DWORD, lpNumberOfCharsRead AS DWORD PTR, lpReserved AS PTR);
	AS LOGIC PASCAL:KERNEL32.ReadConsoleA





_DLL FUNC WriteConsole( hConsoleOutput AS PTR, lpBuffer AS PTR,;
	nNumberOfCharsToWrite AS DWORD, lpNumberOfCharsWritten AS DWORD PTR,;
	lpReserved AS PTR) AS LOGIC PASCAL:KERNEL32.WriteConsoleA


_DLL FUNC CreateConsoleScreenBuffer(dwDesiredAccess AS DWORD, dwShareMode AS DWORD,;
	lpSecurityAttributes AS _winSECURITY_ATTRIBUTES,;
	dwFlags AS DWORD, lpScreenBufferData AS PTR);
	AS PTR PASCAL:KERNEL32.CreateConsoleScreenBuffer

_DLL FUNC GetConsoleCP() AS DWORD PASCAL:KERNEL32.GetConsoleCP


_DLL FUNC SetConsoleCP( wCodePageId AS DWORD) AS LOGIC PASCAL:kernel32.SetConsoleCP


_DLL FUNC GetConsoleOutputCP() AS DWORD PASCAL:KERNEL32.GetConsoleOutputCP


_DLL FUNC SetConsoleOutputCP( wCodePageId AS DWORD) AS LOGIC PASCAL:KERNEL32.SetConsoleOutputCP
		
		
#region defines
DEFINE RIGHT_ALT_PRESSED     := 0x0001
DEFINE LEFT_ALT_PRESSED      := 0x0002
DEFINE RIGHT_CTRL_PRESSED    := 0x0004
DEFINE LEFT_CTRL_PRESSED     := 0x0008
DEFINE SHIFT_PRESSED         := 0x0010
DEFINE NUMLOCK_ON            := 0x0020
DEFINE SCROLLLOCK_ON         := 0x0040
DEFINE CAPSLOCK_ON           := 0x0080
DEFINE ENHANCED_KEY          := 0x0100
DEFINE NLS_DBCSCHAR          := 0x00010000 // DBCS for JPN: SBCS/DBCS mode.
DEFINE NLS_ALPHANUMERIC      := 0x00000000 // DBCS for JPN: Alphanumeric mode.
DEFINE NLS_KATAKANA          := 0x00020000 // DBCS for JPN: Katakana mode.
DEFINE NLS_HIRAGANA          := 0x00040000 // DBCS for JPN: Hiragana mode.
DEFINE NLS_ROMAN             := 0x00400000 // DBCS for JPN: Roman/Noroman mode.
DEFINE NLS_IME_CONVERSION    := 0x00800000 // DBCS for JPN: IME conversion.
DEFINE NLS_IME_DISABLE       := 0x20000000 // DBCS for JPN: IME enable/disable.
DEFINE FROM_LEFT_1ST_BUTTON_PRESSED    := 0x0001
DEFINE RIGHTMOST_BUTTON_PRESSED        := 0x0002
DEFINE FROM_LEFT_2ND_BUTTON_PRESSED    := 0x0004
DEFINE FROM_LEFT_3RD_BUTTON_PRESSED    := 0x0008
DEFINE FROM_LEFT_4TH_BUTTON_PRESSED    := 0x0010
DEFINE MOUSE_MOVED   := 0x0001
DEFINE DOUBLE_CLICK  := 0x0002
DEFINE MOUSE_WHEELED := 0x0004
DEFINE KEY_EVENT                := 0x0001
DEFINE _MOUSE_EVENT             := 0x0002
DEFINE WINDOW_BUFFER_SIZE_EVENT := 0x0004
DEFINE MENU_EVENT               := 0x0008
DEFINE FOCUS_EVENT              := 0x0010
DEFINE FOREGROUND_BLACK     := 0x0000
DEFINE FOREGROUND_BLUE      := 0x0001
DEFINE FOREGROUND_GREEN     := 0x0002
DEFINE FOREGROUND_RED       := 0x0004
DEFINE FOREGROUND_WHITE     := 0x0007
DEFINE FOREGROUND_INTENSITY := 0x0008
DEFINE BACKGROUND_BLACK     := 0x0000
DEFINE BACKGROUND_BLUE      := 0x0010
DEFINE BACKGROUND_GREEN     := 0x0020
DEFINE BACKGROUND_RED       := 0x0040
DEFINE BACKGROUND_WHITE     := 0x0070
DEFINE BACKGROUND_INTENSITY := 0x0080
DEFINE COMMON_LVB_LEADING_BYTE    := 0x0100 // Leading Byte of DBCS
DEFINE COMMON_LVB_TRAILING_BYTE   := 0x0200 // Trailing Byte of DBCS
DEFINE COMMON_LVB_GRID_HORIZONTAL := 0x0400 // DBCS: Grid attribute: top horizontal.
DEFINE COMMON_LVB_GRID_LVERTICAL  := 0x0800 // DBCS: Grid attribute: left vertical.
DEFINE COMMON_LVB_GRID_RVERTICAL  := 0x1000 // DBCS: Grid attribute: right vertical.
DEFINE COMMON_LVB_REVERSE_VIDEO   := 0x4000 // DBCS: Reverse fore/back ground attribute.
DEFINE COMMON_LVB_UNDERSCORE      := 0x8000 // DBCS: Underscore.
DEFINE COMMON_LVB_SBCSDBCS        := 0x0300 // SBCS or DBCS flag.
DEFINE CONSOLE_NO_SELECTION            := 0x0000
DEFINE CONSOLE_SELECTION_IN_PROGRESS   := 0x0001   // selection has begun
DEFINE CONSOLE_SELECTION_NOT_EMPTY     := 0x0002   // non-null select rectangle
DEFINE CONSOLE_MOUSE_SELECTION         := 0x0004   // selecting with mouse
DEFINE CONSOLE_MOUSE_DOWN              := 0x0008   // mouse is down
DEFINE CTRL_C_EVENT        := 0
DEFINE CTRL_BREAK_EVENT    := 1
DEFINE CTRL_CLOSE_EVENT    := 2
// 3 is reserved!
// 4 is reserved!
DEFINE CTRL_LOGOFF_EVENT   := 5
DEFINE CTRL_SHUTDOWN_EVENT := 6
//
//  Input Mode flags:
//
DEFINE ENABLE_PROCESSED_INPUT := 0x0001
DEFINE ENABLE_LINE_INPUT      := 0x0002
DEFINE ENABLE_ECHO_INPUT      := 0x0004
DEFINE ENABLE_WINDOW_INPUT    := 0x0008
DEFINE ENABLE_MOUSE_INPUT     := 0x0010
//
// Output Mode flags:
//
DEFINE ENABLE_PROCESSED_OUTPUT    := 0x0001
DEFINE ENABLE_WRAP_AT_EOL_OUTPUT  := 0x0002
//
// direct API definitions.
//
DEFINE CONSOLE_TEXTMODE_BUFFER  := 1
#endregion
