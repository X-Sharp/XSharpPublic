// GuiWin32.prg
// Static class with some Win32 API functions that are used inside the Unicode GUI Classes

USING System.Runtime.InteropServices
USING System.Text



INTERNAL STATIC CLASS GuiWin32
#region Kernel32

[DllImport("kernel32.dll")];
    STATIC METHOD GetVersion() AS DWORD PASCAL

    [DllImport("kernel32.dll")];
    STATIC METHOD MulDiv(nNumber AS INT, nNumerator AS INT, nDenominator AS INT)AS INT

    [DllImport("kernel32.dll", CharSet:=CharSet.Unicode, EntryPoint := "WinExecW", SetLastError := TRUE)];
    STATIC METHOD WinExec(lpCmdLine AS STRING, uCmdShow AS DWORD) AS DWORD PASCAL

    [DllImport("kernel32.dll", CharSet:=CharSet.Unicode, EntryPoint := "FindResourceW", SetLastError := TRUE)];
    STATIC METHOD FindResource(hModule AS IntPtr, lpName AS STRING, lpType AS INT) AS IntPtr

    [DllImport("kernel32.dll", EntryPoint := "FindResourceW", SetLastError := TRUE)];
    STATIC METHOD FindResource(hModule AS IntPtr, lpName AS IntPtr, lpType AS INT) AS IntPtr

    [DllImport("Kernel32.dll", EntryPoint := "LockResource")];
    STATIC METHOD LockResource(hGlobal AS IntPtr) AS IntPtr

    [DllImport("Kernel32.dll", EntryPoint := "LoadResource", SetLastError := TRUE)];
    STATIC METHOD LoadResource(hModule AS IntPtr, hResInfo AS IntPtr) AS IntPtr

    [DllImport("Kernel32.dll", EntryPoint := "SizeofResource", SetLastError := TRUE)];
    STATIC METHOD SizeOfResource(hModule AS IntPtr, hResource AS IntPtr) AS DWORD

    [DllImport("Kernel32.dll", EntryPoint := "FreeResource", SetLastError := TRUE)];
    STATIC METHOD FreeResource(hResource AS IntPtr) AS LOGIC

    [DllImport("kernel32.dll")];
    STATIC METHOD FreeLibrary(hLibModule AS IntPtr) AS LOGIC

    [DllImport("kernel32.dll")];
    STATIC METHOD LoadLibrary(lpLibFileName AS STRING) AS IntPtr

    [DllImport("kernel32.dll")];
    STATIC METHOD GetProcAddress(hDLL AS IntPtr, lpProcName AS STRING) AS IntPtr

    [DllImport("kernel32.dll")];
    STATIC METHOD GetLastError() AS LONG STRICT


#endregion

#region User32

    [DllImport("User32.dll")];
    STATIC METHOD BringWindowToTop(hwnd AS IntPtr) AS LOGIC

    [DllImport("User32.dll")];
    STATIC METHOD SetCursor( hCursor AS IntPtr ) AS IntPtr

    [DllImport("User32.dll")];
    STATIC METHOD IsWindowEnabled(hwnd AS IntPtr) AS LOGIC

    [DllImport("User32.dll")];
    STATIC INTERNAL METHOD PeekMessage(lpMsg AS _winMSG, hwnd AS IntPtr, wMsgFilterMin AS DWORD, wMsgFilterMax AS DWORD, wRemoveMsg AS DWORD) AS LOGIC

    [DllImport("User32.dll")];
    STATIC INTERNAL METHOD TranslateMessage( lpMsg AS _winMSG) AS LOGIC

    [DllImport("User32.dll")];
    STATIC INTERNAL METHOD DispatchMessage(lpMsg AS _winMSG ) AS LONG

    [DllImport("User32.dll")];
    STATIC METHOD GetFocus() AS IntPtr STRICT

    [DllImport("User32.dll",CharSet:=CharSet.Unicode, EntryPoint := "SetPropW", SetLastError := TRUE)];
    STATIC METHOD SetProp(hwnd AS IntPtr, lpString AS STRING, hData AS IntPtr) AS LOGIC PASCAL

    [DllImport("User32.dll",CharSet:=CharSet.Unicode, EntryPoint := "GetPropW", SetLastError := TRUE)];
    STATIC METHOD GetProp(hwnd AS IntPtr, lpString AS STRING) AS IntPtr

    [DllImport("User32.dll",CharSet:=CharSet.Unicode, EntryPoint := "RemovePropW", SetLastError := TRUE)];
    STATIC METHOD RemoveProp( hwnd AS IntPtr, lpString AS STRING) AS IntPtr

    [DllImport("User32.dll")];
    STATIC METHOD AnimateWindow(HWND AS IntPtr, dwTime AS DWORD, dwFlags AS DWORD) AS LOGIC

    [DllImport("User32.dll")];
    STATIC METHOD GetSystemMetrics( nIndex AS INT) AS INT

    [DllImport("User32.dll")];
    STATIC METHOD GetActiveWindow() AS IntPtr STRICT

    [DllImport("User32.dll")];
    STATIC INTERNAL METHOD MapWindowPoints(hwndFrom AS IntPtr, hWndTo AS IntPtr, lpPoint AS _winPOINT, cPoints AS DWORD) AS INT

    [DllImport("User32.dll", CharSet:=CharSet.Ansi, EntryPoint := "WinHelpA", SetLastError := TRUE)];
    STATIC METHOD WinHelp(hWndMain AS IntPtr, lpszHelp AS STRING, uCommand AS DWORD, dwDate AS DWORD) AS LOGIC STRICT

    [DllImport("HHCTRL.OCX", CharSet:=CharSet.Ansi, EntryPoint := "HtmlHelpA", SetLastError := TRUE)];
    STATIC METHOD HtmlHelp(hwndCaller AS IntPtr, pszFile AS STRING, uCommand AS DWORD, dwData AS DWORD) AS LONGINT STRICT

    [DllImport("User32.dll")];
    STATIC INTERNAL METHOD GetWindowRect(hWnd AS IntPtr, oRect REF WINRECT) AS LOGIC STRICT

    [DllImport("User32.dll")];
    STATIC INTERNAL METHOD GetClientRect(hWnd AS IntPtr, oRect REF WINRECT) AS LOGIC STRICT

    [DllImport("User32.dll", CharSet:=CharSet.Ansi, EntryPoint := "CreateWindowExA", SetLastError := TRUE)];
    STATIC METHOD CreateWindowEx(dwExStyle AS DWORD, lpClasssName AS STRING, lpWindowName AS STRING,;
            dwStyle AS DWORD, x AS INT, y AS INT, nWidth AS INT,;
            nHeight AS INT, hwndParent AS IntPtr, hMenu AS IntPtr,;
            hInstance AS IntPtr, lpParam AS PTR) AS IntPtr

    [DllImport("User32.dll", CharSet:=CharSet.Ansi, EntryPoint := "DefWindowProcA", SetLastError := TRUE)];
    STATIC METHOD DefWindowProc(hWnd AS IntPtr, Msg AS DWORD, wParam AS DWORD,lParam AS LONG ) AS LONG

    [DllImport("User32.dll", SetLastError := TRUE)];
    STATIC INTERNAL METHOD FillRect( hdc AS IntPtr, lprc AS _winRECT, hbr AS IntPtr) AS INT

    [DllImport("User32.dll", CharSet:=CharSet.Ansi, EntryPoint := "GetObjectA", SetLastError := TRUE)];
    STATIC METHOD GetObject(hgdiobj AS IntPtr, cbBuffer AS INT, lpvObject AS IntPtr) AS INT

    [DllImport("User32.dll", CharSet:=CharSet.Ansi, EntryPoint := "CallWindowProcA", SetLastError := TRUE)];
    STATIC METHOD CallWindowProc(lpPrevWndFunc AS PTR, hwnd AS IntPtr, Msg AS DWORD,wParam AS DWORD, lParam AS LONG)AS LONG

    [DllImport("User32.dll")];
    STATIC METHOD IsWindow(hWnd AS IntPtr) AS LOGIC STRICT

    [DllImport("User32.dll")];
    STATIC INTERNAL METHOD ScreenToClient(hWnd AS IntPtr, oPoint REF WINPOINT) AS LOGIC STRICT

    [DllImport("User32.dll")];
    STATIC INTERNAL METHOD ClientToScreen(hWnd AS IntPtr, oPoint REF WINPOINT) AS LOGIC STRICT

    [DllImport("User32.dll")];
    STATIC METHOD LoadAccelerators( hInstance AS IntPtr, lpTableName AS PSZ) AS IntPtr

    [DllImport("User32.dll")];
    STATIC INTERNAL METHOD CopyAcceleratorTable( hAccelSrc AS IntPtr, lpAccelDst AS _winAccel,	cAccelEntries AS INT) AS INT

    [DllImport("User32.dll")];
    STATIC METHOD DestroyAcceleratorTable( hAccel AS IntPtr) AS LOGIC

    [DllImport("User32.dll")];
    STATIC INTERNAL METHOD CreateAcceleratorTable(lpaccl AS _winAccel, cEntries AS INT) AS IntPtr PASCAL

    [DllImport("User32.dll")];
    STATIC METHOD GetDlgItem(hWnd AS IntPtr, iItem AS LONG) AS IntPtr STRICT

    [DllImport("User32.dll")];
    STATIC METHOD DestroyWindow(hWnd AS IntPtr) AS LOGIC STRICT

        // Devexpress Controls have paint problems, when old win32 styles are modified
    [DllImport("User32.dll")];
    STATIC METHOD SetWindowLong(hwnd AS IntPtr, nIndex AS INT, dwNewLong AS LONG) AS LONG

    [DllImport("User32.dll", CharSet:=CharSet.Unicode, EntryPoint := "SetWindowTextW", SetLastError := TRUE)];
    STATIC METHOD SetWindowText(hwnd AS IntPtr, sText as STRING) AS LOGIC

    [DllImport("User32.dll")];
    STATIC METHOD GetWindowLong(hwnd AS IntPtr, nIndex AS INT) AS LONG

    STATIC METHOD GetWindowStyle(hwnd AS IntPtr) AS LONG => GetWindowLong(hwnd, GWL_STYLE)

    [DllImport("User32.dll")];
    STATIC METHOD GetWindow(hwnd AS IntPtr, nIndex AS INT) AS IntPtr

    [DllImport("User32.dll")];
    STATIC METHOD GetDC(hwnd AS IntPtr) AS IntPtr

    [DllImport("User32.dll")];
    STATIC METHOD ReleaseDC(hwnd AS IntPtr, hDc AS IntPtr) AS INT

    [DllImport("User32.dll")];
    STATIC METHOD GetSysColor( nIndex AS INT) AS DWORD


    [DllImport("User32.dll")];
    STATIC METHOD UpdateWindow( hwnd AS IntPtr) AS LOGIC

    [DllImport("User32.dll", CharSet:=CharSet.Unicode, EntryPoint := "CreateDialogParamW", SetLastError := TRUE)];
    STATIC METHOD CreateDialogParam(hModule AS IntPtr, lpTemplateName AS STRING, hwndParent AS IntPtr, lpDialogFunc AS IntPtr, dwInitParam AS IntPtr) AS IntPtr


    [DllImport("User32.dll")];
    STATIC METHOD ShowWindow(hwnd AS IntPtr, nCmdShow AS INT) AS LOGIC


    [DllImport("User32.dll")];
    STATIC METHOD GetDialogBaseUnits() as DWORD STRICT


    [DllImport("User32.dll")];
    STATIC METHOD MessageBox(hwnd AS IntPtr, lpText AS STRING, lpCaption AS STRING, uType AS DWORD) AS INT

    [DllImport("User32.dll")];
    STATIC METHOD MessageBeep(uType AS DWORD) AS LOGIC


    [DllImport("User32.dll", EntryPoint := "LoadImageA")];
    STATIC METHOD LoadImage(hinst AS IntPtr, lpszName AS PSZ, uType AS DWORD, cxDesired AS INT, cydesired AS INT, fuLoad AS DWORD) AS IntPtr PASCAL


    [DllImport("User32.dll")];
    STATIC METHOD GetParent( hwnd AS IntPtr) AS IntPtr
    [DllImport("User32.dll")];
    STATIC METHOD GetClassName(hwnd AS IntPtr, lpClassName AS StringBuilder, nMaxCount AS INT) AS INT
    [DllImport("User32.dll")];
    STATIC METHOD LoadString(hInstance AS IntPtr, uId AS LONG, lpBuffer AS StringBuilder, nBufferMax AS INT) AS INT
    [DllImport("User32.dll", CharSet:=CharSet.Unicode,  EntryPoint := "SendMessageW")];
    STATIC METHOD SendMessage(hwnd AS IntPtr, Msg AS DWORD, wParam AS DWORD, lParam AS STRING) AS LONG

    [DllImport("User32.dll")];
    STATIC METHOD SendMessage(hwnd AS IntPtr, Msg AS INT, wParam AS IntPtr, lParam AS IntPtr) AS IntPtr

    [DllImport("User32.dll")];
    STATIC METHOD PostMessage(hwnd AS IntPtr, Msg AS DWORD, wParam AS DWORD, lParam AS LONG) AS LOGIC

    [DllImport("User32.dll")];
    STATIC METHOD LoadCursor(hInstance AS IntPtr, lpCursorName AS STRING) AS IntPtr

    [DllImport("User32.dll")];
    STATIC METHOD LoadCursor(hInstance AS IntPtr, lpCursorName AS LONG) AS IntPtr
    [DllImport("User32.dll")];
    STATIC METHOD SetTimer(hwnd AS IntPtr, nIDEvent AS DWORD, uElapse AS DWORD, lpTimerFunc AS PTR) AS DWORD

        //	[DllImport("User32.dll", SetLastError := TRUE)];
        //	STATIC INTERNAL METHOD RegisterClass(lpWndClass AS _winWNDCLASS) AS WORD


    [DllImport("User32.dll")];
    STATIC METHOD GetAsyncKeyState( vKey AS INT) AS SHORT


    [DllImport("User32.dll")];
    STATIC METHOD SetWindowPos( hwnd AS IntPtr, hwndInsertAfter AS IntPtr, x AS INT, y AS INT,cx AS INT, cy AS INT, uFlags AS DWORD) AS LOGIC


    [DllImport("User32.dll")];
    STATIC METHOD SetClassLong(hwnd AS IntPtr, nIndex AS INT, dwNewLong AS LONG) AS DWORD

    [DllImport("User32.dll")];
    STATIC METHOD GetClassLong(hwnd AS IntPtr, nIndex AS INT) AS LONG

    [DllImport("User32.dll")];
    STATIC METHOD GetKeyboardState( lpKeyState AS BYTE[]) AS LOGIC

    [DllImport("User32.dll")];
    STATIC METHOD SetKeyboardState( lpKeyState AS BYTE[]) AS LOGIC


    [DllImport("User32.dll", SetLastError := TRUE)];
    STATIC METHOD GetSystemMenu(hWnd AS IntPtr , bRevert AS LOGIC) AS IntPtr

    [DllImport("User32.dll", SetLastError := TRUE)];
    STATIC METHOD EnableMenuItem(hWnd AS IntPtr , uIDEnableItem AS DWORD, uEnable AS DWORD) AS LONG

    [Return: MarshalAs(UnmanagedType.Bool)];
    [DllImport("User32.dll", SetLastError := TRUE)];
    STATIC INTERNAL METHOD GetWindowPlacement(hWnd AS IntPtr, lpwndpl REF WindowPlacement) AS LOGIC STRICT

    [Return: MarshalAs(UnmanagedType.Bool)];
    [DllImport("User32.dll", SetLastError := TRUE)];
    STATIC INTERNAL METHOD SetWindowPlacement(hWnd AS IntPtr, lpwndpl REF WindowPlacement) AS LOGIC STRICT

    [DllImport("User32.dll")];
    STATIC METHOD WindowFromPoint(pt AS System.Drawing.Point) AS IntPtr

#endregion


#region GDI


    [DllImport("GDI32.dll")];
    STATIC METHOD GetDeviceCaps( hdc AS IntPtr, nIndex AS INT) AS INT

    [DllImport("GDI32.dll")];
    STATIC METHOD GetStockObject(fnObject AS INT ) AS IntPtr

    [DllImport("GDI32.dll")];
    STATIC METHOD DeleteDC(hdc AS IntPtr) AS LOGIC

    [DllImport("GDI32.dll")];
    STATIC METHOD CreateIC(lpszDriver AS STRING, lpszDevice AS STRING, lpszOutput AS STRING, lpdvmInit AS IntPtr ) AS IntPtr


    [DllImport("GDI32.dll")];
    STATIC METHOD SetTextColor(hdc AS IntPtr, crColor AS DWORD) AS DWORD

    [DllImport("GDI32.dll")];
    STATIC METHOD SetBkColor(hdc AS IntPtr, crColor AS DWORD) AS DWORD



    [DllImport("GDI32.dll", CharSet:=CharSet.Ansi, EntryPoint := "ExtTextOutA", SetLastError := TRUE)];
    STATIC INTERNAL METHOD ExtTextOut(hdc AS IntPtr, X AS INT, Y AS INT, fOptions AS DWORD,;
            lprc AS _winRECT,  lpString AS STRING, nCount AS DWORD, lpDx AS INT PTR) AS LOGIC


#endregion
END CLASS


//FUNCTION MAKELPARAM(l AS WORD, h AS WORD) AS LONGINT STRICT
//RETURN (MakeLong(l, h))

[StructLayout(LayoutKind.Sequential)];
INTERNAL STRUCTURE WINRECT
    PUBLIC left AS System.Int32
    PUBLIC top AS System.Int32
    PUBLIC right AS System.Int32
    PUBLIC bottom AS System.Int32

END STRUCTURE


[StructLayout(LayoutKind.Sequential)];
INTERNAL STRUCTURE WINPOINT
    PUBLIC x AS System.Int32
    PUBLIC y AS System.Int32

END STRUCTURE

VOSTRUCT _WINBITMAPINFOHEADER
    MEMBER biSize AS DWORD
    MEMBER biWidth AS LONGINT
    MEMBER biHeight AS LONGINT
    MEMBER biPlanes AS WORD
    MEMBER biBitCount AS WORD
    MEMBER biCompression AS DWORD
    MEMBER biSizeImage AS DWORD
    MEMBER biXPelsPerMeter AS LONGINT
    MEMBER biYPelsPerMeter AS LONGINT
    MEMBER biClrUsed AS DWORD
    MEMBER biClrImportant AS DWORD

VOSTRUCT _WINRGBQUAD
    MEMBER rgbBlue AS BYTE
    MEMBER rgbGreen AS BYTE
    MEMBER rgbRed AS BYTE
    MEMBER rgbReserved AS BYTE

VOSTRUCT _WINBITMAPINFO
    MEMBER	bmiHeader IS _WINBITMAPINFOHEADER
MEMBER	DIM	  bmiColors[1] IS _WINRGBQUAD


INTERNAL VOSTRUCT _winAccel
    MEMBER fVirt AS BYTE
    MEMBER key AS WORD
    MEMBER cmd AS WORD


INTERNAL VOSTRUCT _winRECT
    MEMBER left AS LONGINT
    MEMBER top AS LONGINT
    MEMBER right AS LONGINT
    MEMBER bottom AS LONGINT


//INTERNAL VOSTRUCT _winWNDCLASS
//	MEMBER style         AS DWORD
//	MEMBER lpfnWndProc   AS PTR
//	MEMBER cbClsExtra    AS INT
//	MEMBER cbWndExtra    AS INT
//	MEMBER hInstance     AS PTR
//	MEMBER hIcon         AS PTR
//	MEMBER hCursor       AS PTR
//	MEMBER hbrBackground AS PTR
//	MEMBER lpszMenuName  AS PSZ
//	MEMBER lpszClassName AS PSZ
//
//INTERNAL VOSTRUCT _winCREATESTRUCT
//	MEMBER lpCreateParams AS PTR
//	MEMBER hInstance      AS PTR
//	MEMBER hMenu          AS PTR
//	MEMBER hwndParent     AS PTR
//	MEMBER cy             AS INT
//	MEMBER cx             AS INT
//	MEMBER y              AS INT
//	MEMBER x              AS INT
//	MEMBER style          AS LONGINT
//	MEMBER lpszName       AS PSZ
//	MEMBER lpszClass      AS PSZ

INTERNAL VOSTRUCT _winLOGBRUSH
    MEMBER lbStyle AS DWORD
    MEMBER lbColor AS DWORD
    MEMBER lbHatch AS LONGINT


INTERNAL VOSTRUCT _winMULTIKEYHELP  ALIGN 2 // RvdH 070411 added alignment
    MEMBER mkSize AS DWORD
    MEMBER mkKeylist AS BYTE
    MEMBER DIM szKeyphrase[1] AS BYTE


INTERNAL VOSTRUCT _winHH_AKLINK
    MEMBER cbStruct AS INT    // sizeof this structure
    MEMBER fReserved AS LOGIC // must be FALSE (really!)
    MEMBER pszKeywords AS PSZ // semi-colon separated keywords
    MEMBER pszUrl AS PSZ      // URL to jump to if no keywords found (may be NULL)
    MEMBER pszMsgText AS PSZ  // Message text to display in MessageBox if pszUrl is NULL and no keyword match
    MEMBER pszMsgTitle AS PSZ // Message text to display in MessageBox if pszUrl is NULL and no keyword match
    MEMBER pszWindow AS PSZ   // Window to display URL in
    MEMBER fIndexOnFail AS LOGIC // Displays index if keyword lookup fails.


INTERNAL VOSTRUCT _winHELPINFO
    MEMBER cbSize AS DWORD
    MEMBER iContextType AS INT
    MEMBER iCtrlId AS INT
    MEMBER hItemHandle AS PTR
    MEMBER dwContextId AS DWORD
    MEMBER MousePos IS _winPOINT


INTERNAL VOSTRUCT _winMINMAXINFO
    MEMBER ptReserved IS _winPOINT
    MEMBER ptMaxSize IS _winPOINT
    MEMBER ptMaxPosition IS _winPOINT
    MEMBER ptMinTrackSize IS _winPOINT
    MEMBER ptMaxTrackSize IS _winPOINT

INTERNAL VOSTRUCT _winNMLINK ALIGN 1
    MEMBER hdr IS _winNMHDR
    MEMBER item IS _winLITEM



INTERNAL VOSTRUCT _winNMHDR
    MEMBER  hwndFrom AS PTR
    MEMBER  idFrom AS DWORD
    MEMBER  _code AS DWORD


INTERNAL VOSTRUCT _winLITEM //ALIGN 1 RvdH 070411 removed alignment
    MEMBER mask AS DWORD
    MEMBER iLink AS INT
    MEMBER state AS DWORD
    MEMBER stateMask AS DWORD
    MEMBER DIM szID[MAX_LINKID_TEXT] AS WORD
    MEMBER DIM szUrl[L_MAX_URL_LENGTH] AS WORD



INTERNAL VOSTRUCT _winDRAWITEMSTRUCT
    MEMBER CtlType AS DWORD
    MEMBER CtlID AS DWORD
    MEMBER itemID AS DWORD
    MEMBER itemAction AS DWORD
    MEMBER itemState AS DWORD
    MEMBER hwndItem AS PTR
    MEMBER hdc AS PTR
    MEMBER rcItem IS _winRECT
    MEMBER itemData AS DWORD


INTERNAL VOSTRUCT _winMSG
    MEMBER hwnd AS PTR
    MEMBER message AS DWORD
    MEMBER wParam  AS DWORD
    MEMBER lParam AS LONGINT
    MEMBER time AS DWORD
    MEMBER pt IS _winPOINT

[Serializable];
[StructLayout(LayoutKind.Sequential)];
INTERNAL STRUCTURE WindowPlacement
    PUBLIC length AS INT
    PUBLIC flags AS INT
    PUBLIC showCmd AS ShowWindowCommands
    PUBLIC ptMinPosition AS System.Drawing.Point
    PUBLIC ptMaxPosition AS System.Drawing.Point
    PUBLIC rcNormalPosition AS System.Drawing.Rectangle
END STRUCTURE

ENUM ShowWindowCommands
    MEMBER Hide := 0
    MEMBER Normal := 1
    MEMBER Minimized := 2
    MEMBER Maximized := 3
END ENUM

FUNCTION MAKEWPARAM(l AS WORD, h AS WORD) AS DWORD STRICT => (DWORD(_CAST, MAKELONG(l, h)))


