VOSTRUCT _winCBT_CREATEWND
	MEMBER  lpcs AS _winCREATESTRUCT
	MEMBER  hwndInsertAfter AS PTR


VOSTRUCT _winCBTACTIVATESTRUCT
	MEMBER      fMouse AS LOGIC
	MEMBER      hWndActive AS PTR


/*
 * WH_MSGFILTER Filter Proc Codes
 */
VOSTRUCT _winEVENTMSG
	MEMBER   message AS DWORD
	MEMBER   paramLB AS DWORD
	MEMBER  paramH AS DWORD
	MEMBER   time AS DWORD
	MEMBER   hwnd AS PTR


VOSTRUCT _winMOUSEHOOKSTRUCT
	MEMBER   pt IS _winPOINT
	MEMBER   hwnd AS PTR
	MEMBER   wHitTestCode AS DWORD
	MEMBER   dwExtraInfo AS DWORD



VOSTRUCT _winUSEROBJECTFLAGS
	MEMBER  fInherit AS LOGIC
	MEMBER  fReserved AS LOGIC
	MEMBER  dwFlags AS DWORD


VOSTRUCT _winWNDCLASSEX
	MEMBER      cbSize AS DWORD

	MEMBER   style AS DWORD
	MEMBER   lpfnWndProc AS PTR
	MEMBER   cbClsExtra AS INT
	MEMBER   cbWndExtra AS INT
	MEMBER   hInstance AS PTR
	MEMBER   hIcon AS PTR
	MEMBER   hCursor AS PTR
	MEMBER   hbrBackground AS PTR
	MEMBER   lpszMenuName AS PSZ
	MEMBER   lpszClassName AS PSZ

	MEMBER   hIconSm AS PTR


VOSTRUCT _winWNDCLASS
	MEMBER   style AS DWORD
	MEMBER   lpfnWndProc AS PTR
	MEMBER   cbClsExtra AS INT
	MEMBER   cbWndExtra AS INT
	MEMBER   hInstance AS PTR
	MEMBER   hIcon AS PTR
	MEMBER   hCursor AS PTR
	MEMBER   hbrBackground AS PTR
	MEMBER   lpszMenuName AS PSZ
	MEMBER   lpszClassName AS PSZ



VOSTRUCT _winMINMAXINFO
	MEMBER ptReserved IS _winPOINT
	MEMBER ptMaxSize IS _winPOINT
	MEMBER ptMaxPosition IS _winPOINT
	MEMBER ptMinTrackSize IS _winPOINT
	MEMBER ptMaxTrackSize IS _winPOINT

VOSTRUCT _winCOPYDATASTRUCT
	MEMBER  dwData AS DWORD
	MEMBER  cbData AS DWORD
	MEMBER  lpData AS DWORD

VOSTRUCT _winMDINEXTMENU
	MEMBER   hmenuIn AS PTR
	MEMBER  hmenuNext AS PTR
	MEMBER  hwndNext AS PTR

VOSTRUCT _winWINDOWPOS
	MEMBER  hwnd AS PTR
	MEMBER hwndInsertAfter AS PTR
	MEMBER x AS INT
	MEMBER y AS INT
	MEMBER cx AS INT
	MEMBER cy AS INT
	MEMBER Flags AS DWORD


VOSTRUCT _winNCCALCSIZE_PARAMS
	MEMBER   DIM rgrc[3] IS _winRECT
   //RvdH 040407 Next member changed from IS to AS
   MEMBER lppos AS _winWINDOWPOS


VOSTRUCT _winACCEL
	MEMBER  fVirt AS BYTE
	MEMBER  key AS WORD
	MEMBER  cmd AS WORD

VOSTRUCT _winPAINTSTRUCT
	MEMBER  hdc AS PTR
	MEMBER  fErase AS LOGIC
	MEMBER  rcPaint IS _winRECT
	MEMBER  fRestore AS LOGIC
	MEMBER  fIncUpdate AS LOGIC
	MEMBER  DIM rgbReserved[32] AS BYTE

VOSTRUCT _winWINDOWPLACEMENT
	MEMBER  length AS DWORD
	MEMBER  Flags AS DWORD
	MEMBER  showCmd AS DWORD
	MEMBER  ptMinPosition IS _winPOINT
	MEMBER  ptMaxPosition IS _winPOINT
	MEMBER  rcNormalPosition IS _winRECT

VOSTRUCT _winNMHDR
	MEMBER  hwndFrom AS PTR
	MEMBER  idFrom AS DWORD
	MEMBER  _code AS DWORD

VOSTRUCT _winSTYLESTRUCT
	MEMBER   styleOld AS DWORD
	MEMBER  styleNew AS DWORD



VOSTRUCT _winMEASUREITEMSTRUCT
	MEMBER  CtlType AS DWORD
	MEMBER  CtlID AS DWORD
	MEMBER  itemID AS DWORD
	MEMBER  itemWidth AS DWORD
	MEMBER  itemHeight AS DWORD
	MEMBER  itemData AS DWORD


/*
 * DRAWITEMSTRUCT for ownerdraw
 */
VOSTRUCT _winDRAWITEMSTRUCT
	MEMBER  CtlType AS DWORD
	MEMBER  CtlID AS DWORD
	MEMBER  itemID AS DWORD
	MEMBER  itemAction AS DWORD
	MEMBER  itemState AS DWORD
	MEMBER  hwndItem AS PTR
	MEMBER  hdc AS PTR
	MEMBER  rcItem IS _winRECT
	MEMBER  itemData AS DWORD

/*
 * DELETEITEMSTRUCT for ownerdraw
 */

VOSTRUCT _winDELETEITEMSTRUCT
	MEMBER  CtlType AS DWORD
	MEMBER  CtlID AS DWORD
	MEMBER  itemID AS DWORD
	MEMBER  hwndItem AS PTR
	MEMBER  itemData AS DWORD

/*
 * COMPAREITEMSTUCT for ownerdraw sorting
 */

VOSTRUCT _winCOMPAREITEMSTRUCT
	MEMBER  CtlType AS DWORD
	MEMBER  CtlID AS DWORD
	MEMBER  hwndItem AS PTR
	MEMBER  itemID1 AS DWORD
	MEMBER  itemData1 AS DWORD
	MEMBER  itemID2 AS DWORD
	MEMBER  itemData2 AS DWORD
	MEMBER  dwLocaleId AS DWORD




/*
 * Message Function Templates
 */

VOSTRUCT _winTPMPARAMS
	MEMBER  cbSize AS DWORD
	MEMBER  rcExclude IS _winRECT


VOSTRUCT _winMENUITEMINFO
	MEMBER  cbSize AS DWORD
	MEMBER  fMask AS DWORD
	MEMBER  fType AS DWORD
	MEMBER  fState AS DWORD
	MEMBER  wID AS DWORD
	MEMBER  hSubMenu AS PTR
	MEMBER  hbmpChecked AS PTR
	MEMBER  hbmpUnchecked AS PTR
	MEMBER  dwItemData AS DWORD
	MEMBER  dwTypeData AS PSZ
	MEMBER  cch AS DWORD
	MEMBER hbmpItem AS PTR // Win98+, Win2K+




VOSTRUCT _winDROPSTRUCT
	MEMBER      hwndSource AS PTR
	MEMBER      hwndSink AS PTR
	MEMBER      wFmt AS DWORD
	MEMBER   dwData AS DWORD
	MEMBER   ptDrop IS _winPOINT
	MEMBER   dwControlData AS DWORD

VOSTRUCT _winDRAWTEXTPARAMS
	MEMBER  cbSize AS DWORD
	MEMBER  iTabLength AS INT
	MEMBER  iLeftMargin AS INT
	MEMBER  iRightMargin AS INT
	MEMBER  uiLengthDrawn AS DWORD


VOSTRUCT _winHELPINFO
	MEMBER      cbSize AS DWORD
	MEMBER      iContextType AS INT
	MEMBER       iCtrlId AS INT
	MEMBER       hItemHandle AS PTR
	MEMBER      dwContextId AS DWORD
	MEMBER      MousePos IS _winPOINT


VOSTRUCT _winMSGBOXPARAMS
	MEMBER  cbSize AS DWORD
	MEMBER  hwndOwner AS PTR
	MEMBER  hInstance AS PTR
	MEMBER  lpszText AS PSZ
	MEMBER       lpszCaption AS PSZ
	MEMBER  dwStyle AS DWORD
	MEMBER  lpszIcon AS PSZ
	MEMBER  dwContextHelpId AS DWORD
	MEMBER  lpfnMsgBoxCallback AS PTR
	MEMBER  dwLanguageId AS DWORD



VOSTRUCT _winMENUITEMTEMPLATEHEADER
	MEMBER versionNumber AS WORD
	MEMBER offset AS WORD

VOSTRUCT _winMENUITEMTEMPLATE
	MEMBER mtOption AS WORD
	MEMBER mtID AS WORD
	MEMBER DIM mtString[1] AS BYTE


VOSTRUCT _winICONINFO
	MEMBER  fIcon AS LOGIC
	MEMBER  xHotSpot AS DWORD
	MEMBER  yHotSpot AS DWORD
	MEMBER  hbmMask AS PTR
	MEMBER  hbmColor AS PTR


VOSTRUCT _winCURSORSHAPE  ALIGN 2  // RvdH 070411 added alignment
	MEMBER  xHotSpot AS INT
	MEMBER  yHotSpot AS INT
	MEMBER  cx AS INT
	MEMBER  cy AS INT
	MEMBER  cbWidth AS INT
	MEMBER  Planes AS BYTE
	MEMBER  BitsPixel AS BYTE

VOSTRUCT _winBUTTON_IMAGELIST
	MEMBER himl   AS PTR
	MEMBER margin IS _winRECT
	MEMBER uAlign AS DWORD


VOSTRUCT _winSCROLLINFO
	MEMBER      cbSize AS DWORD
	MEMBER      fMask AS DWORD
	MEMBER      nMin AS INT
	MEMBER      nMax AS INT
	MEMBER      nPage AS DWORD
	MEMBER      nPos AS INT
	MEMBER      nTrackPos AS INT


VOSTRUCT _winMDICREATESTRUCT
	MEMBER  szClass AS PSZ
	MEMBER  szTitle AS PSZ
	MEMBER  hOwner AS PTR
	MEMBER  x AS INT
	MEMBER  y AS INT
	MEMBER  cx AS INT
	MEMBER  cy AS INT
	MEMBER  style AS DWORD
	MEMBER  lParam AS LONGINT

VOSTRUCT _winCLIENTCREATESTRUCT
	MEMBER  hWindowMenu AS PTR
	MEMBER   idFirstChild AS DWORD


VOSTRUCT _winMULTIKEYHELP  ALIGN 2 // RvdH 070411 added alignment
	MEMBER  mkSize AS DWORD
	MEMBER  mkKeylist AS BYTE
	MEMBER  DIM szKeyphrase[1] AS BYTE


VOSTRUCT _winHELPWININFO
	MEMBER  wStructSize AS INT
	MEMBER  x AS INT
	MEMBER  y AS INT
	MEMBER  dx AS INT
	MEMBER  dy AS INT
	MEMBER  wMax AS INT
	MEMBER  DIM rgchMember[2] AS BYTE



VOSTRUCT _winNONCLIENTMETRICS
	MEMBER  cbSize AS DWORD
	MEMBER  iBorderWidth AS INT
	MEMBER  iScrollWidth AS INT
	MEMBER  iScrollHeight AS INT
	MEMBER  iCaptionWidth AS INT
	MEMBER  iCaptionHeight AS INT
	MEMBER  lfCaptionFont IS _winLOGFONT
	MEMBER  iSmCaptionWidth AS INT
	MEMBER  iSmCaptionHeight AS INT
	MEMBER  lfSmCaptionFont IS _winLOGFONT
	MEMBER  iMenuWidth AS INT
	MEMBER  iMenuHeight AS INT
	MEMBER  lfMenuFont IS _winLOGFONT
	MEMBER  lfStatusFont IS _winLOGFONT
	MEMBER  lfMessageFont IS _winLOGFONT


VOSTRUCT _winMINIMIZEDMETRICS
	MEMBER  cbSize AS DWORD
	MEMBER  iWidth  AS INT
	MEMBER  iHorzGap AS INT
	MEMBER  iVertGap AS INT
	MEMBER  iArrange AS INT

VOSTRUCT _winICONMETRICS
	MEMBER  cbSize AS DWORD
	MEMBER  iHorzSpacing AS INT
	MEMBER  iVertSpacing AS INT
	MEMBER  iTitleWrap AS INT
	MEMBER  lfFont IS _winLOGFONT


VOSTRUCT _winANIMATIONINFO
	MEMBER  cbSize AS DWORD
	MEMBER  iMinAnimate AS INT

VOSTRUCT _winSERIALKEYS
	MEMBER  cbSize AS DWORD
	MEMBER  dwFlags AS DWORD
	MEMBER  lpszActivePort AS PSZ
	MEMBER  lpszPort AS PSZ
	MEMBER  iBaudRate AS DWORD
	MEMBER  iPortState AS DWORD
	MEMBER  iActive AS DWORD




VOSTRUCT _winHIGHCONTRAST
	MEMBER   cbSize AS DWORD
	MEMBER   dwFlags AS DWORD
	MEMBER   lpszDefaultScheme AS PSZ



VOSTRUCT _winFILTERKEYS
	MEMBER  cbSize AS DWORD
	MEMBER  dwFlags AS DWORD
	MEMBER  iWaitMSec AS DWORD
	MEMBER  iDelayMSec AS DWORD
	MEMBER  iRepeatMSec AS DWORD
	MEMBER  iBounceMSec AS DWORD


VOSTRUCT _winSTICKYKEYS
	MEMBER  cbSize AS DWORD
	MEMBER dwFlags AS DWORD



VOSTRUCT _winMOUSEKEYS
	MEMBER cbSize AS DWORD
	MEMBER dwFlags AS DWORD
	MEMBER iMaxSpeed AS DWORD
	MEMBER iTimeToMaxSpeed AS DWORD
	MEMBER iCtrlSpeed AS DWORD
	MEMBER dwReserved1 AS DWORD
	MEMBER dwReserved2 AS DWORD


VOSTRUCT _winACCESSTIMEOUT
	MEMBER cbSize AS DWORD
	MEMBER dwFlags AS DWORD
	MEMBER iTimeOutMSec AS DWORD


VOSTRUCT _winSOUNDSENTRY
	MEMBER cbSize AS DWORD
	MEMBER dwFlags AS DWORD
	MEMBER iFSTextEffect AS DWORD
	MEMBER iFSTextEffectMSec AS DWORD
	MEMBER iFSTextEffectColorBits AS DWORD
	MEMBER iFSGrafEffect AS DWORD
	MEMBER iFSGrafEffectMSec AS DWORD
	MEMBER iFSGrafEffectColor AS DWORD
	MEMBER iWindowsEffect AS DWORD
	MEMBER iWindowsEffectMSec AS DWORD
	MEMBER lpszWindowsEffectDLL AS DWORD
	MEMBER iWindowsEffectOrdinal AS DWORD



VOSTRUCT _winTOGGLEKEYS
	MEMBER cbSize AS DWORD
	MEMBER dwFlags AS DWORD


VOSTRUCT _winMENUINFO // Win98+, Win2K+
	MEMBER cbSize AS DWORD
	MEMBER fMask AS DWORD
	MEMBER dwStyle AS DWORD
	MEMBER cyMax AS DWORD
	MEMBER hbrBack AS PTR
	MEMBER dwContextHelpId AS DWORD
	MEMBER dwMenuData AS DWORD PTR

VOSTRUCT _winCURSORINFO
	MEMBER cbSize AS DWORD
	MEMBER Flags AS DWORD
	MEMBER hCursor AS PTR
	MEMBER ptScreenPos IS _winPOINT
VOSTRUCT _winSCROLLBARINFO
	MEMBER cbSize AS DWORD
	MEMBER rcScrollBar IS _winRECT
	MEMBER dxyLineButton AS INT
	MEMBER xyThumbTop AS INT
	MEMBER xyThumbBottom AS INT
	MEMBER reserved AS INT
	MEMBER DIM rgstate[CCHILDREN_SCROLLBAR+1] AS DWORD


VOSTRUCT _winCOMBOBOXINFO
	MEMBER cbSize AS DWORD
	MEMBER rcItem IS _winRECT
	MEMBER rcButton IS _winRECT
	MEMBER stateButton AS DWORD
	MEMBER hwndCombo AS PTR
	MEMBER hwndItem AS PTR
	MEMBER hwndList AS PTR
VOSTRUCT _winCWPSTRUCT
	MEMBER  lParam AS LONGINT
	MEMBER  wParam AS DWORD
	MEMBER  message AS DWORD
	MEMBER  hwnd AS PTR


VOSTRUCT _winCWPRETSTRUCT
	MEMBER  lResult AS LONGINT
	MEMBER  lParam AS LONGINT
	MEMBER  wParam AS DWORD
	MEMBER  message AS DWORD
	MEMBER  hwnd AS PTR



VOSTRUCT  _winDEBUGHOOKINFO
	MEMBER  idThread AS DWORD
	MEMBER   idThreadInstaller AS DWORD
	MEMBER   lParam AS LONGINT
	MEMBER   wParam AS DWORD
	MEMBER   _code AS INT


VOSTRUCT _winHARDWAREHOOKSTRUCT
	MEMBER  hwnd AS PTR
	MEMBER  message AS DWORD
	MEMBER  wParam AS DWORD
	MEMBER  lParam AS LONGINT



VOSTRUCT _winMSG
	MEMBER   hwnd AS PTR
	MEMBER   message AS DWORD
	MEMBER   wParam  AS DWORD
	MEMBER   lParam AS LONGINT
	MEMBER   time AS DWORD
	MEMBER   pt IS _winPOINT


VOSTRUCT _winCREATESTRUCT
	MEMBER       lpCreateParams AS PTR
	MEMBER   hInstance AS PTR
	MEMBER          hMenu AS PTR
	MEMBER          hwndParent AS PTR
	MEMBER           cy AS INT
	MEMBER          cx AS INT
	MEMBER           y AS INT
	MEMBER           x AS INT
	MEMBER           style AS LONGINT
	MEMBER           lpszName AS PSZ
	MEMBER          lpszClass AS PSZ
	MEMBER          dwExStyle AS DWORD

VOSTRUCT _winBROADCASTSYSMSG
	MEMBER      uiMessage AS DWORD
	MEMBER  wParam AS DWORD
	MEMBER  lParam AS LONGINT

VOSTRUCT _winDLGTEMPLATE ALIGN 2
	MEMBER style AS DWORD
	MEMBER dwExtendedStyle AS DWORD
	MEMBER cdit AS WORD
	MEMBER x AS SHORTINT
	MEMBER y AS SHORTINT
	MEMBER cx AS SHORTINT
	MEMBER cy AS SHORTINT


VOSTRUCT _winDLGITEMTEMPLATE ALIGN 2 
	MEMBER  style AS DWORD
	MEMBER  dwExtendedStyle AS DWORD
	MEMBER  x AS SHORTINT
	MEMBER  y AS SHORTINT
	MEMBER  cx AS SHORTINT
	MEMBER  cy AS SHORTINT
	MEMBER  id AS WORD



_DLL FUNCTION wvsprintf(lpOut AS PSZ, lpFmt AS PSZ, arglist AS PTR);
	AS INT PASCAL:USER32.wvsprintfA


/*
 * Scroll Bar Constants
 */
_DLL FUNCTION LoadKeyboardLayout(pwszKLID AS PSZ, Flags AS DWORD);
	AS PTR PASCAL:USER32.LoadKeyboardLayoutA



_DLL FUNCTION ActivateKeyboardLayout( hkl AS PTR, Flags AS DWORD);
	AS PTR PASCAL:USER32.ActivateKeyboardLayout


_DLL FUNCTION ToUnicodeEx(wVirtKey AS DWORD, wScanCode AS DWORD, lpKeyState AS BYTE PTR,;
	pwszBuff AS PSZ, cchBuff AS INT, wFlags AS INT, dwhKl AS PTR);
	AS INT PASCAL:USER32.ToUnicodeEx



_DLL FUNCTION  UnloadKeyboardLayout(hkl AS PTR) AS LOGIC PASCAL:USER32.UnloadKeyboardLayout


_DLL FUNCTION GetKeyboardLayoutName(pwszKLID AS PSZ) AS LOGIC PASCAL:USER32.GetKeyboardLayoutNameA



_DLL FUNCTION GetKeyboardLayoutList( nBuff AS INT, lpList AS PTR);
	AS INT PASCAL:USER32.GetKeyboardLayoutList

_DLL FUNCTION GetKeyboardLayout(dwLayout AS DWORD) AS PTR PASCAL:USER32.GetKeyboardLayout



_DLL FUNCTION CreateDesktop(lpszDesktop AS PSZ, lpszDevice AS PSZ, pDevmode AS _winDEVMODE,;
	dwFlags AS DWORD, dwDesiredAccess AS DWORD, lpsa AS _WINSECURITY_ATTRIBUTES);
	AS PTR PASCAL:USER32.CreateDesktopA




_DLL FUNCTION OpenDesktop(lpszDesktop AS PSZ, dwFlags AS DWORD, fInherit AS LOGIC,;
	dwDesiredAccess AS DWORD) AS PTR PASCAL:USER32.OpenDesktopA



_DLL FUNCTION OpenInputDesktop( dwFlags AS DWORD, fInherit AS LOGIC,;
	dwDesiredAccess AS DWORD) AS PTR PASCAL:USER32.OpenInputDesktop


_DLL FUNCTION EnumDesktops( hwinsta AS PTR, lpEnumFunc AS PTR, lParam AS LONG);
	AS LOGIC PASCAL:USER32.EnumDesktopsA



_DLL FUNCTION EnumDesktopWindows(hDesktop AS PTR, lpfn AS PTR, lParam AS LONG);
	AS LOGIC PASCAL:USER32.EnumDesktopWindows


_DLL FUNCTION SwitchDesktop(hDesktop AS PTR) AS LOGIC PASCAL:USER32.SwitchDesktop



_DLL FUNCTION SetThreadDesktop(hDesktop AS PTR) AS LOGIC PASCAL:USER32.SetThreadDesktop



_DLL FUNCTION CloseDesktop( hDesktop AS PTR) AS LOGIC PASCAL:USER32.CloseDesktop


_DLL FUNCTION GetThreadDesktop(dwThreadId AS DWORD) AS PTR PASCAL:USER32.GetThreadDesktop


_DLL FUNCTION CreateWindowStation( lpwinsta AS PSZ, dwReserved AS DWORD,;
	dwDesiredAccess AS DWORD,lpsa AS _WINSECURITY_ATTRIBUTES);
	AS PTR PASCAL:USER32.CreateWindowStationA



_DLL FUNCTION OpenWindowStation(lpszWinSta AS PSZ, fInherit AS LOGIC,;
	dwDesiredAccess AS DWORD) AS PTR PASCAL:USER32.OpenWindowStationA



_DLL FUNCTION EnumWindowStations( lpEnumFunc AS PTR, lParam AS LONG);
	AS LOGIC PASCAL:USER32.EnumWindowStationsA




_DLL FUNCTION CloseWindowStation(hwinsta AS PTR) AS LOGIC PASCAL:USER32.CloseWindowStation


_DLL FUNCTION SetProcessWindowStation( hwinsta AS PTR) AS LOGIC PASCAL:USER32.SetProcessWindowStation


_DLL FUNCTION GetProcessWindowStation() AS PTR PASCAL:USER32.GetProcessWindowStation




_DLL FUNCTION SetUserObjectSecurity(hObj AS PTR, pSIRequested AS DWORD,;
	pSID AS PTR) AS LOGIC PASCAL:USER32.SetUserObjectSecurity


_DLL FUNCTION GetUserObjectSecurity(hObj AS PTR, pSIRequested AS DWORD,;
	pSID AS PTR, nLength AS DWORD, lpnLengthNeeded AS DWORD PTR);
	AS LOGIC PASCAL:USER32.GetUserObjectSecurity
_DLL FUNCTION GetUserObjectInformation(hObj AS PTR, nIndes AS INT, pvInfo AS PTR,;
	nLength AS DWORD, lpnLengthNeeded AS DWORD PTR);
	AS LOGIC PASCAL:USER32.GetUserObjectInformationA



_DLL FUNCTION SetUserObjectInformation(hObj AS PTR, nIndex AS INT, pvInfo AS PTR,;
	nLength AS DWORD) AS LOGIC PASCAL:USER32.SetUserObjectInformationA



FUNCTION MAKEWPARAM(l AS WORD, h AS WORD) AS DWORD STRICT
	RETURN  (DWORD(_CAST, MakeLong(l, h)))

_DLL FUNCTION RegisterWindowMessage(lpString AS PSZ) AS DWORD  PASCAL:USER32.RegisterWindowMessageA



_DLL FUNCTION DrawEdge(hdc AS PTR, qrc AS _winRECT, edge AS DWORD, grfFlags AS DWORD);
	AS LOGIC PASCAL:USER32.DrawEdge




_DLL FUNCTION DrawFrameControl(hdc AS PTR, lprc AS _winRECT, uType AS DWORD,;
	uState AS DWORD) AS LOGIC PASCAL:USER32.DrawFrameControl


_DLL FUNCTION DrawCaption(hwnd AS PTR, hdc AS PTR, lprc AS _winRECT, uFlags AS DWORD);
	AS VOID PASCAL:USER32.DrawCaption

_DLL FUNCTION DrawAnimatedRects( hwnd AS PTR, idAni AS INT, lprcFrom AS _winRECT,;
	lprcTo AS _winRECT)AS LOGIC PASCAL:USER32.DrawAnimatedRects



_DLL FUNCTION GetMessage(lpMsg AS _winMSG, hwnd AS PTR, wMsgFilterMin AS DWORD,;
	wMsgFilterMax AS DWORD) AS LOGIC PASCAL:USER32.GetMessageA



_DLL FUNCTION TranslateMessage( lpMsg AS _winMSG) AS LOGIC PASCAL:USER32.TranslateMessage



_DLL FUNCTION DispatchMessage(lpMsg AS _winMSG ) AS LONG PASCAL:USER32.DispatchMessageA



_DLL FUNCTION SetMessageQueue(cMessagesMax AS INT ) AS LOGIC PASCAL:USER32.SetMessageQueue


_DLL FUNCTION PeekMessage(lpMsg AS _winMSG, hwnd AS PTR, wMsgFilterMin AS DWORD,;
	wMsgFilterMax AS DWORD, wRemoveMsg AS DWORD);
	AS LOGIC PASCAL:USER32.PeekMessageA



_DLL FUNCTION RegisterHotKey(hwnd AS PTR, id AS INT, sfsModifiers AS DWORD, vk AS DWORD);
	AS LOGIC PASCAL:USER32.RegisterHotKey

_DLL FUNCTION UnregisterHotKey( hwnd AS PTR, id AS INT ) AS LOGIC PASCAL:USER32.UnregisterHotKey

FUNCTION ExitWindows(dwReserved AS DWORD, _code AS DWORD) AS LOGIC STRICT
	RETURN ExitWindowsEx( EWX_LOGOFF, 0xFFFFFFFF)


_DLL FUNCTION ExitWindowsEx( uFlags AS DWORD, dwReserved AS DWORD);
	AS LOGIC PASCAL:USER32.ExitWindowsEx

_DLL FUNCTION SwapMouseButton( fSwap AS LOGIC) AS LOGIC PASCAL:USER32.SwapMouseButton


_DLL FUNCTION GetMessagePos() AS DWORD PASCAL:USER32.GetMessagePos


_DLL FUNCTION GetMessageTime() AS LONG PASCAL:USER32.GetMessageTime


_DLL FUNCTION GetMessageExtraInfo() AS LONG PASCAL:USER32.GetMessageExtraInfo



_DLL FUNCTION SetMessageExtraInfo(lParam AS LONG) AS LONG PASCAL:USER32.SetMessageExtraInfo




_DLL FUNCTION SendMessage(hwnd AS PTR, Msg AS DWORD, wParam AS DWORD, lParam AS LONG);
	AS LONG PASCAL:USER32.SendMessageA



_DLL FUNCTION SendMessageTimeout(hwnd AS PTR, Msg AS DWORD, wParam AS DWORD,;
	lParam AS LONG, fuFlags AS DWORD, uTimeOut AS DWORD,;
	lpdwResult AS DWORD PTR) AS LONG PASCAL:USER32.SendMessageTimeoutA





_DLL FUNCTION SendNotifyMessage(hwnd AS PTR, Msg AS DWORD, wParam AS DWORD,;
	lParam AS LONG) AS LOGIC PASCAL:USER32.SendNotifyMessageA




_DLL FUNCTION SendMessageCallback(hwnd AS PTR, Msg AS DWORD, wParam AS DWORD,;
	lParam AS LONG, lpResultCallBack AS PTR,;
	dwData AS DWORD) AS LOGIC PASCAL:USER32.SendMessageCallbackA


_DLL FUNCTION BroadcastSystemMessage(dwFlags AS DWORD, lpdwRecipients AS DWORD PTR,;
	uiMessage AS DWORD, wParam AS DWORD,;
	lParam AS LONG) AS LONG PASCAL:USER32.BroadcastSystemMessage

_DLL FUNCTION PostMessage(hwnd AS PTR, Msg AS DWORD, wParam AS DWORD, lParam AS LONG);
	AS LOGIC PASCAL:USER32.PostMessageA



_DLL FUNCTION PostThreadMessage(idThread AS DWORD, Msg AS DWORD, wParam AS DWORD,;
	lParam AS LONG ) AS LOGIC PASCAL:USER32.PostThreadMessageA




_DLL FUNCTION AttachThreadInput(idArrach AS DWORD, idAttachTo AS DWORD, fAttach AS LOGIC);
	AS LOGIC PASCAL:USER32.AttachThreadInput


_DLL FUNCTION ReplyMessage(lResult AS LONG ) AS LOGIC PASCAL:USER32.ReplyMessage


_DLL FUNCTION WaitMessage() AS LOGIC PASCAL:USER32.WaitMessage


_DLL FUNCTION WaitForInputIdle( hProcwss AS PTR, deMilliseconds AS DWORD);
	AS DWORD PASCAL:USER32.WaitForInputIdle


_DLL FUNCTION DefWindowProc(hwnd AS PTR, Msg AS DWORD, wParam AS DWORD,;
	lParam AS LONG ) AS LONG PASCAL:USER32.DefWindowProcA



_DLL FUNCTION PostQuitMessage(nExitCode AS INT) AS VOID PASCAL:USER32.PostQuitMessage






_DLL FUNCTION CallWindowProc(lpPrevWndFunc AS PTR, hwnd AS PTR, Msg AS DWORD,;
	wParam AS DWORD, lParam AS LONG);
	AS LONG PASCAL:USER32.CallWindowProcA



_DLL FUNCTION InSendMessage() AS LOGIC PASCAL:USER32.InSendMessage


_DLL FUNCTION GetDoubleClickTime() AS DWORD PASCAL:USER32.GetDoubleClickTime


_DLL FUNCTION SetDoubleClickTime(uinterval AS DWORD) AS LOGIC PASCAL:USER32.SetDoubleClickTime



_DLL FUNCTION RegisterClass(lpWndClass AS _winWNDCLASS) AS WORD PASCAL:USER32.RegisterClassA



_DLL FUNCTION UnregisterClass( lpClassName AS PSZ, hInstance AS PTR);
	AS LOGIC PASCAL:USER32.UnregisterClassA




_DLL FUNCTION GetClassInfo(hInstance AS PTR, lpClassName AS PSZ,;
	lpWndClass AS _winWNDCLASS) AS LOGIC PASCAL:USER32.GetClassInfoA




_DLL FUNCTION RegisterClassEx(lpwcx AS _winWNDCLASSEX);
	AS WORD PASCAL:USER32.RegisterClassExA



_DLL FUNCTION GetClassInfoEx(hinst AS PTR, lpszClass AS PSZ, lpwcx AS _winWNDCLASSEX);
	AS LOGIC PASCAL:USER32.GetClassInfoExA



_DLL FUNCTION CreateWindowEx(dwExStyle AS DWORD, lpClasssName AS PSZ, lpWindowName AS PSZ,;
	dwStyle AS DWORD, x AS INT, y AS INT, nWidth AS INT,;
	nHeight AS INT, hwndParent AS PTR, hMenu AS PTR,;
	hInstance AS PTR, lpParam AS PTR);
	AS PTR PASCAL:USER32.CreateWindowExA




FUNCTION CreateWindow(lpClassName AS PSZ, lpWindowName AS PSZ, dwStyle AS DWORD,;
	x AS INT, y AS INT, nWidth AS INT, nHeight AS INT,;
	hwndParent AS PTR, hMenu AS PTR, hInstance AS PTR,;
	lpParam AS PTR) AS PTR STRICT
	RETURN  CreateWindowEx(0L, lpClassName, lpWindowName, dwStyle, x, y,;
		nWidth, nHeight, hwndParent, hMenu, hInstance, lpParam)




_DLL FUNCTION IsWindow(hwnd AS PTR) AS LOGIC PASCAL:USER32.IsWindow


_DLL FUNCTION IsMenu( hMenu AS PTR) AS LOGIC PASCAL:USER32.IsMenu


_DLL FUNCTION IsChild( hwndParent AS PTR, hwnd AS PTR) AS LOGIC PASCAL:USER32.IsChild


_DLL FUNCTION DestroyWindow (hwnd AS PTR) AS LOGIC PASCAL:USER32.DestroyWindow


_DLL FUNCTION ShowWindow(hwnd AS PTR, nCmdShow AS INT) AS LOGIC PASCAL:USER32.ShowWindow


_DLL FUNCTION ShowWindowAsync( hwnd AS PTR, nCmdShow AS INT) AS LOGIC PASCAL:USER32.ShowWindowAsync



_DLL FUNCTION FlashWindow( hwnd AS PTR, bInvert AS LOGIC ) AS LOGIC PASCAL:USER32.FlashWindow


_DLL FUNCTION ShowOwnedPopups( hwnd AS PTR, fShow AS LOGIC ) AS LOGIC PASCAL:USER32.ShowOwnedPopups


_DLL FUNCTION OpenIcon( hwnd AS PTR) AS LOGIC PASCAL:USER32.OpenIcon


_DLL FUNCTION CloseWindow( hwnd AS PTR) AS LOGIC PASCAL:USER32.CloseWindow


_DLL FUNCTION MoveWindow(hwnd AS PTR,  x AS INT, y AS INT,  nWidth AS INT,  nHeight AS INT,  bRepaint AS LOGIC);
	AS LOGIC PASCAL:USER32.MoveWindow


_DLL FUNCTION SetWindowPos( hwnd AS PTR, hwndInsertAfter AS PTR, x AS INT, y AS INT,;
	cx AS INT, cy AS INT, uFlags AS DWORD);
	AS LOGIC PASCAL:USER32.SetWindowPos



_DLL FUNCTION GetWindowPlacement( hwnd AS PTR, lpwndpl AS _winWINDOWPLACEMENT);
	AS LOGIC PASCAL:USER32.GetWindowPlacement

_DLL FUNCTION SetWindowPlacement( hwnd AS PTR, lpwndpl AS _winWINDOWPLACEMENT);
	AS LOGIC PASCAL:USER32.SetWindowPlacement



_DLL FUNCTION BeginDeferWindowPos(nNumWindows AS INT) AS PTR PASCAL:USER32.BeginDeferWindowPos


_DLL FUNCTION DeferWindowPos( hWinPosInfo AS PTR, hwnd AS PTR, hwndInsertAfter AS PTR,;
   x AS INT, y AS INT, cx AS INT, cy AS INT, uFlags AS DWORD);
	AS PTR PASCAL:USER32.DeferWindowPos
//RvdH 040122 Added uFlags parameter. Report from Igor
_DLL FUNCTION EndDeferWindowPos( hWinPosInfo AS PTR) AS PTR PASCAL:USER32.EndDeferWindowPos



_DLL FUNCTION IsWindowVisible(hwnd AS PTR) AS LOGIC PASCAL:USER32.IsWindowVisible


_DLL FUNCTION IsIconic( hwnd AS PTR) AS LOGIC PASCAL:USER32.IsIconic


_DLL FUNCTION AnyPopup() AS LOGIC PASCAL:USER32.AnyPopup


_DLL FUNCTION BringWindowToTop(hwnd AS PTR) AS LOGIC PASCAL:USER32.BringWindowToTop


_DLL FUNCTION IsZoomed(hwnd AS PTR) AS LOGIC PASCAL:USER32.IsZoomed

_DLL FUNCTION CreateDialogParam(hInstance AS PTR, lpTemplateName AS PSZ,;
	hwndParent AS PTR, lpDialogFunc AS PTR,;
	dwInitParam AS LONG) AS PTR PASCAL:USER32.CreateDialogParamA




_DLL FUNCTION CreateDialogIndirectParam(hInstance AS PTR, lpTemplate AS _winDLGTEMPLATE,;
	hwndParent AS PTR, lpDialogFunc AS PTR,;
	dwInitParam AS LONG) AS PTR PASCAL:USER32.CreateDialogIndirectParamA
//RvdH 040122 Changed lpTemplate parameter. Report from Igor


FUNCTION CreateDialog(hInstance AS PTR, lpName AS PSZ, hwndParent AS PTR,;
	lpDialogFunc AS PTR) AS PTR STRICT
	RETURN CreateDialogParam(hInstance, lpName, hwndParent, lpDialogFunc, 0L)


FUNCTION CreateDialogIndirect(hInstance AS PTR, lpTemplate AS _winDLGTEMPLATE, hwndParent AS PTR,;
	lpDialogFunc AS PTR) AS PTR STRICT
	RETURN CreateDialogIndirectParam(hInstance, lpTemplate, hwndParent, lpDialogFunc, 0L)


_DLL FUNCTION DialogBoxParam(hInstance AS PTR, lpTemplateName AS PSZ, hwndParent AS PTR,;
	lpDialogFunc AS PTR, dwInitParam AS LONGINT);
	AS INT PASCAL:USER32.DialogBoxParamA




_DLL FUNCTION DialogBoxIndirectParam(hInstance AS PTR, hDialogTemplate AS _winDLGTEMPLATE,;
	hwndParent AS PTR, lpDialogFunc AS PTR, dwInitParam AS LONGINT);
	AS INT PASCAL:USER32.DialogBoxIndirectParamA


FUNCTION DialogBox(hInstance AS PTR, lpTemplate AS PSZ, hwndParent AS PTR,;
	lpDialogFunc AS PTR) AS INT STRICT
	RETURN DialogBoxParam(hInstance, lpTemplate, hwndParent, lpDialogFunc, 0L)



FUNCTION DialogBoxIndirect(hInstance AS PTR, hDialogTemplate AS _winDLGTEMPLATE, hwndParent AS PTR,;
	lpDialogFunc AS PTR) AS INT STRICT
	RETURN DialogBoxIndirectParam(hInstance, hDialogTemplate , hwndParent, lpDialogFunc, 0L)



_DLL FUNCTION EndDialog(hDlg AS PTR, nResult AS INT) AS LOGIC PASCAL:USER32.EndDialog


_DLL FUNCTION GetDlgItem( hDlg AS PTR, nIDDlgItem AS INT) AS PTR PASCAL:USER32.GetDlgItem



_DLL FUNCTION SetDlgItemInt( hDlg AS PTR, nIDDlgItem AS INT, uValue AS DWORD,;
	bSigned AS LOGIC) AS LOGIC PASCAL:USER32.SetDlgItemInt



_DLL FUNCTION GetDlgItemInt(hDlg AS PTR, nIDDlgItem AS INT, lpTranslated AS LOGIC PTR,;
	bSigned AS LOGIC) AS DWORD PASCAL:USER32.GetDlgItemInt
//RvdH 040122 changed lpTranslated parameter. Report from Igor.


_DLL FUNCTION SetDlgItemText( hDlg AS PTR, nIDDlgItem AS INT, lpString AS PSZ);
	AS LOGIC PASCAL:USER32.SetDlgItemTextA



_DLL FUNCTION GetDlgItemText(hDlg AS PTR, nIDDlgItem AS INT, lpString AS PSZ,;
   nMaxCount AS INT) AS DWORD PASCAL:USER32.GetDlgItemTextA


_DLL FUNCTION CheckDlgButton( hDlg AS PTR, nIDButton AS INT, uCheck AS DWORD);
	AS LOGIC PASCAL:USER32.CheckDlgButton

_DLL FUNCTION CheckRadioButton(hDlg AS PTR, nIDFirstButtom AS INT, nIDLastButton AS INT,;
	nIDCheckButton AS INT) AS LOGIC PASCAL: USER32.CheckRadioButton


_DLL FUNCTION IsDlgButtonChecked(hDlg AS PTR, nIDButton AS INT);
	AS DWORD PASCAL:USER32.IsDlgButtonChecked



_DLL FUNCTION SendDlgItemMessage(hDlg AS PTR, nIDDlgItem AS INT, Msg AS DWORD, wParam AS DWORD,;
	lParam AS LONGINT) AS LONG PASCAL:USER32.SendDlgItemMessageA



_DLL FUNCTION GetNextDlgGroupItem(hDlg AS PTR, hCtl AS PTR, bPrevious AS LOGIC);
	AS PTR PASCAL:USER32.GetNextDlgGroupItem


_DLL FUNCTION GetNextDlgTabItem(hDlg AS PTR, hCtl AS PTR, bPrevious AS LOGIC);
	AS PTR PASCAL:USER32.GetNextDlgTabItem

_DLL FUNCTION GetDlgCtrlID( hwnd AS PTR) AS INT PASCAL:USER32.GetDlgCtrlID


_DLL FUNCTION GetDialogBaseUnits() AS LONG PASCAL:USER32.GetDialogBaseUnits


_DLL FUNCTION DefDlgProc( hDlg AS PTR, Msg AS DWORD, wParam AS DWORD, lParam AS LONGINT);
	AS LONGINT PASCAL:USER32.DefDlgProcA



_DLL FUNCTION CallMsgFilter(lpMsg AS _winMSG, nCode AS INT) AS LOGIC PASCAL:USER32.CallMsgFilterA







_DLL FUNCTION OpenClipboard(hWndNewOwner AS PTR) AS LOGIC PASCAL:USER32.OpenClipboard


_DLL FUNCTION CloseClipboard() AS LOGIC PASCAL:USER32.CloseClipboard


_DLL FUNCTION GetClipboardOwner() AS PTR PASCAL:USER32.GetClipboardOwner


_DLL FUNCTION SetClipboardViewer(hWndNewViewer AS PTR) AS PTR PASCAL:USER32.SetClipboardViewer


_DLL FUNCTION GetClipboardViewer() AS PTR PASCAL:USER32.GetClipboardViewer


_DLL FUNCTION ChangeClipboardChain( hWncRemove AS PTR, hWndNewNext AS PTR);
	AS LOGIC PASCAL:USER32.ChangeClipboardChain

_DLL FUNCTION SetClipboardData(uFormat AS DWORD, hMem AS PTR) AS PTR PASCAL:USER32.SetClipboardData


_DLL FUNCTION GetClipboardData( uFormat AS DWORD) AS PTR PASCAL:USER32.GetClipboardData


_DLL FUNCTION RegisterClipboardFormat(lpszFormat AS PSZ) AS DWORD PASCAL:USER32.RegisterClipboardFormatA



_DLL FUNCTION CountClipboardFormats() AS INT PASCAL:USER32.CountClipboardFormats


_DLL FUNCTION EnumClipboardFormats( format AS DWORD) AS DWORD PASCAL:USER32.EnumClipboardFormats


_DLL FUNCTION GetClipboardFormatName(format AS DWORD, lpszFormaName AS PSZ,;
	cchMaxCount AS INT) AS INT PASCAL:USER32.GetClipboardFormatNameA



_DLL FUNCTION EmptyClipboard() AS LOGIC PASCAL:USER32.EmptyClipboard


_DLL FUNCTION IsClipboardFormatAvailable(format AS DWORD) AS LOGIC PASCAL:USER32.IsClipboardFormatAvailable


_DLL FUNCTION GetPriorityClipboardFormat(paFormatPriorityList AS DWORD PTR, cFormatS AS INT);
	AS INT PASCAL:USER32.GetPriorityClipboardFormat



_DLL FUNCTION GetOpenClipboardWindow() AS PTR PASCAL:USER32.GetOpenClipboardWindow





_DLL FUNCTION CharToOem(lpsxSrc AS PSZ, lpszDst AS PSZ) AS LOGIC PASCAL:USER32.CharToOemA



_DLL FUNCTION OemToChar( lpszSrc AS PSZ, lpszDst AS PSZ) AS LOGIC PASCAL:USER32.OemToCharA



_DLL FUNCTION CharToOemBuff( lpszSrc AS PSZ, lpszDst AS PSZ, cchDstLength AS DWORD);
	AS LOGIC PASCAL:USER32.CharToOemBuffA



_DLL FUNCTION OemToCharBuff(lpszSrc AS PSZ, lpszDst AS PSZ, cchDsrLenght AS DWORD);
	AS LOGIC PASCAL:USER32.OemToCharBuffA



_DLL FUNCTION CharUpper( lpsz AS PSZ) AS PSZ PASCAL:USER32.CharUpperA



_DLL FUNCTION CharUpperBuff( lpsz AS PSZ, cchLenght AS DWORD) AS DWORD PASCAL:USER32.CharUpperBuffA



_DLL FUNCTION CharLower( lpsz AS PSZ) AS PSZ PASCAL:USER32.CharLowerA



_DLL FUNCTION CharLowerBuff(lpsz AS PSZ, cchLenght AS DWORD) AS DWORD PASCAL:USER32.CharLowerBuffA



_DLL FUNCTION CharNext( lpsz AS PSZ) AS PSZ PASCAL:USER32.CharNextA




_DLL FUNCTION CharPrev(lpszStart AS PSZ, lpszCurrent AS PSZ) AS PSZ PASCAL:USER32.CharPrevA



_DLL FUNCTION CharNextExA( CodePage AS WORD, lpCurrentChar  AS PSZ, dwFlags AS DWORD);
	AS PSZ PASCAL:USER32.CharNextExA



_DLL FUNCTION CharPrevExA( CodePage AS WORD, lpCurrentChar  AS PSZ, dwFlags AS DWORD);
	AS PSZ PASCAL:USER32.CharPrevExA







_DLL FUNCTION IsCharAlpha( ch AS BYTE ) AS LOGIC PASCAL:USER32.IsCharAlphaA




_DLL FUNCTION IsCharAlphaNumeric( ch AS BYTE ) AS LOGIC PASCAL:USER32.IsCharAlphaNumericA



_DLL FUNCTION IsCharUpper(ch AS BYTE ) AS LOGIC PASCAL:USER32.IsCharUpperA




_DLL FUNCTION IsCharLower( ch AS BYTE ) AS LOGIC PASCAL:USER32.IsCharLowerA




_DLL FUNCTION SetFocus( hwnd AS PTR ) AS PTR PASCAL:USER32.SetFocus


_DLL FUNCTION GetActiveWindow() AS PTR PASCAL:USER32.GetActiveWindow


_DLL FUNCTION GetFocus() AS PTR PASCAL:USER32.GetFocus


_DLL FUNCTION GetKBCodePage() AS DWORD PASCAL:USER32.GetKBCodePage


_DLL FUNCTION GetKeyState(nVirtkey AS INT) AS SHORT PASCAL:USER32.GetKeyState


_DLL FUNCTION GetAsyncKeyState( vKey AS INT) AS SHORT PASCAL:USER32.GetAsyncKeyState



_DLL FUNCTION GetKeyboardState( lpKeyState AS BYTE PTR) AS LOGIC PASCAL:USER32.GetKeyboardState


_DLL FUNCTION SetKeyboardState( lpKeyState AS BYTE PTR) AS LOGIC PASCAL:USER32.SetKeyboardState


_DLL FUNCTION GetKeyNameText(lParam AS LONG, lpString AS PSZ, nSize AS INT);
	AS INT PASCAL:USER32.GetKeyNameTextA



_DLL FUNCTION GetKeyboardType(nTypeFlag AS INT) AS INT PASCAL:USER32.GetKeyboardType


_DLL FUNCTION ToAscii( uVirtKey AS DWORD, uScanCode AS DWORD, lpKeyState    AS BYTE PTR,;
	lpChar AS WORD PTR, uFlags AS DWORD) AS INT PASCAL:USER32.ToAscii


_DLL FUNCTION ToAsciiEx(uVirtKey AS DWORD, uScanCode AS DWORD, lpKeyState AS BYTE PTR,;
	lpChar AS WORD PTR, uFlags AS DWORD, dwhKl AS PTR);
	AS INT PASCAL:USER32.ToAsciiEx



_DLL FUNCTION ToUnicode( wVirtKey AS DWORD, wScanCode AS DWORD, lpKeyState AS BYTE PTR,;
	pwszBugff AS PSZ, cchBuff AS INT, wFlags AS DWORD);
	AS INT PASCAL:USER32.ToUnicode


_DLL FUNCTION  OemKeyScan( wOemChar AS WORD) AS DWORD PASCAL:USER32.OemKeyScan


_DLL FUNCTION VkKeyScan(ch AS BYTE) AS SHORT PASCAL:USER32.VkKeyScanA




_DLL FUNCTION VkKeyScanEx( ch AS BYTE, dwhKl AS PTR) AS SHORT PASCAL:USER32.VkKeyScanExA


_DLL FUNCTION keybd_event(bVk AS BYTE, bScan AS BYTE, dwFlags AS DWORD,;
	dwExtraInfo AS DWORD) AS VOID PASCAL:USER32.keybd_event

_DLL FUNCTION mouse_event( dwFlags AS DWORD, dx AS DWORD, dy AS DWORD, cButtons AS DWORD,;
	dwExtraInfor AS DWORD) AS VOID PASCAL:USER32.mouse_event

_DLL FUNCTION MapVirtualKey( uCode AS DWORD, uMapType AS DWORD) AS DWORD PASCAL:USER32.MapVirtualKeyA



_DLL FUNCTION MapVirtualKeyEx( uCode AS DWORD, uMapType AS DWORD, dwhKl AS PTR);
	AS DWORD PASCAL:USER32.MapVirtualKeyExA


_DLL FUNCTION GetInputState() AS LOGIC PASCAL:USER32.GetInputState



_DLL FUNCTION GetQueueStatus( Flags AS DWORD) AS DWORD PASCAL:USER32.GetQueueStatus


_DLL FUNCTION GetCapture() AS PTR PASCAL:USER32.GetCapture


_DLL FUNCTION SetCapture( hWnc AS PTR) AS PTR PASCAL:USER32.SetCapture


_DLL FUNCTION ReleaseCapture() AS LOGIC PASCAL:USER32.ReleaseCapture


_DLL FUNCTION MsgWaitForMultipleObjects( nCount AS DWORD, lHandles AS PTR,;
	fWaitAll AS LOGIC, dwMilliseconds AS DWORD,;
	dwWakeMask AS DWORD);
	AS DWORD PASCAL:USER32.MsgWaitForMultipleObjects


_DLL FUNCTION SetTimer(hwnd AS PTR, nIDEvent AS DWORD, uElapse AS DWORD,;
	lpTimerFunc AS PTR) AS DWORD PASCAL:USER32.SetTimer


_DLL FUNCTION KillTimer(hwnd AS PTR, uIDEvent AS DWORD) AS LOGIC PASCAL:USER32.KillTimer


_DLL FUNCTION IsWindowUnicode( hwnd AS PTR) AS LOGIC PASCAL:USER32.IsWindowUnicode


_DLL FUNCTION EnableWindow( hwnd AS PTR, bEnable AS LOGIC) AS LOGIC PASCAL:USER32.EnableWindow



_DLL FUNCTION IsWindowEnabled(hwnd AS PTR) AS LOGIC PASCAL:USER32.IsWindowEnabled


_DLL FUNCTION LoadAccelerators( hInstance AS PTR, lpTableName AS PSZ);
	AS PTR PASCAL:USER32.LoadAcceleratorsA



_DLL FUNCTION CreateAcceleratorTable(lpaccl AS _winACCEL, cEntries AS INT);
	AS PTR PASCAL:USER32.CreateAcceleratorTableA



_DLL FUNCTION DestroyAcceleratorTable( hAccel AS PTR) AS LOGIC PASCAL:USER32.DestroyAcceleratorTable



_DLL FUNCTION CopyAcceleratorTable( hAccelSrc AS PTR, lpAccelDst AS _winACCEL,;
	cAccelEntries AS INT);
	AS INT PASCAL:USER32.CopyAcceleratorTableA




_DLL FUNCTION TranslateAccelerator(hwnd AS PTR, hAccTable AS PTR, lpMsg AS _winMSG);
	AS INT PASCAL:USER32.TranslateAcceleratorA





_DLL FUNCTION GetSystemMetrics( nIndex AS INT) AS INT PASCAL:USER32.GetSystemMetrics




_DLL FUNCTION LoadMenu( hInstance AS PTR, lpMenuName AS PSZ);
	AS PTR PASCAL:USER32.LoadMenuA



_DLL FUNCTION LoadMenuIndirect( lpMenuTemplate AS PTR) AS PTR PASCAL:USER32.LoadMenuIndirectA



_DLL FUNCTION GetMenu( hwnd AS PTR) AS PTR PASCAL:USER32.GetMenu


_DLL FUNCTION SetMenu( hwnd AS PTR, hMenu AS PTR) AS LOGIC PASCAL:USER32.SetMenu


_DLL FUNCTION ChangeMenu( hMenu AS PTR, cmd AS DWORD, lpszNewItem AS PSZ,;
	cmdInsert AS DWORD, Flags AS DWORD) AS LOGIC PASCAL:USER32.ChangeMenuA



_DLL FUNCTION HiliteMenuItem( hwnd AS PTR, hMenu AS PTR, uIDHiliteItem AS DWORD,;
	uHilite AS DWORD) AS LOGIC PASCAL:USER32.HiliteMenuItem


_DLL FUNCTION GetMenuString( hMenu AS PTR, uIDItem AS DWORD, lpString AS PSZ,;
	nMaxCount AS INT, uFlags AS DWORD);
	AS INT PASCAL:USER32.GetMenuStringA



_DLL FUNCTION GetMenuState(hMenu AS PTR, uId AS DWORD, uFlags AS DWORD);
	AS DWORD PASCAL:USER32.GetMenuState



_DLL FUNCTION DrawMenuBar( hwnd AS PTR) AS LOGIC PASCAL:USER32.DrawMenuBar


_DLL FUNCTION GetSystemMenu( hwnd AS PTR, bRevert AS LOGIC) AS PTR PASCAL:USER32.GetSystemMenu


_DLL FUNCTION CreateMenu() AS PTR PASCAL:USER32.CreateMenu


_DLL FUNCTION CreatePopupMenu() AS PTR PASCAL:USER32.CreatePopupMenu


_DLL FUNCTION DestroyMenu( hMenu AS PTR) AS LOGIC PASCAL:USER32.DestroyMenu



_DLL FUNCTION CheckMenuItem( hMenu AS PTR, uIDCheckItem AS DWORD, uCheck AS DWORD);
	AS DWORD PASCAL:USER32.CheckMenuItem

_DLL FUNCTION EnableMenuItem(hMenu AS PTR, uIDEnableItem AS DWORD, uEnable AS DWORD);
	AS LONG PASCAL:USER32.EnableMenuItem


_DLL FUNCTION GetSubMenu(hMenu AS PTR, nPos AS INT) AS PTR PASCAL:USER32.GetSubMenu


_DLL FUNCTION GetMenuItemID( hMenu AS PTR, nPos AS INT ) AS DWORD PASCAL:USER32.GetMenuItemID


_DLL FUNCTION GetMenuItemCount( hMenu AS PTR) AS INT PASCAL:USER32.GetMenuItemCount


_DLL FUNCTION  InsertMenu(hMenu AS PTR, uPosition AS DWORD, uFlasgs AS DWORD,;
	uIdNewItem AS DWORD, lpNewItem AS PSZ);
	AS LOGIC PASCAL:USER32.InsertMenuA



_DLL FUNCTION AppendMenu( hMenu AS PTR, uFlags AS DWORD, uIdNewItem AS DWORD,;
	lpNewItem AS PSZ) AS LOGIC PASCAL:USER32.AppendMenuA



_DLL FUNCTION ModifyMenu(hMnu AS PTR, uPosition AS DWORD, uFlags AS DWORD,;
	uIdNewItem AS DWORD, lpNewItem AS PSZ);
	AS LOGIC PASCAL:USER32.ModifyMenuA



_DLL FUNCTION RemoveMenu( hMenu AS PTR, uPosition AS DWORD, uFlags AS DWORD);
	AS LOGIC PASCAL:USER32.RemoveMenu


_DLL FUNCTION DeleteMenu( hMenu AS PTR, uPosition AS DWORD, uFlags AS DWORD);
	AS LOGIC PASCAL:USER32.DeleteMenu


_DLL FUNCTION SetMenuItemBitmaps( hMenu AS PTR, uPosition AS DWORD, uFlags AS DWORD,;
	hBitmapUnchecked AS PTR, hBitmapChecked AS PTR);
	AS LOGIC PASCAL:USER32.SetMenuItemBitmaps


_DLL FUNCTION GetMenuCheckMarkDimensions() AS LONG PASCAL:USER32.GetMenuCheckMarkDimensions


_DLL FUNCTION TrackPopupMenu( hMenu AS PTR, uFlags AS DWORD, x AS INT, y AS INT,;
	nreserved AS INT, hwnd AS PTR, prcRect AS _winRECT);
	AS LOGIC PASCAL:USER32.TrackPopupMenu



_DLL FUNCTION TrackPopupMenuEx (hMenu AS PTR, fuFlags AS DWORD, x AS INT, y AS INT,;
	hwnd AS PTR, lptpm AS _winTPMPARAMS);
	AS LOGIC PASCAL:USER32.TrackPopupMenuEx


_DLL FUNCTION InsertMenuItem(hMenu AS PTR, uItem AS DWORD, fByPosition AS LOGIC,;
	lpmii AS _winMENUITEMINFO);
	AS LOGIC PASCAL:USER32.InsertMenuItemA



_DLL FUNCTION GetMenuItemInfo( hMenu AS PTR, uItem AS DWORD, fByPosition AS LOGIC,;
	lpmmi AS _winMENUITEMINFO) AS LOGIC PASCAL:USER32.GetMenuItemInfoA




_DLL FUNCTION SetMenuItemInfo(hMenu AS PTR, uItem AS DWORD, fByPosition AS LOGIC,;
	lpmii AS _winMENUITEMINFO) AS LOGIC PASCAL:USER32.SetMenuItemInfoA


_DLL FUNCTION GetMenuDefaultItem ( hMenu AS PTR, fBypOS AS DWORD, gmdiFlags AS DWORD);
	AS DWORD PASCAL:USER32.GetMenuDefaultItem


_DLL FUNCTION SetMenuDefaultItem(hMenu AS PTR, uItem AS DWORD, fBypOS AS DWORD);
	AS LOGIC PASCAL:USER32.SetMenuDefaultItem


_DLL FUNCTION GetMenuItemRect( hwnd AS PTR, hMenu AS PTR, uItem AS DWORD, lprcItem AS _winRECT);
	AS LOGIC PASCAL:USER32.GetMenuItemRect


_DLL FUNCTION MenuItemFromPoint( hwnd AS PTR, hMenu AS PTR, ptScreen AS _winPOINT);
	AS INT PASCAL:USER32.MenuItemFromPoint


_DLL FUNCTION DragDetect( hwnd AS PTR, pt AS _winPOINT) AS LOGIC PASCAL:USER32.DragDetect



_DLL FUNCTION DrawIcon(hdc AS PTR, x AS INT, y AS INT, hIcon AS PTR);
	AS LOGIC PASCAL:USER32.DrawIcon



_DLL FUNCTION DrawText(hdc AS PTR, lpString AS PSZ, nCount AS INT, lpRect AS _winRECT,;
	uFormat AS DWORD) AS INT PASCAL:USER32.DrawTextA




_DLL FUNCTION DrawTextEx( hdc AS PTR, lpchTexT AS PSZ, cchText AS INT, lprc AS _winRECT,;
	dwTDFormat AS DWORD, lpDTParams AS _winDRAWTEXTPARAMS);
	AS INT PASCAL:USER32.DrawTextExA




_DLL FUNCTION GrayString( hdc AS PTR, hBrush AS PTR, lpOutputFunc AS PTR, lpData AS LONGINT,;
	nCount AS INT, x AS INT, y AS INT, nWidth AS INT,;
	nHeight AS INT) AS LOGIC PASCAL:USER32.GrayStringA





_DLL FUNCTION DrawState( hdc AS PTR, hbr AS PTR, lpOutpufunc AS PTR, lData AS LONGINT,;
	wData AS DWORD, x AS INT, y AS INT, cx AS INT, cy AS INT,;
	fuFlags AS DWORD) AS LOGIC PASCAL:USER32.DrawStateA




_DLL FUNCTION TabbedTextOut( hdc AS PTR, x AS INT, y AS INT, lpString AS PSZ, nCount AS INT,;
	nTabPositions AS INT, lpnTabStopPosition AS INT PTR,;
	nTabOrigin AS INT) AS LOGIC PASCAL:USER32.TabbedTextOutA



_DLL FUNCTION GetTabbedTextExtent( hdc AS PTR, lpString AS PSZ, nCount AS INT,;
	nTabPositions AS INT, lpnTabStopPosition AS INT PTR);
	AS DWORD PASCAL:USER32.GetTabbedTextExtentA



_DLL FUNCTION UpdateWindow( hwnd AS PTR) AS LOGIC PASCAL:USER32.UpdateWindow


_DLL FUNCTION SetActiveWindow( hwnd AS PTR) AS PTR PASCAL:USER32.SetActiveWindow


_DLL FUNCTION GetForegroundWindow() AS PTR PASCAL:USER32.GetForegroundWindow





_DLL FUNCTION SetForegroundWindow(hwnd AS PTR) AS LOGIC PASCAL:USER32.SetForegroundWindow


_DLL FUNCTION WindowFromDC( hdc AS PTR) AS PTR PASCAL:USER32.WindowFromDC


_DLL FUNCTION GetDC( hwnd AS PTR) AS PTR PASCAL:USER32.GetDC


_DLL FUNCTION GetDCEx(hwnd AS PTR, hrgnClip AS PTR, Flags AS DWORD);
	AS PTR PASCAL:USER32.GetDCEx

_DLL FUNCTION GetWindowDC( hwnd AS PTR) AS PTR PASCAL:USER32.GetWindowDC


_DLL FUNCTION ReleaseDC( hwnd AS PTR, hdc AS PTR) AS INT PASCAL:USER32.ReleaseDC


_DLL FUNCTION BeginPaint( hwnd AS PTR, lpPaint AS _winPAINTSTRUCT);
	AS PTR PASCAL:USER32.BeginPaint


_DLL FUNCTION EndPaint( hwnd AS PTR, lpPaint AS _winPAINTSTRUCT);
	AS LOGIC PASCAL:USER32.EndPaint


_DLL FUNCTION GetUpdateRect(hwnd AS PTR, lpRect AS _winRECT, bErase AS LOGIC );
	AS LOGIC PASCAL:USER32.GetUpdateRect


_DLL FUNCTION GetUpdateRgn( hwnd AS PTR, hRgn AS PTR, bErase AS LOGIC);
	AS INT PASCAL:USER32.GetUpdateRgn

_DLL FUNCTION SetWindowRgn(hwnd AS PTR, hRgn AS PTR, bRedraw AS LOGIC);
	AS INT PASCAL:USER32.SetWindowRgn

_DLL FUNCTION GetWindowRgn( hwnd AS PTR, hRgn AS PTR) AS INT PASCAL:USER32.GetWindowRgn


_DLL FUNCTION ExcludeUpdateRgn( hdc AS PTR, hwnd AS PTR) AS INT PASCAL:USER32.ExcludeUpdateRgn



_DLL FUNCTION InvalidateRect(hwnd AS PTR, lpRect AS _winRECT, bErase AS LOGIC);
	AS LOGIC PASCAL:USER32.InvalidateRect


_DLL FUNCTION ValidateRect(hwnd AS PTR, lpRect AS _winRECT) AS LOGIC PASCAL:USER32.ValidateRect


_DLL FUNCTION InvalidateRgn(hwnd AS PTR, hRgn AS PTR, bErase AS LOGIC);
	AS LOGIC PASCAL:USER32.InvalidateRgn


_DLL FUNCTION ValidateRgn(hwnd AS PTR, hRgn AS PTR) AS LOGIC PASCAL:USER32.ValidateRgn


_DLL FUNCTION RedrawWindow( hwnd AS PTR, lprcUpdate AS _winRECT, hrgnUpdate AS PTR,;
	Flags AS DWORD) AS LOGIC PASCAL:USER32.RedrawWindow


_DLL FUNCTION LockWindowUpdate( hwndLock AS PTR) AS LOGIC PASCAL:USER32.LockWindowUpdate


_DLL FUNCTION ScrollWindow( hwnd AS PTR, XAmount AS INT, YAmount AS INT,;
	lpRect AS _winRECT, lpClipRect AS _winRECT);
	AS LOGIC PASCAL:USER32.ScrollWindow


_DLL FUNCTION ScrollDC(hdc AS PTR, dx AS INT, dy AS INT, lprcScroll AS _winRECT,;
	lprecClip AS _winRECT, hrgnUpdate AS PTR, lprcUpdate AS _winRECT);
	AS LOGIC PASCAL:USER32.ScrollDC


_DLL FUNCTION ScrollWindowEx(hwnd AS PTR, dx AS INT, dy AS INT, prcScroll AS _winRECT,;
	prcClip AS _winRECT, hrgnUpdate AS PTR, prcUpdate AS PTR,;
	Flags AS DWORD) AS INT PASCAL:USER32.ScrollWindowEx

_DLL FUNCTION SetScrollPos(hwnd AS PTR, nBar AS INT, nPos AS INT, vRedraw AS LOGIC);
	AS INT PASCAL:USER32.SetScrollPos


_DLL FUNCTION GetScrollPos(hwnd AS PTR, nBar AS INT) AS INT PASCAL:USER32.GetScrollPos


_DLL FUNCTION SetScrollRange(hwnd AS PTR, nBar AS INT, nMinPos AS INT, nMaxPos AS INT,;
	bRedraw AS LOGIC) AS LOGIC PASCAL:USER32.SetScrollRange


_DLL FUNCTION GetScrollRange(hwnd AS PTR, nBar AS INT, lpMinPos AS INT PTR,;
	lpMaxPos AS INT PTR) AS LOGIC PASCAL:USER32.GetScrollRange


_DLL FUNCTION ShowScrollBar( hwnd AS PTR, wBar AS INT, bShow AS LOGIC);
	AS LOGIC PASCAL:USER32.ShowScrollBar


_DLL FUNCTION EnableScrollBar(hwnd AS PTR, wsBflags AS DWORD, wArrows AS DWORD);
	AS LOGIC PASCAL:USER32.EnableScrollBar


_DLL FUNCTION SetProp(hwnd AS PTR, lpString AS PSZ, hData AS PTR);
	AS LOGIC PASCAL:USER32.SetPropA



_DLL FUNCTION GetProp(hwnd AS PTR, lpString AS PSZ) AS PTR PASCAL:USER32.GetPropA



_DLL FUNCTION RemoveProp( hwnd AS PTR, lpString AS PSZ) AS PTR PASCAL:USER32.RemovePropA



_DLL FUNCTION EnumPropsEx(hwnd AS PTR, lpEnumFunc AS PTR, lParam AS LONGINT);
	AS INT PASCAL:USER32.EnumPropsExA



_DLL FUNCTION EnumProps(hwnd AS PTR, lpEnumFunc AS PTR) AS INT PASCAL:USER32.EnumPropsA



_DLL FUNCTION SetWindowText(hwnd AS PTR, lpString AS PSZ) AS LOGIC PASCAL:USER32.SetWindowTextA



_DLL FUNCTION GetWindowText(hwnd AS PTR, lpString AS PSZ, nMaxCount AS INT);
	AS INT PASCAL:USER32.GetWindowTextA



_DLL FUNCTION GetWindowTextLength(hwnd AS PTR) AS INT PASCAL:USER32.GetWindowTextLengthA



_DLL FUNCTION GetClientRect(hwnd AS PTR, lpRect AS _winRECT);
	AS LOGIC PASCAL:USER32.GetClientRect


_DLL FUNCTION GetWindowRect(hwnd AS PTR, lpRect AS _winRECT) AS LOGIC PASCAL:USER32.GetWindowRect


_DLL FUNCTION AdjustWindowRect( lpRect AS _winRECT, dwStyle AS DWORD, bMenu AS LOGIC);
	AS LOGIC PASCAL:USER32.AdjustWindowRect


_DLL FUNCTION AdjustWindowRectEx( lpRect AS _winRECT, dwStyle AS DWORD, bMenu AS LOGIC,;
	dwExStyle AS DWORD) AS LOGIC PASCAL:USER32.AdjustWindowRectEx

_DLL FUNCTION SetWindowContextHelpId( hwnd AS PTR, dwContexHelpId AS DWORD);
	AS LOGIC PASCAL:USER32.SetWindowContextHelpId


_DLL FUNCTION GetWindowContextHelpId( hwnd AS PTR) AS DWORD PASCAL:USER32.GetWindowContextHelpId


_DLL FUNCTION SetMenuContextHelpId(hMenu AS PTR, dwContesHelpId AS DWORD);
	AS LOGIC PASCAL:USER32.SetMenuContextHelpId


_DLL FUNCTION GetMenuContextHelpId( hMenu AS PTR) AS DWORD PASCAL:USER32.GetMenuContextHelpId




_DLL FUNCTION MessageBox(hwnd AS PTR, lpText AS PSZ, lpCaption AS PSZ, uType AS DWORD);
	AS INT PASCAL:USER32.MessageBoxA



_DLL FUNCTION MessageBoxEx(hwnd AS PTR, lpText AS PSZ, lpCaption AS PSZ, uType AS DWORD,;
	wLanguageId AS WORD) AS INT PASCAL:USER32.MessageBoxExA




_DLL FUNCTION MessageBoxIndirect(lpMsgBoxparam AS _winMSGBOXPARAMS);
	AS INT PASCAL:USER32.MessageBoxIndirectA



_DLL FUNCTION MessageBeep(uType AS DWORD) AS LOGIC PASCAL:USER32.MessageBeep


_DLL FUNCTION ShowCursor(bShow AS LOGIC ) AS INT PASCAL:USER32.ShowCursor


_DLL FUNCTION SetCursorPos(x AS INT, y AS INT) AS LOGIC PASCAL:USER32.SetCursorPos


_DLL FUNCTION SetCursor(hCursor AS PTR) AS PTR PASCAL:USER32.SetCursor


_DLL FUNCTION GetCursorPos( lpPoint AS _winPOINT) AS LOGIC PASCAL:USER32.GetCursorPos


_DLL FUNCTION ClipCursor(lpRect AS _winRECT) AS LOGIC PASCAL:USER32.ClipCursor


_DLL FUNCTION GetClipCursor(lpRect AS _winRECT) AS LOGIC PASCAL:USER32.GetClipCursor


_DLL FUNCTION GetCursor() AS PTR PASCAL:USER32.GetCursor


_DLL FUNCTION CreateCaret(hwnd AS PTR, hBitmap AS PTR, nWidth AS INT, nHeight AS INT);
	AS LOGIC PASCAL:USER32.CreateCaret

_DLL FUNCTION GetCaretBlinkTime() AS DWORD PASCAL:USER32.GetCaretBlinkTime


_DLL FUNCTION SetCaretBlinkTime(uMSeconds AS DWORD) AS LOGIC PASCAL:USER32.SetCaretBlinkTime


_DLL FUNCTION DestroyCaret() AS LOGIC PASCAL:USER32.DestroyCaret


_DLL FUNCTION HideCaret( hwnd AS PTR) AS LOGIC PASCAL:USER32.HideCaret


_DLL FUNCTION ShowCaret(hwnd AS PTR) AS LOGIC PASCAL:USER32.ShowCaret


_DLL FUNCTION SetCaretPos(x AS INT, y AS INT) AS LOGIC PASCAL:USER32.SetCaretPos


_DLL FUNCTION GetCaretPos( lpPoint AS _winPOINT) AS LOGIC PASCAL:USER32.GetCaretPos



_DLL FUNCTION ClientToScreen(hwnd AS PTR, lpPoint AS _winPOINT);
	AS LOGIC PASCAL:USER32.ClientToScreen


_DLL FUNCTION ScreenToClient( hwnd AS PTR, lpPoint AS _winPOINT) AS LOGIC PASCAL:USER32.ScreenToClient


_DLL FUNCTION MapWindowPoints(hwndFrom AS PTR, hWndTo AS PTR, lpPoint AS _winPOINT,;
	cPoints AS DWORD) AS INT PASCAL:USER32.MapWindowPoints



_DLL FUNCTION WindowFromPoint(x AS LONG, y AS LONG) AS PTR PASCAL:USER32.WindowFromPoint


_DLL FUNCTION ChildWindowFromPoint( hwndParent AS PTR, x AS LONG, y AS LONG);
	AS PTR PASCAL:USER32.ChildWindowFromPoint

_DLL FUNCTION ChildWindowFromPointEx( hwndParent AS PTR, x AS LONG, y AS LONG,;
	uFlags AS DWORD) AS PTR PASCAL:USER32.ChildWindowFromPointEx




_DLL FUNCTION GetSysColor( nIndex AS INT) AS DWORD PASCAL:USER32.GetSysColor


_DLL FUNCTION GetSysColorBrush(nIndex AS INT) AS PTR PASCAL:USER32.GetSysColorBrush



_DLL FUNCTION SetSysColors( cElements AS INT, lpaElements AS INT PTR, lpaRgbValues AS DWORD PTR);
	AS LOGIC PASCAL:USER32.SetSysColors


_DLL FUNCTION DrawFocusRect( hdc AS PTR, lprc AS _winRECT) AS LOGIC PASCAL:USER32.DrawFocusRect


_DLL FUNCTION FillRect( hdc AS PTR, lprc AS _winRECT, hbr AS PTR);
	AS INT PASCAL:USER32.FillRect


_DLL FUNCTION FrameRect( hdc AS PTR, lprc AS _winRECT, hbr AS PTR);
	AS INT PASCAL:USER32.FrameRect



_DLL FUNCTION InvertRect( hdc AS PTR, lprc AS _winRECT) AS LOGIC PASCAL:USER32.InvertRect



_DLL FUNCTION SetRect( lprc AS _winRECT, xLeft AS INT, yTop AS INT, xRight AS INT,;
	yBottom AS INT) AS LOGIC PASCAL:USER32.SetRect


_DLL FUNCTION SetRectEmpty(lprc AS _winRECT) AS LOGIC PASCAL:USER32.SetRectEmpty


_DLL FUNCTION CopyRect( lprcDst AS _winRECT, lprcSrc AS _winRECT);
	AS LOGIC PASCAL:USER32.CopyRect


_DLL FUNCTION InflateRect(lprc AS _winRECT, dx AS INT, dy AS INT);
	AS LOGIC PASCAL:USER32.InflateRect


_DLL FUNCTION IntersectRect( lprcDst AS _winRECT, lprcSrc1 AS _winRECT, lprcSrc2 AS _winRECT);
	AS LOGIC PASCAL:USER32.IntersectRect


_DLL FUNCTION UnionRect( lprcDst AS _winRECT, lprcSrc1 AS _winRECT, lprcSrc2 AS _winRECT);
	AS LOGIC PASCAL:USER32.UnionRect

_DLL FUNCTION SubtractRect(lprcDst AS _winRECT, lprcSr1 AS _winRECT, lprcSrc2 AS _winRECT);
	AS LOGIC PASCAL:USER32.SubtractRect


_DLL FUNCTION OffsetRect(lprc AS _winRECT, dx AS INT, dy AS INT) AS LOGIC PASCAL:USER32.OffsetRect


_DLL FUNCTION IsRectEmpty( lprc AS _winRECT) AS LOGIC PASCAL:USER32.IsRectEmpty


_DLL FUNCTION EqualRect( lprc1 AS _winRECT, lprc2 AS _winRECT) AS LOGIC PASCAL:USER32.EqualRect


_DLL FUNCTION PtInRect( lprc AS _winRECT, pt IS _winPOINT) AS LOGIC PASCAL:USER32.PtInRect


_DLL FUNCTION GetWindowWord( hwnd AS PTR, nIndex AS INT) AS WORD PASCAL:USER32.GetWindowWord


_DLL FUNCTION SetWindowWord( hwnd AS PTR, nIndex AS INT, wNewWord AS WORD);
	AS WORD PASCAL:USER32.SetWindowWord


_DLL FUNCTION GetWindowLong(hwnd AS PTR, nIndex AS INT) AS LONG PASCAL:USER32.GetWindowLongA



_DLL FUNCTION SetWindowLong(hwnd AS PTR, nIndex AS INT, dwNewLong AS LONG);
	AS LONG PASCAL:USER32.SetWindowLongA


_DLL FUNCTION GetClassWord(hwnd AS PTR, nIndex AS INT) AS WORD PASCAL:USER32.GetClassWord


_DLL FUNCTION SetClassWord( hwnd AS PTR, nIndex AS INT, wNewWord AS WORD);
	AS WORD PASCAL:USER32.SetClassWord


_DLL FUNCTION GetClassLong(hwnd AS PTR, nIndex AS INT) AS DWORD PASCAL:USER32.GetClassLongA



_DLL FUNCTION SetClassLong(hwnd AS PTR, nIndex AS INT, dwNewLong AS LONG);
	AS DWORD PASCAL:USER32.SetClassLongA




_DLL FUNCTION GetDesktopWindow() AS PTR PASCAL:USER32.GetDesktopWindow


_DLL FUNCTION GetParent( hwnd AS PTR) AS PTR PASCAL:USER32.GetParent


_DLL FUNCTION SetParent( hWndChild AS PTR, hWndNewParent AS PTR);
	AS PTR PASCAL:USER32.SetParent


_DLL FUNCTION EnumChildWindows(hwndParent AS PTR, lpEnumFunc AS PTR, lParam AS LONG);
	AS LOGIC PASCAL:USER32.EnumChildWindows


_DLL FUNCTION FindWindow(lpClassName AS PSZ, lpWindowName AS PSZ);
	AS PTR PASCAL:USER32.FindWindowA




_DLL FUNCTION FindWindowEx( hwndParent AS PTR, hwndChildAfter AS PTR, lpszClass AS PSZ,;
	lpszWindow AS PSZ) AS PTR PASCAL:USER32.FindWindowExA




_DLL FUNCTION EnumWindows( lpEnumFunc AS PTR, lParam AS LONG) AS LOGIC PASCAL:USER32.EnumWindows


_DLL FUNCTION EnumThreadWindows( dwThreadId AS DWORD, lpfn AS PTR, lParam AS LONG);
	AS LOGIC PASCAL:USER32.EnumThreadWindows


_DLL FUNCTION GetClassName(hwnd AS PTR, lpClassName AS PSZ, nMaxCount AS INT);
	AS INT PASCAL:USER32.GetClassNameA



_DLL FUNCTION GetTopWindow( hwnd AS PTR) AS PTR PASCAL:USER32.GetTopWindow

FUNCTION GetNextWindow(hwnd AS PTR, cCmd AS DWORD) AS PTR STRICT
	RETURN GetWindow(hwnd, cCmd)



_DLL FUNCTION GetWindowThreadProcessId( hwnd AS PTR, lpdwProcessId AS DWORD PTR);
	AS DWORD PASCAL:USER32.GetWindowThreadProcessId

FUNCTION GetWindowTask(hwnd AS PTR) AS PTR STRICT
	RETURN PTR(_CAST, (GetWindowThreadProcessId(hwnd, NULL_PTR)))



_DLL FUNCTION GetLastActivePopup(hwnd AS PTR) AS PTR PASCAL:USER32.GetLastActivePopup



_DLL FUNCTION GetWindow(hwnd AS PTR, uCmd AS DWORD) AS PTR PASCAL:USER32.GetWindow




_DLL FUNCTION SetWindowsHook( nFilterType AS INT, pfnFilterProc AS PTR);
	AS PTR PASCAL:USER32.SetWindowsHookA


_DLL FUNCTION UnhookWindowsHook( nCode AS INT, pfnFilterProc AS PTR);
	AS LOGIC PASCAL:USER32.UnhookWindowsHook

_DLL FUNCTION SetWindowsHookEx( idHook AS INT, lpfn AS PTR, hmod AS PTR,;
	dwThreadId AS DWORD) AS PTR PASCAL:USER32.SetWindowsHookExA

_DLL FUNCTION UnhookWindowsHookEx( hhk AS PTR) AS LOGIC PASCAL:USER32.UnhookWindowsHookEx


_DLL FUNCTION CallNextHookEx(hhk AS PTR, nCode AS INT, wParam AS DWORD, lParam AS LONG);
	AS LONG PASCAL:USER32.CallNextHookEx


_DLL FUNCTION CheckMenuRadioItem(hMenu AS PTR, idFirst AS DWORD,idLast AS DWORD,;
	idCheck AS DWORD, uFlags AS DWORD);
	AS LOGIC PASCAL:USER32.CheckMenuRadioItem

_DLL FUNCTION LoadBitmap(hInstance AS PTR, lpBitmapName AS PSZ);
	AS PTR PASCAL:USER32.LoadBitmapA


_DLL FUNCTION LoadCursor(hInstance AS PTR, lpCursorName AS PSZ);
	AS PTR PASCAL:USER32.LoadCursorA


_DLL FUNCTION LoadCursorFromFile(lpFileName AS PSZ) AS PTR PASCAL:USER32.LoadCursorFromFileA

_DLL FUNCTION CreateCursor(hinst AS PTR, xHotSpot AS INT, yHotSpot AS INT, nWidth AS INT,;
	nHeight AS INT, pvANDPlane AS PTR, pvXORPlane AS PTR);
	AS PTR PASCAL:USER32.CreateCursor

_DLL FUNCTION DestroyCursor(hCursor AS PTR) AS LOGIC PASCAL:USER32.DestroyCursor


FUNCTION CopyCursor(pcur AS PTR) AS PTR STRICT
	RETURN  CopyIcon(pcur)


_DLL FUNCTION SetSystemCursor(hcur AS PTR, id AS DWORD) AS LOGIC PASCAL:USER32.SetSystemCursor

_DLL FUNCTION LoadIcon( hInstance AS PTR, lpIconName AS PSZ) AS PTR PASCAL:USER32.LoadIconA



_DLL FUNCTION CreateIcon(hInstance AS PTR, nWidth AS INT, nHeight AS INT, cPlaneS AS BYTE,;
	cBitsPixel AS BYTE, lpbANDbits AS BYTE, lpbXORbits AS BYTE);
	AS PTR PASCAL:USER32.CreateIcon

_DLL FUNCTION DestroyIcon(hIcon AS PTR) AS LOGIC PASCAL:USER32.DestroyIcon


_DLL FUNCTION LookupIconIdFromDirectory( presbits AS BYTE PTR, fIcon AS LOGIC );
	AS INT PASCAL:USER32.LookupIconIdFromDirectory


_DLL FUNCTION LookupIconIdFromDirectoryEx(presbits AS BYTE PTR, fIcon AS LOGIC,;
	cxDesired AS INT, cydesired AS INT,;
	Flags AS DWORD) AS INT PASCAL:USER32.LookupIconIdFromDirectoryEx


_DLL FUNCTION CreateIconFromResource( presbits AS BYTE PTR, dwResSize AS DWORD,;
	fIcon AS LOGIC, dwVer AS DWORD);
	AS PTR PASCAL:USER32.CreateIconFromResource


_DLL FUNCTION CreateIconFromResourceEx( presbits AS BYTE PTR, dwResSize AS DWORD,;
	fIcon AS LOGIC, dwVer AS DWORD, cxDesired AS INT,;
	cydesired AS INT, Flags AS DWORD);
	AS PTR PASCAL:USER32.CreateIconFromResourceEx


_DLL FUNCTION LoadImage(hinst AS PTR, lpszName AS PSZ, uType AS DWORD, cxDesired AS INT,;
	cydesired AS INT, fuLoad AS DWORD);
	AS PTR PASCAL:USER32.LoadImageA



_DLL FUNCTION CopyImage(hImage AS PTR, uType AS DWORD, cxDesired AS INT, cydesired AS INT,;
	fuFlags AS DWORD) AS PTR PASCAL:USER32.CopyImage

_DLL FUNCTION DrawIconEx(hdc     AS PTR, xLeft AS INT, yTop AS INT, hIcon AS PTR,;
	cxWidth AS INT, cyWidth AS INT, istepIfAniCur AS DWORD,;
	hbrFlickerFreeDraw AS PTR, nFlags AS DWORD) AS LOGIC PASCAL:USER32.DrawIconEx


_DLL FUNCTION CreateIconIndirect( piconinfo AS _winICONINFO) AS PTR PASCAL:USER32.CreateIconIndirect


_DLL FUNCTION CopyIcon( hIcon AS PTR) AS PTR PASCAL:USER32.CopyIcon


_DLL FUNCTION GetIconInfo( hIcon AS PTR, piconinfo AS _winICONINFO);
	AS LOGIC PASCAL:USER32.GetIconInfo

_DLL FUNCTION LoadString(hInstance AS PTR, uId AS DWORD, lpBuffer AS PSZ,;
	nBufferMax AS INT) AS INT PASCAL:USER32.LoadStringA


_DLL FUNCTION IsDialogMessage(hDlg AS PTR, lpMsg AS _winMSG) AS LOGIC PASCAL:USER32.IsDialogMessageA




_DLL FUNCTION MapDialogRect( hDlg AS PTR, lpRect AS _winRECT) AS LOGIC PASCAL:USER32.MapDialogRect


_DLL FUNCTION DlgDirList(hDlg AS PTR, lpPathSpec AS PSZ, nIDListBox AS INT,;
	nIDStaticPath AS INT, uFileType AS DWORD);
	AS INT PASCAL:USER32.DlgDirListA




_DLL FUNCTION DlgDirSelectEx( hDlg AS PTR, lpString AS PSZ, nCount AS INT,;
	nIDListBox AS INT) AS LOGIC PASCAL:USER32.DlgDirSelectExA




_DLL FUNCTION DlgDirListComboBox( hDlg AS PTR, lpPathSpec AS PSZ, nIDComboBox AS INT,;
	nIDStaticPath AS INT, uFileType AS DWORD);
	AS INT PASCAL:USER32.DlgDirListComboBoxA


_DLL FUNCTION DlgDirSelectComboBoxEx( hDlg AS PTR, lpString AS PSZ, nCount AS INT,;
	nIDComboBox AS INT) AS LOGIC PASCAL:USER32.DlgDirSelectComboBoxExA



_DLL FUNCTION SetScrollInfo(hwnd AS PTR,fnBar AS INT, lpsi AS _winSCROLLINFO,;
	fRedraw AS LOGIC) AS INT PASCAL:USER32.SetScrollInfo


_DLL FUNCTION GetScrollInfo( hwnd AS PTR, fnBar AS INT, lpsi AS _winSCROLLINFO);
	AS LOGIC PASCAL:USER32.GetScrollInfo




_DLL FUNCTION DefFrameProc(hwnd AS PTR, hWndMDIClient AS PTR, uMsg AS DWORD,;
	wParam AS DWORD, lParam AS LONGINT);
	AS LONG PASCAL:USER32.DefFrameProcA


_DLL FUNCTION DefMDIChildProc( hwnd AS PTR, uMsg AS DWORD, wParam AS DWORD,;
	lParam AS LONGINT) AS LONG PASCAL:USER32.DefMDIChildProcA




_DLL FUNCTION TranslateMDISysAccel( hWndClient AS PTR, lpMsg AS _winMSG);
	AS LOGIC PASCAL:USER32.TranslateMDISysAccel


_DLL FUNCTION ArrangeIconicWindows( hwnd AS PTR) AS DWORD PASCAL:USER32.ArrangeIconicWindows


_DLL FUNCTION CreateMDIWindow( lpClassName AS PSZ, lpWindowName AS PSZ, dsStyle AS DWORD,;
	x AS INT, y AS INT, nWidth AS INT, nHeight AS INT,;
	hwndParent AS PTR, hInstance AS PTR, lParam AS LONGINT);
	AS PTR PASCAL:USER32.CreateMDIWindowA



_DLL FUNCTION TileWindows( hwndParent AS PTR, wHOW AS DWORD, lpRect AS _winRECT,;
	cKids AS DWORD, lpKids AS PTR);
	AS WORD PASCAL:USER32.TileWindows


_DLL FUNCTION CascadeWindows( hwndParent AS PTR, wHOW AS DWORD, lpRect AS _winRECT,;
	cKids AS DWORD, lpKids AS PTR);
	AS WORD PASCAL:USER32.CascadeWindows





_DLL FUNCTION WinHelp(hWndMain AS PTR, lpszHelp AS PSZ, uCommand AS DWORD, dwDate AS DWORD);
	AS LOGIC PASCAL:USER32.WinHelpA

_DLL FUNCTION ChangeDisplaySettings( lpDevMode AS _winDEVMODE, dwFlags AS DWORD);
	AS LONG PASCAL:USER32.ChangeDisplaySettingsA


_DLL FUNCTION EnumDisplaySettings( lpszDeViceName AS PSZ, iModeNum AS DWORD,;
	lpDevMode AS _WINDEVMODE);
	AS LOGIC PASCAL:USER32.EnumDisplaySettingsA




_DLL FUNCTION SystemParametersInfo(uiAction AS DWORD, uiParam AS DWORD, pvParam AS PTR,;
	fWinIni AS DWORD) AS LOGIC PASCAL:USER32.SystemParametersInfoA





_DLL FUNCTION SetDebugErrorLevel( dwLevel AS DWORD) AS VOID PASCAL:USER32.SetDebugErrorLevel



_DLL FUNCTION SetLastErrorEx( dwErrCode AS DWORD, dwType AS DWORD) AS VOID PASCAL:USER32.SetLastErrorEx
_DLL FUNCTION wsprintf(pszBuffer AS PSZ, pszFormat AS PSZ, ...) AS INT STRICT:USER32.wsprintfA

//New Get_X_Info functions and defines, eg
_DLL FUNCTION GetScrollBarInfo(hwnd AS PTR, idObject AS LONG, psbi AS _winSCROLLBARINFO) AS LOGIC PASCAL:USER32.GetScrollBarInfo // 98+, NT-SP6+
_DLL FUNCTION GetCursorInfo(pci AS _winCURSORINFO) AS LOGIC PASCAL:USER32.GetCursorInfo // 98+, NT-SP6+

_DLL FUNCTION GetComboBoxInfo(hwndCombo AS PTR, pcbi AS _winCOMBOBOXINFO) AS LOGIC PASCAL:USER32.GetComboBoxInfo // 98+, NT-SP6+
/*
function XAmount
function abc
struct xyz
*/ 
FUNCTION POINTTOPOINTS(pt AS _winPOINT) AS LONGINT STRICT
	LOCAL cx AS WORD
	LOCAL cy AS WORD
	//PP-030924 correct 51422
	cx := WORD(pt:x)
	cy := WORD(pt:y)
	RETURN (MakeLong(cx, cy))

FUNCTION MAKELPARAM(l AS WORD, h AS WORD) AS LONGINT STRICT
	RETURN (MakeLong(l, h))

FUNCTION PostAppMessage(idThread AS DWORD, wMsg AS DWORD, wParam AS DWORD,;
	lParam AS LONGINT) AS LOGIC STRICT
	RETURN PostThreadMessage(idThread, wMsg, wParam, lParam)




FUNCTION EnumTaskWindows (hTask AS DWORD, lpfn AS PTR, lParam AS LONGINT) AS LOGIC STRICT
	RETURN  EnumThreadWindows(hTask, lpfn, lParam)


FUNCTION DefHookProc( nCode AS INT, wParam AS DWORD, lParam AS LONGINT, phhk AS PTR) AS LONGINT  STRICT
	RETURN  CallNextHookEx(phhk, nCode, wParam, lParam)



#region defines
DEFINE RC_RT_CURSOR                := 1
DEFINE RC_RT_BITMAP                := 2
DEFINE RC_RT_ICON                  := 3
DEFINE RC_RT_MENU                  := 4
DEFINE RC_RT_DIALOG                := 5
DEFINE RC_RT_STRING                := 6
DEFINE RC_RT_FONTDIR               := 7
DEFINE RC_RT_FONT                  := 8
DEFINE RC_RT_ACCELERATOR           := 9
DEFINE RC_RT_RCDATA                := 10
DEFINE RC_RT_MESSAGETABLE          := 11
DEFINE RC_RT_GROUP_CURSOR          := 12
DEFINE RC_RT_GROUP_ICON            := 14
DEFINE RC_RT_VERSION               := 16
DEFINE RC_RT_DLGINCLUDE            := 17
DEFINE RC_RT_PLUGPLAY              := 19
DEFINE RC_RT_VXD                   := 20
DEFINE RC_RT_ANICURSOR             := 21
DEFINE RC_RT_ANIICON               := 22
DEFINE RC_RT_HTML                  := 23
DEFINE RC_RT_MANIFEST              := 24
DEFINE RT_CURSOR                := PTR (_CAST,1)
DEFINE RT_BITMAP                := PTR (_CAST,2)
DEFINE RT_ICON                  := PTR (_CAST,3)
DEFINE RT_MENU                  := PTR (_CAST,4)
DEFINE RT_DIALOG                := PTR (_CAST,5)
DEFINE RT_STRING                := PTR (_CAST,6)
DEFINE RT_FONTDIR               := PTR (_CAST,7)
DEFINE RT_FONT                  := PTR (_CAST,8)
DEFINE RT_ACCELERATOR           := PTR (_CAST,9)
DEFINE RT_RCDATA                := PTR (_CAST,10)
DEFINE RT_MESSAGETABLE          := PTR (_CAST,11)
DEFINE DIFFERENCE               := 11
DEFINE RT_GROUP_CURSOR          := PTR(_CAST, 12)
DEFINE RT_GROUP_ICON            := PTR(_CAST, 14)
DEFINE RT_VERSION               := PTR (_CAST,16)
DEFINE RT_DLGINCLUDE            := PTR (_CAST,17)
DEFINE RT_PLUGPLAY              := PTR (_CAST,19)
DEFINE RT_VXD                   := PTR (_CAST,20)
DEFINE RT_ANICURSOR             := PTR (_CAST,21)
DEFINE RT_ANIICON               := PTR (_CAST,22)
DEFINE RT_HTML                  := PTR (_CAST,23)
DEFINE RT_MANIFEST              := PTR (_CAST,24)
DEFINE CREATEPROCESS_MANIFEST_RESOURCE_ID := 1
DEFINE ISOLATIONAWARE_MANIFEST_RESOURCE_ID := 2
DEFINE ISOLATIONAWARE_NOSTATICIMPORT_MANIFEST_RESOURCE_ID := 3
DEFINE SB_HORZ                       := 0
DEFINE SB_VERT                       := 1
DEFINE SB_CTL                        := 2
DEFINE SB_BOTH                       := 3
/*
 * Scroll Bar Commands
 */
DEFINE SB_LINEUP                 := 0
DEFINE SB_LINELEFT               := 0
DEFINE SB_LINEDOWN               := 1
DEFINE SB_LINERIGHT              := 1
DEFINE SB_PAGEUP                 := 2
DEFINE SB_PAGELEFT               := 2
DEFINE SB_PAGEDOWN               := 3
DEFINE SB_PAGERIGHT              := 3
DEFINE SB_THUMBPOSITION              := 4
DEFINE SB_THUMBTRACK             := 5
DEFINE SB_TOP                        := 6
DEFINE SB_LEFT                       := 6
DEFINE SB_BOTTOM                     := 7
DEFINE SB_RIGHT                      := 7
DEFINE SB_ENDSCROLL              := 8
/*
 * ShowWindow() Commands
 */
DEFINE SW_HIDE                       := 0
DEFINE SW_SHOWNORMAL             := 1
DEFINE SW_NORMAL                     := 1
DEFINE SW_SHOWMINIMIZED      := 2
DEFINE SW_SHOWMAXIMIZED      := 3
DEFINE SW_MAXIMIZE               := 3
DEFINE SW_SHOWNOACTIVATE     := 4
DEFINE SW_SHOW                       := 5
DEFINE SW_MINIMIZE               := 6
DEFINE SW_SHOWMINNOACTIVE  := 7
DEFINE SW_SHOWNA                     := 8
DEFINE SW_RESTORE                := 9
DEFINE SW_SHOWDEFAULT        := 10
DEFINE SW_FORCEMINIMIZE    := 11
DEFINE SW_MAX              := 11
/*
 * Old ShowWindow() Commands
 */
DEFINE HIDE_WINDOW               := 0
DEFINE SHOW_OPENWINDOW       := 1
DEFINE SHOW_ICONWINDOW       := 2
DEFINE SHOW_FULLSCREEN       := 3
DEFINE SHOW_OPENNOACTIVATE := 4
/*
 * Identifiers for the WM_SHOWWINDOW message
 */
DEFINE SW_PARENTCLOSING      := 1
DEFINE SW_OTHERZOOM              := 2
DEFINE SW_PARENTOPENING      := 3
DEFINE SW_OTHERUNZOOM        := 4
/*
 * AnimateWindow() Commands
 */
DEFINE AW_HOR_POSITIVE           :=  0x00000001
DEFINE AW_HOR_NEGATIVE           :=  0x00000002
DEFINE AW_VER_POSITIVE           :=  0x00000004
DEFINE AW_VER_NEGATIVE           :=  0x00000008
DEFINE AW_CENTER                 :=  0x00000010
DEFINE AW_HIDE                   :=  0x00010000
DEFINE AW_ACTIVATE               :=  0x00020000
DEFINE AW_SLIDE                  :=  0x00040000
DEFINE AW_BLEND                  :=  0x00080000
/*
 * WM_KEYUP/DOWN/CHAR HIWORD(lParam) flags
 */
DEFINE KF_EXTENDED               := 0x0100
DEFINE KF_DLGMODE                := 0x0800
DEFINE KF_MENUMODE               := 0x1000
DEFINE KF_ALTDOWN                := 0x2000
DEFINE KF_REPEAT                     := 0x4000
DEFINE KF_UP                             := 0x8000
/*
 * Virtual Keys, Standard Set
 */
DEFINE VK_LBUTTON            := 0x01
DEFINE VK_RBUTTON            := 0x02
DEFINE VK_CANCEL                 := 0x03
DEFINE VK_MBUTTON            := 0x04    /* NOT contiguous with L & RBUTTON */
DEFINE VK_XBUTTON1       := 0x05    /* NOT contiguous with L & RBUTTON */
DEFINE VK_XBUTTON2       := 0x06    /* NOT contiguous with L & RBUTTON */
/*
 * 0x07 : unassigned
 */
DEFINE VK_BACK                   := 0x08
DEFINE VK_TAB                    := 0x09
/*
 * 0x0A - 0x0B : reserved
 */
DEFINE VK_CLEAR                  := 0x0C
DEFINE VK_RETURN                 := 0x0D
DEFINE VK_SHIFT                  := 0x10
DEFINE VK_CONTROL            := 0x11
DEFINE VK_MENU                   := 0x12
DEFINE VK_PAUSE                  := 0x13
DEFINE VK_CAPITAL            := 0x14
DEFINE VK_KANA           := 0x15
DEFINE VK_HANGEUL        := 0x15  /* old name - should be here for compatibility */
DEFINE VK_HANGUL         := 0x15
DEFINE VK_JUNJA          := 0x17
DEFINE VK_FINAL          := 0x18
DEFINE VK_HANJA          := 0x19
DEFINE VK_KANJI          := 0x19
DEFINE VK_ESCAPE                 := 0x1B
DEFINE VK_CONVERT        := 0x1C
DEFINE VK_NONCONVERT     := 0x1D
DEFINE VK_ACCEPT         := 0x1E
DEFINE VK_MODECHANGE     := 0x1F
DEFINE VK_SPACE                  := 0x20
DEFINE VK_PRIOR                  := 0x21
DEFINE VK_NEXT                   := 0x22
DEFINE VK_END                    := 0x23
DEFINE VK_HOME                   := 0x24
DEFINE VK_LEFT                   := 0x25
DEFINE VK_UP                         := 0x26
DEFINE VK_RIGHT                  := 0x27
DEFINE VK_DOWN                   := 0x28
DEFINE VK_SELECT                 := 0x29
DEFINE VK_PRINT                  := 0x2A
DEFINE VK_EXECUTE            := 0x2B
DEFINE VK_SNAPSHOT           := 0x2C
DEFINE VK_INSERT                 := 0x2D
DEFINE VK_DELETE                 := 0x2E
DEFINE VK_HELP                   := 0x2F
/*
 * VK_0 - VK_9 are the same as ASCII '0' - '9' (0x30 - 0x39)
 * 0x40 : unassigned
 * VK_A - VK_Z are the same as ASCII 'A' - 'Z' (0x41 - 0x5A)
 */
DEFINE VK_LWIN                   := 0x5B
DEFINE VK_RWIN                   := 0x5C
DEFINE VK_APPS                   := 0x5D
/*
 * 0x5E : reserved
 */
DEFINE VK_SLEEP          := 0x5F
DEFINE VK_NUMPAD0            := 0x60
DEFINE VK_NUMPAD1            := 0x61
DEFINE VK_NUMPAD2            := 0x62
DEFINE VK_NUMPAD3            := 0x63
DEFINE VK_NUMPAD4            := 0x64
DEFINE VK_NUMPAD5            := 0x65
DEFINE VK_NUMPAD6            := 0x66
DEFINE VK_NUMPAD7            := 0x67
DEFINE VK_NUMPAD8            := 0x68
DEFINE VK_NUMPAD9            := 0x69
DEFINE VK_MULTIPLY           := 0x6A
DEFINE VK_ADD                    := 0x6B
DEFINE VK_SEPARATOR          := 0x6C
DEFINE VK_SUBTRACT           := 0x6D
DEFINE VK_DECIMAL            := 0x6E
DEFINE VK_DIVIDE                 := 0x6F
DEFINE VK_F1                         := 0x70
DEFINE VK_F2                         := 0x71
DEFINE VK_F3                         := 0x72
DEFINE VK_F4                         := 0x73
DEFINE VK_F5                         := 0x74
DEFINE VK_F6                         := 0x75
DEFINE VK_F7                         := 0x76
DEFINE VK_F8                         := 0x77
DEFINE VK_F9                         := 0x78
DEFINE VK_F10                    := 0x79
DEFINE VK_F11                    := 0x7A
DEFINE VK_F12                    := 0x7B
DEFINE VK_F13                    := 0x7C
DEFINE VK_F14                    := 0x7D
DEFINE VK_F15                    := 0x7E
DEFINE VK_F16                    := 0x7F
DEFINE VK_F17                    := 0x80
DEFINE VK_F18                    := 0x81
DEFINE VK_F19                    := 0x82
DEFINE VK_F20                    := 0x83
DEFINE VK_F21                    := 0x84
DEFINE VK_F22                    := 0x85
DEFINE VK_F23                    := 0x86
DEFINE VK_F24                    := 0x87
/*
 * 0x88 - 0x8F : unassigned
 */
DEFINE VK_NUMLOCK            := 0x90
DEFINE VK_SCROLL                 := 0x91
DEFINE VK_OEM_SCROLL          := 0x91
/*
 * NEC PC-9800 kbd definitions
 */
DEFINE VK_OEM_NEC_EQUAL     := 0x92   // '=' key on numpad
/*
 * Fujitsu/OASYS kbd definitions
 */
DEFINE VK_OEM_FJ_JISHO   := 0x92   // 'Dictionary' key
DEFINE VK_OEM_FJ_MASSHOU := 0x93   // 'Unregister word' key
DEFINE VK_OEM_FJ_TOUROKU := 0x94   // 'Register word' key
DEFINE VK_OEM_FJ_LOYA    := 0x95   // 'Left OYAYUBI' key
DEFINE VK_OEM_FJ_ROYA    := 0x96   // 'Right OYAYUBI' key
/*
 * 0x97 - 0x9F : unassigned
 */
/*
 * VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
 * Used only as parameters to GetAsyncKeyState() and GetKeyState().
 * No other API or message will distinguish left and right keys in this way.
 */
DEFINE VK_LSHIFT                 := 0xA0
DEFINE VK_RSHIFT                 := 0xA1
DEFINE VK_LCONTROL           := 0xA2
DEFINE VK_RCONTROL           := 0xA3
DEFINE VK_LMENU                  := 0xA4
DEFINE VK_RMENU                  := 0xA5
DEFINE VK_BROWSER_BACK        := 0xA6
DEFINE VK_BROWSER_FORWARD     := 0xA7
DEFINE VK_BROWSER_REFRESH     := 0xA8
DEFINE VK_BROWSER_STOP        := 0xA9
DEFINE VK_BROWSER_SEARCH      := 0xAA
DEFINE VK_BROWSER_FAVORITES   := 0xAB
DEFINE VK_BROWSER_HOME        := 0xAC
DEFINE VK_VOLUME_MUTE         := 0xAD
DEFINE VK_VOLUME_DOWN         := 0xAE
DEFINE VK_VOLUME_UP           := 0xAF
DEFINE VK_MEDIA_NEXT_TRACK    := 0xB0
DEFINE VK_MEDIA_PREV_TRACK    := 0xB1
DEFINE VK_MEDIA_STOP          := 0xB2
DEFINE VK_MEDIA_PLAY_PAUSE    := 0xB3
DEFINE VK_LAUNCH_MAIL         := 0xB4
DEFINE VK_LAUNCH_MEDIA_SELECT := 0xB5
DEFINE VK_LAUNCH_APP1         := 0xB6
DEFINE VK_LAUNCH_APP2         := 0xB7
/*
 * 0xB8 - 0xB9 : reserved
 */
DEFINE VK_OEM_1          := 0xBA   // ';:' for US
DEFINE VK_OEM_PLUS       := 0xBB   // '+' any country
DEFINE VK_OEM_COMMA      := 0xBC   // ',' any country
DEFINE VK_OEM_MINUS      := 0xBD   // '-' any country
DEFINE VK_OEM_PERIOD     := 0xBE   // '.' any country
DEFINE VK_OEM_2          := 0xBF   // '/?' for US
DEFINE VK_OEM_3          := 0xC0 // O`~O for US
/*
 * 0xC1 - 0xD7 : reserved
 */
/*
 * 0xD8 - 0xDA : unassigned
 */
DEFINE VK_OEM_4          := 0xDB  //  '[{' for US
DEFINE VK_OEM_5          := 0xDC  //  '\|' for US
DEFINE VK_OEM_6          := 0xDD  //  ']}' for US
DEFINE VK_OEM_7          := 0xDE  //  ''"' for US
DEFINE VK_OEM_8          := 0xDF
/*
 * 0xE0 : reserved
 */
/*
 * Various extended or enhanced keyboards
 */
DEFINE VK_OEM_AX         := 0xE1  //  'AX' key on Japanese AX kbd
DEFINE VK_OEM_102        := 0xE2  //  "<>" or "\|" on RT 102-key kbd.
DEFINE VK_ICO_HELP       := 0xE3  //  Help key on ICO
DEFINE VK_ICO_00         := 0xE4  //  00 key on ICO
DEFINE VK_PROCESSKEY     := 0xE5
DEFINE VK_ICO_CLEAR      := 0xE6
DEFINE VK_PACKET         := 0xE7
/*
 * 0xE8 : unassigned
 */
/*
 * Nokia/Ericsson definitions
 */
DEFINE VK_OEM_RESET      := 0xE9
DEFINE VK_OEM_JUMP       := 0xEA
DEFINE VK_OEM_PA1        := 0xEB
DEFINE VK_OEM_PA2        := 0xEC
DEFINE VK_OEM_PA3        := 0xED
DEFINE VK_OEM_WSCTRL     := 0xEE
DEFINE VK_OEM_CUSEL      := 0xEF
DEFINE VK_OEM_ATTN       := 0xF0
DEFINE VK_OEM_FINISH     := 0xF1
DEFINE VK_OEM_COPY       := 0xF2
DEFINE VK_OEM_AUTO       := 0xF3
DEFINE VK_OEM_ENLW       := 0xF4
DEFINE VK_OEM_BACKTAB    := 0xF5
DEFINE VK_ATTN                   := 0xF6
DEFINE VK_CRSEL                  := 0xF7
DEFINE VK_EXSEL                  := 0xF8
DEFINE VK_EREOF                  := 0xF9
DEFINE VK_PLAY                   := 0xFA
DEFINE VK_ZOOM                   := 0xFB
DEFINE VK_NONAME                 := 0xFC
DEFINE VK_PA1                    := 0xFD
DEFINE VK_OEM_CLEAR          := 0xFE
/*
 * 0xFF : reserved
 */
/*
 * SetWindowsHook() codes
 */
DEFINE WH_MIN                        := (-1)
DEFINE WH_MSGFILTER              := (-1)
DEFINE WH_JOURNALRECORD      := 0
DEFINE WH_JOURNALPLAYBACK  := 1
DEFINE WH_KEYBOARD               := 2
DEFINE WH_GETMESSAGE             := 3
DEFINE WH_CALLWNDPROC        := 4
DEFINE WH_CBT                        := 5
DEFINE WH_SYSMSGFILTER       := 6
DEFINE WH_MOUSE                      := 7
DEFINE WH_HARDWARE               := 8
DEFINE WH_DEBUG                      := 9
DEFINE WH_SHELL                      := 10
DEFINE WH_FOREGROUNDIDLE     := 11
DEFINE WH_CALLWNDPROCRET     := 12
DEFINE WH_KEYBOARD_LL       := 13
DEFINE WH_MOUSE_LL          := 14
DEFINE WH_MAX                 := 14
DEFINE WH_MINHOOK               := WH_MIN
DEFINE WH_MAXHOOK               := WH_MAX
/*
 * Hook Codes
 */
DEFINE HC_ACTION                     := 0
DEFINE HC_GETNEXT                := 1
DEFINE HC_SKIP                       := 2
DEFINE HC_NOREMOVE               := 3
DEFINE HC_NOREM                      := HC_NOREMOVE
DEFINE HC_SYSMODALON             := 4
DEFINE HC_SYSMODALOFF        := 5
/*
 * CBT Hook Codes
 */
DEFINE HCBT_MOVESIZE             := 0
DEFINE HCBT_MINMAX               := 1
DEFINE HCBT_QS                       := 2
DEFINE HCBT_CREATEWND        := 3
DEFINE HCBT_DESTROYWND       := 4
DEFINE HCBT_ACTIVATE             := 5
DEFINE HCBT_CLICKSKIPPED     := 6
DEFINE HCBT_KEYSKIPPED       := 7
DEFINE HCBT_SYSCOMMAND       := 8
DEFINE HCBT_SETFOCUS             := 9
DEFINE MSGF_DIALOGBOX        := 0
DEFINE MSGF_MESSAGEBOX          := 1
DEFINE MSGF_MENU                        := 2
DEFINE MSGF_MOVE                        := 3
DEFINE MSGF_SIZE                        := 4
DEFINE MSGF_SCROLLBAR           := 5
DEFINE MSGF_NEXTWINDOW          := 6
DEFINE MSGF_MAINLOOP                := 8
DEFINE MSGF_MAX                         := 8
DEFINE MSGF_USER                        := 4096
/*
 * Shell support
 */
DEFINE HSHELL_WINDOWCREATED        := 1
DEFINE HSHELL_WINDOWDESTROYED      := 2
DEFINE HSHELL_ACTIVATESHELLWINDOW  := 3
DEFINE HSHELL_WINDOWACTIVATED      := 4
DEFINE HSHELL_GETMINRECT           := 5
DEFINE HSHELL_REDRAW               := 6
DEFINE HSHELL_TASKMAN              := 7
DEFINE HSHELL_LANGUAGE             := 8
DEFINE HSHELL_SYSMENU              := 9
DEFINE HSHELL_ENDTASK              := 10
DEFINE HSHELL_ACCESSIBILITYSTATE   := 11
DEFINE HSHELL_APPCOMMAND           := 12
DEFINE HSHELL_WINDOWREPLACED       := 13
DEFINE HSHELL_WINDOWREPLACING      := 14
DEFINE HSHELL_HIGHBIT                := 0x8000
DEFINE HSHELL_FLASH                := 0x8005 // (HSHELL_REDRAW|HSHELL_HIGHBIT)
DEFINE HSHELL_RUDEAPPACTIVATED     := 0x8004 // (HSHELL_WINDOWACTIVATED|HSHELL_HIGHBIT)
/* wparam for HSHELL_ACCESSIBILITYSTATE */
DEFINE    ACCESS_STICKYKEYS            := 0x0001
DEFINE    ACCESS_FILTERKEYS            := 0x0002
DEFINE    ACCESS_MOUSEKEYS             := 0x0003
/* cmd for HSHELL_APPCOMMAND and WM_APPCOMMAND */
DEFINE APPCOMMAND_BROWSER_BACKWARD := 1
DEFINE APPCOMMAND_BROWSER_FORWARD := 2
DEFINE APPCOMMAND_BROWSER_REFRESH := 3
DEFINE APPCOMMAND_BROWSER_STOP := 4
DEFINE APPCOMMAND_BROWSER_SEARCH := 5
DEFINE APPCOMMAND_BROWSER_FAVORITES := 6
DEFINE APPCOMMAND_BROWSER_HOME := 7
DEFINE APPCOMMAND_VOLUME_MUTE := 8
DEFINE APPCOMMAND_VOLUME_DOWN := 9
DEFINE APPCOMMAND_VOLUME_UP := 10
DEFINE APPCOMMAND_MEDIA_NEXTTRACK := 11
DEFINE APPCOMMAND_MEDIA_PREVIOUSTRACK := 12
DEFINE APPCOMMAND_MEDIA_STOP := 13
DEFINE APPCOMMAND_MEDIA_PLAY_PAUSE := 14
DEFINE APPCOMMAND_LAUNCH_MAIL := 15
DEFINE APPCOMMAND_LAUNCH_MEDIA_SELECT := 16
DEFINE APPCOMMAND_LAUNCH_APP1 := 17
DEFINE APPCOMMAND_LAUNCH_APP2 := 18
DEFINE APPCOMMAND_BASS_DOWN := 19
DEFINE APPCOMMAND_BASS_BOOST := 20
DEFINE APPCOMMAND_BASS_UP := 21
DEFINE APPCOMMAND_TREBLE_DOWN := 22
DEFINE APPCOMMAND_TREBLE_UP := 23
DEFINE APPCOMMAND_MICROPHONE_VOLUME_MUTE := 24
DEFINE APPCOMMAND_MICROPHONE_VOLUME_DOWN := 25
DEFINE APPCOMMAND_MICROPHONE_VOLUME_UP := 26
DEFINE APPCOMMAND_HELP := 27
DEFINE APPCOMMAND_FIND := 28
DEFINE APPCOMMAND_NEW := 29
DEFINE APPCOMMAND_OPEN := 30
DEFINE APPCOMMAND_CLOSE := 31
DEFINE APPCOMMAND_SAVE := 32
DEFINE APPCOMMAND_PRINT := 33
DEFINE APPCOMMAND_UNDO := 34
DEFINE APPCOMMAND_REDO := 35
DEFINE APPCOMMAND_COPY := 36
DEFINE APPCOMMAND_CUT := 37
DEFINE APPCOMMAND_PASTE := 38
DEFINE APPCOMMAND_REPLY_TO_MAIL := 39
DEFINE APPCOMMAND_FORWARD_MAIL := 40
DEFINE APPCOMMAND_SEND_MAIL := 41
DEFINE APPCOMMAND_SPELL_CHECK := 42
DEFINE APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE := 43
DEFINE APPCOMMAND_MIC_ON_OFF_TOGGLE := 44
DEFINE APPCOMMAND_CORRECTION_LIST := 45
DEFINE APPCOMMAND_MEDIA_CHANNEL_DOWN := 52
DEFINE APPCOMMAND_MEDIA_CHANNEL_UP := 51
DEFINE APPCOMMAND_MEDIA_FASTFORWARD := 49
DEFINE APPCOMMAND_MEDIA_PAUSE := 47
DEFINE APPCOMMAND_MEDIA_PLAY := 46
DEFINE APPCOMMAND_MEDIA_RECORD := 48
DEFINE APPCOMMAND_MEDIA_REWIND := 50
DEFINE FAPPCOMMAND_KEY := 0
DEFINE FAPPCOMMAND_MOUSE := 0x8000
DEFINE FAPPCOMMAND_OEM := 0x1000
DEFINE FAPPCOMMAND_MASK := 0xF000
DEFINE HKL_PREV                      := 0
DEFINE HKL_NEXT                      := 1
DEFINE KLF_ACTIVATE              := 0x00000001
DEFINE KLF_SUBSTITUTE_OK     := 0x00000002
DEFINE KLF_UNLOADPREVIOUS  := 0x00000004
DEFINE KLF_REORDER               := 0x00000008
DEFINE KLF_REPLACELANG       := 0x00000010
DEFINE KLF_NOTELLSHELL       := 0x00000080
DEFINE KLF_SETFORPROCESS   := 0x00000100
DEFINE KLF_SHIFTLOCK       := 0x00010000
DEFINE KLF_RESET           := 0x40000000
DEFINE KL_NAMELENGTH             := 9
DEFINE DESKTOP_READOBJECTS               := 0x0001L
DEFINE DESKTOP_CREATEWINDOW              := 0x0002L
DEFINE DESKTOP_CREATEMENU                := 0x0004L
DEFINE DESKTOP_HOOKCONTROL               := 0x0008L
DEFINE DESKTOP_JOURNALRECORD             := 0x0010L
DEFINE DESKTOP_JOURNALPLAYBACK       := 0x0020L
DEFINE DESKTOP_ENUMERATE                     := 0x0040L
DEFINE DESKTOP_WRITEOBJECTS              := 0x0080L
DEFINE DESKTOP_SWITCHDESKTOP             := 0x0100L
DEFINE DF_ALLOWOTHERACCOUNTHOOK      := 0x0001L
DEFINE WINSTA_ENUMDESKTOPS               := 0x0001L
DEFINE WINSTA_READATTRIBUTES             := 0x0002L
DEFINE WINSTA_ACCESSCLIPBOARD        := 0x0004L
DEFINE WINSTA_CREATEDESKTOP              := 0x0008L
DEFINE WINSTA_WRITEATTRIBUTES        := 0x0010L
DEFINE WINSTA_ACCESSGLOBALATOMS      := 0x0020L
DEFINE WINSTA_EXITWINDOWS                := 0x0040L
DEFINE WINSTA_ENUMERATE                      := 0x0100L
DEFINE WINSTA_READSCREEN                     := 0x0200L
DEFINE WSF_VISIBLE                               := 0x0001L
DEFINE UOI_FLAGS            :=  1
DEFINE UOI_NAME             :=  2
DEFINE UOI_TYPE             :=  3
DEFINE GWL_WNDPROC               := (-4)
DEFINE GWL_HINSTANCE             := (-6)
DEFINE GWL_HWNDPARENT        := (-8)
DEFINE GWL_STYLE                     := (-16)
DEFINE GWL_EXSTYLE               := (-20)
DEFINE GWL_USERDATA              := (-21)
DEFINE GWL_ID                        := (-12)
DEFINE GCL_MENUNAME              := (-8)
DEFINE GCL_HBRBACKGROUND     := (-10)
DEFINE GCL_HCURSOR               := (-12)
DEFINE GCL_HICON                     := (-14)
DEFINE GCL_HMODULE               := (-16)
DEFINE GCL_CBWNDEXTRA        := (-18)
DEFINE GCL_CBCLSEXTRA        := (-20)
DEFINE GCL_WNDPROC               := (-24)
DEFINE GCL_STYLE                     := (-26)
DEFINE GCW_ATOM                      := (-32)
DEFINE GCL_HICONSM               := (-34)
DEFINE WM_NULL                                               := 0x0000
DEFINE WM_CREATE                                             := 0x0001
DEFINE WM_DESTROY                                        := 0x0002
DEFINE WM_MOVE                                               := 0x0003
DEFINE WM_SIZE                                               := 0x0005
DEFINE WM_ACTIVATE                                       := 0x0006
/*
 * WM_ACTIVATE state values
 */
DEFINE       WA_INACTIVE         := 0
DEFINE       WA_ACTIVE           := 1
DEFINE       WA_CLICKACTIVE  := 2
DEFINE WM_SETFOCUS                                       := 0x0007
DEFINE WM_KILLFOCUS                                      := 0x0008
DEFINE WM_ENABLE                                             := 0x000A
DEFINE WM_SETREDRAW                                      := 0x000B
DEFINE WM_SETTEXT                                        := 0x000C
DEFINE WM_GETTEXT                                        := 0x000D
DEFINE WM_GETTEXTLENGTH                              := 0x000E
DEFINE WM_PAINT                                              := 0x000F
DEFINE WM_CLOSE                                              := 0x0010
DEFINE WM_QUERYENDSESSION                        := 0x0011
DEFINE WM_QUIT                                               := 0x0012
DEFINE WM_QUERYOPEN                                      := 0x0013
DEFINE WM_ERASEBKGND                                     := 0x0014
DEFINE WM_SYSCOLORCHANGE                             := 0x0015
DEFINE WM_ENDSESSION                                     := 0x0016
DEFINE WM_SYSTEMERROR                                           :=0x0017
DEFINE WM_SHOWWINDOW                                                := 0x0018
DEFINE WM_CTLCOLOR                                              :=0x0019
DEFINE WM_WININICHANGE                               := 0x001A
DEFINE WM_SETTINGCHANGE                              := WM_WININICHANGE
DEFINE WM_DEVMODECHANGE                              := 0x001B
DEFINE WM_ACTIVATEAPP                                := 0x001C
DEFINE WM_FONTCHANGE                                     := 0x001D
DEFINE WM_TIMECHANGE                                     := 0x001E
DEFINE WM_CANCELMODE                                     := 0x001F
DEFINE WM_SETCURSOR                                      := 0x0020
DEFINE WM_MOUSEACTIVATE                              := 0x0021
DEFINE WM_CHILDACTIVATE                              := 0x0022
DEFINE WM_QUEUESYNC                                      := 0x0023
DEFINE WM_GETMINMAXINFO                              := 0x0024
/*
 * Struct pointed to by WM_GETMINMAXINFO lParam
 */
DEFINE WM_PAINTICON                                      := 0x0026
DEFINE WM_ICONERASEBKGND                             := 0x0027
DEFINE WM_NEXTDLGCTL                                     := 0x0028
DEFINE WM_SPOOLERSTATUS                              := 0x002A
DEFINE WM_DRAWITEM                                       := 0x002B
DEFINE WM_MEASUREITEM                                := 0x002C
DEFINE WM_DELETEITEM                                     := 0x002D
DEFINE WM_VKEYTOITEM                                     := 0x002E
DEFINE WM_CHARTOITEM                                     := 0x002F
DEFINE WM_SETFONT                                        := 0x0030
DEFINE WM_GETFONT                                        := 0x0031
DEFINE WM_SETHOTKEY                                      := 0x0032
DEFINE WM_GETHOTKEY                                      := 0x0033
DEFINE WM_QUERYDRAGICON                              := 0x0037
DEFINE WM_COMPAREITEM                                := 0x0039
DEFINE WM_GETOBJECT                    := 0x003D
DEFINE WM_COMPACTING                                     := 0x0041
DEFINE WM_COMMNOTIFY                                     := 0x0044
DEFINE WM_WINDOWPOSCHANGING                      := 0x0046
DEFINE WM_WINDOWPOSCHANGED                       := 0x0047
DEFINE WM_POWER                                              := 0x0048
DEFINE PWR_OK                        := 1
DEFINE PWR_FAIL                      := (-1)
DEFINE PWR_SUSPENDREQUEST  := 1
DEFINE PWR_SUSPENDRESUME     := 2
DEFINE PWR_CRITICALRESUME  := 3
DEFINE WM_COPYDATA                                       := 0x004A
DEFINE WM_CANCELJOURNAL                              := 0x004B
DEFINE WM_NOTIFY                                             := 0x004E
DEFINE WM_INPUTLANGCHANGEREQUEST             := 0x0050
DEFINE WM_INPUTLANGCHANGE                        := 0x0051
DEFINE WM_TCARD                                              := 0x0052
DEFINE WM_HELP                                               := 0x0053
DEFINE WM_USERCHANGED                                := 0x0054
DEFINE WM_NOTIFYFORMAT                               := 0x0055
DEFINE NFR_ANSI                                                         := 1
DEFINE NFR_UNICODE                                                  := 2
DEFINE NF_QUERY                                                         := 3
DEFINE NF_REQUERY                                                   := 4
DEFINE WM_CONTEXTMENU                                := 0x007B
DEFINE WM_STYLECHANGING                              := 0x007C
DEFINE WM_STYLECHANGED                               := 0x007D
DEFINE WM_DISPLAYCHANGE                              := 0x007E
DEFINE WM_GETICON                                        := 0x007F
DEFINE WM_SETICON                                        := 0x0080
DEFINE WM_NCCREATE                                       := 0x0081
DEFINE WM_NCDESTROY                                      := 0x0082
DEFINE WM_NCCALCSIZE                                     := 0x0083
DEFINE WM_NCHITTEST                                      := 0x0084
DEFINE WM_NCPAINT                                        := 0x0085
DEFINE WM_NCACTIVATE                                     := 0x0086
DEFINE WM_GETDLGCODE                                     := 0x0087
DEFINE WM_SYNCPAINT                    := 0x0088
DEFINE WM_NCMOUSEMOVE                                := 0x00A0
DEFINE WM_NCLBUTTONDOWN                              := 0x00A1
DEFINE WM_NCLBUTTONUP                                := 0x00A2
DEFINE WM_NCLBUTTONDBLCLK                        := 0x00A3
DEFINE WM_NCRBUTTONDOWN                              := 0x00A4
DEFINE WM_NCRBUTTONUP                                := 0x00A5
DEFINE WM_NCRBUTTONDBLCLK                        := 0x00A6
DEFINE WM_NCMBUTTONDOWN                              := 0x00A7
DEFINE WM_NCMBUTTONUP                                := 0x00A8
DEFINE WM_NCMBUTTONDBLCLK                        := 0x00A9
DEFINE WM_NCXBUTTONDOWN                := 0x00AB
DEFINE WM_NCXBUTTONUP                  := 0x00AC
DEFINE WM_NCXBUTTONDBLCLK              := 0x00AD
DEFINE WM_INPUT                        := 0x00FF
DEFINE WM_KEYFIRST                                       := 0x0100
DEFINE WM_KEYDOWN                                        := 0x0100
DEFINE WM_KEYUP                                              := 0x0101
DEFINE WM_CHAR                                               := 0x0102
DEFINE WM_DEADCHAR                                       := 0x0103
DEFINE WM_SYSKEYDOWN                                     := 0x0104
DEFINE WM_SYSKEYUP                                       := 0x0105
DEFINE WM_SYSCHAR                                        := 0x0106
DEFINE WM_SYSDEADCHAR                                := 0x0107
DEFINE WM_UNICHAR                      := 0x0109
DEFINE WM_KEYLAST                      := 0x0109
DEFINE UNICODE_NOCHAR                  := 0xFFFF
DEFINE WM_IME_STARTCOMPOSITION               := 0x010D
DEFINE WM_IME_ENDCOMPOSITION                     := 0x010E
DEFINE WM_IME_COMPOSITION                        := 0x010F
DEFINE WM_IME_KEYLAST                                := 0x010F
DEFINE WM_INITDIALOG                                     := 0x0110
DEFINE WM_COMMAND                                        := 0x0111
DEFINE WM_SYSCOMMAND                                     := 0x0112
DEFINE WM_TIMER                                              := 0x0113
DEFINE WM_HSCROLL                                        := 0x0114
DEFINE WM_VSCROLL                                        := 0x0115
DEFINE WM_INITMENU                                       := 0x0116
DEFINE WM_INITMENUPOPUP                              := 0x0117
DEFINE WM_SYSTIMER                                              :=0x0118U
DEFINE WM_MENUSELECT                                     := 0x011F
DEFINE WM_MENUCHAR                                       := 0x0120
DEFINE WM_ENTERIDLE                                      := 0x0121
DEFINE WM_MENURBUTTONUP                := 0x0122
DEFINE WM_MENUDRAG                     := 0x0123
DEFINE WM_MENUGETOBJECT                := 0x0124
DEFINE WM_UNINITMENUPOPUP              := 0x0125
DEFINE WM_MENUCOMMAND                  := 0x0126
DEFINE WM_CHANGEUISTATE                := 0x0127
DEFINE WM_UPDATEUISTATE                := 0x0128
DEFINE WM_QUERYUISTATE                 := 0x0129
/*
 * LOWORD(wParam) values in WM_*UISTATE*
 */
DEFINE UIS_SET                         := 1
DEFINE UIS_CLEAR                       := 2
DEFINE UIS_INITIALIZE                  := 3
/*
 * HIWORD(wParam) values in WM_*UISTATE*
 */
DEFINE UISF_HIDEFOCUS                  := 0x1
DEFINE UISF_HIDEACCEL                  := 0x2
DEFINE UISF_ACTIVE                     := 0x4
DEFINE WM_CTLCOLORMSGBOX                             := 0x0132
DEFINE WM_CTLCOLOREDIT                               := 0x0133
DEFINE WM_CTLCOLORLISTBOX                        := 0x0134
DEFINE WM_CTLCOLORBTN                                := 0x0135
DEFINE WM_CTLCOLORDLG                                := 0x0136
DEFINE WM_CTLCOLORSCROLLBAR                      := 0x0137
DEFINE WM_CTLCOLORSTATIC                             := 0x0138
DEFINE MN_GETHMENU                     := 0x01E1
DEFINE WM_MOUSEFIRST                                     := 0x0200
DEFINE WM_MOUSEMOVE                                      := 0x0200
DEFINE WM_LBUTTONDOWN                                := 0x0201
DEFINE WM_LBUTTONUP                                      := 0x0202
DEFINE WM_LBUTTONDBLCLK                              := 0x0203
DEFINE WM_RBUTTONDOWN                                := 0x0204
DEFINE WM_RBUTTONUP                                      := 0x0205
DEFINE WM_RBUTTONDBLCLK                              := 0x0206
DEFINE WM_MBUTTONDOWN                                := 0x0207
DEFINE WM_MBUTTONUP                                      := 0x0208
DEFINE WM_MBUTTONDBLCLK                              := 0x0209
DEFINE WM_MOUSEWHEEL                := 0x020A
DEFINE WM_XBUTTONDOWN           := 0x020B
DEFINE WM_XBUTTONUP                 := 0x020C
DEFINE WM_XBUTTONDBLCLK         := 0x020D
DEFINE WM_MOUSELAST                   :=  0x020D
DEFINE WHEEL_DELTA                    :=  120
DEFINE WHEEL_PAGESCROLL               :=  (0xFFFFFFFF)
DEFINE WM_PARENTNOTIFY                               := 0x0210
DEFINE MENULOOP_WINDOW                               := 0
DEFINE MENULOOP_POPUP                                := 1
DEFINE WM_ENTERMENULOOP                              := 0x0211
DEFINE WM_EXITMENULOOP                               := 0x0212
DEFINE WM_NEXTMENU                                       := 0x0213
DEFINE WM_SIZING                                             := 0x0214
DEFINE WM_CAPTURECHANGED                             := 0x0215
DEFINE WM_MOVING                                             := 0x0216
DEFINE WM_POWERBROADCAST                             := 0x0218
DEFINE WM_DEVICECHANGE                               := 0x0219
DEFINE WM_MDICREATE                                      := 0x0220
DEFINE WM_MDIDESTROY                                     := 0x0221
DEFINE WM_MDIACTIVATE                                := 0x0222
DEFINE WM_MDIRESTORE                                     := 0x0223
DEFINE WM_MDINEXT                                        := 0x0224
DEFINE WM_MDIMAXIMIZE                                := 0x0225
DEFINE WM_MDITILE                                        := 0x0226
DEFINE WM_MDICASCADE                                     := 0x0227
DEFINE WM_MDIICONARRANGE                             := 0x0228
DEFINE WM_MDIGETACTIVE                               := 0x0229
DEFINE WM_MDISETMENU                                     := 0x0230
DEFINE WM_ENTERSIZEMOVE                              := 0x0231
DEFINE WM_EXITSIZEMOVE                               := 0x0232
DEFINE WM_DROPFILES                                      := 0x0233
DEFINE WM_MDIREFRESHMENU                             := 0x0234
DEFINE WM_IME_SETCONTEXT                             := 0x0281
DEFINE WM_IME_NOTIFY                                     := 0x0282
DEFINE WM_IME_CONTROL                                := 0x0283
DEFINE WM_IME_COMPOSITIONFULL                := 0x0284
DEFINE WM_IME_SELECT                                     := 0x0285
DEFINE WM_IME_CHAR                                       := 0x0286
DEFINE WM_IME_REQUEST                                       :=0x0288
DEFINE WM_IME_KEYDOWN                                := 0x0290
DEFINE WM_IME_KEYUP                                      := 0x0291
DEFINE WM_MOUSEHOVER                   := 0x02A1
DEFINE WM_MOUSELEAVE                   := 0x02A3
DEFINE WM_NCMOUSEHOVER                                      :=0x02A0
DEFINE WM_NCMOUSELEAVE                                      :=0x02A2
DEFINE WM_WTSSESSION_CHANGE                                 :=0x02B1
DEFINE WM_TABLET_FIRST                 := 0x02c0
DEFINE WM_TABLET_LAST                  := 0x02df
DEFINE WM_CUT                                                := 0x0300
DEFINE WM_COPY                                               := 0x0301
DEFINE WM_PASTE                                              := 0x0302
DEFINE WM_CLEAR                                              := 0x0303
DEFINE WM_UNDO                                               := 0x0304
DEFINE WM_RENDERFORMAT                               := 0x0305
DEFINE WM_RENDERALLFORMATS                       := 0x0306
DEFINE WM_DESTROYCLIPBOARD                       := 0x0307
DEFINE WM_DRAWCLIPBOARD                              := 0x0308
DEFINE WM_PAINTCLIPBOARD                             := 0x0309
DEFINE WM_VSCROLLCLIPBOARD                       := 0x030A
DEFINE WM_SIZECLIPBOARD                              := 0x030B
DEFINE WM_ASKCBFORMATNAME                        := 0x030C
DEFINE WM_CHANGECBCHAIN                              := 0x030D
DEFINE WM_HSCROLLCLIPBOARD                       := 0x030E
DEFINE WM_QUERYNEWPALETTE                        := 0x030F
DEFINE WM_PALETTEISCHANGING                      := 0x0310
DEFINE WM_PALETTECHANGED                             := 0x0311
DEFINE WM_HOTKEY                                             := 0x0312
DEFINE WM_PRINT                                              := 0x0317
DEFINE WM_PRINTCLIENT                                := 0x0318
DEFINE WM_APPCOMMAND := 0x0319
DEFINE WM_THEMECHANGED                :=  0x031A
DEFINE WM_HANDHELDFIRST                              := 0x0358
DEFINE WM_HANDHELDLAST                               := 0x035F
DEFINE WM_AFXFIRST                                       := 0x0360
DEFINE WM_AFXLAST                                        := 0x037F
DEFINE WM_PENWINFIRST                                := 0x0380
DEFINE WM_PENWINLAST                                     := 0x038F
DEFINE WM_APP                                                := 0x8000
// RvdH Added 030902 to avoid conflicts between messages used by the runtime
//      and messages used by VO users
DEFINE WM_VOAPP                          := WM_APP + 0x1000
DEFINE WM_USER                                               := 0x0400
DEFINE WMSZ_LEFT                     := 1
DEFINE WMSZ_RIGHT                := 2
DEFINE WMSZ_TOP                      := 3
DEFINE WMSZ_TOPLEFT              := 4
DEFINE WMSZ_TOPRIGHT             := 5
DEFINE WMSZ_BOTTOM               := 6
DEFINE WMSZ_BOTTOMLEFT       := 7
DEFINE WMSZ_BOTTOMRIGHT      := 8
DEFINE ST_BEGINSWP               := 0
DEFINE ST_ENDSWP                     := 1
DEFINE HTERROR                       := (-2)
DEFINE HTTRANSPARENT             := (-1)
DEFINE HTNOWHERE                     := 0
DEFINE HTCLIENT                      := 1
DEFINE HTCAPTION                     := 2
DEFINE HTSYSMENU                     := 3
DEFINE HTGROWBOX                     := 4
DEFINE HTSIZE                        := HTGROWBOX
DEFINE HTMENU                        := 5
DEFINE HTHSCROLL                     := 6
DEFINE HTVSCROLL                     := 7
DEFINE HTMINBUTTON               := 8
DEFINE HTMAXBUTTON               := 9
DEFINE HTLEFT                        := 10
DEFINE HTRIGHT                       := 11
DEFINE HTTOP                             := 12
DEFINE HTTOPLEFT                     := 13
DEFINE HTTOPRIGHT                := 14
DEFINE HTBOTTOM                      := 15
DEFINE HTBOTTOMLEFT              := 16
DEFINE HTBOTTOMRIGHT             := 17
DEFINE HTBORDER                      := 18
DEFINE HTREDUCE                      := HTMINBUTTON
DEFINE HTZOOM                        := HTMAXBUTTON
DEFINE HTSIZEFIRST               := HTLEFT
DEFINE HTSIZELAST                := HTBOTTOMRIGHT
DEFINE HTOBJECT                      := 19
DEFINE HTCLOSE                       := 20
DEFINE HTHELP                        := 21
DEFINE SMTO_NORMAL               := 0x0000
DEFINE SMTO_BLOCK                := 0x0001
DEFINE SMTO_ABORTIFHUNG      := 0x0002
DEFINE SMTO_NOTIMEOUTIFNOTHUNG := 0x0008
DEFINE MA_ACTIVATE               := 1
DEFINE MA_ACTIVATEANDEAT     := 2
DEFINE MA_NOACTIVATE             := 3
DEFINE MA_NOACTIVATEANDEAT := 4
/*
 * WM_SETICON / WM_GETICON Type Codes
 */
DEFINE ICON_SMALL := 0
DEFINE ICON_BIG   := 1
// and also 'new' WM_ like
DEFINE SIZE_RESTORED             := 0
DEFINE SIZE_MINIMIZED        := 1
DEFINE SIZE_MAXIMIZED        := 2
DEFINE SIZE_MAXSHOW              := 3
DEFINE SIZE_MAXHIDE              := 4
DEFINE SIZENORMAL                := SIZE_RESTORED
DEFINE SIZEICONIC                := SIZE_MINIMIZED
DEFINE SIZEFULLSCREEN        := SIZE_MAXIMIZED
DEFINE SIZEZOOMSHOW              := SIZE_MAXSHOW
DEFINE SIZEZOOMHIDE              := SIZE_MAXHIDE
DEFINE WVR_ALIGNTOP              := 0x0010
DEFINE WVR_ALIGNLEFT             := 0x0020
DEFINE WVR_ALIGNBOTTOM       := 0x0040
DEFINE WVR_ALIGNRIGHT        := 0x0080
DEFINE WVR_HREDRAW               := 0x0100
DEFINE WVR_VREDRAW               := 0x0200
DEFINE WVR_REDRAW                := 0x0300
DEFINE WVR_VALIDRECTS        := 0x0400
DEFINE MK_LBUTTON                := 0x0001
DEFINE MK_RBUTTON                := 0x0002
DEFINE MK_SHIFT                      := 0x0004
DEFINE MK_CONTROL                := 0x0008
DEFINE MK_MBUTTON                := 0x0010
DEFINE MK_XBUTTON1 := 0x0020
DEFINE MK_XBUTTON2 := 0x0040
/*
 * Window Styles
 */
DEFINE WS_OVERLAPPED             := 0x00000000L
DEFINE WS_POPUP               := 0x80000000L
DEFINE WS_CHILD               := 0x40000000L
DEFINE WS_MINIMIZE            := 0x20000000L
DEFINE WS_VISIBLE             := 0x10000000L
DEFINE WS_DISABLED            := 0x08000000L
DEFINE WS_CLIPSIBLINGS        := 0x04000000L
DEFINE WS_CLIPCHILDREN        := 0x02000000L
DEFINE WS_MAXIMIZE            := 0x01000000L
DEFINE WS_CAPTION             := 0x00C00000L
DEFINE WS_BORDER              := 0x00800000L
DEFINE WS_DLGFRAME            := 0x00400000L
DEFINE WS_VSCROLL             := 0x00200000L
DEFINE WS_HSCROLL             := 0x00100000L
DEFINE WS_SYSMENU             := 0x00080000L
DEFINE WS_THICKFRAME          := 0x00040000L
DEFINE WS_GROUP               := 0x00020000L
DEFINE WS_TABSTOP             := 0x00010000L
DEFINE WS_MINIMIZEBOX        := 0x00020000L
DEFINE WS_MAXIMIZEBOX        := 0x00010000L
DEFINE WS_TILED                      := WS_OVERLAPPED
DEFINE WS_ICONIC                     := WS_MINIMIZE
DEFINE WS_SIZEBOX                := WS_THICKFRAME
DEFINE WS_TILEDWINDOW        := WS_OVERLAPPEDWINDOW
DEFINE WS_OVERLAPPEDWINDOW := 0x00CF0000L
DEFINE WS_POPUPWINDOW        :=0X80880000L
DEFINE WS_CHILDWINDOW        := (WS_CHILD)
DEFINE WS_EX_DLGMODALFRAME  := 0x00000001L
DEFINE WS_EX_NOPARENTNOTIFY := 0x00000004L
DEFINE WS_EX_TOPMOST                := 0x00000008L
DEFINE WS_EX_ACCEPTFILES        := 0x00000010L
DEFINE WS_EX_TRANSPARENT        := 0x00000020L
DEFINE WS_EX_MDICHILD                := 0x00000040L
DEFINE WS_EX_TOOLWINDOW              := 0x00000080L
DEFINE WS_EX_WINDOWEDGE              := 0x00000100L
DEFINE WS_EX_CLIENTEDGE              := 0x00000200L
DEFINE WS_EX_CONTEXTHELP             := 0x00000400L
DEFINE WS_EX_RIGHT                       := 0x00001000L
DEFINE WS_EX_LEFT                        := 0x00000000L
DEFINE WS_EX_RTLREADING              := 0x00002000L
DEFINE WS_EX_LTRREADING              := 0x00000000L
DEFINE WS_EX_LEFTSCROLLBAR       := 0x00004000L
DEFINE WS_EX_RIGHTSCROLLBAR      := 0x00000000L
DEFINE WS_EX_CONTROLPARENT       := 0x00010000L
DEFINE WS_EX_STATICEDGE              := 0x00020000L
DEFINE WS_EX_APPWINDOW               := 0x00040000L
DEFINE WS_EX_OVERLAPPEDWINDOW := 0x00000300L
DEFINE WS_EX_PALETTEWINDOW      := 0x00000188L
DEFINE WS_EX_LAYERED           := 0x00080000
DEFINE WS_EX_NOINHERITLAYOUT   := 0x00100000L // Disable inheritence of mirroring by children
DEFINE WS_EX_LAYOUTRTL         := 0x00400000L // Right to left mirroring
/*
 * Class styles
 */
DEFINE CS_VREDRAW                := 0x0001
DEFINE CS_HREDRAW                := 0x0002
DEFINE CS_KEYCVTWINDOW       := 0x0004
DEFINE CS_DBLCLKS                := 0x0008
DEFINE CS_OWNDC                      := 0x0020
DEFINE CS_CLASSDC                := 0x0040
DEFINE CS_PARENTDC               := 0x0080
DEFINE CS_NOKEYCVT               := 0x0100
DEFINE CS_NOCLOSE                := 0x0200
DEFINE CS_SAVEBITS               := 0x0800
DEFINE CS_BYTEALIGNCLIENT  := 0x1000
DEFINE CS_BYTEALIGNWINDOW  := 0x2000
DEFINE CS_GLOBALCLASS        := 0x4000
DEFINE CS_IME                        := 0x00010000
DEFINE CS_DROPSHADOW      := 0x00020000
DEFINE PRF_CHECKVISIBLE      := 0x00000001L
DEFINE PRF_NONCLIENT             := 0x00000002L
DEFINE PRF_CLIENT                := 0x00000004L
DEFINE PRF_ERASEBKGND        := 0x00000008L
DEFINE PRF_CHILDREN              := 0x00000010L
DEFINE PRF_OWNED                     := 0x00000020L
DEFINE BDR_RAISEDOUTER := 0x0001
DEFINE BDR_SUNKENOUTER := 0x0002
DEFINE BDR_RAISEDINNER := 0x0004
DEFINE BDR_SUNKENINNER := 0x0008
DEFINE BDR_OUTER             := 0x0003
DEFINE BDR_INNER             := 0x000c
DEFINE BDR_RAISED        := 0x0005
DEFINE BDR_SUNKEN        := 0x000a
DEFINE EDGE_RAISED      := 0x0005
DEFINE EDGE_SUNKEN      := 0x000A
DEFINE EDGE_ETCHED      := 0x0006
DEFINE EDGE_BUMP            := 0x0009
DEFINE BF_LEFT               := 0x0001
DEFINE BF_TOP                := 0x0002
DEFINE BF_RIGHT              := 0x0004
DEFINE BF_BOTTOM             := 0x0008
DEFINE BF_TOPLEFT        := 0x0003
DEFINE BF_TOPRIGHT       := 0x0006
DEFINE BF_BOTTOMLEFT     := 0x0009
DEFINE BF_BOTTOMRIGHT  := 0x000C
DEFINE BF_RECT               := 0x000F
DEFINE BF_DIAGONAL       := 0x0010
DEFINE BF_DIAGONAL_ENDTOPRIGHT       := 0x0016
DEFINE BF_DIAGONAL_ENDTOPLEFT        := 0x0013
DEFINE BF_DIAGONAL_ENDBOTTOMLEFT     := 0x0019
DEFINE BF_DIAGONAL_ENDBOTTOMRIGHT  := 0x001C
DEFINE BF_MIDDLE             := 0x0800
DEFINE BF_SOFT               := 0x1000
DEFINE BF_ADJUST             := 0x2000
DEFINE BF_FLAT               := 0x4000
DEFINE BF_MONO               := 0x8000
DEFINE DFC_CAPTION                       := 1
DEFINE DFC_MENU                              := 2
DEFINE DFC_SCROLL                        := 3
DEFINE DFC_BUTTON                        := 4
DEFINE DFC_POPUPMENU           := 5
DEFINE DFCS_CAPTIONCLOSE             := 0x0000
DEFINE DFCS_CAPTIONMIN               := 0x0001
DEFINE DFCS_CAPTIONMAX               := 0x0002
DEFINE DFCS_CAPTIONRESTORE       := 0x0003
DEFINE DFCS_CAPTIONHELP              := 0x0004
DEFINE DFCS_MENUARROW                := 0x0000
DEFINE DFCS_MENUCHECK                := 0x0001
DEFINE DFCS_MENUBULLET               := 0x0002
DEFINE DFCS_MENUARROWRIGHT       := 0x0004
DEFINE DFCS_SCROLLUP                     := 0x0000
DEFINE DFCS_SCROLLDOWN               := 0x0001
DEFINE DFCS_SCROLLLEFT               := 0x0002
DEFINE DFCS_SCROLLRIGHT              := 0x0003
DEFINE DFCS_SCROLLCOMBOBOX       := 0x0005
DEFINE DFCS_SCROLLSIZEGRIP       := 0x0008
DEFINE DFCS_SCROLLSIZEGRIPRIGHT := 0x0010
DEFINE DFCS_BUTTONCHECK              := 0x0000
DEFINE DFCS_BUTTONRADIOIMAGE     := 0x0001
DEFINE DFCS_BUTTONRADIOMASK      := 0x0002
DEFINE DFCS_BUTTONRADIO              := 0x0004
DEFINE DFCS_BUTTON3STATE             := 0x0008
DEFINE DFCS_BUTTONPUSH               := 0x0010
DEFINE DFCS_INACTIVE                     := 0x0100
DEFINE DFCS_PUSHED                       := 0x0200
DEFINE DFCS_CHECKED                      := 0x0400
DEFINE DFCS_TRANSPARENT        := 0x0800
DEFINE DFCS_HOT                := 0x1000
DEFINE DFCS_ADJUSTRECT               := 0x2000
DEFINE DFCS_FLAT                             := 0x4000
DEFINE DFCS_MONO                             := 0x8000
DEFINE DC_ACTIVE                     := 0x0001
DEFINE DC_SMALLCAP               := 0x0002
DEFINE DC_ICON                       := 0x0004
DEFINE DC_TEXT                       := 0x0008
DEFINE DC_INBUTTON                   := 0x0010
// for DrawCaption():
DEFINE DC_GRADIENT             := 0x0020
DEFINE DC_BUTTONS          := 0x1000
DEFINE IDANI_OPEN                := 1
DEFINE IDANI_CLOSE               := 2
DEFINE IDANI_CAPTION             := 3
DEFINE CF_TEXT                       := 1
DEFINE CF_BITMAP                     := 2
DEFINE CF_METAFILEPICT       := 3
DEFINE CF_SYLK                       := 4
DEFINE CF_DIF                        := 5
DEFINE CF_TIFF                       := 6
DEFINE CF_OEMTEXT                := 7
DEFINE CF_DIB                        := 8
DEFINE CF_PALETTE                := 9
DEFINE CF_PENDATA                := 10
DEFINE CF_RIFF                       := 11
DEFINE CF_WAVE                       := 12
DEFINE CF_UNICODETEXT        := 13
DEFINE CF_ENHMETAFILE        := 14
DEFINE CF_HDROP                      := 15
DEFINE CF_LOCALE                     := 16
DEFINE CF_DIBV5           :=  17
DEFINE CF_MAX                        := 17
DEFINE CF_OWNERDISPLAY       := 0x0080
DEFINE CF_DSPTEXT                := 0x0081
DEFINE CF_DSPBITMAP              := 0x0082
DEFINE CF_DSPMETAFILEPICT  := 0x0083
DEFINE CF_DSPENHMETAFILE     := 0x008E
DEFINE CF_PRIVATEFIRST       := 0x0200
DEFINE CF_PRIVATELAST        := 0x02FF
DEFINE CF_GDIOBJFIRST        := 0x0300
DEFINE CF_GDIOBJLAST             := 0x03FF
DEFINE FVIRTKEY  := TRUE
DEFINE FNOINVERT := 0x02
DEFINE FSHIFT    := 0x04
DEFINE FCONTROL  := 0x08
DEFINE FALT          := 0x10
DEFINE WPF_SETMINPOSITION        := 0x0001
DEFINE WPF_RESTORETOMAXIMIZED  := 0x0002
DEFINE ODT_MENU              := 1
DEFINE ODT_LISTBOX       := 2
DEFINE ODT_COMBOBOX      := 3
DEFINE ODT_BUTTON        := 4
DEFINE ODT_STATIC        := 5
DEFINE ODA_DRAWENTIRE  := 0x0001
DEFINE ODA_SELECT        := 0x0002
DEFINE ODA_FOCUS             := 0x0004
DEFINE ODS_SELECTED      := 0x0001
DEFINE ODS_GRAYED        := 0x0002
DEFINE ODS_DISABLED      := 0x0004
DEFINE ODS_CHECKED       := 0x0008
DEFINE ODS_FOCUS             := 0x0010
DEFINE ODS_DEFAULT               := 0x0020
DEFINE ODS_COMBOBOXEDIT      := 0x1000
DEFINE ODS_HOTLIGHT        := 0x0040
DEFINE ODS_INACTIVE        := 0x0080
DEFINE ODS_NOACCEL         := 0x0100
DEFINE ODS_NOFOCUSRECT     := 0x0200
/*
 * MEASUREITEMSTRUCT for ownerdraw
 */
DEFINE PM_NOREMOVE               := 0x0000
DEFINE PM_REMOVE                     := 0x0001
DEFINE PM_NOYIELD                := 0x0002
DEFINE MOD_ALT               := 0x0001
DEFINE MOD_CONTROL       := 0x0002
DEFINE MOD_SHIFT             := 0x0004
DEFINE MOD_WIN               := 0x0008
DEFINE IDHOT_SNAPWINDOW              :=(-1)
DEFINE IDHOT_SNAPDESKTOP                :=(-2)
DEFINE EW_RESTARTWINDOWS        := 0x0042L
DEFINE EW_REBOOTSYSTEM          := 0x0043L
DEFINE EW_EXITANDEXECAPP        := 0x0044L
DEFINE EWX_LOGOFF    := 0
DEFINE EWX_SHUTDOWN  := 1
DEFINE EWX_REBOOT    := 2
DEFINE EWX_FORCE         := 4
DEFINE EWX_POWEROFF  := 8
DEFINE EWX_FORCEIFHUNG     := 0x00000010
DEFINE BSM_ALLCOMPONENTS             := 0x00000000
DEFINE BSM_VXDS                              := 0x00000001
DEFINE BSM_NETDRIVER                     := 0x00000002
DEFINE BSM_INSTALLABLEDRIVERS  := 0x00000004
DEFINE BSM_APPLICATIONS              := 0x00000008
DEFINE BSM_ALLDESKTOPS        := 0x00000010
DEFINE BSF_QUERY                             := 0x00000001
DEFINE BSF_IGNORECURRENTTASK     := 0x00000002
DEFINE BSF_FLUSHDISK                     := 0x00000004
DEFINE BSF_NOHANG                        := 0x00000008
DEFINE BSF_POSTMESSAGE               := 0x00000010
DEFINE BSF_FORCEIFHUNG               := 0x00000020
DEFINE BSF_NOTIMEOUTIFNOTHUNG  := 0x00000040
DEFINE BSF_ALLOWSFW          :=   0x00000080
DEFINE BSF_SENDNOTIFYMESSAGE  :=  0x00000100
DEFINE BSF_RETURNHDESK       :=   0x00000200
DEFINE BSF_LUID              :=   0x00000400
DEFINE DBWF_LPARAMPOINTER  := 0x8000
DEFINE BROADCAST_QUERY_DENY                 := 0x424D5144
DEFINE HWND_BROADCAST  := PTR(_CAST, 0xffff)
DEFINE CW_USEDEFAULT             := INT(_CAST,0x80000000)
DEFINE HWND_DESKTOP              := PTR(_CAST, 0)
DEFINE SWP_NOSIZE                := 0x0001
DEFINE SWP_NOMOVE                := 0x0002
DEFINE SWP_NOZORDER              := 0x0004
DEFINE SWP_NOREDRAW              := 0x0008
DEFINE SWP_NOACTIVATE        := 0x0010
DEFINE SWP_FRAMECHANGED      := 0x0020
DEFINE SWP_SHOWWINDOW        := 0x0040
DEFINE SWP_HIDEWINDOW        := 0x0080
DEFINE SWP_NOCOPYBITS        := 0x0100
DEFINE SWP_NOOWNERZORDER     := 0x0200
DEFINE SWP_NOSENDCHANGING  := 0x0400
DEFINE SWP_DRAWFRAME             := SWP_FRAMECHANGED
DEFINE SWP_NOREPOSITION      := SWP_NOOWNERZORDER
DEFINE SWP_DEFERERASE        := 0x2000
DEFINE SWP_ASYNCWINDOWPOS  := 0x4000
DEFINE HWND_TOP             := PTR(_CAST, 0)
DEFINE HWND_BOTTOM          := PTR(_CAST, 1)
DEFINE HWND_TOPMOST         := PTR(_CAST, 0xFFFFFFFF)
DEFINE HWND_NOTOPMOST       := PTR(_CAST, 0xFFFFFFFE)
DEFINE DLGWINDOWEXTRA := 30
DEFINE KEYEVENTF_EXTENDEDKEY     :=  0x0001
DEFINE KEYEVENTF_KEYUP               :=  0x0002
DEFINE MOUSEEVENTF_MOVE              := 0x0001
DEFINE MOUSEEVENTF_LEFTDOWN      := 0x0002
DEFINE MOUSEEVENTF_LEFTUP        := 0x0004
DEFINE MOUSEEVENTF_RIGHTDOWN     := 0x0008
DEFINE MOUSEEVENTF_RIGHTUP       := 0x0010
DEFINE MOUSEEVENTF_MIDDLEDOWN  := 0x0020
DEFINE MOUSEEVENTF_MIDDLEUP      := 0x0040
DEFINE MOUSEEVENTF_XDOWN       :=0x0080 /* x button down */
DEFINE MOUSEEVENTF_XUP         :=0x0100 /* x button down */
DEFINE MOUSEEVENTF_WHEEL       :=0x0800 /* wheel button rolled */
DEFINE MOUSEEVENTF_VIRTUALDESK :=0x4000 /* map to entire virtual desktop */
DEFINE MOUSEEVENTF_ABSOLUTE      := 0x8000
DEFINE QS_KEY                        := 0x0001
DEFINE QS_MOUSEMOVE              := 0x0002
DEFINE QS_MOUSEBUTTON        := 0x0004
DEFINE QS_POSTMESSAGE        := 0x0008
DEFINE QS_TIMER                      := 0x0010
DEFINE QS_PAINT                      := 0x0020
DEFINE QS_SENDMESSAGE        := 0x0040
DEFINE QS_HOTKEY                     := 0x0080
DEFINE QS_ALLPOSTMESSAGE   :=0x0100
DEFINE QS_RAWINPUT         :=0x0400
DEFINE QS_MOUSE                      := 0x0006
DEFINE QS_INPUT                      := 0x0007
DEFINE QS_ALLEVENTS              := 0x00BF
DEFINE QS_ALLINPUT               := 0x00FF
DEFINE USER_TIMER_MAXIMUM  :=0x7FFFFFFF
DEFINE USER_TIMER_MINIMUM  :=0x0000000A
DEFINE SM_CXSCREEN                       := 0
DEFINE SM_CYSCREEN                       := 1
DEFINE SM_CXVSCROLL                      := 2
DEFINE SM_CYHSCROLL                      := 3
DEFINE SM_CYCAPTION                      := 4
DEFINE SM_CXBORDER                       := 5
DEFINE SM_CYBORDER                       := 6
DEFINE SM_CXDLGFRAME                     := 7
DEFINE SM_CYDLGFRAME                     := 8
DEFINE SM_CYVTHUMB                       := 9
DEFINE SM_CXHTHUMB                       := 10
DEFINE SM_CXICON                             := 11
DEFINE SM_CYICON                             := 12
DEFINE SM_CXCURSOR                       := 13
DEFINE SM_CYCURSOR                       := 14
DEFINE SM_CYMENU                             := 15
DEFINE SM_CXFULLSCREEN               := 16
DEFINE SM_CYFULLSCREEN               := 17
DEFINE SM_CYKANJIWINDOW              := 18
DEFINE SM_MOUSEPRESENT               := 19
DEFINE SM_CYVSCROLL                      := 20
DEFINE SM_CXHSCROLL                      := 21
DEFINE SM_DEBUG                              := 22
DEFINE SM_SWAPBUTTON                     := 23
DEFINE SM_RESERVED1                      := 24
DEFINE SM_RESERVED2                      := 25
DEFINE SM_RESERVED3                      := 26
DEFINE SM_RESERVED4                      := 27
DEFINE SM_CXMIN                              := 28
DEFINE SM_CYMIN                              := 29
DEFINE SM_CXSIZE                             := 30
DEFINE SM_CYSIZE                             := 31
DEFINE SM_CXFRAME                        := 32
DEFINE SM_CYFRAME                        := 33
DEFINE SM_CXMINTRACK                     := 34
DEFINE SM_CYMINTRACK                     := 35
DEFINE SM_CXDOUBLECLK                := 36
DEFINE SM_CYDOUBLECLK                := 37
DEFINE SM_CXICONSPACING              := 38
DEFINE SM_CYICONSPACING              := 39
DEFINE SM_MENUDROPALIGNMENT      := 40
DEFINE SM_PENWINDOWS                     := 41
DEFINE SM_DBCSENABLED                := 42
DEFINE SM_CMOUSEBUTTONS              := 43
DEFINE SM_CXFIXEDFRAME                   :=SM_CXDLGFRAME
DEFINE SM_CYFIXEDFRAME                   := SM_CYDLGFRAME
DEFINE SM_CXSIZEFRAME                    :=SM_CXFRAME
DEFINE SM_CYSIZEFRAME                    :=SM_CYFRAME
DEFINE SM_SECURE                             := 44
DEFINE SM_CXEDGE                             := 45
DEFINE SM_CYEDGE                             := 46
DEFINE SM_CXMINSPACING               := 47
DEFINE SM_CYMINSPACING               := 48
DEFINE SM_CXSMICON                       := 49
DEFINE SM_CYSMICON                       := 50
DEFINE SM_CYSMCAPTION                := 51
DEFINE SM_CXSMSIZE                       := 52
DEFINE SM_CYSMSIZE                       := 53
DEFINE SM_CXMENUSIZE                     := 54
DEFINE SM_CYMENUSIZE                     := 55
DEFINE SM_ARRANGE                        := 56
DEFINE SM_CXMINIMIZED                := 57
DEFINE SM_CYMINIMIZED                := 58
DEFINE SM_CXMAXTRACK                     := 59
DEFINE SM_CYMAXTRACK                     := 60
DEFINE SM_CXMAXIMIZED                := 61
DEFINE SM_CYMAXIMIZED                := 62
DEFINE SM_NETWORK                        := 63
DEFINE SM_CLEANBOOT                      := 67
DEFINE SM_CXDRAG                             := 68
DEFINE SM_CYDRAG                             := 69
DEFINE SM_SHOWSOUNDS                     := 70
DEFINE SM_CXMENUCHECK                := 71
DEFINE SM_CYMENUCHECK                := 72
DEFINE SM_SLOWMACHINE                := 73
DEFINE SM_MIDEASTENABLED             := 74
DEFINE SM_MOUSEWHEELPRESENT    :=75
DEFINE SM_XVIRTUALSCREEN       :=76
DEFINE SM_YVIRTUALSCREEN       :=77
DEFINE SM_CXVIRTUALSCREEN      :=78
DEFINE SM_CYVIRTUALSCREEN      :=79
DEFINE SM_CMONITORS            :=80
DEFINE SM_SAMEDISPLAYFORMAT    :=81
DEFINE SM_IMMENABLED           :=82
DEFINE SM_CXFOCUSBORDER        :=83
DEFINE SM_CYFOCUSBORDER        :=84
DEFINE SM_TABLETPC             :=86
DEFINE SM_MEDIACENTER          :=87
DEFINE SM_STARTER              :=88
DEFINE SM_SERVERR2             :=89
DEFINE SM_REMOTESESSION        :=0x1000
DEFINE SM_SHUTTINGDOWN         :=0x2000
DEFINE SM_REMOTECONTROL        :=0x2001
DEFINE SM_CARETBLINKINGENABLED :=0x2002
DEFINE MNC_IGNORE  := 0
DEFINE MNC_CLOSE     := 1
DEFINE MNC_EXECUTE := 2
DEFINE MNC_SELECT  := 3
DEFINE MIIM_STATE           := 0x00000001
DEFINE MIIM_ID                  := 0x00000002
DEFINE MIIM_SUBMENU         := 0x00000004
DEFINE MIIM_CHECKMARKS      := 0x00000008
DEFINE MIIM_TYPE                := 0x00000010
DEFINE MIIM_DATA                := 0x00000020
DEFINE GMDI_USEDISABLED      := 0x0001L
DEFINE GMDI_GOINTOPOPUPS     := 0x0002L
DEFINE TPM_LEFTBUTTON  := 0x0000L
DEFINE TPM_RIGHTBUTTON := 0x0002L
DEFINE TPM_LEFTALIGN     := 0x0000L
DEFINE TPM_CENTERALIGN := 0x0004L
DEFINE TPM_RIGHTALIGN  := 0x0008L
DEFINE TPM_TOPALIGN              := 0x0000L
DEFINE TPM_VCENTERALIGN      := 0x0010L
DEFINE TPM_BOTTOMALIGN       := 0x0020L
DEFINE TPM_HORIZONTAL        := 0x0000L
DEFINE TPM_VERTICAL              := 0x0040L
DEFINE TPM_NONOTIFY              := 0x0080L
DEFINE TPM_RETURNCMD             := 0x0100L
DEFINE DOF_EXECUTABLE        := 0x8001
DEFINE DOF_DOCUMENT              := 0x8002
DEFINE DOF_DIRECTORY             := 0x8003
DEFINE DOF_MULTIPLE              := 0x8004
DEFINE DOF_PROGMAN               := 0x0001
DEFINE DOF_SHELLDATA             := 0x0002
DEFINE DO_DROPFILE               := 0x454C4946L
DEFINE DO_PRINTFILE              := 0x544E5250L
DEFINE DT_TOP                        := 0x00000000
DEFINE DT_LEFT                       := 0x00000000
DEFINE DT_CENTER                     := 0x00000001
DEFINE DT_RIGHT                      := 0x00000002
DEFINE DT_VCENTER                := 0x00000004
DEFINE DT_BOTTOM                     := 0x00000008
DEFINE DT_WORDBREAK              := 0x00000010
DEFINE DT_SINGLELINE             := 0x00000020
DEFINE DT_EXPANDTABS             := 0x00000040
DEFINE DT_TABSTOP                := 0x00000080
DEFINE DT_NOCLIP                     := 0x00000100
DEFINE DT_EXTERNALLEADING  := 0x00000200
DEFINE DT_CALCRECT               := 0x00000400
DEFINE DT_NOPREFIX               := 0x00000800
DEFINE DT_INTERNAL               := 0x00001000
DEFINE DT_EDITCONTROL        := 0x00002000
DEFINE DT_PATH_ELLIPSIS      := 0x00004000
DEFINE DT_END_ELLIPSIS       := 0x00008000
DEFINE DT_MODIFYSTRING       := 0x00010000
DEFINE DT_RTLREADING             := 0x00020000
DEFINE DT_WORD_ELLIPSIS      := 0x00040000
DEFINE DST_COMPLEX       := 0x0000
DEFINE DST_TEXT              := 0x0001
DEFINE DST_PREFIXTEXT  := 0x0002
DEFINE DST_ICON              := 0x0003
DEFINE DST_BITMAP        := 0x0004
DEFINE DSS_NORMAL        := 0x0000
DEFINE DSS_UNION             := 0x0010
DEFINE DSS_DISABLED      := 0x0020
DEFINE DSS_MONO              := 0x0080
DEFINE DSS_RIGHT             := 0x8000
DEFINE DCX_WINDOW                   := 0x00000001L
DEFINE DCX_CACHE                        := 0x00000002L
DEFINE DCX_NORESETATTRS         := 0x00000004L
DEFINE DCX_CLIPCHILDREN         := 0x00000008L
DEFINE DCX_CLIPSIBLINGS         := 0x00000010L
DEFINE DCX_PARENTCLIP           := 0x00000020L
DEFINE DCX_EXCLUDERGN           := 0x00000040L
DEFINE DCX_INTERSECTRGN         := 0x00000080L
DEFINE DCX_EXCLUDEUPDATE        := 0x00000100L
DEFINE DCX_INTERSECTUPDATE  := 0x00000200L
DEFINE DCX_LOCKWINDOWUPDATE := 0x00000400L
DEFINE DCX_VALIDATE                 := 0x00200000L
DEFINE RDW_INVALIDATE                := 0x0001
DEFINE RDW_INTERNALPAINT             := 0x0002
DEFINE RDW_ERASE                             := 0x0004
DEFINE RDW_VALIDATE                      := 0x0008
DEFINE RDW_NOINTERNALPAINT       := 0x0010
DEFINE RDW_NOERASE                       := 0x0020
DEFINE RDW_NOCHILDREN                := 0x0040
DEFINE RDW_ALLCHILDREN               := 0x0080
DEFINE RDW_UPDATENOW                     := 0x0100
DEFINE RDW_ERASENOW                      := 0x0200
DEFINE RDW_FRAME                             := 0x0400
DEFINE RDW_NOFRAME                       := 0x0800
DEFINE SW_SCROLLCHILDREN     := 0x0001
DEFINE SW_INVALIDATE             := 0x0002
DEFINE SW_ERASE                      := 0x0004
DEFINE ESB_ENABLE_BOTH       := 0x0000
DEFINE ESB_DISABLE_BOTH      := 0x0003
DEFINE ESB_DISABLE_LEFT      := 0x0001
DEFINE ESB_DISABLE_RIGHT     := 0x0002
DEFINE ESB_DISABLE_UP        := 0x0001
DEFINE ESB_DISABLE_DOWN      := 0x0002
DEFINE ESB_DISABLE_LTUP      := ESB_DISABLE_LEFT
DEFINE ESB_DISABLE_RTDN      := ESB_DISABLE_RIGHT
DEFINE HELPINFO_WINDOW      := 0x0001
DEFINE HELPINFO_MENUITEM    := 0x0002
DEFINE MB_OK                  := 0x00000000U
DEFINE MB_OKCANCEL            := 0x00000001U
DEFINE MB_ABORTRETRYIGNORE    := 0x00000002U
DEFINE MB_YESNOCANCEL         := 0x00000003U
DEFINE MB_YESNO               := 0x00000004U
DEFINE MB_RETRYCANCEL         := 0x00000005U
DEFINE MB_CANCELTRYCONTINUE   :=0x00000006U
DEFINE MB_ICONHAND            := 0x00000010U
DEFINE MB_ICONQUESTION        := 0x00000020U
DEFINE MB_ICONEXCLAMATION     := 0x00000030U
DEFINE MB_ICONASTERISK        := 0x00000040U
DEFINE MB_USERICON            := 0x00000080U
DEFINE MB_ICONWARNING         := MB_ICONEXCLAMATION
DEFINE MB_ICONERROR           := MB_ICONHAND
DEFINE MB_ICONINFORMATION     := MB_ICONASTERISK
DEFINE MB_ICONSTOP            := MB_ICONHAND
DEFINE MB_DEFBUTTON1          := 0x00000000U
DEFINE MB_DEFBUTTON2          := 0x00000100U
DEFINE MB_DEFBUTTON3          := 0x00000200U
DEFINE MB_DEFBUTTON4          := 0x00000300U
DEFINE MB_APPLMODAL           := 0x00000000U
DEFINE MB_SYSTEMMODAL         := 0x00001000U
DEFINE MB_TASKMODAL           := 0x00002000U
DEFINE MB_HELP                := 0x00004000U
DEFINE MB_NOFOCUS             := 0x00008000U
DEFINE MB_SETFOREGROUND       := 0x00010000U
DEFINE MB_DEFAULT_DESKTOP_ONLY:= 0x00020000U
DEFINE MB_TOPMOST             := 0x00040000U
DEFINE MB_RIGHT               := 0x00080000U
DEFINE MB_RTLREADING          := 0x00100000U
DEFINE MB_SERVICE_NOTIFICATION:= 0x00040000U
DEFINE MB_TYPEMASK             := 0x0000000FU
DEFINE MB_ICONMASK             := 0x000000F0U
DEFINE MB_DEFMASK              := 0x00000F00U
DEFINE MB_MODEMASK             := 0x00003000U
DEFINE MB_MISCMASK             := 0x0000C000U
DEFINE CWP_ALL                       := 0x0000
DEFINE CWP_SKIPINVISIBLE     := 0x0001
DEFINE CWP_SKIPDISABLED      := 0x0002
DEFINE CWP_SKIPTRANSPARENT := 0x0004
DEFINE CTLCOLOR_MSGBOX               := 0
DEFINE CTLCOLOR_EDIT                     := 1
DEFINE CTLCOLOR_LISTBOX              := 2
DEFINE CTLCOLOR_BTN                      := 3
DEFINE CTLCOLOR_DLG                      := 4
DEFINE CTLCOLOR_SCROLLBAR        := 5
DEFINE CTLCOLOR_STATIC               := 6
DEFINE CTLCOLOR_MAX                      := 7
DEFINE COLOR_SCROLLBAR               := 0
DEFINE COLOR_BACKGROUND              := 1
DEFINE COLOR_ACTIVECAPTION       := 2
DEFINE COLOR_INACTIVECAPTION     := 3
DEFINE COLOR_MENU                        := 4
DEFINE COLOR_WINDOW                      := 5
DEFINE COLOR_WINDOWFRAME             := 6
DEFINE COLOR_MENUTEXT                := 7
DEFINE COLOR_WINDOWTEXT              := 8
DEFINE COLOR_CAPTIONTEXT             := 9
DEFINE COLOR_ACTIVEBORDER        := 10
DEFINE COLOR_INACTIVEBORDER      := 11
DEFINE COLOR_APPWORKSPACE        := 12
DEFINE COLOR_HIGHLIGHT               := 13
DEFINE COLOR_HIGHLIGHTTEXT       := 14
DEFINE COLOR_BTNFACE                     := 15
DEFINE COLOR_BTNSHADOW               := 16
DEFINE COLOR_GRAYTEXT                := 17
DEFINE COLOR_BTNTEXT                     := 18
DEFINE COLOR_INACTIVECAPTIONTEXT    := 19
DEFINE COLOR_BTNHIGHLIGHT               := 20
DEFINE COLOR_3DDKSHADOW              := 21
DEFINE COLOR_3DLIGHT                     := 22
DEFINE COLOR_INFOTEXT                := 23
DEFINE COLOR_INFOBK                      := 24
DEFINE COLOR_HOTLIGHT          := 26
DEFINE COLOR_GRADIENTACTIVECAPTION := 27
DEFINE COLOR_GRADIENTINACTIVECAPTION := 28
DEFINE COLOR_MENUHILIGHT       := 29
DEFINE COLOR_MENUBAR           := 30
DEFINE COLOR_DESKTOP                     := COLOR_BACKGROUND
DEFINE COLOR_3DFACE                      := COLOR_BTNFACE
DEFINE COLOR_3DSHADOW                := COLOR_BTNSHADOW
DEFINE COLOR_3DHIGHLIGHT             := COLOR_BTNHIGHLIGHT
DEFINE COLOR_3DHILIGHT               := COLOR_BTNHIGHLIGHT
DEFINE COLOR_BTNHILIGHT              := COLOR_BTNHIGHLIGHT
DEFINE GW_HWNDFIRST              := 0
DEFINE GW_HWNDLAST               := 1
DEFINE GW_HWNDNEXT               := 2
DEFINE GW_HWNDPREV               := 3
DEFINE GW_OWNER                      := 4
DEFINE GW_CHILD                      := 5
DEFINE GW_ENABLEDPOPUP     := 6
DEFINE GW_MAX                        := 6
DEFINE MF_INSERT                     := 0x00000000L
DEFINE MF_CHANGE                     := 0x00000080L
DEFINE MF_APPEND                     := 0x00000100L
DEFINE MF_DELETE                     := 0x00000200L
DEFINE MF_REMOVE                     := 0x00001000L
DEFINE MF_BYCOMMAND              := 0x00000000L
DEFINE MF_BYPOSITION             := 0x00000400L
DEFINE MF_SEPARATOR              := 0x00000800L
DEFINE MF_ENABLED                := 0x00000000L
DEFINE MF_GRAYED                     := 0x00000001L
DEFINE MF_DISABLED               := 0x00000002L
DEFINE MF_UNCHECKED              := 0x00000000L
DEFINE MF_CHECKED                := 0x00000008L
DEFINE MF_USECHECKBITMAPS  := 0x00000200L
DEFINE MF_STRING                     := 0x00000000L
DEFINE MF_BITMAP                     := 0x00000004L
DEFINE MF_OWNERDRAW              := 0x00000100L
DEFINE MF_POPUP                      := 0x00000010L
DEFINE MF_MENUBARBREAK       := 0x00000020L
DEFINE MF_MENUBREAK              := 0x00000040L
DEFINE MF_UNHILITE               := 0x00000000L
DEFINE MF_HILITE                     := 0x00000080L
DEFINE MF_DEFAULT                := 0x00001000L
DEFINE MF_SYSMENU                := 0x00002000L
DEFINE MF_HELP                       := 0x00004000L
DEFINE MF_RIGHTJUSTIFY       := 0x00004000L
DEFINE MF_MOUSESELECT        := 0x00008000L
DEFINE MF_END                        := 0x00000080L
DEFINE MFT_STRING                := MF_STRING
DEFINE MFT_BITMAP                := MF_BITMAP
DEFINE MFT_MENUBARBREAK      := MF_MENUBARBREAK
DEFINE MFT_MENUBREAK             := MF_MENUBREAK
DEFINE MFT_OWNERDRAW             := MF_OWNERDRAW
DEFINE MFT_RADIOCHECK        := 0x00000200L
DEFINE MFT_SEPARATOR             := MF_SEPARATOR
DEFINE MFT_RIGHTORDER        := 0x00002000L
DEFINE MFT_RIGHTJUSTIFY      := MF_RIGHTJUSTIFY
DEFINE MFS_GRAYED                := 0x00000003L
DEFINE MFS_DISABLED              := MFS_GRAYED
DEFINE MFS_CHECKED               := MF_CHECKED
DEFINE MFS_HILITE                := MF_HILITE
DEFINE MFS_ENABLED               := MF_ENABLED
DEFINE MFS_UNCHECKED             := MF_UNCHECKED
DEFINE MFS_UNHILITE              := MF_UNHILITE
DEFINE MFS_DEFAULT               := MF_DEFAULT
DEFINE SC_SIZE               := 0xF000
DEFINE SC_MOVE               := 0xF010
DEFINE SC_MINIMIZE       := 0xF020
DEFINE SC_MAXIMIZE       := 0xF030
DEFINE SC_NEXTWINDOW     := 0xF040
DEFINE SC_PREVWINDOW     := 0xF050
DEFINE SC_CLOSE              := 0xF060
DEFINE SC_VSCROLL        := 0xF070
DEFINE SC_HSCROLL        := 0xF080
DEFINE SC_MOUSEMENU      := 0xF090
DEFINE SC_KEYMENU        := 0xF100
DEFINE SC_ARRANGE        := 0xF110
DEFINE SC_RESTORE        := 0xF120
DEFINE SC_TASKLIST       := 0xF130
DEFINE SC_SCREENSAVE     := 0xF140
DEFINE SC_HOTKEY             := 0xF150
DEFINE SC_DEFAULT        := 0xF160
DEFINE SC_MONITORPOWER := 0xF170
DEFINE SC_CONTEXTHELP  := 0xF180
DEFINE SC_SEPARATOR      := 0xF00F
DEFINE SC_ICON               := SC_MINIMIZE
DEFINE SC_ZOOM               := SC_MAXIMIZE
DEFINE IDC_ARROW                     := PTR(_CAST, 32512)
DEFINE IDC_IBEAM                     := PTR(_CAST, 32513)
DEFINE IDC_WAIT                      := PTR(_CAST, 32514)
DEFINE IDC_CROSS                     := PTR(_CAST, 32515)
DEFINE IDC_UPARROW               := PTR(_CAST, 32516)
DEFINE IDC_SIZE                      := PTR(_CAST, 32640)
DEFINE IDC_ICON                      := PTR(_CAST, 32641)
DEFINE IDC_SIZENWSE              := PTR(_CAST, 32642)
DEFINE IDC_SIZENESW              := PTR(_CAST, 32643)
DEFINE IDC_SIZEWE                := PTR(_CAST, 32644)
DEFINE IDC_SIZENS                := PTR(_CAST, 32645)
DEFINE IDC_SIZEALL               := PTR(_CAST, 32646)
DEFINE IDC_NO                        := PTR(_CAST, 32648)
DEFINE IDC_APPSTARTING       := PTR(_CAST, 32650)
DEFINE IDC_HELP                      := PTR(_CAST, 32651)
DEFINE IMAGE_BITMAP              := 0
DEFINE IMAGE_ICON                := 1
DEFINE IMAGE_CURSOR              := 2
DEFINE IMAGE_ENHMETAFILE     := 3
DEFINE BUTTON_IMAGELIST_ALIGN_BOTTOM := 3
DEFINE BUTTON_IMAGELIST_ALIGN_CENTER := 4 // Doesn't draw text
DEFINE BUTTON_IMAGELIST_ALIGN_LEFT := 0
DEFINE BUTTON_IMAGELIST_ALIGN_RIGHT := 1
DEFINE BUTTON_IMAGELIST_ALIGN_TOP := 2
DEFINE BCM_GETIDEALSIZE := (BCM_FIRST + 0x0001)
DEFINE BCM_GETIMAGELIST := (BCM_FIRST + 0x0003)
DEFINE BCM_SETIMAGELIST := (BCM_FIRST + 0x0002)
DEFINE LR_DEFAULTCOLOR       := 0x0000
DEFINE LR_MONOCHROME             := 0x0001
DEFINE LR_COLOR                      := 0x0002
DEFINE LR_COPYRETURNORG      := 0x0004
DEFINE LR_COPYDELETEORG      := 0x0008
DEFINE LR_LOADFROMFILE       := 0x0010
DEFINE LR_LOADTRANSPARENT  := 0x0020
DEFINE LR_DEFAULTSIZE        := 0x0040
DEFINE LR_LOADREALSIZE       := 0x0080
DEFINE LR_LOADMAP3DCOLORS  := 0x1000
DEFINE LR_CREATEDIBSECTION := 0x2000
// for LoadImage(), CopyImage():
DEFINE LR_COPYFROMRESOURCE := 0x4000
DEFINE LR_SHARED           := 0x8000
DEFINE DI_MASK               := 0x0001
DEFINE DI_IMAGE              := 0x0002
DEFINE DI_NORMAL             := 0x0003
DEFINE DI_COMPAT             := 0x0004
DEFINE DI_DEFAULTSIZE  := 0x0008
DEFINE DI_NOMIRROR     := 0x0010
DEFINE RES_ICON      := 1
DEFINE RES_CURSOR  := 2
DEFINE OBM_CLOSE                     := 32754
DEFINE OBM_UPARROW               := 32753
DEFINE OBM_DNARROW               := 32752
DEFINE OBM_RGARROW               := 32751
DEFINE OBM_LFARROW               := 32750
DEFINE OBM_REDUCE                := 32749
DEFINE OBM_ZOOM                      := 32748
DEFINE OBM_RESTORE               := 32747
DEFINE OBM_REDUCED               := 32746
DEFINE OBM_ZOOMD                     := 32745
DEFINE OBM_RESTORED              := 32744
DEFINE OBM_UPARROWD              := 32743
DEFINE OBM_DNARROWD              := 32742
DEFINE OBM_RGARROWD              := 32741
DEFINE OBM_LFARROWD              := 32740
DEFINE OBM_MNARROW               := 32739
DEFINE OBM_COMBO                     := 32738
DEFINE OBM_UPARROWI              := 32737
DEFINE OBM_DNARROWI              := 32736
DEFINE OBM_RGARROWI              := 32735
DEFINE OBM_LFARROWI              := 32734
DEFINE OBM_OLD_CLOSE             := 32767
DEFINE OBM_SIZE                      := 32766
DEFINE OBM_OLD_UPARROW       := 32765
DEFINE OBM_OLD_DNARROW       := 32764
DEFINE OBM_OLD_RGARROW       := 32763
DEFINE OBM_OLD_LFARROW       := 32762
DEFINE OBM_BTSIZE                := 32761
DEFINE OBM_CHECK                     := 32760
DEFINE OBM_CHECKBOXES        := 32759
DEFINE OBM_BTNCORNERS        := 32758
DEFINE OBM_OLD_REDUCE        := 32757
DEFINE OBM_OLD_ZOOM              := 32756
DEFINE OBM_OLD_RESTORE       := 32755
DEFINE OCR_NORMAL                := 32512
DEFINE OCR_IBEAM                     := 32513
DEFINE OCR_WAIT                      := 32514
DEFINE OCR_CROSS                     := 32515
DEFINE OCR_UP                        := 32516
DEFINE OCR_SIZE                      := 32640
DEFINE OCR_ICON                      := 32641
DEFINE OCR_SIZENWSE              := 32642
DEFINE OCR_SIZENESW              := 32643
DEFINE OCR_SIZEWE                := 32644
DEFINE OCR_SIZENS                := 32645
DEFINE OCR_SIZEALL               := 32646
DEFINE OCR_ICOCUR                := 32647
DEFINE OCR_NO                        := 32648
DEFINE OCR_APPSTARTING       := 32650
// for LoadCursor():
DEFINE OCR_HAND := 32649 // Win2K+
DEFINE IDC_HAND := PTR(_CAST, 32649) // Win2K+
DEFINE OIC_SAMPLE                := 32512
DEFINE OIC_HAND                      := 32513
DEFINE OIC_QUES                      := 32514
DEFINE OIC_BANG                      := 32515
DEFINE OIC_NOTE                      := 32516
DEFINE OIC_WINLOGO               := 32517
DEFINE OIC_WARNING               := OIC_BANG
DEFINE OIC_ERROR                     := OIC_HAND
DEFINE OIC_INFORMATION       := OIC_NOTE
DEFINE ORD_LANGDRIVER    := 1
DEFINE IDI_APPLICATION       := PTR(_CAST, 32512)
DEFINE IDI_HAND                      := PTR(_CAST, 32513)
DEFINE IDI_QUESTION              := PTR(_CAST, 32514)
DEFINE IDI_EXCLAMATION       := PTR(_CAST, 32515)
DEFINE IDI_ASTERISK              := PTR(_CAST, 32516)
DEFINE IDI_WINLOGO               := PTR(_CAST, 32517)
DEFINE IDI_WARNING       := IDI_EXCLAMATION
DEFINE IDI_ERROR             := IDI_HAND
DEFINE IDI_INFORMATION := IDI_ASTERISK
DEFINE IDOK                              := 1
DEFINE IDCANCEL                      := 2
DEFINE IDABORT                       := 3
DEFINE IDRETRY                       := 4
DEFINE IDIGNORE                      := 5
DEFINE IDYES                             := 6
DEFINE IDNO                              := 7
DEFINE IDCLOSE              := 8
DEFINE IDHELP                :=9
DEFINE IDTRYAGAIN      := 10
DEFINE IDCONTINUE      := 11
DEFINE IDTIMEOUT := 32000
DEFINE ES_LEFT                       := 0x0000L
DEFINE ES_CENTER                     := 0x0001L
DEFINE ES_RIGHT                      := 0x0002L
DEFINE ES_MULTILINE              := 0x0004L
DEFINE ES_UPPERCASE              := 0x0008L
DEFINE ES_LOWERCASE              := 0x0010L
DEFINE ES_PASSWORD               := 0x0020L
DEFINE ES_AUTOVSCROLL        := 0x0040L
DEFINE ES_AUTOHSCROLL        := 0x0080L
DEFINE ES_NOHIDESEL              := 0x0100L
DEFINE ES_OEMCONVERT             := 0x0400L
DEFINE ES_READONLY               := 0x0800L
DEFINE ES_WANTRETURN             := 0x1000L
DEFINE ES_NUMBER                    := 0x2000L
DEFINE EN_SETFOCUS               := 0x0100
DEFINE EN_KILLFOCUS              := 0x0200
DEFINE EN_CHANGE                     := 0x0300
DEFINE EN_UPDATE                     := 0x0400
DEFINE EN_ERRSPACE               := 0x0500
DEFINE EN_MAXTEXT                := 0x0501
DEFINE EN_HSCROLL                := 0x0601
DEFINE EN_VSCROLL                := 0x0602
DEFINE EN_ALIGN_LTR_EC     := 0x0700
DEFINE EN_ALIGN_RTL_EC     := 0x0701
DEFINE EC_LEFTMARGIN             := 0x0001
DEFINE EC_RIGHTMARGIN        := 0x0002
DEFINE EC_USEFONTINFO        := 0xffff
/* wParam of EM_GET/SETIMESTATUS  */
DEFINE EMSIS_COMPOSITIONSTRING        := 0x0001
/* lParam for EMSIS_COMPOSITIONSTRING  */
DEFINE EIMES_GETCOMPSTRATONCE         := 0x0001
DEFINE EIMES_CANCELCOMPSTRINFOCUS     := 0x0002
DEFINE EIMES_COMPLETECOMPSTRKILLFOCUS := 0x0004
DEFINE EM_GETSEL                             := 0x00B0
DEFINE EM_SETSEL                             := 0x00B1
DEFINE EM_GETRECT                        := 0x00B2
DEFINE EM_SETRECT                        := 0x00B3
DEFINE EM_SETRECTNP                      := 0x00B4
DEFINE EM_SCROLL                             := 0x00B5
DEFINE EM_LINESCROLL                     := 0x00B6
DEFINE EM_SCROLLCARET                := 0x00B7
DEFINE EM_GETMODIFY                      := 0x00B8
DEFINE EM_SETMODIFY                      := 0x00B9
DEFINE EM_GETLINECOUNT               := 0x00BA
DEFINE EM_LINEINDEX                      := 0x00BB
DEFINE EM_SETHANDLE                      := 0x00BC
DEFINE EM_GETHANDLE                      := 0x00BD
DEFINE EM_GETTHUMB                       := 0x00BE
DEFINE EM_LINELENGTH                     := 0x00C1
DEFINE EM_REPLACESEL                     := 0x00C2
DEFINE EM_GETLINE                        := 0x00C4
DEFINE EM_LIMITTEXT                      := 0x00C5
DEFINE EM_CANUNDO                        := 0x00C6
DEFINE EM_UNDO                               := 0x00C7
DEFINE EM_FMTLINES                       := 0x00C8
DEFINE EM_LINEFROMCHAR               := 0x00C9
DEFINE EM_SETTABSTOPS                := 0x00CB
DEFINE EM_SETPASSWORDCHAR        := 0x00CC
DEFINE EM_EMPTYUNDOBUFFER        := 0x00CD
DEFINE EM_GETFIRSTVISIBLELINE  := 0x00CE
DEFINE EM_SETREADONLY                := 0x00CF
DEFINE EM_SETWORDBREAKPROC       := 0x00D0
DEFINE EM_GETWORDBREAKPROC       := 0x00D1
DEFINE EM_GETPASSWORDCHAR        := 0x00D2
DEFINE EM_SETMARGINS                     := 0x00D3
DEFINE EM_GETMARGINS                     := 0x00D4
DEFINE EM_SETLIMITTEXT               := EM_LIMITTEXT
DEFINE EM_GETLIMITTEXT               := 0x00D5
DEFINE EM_POSFROMCHAR                := 0x00D6
DEFINE EM_CHARFROMPOS                := 0x00D7
DEFINE EM_SETIMESTATUS         := 0x00D8
DEFINE EM_GETIMESTATUS         := 0x00D9
DEFINE WB_LEFT                      := 0
DEFINE WB_RIGHT                     := 1
DEFINE WB_ISDELIMITER       := 2
DEFINE BS_PUSHBUTTON             := 0x00000000L
DEFINE BS_DEFPUSHBUTTON      := 0x00000001L
DEFINE BS_CHECKBOX               := 0x00000002L
DEFINE BS_AUTOCHECKBOX       := 0x00000003L
DEFINE BS_RADIOBUTTON        := 0x00000004L
DEFINE BS_3STATE                     := 0x00000005L
DEFINE BS_AUTO3STATE             := 0x00000006L
DEFINE BS_GROUPBOX               := 0x00000007L
DEFINE BS_USERBUTTON             := 0x00000008L
DEFINE BS_AUTORADIOBUTTON  := 0x00000009L
DEFINE BS_PUSHBOX          := 0x0000000AL
DEFINE BS_OWNERDRAW              := 0x0000000BL
DEFINE BS_TYPEMASK         := 0x0000000FL
DEFINE BS_LEFTTEXT               := 0x00000020L
DEFINE BS_TEXT                       := 0x00000000L
DEFINE BS_ICON                       := 0x00000040L
DEFINE BS_BITMAP                     := 0x00000080L
DEFINE BS_LEFT                       := 0x00000100L
DEFINE BS_RIGHT                      := 0x00000200L
DEFINE BS_CENTER                     := 0x00000300L
DEFINE BS_TOP                        := 0x00000400L
DEFINE BS_BOTTOM                     := 0x00000800L
DEFINE BS_VCENTER                := 0x00000C00L
DEFINE BS_PUSHLIKE               := 0x00001000L
DEFINE BS_MULTILINE              := 0x00002000L
DEFINE BS_NOTIFY                     := 0x00004000L
DEFINE BS_FLAT                       := 0x00008000L
DEFINE BS_RIGHTBUTTON        := BS_LEFTTEXT
DEFINE BN_CLICKED                := 0
DEFINE BN_PAINT                      := 1
DEFINE BN_HILITE                     := 2
DEFINE BN_UNHILITE               := 3
DEFINE BN_DISABLE                := 4
DEFINE BN_DOUBLECLICKED      := 5
DEFINE BN_PUSHED                     := BN_HILITE
DEFINE BN_UNPUSHED               := BN_UNHILITE
DEFINE BN_DBLCLK                     := BN_DOUBLECLICKED
DEFINE BN_SETFOCUS               := 6
DEFINE BN_KILLFOCUS              := 7
DEFINE BM_GETCHECK              := 0x00F0
DEFINE BM_SETCHECK              := 0x00F1
DEFINE BM_GETSTATE              := 0x00F2
DEFINE BM_SETSTATE              := 0x00F3
DEFINE BM_SETSTYLE              := 0x00F4
DEFINE BM_CLICK                     := 0x00F5
DEFINE BM_GETIMAGE              := 0x00F6
DEFINE BM_SETIMAGE              := 0x00F7
DEFINE BST_UNCHECKED            := 0x0000
DEFINE BST_CHECKED              := 0x0001
DEFINE BST_INDETERMINATE    := 0x0002
DEFINE BST_PUSHED               := 0x0004
DEFINE BST_FOCUS                    := 0x0008
DEFINE SS_LEFT                       := 0x00000000L
DEFINE SS_CENTER                     := 0x00000001L
DEFINE SS_RIGHT                      := 0x00000002L
DEFINE SS_ICON                       := 0x00000003L
DEFINE SS_BLACKRECT              := 0x00000004L
DEFINE SS_GRAYRECT               := 0x00000005L
DEFINE SS_WHITERECT              := 0x00000006L
DEFINE SS_BLACKFRAME             := 0x00000007L
DEFINE SS_GRAYFRAME              := 0x00000008L
DEFINE SS_WHITEFRAME             := 0x00000009L
DEFINE SS_USERITEM               := 0x0000000AL
DEFINE SS_SIMPLE                     := 0x0000000BL
DEFINE SS_LEFTNOWORDWRAP     := 0x0000000CL
DEFINE SS_OWNERDRAW              := 0x0000000DL
DEFINE SS_BITMAP                     := 0x0000000EL
DEFINE SS_ENHMETAFILE        := 0x0000000FL
DEFINE SS_ETCHEDHORZ             := 0x00000010L
DEFINE SS_ETCHEDVERT             := 0x00000011L
DEFINE SS_ETCHEDFRAME        := 0x00000012L
DEFINE SS_TYPEMASK               := 0x0000001FL
DEFINE SS_REALSIZECONTROL  :=0x00000040L
DEFINE SS_NOPREFIX               := 0x00000080L
DEFINE SS_NOTIFY                     := 0x00000100L
DEFINE SS_CENTERIMAGE        := 0x00000200L
DEFINE SS_RIGHTJUST              := 0x00000400L
DEFINE SS_REALSIZEIMAGE      := 0x00000800L
DEFINE SS_SUNKEN                     := 0x00001000L
DEFINE SS_EDITCONTROL      :=0x00002000L
DEFINE SS_ENDELLIPSIS      :=0x00004000L
DEFINE SS_PATHELLIPSIS     :=0x00008000L
DEFINE SS_WORDELLIPSIS     :=0x0000C000L
DEFINE SS_ELLIPSISMASK     :=0x0000C000L
DEFINE STM_SETICON               := 0x0170
DEFINE STM_GETICON               := 0x0171
DEFINE STM_SETIMAGE              := 0x0172
DEFINE STM_GETIMAGE              := 0x0173
DEFINE STN_CLICKED               := 0
DEFINE STN_DBLCLK                := 1
DEFINE STN_ENABLE                := 2
DEFINE STN_DISABLE               := 3
DEFINE STM_MSGMAX                := 0x0174
DEFINE WC_DIALOG                := PTR(_CAST, 0X8002)
DEFINE DWL_MSGRESULT     := 0
DEFINE DWL_DLGPROC       := 4
DEFINE DWL_USER              := 8
DEFINE DDL_READWRITE             := 0x0000
DEFINE DDL_READONLY              := 0x0001
DEFINE DDL_HIDDEN                := 0x0002
DEFINE DDL_SYSTEM                := 0x0004
DEFINE DDL_DIRECTORY             := 0x0010
DEFINE DDL_ARCHIVE               := 0x0020
DEFINE DDL_POSTMSGS              := 0x2000
DEFINE DDL_DRIVES                := 0x4000
DEFINE DDL_EXCLUSIVE             := 0x8000
DEFINE DS_ABSALIGN               := 0x01L
DEFINE DS_SYSMODAL               := 0x02L
DEFINE DS_LOCALEDIT              := 0x20L
DEFINE DS_SETFONT                := 0x40L
DEFINE DS_MODALFRAME             := 0x80L
DEFINE DS_NOIDLEMSG              := 0x100L
DEFINE DS_SETFOREGROUND      := 0x200L
DEFINE DS_3DLOOK                     := 0x0004L
DEFINE DS_FIXEDSYS               := 0x0008L
DEFINE DS_NOFAILCREATE       := 0x0010L
DEFINE DS_CONTROL                := 0x0400L
DEFINE DS_CENTER                     := 0x0800L
DEFINE DS_CENTERMOUSE        := 0x1000L
DEFINE DS_CONTEXTHELP        := 0x2000L
DEFINE DS_SHELLFONT        := DS_SETFONT | DS_FIXEDSYS
DEFINE DS_USEPIXELS        := 0x8000L
DEFINE DM_GETDEFID               := (WM_USER+0)
DEFINE DM_SETDEFID               := (WM_USER+1)
DEFINE DM_REPOSITION             := (WM_USER+2)
DEFINE PSM_PAGEINFO              := (WM_USER+100)
DEFINE PSM_SHEETINFO             := (WM_USER+101)
DEFINE PSI_SETACTIVE             := 0x0001L
DEFINE PSI_KILLACTIVE        := 0x0002L
DEFINE PSI_APPLY                     := 0x0003L
DEFINE PSI_RESET                     := 0x0004L
DEFINE PSI_HASHELP               := 0x0005L
DEFINE PSI_HELP                      := 0x0006L
DEFINE PSI_CHANGED               := 0x0001L
DEFINE PSI_GUISTART              := 0x0002L
DEFINE PSI_REBOOT                := 0x0003L
DEFINE PSI_GETSIBLINGS       := 0x0004L
DEFINE DC_HASDEFID              := 0x534B
DEFINE DLGC_WANTARROWS       := 0x0001
DEFINE DLGC_WANTTAB              := 0x0002
DEFINE DLGC_WANTALLKEYS      := 0x0004
DEFINE DLGC_WANTMESSAGE      := 0x0004
DEFINE DLGC_HASSETSEL        := 0x0008
DEFINE DLGC_DEFPUSHBUTTON  := 0x0010
DEFINE DLGC_UNDEFPUSHBUTTON := 0x0020
DEFINE DLGC_RADIOBUTTON      := 0x0040
DEFINE DLGC_WANTCHARS        := 0x0080
DEFINE DLGC_STATIC               := 0x0100
DEFINE DLGC_BUTTON               := 0x2000
DEFINE LB_CTLCODE                := 0L
DEFINE LB_OKAY                       := 0
DEFINE LB_ERR                        := (-1)
DEFINE LB_ERRSPACE               := (-2)
DEFINE LBN_ERRSPACE              := (-2)
DEFINE LBN_SELCHANGE             := 1
DEFINE LBN_DBLCLK                := 2
DEFINE LBN_SELCANCEL             := 3
DEFINE LBN_SETFOCUS              := 4
DEFINE LBN_KILLFOCUS             := 5
DEFINE LB_ADDSTRING                      := 0x0180
DEFINE LB_INSERTSTRING               := 0x0181
DEFINE LB_DELETESTRING               := 0x0182
DEFINE LB_SELITEMRANGEEX             := 0x0183
DEFINE LB_RESETCONTENT               := 0x0184
DEFINE LB_SETSEL                             := 0x0185
DEFINE LB_SETCURSEL                      := 0x0186
DEFINE LB_GETSEL                             := 0x0187
DEFINE LB_GETCURSEL                      := 0x0188
DEFINE LB_GETTEXT                        := 0x0189
DEFINE LB_GETTEXTLEN                     := 0x018A
DEFINE LB_GETCOUNT                       := 0x018B
DEFINE LB_SELECTSTRING               := 0x018C
DEFINE LB_DIR                                := 0x018D
DEFINE LB_GETTOPINDEX                := 0x018E
DEFINE LB_FINDSTRING                     := 0x018F
DEFINE LB_GETSELCOUNT                := 0x0190
DEFINE LB_GETSELITEMS                := 0x0191
DEFINE LB_SETTABSTOPS                := 0x0192
DEFINE LB_GETHORIZONTALEXTENT  := 0x0193
DEFINE LB_SETHORIZONTALEXTENT  := 0x0194
DEFINE LB_SETCOLUMNWIDTH             := 0x0195
DEFINE LB_ADDFILE                        := 0x0196
DEFINE LB_SETTOPINDEX                := 0x0197
DEFINE LB_GETITEMRECT                := 0x0198
DEFINE LB_GETITEMDATA                := 0x0199
DEFINE LB_SETITEMDATA                := 0x019A
DEFINE LB_SELITEMRANGE               := 0x019B
DEFINE LB_SETANCHORINDEX             := 0x019C
DEFINE LB_GETANCHORINDEX             := 0x019D
DEFINE LB_SETCARETINDEX              := 0x019E
DEFINE LB_GETCARETINDEX              := 0x019F
DEFINE LB_SETITEMHEIGHT              := 0x01A0
DEFINE LB_GETITEMHEIGHT              := 0x01A1
DEFINE LB_FINDSTRINGEXACT        := 0x01A2
DEFINE LB_SETLOCALE                      := 0x01A5
DEFINE LB_GETLOCALE                      := 0x01A6
DEFINE LB_SETCOUNT                       := 0x01A7
DEFINE LB_INITSTORAGE                := 0x01A8
DEFINE LB_ITEMFROMPOINT              := 0x01A9
DEFINE LB_MULTIPLEADDSTRING          :=0x01B1
DEFINE LB_GETLISTBOXINFO                := 0x01B2
DEFINE LB_MSGMAX                      := 0x01B2
DEFINE LBS_NOTIFY                    := 0x0001L
DEFINE LBS_SORT                          := 0x0002L
DEFINE LBS_NOREDRAW                  := 0x0004L
DEFINE LBS_MULTIPLESEL           := 0x0008L
DEFINE LBS_OWNERDRAWFIXED    := 0x0010L
DEFINE LBS_OWNERDRAWVARIABLE := 0x0020L
DEFINE LBS_HASSTRINGS            := 0x0040L
DEFINE LBS_USETABSTOPS           := 0x0080L
DEFINE LBS_NOINTEGRALHEIGHT  := 0x0100L
DEFINE LBS_MULTICOLUMN           := 0x0200L
DEFINE LBS_WANTKEYBOARDINPUT := 0x0400L
DEFINE LBS_EXTENDEDSEL           := 0x0800L
DEFINE LBS_DISABLENOSCROLL   := 0x1000L
DEFINE LBS_NODATA                    := 0x2000L
DEFINE LBS_NOSEL                         := 0x4000L
DEFINE LBS_COMBOBOX          :=0x8000L
DEFINE LBS_STANDARD          := (LBS_NOTIFY | LBS_SORT | WS_VSCROLL | WS_BORDER)
DEFINE CB_OKAY                       := 0
DEFINE CB_ERR                        := (-1)
DEFINE CB_ERRSPACE               := (-2)
DEFINE CBN_ERRSPACE              := (-1)
DEFINE CBN_SELCHANGE             := 1
DEFINE CBN_DBLCLK                := 2
DEFINE CBN_SETFOCUS              := 3
DEFINE CBN_KILLFOCUS             := 4
DEFINE CBN_EDITCHANGE        := 5
DEFINE CBN_EDITUPDATE        := 6
DEFINE CBN_DROPDOWN              := 7
DEFINE CBN_CLOSEUP               := 8
DEFINE CBN_SELENDOK              := 9
DEFINE CBN_SELENDCANCEL      := 10
DEFINE CBS_SIMPLE                    := 0x0001L
DEFINE CBS_DROPDOWN                  := 0x0002L
DEFINE CBS_DROPDOWNLIST          := 0x0003L
DEFINE CBS_OWNERDRAWFIXED    := 0x0010L
DEFINE CBS_OWNERDRAWVARIABLE := 0x0020L
DEFINE CBS_AUTOHSCROLL           := 0x0040L
DEFINE CBS_OEMCONVERT            := 0x0080L
DEFINE CBS_SORT                          := 0x0100L
DEFINE CBS_HASSTRINGS            := 0x0200L
DEFINE CBS_NOINTEGRALHEIGHT  := 0x0400L
DEFINE CBS_DISABLENOSCROLL   := 0x0800L
DEFINE CBS_UPPERCASE                     := 0x2000L
DEFINE CBS_LOWERCASE                     := 0x4000L
DEFINE CB_GETEDITSEL                             := 0x0140
DEFINE CB_LIMITTEXT                              := 0x0141
DEFINE CB_SETEDITSEL                             := 0x0142
DEFINE CB_ADDSTRING                              := 0x0143
DEFINE CB_DELETESTRING                       := 0x0144
DEFINE CB_DIR                                        := 0x0145
DEFINE CB_GETCOUNT                               := 0x0146
DEFINE CB_GETCURSEL                              := 0x0147
DEFINE CB_GETLBTEXT                              := 0x0148
DEFINE CB_GETLBTEXTLEN                       := 0x0149
DEFINE CB_INSERTSTRING                       := 0x014A
DEFINE CB_RESETCONTENT                       := 0x014B
DEFINE CB_FINDSTRING                             := 0x014C
DEFINE CB_SELECTSTRING                       := 0x014D
DEFINE CB_SETCURSEL                              := 0x014E
DEFINE CB_SHOWDROPDOWN                       := 0x014F
DEFINE CB_GETITEMDATA                        := 0x0150
DEFINE CB_SETITEMDATA                        := 0x0151
DEFINE CB_GETDROPPEDCONTROLRECT      := 0x0152
DEFINE CB_SETITEMHEIGHT                      := 0x0153
DEFINE CB_GETITEMHEIGHT                      := 0x0154
DEFINE CB_SETEXTENDEDUI                      := 0x0155
DEFINE CB_GETEXTENDEDUI                      := 0x0156
DEFINE CB_GETDROPPEDSTATE                := 0x0157
DEFINE CB_FINDSTRINGEXACT                := 0x0158
DEFINE CB_SETLOCALE                              := 0x0159
DEFINE CB_GETLOCALE                              := 0x015A
DEFINE CB_GETTOPINDEX                        := 0x015b
DEFINE CB_SETTOPINDEX                        := 0x015c
DEFINE CB_GETHORIZONTALEXTENT        := 0x015d
DEFINE CB_SETHORIZONTALEXTENT        := 0x015e
DEFINE CB_GETDROPPEDWIDTH                := 0x015f
DEFINE CB_SETDROPPEDWIDTH                := 0x0160
DEFINE CB_INITSTORAGE                        := 0x0161
DEFINE CB_MULTIPLEADDSTRING                   :=0x0163U
DEFINE CB_GETCOMBOBOXINFO         :=  0x0164
DEFINE CB_MSGMAX                                     := 0x0164
DEFINE SBS_HORZ                                      := 0x0000L
DEFINE SBS_VERT                                      := 0x0001L
DEFINE SBS_TOPALIGN                              := 0x0002L
DEFINE SBS_LEFTALIGN                             := 0x0002L
DEFINE SBS_BOTTOMALIGN                       := 0x0004L
DEFINE SBS_RIGHTALIGN                        := 0x0004L
DEFINE SBS_SIZEBOXTOPLEFTALIGN       := 0x0002L
DEFINE SBS_SIZEBOXBOTTOMRIGHTALIGN := 0x0004L
DEFINE SBS_SIZEBOX                               := 0x0008L
DEFINE SBS_SIZEGRIP                              := 0x0010L
DEFINE SBM_SETPOS                                := 0x00E0
DEFINE SBM_GETPOS                                := 0x00E1
DEFINE SBM_SETRANGE                              := 0x00E2
DEFINE SBM_SETRANGEREDRAW                := 0x00E6
DEFINE SBM_GETRANGE                              := 0x00E3
DEFINE SBM_ENABLE_ARROWS                     := 0x00E4
DEFINE SBM_SETSCROLLINFO                     := 0x00E9
DEFINE SBM_GETSCROLLINFO                     := 0x00EA
DEFINE SBM_GETSCROLLBARINFO       :=  0x00EB
DEFINE SIF_RANGE                     := 0x0001
DEFINE SIF_PAGE                      := 0x0002
DEFINE SIF_POS                       := 0x0004
DEFINE SIF_DISABLENOSCROLL := 0x0008
DEFINE SIF_TRACKPOS              := 0x0010
DEFINE SIF_ALL            :=  (SIF_RANGE | SIF_PAGE | SIF_POS | SIF_TRACKPOS)
DEFINE MDIS_ALLCHILDSTYLES      := 0x0001
DEFINE MDITILE_VERTICAL             := 0x0000
DEFINE MDITILE_HORIZONTAL       := 0x0001
DEFINE MDITILE_SKIPDISABLED     := 0x0002
DEFINE MDITILE_ZORDER         := 0x0004
DEFINE IMC_GETCANDIDATEPOS                       := 0x0007
DEFINE IMC_SETCANDIDATEPOS                       := 0x0008
DEFINE IMC_GETCOMPOSITIONFONT                := 0x0009
DEFINE IMC_SETCOMPOSITIONFONT                := 0x000A
DEFINE IMC_GETCOMPOSITIONWINDOW              := 0x000B
DEFINE IMC_SETCOMPOSITIONWINDOW              := 0x000C
DEFINE IMC_GETSTATUSWINDOWPOS                := 0x000F
DEFINE IMC_SETSTATUSWINDOWPOS                := 0x0010
DEFINE IMC_CLOSESTATUSWINDOW                     := 0x0021
DEFINE IMC_OPENSTATUSWINDOW                      := 0x0022
DEFINE IMN_CLOSESTATUSWINDOW            := 0x0001
DEFINE IMN_OPENSTATUSWINDOW             := 0x0002
DEFINE IMN_CHANGECANDIDATE              := 0x0003
DEFINE IMN_CLOSECANDIDATE               := 0x0004
DEFINE IMN_OPENCANDIDATE                    := 0x0005
DEFINE IMN_SETCONVERSIONMODE            := 0x0006
DEFINE IMN_SETSENTENCEMODE              := 0x0007
DEFINE IMN_SETOPENSTATUS                    := 0x0008
DEFINE IMN_SETCANDIDATEPOS              := 0x0009
DEFINE IMN_SETCOMPOSITIONFONT       := 0x000A
DEFINE IMN_SETCOMPOSITIONWINDOW     := 0x000B
DEFINE IMN_SETSTATUSWINDOWPOS       := 0x000C
DEFINE IMN_GUIDELINE                            := 0x000D
DEFINE IMN_PRIVATE                              := 0x000E
DEFINE HELP_CONTEXT          := 0x0001L
DEFINE HELP_QUIT                 := 0x0002L
DEFINE HELP_INDEX            := 0x0003L
DEFINE HELP_CONTENTS         := 0x0003L
DEFINE HELP_HELPONHELP   := 0x0004L
DEFINE HELP_SETINDEX         := 0x0005L
DEFINE HELP_SETCONTENTS  := 0x0005L
DEFINE HELP_CONTEXTPOPUP := 0x0008L
DEFINE HELP_FORCEFILE    := 0x0009L
DEFINE HELP_KEY                  := 0x0101L
DEFINE HELP_COMMAND          := 0x0102L
DEFINE HELP_PARTIALKEY   := 0x0105L
DEFINE HELP_MULTIKEY         := 0x0201L
DEFINE HELP_SETWINPOS    := 0x0203L
DEFINE HELP_CONTEXTMENU  := 0x000a
DEFINE HELP_FINDER           := 0x000b
DEFINE HELP_WM_HELP          := 0x000c
DEFINE HELP_SETPOPUP_POS := 0x000d
DEFINE HELP_TCARD                        := 0x8000
DEFINE HELP_TCARD_DATA               := 0x0010
DEFINE HELP_TCARD_OTHER_CALLER := 0x0011
DEFINE IDH_NO_HELP                                       := 28440
DEFINE IDH_MISSING_CONTEXT                       := 28441
DEFINE IDH_GENERIC_HELP_BUTTON               := 28442
DEFINE IDH_OK                                                := 28443
DEFINE IDH_CANCEL                                        := 28444
DEFINE IDH_HELP                                              := 28445
DEFINE SPI_GETBEEP                               := 1
DEFINE SPI_SETBEEP                               := 2
DEFINE SPI_GETMOUSE                              := 3
DEFINE SPI_SETMOUSE                              := 4
DEFINE SPI_GETBORDER                             := 5
DEFINE SPI_SETBORDER                             := 6
DEFINE SPI_GETKEYBOARDSPEED             := 10
DEFINE SPI_SETKEYBOARDSPEED             := 11
DEFINE SPI_LANGDRIVER                       := 12
DEFINE SPI_ICONHORIZONTALSPACING    := 13
DEFINE SPI_GETSCREENSAVETIMEOUT     := 14
DEFINE SPI_SETSCREENSAVETIMEOUT     := 15
DEFINE SPI_GETSCREENSAVEACTIVE      := 16
DEFINE SPI_SETSCREENSAVEACTIVE      := 17
DEFINE SPI_GETGRIDGRANULARITY       := 18
DEFINE SPI_SETGRIDGRANULARITY       := 19
DEFINE SPI_SETDESKWALLPAPER             := 20
DEFINE SPI_SETDESKPATTERN               := 21
DEFINE SPI_GETKEYBOARDDELAY             := 22
DEFINE SPI_SETKEYBOARDDELAY             := 23
DEFINE SPI_ICONVERTICALSPACING      := 24
DEFINE SPI_GETICONTITLEWRAP             := 25
DEFINE SPI_SETICONTITLEWRAP             := 26
DEFINE SPI_GETMENUDROPALIGNMENT     := 27
DEFINE SPI_SETMENUDROPALIGNMENT     := 28
DEFINE SPI_SETDOUBLECLKWIDTH            := 29
DEFINE SPI_SETDOUBLECLKHEIGHT       := 30
DEFINE SPI_GETICONTITLELOGFONT      := 31
DEFINE SPI_SETDOUBLECLICKTIME       := 32
DEFINE SPI_SETMOUSEBUTTONSWAP       := 33
DEFINE SPI_SETICONTITLELOGFONT      := 34
DEFINE SPI_GETFASTTASKSWITCH            := 35
DEFINE SPI_SETFASTTASKSWITCH            := 36
DEFINE SPI_SETDRAGFULLWINDOWS       := 37
DEFINE SPI_GETDRAGFULLWINDOWS       := 38
DEFINE SPI_GETNONCLIENTMETRICS      := 41
DEFINE SPI_SETNONCLIENTMETRICS      := 42
DEFINE SPI_GETMINIMIZEDMETRICS      := 43
DEFINE SPI_SETMINIMIZEDMETRICS      := 44
DEFINE SPI_GETICONMETRICS               := 45
DEFINE SPI_SETICONMETRICS               := 46
DEFINE SPI_SETWORKAREA                      := 47
DEFINE SPI_GETWORKAREA                      := 48
DEFINE SPI_SETPENWINDOWS                    := 49
DEFINE SPI_GETHIGHCONTRAST              := 66
DEFINE SPI_SETHIGHCONTRAST              := 67
DEFINE SPI_GETKEYBOARDPREF              := 68
DEFINE SPI_SETKEYBOARDPREF              := 69
DEFINE SPI_GETSCREENREADER              := 70
DEFINE SPI_SETSCREENREADER              := 71
DEFINE SPI_GETANIMATION                     := 72
DEFINE SPI_SETANIMATION                     := 73
DEFINE SPI_GETFONTSMOOTHING             := 74
DEFINE SPI_SETFONTSMOOTHING             := 75
DEFINE SPI_SETDRAGWIDTH                     := 76
DEFINE SPI_SETDRAGHEIGHT                    := 77
DEFINE SPI_SETHANDHELD                      := 78
DEFINE SPI_GETLOWPOWERTIMEOUT       := 79
DEFINE SPI_GETPOWEROFFTIMEOUT       := 80
DEFINE SPI_SETLOWPOWERTIMEOUT       := 81
DEFINE SPI_SETPOWEROFFTIMEOUT       := 82
DEFINE SPI_GETLOWPOWERACTIVE            := 83
DEFINE SPI_GETPOWEROFFACTIVE            := 84
DEFINE SPI_SETLOWPOWERACTIVE            := 85
DEFINE SPI_SETPOWEROFFACTIVE            := 86
DEFINE SPI_SETCURSORS                       := 87
DEFINE SPI_SETICONS                             := 88
DEFINE SPI_GETDEFAULTINPUTLANG      := 89
DEFINE SPI_SETDEFAULTINPUTLANG      := 90
DEFINE SPI_SETLANGTOGGLE                    := 91
DEFINE SPI_GETWINDOWSEXTENSION      := 92
DEFINE SPI_SETMOUSETRAILS               := 93
DEFINE SPI_GETMOUSETRAILS               := 94
DEFINE SPI_SETSCREENSAVERRUNNING   := 0x0061
DEFINE SPI_SCREENSAVERRUNNING     :=SPI_SETSCREENSAVERRUNNING
DEFINE SPI_GETFILTERKEYS                    := 50
DEFINE SPI_SETFILTERKEYS                    := 51
DEFINE SPI_GETTOGGLEKEYS                    := 52
DEFINE SPI_SETTOGGLEKEYS                    := 53
DEFINE SPI_GETMOUSEKEYS                     := 54
DEFINE SPI_SETMOUSEKEYS                     := 55
DEFINE SPI_GETSHOWSOUNDS                    := 56
DEFINE SPI_SETSHOWSOUNDS                    := 57
DEFINE SPI_GETSTICKYKEYS                    := 58
DEFINE SPI_SETSTICKYKEYS                    := 59
DEFINE SPI_GETACCESSTIMEOUT             := 60
DEFINE SPI_SETACCESSTIMEOUT             := 61
DEFINE SPI_GETSERIALKEYS                    := 62
DEFINE SPI_SETSERIALKEYS                    := 63
DEFINE SPI_GETSOUNDSENTRY               := 64
DEFINE SPI_SETSOUNDSENTRY               := 65
DEFINE SPI_GETSNAPTODEFBUTTON     :=0x005F
DEFINE SPI_SETSNAPTODEFBUTTON     :=0x0060
DEFINE SPI_GETMOUSEHOVERWIDTH     :=0x0062
DEFINE SPI_SETMOUSEHOVERWIDTH     :=0x0063
DEFINE SPI_GETMOUSEHOVERHEIGHT    :=0x0064
DEFINE SPI_SETMOUSEHOVERHEIGHT    :=0x0065
DEFINE SPI_GETMOUSEHOVERTIME      :=0x0066
DEFINE SPI_SETMOUSEHOVERTIME      :=0x0067
DEFINE SPI_GETWHEELSCROLLLINES    :=0x0068
DEFINE SPI_SETWHEELSCROLLLINES    :=0x0069
DEFINE SPI_GETMENUSHOWDELAY       :=0x006A
DEFINE SPI_SETMENUSHOWDELAY       :=0x006B
DEFINE SPI_GETSHOWIMEUI          :=0x006E
DEFINE SPI_SETSHOWIMEUI          :=0x006F
DEFINE SPI_GETMOUSESPEED         :=0x0070
DEFINE SPI_SETMOUSESPEED         :=0x0071
DEFINE SPI_GETSCREENSAVERRUNNING :=0x0072
DEFINE SPI_GETDESKWALLPAPER      :=0x0073
DEFINE SPI_GETACTIVEWINDOWTRACKING         :=0x1000
DEFINE SPI_SETACTIVEWINDOWTRACKING         :=0x1001
DEFINE SPI_GETMENUANIMATION                :=0x1002
DEFINE SPI_SETMENUANIMATION                :=0x1003
DEFINE SPI_GETCOMBOBOXANIMATION            :=0x1004
DEFINE SPI_SETCOMBOBOXANIMATION            :=0x1005
DEFINE SPI_GETLISTBOXSMOOTHSCROLLING       :=0x1006
DEFINE SPI_SETLISTBOXSMOOTHSCROLLING       :=0x1007
DEFINE SPI_GETGRADIENTCAPTIONS             :=0x1008
DEFINE SPI_SETGRADIENTCAPTIONS             :=0x1009
DEFINE SPI_GETKEYBOARDCUES                 :=0x100A
DEFINE SPI_SETKEYBOARDCUES                 :=0x100B
DEFINE SPI_GETMENUUNDERLINES               :=SPI_GETKEYBOARDCUES
DEFINE SPI_SETMENUUNDERLINES               :=SPI_SETKEYBOARDCUES
DEFINE SPI_GETACTIVEWNDTRKZORDER           :=0x100C
DEFINE SPI_SETACTIVEWNDTRKZORDER           :=0x100D
DEFINE SPI_GETHOTTRACKING                  :=0x100E
DEFINE SPI_SETHOTTRACKING                  :=0x100F
DEFINE SPI_GETMENUFADE                     :=0x1012
DEFINE SPI_SETMENUFADE                     :=0x1013
DEFINE SPI_GETSELECTIONFADE                :=0x1014
DEFINE SPI_SETSELECTIONFADE                :=0x1015
DEFINE SPI_GETTOOLTIPANIMATION             :=0x1016
DEFINE SPI_SETTOOLTIPANIMATION             :=0x1017
DEFINE SPI_GETTOOLTIPFADE                  :=0x1018
DEFINE SPI_SETTOOLTIPFADE                  :=0x1019
DEFINE SPI_GETCURSORSHADOW                 :=0x101A
DEFINE SPI_SETCURSORSHADOW                 :=0x101B
DEFINE SPI_GETMOUSESONAR                   :=0x101C
DEFINE SPI_SETMOUSESONAR                   :=0x101D
DEFINE SPI_GETMOUSECLICKLOCK               :=0x101E
DEFINE SPI_SETMOUSECLICKLOCK               :=0x101F
DEFINE SPI_GETMOUSEVANISH                  :=0x1020
DEFINE SPI_SETMOUSEVANISH                  :=0x1021
DEFINE SPI_GETFLATMENU                     :=0x1022
DEFINE SPI_SETFLATMENU                     :=0x1023
DEFINE SPI_GETDROPSHADOW                   :=0x1024
DEFINE SPI_SETDROPSHADOW                   :=0x1025
DEFINE SPI_GETBLOCKSENDINPUTRESETS         :=0x1026
DEFINE SPI_SETBLOCKSENDINPUTRESETS         :=0x1027
DEFINE SPI_GETUIEFFECTS                    :=0x103E
DEFINE SPI_SETUIEFFECTS                    :=0x103F
DEFINE SPI_GETFOREGROUNDLOCKTIMEOUT        := 0x2000
DEFINE SPI_SETFOREGROUNDLOCKTIMEOUT        := 0x2001
DEFINE SPI_GETACTIVEWNDTRKTIMEOUT          := 0x2002
DEFINE SPI_SETACTIVEWNDTRKTIMEOUT          := 0x2003
DEFINE SPI_GETFOREGROUNDFLASHCOUNT         := 0x2004
DEFINE SPI_SETFOREGROUNDFLASHCOUNT         := 0x2005
DEFINE SPI_GETCARETWIDTH                   := 0x2006
DEFINE SPI_SETCARETWIDTH                   := 0x2007
DEFINE SPI_GETMOUSECLICKLOCKTIME           := 0x2008
DEFINE SPI_SETMOUSECLICKLOCKTIME           := 0x2009
DEFINE SPI_GETFONTSMOOTHINGTYPE            := 0x200A
DEFINE SPI_SETFONTSMOOTHINGTYPE            := 0x200B
/* constants for SPI_GETFONTSMOOTHINGTYPE and SPI_SETFONTSMOOTHINGTYPE: */
DEFINE FE_FONTSMOOTHINGSTANDARD           :=  0x0001
DEFINE FE_FONTSMOOTHINGCLEARTYPE          :=  0x0002
DEFINE FE_FONTSMOOTHINGDOCKING            :=  0x8000
DEFINE SPI_GETFONTSMOOTHINGCONTRAST        :=   0x200C
DEFINE SPI_SETFONTSMOOTHINGCONTRAST        :=   0x200D
DEFINE SPI_GETFOCUSBORDERWIDTH             := 0x200E
DEFINE SPI_SETFOCUSBORDERWIDTH             := 0x200F
DEFINE SPI_GETFOCUSBORDERHEIGHT            := 0x2010
DEFINE SPI_SETFOCUSBORDERHEIGHT            := 0x2011
DEFINE SPI_GETFONTSMOOTHINGORIENTATION       :=    0x2012
DEFINE SPI_SETFONTSMOOTHINGORIENTATION       :=    0x2013
/* constants for SPI_GETFONTSMOOTHINGORIENTATION and SPI_SETFONTSMOOTHINGORIENTATION: */
DEFINE FE_FONTSMOOTHINGORIENTATIONBGR  := 0x0000
DEFINE FE_FONTSMOOTHINGORIENTATIONRGB  := 0x0001
DEFINE SPIF_UPDATEINIFILE    := 0x0001
DEFINE SPIF_SENDWININICHANGE := 0x0002
DEFINE SPIF_SENDCHANGE           := SPIF_SENDWININICHANGE
DEFINE METRICS_USEDEFAULT := -1
DEFINE ARW_BOTTOMLEFT                        := 0x0000L
DEFINE ARW_BOTTOMRIGHT                       := 0x0001L
DEFINE ARW_TOPLEFT                               := 0x0002L
DEFINE ARW_TOPRIGHT                              := 0x0003L
DEFINE ARW_STARTMASK                             := 0x0003L
DEFINE ARW_STARTRIGHT                        := 0x0001L
DEFINE ARW_STARTTOP                              := 0x0002L
DEFINE ARW_LEFT                                      := 0x0000L
DEFINE ARW_RIGHT                                     := 0x0000L
DEFINE ARW_UP                                        := 0x0004L
DEFINE ARW_DOWN                                      := 0x0004L
DEFINE ARW_HIDE                                      := 0x0008L
DEFINE ARW_VALID                                     := 0x000FL
DEFINE SERKF_SERIALKEYSON  := 0x00000001
DEFINE SERKF_AVAILABLE       := 0x00000002
DEFINE SERKF_INDICATOR       := 0x00000004
DEFINE HCF_HIGHCONTRASTON  := 0x00000001
DEFINE HCF_AVAILABLE             := 0x00000002
DEFINE HCF_HOTKEYACTIVE      := 0x00000004
DEFINE HCF_CONFIRMHOTKEY     := 0x00000008
DEFINE HCF_HOTKEYSOUND       := 0x00000010
DEFINE HCF_INDICATOR             := 0x00000020
DEFINE HCF_HOTKEYAVAILABLE := 0x00000040
DEFINE HCF_LOGONDESKTOP    := 0x00000100
DEFINE HCF_DEFAULTDESKTOP  := 0x00000200
DEFINE CDS_UPDATEREGISTRY  := 0x00000001
DEFINE CDS_TEST                      := 0x00000002
DEFINE CDS_FULLSCREEN        := 0x00000004
DEFINE CDS_GLOBAL          := 0x00000008
DEFINE CDS_SET_PRIMARY     := 0x00000010
DEFINE CDS_VIDEOPARAMETERS := 0x00000020
DEFINE CDS_RESET           := 0x40000000
DEFINE CDS_NORESET         := 0x10000000
DEFINE DISP_CHANGE_SUCCESSFUL           := 0
DEFINE DISP_CHANGE_RESTART                  := 1
DEFINE DISP_CHANGE_FAILED                := -1
DEFINE DISP_CHANGE_BADMODE               := -2
DEFINE DISP_CHANGE_NOTUPDATED        := -3
DEFINE DISP_CHANGE_BADFLAGS              := -4
DEFINE DISP_CHANGE_BADPARAM       := -5
DEFINE DISP_CHANGE_BADDUALVIEW    := -6
DEFINE ENUM_CURRENT_SETTINGS       := 0xFFFFFFFU
DEFINE ENUM_REGISTRY_SETTINGS      := 0xFFFFFFEU
DEFINE FKF_FILTERKEYSON      := 0x00000001
DEFINE FKF_AVAILABLE             := 0x00000002
DEFINE FKF_HOTKEYACTIVE      := 0x00000004
DEFINE FKF_CONFIRMHOTKEY     := 0x00000008
DEFINE FKF_HOTKEYSOUND       := 0x00000010
DEFINE FKF_INDICATOR             := 0x00000020
DEFINE FKF_CLICKON               := 0x00000040
DEFINE SKF_STICKYKEYSON      := 0x00000001
DEFINE SKF_AVAILABLE             := 0x00000002
DEFINE SKF_HOTKEYACTIVE      := 0x00000004
DEFINE SKF_CONFIRMHOTKEY     := 0x00000008
DEFINE SKF_HOTKEYSOUND       := 0x00000010
DEFINE SKF_INDICATOR             := 0x00000020
DEFINE SKF_AUDIBLEFEEDBACK := 0x00000040
DEFINE SKF_TRISTATE              := 0x00000080
DEFINE SKF_TWOKEYSOFF        := 0x00000100
DEFINE SKF_LALTLATCHED       := 0x10000000
DEFINE SKF_LCTLLATCHED       := 0x04000000
DEFINE SKF_LSHIFTLATCHED     := 0x01000000
DEFINE SKF_RALTLATCHED       := 0x20000000
DEFINE SKF_RCTLLATCHED       := 0x08000000
DEFINE SKF_RSHIFTLATCHED     := 0x02000000
DEFINE SKF_LWINLATCHED       := 0x40000000
DEFINE SKF_RWINLATCHED       := 0x80000000
DEFINE SKF_LALTLOCKED        := 0x00100000
DEFINE SKF_LCTLLOCKED        := 0x00040000
DEFINE SKF_LSHIFTLOCKED      := 0x00010000
DEFINE SKF_RALTLOCKED        := 0x00200000
DEFINE SKF_RCTLLOCKED        := 0x00080000
DEFINE SKF_RSHIFTLOCKED      := 0x00020000
DEFINE SKF_LWINLOCKED        := 0x00400000
DEFINE SKF_RWINLOCKED        := 0x00800000
DEFINE MKF_MOUSEKEYSON       := 0x00000001
DEFINE MKF_AVAILABLE             := 0x00000002
DEFINE MKF_HOTKEYACTIVE      := 0x00000004
DEFINE MKF_CONFIRMHOTKEY     := 0x00000008
DEFINE MKF_HOTKEYSOUND       := 0x00000010
DEFINE MKF_INDICATOR             := 0x00000020
DEFINE MKF_MODIFIERS             := 0x00000040
DEFINE MKF_REPLACENUMBERS  := 0x00000080
DEFINE MKF_LEFTBUTTONSEL   :=0x10000000
DEFINE MKF_RIGHTBUTTONSEL  :=0x20000000
DEFINE MKF_LEFTBUTTONDOWN  :=0x01000000
DEFINE MKF_RIGHTBUTTONDOWN :=0x02000000
DEFINE MKF_MOUSEMODE       :=0x80000000
DEFINE ATF_TIMEOUTON             := 0x00000001
DEFINE ATF_ONOFFFEEDBACK     := 0x00000002
DEFINE SSGF_NONE             := 0
DEFINE SSGF_DISPLAY      := 3
DEFINE SSTF_NONE             := 0
DEFINE SSTF_CHARS        := 1
DEFINE SSTF_BORDER       := 2
DEFINE SSTF_DISPLAY      := 3
DEFINE SSWF_NONE         := 0
DEFINE SSWF_TITLE    := 1
DEFINE SSWF_WINDOW   := 2
DEFINE SSWF_DISPLAY  := 3
DEFINE SSWF_CUSTOM   := 4
DEFINE SSF_SOUNDSENTRYON     := 0x00000001
DEFINE SSF_AVAILABLE             := 0x00000002
DEFINE SSF_INDICATOR             := 0x00000004
DEFINE TKF_TOGGLEKEYSON      := 0x00000001
DEFINE TKF_AVAILABLE             := 0x00000002
DEFINE TKF_HOTKEYACTIVE      := 0x00000004
DEFINE TKF_CONFIRMHOTKEY     := 0x00000008
DEFINE TKF_HOTKEYSOUND       := 0x00000010
DEFINE TKF_INDICATOR             := 0x00000020
DEFINE SLE_ERROR             := 0x00000001
DEFINE SLE_MINORERROR  := 0x00000002
DEFINE SLE_WARNING       := 0x00000003
DEFINE MONITOR_DEFAULTTONULL      :=  0x00000000
DEFINE MONITOR_DEFAULTTOPRIMARY    := 0x00000001
DEFINE MONITOR_DEFAULTTONEAREST    := 0x00000002
DEFINE CCHILDREN_TITLEBAR              := 5
DEFINE CCHILDREN_SCROLLBAR             := 5
DEFINE CCHILDREN_FRAME                 := 7
DEFINE OBJID_WINDOW        := 0x00000000L
DEFINE OBJID_SYSMENU       := 0xFFFFFFFFL
DEFINE OBJID_TITLEBAR      := 0xFFFFFFFEL
DEFINE OBJID_MENU          := 0xFFFFFFFDL
DEFINE OBJID_CLIENT        := 0xFFFFFFFCL
DEFINE OBJID_VSCROLL       := 0xFFFFFFFBL
DEFINE OBJID_HSCROLL       := 0xFFFFFFFAL
DEFINE OBJID_SIZEGRIP      := 0xFFFFFFF9L
DEFINE OBJID_CARET         := 0xFFFFFFF8L
DEFINE OBJID_CURSOR        := 0xFFFFFFF7L
DEFINE OBJID_ALERT         := 0xFFFFFFF6L
DEFINE OBJID_SOUND         := 0xFFFFFFF5L
DEFINE OBJID_QUERYCLASSNAMEIDX := 0xFFFFFFF4L
DEFINE OBJID_NATIVEOM     := 0xFFFFFFF0L
DEFINE ASC_NUL := 0x00  // 00 // Null
DEFINE ASC_SOH := 0x01  // 01 // Start of heading
DEFINE ASC_STX := 0x02  // 02   // Start of text
DEFINE ASC_ETX := 0x03  // 03   // End of text
DEFINE ASC_EOT := 0x04  // 04   // End of transmission
DEFINE ASC_ENQ := 0x05  // 05   // Enquiry
DEFINE ASC_ACK := 0x06  // 06   // Acknowledge
DEFINE ASC_BEL := 0x07  // 07   // Audible signal
//DEFINE ASC_BS  := 0x08  // 08   // Backspace
DEFINE ASC_HT  := 0x09  // 09   // Horizontal tabulation
//DEFINE ASC_LF  := 0x0A  // 10   // Line feed
DEFINE ASC_VT  := 0x0B  // 11 // Vertical tabulation
//DEFINE ASC_FF  := 0x0C  // 12   // Form feed
//DEFINE ASC_CR  := 0x0D  // 13   // Carriege return
DEFINE ASC_SO  := 0x0E  // 14   // Shift off
DEFINE ASC_SI  := 0x0F  // 15   // Shift in
DEFINE ASC_DLE := 0x10  // 16   // Data link escape
DEFINE ASC_DC1 := 0x11  // 17   // Device control 1
DEFINE ASC_DC2 := 0x12  // 18   // Device control 2
DEFINE ASC_DC3 := 0x13  // 19   // Device control 3
DEFINE ASC_DC4 := 0x14  // 20   // Device stop
DEFINE ASC_NAK := 0x15  // 21 // Negative acknowledge
DEFINE ASC_SYN := 0x16  // 22   // Synchronous idle
DEFINE ASC_ETB := 0x17  // 23   // End of transmission block
DEFINE ASC_CAN := 0x18  // 24   // Cancel
DEFINE ASC_EM  := 0x19  // 25   // End of medium
DEFINE ASC_SS  := 0x1A  // 26   // Start of special sequence
//DEFINE ASC_ESC := 0x1B  // 27   // Escape
DEFINE ASC_FS  := 0x1C  // 28   // File separator
DEFINE ASC_GS  := 0x1D  // 29   // Group separator
DEFINE ASC_RS  := 0x1E  // 30   // Record separator
DEFINE ASC_US  := 0x1F  // 31   // Unit separator
DEFINE ASC_SP  := 0x20  // 32   // Space
DEFINE ASC_Exclamation := 0x21  // 33   // !
DEFINE ASC_Quotation   := 0x22  // 34   // "
DEFINE ASC_DoubleQMark := 0x22  // 34   // "
DEFINE ASC_Number      := 0x23  // 35   // #
DEFINE ASC_Dollar      := 0x24  // 36   // $
DEFINE ASC_Percent     := 0x25  // 37   // %
DEFINE ASC_Ampersand   := 0x26  // 38   // &
DEFINE ASC_Apostrophe  := 0x27  // 39   // '
DEFINE ASC_SingleQMark := 0x27  // 39   // '
DEFINE ASC_LeftP       := 0x28  // 40   // (
DEFINE ASC_RightP      := 0x29  // 41   // )
DEFINE ASC_Asterisk    := 0x2A  // 42   // *
DEFINE ASC_Plus        := 0x2B  // 43   // +
DEFINE ASC_Comma       := 0x2C  // 44   // ,
DEFINE ASC_Minus       := 0x2D  // 45   // -
DEFINE ASC_Hyphen      := 0x2D  // 45   // -
DEFINE ASC_Dot         := 0x2E  // 46   // .
DEFINE ASC_Period      := 0x2E  // 46   // .
DEFINE ASC_FullStop    := 0x2E  // 46   // .
DEFINE ASC_Solidus     := 0x2F  // 47   // /
DEFINE ASC_Slash       := 0x2F  // 47   // /
//DEFINE ASC_0   := 0x30  // 48   // 0
//DEFINE ASC_1   := 0x31  // 49   // 1
DEFINE ASC_2   := 0x32  // 50   // 2
DEFINE ASC_3   := 0x33  // 51   // 3
DEFINE ASC_4   := 0x34  // 52   // 4
DEFINE ASC_5   := 0x35  // 53   // 5
DEFINE ASC_6   := 0x36  // 54   // 6
DEFINE ASC_7   := 0x37  // 55   // 7
DEFINE ASC_8   := 0x38  // 56   // 8
//DEFINE ASC_9   := 0x39  // 57   // 9
DEFINE ASC_Colon       := 0x3A  // 58   // :
DEFINE ASC_Semicolon   := 0x3B  // 59   //
DEFINE ASC_Less        := 0x3C  // 60   // <
DEFINE ASC_Equals      := 0x3D  // 61   // =
DEFINE ASC_Greater     := 0x3E  // 62   // >
DEFINE ASC_Question    := 0x3F  // 63   // ?
DEFINE ASC_AT          := 0x40  // 64   // @
//DEFINE ASC_A   := 0x41  // 65   // A
DEFINE ASC_B   := 0x42  // 66   // B
DEFINE ASC_C   := 0x43  // 67   // C
DEFINE ASC_D   := 0x44  // 68   // D
DEFINE ASC_E   := 0x45  // 69   // E
DEFINE ASC_F   := 0x46  // 70   // F
DEFINE ASC_G   := 0x47  // 71   // G
DEFINE ASC_H   := 0x48  // 72   // H
DEFINE ASC_I   := 0x49  // 73   // I
DEFINE ASC_J   := 0x4A  // 74   // J
DEFINE ASC_K   := 0x4B  // 75   // K
DEFINE ASC_L   := 0x4C  // 76   // L
DEFINE ASC_M   := 0x4D  // 77   // M
DEFINE ASC_N   := 0x4E  // 78   // N
DEFINE ASC_O   := 0x4F  // 79   // O
DEFINE ASC_P   := 0x50  // 80   // P
DEFINE ASC_Q   := 0x51  // 81   // Q
DEFINE ASC_R   := 0x52  // 82   // R
DEFINE ASC_S   := 0x53  // 83   // S
DEFINE ASC_T   := 0x54  // 84   // T
DEFINE ASC_U   := 0x55  // 85   // U
DEFINE ASC_V   := 0x56  // 86   // V
DEFINE ASC_W   := 0x57  // 87   // W
DEFINE ASC_X   := 0x58  // 88   // X
DEFINE ASC_Y   := 0x59  // 89   // Y
//DEFINE ASC_Z   := 0x5A  // 90   // Z
DEFINE ASC_LeftSq      := 0x5B  // 91   // [
DEFINE ASC_BackSlash   := 0x5C  // 92   // \
DEFINE ASC_RightSq     := 0x5D  // 93   // ]
DEFINE ASC_Circumflex  := 0x5E  // 94   // ^
DEFINE ASC_Underscore  := 0x5f  // 95   // _
DEFINE ASC_a_  := 0x61  //  97  // a
DEFINE ASC_b_  := 0x62  //  98  // b
DEFINE ASC_c_  := 0x63  //  99  // c
DEFINE ASC_d_  := 0x64  // 100  // d
DEFINE ASC_e_  := 0x65  // 101  // e
DEFINE ASC_f_  := 0x66  // 102  // f
DEFINE ASC_g_  := 0x67  // 103  // g
DEFINE ASC_h_  := 0x68  // 104  // h
DEFINE ASC_i_  := 0x69  // 105  // i
DEFINE ASC_j_  := 0x6A  // 106  // j
DEFINE ASC_k_  := 0x6B  // 107  // k
DEFINE ASC_l_  := 0x6C  // 108  // l
DEFINE ASC_m_  := 0x6D  // 109  // m
DEFINE ASC_n_  := 0x6E  // 110  // n
DEFINE ASC_o_  := 0x6F  // 111  // o
DEFINE ASC_p_  := 0x70  // 112  // p
DEFINE ASC_q_  := 0x71  // 113  // q
DEFINE ASC_r_  := 0x72  // 114  // r
DEFINE ASC_s_  := 0x73  // 115  // s
DEFINE ASC_t_  := 0x74  // 116  // t
DEFINE ASC_u_  := 0x75  // 117  // u
DEFINE ASC_v_  := 0x76  // 118  // v
DEFINE ASC_w_  := 0x77  // 119  // w
DEFINE ASC_x_  := 0x78  // 120  // x
DEFINE ASC_y_  := 0x79  // 121  // y
DEFINE ASC_z_  := 0x7A  // 122  // z
DEFINE ASC_LeftCurly   := 0x7B  // 123 // {
DEFINE ASC_VertLine    := 0x7C  // 124 // |
DEFINE ASC_RightCurly  := 0x7D  // 125 // }
DEFINE ASC_Tilde       := 0x7E  // 126 // ~
DEFINE ASC_Delete      := 0x7f  // 127 //
DEFINE SIZEOFWIN95MENUITEMINFO := 44
DEFINE MIIM_STRING := 0x00000040
DEFINE MIIM_BITMAP := 0x00000080
DEFINE MIIM_FTYPE  := 0x00000100
DEFINE EM_SETUNDOLIMIT      := (WM_USER + 82) /* Richedit v2.0 messages */
DEFINE EM_REDO              := (WM_USER + 84)
DEFINE EM_CANREDO           := (WM_USER + 85)
DEFINE EM_GETUNDONAME       := (WM_USER + 86)
DEFINE EM_GETREDONAME       := (WM_USER + 87)
DEFINE EM_STOPGROUPTYPING   := (WM_USER + 88)
DEFINE EM_SETTEXTMODE       := (WM_USER + 89)
DEFINE EM_GETTEXTMODE       := (WM_USER + 90)
/* enum for use with EM_GET/SETTEXTMODE */
DEFINE  TM_PLAINTEXT       :=  1
DEFINE  TM_RICHTEXT        :=  2    /* default behavior */
DEFINE  TM_SINGLELEVELUNDO :=  4
DEFINE  TM_MULTILEVELUNDO  :=  8    /* default behavior */
DEFINE  TM_SINGLECODEPAGE  :=  16
DEFINE  TM_MULTICODEPAGE   :=  32   /* default behavior */
DEFINE EM_AUTOURLDETECT     := (WM_USER + 91)
DEFINE EM_GETAUTOURLDETECT  := (WM_USER + 92)
DEFINE EM_SETPALETTE        := (WM_USER + 93)
DEFINE EM_GETTEXTEX         := (WM_USER + 94)
DEFINE EM_GETTEXTLENGTHEX   := (WM_USER + 95)
DEFINE STATE_SYSTEM_UNAVAILABLE        := 0x00000001U  // Disabled
DEFINE STATE_SYSTEM_SELECTED           := 0x00000002U
DEFINE STATE_SYSTEM_FOCUSED            := 0x00000004U
DEFINE STATE_SYSTEM_PRESSED            := 0x00000008U
DEFINE STATE_SYSTEM_CHECKED            := 0x00000010U
DEFINE STATE_SYSTEM_MIXED              := 0x00000020U  // 3-state checkbox or toolbar button
DEFINE STATE_SYSTEM_READONLY           := 0x00000040U
DEFINE STATE_SYSTEM_HOTTRACKED         := 0x00000080U
DEFINE STATE_SYSTEM_DEFAULT            := 0x00000100U
DEFINE STATE_SYSTEM_EXPANDED           := 0x00000200U
DEFINE STATE_SYSTEM_COLLAPSED          := 0x00000400U
DEFINE STATE_SYSTEM_BUSY               := 0x00000800U
DEFINE STATE_SYSTEM_FLOATING           := 0x00001000U  // Children "owned" not "contained" by parent
DEFINE STATE_SYSTEM_MARQUEED           := 0x00002000U
DEFINE STATE_SYSTEM_ANIMATED           := 0x00004000U
DEFINE STATE_SYSTEM_INVISIBLE          := 0x00008000U
DEFINE STATE_SYSTEM_OFFSCREEN          := 0x00010000U
DEFINE STATE_SYSTEM_SIZEABLE           := 0x00020000U
DEFINE STATE_SYSTEM_MOVEABLE           := 0x00040000U
DEFINE STATE_SYSTEM_SELFVOICING        := 0x00080000U
DEFINE STATE_SYSTEM_FOCUSABLE          := 0x00100000U
DEFINE STATE_SYSTEM_SELECTABLE         := 0x00200000U
DEFINE STATE_SYSTEM_LINKED             := 0x00400000U
DEFINE STATE_SYSTEM_TRAVERSED          := 0x00800000U
DEFINE STATE_SYSTEM_MULTISELECTABLE    := 0x01000000U  // Supports multiple selection
DEFINE STATE_SYSTEM_EXTSELECTABLE      := 0x02000000U  // Supports extended selection
DEFINE STATE_SYSTEM_ALERT_LOW          := 0x04000000U  // This information is of low priority
DEFINE STATE_SYSTEM_ALERT_MEDIUM       := 0x08000000U  // This information is of medium priority
DEFINE STATE_SYSTEM_ALERT_HIGH         := 0x10000000U  // This information is of high priority
DEFINE STATE_SYSTEM_VALID              := 0x1FFFFFFFU
#endregion
