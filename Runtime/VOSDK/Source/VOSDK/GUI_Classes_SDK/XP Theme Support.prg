PROCEDURE __InitTheme() _INIT3
	//PP-030909
	__LoadThemeDll()
	RETURN

FUNCTION __LoadThemeDll() AS LOGIC STRICT
	//PP-030909
	LOCAL hModule AS PTR
   LOCAL oVers	AS WinDLLVersion
	IF glThemeDllLoaded
		RETURN TRUE
	ENDIF

	glThemeDllLoaded := FALSE
	// Check if the app is using the newer
	// common control for XP visual styles
	oVers := WinDLLVersion{"COMCTL32"}
	IF oVers:MajorVersion >= 6
		// Get the theme DLL
		IF (hModule := LoadLibrary(String2Psz("uxtheme.dll"))) != NULL_PTR
			// Find the function we need
			gfpEnableThemeDialogTexture  := GetProcAddress(hModule, String2Psz("EnableThemeDialogTexture"))
			gfpCloseThemeData            := GetProcAddress(hModule, String2Psz("CloseThemeData"))
			gfpDrawThemeBackground       := GetProcAddress(hModule, String2Psz("DrawThemeBackground"))
			gfpDrawThemeParentBackground := GetProcAddress(hModule, String2Psz("DrawThemeParentBackground"))
			gfpDrawThemeEdge             := GetProcAddress(hModule, String2Psz("DrawThemeEdge"))
			gfpOpenThemeData             := GetProcAddress(hModule, String2Psz("OpenThemeData"))
			gfpGetThemeAppProperties     := GetProcAddress(hModule, String2Psz("GetThemeAppProperties")) //SE-060526
			gfpGetThemeColor             := GetProcAddress(hModule, String2Psz("GetThemeColor"))
			gfpGetWindowTheme            := GetProcAddress(hModule, String2Psz("GetWindowTheme"))
			gfpSetThemeAppProperties     := GetProcAddress(hModule, String2Psz("SetThemeAppProperties"))
			gfpSetWindowTheme            := GetProcAddress(hModule, String2Psz("SetWindowTheme"))
			gfpIsAppThemed               := GetProcAddress(hModule, String2Psz("IsAppThemed")) //SE-060526

			glThemeDLLLoaded := TRUE

			EnableAppVisualTheme()

		ENDIF
	ENDIF

	RETURN glThemeDLLLoaded

FUNCTION AllocUnicodeString(cValue AS STRING) AS PTR STRICT
   LOCAL pUnicodeString AS PTR
	LOCAL dwSize         AS DWORD

   dwSize := SLen(cValue)
   IF (pUnicodeString := MemAlloc((dwSize + 1) * _SizeOf(WORD))) != Null_Ptr
      // Transform the string
      MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, String2Psz(cValue), INT(dwSize), pUnicodeString, INT(dwSize))
   ENDIF

   RETURN pUnicodeString
FUNCTION EnableAppVisualTheme(lEnable := TRUE AS LOGIC) AS LOGIC STRICT
	//SE-060526
	LOCAL dwStyle AS DWORD

	IF lEnable
		dwStyle := _Or(STAP_ALLOW_NONCLIENT, STAP_ALLOW_CONTROLS, STAP_ALLOW_WEBCONTENT)
	ELSE
		dwStyle := STAP_ALLOW_NONCLIENT
	ENDIF

   SetThemeAppProperties(dwStyle)

   RETURN VerifyThemeState()

FUNCTION GetThemeAppProperties() AS DWORD STRICT
   //SE-060526
   IF gfpGetThemeAppProperties != Null_Ptr
      RETURN DWORD(_CAST, PCALL(gfpGetThemeAppProperties))
   ENDIF
	RETURN 0u

FUNCTION GetWindowTheme(hWnd AS PTR) AS PTR STRICT
   IF gfpGetWindowTheme != Null_Ptr
      RETURN PCALL(gfpGetWindowTheme, hWnd)
   ENDIF
   RETURN Null_Ptr

// dcaton 070625 - added prototypes so PCALL() can work.  
// Somehow this compiles in VO with these typed simply as PTR,
// but Vulcan needs parameter and return type
// information in order to generate proper IL code.

STATIC FUNCTION _CloseThemeData( hTheme AS PTR ) AS INT PASCAL
   RETURN 0
   
STATIC FUNCTION _DrawThemeBackground( hTheme AS PTR, hDC AS PTR, iPartId AS INT, iStateId AS INT, pRect AS PTR, pClipRect AS PTR ) AS INT PASCAL
   RETURN 0

STATIC FUNCTION _DrawThemeEdge( hTheme AS PTR, hDC AS PTR, iPartId AS INT, iStateId AS INT, pDestRect AS PTR, uEdge AS DWORD, uFlags AS DWORD, pContentRect AS PTR ) AS INT PASCAL
   RETURN 0
   
STATIC FUNCTION _DrawThemeParentBackground( hTheme AS PTR, hDC AS PTR, prc AS PTR ) AS INT PASCAL
   RETURN 0
   
STATIC FUNCTION _EnableThemeDialogTexture( hwnd AS PTR, dwFlags AS DWORD ) AS INT PASCAL
   RETURN 0
   
STATIC FUNCTION _GetThemeAppProperties() AS DWORD PASCAL
   RETURN 0
   
STATIC FUNCTION _GetThemeColor( hTheme AS PTR, iPartId AS INT, iStateId AS INT, iPropId AS INT, pColor AS PTR ) AS INT PASCAL
   RETURN 0

STATIC FUNCTION _GetWindowTheme( hWnd AS PTR ) AS PTR PASCAL
   RETURN 0
   
STATIC FUNCTION _OpenThemeData( hWnd AS PTR, lpcwstr AS PTR ) AS PTR PASCAL
   RETURN 0

STATIC FUNCTION _SetThemeAppProperties( dwFlags AS DWORD ) AS VOID PASCAL
   RETURN   
   
STATIC FUNCTION _SetWindowTheme( hWnd AS PTR, SubAppName AS PTR, SubIdList AS PTR ) AS PTR PASCAL
   RETURN 0   

STATIC FUNCTION _IsAppThemed() AS LOGIC PASCAL
   RETURN FALSE   

STATIC GLOBAL gfpCloseThemeData            AS _CloseThemeData PTR
STATIC GLOBAL gfpDrawThemeBackground       AS _DrawThemeBackground PTR
STATIC GLOBAL gfpDrawThemeEdge             AS _DrawThemeEdge PTR
STATIC GLOBAL gfpDrawThemeParentBackground AS _DrawThemeParentBackground PTR
STATIC GLOBAL gfpEnableThemeDialogTexture  AS _EnableThemeDialogTexture PTR
STATIC GLOBAL gfpGetThemeAppProperties     AS _GetThemeAppProperties PTR
STATIC GLOBAL gfpGetThemeColor             AS _GetThemeColor PTR
STATIC GLOBAL gfpGetWindowTheme            AS _GetWindowTheme PTR
STATIC GLOBAL gfpOpenThemeData             AS _OpenThemeData PTR
STATIC GLOBAL gfpSetThemeAppProperties     AS _SetThemeAppProperties PTR
STATIC GLOBAL gfpSetWindowTheme            AS _SetWindowTheme PTR
STATIC GLOBAL gfpIsAppThemed               AS _IsAppThemed PTR
STATIC GLOBAL glThemeDLLLoaded AS LOGIC
STATIC GLOBAL glThemeEnabled AS LOGIC

FUNCTION IsAppThemed() AS LOGIC STRICT
   //SE-060526
   IF gfpIsAppThemed != Null_Ptr
      RETURN LOGIC(_CAST, PCALL(gfpIsAppThemed))
   ENDIF
   RETURN FALSE

FUNCTION IsThemeEnabled() AS LOGIC STRICT
	RETURN glThemeEnabled

FUNCTION IsThemeSupported() AS LOGIC STRICT
	RETURN glThemeDLLLoaded

FUNCTION OpenThemeData (hWnd AS PTR, cClassList AS STRING) AS PTR STRICT
   LOCAL pClassList AS PTR
   LOCAL hTheme AS PTR

	IF gfpOpenThemeData != Null_Ptr
   	IF (pClassList := AllocUnicodeString(cClassList)) != Null_Ptr
         hTheme := PCALL(gfpOpenThemeData, hwnd, pClassList)
         MemFree(pClassList)
      ENDIF
   ENDIF
   RETURN hTheme

FUNCTION SetThemeAppProperties(dwFlags AS DWORD) AS VOID STRICT
   IF gfpSetThemeAppProperties != Null_Ptr
      PCALL(gfpSetThemeAppProperties, dwFlags)
   ENDIF
	RETURN

FUNCTION VerifyThemeState() AS LOGIC STRICT
   //SE-060526
   glThemeEnabled := FALSE
   IF IsAppThemed()
   	IF _AND(GetThemeAppProperties(), STAP_ALLOW_CONTROLS) > 0
         glThemeEnabled := TRUE
      ENDIF
   ENDIF
   RETURN glThemeEnabled

FUNCTION CloseThemeData(hTheme AS PTR) AS LONGINT STRICT
   IF gfpCloseThemeData != Null_Ptr
      RETURN LONGINT(_CAST, PCALL(gfpCloseThemeData, hTheme))
   ENDIF
   RETURN S_FALSE
FUNCTION DrawThemeEdge(hTheme AS PTR, hdc AS PTR, iPartId AS INT, iStateId AS INT, pDestRect AS PTR, uEdge AS DWORD, uFlags AS DWORD, pContentRect AS PTR) AS LONGINT STRICT
   IF gfpDrawThemeEdge != Null_Ptr
      RETURN LONGINT(_CAST, PCALL(gfpDrawThemeEdge, hTheme, hdc, iPartId, iStateId, pDestRect, uEdge, uFlags, pContentRect))
   ENDIF
   RETURN S_FALSE

FUNCTION DrawThemeParentBackground(hwnd AS PTR, hDC AS PTR, pRect AS PTR) AS LOGIC STRICT
   LOCAL sRect IS _winRect
	//PP-030910
	IF ! gfpDrawThemeParentBackground == NULL_PTR
		IF pRect = Null_Ptr
			pRect := @sRect
			GetClientRect(hWnd, pRect)
		ENDIF
		RETURN LONGINT(_CAST, PCALL(gfpDrawThemeParentBackground, hWnd, hDC, pRect)) == S_OK
	ENDIF
	RETURN FALSE

FUNCTION DrawThemeBackground(hTheme AS PTR, hdc AS PTR, iPartId AS INT, iStateId AS INT, pRect AS PTR, pClipRect AS PTR) AS LONGINT STRICT
	//PP-040410
	IF gfpDrawThemeBackground != Null_Ptr
		RETURN LONGINT(_CAST, PCALL(gfpDrawThemeBackground, hTheme, hdc, iPartId, iStateId, pRect, pClipRect))
	ENDIF
	RETURN S_FALSE


FUNCTION EnableThemeDialogTexture(oWindow AS USUAL, dwStyle AS DWORD) AS LOGIC STRICT
	//PP-030909
   LOCAL hwnd AS PTR            

   IF ! gfpEnableThemeDialogTexture == NULL_PTR .and. glThemeEnabled
   	IF IsInstanceOfUsual(oWindow, #Window)
   		IF IsInstanceOf(oWindow, #DataWindow)
   			oWindow := oWindow:__GetFormSurface()
   		ENDIF
	      IF (hWnd := oWindow:Handle()) != NULL_PTR
	         RETURN LONGINT(_CAST, PCALL(gfpEnableThemeDialogTexture, hWnd, dwStyle)) == S_OK
	      ENDIF
	   ENDIF
   ENDIF
   RETURN FALSE

FUNCTION GetThemeColor (hTheme AS PTR, iPartId AS INT, iStateId AS INT, iPropID AS INT, pColor AS PTR) AS LONGINT STRICT
   IF gfpGetThemeColor != Null_Ptr
      RETURN LONGINT(_CAST, PCALL(gfpGetThemeColor, hTheme, iPartId, iStateId, iPropID, pColor))
   ENDIF
   RETURN S_FALSE

FUNCTION SetWindowTheme(hWnd AS PTR, cAppName AS STRING, cIdList AS STRING) AS LOGIC STRICT
	//PP-030910
	LOCAL pAppName AS PTR
	LOCAL pIdList  AS PTR
	LOCAL lResult AS LOGIC

	IF ! gfpSetWindowTheme == NULL_PTR
#ifndef __VULCAN__
		//DHer-110622 : Replaced !Empty() with SLen() > 0
		IF SLen(cAppName) > 0
			pAppName := String2W(cAppName)
		ENDIF
		IF SLen(cIdList) > 0
			pIdList := String2W(cIdList)
		ENDIF
#else
		pAppName := String2W(cAppName)
		pIdList := String2W(cIdList)
#endif
		lResult := LONGINT(_CAST, PCALL(gfpSetWindowTheme, hWnd, pAppName, pIdList)) == S_OK
		IF ! pAppName == NULL_PTR
			SysFreeString(pAppName)
		ENDIF
		IF ! pIdList == NULL_PTR
			SysFreeString(pIdList)
		ENDIF
	ENDIF

	RETURN lResult



#region defines
DEFINE CBXS_DISABLED := 4l
DEFINE CBXS_HOT := 2l
DEFINE CBXS_NORMAL := 1l
DEFINE CBXS_PRESSED := 3l
DEFINE CP_DROPDOWNBUTTON := 1l
DEFINE ETDT_DISABLE := 0x01
DEFINE ETDT_ENABLE := 0x02
DEFINE ETDT_ENABLETAB := 0x06
DEFINE ETDT_USETABTEXTURE := 0x04
DEFINE STAP_ALLOW_CONTROLS := 0x02
DEFINE STAP_ALLOW_NONCLIENT := 0x01
DEFINE STAP_ALLOW_WEBCONTENT := 0x04
#endregion
