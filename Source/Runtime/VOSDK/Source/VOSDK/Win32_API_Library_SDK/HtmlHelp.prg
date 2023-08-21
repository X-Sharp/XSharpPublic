VOSTRUCT _winHH_AKLINK
	MEMBER cbStruct AS INT    // sizeof this structure
	MEMBER fReserved AS LOGIC // must be FALSE (really!)
	MEMBER pszKeywords AS PSZ // semi-colon separated keywords
	MEMBER pszUrl AS PSZ      // URL to jump to if no keywords found (may be NULL)
	MEMBER pszMsgText AS PSZ  // Message text to display in MessageBox if pszUrl is NULL and no keyword match
	MEMBER pszMsgTitle AS PSZ // Message text to display in MessageBox if pszUrl is NULL and no keyword match
	MEMBER pszWindow AS PSZ   // Window to display URL in
	MEMBER fIndexOnFail AS LOGIC // Displays index if keyword lookup fails.

_DLL FUNC HTMLHelp(hwndCaller AS PTR, pszFile AS PSZ, uCommand AS LONG, dwData AS LONG) AS LONG PASCAL:HHCTRL.OCX.HtmlHelpA



#region defines
DEFINE HH_DISPLAY_TOPIC        := 0x0000
DEFINE HH_HELP_FINDER          := 0x0000  // WinHelp equivalent
DEFINE HH_DISPLAY_TOC          := 0x0001  // not currently implemented
DEFINE HH_DISPLAY_INDEX        := 0x0002  // not currently implemented
DEFINE HH_DISPLAY_SEARCH       := 0x0003  // not currently implemented
DEFINE HH_SET_WIN_TYPE         := 0x0004
DEFINE HH_GET_WIN_TYPE         := 0x0005
DEFINE HH_GET_WIN_HANDLE       := 0x0006
DEFINE HH_ENUM_INFO_TYPE       := 0x0007  // Get Info type name, call repeatedly to enumerate, -1 at end
DEFINE HH_SET_INFO_TYPE        := 0x0008  // Add Info type to filter.
DEFINE HH_SYNC                 := 0x0009
DEFINE HH_RESERVED1            := 0x000A
DEFINE HH_RESERVED2            := 0x000B
DEFINE HH_RESERVED3            := 0x000C
DEFINE HH_KEYWORD_LOOKUP       := 0x000D
DEFINE HH_DISPLAY_TEXT_POPUP   := 0x000E  // display string resource id or text in a popup window
DEFINE HH_HELP_CONTEXT         := 0x000F  // display mapped numeric value in dwData
DEFINE HH_TP_HELP_CONTEXTMENU  := 0x0010  // text popup help, same as WinHelp HELP_CONTEXTMENU
DEFINE HH_TP_HELP_WM_HELP      := 0x0011  // text popup help, same as WinHelp HELP_WM_HELP
DEFINE HH_CLOSE_ALL            := 0x0012  // close all windows opened directly or indirectly by the caller
DEFINE HH_ALINK_LOOKUP         := 0x0013  // ALink version of HH_KEYWORD_LOOKUP
DEFINE HH_GET_LAST_ERROR       := 0x0014  // not currently implemented // See HHERROR.h
DEFINE HH_ENUM_CATEGORY        := 0x0015	// Get category name, call repeatedly to enumerate, -1 at end
DEFINE HH_ENUM_CATEGORY_IT     := 0x0016  // Get category info type members, call repeatedly to enumerate, -1 at end
DEFINE HH_RESET_IT_FILTER      := 0x0017  // Clear the info type filter of all info types.
DEFINE HH_SET_INCLUSIVE_FILTER := 0x0018  // set inclusive filtering method for untyped topics to be included in display
DEFINE HH_SET_EXCLUSIVE_FILTER := 0x0019  // set exclusive filtering method for untyped topics to be excluded from display
DEFINE HH_INITIALIZE            := 0x001C  // Initializes the help system.
DEFINE HH_UNINITIALIZE          := 0x001D  // Uninitializes the help system.
DEFINE HH_PRETRANSLATEMESSAGE  := 0x00fd  // Pumps messages. (NULL, NULL, MSG*).
DEFINE HH_SET_GLOBAL_PROPERTY  := 0x00fc  // Set a global property. (NULL, NULL, HH_GPROP)
#endregion
