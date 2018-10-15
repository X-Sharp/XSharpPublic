VOSTRUCT _winDRAGINFO ALIGN 1
	MEMBER  uSize AS DWORD
	MEMBER  pt IS _winPOINT
	MEMBER  fNC AS LOGIC
	MEMBER  lpFileList  AS PSZ
	MEMBER  grfKeyState AS DWORD


VOSTRUCT _winAppBarData ALIGN 1
	MEMBER  cbSize AS DWORD
	MEMBER  hWnd AS PTR
	MEMBER  uCallbackMessage AS DWORD
	MEMBER  uEdge AS DWORD
	MEMBER  rc IS _winRECT
	MEMBER  lParam AS LONGINT

VOSTRUCT _winSHFILEOPSTRUCT ALIGN 1
	MEMBER  hWnd AS PTR
	MEMBER  wFunc AS DWORD
	MEMBER  pFrom AS PSZ
	MEMBER  pTo AS PSZ
	MEMBER  fFlags AS WORD     //FILEOP_FLAGS
	MEMBER  fAnyOperationsAborted AS LOGIC
	MEMBER  hNameMappings AS PTR
	MEMBER  lpszProgressTitle AS PSZ // only used if FOF_SIMPLEPROGRESS

VOSTRUCT _winSHNAMEMAPPING ALIGN 1
	MEMBER   pszOldPath AS PSZ
	MEMBER  pszNewPath AS PSZ
	MEMBER  cchOldPath AS INT
	MEMBER  cchNewPath AS INT

VOSTRUCT _winSHELLEXECUTEINFO ALIGN 1
	MEMBER  cbSize AS DWORD
	MEMBER  fMask AS DWORD
	MEMBER  hWnd AS PTR
	MEMBER  lpVerb AS PSZ
	MEMBER  lpFile AS PSZ
	MEMBER  lpParameters AS PSZ
	MEMBER  lpDirectory AS PSZ
	MEMBER  nShow AS INT
	MEMBER  hInstApp AS PTR
	MEMBER  lpIDList AS PTR
	MEMBER  lpClass AS PSZ
	MEMBER  hkeyClass AS PTR
	MEMBER  dwHotKey AS DWORD
	MEMBER  hIcon AS PTR
	MEMBER  hProcess AS PTR

VOSTRUCT _winNOTIFYICONDATA  ALIGN 1
	MEMBER  cbSize AS DWORD
	MEMBER  hWnd AS PTR
	MEMBER  uID AS DWORD
	MEMBER  uFlags AS DWORD
	MEMBER  uCallbackMessage AS DWORD
	MEMBER  hIcon AS PTR
	MEMBER  DIM  szTip[128] AS BYTE
	//PP-030902
	MEMBER dwState AS DWORD  // Shell Version 5
	MEMBER dwStateMask AS DWORD  // Shell Version 5
	MEMBER DIM szInfo[256] AS BYTE  // Shell Version 5
	MEMBER uTimeoutVersion IS _winNOTIFYICONTIMEOUTVERSION // Shell Version 5
	MEMBER DIM szInfoTitle[64] AS BYTE  // Shell Version 5
	MEMBER dwInfoFlags AS DWORD  // Shell Version 5
	MEMBER guidItem IS _WINGUID  // Shell Version 6

VOSTRUCT _winSHFILEINFO ALIGN 1
	MEMBER  hIcon AS PTR
	MEMBER  iIcon AS INT
	MEMBER  dwAttributes AS DWORD
	MEMBER  DIM szDisplayName[MAX_PATH] AS BYTE
	MEMBER  DIM szTypeName[80] AS BYTE

_DLL FUNCTION DragQueryFile( hDrop AS PTR, iFile AS DWORD, lpszFile AS PSZ, cch AS DWORD);
	AS DWORD PASCAL:SHELL32.DragQueryFileA

_DLL FUNCTION DragQueryPoint( hDrop AS PTR, lppt AS _winPOINT);
	AS LOGIC PASCAL:SHELL32.DragQueryPoint

_DLL FUNCTION DragFinish(hDrop AS PTR) AS VOID PASCAL:SHELL32.DragFinish

_DLL FUNCTION DragAcceptFiles(hWnd AS PTR, fAccept AS LOGIC);
	AS VOID PASCAL:SHELL32.DragAcceptFiles

_DLL FUNCTION ShellExecute( hWnd AS PTR, lpOperation AS PSZ, lpFile AS PSZ,;
	lpParameters AS PSZ, lpDirectory AS PSZ, nShowCmd AS INT);
	AS PTR PASCAL:SHELL32.ShellExecuteA

_DLL FUNCTION FindExecutable( lpFile AS PSZ, lpDirectory AS PSZ,  lpResult AS PSZ);
	AS PTR PASCAL:SHELL32.FindExecutableA


_DLL FUNCTION CommandLineToArgvW(lpCmdLine AS PSZ,     pNumArgs AS INT PTR) AS PTR PASCAL:SHELL32.CommandLineToArgvW

_DLL FUNCTION ShellAbout(hWnd AS PTR, szApp AS PSZ, szOtherstuff AS PSZ , hIcon AS PTR)AS INT PASCAL:SHELL32.ShellAboutA

_DLL FUNCTION DuplicateIcon(hInst AS PTR, hIcon AS PTR) AS PTR PASCAL:SHELL32.DuplicateIcon

_DLL FUNCTION ExtractAssociatedIcon(hInst AS PTR, lpIconPath AS PSZ, lpiIcon AS WORD PTR);
	AS PTR PASCAL:SHELL32.ExtractAssociatedIconA

_DLL FUNCTION ExtractIcon( hInst AS PTR, lpszExeFileName AS PSZ, nIconIndex AS DWORD);
	AS PTR PASCAL:SHELL32.ExtractIconA

_DLL FUNCTION SHAppBarMessage( dwMessage AS DWORD, pData AS _winAppBarData);
	AS DWORD PASCAL:SHELL32.SHAppBarMessage

_DLL FUNCTION DoEnvironmentSubst(szString AS PSZ, cbString AS DWORD);
	AS DWORD PASCAL:SHELL32.DoEnvironmentSubstA

FUNCTION EIRESID(x) AS INT
	RETURN (INT(_CAST, x) * (-1))


_DLL FUNCTION ExtractIconEx( lpszFile AS PSZ, nIconIndex AS INT,  phiconLarge AS PTR,;
	phiconSmall AS PTR, nIcons AS DWORD);
	AS DWORD PASCAL:SHELL32.ExtractIconExA

_DLL FUNCTION SHFileOperation(lpFileOp AS _winSHFILEOPSTRUCT) AS INT PASCAL:SHELL32.SHFileOperationA

_DLL FUNCTION SHFreeNameMappings(hNameMappings AS PTR) AS VOID PASCAL:SHELL32.SHFreeNameMappings

_DLL FUNCTION ShellExecuteEx(lpExecInfo AS _winSHELLEXECUTEINFO) AS LOGIC PASCAL:SHELL32.ShellExecuteExA

UNION _winNOTIFYICONTIMEOUTVERSION
	//PP-030902
	MEMBER uTimeOut AS DWORD
	MEMBER uVersion AS DWORD

FUNCTION GetDLLMajorVersion(cDLL AS STRING) AS DWORD
	//PP-030902
	LOCAL oVersion AS WinDLLVersion

	oVersion := WinDLLVersion{cDLL}

	RETURN oVersion:MajorVersion

FUNCTION GetShellMajorVersion() AS DWORD
	//PP-030902
	RETURN GetDLLMajorVersion("SHELL32.DLL")


FUNCTION SizeOfNotifyIconData() AS DWORD
	//PP-030902
	LOCAL dwShellVersion AS DWORD
	LOCAL dwNID AS DWORD

	dwShellVersion := GetShellMajorVersion()

	// See _WINNOTIFYICONDATA
	DO CASE
	CASE dwShellVersion = 5
		dwNID := 488
	CASE dwShellVersion = 6
		dwNID := 504
	OTHERWISE
		dwNID := 88
	ENDCASE

	RETURN dwNID

_DLL FUNCTION Shell_NotifyIcon(dwMessage AS DWORD, lpData AS _winNOTIFYICONDATA ) ;
	AS LOGIC PASCAL:SHELL32.Shell_NotifyIconA

/*
 * The SHGetFileInfo API provides an easy way to get attributes
 * for a file given a pathname.
 *
 *   PARAMETERS
 *
 *     pszPath              file name to get info about
 *     dwFileAttributes     file attribs, only used with SHGFI_USEFILEATTRIBUTES
 *     psfi                 place to return file info
 *     cbFileInfo           size of structure
 *     uFlags               flags
 *
 *   RETURN
 *     TRUE if things worked
 */

_DLL FUNCTION SHGetFileInfo( pszPath AS PSZ, dwFileAttributes AS DWORD, psfi AS _winSHFILEINFO,;
	cbFileINFO AS DWORD, uFlags AS DWORD);
	AS DWORD PASCAL:SHELL32.SHGetFileInfoA

FUNCTION ShellAutoComplete(hWnd AS PTR, dwFlags AS DWORD) AS LOGIC
	//PP-030902
	LOCAL hDLL AS PTR
	LOCAL hFunc AS PTR
	LOCAL cDLL AS STRING
	LOCAL lOK AS LOGIC

	cDLL := "shlwapi"

	IF ! (hDLL := GetModuleHandle(String2Psz(cDLL))) == NULL_PTR
		IF ! (hFunc := GetProcAddress(hDLL,String2Psz("SHAutoComplete"))) == NULL_PTR
#ifdef __VULCAN__		
			lOK := PCallNative<INT>(hFunc,hWnd,dwFlags) == S_OK
#else			
			lOK := PCALL(hFunc,hWnd,dwFlags) == S_OK
#endif			
		ENDIF
	ENDIF

	RETURN lOK

FUNCTION GetFolderPath(nFolder AS INT,lCreate := FALSE AS LOGIC,hToken := NULL_PTR AS PTR,;
	dwFlags := SHGFP_TYPE_CURRENT AS DWORD) AS STRING
	//PP-030909 Requires >= IE4 (Shell 4.71)
	LOCAL hwndOwner AS PTR
	LOCAL pPath AS PTR
	LOCAL hDLL AS PTR
	LOCAL hFunc AS PTR
	LOCAL cResult AS STRING
	// LL-070705	Use static buffer to avoid allocating (and freeing..)
	LOCAL	DIM aPath[MAX_PATH] AS BYTE		// LL-070705
	pPath := @aPath[1]	// LL-070705
	IF lCreate
		nFolder := _OR(nFolder,CSIDL_FLAG_CREATE)
	ENDIF
	IF ( hDLL := LoadLibrary(String2Psz("ShFolder.DLL")) ) == NULL_PTR
		hDLL := LoadLibrary(String2Psz("Shell32.DLL"))
	ENDIF

	IF ! hDLL == NULL_PTR
		hFunc := GetProcAddress(hDLL,String2Psz("SHGetFolderPathA"))
	ENDIF

	IF ! hFunc == NULL_PTR
 		//*!!!pPath := MemAlloc(MAX_PATH) // LL-070705. Allocated memory not deallocate 
#ifdef __VULCAN__		
		PCallNative<INT>(hFunc,hwndOwner,nFolder,hToken,dwFlags,pPath)
#else
		PCALL(hFunc,hwndOwner,nFolder,hToken,dwFlags,pPath)
#endif		
		cResult := Psz2String(pPath)
	ELSE
		hFunc := GetProcAddress(hDLL,String2Psz("SHGetSpecialFolderPath"))
		IF ! hFunc == NULL_PTR
#ifdef __VULCAN__		
			PCallNative<INT>(hFunc,hwndOwner,pPath,nFolder,lCreate)
#else			
			PCALL(hFunc,hwndOwner,pPath,nFolder,lCreate)
#endif
			cResult := Psz2String(pPath)
		ENDIF
	ENDIF

	FreeLibrary(hDLL)

	RETURN cResult



#region defines
DEFINE ABM_NEW             := 0x00000000
DEFINE ABM_REMOVE          := 0x00000001
DEFINE ABM_QUERYPOS        := 0x00000002
DEFINE ABM_SETPOS          := 0x00000003
DEFINE ABM_GETSTATE        := 0x00000004
DEFINE ABM_GETTASKBARPOS   := 0x00000005
DEFINE ABM_ACTIVATE        := 0x00000006
DEFINE ABM_GETAUTOHIDEBAR  := 0x00000007
DEFINE ABM_SETAUTOHIDEBAR  := 0x00000008
DEFINE ABM_WINDOWPOSCHANGED := 0x0000009
DEFINE ABM_SETSTATE      := 0x0000000a
DEFINE ABN_STATECHANGE    := 0x0000000
DEFINE ABN_POSCHANGED     := 0x0000001
DEFINE ABN_FULLSCREENAPP  := 0x0000002
DEFINE ABN_WINDOWARRANGE  := 0x0000003
DEFINE ABS_AUTOHIDE    := 0x0000001
DEFINE ABS_ALWAYSONTOP := 0x0000002
DEFINE ABE_LEFT        := 0
DEFINE ABE_TOP         := 1
DEFINE ABE_RIGHT       := 2
DEFINE ABE_BOTTOM      := 3
DEFINE FO_MOVE           := 0x0001
DEFINE FO_COPY        := 0x0002
DEFINE FO_DELETE      := 0x0003
DEFINE FO_RENAME      := 0x0004
DEFINE FOF_MULTIDESTFILES        :=  0x0001
DEFINE FOF_CONFIRMMOUSE          :=  0x0002
DEFINE FOF_SILENT                :=  0x0004
DEFINE FOF_RENAMEONCOLLISION     :=  0x0008
DEFINE FOF_NOCONFIRMATION        :=  0x0010
DEFINE FOF_WANTMAPPINGHANDLE     :=  0x0020
DEFINE FOF_ALLOWUNDO             :=  0x0040
DEFINE FOF_FILESONLY             :=  0x0080
DEFINE FOF_SIMPLEPROGRESS        :=  0x0100
DEFINE FOF_NOCONFIRMMKDIR        :=  0x0200
DEFINE FOF_NOERRORUI 		 :=  0x0400
DEFINE FOF_NOCOPYSECURITYATTRIBS  :=  0x0800  // dont copy NT file Security Attributes
DEFINE FOF_NORECURSION            :=  0x1000  // don't recurse into directories.
DEFINE FOF_NO_CONNECTED_ELEMENTS  :=  0x2000  // don't operate on connected elements.
DEFINE FOF_WANTNUKEWARNING        :=  0x4000  // during delete operation, warn if nuking instead of recycling (partially overrides FOF_NOCONFIRMATION)
DEFINE FOF_NORECURSEREPARSE       :=  0x8000  // treat reparse points as objects, not containers
DEFINE PO_DELETE     := 0x0013
DEFINE PO_RENAME     := 0x0014
DEFINE PO_PORTCHANGE := 0x0020
DEFINE PO_REN_PORT   :=    0x0034  // PO_RENAME and PO_PORTCHANGE at same time.
DEFINE SE_ERR_FNF              := 2
DEFINE SE_ERR_PNF              := 3
DEFINE SE_ERR_ACCESSDENIED     := 5
DEFINE SE_ERR_OOM              := 8
DEFINE SE_ERR_DLLNOTFOUND      := 32
DEFINE SE_ERR_SHARE                    := 26
DEFINE SE_ERR_ASSOCINCOMPLETE          := 27
DEFINE SE_ERR_DDETIMEOUT               := 28
DEFINE SE_ERR_DDEFAIL                  := 29
DEFINE SE_ERR_DDEBUSY                  := 30
DEFINE SE_ERR_NOASSOC                  := 31
DEFINE SEE_MASK_CLASSNAME      := 0x00000001
DEFINE SEE_MASK_CLASSKEY       := 0x00000003
DEFINE SEE_MASK_IDLIST         := 0x00000004
DEFINE SEE_MASK_INVOKEIDLIST   := 0x0000000c
DEFINE SEE_MASK_ICON           := 0x00000010
DEFINE SEE_MASK_HOTKEY         := 0x00000020
DEFINE SEE_MASK_NOCLOSEPROCESS := 0x00000040
DEFINE SEE_MASK_CONNECTNETDRV  := 0x00000080
DEFINE SEE_MASK_FLAG_DDEWAIT   := 0x00000100
DEFINE SEE_MASK_DOENVSUBST     := 0x00000200
DEFINE SEE_MASK_FLAG_NO_UI     := 0x00000400
DEFINE SEE_MASK_UNICODE        := 0x00010000
DEFINE SEE_MASK_NO_CONSOLE        :=  0x00008000
DEFINE SEE_MASK_ASYNCOK           :=  0x00100000
DEFINE SEE_MASK_HMONITOR          :=  0x00200000
DEFINE SEE_MASK_NOZONECHECKS      :=  0x00800000
DEFINE SEE_MASK_NOQUERYCLASSSTORE :=  0x01000000
DEFINE SEE_MASK_WAITFORINPUTIDLE  :=  0x02000000
DEFINE SEE_MASK_FLAG_LOG_USAGE    :=  0x04000000
DEFINE NIN_BALLOONSHOW := (WM_USER + 2)
DEFINE NIN_BALLOONHIDE := (WM_USER + 3)
DEFINE NIN_BALLOONTIMEOUT := (WM_USER + 4)
DEFINE NIN_BALLOONUSERCLICK := (WM_USER + 5)
DEFINE NIM_ADD := 0x00000000
DEFINE NIM_MODIFY := 0x00000001
DEFINE NIM_DELETE := 0x00000002
DEFINE NIM_SETFOCUS    := 0x00000003
DEFINE NIM_SETVERSION := 0x00000004
DEFINE NOTIFYICON_VERSION := 0x00000003
DEFINE NIF_MESSAGE := 0x00000001
DEFINE NIF_ICON := 0x00000002
DEFINE NIF_TIP := 0x00000004
DEFINE NIF_STATE       := 0x00000008
DEFINE NIF_INFO := 0x00000010
DEFINE NIF_GUID        := 0x00000020
DEFINE NIS_HIDDEN              := 0x00000001
DEFINE NIS_SHAREDICON          := 0x00000002
// says this is the source of a shared icon
// Notify Icon Infotip flags
DEFINE NIIF_NONE := 0x00
// icon flags are mutually exclusive
// and take only the lowest 2 bits
DEFINE NIIF_INFO := 0x01
DEFINE NIIF_WARNING := 0x02
DEFINE NIIF_ERROR := 0x03
DEFINE NIIF_USER       := 0x00000004
DEFINE NIIF_ICON_MASK := 0x0000000F
DEFINE NIIF_NOSOUND := 0x00000010
DEFINE SHGFI_ICON              := 0x00000100
DEFINE SHGFI_DISPLAYNAME       := 0x00000200
DEFINE SHGFI_TYPENAME          := 0x00000400
DEFINE SHGFI_ATTRIBUTES        := 0x00000800
DEFINE SHGFI_ICONLOCATION      := 0x00001000
DEFINE SHGFI_EXETYPE           := 0x00002000
DEFINE SHGFI_SYSICONINDEX      := 0x00004000
DEFINE SHGFI_LINKOVERLAY       := 0x00008000
DEFINE SHGFI_SELECTED          := 0x00010000
DEFINE SHGFI_ATTR_SPECIFIED    := 0x00020000     // get only specified attributes
DEFINE SHGFI_LARGEICON         := 0x00000000
DEFINE SHGFI_SMALLICON         := 0x00000001
DEFINE SHGFI_OPENICON          := 0x00000002
DEFINE SHGFI_SHELLICONSIZE     := 0x00000004
DEFINE SHGFI_PIDL              := 0x00000008
DEFINE SHGFI_USEFILEATTRIBUTES := 0x00000010
DEFINE SHGFI_ADDOVERLAYS       := 0x00000020     // apply the appropriate overlays
DEFINE SHGFI_OVERLAYINDEX      := 0x00000040     // Get the index of the overlay
DEFINE SHGNLI_PIDL             := 0x00000001
DEFINE SHGNLI_PREFIXNAME       := 0x00000002
DEFINE SHGNLI_NOUNIQUE         := 0x00000004     // don't do the unique name generation
DEFINE SHGNLI_NOLNK            := 0x00000008     // don't add ".lnk" extension
DEFINE SHACF_DEFAULT := 0x00000000  // Currently (SHACF_FILESYSTEM | SHACF_URLALL)
DEFINE SHACF_FILESYSTEM := 0x00000001  // This includes the File System as well as the rest of the shell (Desktop\My Computer\Control Panel\)
DEFINE SHACF_URLALL := 0x00000006 // _or(SHACF_URLHISTORY,SHACF_URLMRU)
DEFINE SHACF_URLHISTORY := 0x00000002  // URLs in the User's History
DEFINE SHACF_URLMRU := 0x00000004  // URLs in the User's Recently Used list.
DEFINE SHACF_USETAB := 0x00000008  // Use the tab to move thru the autocomplete possibilities instead of to the next dialog/window control.
DEFINE SHACF_FILESYS_ONLY := 0x00000010  // This includes the File System
DEFINE SHACF_FILESYS_DIRS := 0x00000020  // Same as SHACF_FILESYS_ONLY except it only includes directories, UNC servers, and UNC server shares.
DEFINE SHACF_AUTOSUGGEST_FORCE_ON := 0x10000000  // Ignore the registry default and force the feature on.
DEFINE SHACF_AUTOSUGGEST_FORCE_OFF := 0x20000000  // Ignore the registry default and force the feature off.
DEFINE SHACF_AUTOAPPEND_FORCE_ON := 0x40000000  // Ignore the registry default and force the feature on. (Also know as AutoComplete)
DEFINE SHACF_AUTOAPPEND_FORCE_OFF := 0x80000000  // Ignore the registry default and force the feature off. (Also know as AutoComplete)
DEFINE SHGFP_TYPE_CURRENT := 0   // current value for user, verify it exists
DEFINE SHGFP_TYPE_DEFAULT := 1   // default value, may not exist
DEFINE CSIDL_DESKTOP                   := 0x0000
DEFINE CSIDL_INTERNET                  := 0x0001
DEFINE CSIDL_PROGRAMS                  := 0x0002
DEFINE CSIDL_CONTROLS                  := 0x0003
DEFINE CSIDL_PRINTERS                  := 0x0004
DEFINE CSIDL_PERSONAL 		       		:= 0x0005      // My Documents
DEFINE CSIDL_FAVORITES                 := 0x0006
DEFINE CSIDL_STARTUP                   := 0x0007
DEFINE CSIDL_RECENT                    := 0x0008
DEFINE CSIDL_SENDTO                    := 0x0009
DEFINE CSIDL_BITBUCKET                 := 0x000A
DEFINE CSIDL_STARTMENU                 := 0x000B
DEFINE CSIDL_MYDOCUMENTS               := 0x000c        // logical "My Documents" desktop icon
DEFINE CSIDL_MYMUSIC                   := 0x000d        // "My Music" folder
DEFINE CSIDL_MYVIDEO                   := 0x000e        // "My Videos" folder
DEFINE CSIDL_DESKTOPDIRECTORY          := 0x0010
DEFINE CSIDL_DRIVES                    := 0x0011
DEFINE CSIDL_NETWORK                   := 0x0012
DEFINE CSIDL_NETHOOD                   := 0x0013
DEFINE CSIDL_FONTS                     := 0x0014
DEFINE CSIDL_TEMPLATES                 := 0x0015
DEFINE CSIDL_COMMON_STARTMENU          := 0x0016
DEFINE CSIDL_COMMON_PROGRAMS           := 0x0017
DEFINE CSIDL_COMMON_STARTUP            := 0x0018
DEFINE CSIDL_COMMON_DESKTOPDIRECTORY   := 0x0019
DEFINE CSIDL_APPDATA 						:= 0x001A      // Application Data, new for NT4
DEFINE CSIDL_PRINTHOOD                 := 0x001B
DEFINE CSIDL_LOCAL_APPDATA 				:= 0x001C      // non roaming, user\Local Settings\Application Data
DEFINE CSIDL_ALTSTARTUP                := 0x001D         // DBCS
DEFINE CSIDL_COMMON_ALTSTARTUP         := 0x001E         // DBCS
DEFINE CSIDL_COMMON_FAVORITES          := 0x001F
DEFINE CSIDL_INTERNET_CACHE 				:= 0x0020
DEFINE CSIDL_COOKIES 						:= 0x0021
DEFINE CSIDL_HISTORY 						:= 0x0022
DEFINE CSIDL_COMMON_APPDATA 				:= 0x0023      // All Users\Application Data
DEFINE CSIDL_WINDOWS 						:= 0x0024      // GetWindowsDirectory()
DEFINE CSIDL_SYSTEM 							:= 0x0025      // GetSystemDirectory()
DEFINE CSIDL_PROGRAM_FILES 				:= 0x0026      // C:\Program Files
DEFINE CSIDL_MYPICTURES 					:= 0x0027      // My Pictures, new for Win2K
DEFINE CSIDL_PROFILE                   := 0x0028        // USERPROFILE
DEFINE CSIDL_SYSTEMX86                 := 0x0029        // x86 system directory on RISC
DEFINE CSIDL_PROGRAM_FILESX86          := 0x002a        // x86 C:\Program Files on RISC
DEFINE CSIDL_PROGRAM_FILES_COMMON      := 0x002b      // C:\Program Files\Common
DEFINE CSIDL_PROGRAM_FILES_COMMONX86   := 0x002c        // x86 Program Files\Common on RISC
DEFINE CSIDL_COMMON_TEMPLATES          := 0x002d        // All Users\Templates
DEFINE CSIDL_COMMON_DOCUMENTS          := 0x002e      // All Users\Documents
DEFINE CSIDL_COMMON_ADMINTOOLS         := 0x002f      // All Users\Start Menu\Programs\Administrative Tools
DEFINE CSIDL_ADMINTOOLS                := 0x0030
DEFINE CSIDL_CONNECTIONS               := 0x0031        // Network and Dial-up Connections
DEFINE CSIDL_COMMON_MUSIC              := 0x0035        // All Users\My Music
DEFINE CSIDL_COMMON_PICTURES           := 0x0036        // All Users\My Pictures
DEFINE CSIDL_COMMON_VIDEO              := 0x0037        // All Users\My Video
DEFINE CSIDL_RESOURCES 						:= 0x0038      // %windir%\Resources\, For theme and other windows resources.
DEFINE CSIDL_RESOURCES_LOCALIZED 		:= 0x0039      // %windir%\Resources\<LangID>, for theme and other windows specific resources.
DEFINE CSIDL_COMMON_OEM_LINKS          := 0x003a        // Links to All Users OEM specific apps
DEFINE CSIDL_CDBURN_AREA               := 0x003b        // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
// unused                               0x003c
DEFINE CSIDL_COMPUTERSNEARME           := 0x003d        // Computers Near Me (computered from Workgroup membership)
DEFINE CSIDL_FLAG_CREATE               := 0x8000      // new for Win2K, or this in to force creation of folder
DEFINE CSIDL_FLAG_DONT_VERIFY          := 0x4000        // combine with CSIDL_ value to return an unverified folder path
DEFINE CSIDL_FLAG_NO_ALIAS             := 0x1000        // combine with CSIDL_ value to insure non-alias versions of the pidl
DEFINE CSIDL_FLAG_PER_USER_INIT        := 0x0800        // combine with CSIDL_ value to indicate per-user init (eg. upgrade)
DEFINE CSIDL_FLAG_MASK                 := 0xFF00        // mask for all possible flag values
//
//  File System Notification flags
//
DEFINE SHCNE_RENAMEITEM          := 0x00000001L
DEFINE SHCNE_CREATE              := 0x00000002L
DEFINE SHCNE_DELETE              := 0x00000004L
DEFINE SHCNE_MKDIR               := 0x00000008L
DEFINE SHCNE_RMDIR               := 0x00000010L
DEFINE SHCNE_MEDIAINSERTED       := 0x00000020L
DEFINE SHCNE_MEDIAREMOVED        := 0x00000040L
DEFINE SHCNE_DRIVEREMOVED        := 0x00000080L
DEFINE SHCNE_DRIVEADD            := 0x00000100L
DEFINE SHCNE_NETSHARE            := 0x00000200L
DEFINE SHCNE_NETUNSHARE          := 0x00000400L
DEFINE SHCNE_ATTRIBUTES          := 0x00000800L
DEFINE SHCNE_UPDATEDIR           := 0x00001000L
DEFINE SHCNE_UPDATEITEM          := 0x00002000L
DEFINE SHCNE_SERVERDISCONNECT    := 0x00004000L
DEFINE SHCNE_UPDATEIMAGE         := 0x00008000L
DEFINE SHCNE_DRIVEADDGUI         := 0x00010000L
DEFINE SHCNE_RENAMEFOLDER        := 0x00020000L
DEFINE SHCNE_FREESPACE           := 0x00040000L
DEFINE SHCNE_EXTENDED_EVENT      := 0x04000000L
DEFINE SHCNE_ASSOCCHANGED        := 0x08000000L
DEFINE SHCNE_DISKEVENTS          := 0x0002381FL
DEFINE SHCNE_GLOBALEVENTS        := 0x0C0581E0L // Events that dont match pidls first
DEFINE SHCNE_ALLEVENTS           := 0x7FFFFFFFL
DEFINE SHCNE_INTERRUPT           := 0x80000000L // The presence of this flag indicates
                                            // that the event was generated by an
                                            // interrupt.  It is stripped out before
                                            // the clients of SHCNNotify_ see it.
DEFINE SHCNEE_ORDERCHANGED         := 2L  // pidl2 is the changed folder
DEFINE SHCNEE_MSI_CHANGE           := 4L  // pidl2 is a SHChangeProductKeyAsIDList
DEFINE SHCNEE_MSI_UNINSTALL        := 5L  // pidl2 is a SHChangeProductKeyAsIDList
DEFINE SHCNF_IDLIST      := 0x0000        // LPITEMIDLIST
DEFINE SHCNF_PATHA       := 0x0001        // path name
DEFINE SHCNF_PRINTERA    := 0x0002        // printer friendly name
DEFINE SHCNF_DWORD       := 0x0003        // DWORD
DEFINE SHCNF_PATHW       := 0x0005        // path name
DEFINE SHCNF_PRINTERW    := 0x0006        // printer friendly name
DEFINE SHCNF_TYPE        := 0x00FF
DEFINE SHCNF_FLUSH       := 0x1000
DEFINE SHCNF_FLUSHNOWAIT := 0x2000
DEFINE SHCNF_PATH        := SHCNF_PATHA
DEFINE SHCNF_PRINTER     := SHCNF_PRINTERA
#endregion
