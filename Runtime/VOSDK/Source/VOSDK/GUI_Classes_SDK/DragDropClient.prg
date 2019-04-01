CLASS DragDropClient INHERIT VObject
	PROTECT hOwner AS PTR
	PROTECT oParent AS Window
	//SE-060520
	//PROTECT ptrOldWinProc AS PTR
	//PROTECT __ptrOldSelf AS PTR //Riz compiler bug. protect creates and error when using @ (address of)

METHOD Destroy() 
	

	IF (hOwner != NULL_PTR)
		PCALL(gpfnDragAcceptFiles, hOwner,FALSE)
		//Restore the old object ptr in the window class
		//SetWindowLong(hOwner, DWL_User, long(_cast, __ptrOldSelf))
		//UnRegisterKid(@__ptrOldSelf)
		//Restore the old proc ptr
		//SetWindowLong(hOwner, GWL_WNDPROC, long(_cast, ptrOldWinProc))
	ENDIF

	IF !InCollect()
		UnregisterAxit(SELF)
		oParent := NULL_OBJECT
		hOwner := NULL_PTR
	ENDIF

	SUPER:Destroy()

	RETURN NIL

METHOD Dispatch(oEvent) 
	//SE-060520
	LOCAL oEvt AS @@Event
	LOCAL uMsg AS DWORD

	
	oEvt := oEvent
	uMsg := oEvt:uMsg

	SWITCH uMsg
	CASE WM_DRAGSELECT
		IF (oEvt:wParam == 0)
			SELF:DragLeave(oEvt)
		ENDIF
	CASE WM_DROPFILES 
		SELF:Drop(DragEvent{oEvt})
		PCALL(gpfnDragFinish, PTR(_CAST, oEvt:wParam))
	CASE WM_QUERYDROPOBJECT
		oParent:EventReturnvalue := IIF(SELF:DragOver(DragEvent{oEvt}), 0L, 1L)
	END SWITCH

	RETURN NIL

METHOD DragLeave(oEvent) 
	

	IF IsMethod(oParent, #DragLeave)
		Send(oParent, #DragLeave, oEvent)
	ENDIF

	RETURN NIL

METHOD DragOver(oDragEvent) 
	

	IF (IsMethod(oParent, #DragOver))
		RETURN Send(oParent, #DragOver, oDragEvent)
	ENDIF

	RETURN TRUE

METHOD Drop(oDragEvent) 
	

	IF IsMethod(oParent, #Drop)
		Send(oParent, #Drop, oDragEvent)
	ENDIF

	RETURN NIL

CONSTRUCTOR(oOwner) 
	

	IF !IsInstanceOfUsual(oOwner,#Window)
		WCError{#Init,#DragDropClient,__WCSTypeError,oOwner,1}:@@Throw()
	ENDIF

	IF !glShellDllLoaded
		__LoadShellDll()
	ENDIF

	
	oParent := oOwner
	hOwner := oParent:Handle()  
	//SE-070501 for better	Datawindow support
	IF IsInstanceOf(oParent, #__FormFrame)
		oParent := IVarGet(oParent, #DataWindow)
	ELSEIF IsInstanceOf(oParent, #__FormDialogWindow)
		oParent := IVarGet(oParent:Owner, #DataWindow) 
	ENDIF
	IF oParent = NULL_OBJECT
		oParent := oOwner
	ENDIF    

	//Put the address of our proc in the window class and save the old one
	//	ptrOldWinProc := SetWindowLong(hOwner, GWL_WNDPROC, long(_cast, @__WCDDClientProc()))
	//Put the ptr to this object in the window class and save the old object ptr
	//	RegisterKid(@__ptrOldSelf,1,false)
	//	__ptrOldSelf := SetWindowLong(hOwner, DWL_User, long(_cast,self))
	PCALL(gpfnDragAcceptFiles, hOwner, TRUE)

	RETURN 

ACCESS Owner 
	

	RETURN oParent
END CLASS

GLOBAL glShellDllLoaded := FALSE AS LOGIC

	// Function Declarations
GLOBAL gpfnDragAcceptFiles AS TDragAcceptFiles PTR
GLOBAL gpfnDragFinish AS TDragFinish PTR
GLOBAL gpfnDragQueryFile AS TDragQueryFile PTR
//GLOBAL gpfnDragQueryPoint AS TDragQueryPoint PTR
	// the following are used by StandardFolderDialog
GLOBAL gpfnSHBrowseForFolder AS TSHBrowseForFolder PTR
GLOBAL gpfnShell_NotifyIcon AS TShell_NotifyIcon PTR

//GLOBAL gpfnSHGetMalloc AS TSHGetMalloc PTR
GLOBAL gpfnSHGetPathFromIDList AS TSHGetPathFromIDList PTR
	// tray icon
FUNCTION __LoadShellDll()
	LOCAL hDll AS PTR
	LOCAL rsFormat AS ResourceString

	IF glShellDllLoaded
		RETURN TRUE
	ENDIF

	hDll := LoadLibrary(String2Psz( "SHELL32.DLL"))
	IF (hDll == NULL_PTR)
		rsFormat := ResourceString{__WCSLoadLibraryError}
		WCError{#LoadShellDLL, #DragDropClient, VO_Sprintf(rsFormat:value, "SHELL32.DLL"),,,FALSE}:@@Throw()
		RETURN FALSE
	ENDIF
	
	gpfnDragFinish 			:= GetProcAddress(hDll, String2Psz("DragFinish"))
	gpfnDragAcceptFiles 	:= GetProcAddress(hDll, String2Psz("DragAcceptFiles"))
	gpfnDragQueryFile 		:= GetProcAddress(hDll, String2Psz("DragQueryFileA"))
	//gpfnDragQueryPoint 		:= GetProcAddress(hDll, String2Psz("DragQueryPoint"))
	//gpfnSHGetMalloc 			:= GetProcAddress(hDll, PSZ(_CAST, "SHGetMalloc"))
	gpfnSHBrowseForFolder 	:= GetProcAddress(hDll, String2Psz("SHBrowseForFolderA"))
	gpfnSHGetPathFromIDList := GetProcAddress(hDll, String2Psz("SHGetPathFromIDListA"))
	gpfnShell_NotifyIcon 	:= GetProcAddress(hDll, String2Psz("Shell_NotifyIcon"))

	RETURN (glShellDllLoaded := TRUE)

FUNCTION TDragAcceptFiles(hWnd AS PTR, fAccept AS LOGIC) AS VOID STRICT
	RETURN

FUNCTION TDragFinish(hDrop AS PTR) AS VOID STRICT
	RETURN
	
FUNCTION TDragQueryFile(hDrop AS PTR, iFile AS DWORD, lpszFile AS PSZ, cch AS DWORD) AS DWORD STRICT
	//SYSTEM
	RETURN 0

//STATIC FUNCTION TDragQueryPoint(hDrOP AS PTR, lppt AS _winPOINT) AS LOGIC STRICT
	////SYSTEM
	//RETURN FALSE
//
FUNCTION TSHBrowseForFolder(bi AS PTR) AS PTR STRICT
	//SYSTEM
	RETURN NULL_PTR

FUNCTION TShell_NotifyIcon(dwMessage AS DWORD, lpData AS _winNOTIFYICONDATA) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE


FUNCTION TSHGetPathFromIDList(pidl AS PTR, pszDisplayName AS PSZ) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE



#region defines
DEFINE WM_DRAGSELECT := 0x022E
DEFINE WM_QUERYDROPOBJECT := 0x022B
#endregion
