/// <include file="Gui.xml" path="doc/DragDropClient/*" />
CLASS DragDropClient INHERIT VObject
	PROTECT hOwner AS PTR
	PROTECT oParent AS Window
	//SE-060520
	//PROTECT ptrOldWinProc AS PTR
	//PROTECT __ptrOldSelf AS PTR //Riz compiler bug. protect creates and error when using @ (address of)


/// <include file="Gui.xml" path="doc/DragDropClient.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	
	


	IF (hOwner != NULL_PTR)
		DragAcceptFiles( hOwner,FALSE)
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


/// <include file="Gui.xml" path="doc/DragDropClient.Dispatch/*" />
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
		DragFinish( PTR(_CAST, oEvt:wParam))
	CASE WM_QUERYDROPOBJECT
		oParent:EventReturnvalue := IIF(SELF:DragOver(DragEvent{oEvt}), 0L, 1L)
	END SWITCH


	RETURN NIL


/// <include file="Gui.xml" path="doc/DragDropClient.DragLeave/*" />
METHOD DragLeave(oEvent) 
	
	


	IF IsMethod(oParent, #DragLeave)
		Send(oParent, #DragLeave, oEvent)
	ENDIF


	RETURN NIL


/// <include file="Gui.xml" path="doc/DragDropClient.DragOver/*" />
METHOD DragOver(oDragEvent) 
	
	


	IF (IsMethod(oParent, #DragOver))
		RETURN Send(oParent, #DragOver, oDragEvent)
	ENDIF


	RETURN TRUE


/// <include file="Gui.xml" path="doc/DragDropClient.Drop/*" />
METHOD Drop(oDragEvent) 
	
	


	IF IsMethod(oParent, #Drop)
		Send(oParent, #Drop, oDragEvent)
	ENDIF


	RETURN NIL


/// <include file="Gui.xml" path="doc/DragDropClient.ctor/*" />
CONSTRUCTOR(oOwner) 
	
	


	IF !IsInstanceOfUsual(oOwner,#Window)
		WCError{#Init,#DragDropClient,__WCSTypeError,oOwner,1}:Throw()
	ENDIF


//	IF !glShellDllLoaded
//		__LoadShellDll()
//	ENDIF


	
	
	oParent := oOwner
	hOwner := oParent:Handle()  
	//SE-070501 for better	Datawindow support
	IF oParent IS __FormFrame  VAR oFF
		oParent := oFF:DataWindow
    ELSEIF oParent IS __FormDialogWindow 
        VAR oFFr := (__FormFrame) oParent:Owner
		oParent := oFFr:DataWindow 
	ENDIF
	IF oParent == NULL_OBJECT
		oParent := oOwner
	ENDIF    


	//Put the address of our proc in the window class and save the old one
	//	ptrOldWinProc := SetWindowLong(hOwner, GWL_WNDPROC, long(_cast, @__WCDDClientProc()))
	//Put the ptr to this object in the window class and save the old object ptr
	//	RegisterKid(@__ptrOldSelf,1,false)
	//	__ptrOldSelf := SetWindowLong(hOwner, DWL_User, long(_cast,self))
	DragAcceptFiles( hOwner, TRUE)


	RETURN 


/// <include file="Gui.xml" path="doc/DragDropClient.Owner/*" />
ACCESS Owner 
	
	


	RETURN oParent
END CLASS




 /// <exclude />
FUNCTION __LoadShellDll()
    RETURN TRUE


/// <exclude/>
_DLL FUNCTION DragAcceptFiles(hWnd AS PTR, fAccept AS LOGIC) AS VOID PASCAL:Shell32.DragAcceptFiles
/// <exclude/>
_DLL FUNCTION DragFinish(hDrop AS PTR) AS VOID PASCAL:Shell32.DragFinish
/// <exclude/>
_DLL FUNCTION DragQueryFile(hDrop AS PTR, iFile AS DWORD, lpszFile AS PSZ, cch AS DWORD) AS DWORD PASCAL:Shell32.DragQueryFileA 
/// <exclude/>
_DLL FUNCTION SHBrowseForFolder(bi AS PTR) AS PTR PASCAL:Shell32.SHBrowseForFolderA
/// <exclude/>
_DLL FUNCTION Shell_NotifyIcon(dwMessage AS DWORD, lpData AS _winNOTIFYICONDATA) AS LOGIC PASCAL:Shell32.Shell_NotifyIcon
/// <exclude/>
_DLL FUNCTION SHGetPathFromIDList(pidl AS PTR, pszDisplayName AS PSZ) AS LOGIC PASCAL:Shell32.SHGetPathFromIDListA


#region defines
DEFINE WM_DRAGSELECT := 0x022E
DEFINE WM_QUERYDROPOBJECT := 0x022B
#endregion
