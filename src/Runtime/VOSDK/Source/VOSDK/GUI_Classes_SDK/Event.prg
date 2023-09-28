STATIC DEFINE SYSTEM_KEYCODE := 0x20000000


/// <include file="Gui.xml" path="doc/AppCommandEvent/*" />
CLASS AppCommandEvent INHERIT @@Event
	//PP-030904
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/AppCommandEvent.Command/*" />
	ACCESS Command AS DWORD STRICT
	RETURN Get_AppCommand_lParam(DWORD(_CAST,SELF:lParam))


/// <include file="Gui.xml" path="doc/AppCommandEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return


/// <include file="Gui.xml" path="doc/AppCommandEvent.IsControl/*" />
ACCESS IsControl AS LOGIC STRICT
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_CONTROL) > 0


/// <include file="Gui.xml" path="doc/AppCommandEvent.IsDeviceKey/*" />
ACCESS IsDeviceKey AS LOGIC STRICT
	RETURN Get_Device_lParam(DWORD(_CAST,SELF:lParam)) == FAPPCOMMAND_KEY


/// <include file="Gui.xml" path="doc/AppCommandEvent.IsDeviceMouse/*" />
ACCESS IsDeviceMouse AS LOGIC STRICT
	RETURN Get_Device_lParam(DWORD(_CAST,SELF:lParam)) == FAPPCOMMAND_MOUSE


/// <include file="Gui.xml" path="doc/AppCommandEvent.IsDeviceOEM/*" />
ACCESS IsDeviceOEM AS LOGIC STRICT
	RETURN Get_Device_lParam(DWORD(_CAST,SELF:lParam)) == FAPPCOMMAND_OEM


/// <include file="Gui.xml" path="doc/AppCommandEvent.IsLeftButton/*" />
ACCESS IsLeftButton AS LOGIC STRICT
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_LBUTTON) > 0


/// <include file="Gui.xml" path="doc/AppCommandEvent.IsMiddleButton/*" />
ACCESS IsMiddleButton AS LOGIC STRICT
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_MBUTTON) > 0


/// <include file="Gui.xml" path="doc/AppCommandEvent.IsRightButton/*" />
ACCESS IsRightButton AS LOGIC STRICT
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_RBUTTON) > 0


/// <include file="Gui.xml" path="doc/AppCommandEvent.IsShift/*" />
ACCESS IsShift AS LOGIC STRICT
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_SHIFT) > 0


/// <include file="Gui.xml" path="doc/AppCommandEvent.IsXButton1/*" />
ACCESS IsXButton1 AS LOGIC STRICT
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_XBUTTON1) > 0


/// <include file="Gui.xml" path="doc/AppCommandEvent.IsXButton2/*" />
ACCESS IsXButton2 AS LOGIC STRICT
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_XBUTTON2) > 0


/// <include file="Gui.xml" path="doc/AppCommandEvent.oTarget/*" />
ACCESS oTarget AS OBJECT STRICT
	RETURN __WCGetObjectByHandle(PTR(_CAST,SELF:wParam))




END CLASS


/// <include file="Gui.xml" path="doc/ComboBoxExEndEditEvent/*" />
CLASS ComboBoxExEndEditEvent INHERIT ControlNotifyEvent
	//SE-060519
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/ComboBoxExEndEditEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/ComboBoxExEndEditEvent.IsChanged/*" />
ACCESS IsChanged AS LOGIC STRICT
	//SE-060519
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT






	sNMCBEENDEDIT := PTR(_CAST, SELF:lParam)
	RETURN sNMCBEENDEDIT:fChanged


/// <include file="Gui.xml" path="doc/ComboBoxExEndEditEvent.NewSelection/*" />
ACCESS NewSelection AS LONGINT STRICT
	//SE-060519
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT






	sNMCBEENDEDIT := PTR(_CAST, SELF:lParam)
	RETURN sNMCBEENDEDIT:iNewSelection + 1l


/// <include file="Gui.xml" path="doc/ComboBoxExEndEditEvent.TextValue/*" />
ACCESS TextValue AS STRING STRICT
	//SE-060519
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT






	sNMCBEENDEDIT := PTR(_CAST, SELF:lParam)
	IF sNMCBEENDEDIT:szText[1] = 0
		RETURN NULL_STRING
	ENDIF
   RETURN Psz2String(PSZ(_CAST, @sNMCBEENDEDIT:szText[1]))


/// <include file="Gui.xml" path="doc/ComboBoxExEndEditEvent.Why/*" />
ACCESS Why AS LONGINT STRICT
	//SE-060519
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT






	sNMCBEENDEDIT := PTR(_CAST, SELF:lParam)
	RETURN sNMCBEENDEDIT:iWhy


/* possible values:
   CBENF_DROPDOWN   The user activated the drop-down list.
   CBENF_ESCAPE     The user pressed ESC.
   CBENF_KILLFOCUS  The edit box lost the keyboard focus.
   CBENF_RETURN     The user completed the edit operation by pressing ENTER.
*/


END CLASS


/// <include file="Gui.xml" path="doc/ControlEvent/*" />
CLASS ControlEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance


/// <include file="Gui.xml" path="doc/ControlEvent.Control/*" />
ACCESS Control AS Control STRICT
	RETURN __WCGetControlByHandle(lParam)


/// <include file="Gui.xml" path="doc/ControlEvent.ControlID/*" />
ACCESS ControlID AS LONGINT STRICT
	RETURN LoWord(wParam)


/// <include file="Gui.xml" path="doc/ControlEvent.Description/*" />
ACCESS Description AS STRING STRICT
	LOCAL oHL AS HyperLabel




	oHL:=SELF:HyperLabel
	IF oHL!=NULL_OBJECT
		RETURN oHL:Description
	ENDIF
	RETURN NULL_STRING


/// <include file="Gui.xml" path="doc/ControlEvent.HelpContext/*" />
ACCESS HelpContext AS STRING STRICT
	LOCAL oHL AS HyperLabel




	oHL:=SELF:HyperLabel


	IF oHL!=NULL_OBJECT
		RETURN oHL:HelpContext
	ENDIF


	RETURN NULL_STRING


/// <include file="Gui.xml" path="doc/ControlEvent.HyperLabel/*" />
ACCESS HyperLabel AS HyperLabel STRICT
	LOCAL oControl AS Control






	oControl:=SELF:Control
	IF oControl!=NULL_OBJECT
		RETURN oControl:HyperLabel
	ENDIF


	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/ControlEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/ControlEvent.Name/*" />
ACCESS Name AS STRING STRICT
	LOCAL oHL AS HyperLabel






	oHL:=SELF:HyperLabel
	IF oHL!=NULL_OBJECT
		RETURN oHL:Name
	ENDIF


	RETURN NULL_STRING


/// <include file="Gui.xml" path="doc/ControlEvent.NameSym/*" />
ACCESS NameSym AS SYMBOL STRICT
	LOCAL oHL AS HyperLabel






	oHL:=SELF:HyperLabel
	IF oHL!=NULL_OBJECT
		RETURN oHL:NameSym
	ENDIF


	RETURN NULL_SYMBOL


END CLASS


/// <include file="Gui.xml" path="doc/ControlFocusChangeEvent/*" />
CLASS ControlFocusChangeEvent INHERIT FocusChangeEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/ControlFocusChangeEvent.Control/*" />
	ACCESS Control AS CONTROL STRICT
  RETURN SELF:Window


/// <include file="Gui.xml" path="doc/ControlFocusChangeEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return

end class


/// <include file="Gui.xml" path="doc/ControlNotifyEvent/*" />
CLASS ControlNotifyEvent INHERIT ControlEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/ControlNotifyEvent.Control/*" />
ACCESS Control AS Control STRICT
	LOCAL strucNotify AS _winNMHDR
	strucNotify := PTR(_CAST, SELF:LParam)
	RETURN __WCGetControlByHandle(strucNotify:hwndFrom)


/// <include file="Gui.xml" path="doc/ControlNotifyEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/ControlNotifyEvent.NotifyCode/*" />
ACCESS NotifyCode AS DWORD STRICT
	LOCAL strucNotify AS _winNMHDR






	strucNotify := PTR(_CAST, SELF:lParam)


	RETURN  strucNotify:_code


END CLASS


/// <include file="Gui.xml" path="doc/DateTimeSelectionEvent/*" />
CLASS DateTimeSelectionEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/DateTimeSelectionEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/DateTimeSelectionEvent.SelectedDate/*" />
ACCESS SelectedDate AS DATE STRICT
	LOCAL sc AS _winNMDATETIMECHANGE


	sc := PTR(_CAST, lParam)
	RETURN ConDate(sc:st:wYear, sc:st:wMonth, sc:st:wDay)


/// <include file="Gui.xml" path="doc/DateTimeSelectionEvent.SelectedTime/*" />
ACCESS SelectedTime AS STRING STRICT
	LOCAL sc AS _winNMDATETIMECHANGE
	LOCAL sReturn AS STRING


	sc := PTR(_CAST, lParam)
	sReturn := ConTime(sc:st:wHour, sc:st:wMinute, sc:st:wSecond)
	//#ifdef __VULCAN__   // buffer() isn't supported
	   //sReturn := String.Format( "{0,d2}:{1,d2}:{2,d2}", sc:st:wHour, sc:st:wMinute, sc:st:wSecond )
	//#else
	//sReturn := Buffer(8)
	//wsprintf(String2Psz(sReturn), String2Psz("%02d:%02d:%02d"), sc:st:wHour, sc:st:wMinute, sc:st:wSecond)
	//#endif
//
	RETURN sReturn


END CLASS


/// <include file="Gui.xml" path="doc/DragEvent/*" />
CLASS DragEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	PROTECT oControl AS Control
/// <include file="Gui.xml" path="doc/DragEvent.Control/*" />
	ACCESS Control AS OBJECT STRICT




	RETURN SELF:oControl


/// <include file="Gui.xml" path="doc/DragEvent.FileCount/*" />
ACCESS FileCount
	//local strucDragInfo as __WCDragInfo




	//if uMsg==WM_QueryDropObject //Riz This is undocumented windows stuff
	//Riz This is undocumented windows stuff
	//strucDragInfo := ptr(_cast,lParam)
	// Get source app (File Manager, usually) DS
	//wSourceDS := _Or( _And( GetWindowWord(StrucDragInfo.hWndSource,GWW_HINSTANCE), 0xFFFC), 1)
	//pszP := psz(_cast,(dword(wSourceDS)<<16) + StrucDragInfo.pszList)
	//return Occurs(Psz2String(pszP)," ")
	//elseif uMsg==WM_DropFiles


	IF (uMsg == WM_DROPFILES) .AND. __LoadShellDll()
		RETURN DragQueryFile( wParam, 0Xffffffff, NULL_PSZ, 0)
	ENDIF


	RETURN 0


/// <include file="Gui.xml" path="doc/DragEvent.FileName/*" />
METHOD FileName(nfile)
	LOCAL dwSize AS DWORD
	LOCAL pszBuf AS PSZ
	LOCAL cBuf AS STRING






	IF !IsLong(nFile)
		WCError{#FileName,#DragEvent,__WCSTypeError,nfile,1}:Throw()
	ENDIF


	nfile--


	IF ((uMsg == WM_DROPFILES) .OR. ((uMsg == WM_QUERYDROPOBJECT) .AND. (wParam != 0))) .AND. __LoadShellDll()
		dwSize := DragQueryFile( PTR(_CAST, wParam), nfile, NULL_PSZ, 0) + 1
		IF (dwSize > 0)
			pszBuf := MemAlloc(dwSize)
			DragQueryFile( PTR(_CAST, wParam), nfile, pszBuf, dwSize)
			cBuf := Psz2String(pszBuf)
			MemFree(pszBuf)
		ENDIF
	ENDIF


	RETURN cBuf


/// <include file="Gui.xml" path="doc/DragEvent.ctor/*" />
CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow)
	super(_hWnd, _uMsg, _wParam, _lParam, _oWindow)
	if !IsPtr(_hWnd)
		if _uMsg is Control var oC
			self:oControl := oC
		ENDIF
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/DragEvent.Origin/*" />
ACCESS Origin
	LOCAL strucPoint IS _WinPoint
	LOCAL strucRect IS _WinRect


	//if uMsg==WM_QueryDropObject //Riz This is undocumented windows stuff
	//Riz Gets the current cursor position, not the position when the
	//message was created. To get the position of the cursor when the message
	//was created we would have to use undocumented windows stuff.
	GetCursorPos(@strucPoint) //current cursor position
	GetWindowRect(hWnd,@strucRect) //client window rectangle
	RETURN Point{strucPoint:X-strucRect:Left,strucRect:Bottom-strucPoint:Y}
	//elseif uMsg==WM_DropFiles
   //RvdH 070116 Unreachable code.
	// 	IF (uMsg == WM_DropFiles) .and. __LoadShellDll()
	// 		PCALL(gpfnDragQueryPoint, wParam, @strucPoint)
	// 		RETURN Point{strucPoint.X,strucPoint.Y}
	// 	ENDIF


	// 	RETURN Point{0,0}


END CLASS


/// <include file="Gui.xml" path="doc/EditFocusChangeEvent/*" />
CLASS EditFocusChangeEvent INHERIT ControlEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/EditFocusChangeEvent.GotFocus/*" />
	ACCESS GotFocus AS LOGIC STRICT
	LOCAL dwHiWord AS DWORD


	dwHiWord:=HiWord(wParam)


	RETURN (dwHiWord==EN_SetFocus) .OR. (dwHiWord==CBN_SetFocus)


/// <include file="Gui.xml" path="doc/EditFocusChangeEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return

end class


/// <include file="Gui.xml" path="doc/Event/*" />
CLASS @@Event //inherit object
	//RvdH 061218 Declared properties for performance
	EXPORT hWnd 	AS PTR
	EXPORT uMsg 	AS DWORD
	EXPORT wParam 	AS DWORD
	EXPORT lParam 	AS LONGINT
	EXPORT oWindow AS OBJECT
/// <include file="Gui.xml" path="doc/Event.Handle/*" />
	ACCESS Handle AS PTR STRICT




	RETURN hWnd


/// <include file="Gui.xml" path="doc/Event.ctor/*" />
CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow)

	//super:Init()
	IF IsPtr(_hWnd)
		hWnd := _hWnd
		uMsg := _uMsg
		wParam := _wParam
		lParam := _lParam
		oWindow := _oWindow
	elseif _hWnd is @@Event var oEvent
		hWnd := oEvent:hWnd
		uMsg := oEvent:uMsg
		wParam := oEvent:wParam
		lParam := oEvent:lParam
		oWindow := oEvent:oWindow
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/Event.Message/*" />
ACCESS Message AS DWORD STRICT


	RETURN uMsg


/// <include file="Gui.xml" path="doc/Event.Window/*" />
ACCESS Window AS OBJECT STRICT




	IF (oWindow == NULL_OBJECT)
		RETURN __WCGetObjectByHandle(hWnd)
	ENDIF
	RETURN oWindow




END CLASS


/// <include file="Gui.xml" path="doc/ExposeEvent/*" />
CLASS ExposeEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/ExposeEvent.ExposedArea/*" />
	ACCESS ExposedArea AS BoundingBox STRICT
	LOCAL hwndWindow AS PTR
	LOCAL strucExpRect IS _WinRect
	LOCAL strucCtlRect IS _WinRect
	LOCAL siY AS LONGINT






	hwndWindow := SELF:oWindow:Handle()
	GetClientRect(hwndWindow, @strucCtlRect)
	IF IsMethod(SELF:oWindow, #__GetPaintRect) .AND. !IsRectEmpty(SELF:oWindow:__GetPaintRect())
		CopyRect(@strucExpRect, SELF:oWindow:__GetPaintRect())
	ELSE
		GetUpdateRect(hwndWindow, @strucExpRect, FALSE)
	ENDIF


	IF WCGetCoordinateSystem()==WCCartesianCoordinates
		siY := strucCtlRect:bottom - strucExpRect:bottom
	ELSE
		siY := strucExpRect:top
	ENDIF


	RETURN BoundingBox{Point{strucExpRect:left, siY},;
		Dimension{strucExpRect:right - strucExpRect:left, strucExpRect:bottom - strucExpRect:top}}


/// <include file="Gui.xml" path="doc/ExposeEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return

end class


/// <include file="Gui.xml" path="doc/FocusChangeEvent/*" />
CLASS FocusChangeEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/FocusChangeEvent.GotFocus/*" />
	ACCESS GotFocus AS LOGIC STRICT


	RETURN uMsg == WM_SETFOCUS


/// <include file="Gui.xml" path="doc/FocusChangeEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return

end class


/// <include file="Gui.xml" path="doc/HelpRequestEvent/*" />
CLASS HelpRequestEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/HelpRequestEvent.HelpContext/*" />
	ACCESS HelpContext AS STRING STRICT
	LOCAL oHL AS HyperLabel






	oHL:=SELF:HyperLabel


	IF oHL!=NULL_OBJECT
		RETURN oHL:HelpContext
	ENDIF


	RETURN NULL_STRING


/// <include file="Gui.xml" path="doc/HelpRequestEvent.HelpInfo/*" />
ACCESS HelpInfo AS PTR STRICT
	//SE-060522




	IF uMsg = WM_HELP
		RETURN PTR(_CAST, lParam)
   ENDIF


	RETURN NULL_PTR


/// <include file="Gui.xml" path="doc/HelpRequestEvent.HelpType/*" />
ACCESS HelpType AS DWORD STRICT




	IF uMsg = WM_HELP
		RETURN HELPINFO
   ENDIF


	RETURN wParam


/// <include file="Gui.xml" path="doc/HelpRequestEvent.HyperLabel/*" />
ACCESS HyperLabel AS HyperLabel STRICT
	 LOCAL oObject AS OBJECT
	 LOCAL oMenu AS Menu
	 LOCAL oHL AS HyperLabel
    LOCAL sHelpInfo AS _winHelpInfo
    LOCAL hTemp AS PTR






	 IF uMsg = WM_HELP
       sHelpInfo := PTR(_CAST, lParam)


       hTemp := sHelpInfo:hItemHandle


       IF (oObject :=__WCGetObjectByHandle(hTemp)) == NULL_OBJECT
          hTemp   := PTR(_CAST, GetWindowLong(hTemp, GWL_HWNDPARENT))
          oObject :=__WCGetObjectByHandle(hTemp)
       ENDIF


       IF oObject != NULL_OBJECT
          IF sHelpInfo:iContextType == HELPINFO_WINDOW
   			 if oObject is Control var oC
   				 oHL := oC:Hyperlabel
   				 IF oHL != NULL_OBJECT .AND. oHL:HelpContext == NULL_STRING
   				    IF IsInstanceOf(oObject, #TabControl)
   				       oObject := oObject:CurrentPage
   				    ELSE
                      oObject := oObject:Owner
   				    ENDIF
   				 ENDIF
   	       ENDIF
   			 if oObject is Window
   				 IF IsInstanceOf(oObject,#__DocApp) .OR.;
   					 IsInstanceOf(oObject,#__WindApp) .OR.;
   		 	       IsInstanceOf(oObject,#__FormDialogWindow)
   		 	   	 oObject := oObject:Owner
   		 	    ENDIF
   		 	    IF IsInstanceOf(oObject,#__FormFrame)
   		 	   	 oObject := oObject:DataWindow
   		 	    ENDIF
   		 	    oHL := oObject:Hyperlabel
   			 ENDIF
   		 ELSE
   		    IF IsInstanceOf(oObject, #Menu)
   		       oHL := oObject:HyperLabel(sHelpInfo:iCtrlId)
   		    ENDIF
   		 ENDIF
       ENDIF


	ELSEIF oWindow!=NULL_OBJECT
		IF wParam==HELPCONTROL
			//Riz Old code --> If this is a datawindow use formwindow as oObject
			oObject :=__WCGetObjectByHandle(PTR(_CAST,lParam))
			IF oObject!=NULL_OBJECT
				oHL:=oObject:HyperLabel
			ENDIF
		ELSEIF wParam==HELPMENU
			oMenu:=oWindow:Menu
			IF oMenu!=NULL_OBJECT
				oHL:=oMenu:HyperLabel(SELF:ItemID)
			ENDIF
		ELSE
			oHL:=oWindow:HyperLabel
		ENDIF
	ENDIF


	RETURN oHL


/// <include file="Gui.xml" path="doc/HelpRequestEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/HelpRequestEvent.ItemID/*" />
ACCESS ItemID AS DWORD STRICT
	LOCAL dwID AS DWORD


	IF wParam == HELPMENU
		RETURN LoWord(DWORD(_CAST,lParam))
	ELSEIF wParam == HelpControl
		IF (dwID := DWORD(GetWindowLong(INT(LoWord(DWORD(lParam))), GWL_ID))) == 0xFFFF
			RETURN dwID
		ELSE
			RETURN _AND(dwID,_NOT(WC_CTRL_Mask))
		ENDIF
	ENDIF
	RETURN 0


/// <include file="Gui.xml" path="doc/HelpRequestEvent.Position/*" />
ACCESS Position AS POINT STRICT
	LOCAL strucPoint IS _WinPoint






	GetCursorPos(@strucPoint)
	ScreenToClient(oWindow:Handle(), @strucPoint)


	RETURN __WCConvertPoint(oWindow,Point{strucPoint:X,strucPoint:Y})


/// <include file="Gui.xml" path="doc/HelpRequestEvent.WindowRegion/*" />
ACCESS WindowRegion() AS LONGINT STRICT




	IF wParam == HelpWindow
		RETURN lParam
	ENDIF


	RETURN RegionUnknown


END CLASS


/// <include file="Gui.xml" path="doc/KeyEvent/*" />
CLASS KeyEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/KeyEvent.ASCIIChar/*" />
	ACCESS ASCIIChar AS DWORD STRICT
	LOCAL retVal AS DWORD
	LOCAL DIM keyState[256] AS BYTE
	LOCAL DIM result[5] AS WORD   // dcaton 070328 was BYTE, wrong type for ToAscii()






	IF (uMsg == WM_CHAR)
		retVal := wParam
	ELSE
		GetKeyboardState(@keyState)
		IF ToAscii(wParam, HiWord(_AND(DWORD(_CAST, lParam), 0x00FF0000U)), @keyState, @result, 1) == 1
			retVal := result[1]
		ELSE
			retVal := 0
		ENDIF
	ENDIF


	RETURN retVal


/// <include file="Gui.xml" path="doc/KeyEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/KeyEvent.KeyCode/*" />
ACCESS KeyCode AS DWORD STRICT




	IF (uMsg != WM_CHAR)
		RETURN wParam
	ENDIF
	RETURN MapVirtualKey(HiWord(_AND(DWORD(_CAST, lParam), 0x00FF0000U)), 1)


/// <include file="Gui.xml" path="doc/KeyEvent.RepeatCount/*" />
ACCESS RepeatCount AS LONGINT STRICT
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD
	RETURN LoWord(dw)


/// <include file="Gui.xml" path="doc/KeyEvent.System/*" />
ACCESS System AS LOGIC STRICT


	RETURN _AND(lParam, SYSTEM_KEYCODE) != 0


END CLASS


/// <include file="Gui.xml" path="doc/ListViewColumnClickEvent/*" />
CLASS ListViewColumnClickEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/ListViewColumnClickEvent.ctor/*" />
	constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/ListViewColumnClickEvent.ListViewColumn/*" />
ACCESS ListViewColumn AS ListViewCOlumn STRICT
	LOCAL strucListView AS _winNM_ListView
	LOCAL oListView		AS ListView
	oListView := OBJECT(SELF:Control)




	strucListView := PTR(_CAST, SELF:lParam)
	RETURN oListView:__GetColumnFromIndex(DWORD(strucListView:iSubItem + 1))


END CLASS


/// <include file="Gui.xml" path="doc/ListViewDeleteEvent/*" />
CLASS ListViewDeleteEvent INHERIT ListViewItemEvent


/// <include file="Gui.xml" path="doc/ListViewDeleteEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return

end class


/// <include file="Gui.xml" path="doc/ListViewDragEvent/*" />
CLASS ListViewDragEvent INHERIT ListViewItemEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/ListViewDragEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/ListViewDragEvent.IsLeftButton/*" />
ACCESS IsLeftButton AS LOGIC STRICT




	IF SELF:NotifyCode == LVN_BEGINRDRAG
		RETURN FALSE
	ENDIF


	RETURN TRUE


/// <include file="Gui.xml" path="doc/ListViewDragEvent.IsRightButton/*" />
ACCESS IsRightButton AS LOGIC STRICT




	IF SELF:NotifyCode == LVN_BEGINRDRAG
		RETURN TRUE
	ENDIF


	RETURN FALSE


/// <include file="Gui.xml" path="doc/ListViewDragEvent.Position/*" />
ACCESS Position AS POINT STRICT
	LOCAL strucListView AS _winNM_ListView
	LOCAL oPoint AS Point






	strucListView := PTR(_CAST, SELF:lParam)
	oPoint := Point{strucListView:ptAction:x, strucListView:ptAction:y}


	RETURN __WCConvertPoint(SELF:Control, oPoint)


END CLASS


/// <include file="Gui.xml" path="doc/ListViewEditEvent/*" />
CLASS ListViewEditEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/ListViewEditEvent.EditBeginning/*" />
	ACCESS EditBeginning AS LOGIC STRICT




	RETURN (SELF:NotifyCode == LVN_BEGINLABELEDIT)


/// <include file="Gui.xml" path="doc/ListViewEditEvent.EditEnding/*" />
ACCESS EditEnding AS LOGIC STRICT




	RETURN (SELF:NotifyCode == LVN_ENDLABELEDIT)


/// <include file="Gui.xml" path="doc/ListViewEditEvent.EditText/*" />
ACCESS EditText AS STRING STRICT
	LOCAL strucDispInfo AS _winLV_DispInfo






	strucDispInfo := PTR(_CAST, SELF:lParam)


	IF SELF:EditEnding
		RETURN Psz2String(strucDispInfo:item:pszText)
	ENDIF
	RETURN NULL_STRING


/// <include file="Gui.xml" path="doc/ListViewEditEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/ListViewEditEvent.ListViewItem/*" />
ACCESS ListViewItem
	LOCAL strucDispInfo AS _winLV_DispInfo
	LOCAL oListView		AS ListView
	oListView := OBJECT(SELF:Control)






	strucDispInfo := PTR(_CAST, SELF:lParam)
	RETURN oListView:GetItemAttributes(strucDispInfo:item:iItem + 1)


END CLASS


/// <include file="Gui.xml" path="doc/ListViewItemEvent/*" />
CLASS ListViewItemEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/ListViewItemEvent.ctor/*" />
	constructor(args params usual[])
    super(args)
return


/// <include file="Gui.xml" path="doc/ListViewItemEvent.ListViewItem/*" />
ACCESS ListViewItem AS ListViewItem STRICT
	LOCAL oListView AS ListView
	LOCAL strucListView AS _winNM_ListView






	strucListView := PTR(_CAST, SELF:lParam)
	oListView := OBJECT(SELF:Control)


	RETURN oListView:GetItemAttributes(strucListView:iItem + 1)




END CLASS


/// <include file="Gui.xml" path="doc/ListViewKeyEvent/*" />
CLASS ListViewKeyEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/ListViewKeyEvent.ctor/*" />
	constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/ListViewKeyEvent.KeyCode/*" />
ACCESS KeyCode AS LONGINT STRICT
	LOCAL strucKeyDown AS _winLV_KeyDown






	strucKeyDown := PTR(_CAST, SELF:LParam)
	RETURN strucKeyDown:wVKey


END CLASS


/// <include file="Gui.xml" path="doc/ListViewMouseEvent/*" />
CLASS ListViewMouseEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	PROTECT nX AS INT
	PROTECT nY AS INT
/// <include file="Gui.xml" path="doc/ListViewMouseEvent.ButtonID/*" />
	ACCESS ButtonID AS LONGINT STRICT
	LOCAL dwNotifyCode AS DWORD






	dwNotifyCode := SELF:NotifyCode
	IF dwNotifyCode == NM_CLICK .OR. dwNotifyCode == NM_DBLCLK
		RETURN BUTTONLEFT
	ENDIF
	IF dwNotifyCode == NM_RCLICK .OR. dwNotifyCode == NM_RDBLCLK
		RETURN BUTTONRIGHT
	ENDIF


	RETURN 0


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.ctor/*" />
CONSTRUCTOR(oControlNotifyEvent)
	LOCAL strucPoint IS _winPoint






	SUPER(oControlNotifyEvent)


	GetCursorPos(@strucPoint)
	ScreenToClient(SELF:Control:Handle(), @strucPoint)
	nX := strucPoint:x
	nY := strucPoint:y


	RETURN


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.IsLeftButton/*" />
ACCESS IsLeftButton AS LOGIC STRICT
	//RvdH 061214 Added type (was untyped)
	LOCAL dwNotifyCode AS DWORD






	dwNotifyCode := SELF:NotifyCode
	IF dwNotifyCode == NM_CLICK .OR. dwNotifyCode == NM_DBLCLK
		RETURN TRUE
	ENDIF


	RETURN FALSE


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.IsRightButton/*" />
ACCESS IsRightButton AS LOGIC STRICT
	//RvdH 061214 Added type (was untyped)
	LOCAL dwNotifyCode AS DWORD






	dwNotifyCode := SELF:NotifyCode
	IF dwNotifyCode == NM_RCLICK .OR. dwNotifyCode == NM_RDBLCLK
		RETURN TRUE
	ENDIF


	RETURN FALSE


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.ListViewItem/*" />
ACCESS ListViewItem AS ListViewItem STRICT
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo
	LOCAL oListView AS ListView






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)
	oListView := OBJECT(SELF:Control)


	IF ( _AND(strucHitTestInfo:flags, LVHT_ONITEM) != 0)
		RETURN oListView:GetItemAttributes(strucHitTestInfo:iItem + 1)
	ENDIF
	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.PointAboveClientArea/*" />
ACCESS PointAboveClientArea AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, LVHT_ABOVE) != 0


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.PointBelowClientArea/*" />
ACCESS PointBelowClientArea AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, LVHT_BELOW) != 0


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.PointLeftOfClientArea/*" />
ACCESS PointLeftOfClientArea AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, LVHT_TOLEFT) != 0


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.PointNowhere/*" />
ACCESS PointNowhere AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, LVHT_NOWHERE) != 0


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.PointOnItem/*" />
ACCESS PointOnItem AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, LVHT_ONITEM) != 0


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.PointOnItemImage/*" />
ACCESS PointOnItemImage AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, LVHT_ONITEMICON) != 0


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.PointOnItemLabel/*" />
ACCESS PointOnItemLabel AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, LVHT_ONITEMLABEL) != 0


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.PointOnItemStateImage/*" />
ACCESS PointOnItemStateImage AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, LVHT_ONITEMSTATEICON) != 0


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.PointRightOfClientArea/*" />
ACCESS PointRightOfClientArea AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, LVHT_TORIGHT) != 0


/// <include file="Gui.xml" path="doc/ListViewMouseEvent.Position/*" />
ACCESS Position AS POINT STRICT






	RETURN __WCConvertPoint(SELF:Control, Point{nX, nY})


END CLASS


/// <include file="Gui.xml" path="doc/MenuCommandEvent/*" />
CLASS MenuCommandEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	PROTECT oMenu AS Menu

 /// <exclude />
	METHOD __SetMenu(oParam AS OBJECT) AS MenuCommandEvent STRICT

	if uMsg == WM_SYSCOMMAND .and. oWindow is AppWindow var appWin
		omenu := appWin:__SysMenu
	ELSE
		// Was Menu Passed?
		if oParam is Menu var menu
			oMenu := menu
			// Window was passed
        elseif oParam is Window var oWin .and. oWin:ContextMenu!= null_object .and. oWin:ContextMenu:HyperLabel(self:ItemID) != null_object
			oMenu := oWin:ContextMenu
        elseif oParam is Control var oC .and. oC:ContextMenu!= null_object .and. oC:ContextMenu:HyperLabel(self:ItemID) != null_object
			oMenu := oC:ContextMenu
        elseif oParam is Window var oParent
			oMenu := oParent:Menu
			do while (oMenu == null_object) .and. oParent:Owner is Window var oWindow
				oParent := oWindow
				if (oParent:ContextMenu != null_object) .and. (oParent:ContextMenu:HyperLabel(self:ItemID) != null_object)
					oMenu := oParent:ContextMenu
				else
					oMenu := oParent:Menu
				endif
			enddo
		endif
	endif


	RETURN SELF


/// <include file="Gui.xml" path="doc/MenuCommandEvent.AsString/*" />
METHOD AsString() AS STRING STRICT
	LOCAL pszBuffer AS PSZ
	LOCAL cString AS STRING
	LOCAL liLength AS LONGINT

	pszBuffer := MemAlloc(256)
	liLength := GetMenuString(SELF:Menu:Handle(), wParam, pszBuffer, 256, MF_BYCOMMAND)

	if (liLength != 0)
		cString := Psz2String(pszBuffer)
	ENDIF

	MemFree(pszBuffer)

	return cString


/// <include file="Gui.xml" path="doc/MenuCommandEvent.HyperLabel/*" />
ACCESS HyperLabel AS HyperLabel STRICT
	LOCAL oHL AS HyperLabel
	if oMenu != null
		oHL := oMenu:HyperLabel(SELF:ItemID)
	ENDIF
	return oHL


/// <include file="Gui.xml" path="doc/MenuCommandEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return


/// <include file="Gui.xml" path="doc/MenuCommandEvent.ItemID/*" />
ACCESS ItemID AS LONGINT STRICT
	return LoWord(wParam)


/// <include file="Gui.xml" path="doc/MenuCommandEvent.Menu/*" />
ACCESS Menu AS MENU STRICT
	return oMenu


/// <include file="Gui.xml" path="doc/MenuCommandEvent.Name/*" />
ACCESS Name AS STRING STRICT
	LOCAL oHL AS HyperLabel
	oHL := self:HyperLabel
	if oHL != null_object
		RETURN oHL:Name
	ENDIF
	return null_string


/// <include file="Gui.xml" path="doc/MenuCommandEvent.NameSym/*" />
ACCESS NameSym AS SYMBOL STRICT
	LOCAL oHL AS HyperLabel

	oHL := self:HyperLabel
	if oHL != null_object
		RETURN oHL:NameSym
	ENDIF

	return null_symbol

END CLASS


/// <include file="Gui.xml" path="doc/MenuInitEvent/*" />
CLASS MenuInitEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/MenuInitEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return


/// <include file="Gui.xml" path="doc/MenuInitEvent.Menu/*" />
ACCESS Menu AS Menu STRICT




	RETURN __WCGetMenuByHandle(PTR(_CAST, wParam))


END CLASS


/// <include file="Gui.xml" path="doc/MenuSelectEvent/*" />
CLASS MenuSelectEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/MenuSelectEvent.AsString/*" />
	METHOD AsString() AS STRING STRICT
	LOCAL hMenu AS PTR
	LOCAL pszBuffer AS PSZ
	LOCAL cString AS STRING




	IF (_AND(HiWord(wParam), MF_SYSMENU) != 0)
		hMenu := GetSystemMenu(hWnd, FALSE)
	ELSE
		hMenu := SELF:Menu:Handle()
	ENDIF


	pszBuffer := MemAlloc(256)
	GetMenuString(hMenu, DWORD(SELF:ItemId), pszBuffer, 256, MF_BYCOMMAND)
	cString := Psz2String(pszBuffer)
	MemFree(pszBuffer)


	RETURN cString


/// <include file="Gui.xml" path="doc/MenuSelectEvent.HyperLabel/*" />
ACCESS HyperLabel AS HyperLabel STRICT
	LOCAL oHyperLabel AS HyperLabel






	IF (SELF:Menu != NULL_OBJECT)
		oHyperLabel := SELF:Menu:HyperLabel(SELF:ItemID)
	ENDIF


	RETURN oHyperLabel


/// <include file="Gui.xml" path="doc/MenuSelectEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/MenuSelectEvent.ItemID/*" />
ACCESS ItemID AS LONGINT STRICT


	RETURN LoWord(wParam)


/// <include file="Gui.xml" path="doc/MenuSelectEvent.Menu/*" />
ACCESS Menu AS Menu STRICT
	LOCAL oMenu AS Menu


	oMenu := __WCGetMenuByHandle(PTR(_CAST, lParam))


	IF (oMenu == NULL_OBJECT) .AND. (_AND(HiWord(wParam), MF_SYSMENU) == 0) .AND. IsInstanceOf(oWindow, #AppWindow)
		oMenu := oWindow:__SysMenu
	ENDIF


	RETURN oMenu


/// <include file="Gui.xml" path="doc/MenuSelectEvent.Name/*" />
ACCESS Name AS STRING STRICT
	LOCAL retVal AS STRING






	IF SELF:HyperLabel != NULL_OBJECT
		retVal := SELF:HyperLabel:Name
	ENDIF


	RETURN retVal


/// <include file="Gui.xml" path="doc/MenuSelectEvent.NameSym/*" />
ACCESS NameSym AS SYMBOL STRICT
	LOCAL retVal AS SYMBOL






	IF SELF:HyperLabel != NULL_OBJECT
		retVal := SELF:HyperLabel:NameSym
	ENDIF


	RETURN retVal


END CLASS


/// <include file="Gui.xml" path="doc/MinMaxInfoEvent/*" />
CLASS MinMaxInfoEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/MinMaxInfoEvent.ctor/*" />
	constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/MinMaxInfoEvent.MaxPosition/*" />
ACCESS MaxPosition AS POINT STRICT
	LOCAL sMinMax AS _WINMINMAXINFO


	sMinMax := PTR(_CAST, lParam)
	RETURN Point{sMinMax:ptMaxPosition:X, sMinMax:ptMaxPosition:Y}


/// <include file="Gui.xml" path="doc/MinMaxInfoEvent.MaxPosition/*" />
ASSIGN MaxPosition(oPoint AS point)  STRICT
	LOCAL oMaxPos  AS Point
	LOCAL sMinMax  AS _WINMINMAXINFO


	oMaxPos  := oPoint
	sMinMax  := PTR(_CAST, lParam)
	sMinMax:ptMaxPosition:X := oMaxPos:X
	sMinMax:ptMaxPosition:Y := oMaxPos:Y
	RETURN


/// <include file="Gui.xml" path="doc/MinMaxInfoEvent.MaxSize/*" />
ACCESS MaxSize AS Dimension STRICT
	LOCAL sMinMax AS _WINMINMAXINFO


	sMinMax := PTR(_CAST, lParam)
	RETURN Dimension{sMinMax:ptMaxSize:X, sMinMax:ptMaxSize:Y}


/// <include file="Gui.xml" path="doc/MinMaxInfoEvent.MaxSize/*" />
ASSIGN MaxSize(oSize AS Dimension)  STRICT
	LOCAL oMaxSize AS Dimension
	LOCAL sMinMax  AS _WINMINMAXINFO


	oMaxSize := oSize
	sMinMax  := PTR(_CAST, lParam)
	sMinMax:ptMaxSize:X := oMaxSize:Width
	sMinMax:ptMaxSize:Y := oMaxSize:Height
	RETURN


/// <include file="Gui.xml" path="doc/MinMaxInfoEvent.MaxTrackSize/*" />
ACCESS MaxTrackSize AS Dimension STRICT
	LOCAL sMinMax AS _WINMINMAXINFO


	sMinMax := PTR(_CAST, lParam)
	RETURN Dimension{sMinMax:ptMaxTrackSize:X, sMinMax:ptMaxTrackSize:Y}


/// <include file="Gui.xml" path="doc/MinMaxInfoEvent.MaxTrackSize/*" />
ASSIGN MaxTrackSize(oSize AS Dimension)  STRICT
	LOCAL oMaxSize AS Dimension
	LOCAL sMinMax  AS _WINMINMAXINFO


	oMaxSize := oSize
	sMinMax  := PTR(_CAST, lParam)
	sMinMax:ptMaxTrackSize:X := oMaxSize:Width
	sMinMax:ptMaxTrackSize:Y := oMaxSize:Height
	RETURN


/// <include file="Gui.xml" path="doc/MinMaxInfoEvent.MinTrackSize/*" />
ACCESS MinTrackSize AS Dimension STRICT
	LOCAL sMinMax AS _WINMINMAXINFO


	sMinMax := PTR(_CAST, lParam)
	RETURN Dimension{sMinMax:ptMinTrackSize:X, sMinMax:ptMinTrackSize:Y}


/// <include file="Gui.xml" path="doc/MinMaxInfoEvent.MinTrackSize/*" />
ASSIGN MinTrackSize(oSize AS Dimension)  STRICT
	LOCAL oMinSize AS Dimension
	LOCAL sMinMax  AS _WINMINMAXINFO


	oMinSize := oSize
	sMinMax  := PTR(_CAST, lParam)
	sMinMax:ptMinTrackSize:X := oMinSize:Width
	sMinMax:ptMinTrackSize:Y := oMinSize:Height
	RETURN


END CLASS


/// <include file="Gui.xml" path="doc/MonthCalSelectionEvent/*" />
CLASS MonthCalSelectionEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/MonthCalSelectionEvent.Explicit/*" />
	ACCESS Explicit AS LOGIC STRICT
	RETURN (SELF:NotifyCode == MCN_SELECT)


/// <include file="Gui.xml" path="doc/MonthCalSelectionEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/MonthCalSelectionEvent.Selection/*" />
ACCESS Selection AS DateRange STRICT
	LOCAL sc AS _winNMSELCHANGE


	sc := PTR(_CAST, lParam)
	RETURN DateRange{ConDate(sc:stSelStart:wYear, sc:stSelStart:wMonth, sc:stSelStart:wDay), ConDate(sc:stSelEnd:wYear, sc:stSelEnd:wMonth, sc:stSelEnd:wDay)}


END CLASS


/// <include file="Gui.xml" path="doc/MouseEvent/*" />
CLASS MouseEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/MouseEvent.ButtonID/*" />
	ACCESS ButtonID AS LONGINT STRICT
	LOCAL retVal AS LONGINT






	SWITCH uMsg
	CASE WM_LBUTTONDOWN
    CASE WM_LBUTTONUP
    CASE WM_LBUTTONDBLCLK
		retVal := ButtonLeft
	CASE WM_MBUTTONDOWN
    CASE WM_MBUTTONUP
    CASE WM_MBUTTONDBLCLK
		retVal := ButtonMiddle
	CASE WM_RBUTTONDOWN
    CASE WM_RBUTTONUP
    CASE WM_RBUTTONDBLCLK
		retVal := ButtonRight
	CASE WM_XBUTTONDOWN
    CASE WM_XBUTTONUP
    CASE WM_XBUTTONDBLCLK
		IF _AND(HiWord(wParam), XButton1) == XButton1
			retVal := ButtonX1
		ELSE
			retVal := ButtonX2
		ENDIF
	CASE WM_MOUSEMOVE
		retVal := LONGINT(_CAST, _AND(wParam, 0x001FU))
	OTHERWISE
		retVal := 0
	END SWITCH


	RETURN retVal


/// <include file="Gui.xml" path="doc/MouseEvent.Height/*" />
ACCESS Height AS LONGINT STRICT


	RETURN HiWord(DWORD(lParam))


/// <include file="Gui.xml" path="doc/MouseEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/MouseEvent.IsControlButton/*" />
ACCESS IsControlButton AS LOGIC STRICT


	RETURN _AND(wParam, ButtonControl) == ButtonControl


/// <include file="Gui.xml" path="doc/MouseEvent.IsLeftButton/*" />
ACCESS IsLeftButton AS LOGIC STRICT
	LOCAL retVal AS LOGIC






	SWITCH uMsg
	CASE WM_LBUTTONUP
    CASE WM_LBUTTONDOWN
    CASE WM_LBUTTONDBLCLK
		retVal := TRUE
	CASE WM_MOUSEMOVE
		retVal := _AND(wParam, ButtonLeft) == ButtonLeft
	OTHERWISE
		retVal := FALSE
	END SWITCH


	RETURN retVal


/// <include file="Gui.xml" path="doc/MouseEvent.IsMiddleButton/*" />
ACCESS IsMiddleButton AS LOGIC STRICT
	LOCAL retVal AS LOGIC






	SWITCH uMsg
	CASE WM_MBUTTONUP
    CASE WM_MBUTTONDOWN
    CASE WM_MBUTTONDBLCLK
		retVal := TRUE
	CASE WM_MOUSEMOVE
		retVal := _AND(wParam, ButtonMiddle) == ButtonMiddle
	OTHERWISE
		retVal := FALSE
	END SWITCH


	RETURN retVal


/// <include file="Gui.xml" path="doc/MouseEvent.IsRightButton/*" />
ACCESS IsRightButton AS LOGIC STRICT
	LOCAL retVal AS LOGIC


	SWITCH uMsg
    CASE WM_RBUTTONUP
    CASE WM_RBUTTONDOWN
    CASE WM_RBUTTONDBLCLK
		retVal := TRUE
	CASE WM_MOUSEMOVE
		retVal := _AND(wParam, ButtonRight) == ButtonRight
	OTHERWISE
		retVal := FALSE
	END SWITCH


	RETURN retVal


/// <include file="Gui.xml" path="doc/MouseEvent.IsShiftButton/*" />
ACCESS IsShiftButton AS LOGIC STRICT


	RETURN _AND(wParam, ButtonShift) == ButtonShift


/// <include file="Gui.xml" path="doc/MouseEvent.IsXButton1/*" />
ACCESS IsXButton1 AS LOGIC STRICT
	//PP-030904
	LOCAL retVal AS LOGIC






	SWITCH uMsg
    CASE WM_XBUTTONUP
    CASE WM_XBUTTONDOWN
    CASE WM_XBUTTONDBLCLK
		IF _AND(HiWord(wParam), XButton1) == XButton1
			retVal := TRUE
		ENDIF
	CASE WM_MOUSEMOVE
		retVal := _AND(wParam, ButtonX1) == ButtonX1
	OTHERWISE
		retVal := FALSE
	END SWITCH


	RETURN retVal


/// <include file="Gui.xml" path="doc/MouseEvent.IsXButton2/*" />
ACCESS IsXButton2 AS LOGIC STRICT
	//PP-030904
	LOCAL retVal AS LOGIC






	SWITCH uMsg
    CASE WM_XBUTTONUP
    CASE WM_XBUTTONDOWN
    CASE WM_XBUTTONDBLCLK
		IF _AND(HiWord(wParam), XButton2) == XButton2
			retVal := TRUE
		ENDIF
	CASE WM_MOUSEMOVE
		retVal := _AND(wParam, ButtonX2) == ButtonX2
	OTHERWISE
		retVal := FALSE
	END SWITCH


	RETURN retVal




/// <include file="Gui.xml" path="doc/MouseEvent.Position/*" />
ACCESS Position AS POINT STRICT
   //SE-080520
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD
	// In 1.0 there was a problem if the window was a control.
	// This should not be the case in 2.0, but you never really know!
	RETURN __WCConvertPoint(oWindow, Point{SHORT(_CAST, LoWord(dw)), SHORT(_CAST, HiWord(dw))})


/// <include file="Gui.xml" path="doc/MouseEvent.Size/*" />
ACCESS Size AS Dimension STRICT
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD
	RETURN Dimension{LoWord(dw), HiWord(dw)}


/// <include file="Gui.xml" path="doc/MouseEvent.Width/*" />
ACCESS Width AS LONGINT STRICT
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD
	RETURN LoWord(dw)


END CLASS


/// <include file="Gui.xml" path="doc/MoveEvent/*" />
CLASS MoveEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/MoveEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/MoveEvent.Origin/*" />
ACCESS Origin AS Point STRICT
  //SE-080520
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD
	RETURN __WCConvertPoint(oWindow, Point{SHORT(_CAST, LoWord(dw)), SHORT(_CAST, HiWord(dw))})




END CLASS


/// <include file="Gui.xml" path="doc/PrinterErrorEvent/*" />
CLASS PrinterErrorEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/PrinterErrorEvent.ErrorType/*" />
	ACCESS ErrorType AS DWORD STRICT


	RETURN wParam


/// <include file="Gui.xml" path="doc/PrinterErrorEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return

end class


/// <include file="Gui.xml" path="doc/PrinterExposeEvent/*" />
CLASS PrinterExposeEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/PrinterExposeEvent.ExposedArea/*" />
	ACCESS ExposedArea AS OBJECT STRICT




	RETURN oWindow


/// <include file="Gui.xml" path="doc/PrinterExposeEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/PrinterExposeEvent.PageNo/*" />
ACCESS PageNo AS DWORD STRICT




	RETURN wParam


END CLASS


/// <include file="Gui.xml" path="doc/ResizeEvent/*" />
CLASS ResizeEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/ResizeEvent.Height/*" />
	ACCESS Height AS LONGINT STRICT
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD
	RETURN HiWord(dw)


/// <include file="Gui.xml" path="doc/ResizeEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/ResizeEvent.Size/*" />
ACCESS Size AS Dimension STRICT
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD
	RETURN Dimension{LoWord(dw), HiWord(dw)}


/// <include file="Gui.xml" path="doc/ResizeEvent.Width/*" />
ACCESS Width AS LONGINT STRICT
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD
	RETURN LoWord(dw)


END CLASS


/// <include file="Gui.xml" path="doc/RichEditProtectEvent/*" />
CLASS RichEditProtectEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/RichEditProtectEvent.ctor/*" />
	constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/RichEditProtectEvent.Selection/*" />
ACCESS Selection AS Selection STRICT
	//PP-030910
	LOCAL strucENProtect AS _winENProtected






	strucENProtect := PTR(_CAST, lParam)


	RETURN Selection{strucENProtect:chrg:cpMin + 1, strucENProtect:chrg:cpMax + 1}




/// <include file="Gui.xml" path="doc/RichEditProtectEvent.SelectionRange/*" />
ACCESS SelectionRange AS Range STRICT
	LOCAL strucENProtect AS _winENProtected






	strucENProtect := PTR(_CAST, lParam)


	RETURN Range{strucENProtect:chrg:cpMin + 1, strucENProtect:chrg:cpMax + 1}


END CLASS


/// <include file="Gui.xml" path="doc/RichEditSelectionEvent/*" />
CLASS RichEditSelectionEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/RichEditSelectionEvent.ctor/*" />
	constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/RichEditSelectionEvent.Selection/*" />
ACCESS Selection AS Selection STRICT
	//PP-030910
	LOCAL strucSelChange AS _winSelChange






	strucSelChange := PTR(_CAST, lParam)
	RETURN Selection{strucSelChange:chrg:cpMin + 1, strucSelChange:chrg:cpMax + 1}


/// <include file="Gui.xml" path="doc/RichEditSelectionEvent.SelectionRange/*" />
ACCESS SelectionRange AS Range STRICT
	LOCAL strucSelChange AS _winSelChange






	strucSelChange := PTR(_CAST, lParam)
	RETURN Range{strucSelChange:chrg:cpMin + 1, strucSelChange:chrg:cpMax + 1}


/// <include file="Gui.xml" path="doc/RichEditSelectionEvent.SelectionType/*" />
ACCESS SelectionType AS LONGINT STRICT
	LOCAL strucSelChange AS _winSelChange






	strucSelChange := PTR(_CAST, lParam)
	RETURN strucSelChange:seltyp


END CLASS


/// <include file="Gui.xml" path="doc/ScrollEvent/*" />
CLASS ScrollEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/ScrollEvent.ctor/*" />
	constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/ScrollEvent.IsWindowScroll/*" />
ACCESS IsWindowScroll AS LOGIC STRICT




	RETURN (lParam == 0)


/// <include file="Gui.xml" path="doc/ScrollEvent.OldPosition/*" />
ACCESS OldPosition AS LONGINT STRICT
	//SE-051114
	LOCAL sScrollInfo IS _WINSCROLLINFO
	LOCAL hHandle     AS PTR
	LOCAL oSB         AS OBJECT
	LOCAL dwSBType    AS DWORD






	oSB := SELF:ScrollBar


	IF oSB != NULL_OBJECT
		RETURN oSB:ThumbPosition
	ENDIF


	hHandle := PTR(_CAST, lParam)
	IF (hHandle != NULL_PTR)
		dwSBType := SB_CTL
	ELSE
		hHandle := oWindow:Owner:Handle()
		IF (uMsg == WM_HSCROLL)
			dwSBType := SB_HORZ
		ELSE
			dwSBType := SB_VERT
		ENDIF
	ENDIF


	sScrollInfo:cbSize := _SIZEOF(_WINSCROLLINFO)
	sScrollInfo:fMask  := SIF_POS
	GetScrollInfo(hHandle, INT(_CAST, dwSBType), @sScrollInfo)
	RETURN sScrollInfo:nPos




/// <include file="Gui.xml" path="doc/ScrollEvent.Position/*" />
ACCESS Position AS LONGINT STRICT
	//SE-051114
	//SE-070421
	LOCAL strucScrollInfo IS _winScrollInfo
	LOCAL hHandle AS PTR
	LOCAL oSB       AS ScrollBar
	LOCAL oRange    AS Range
	LOCAL liRetVal  AS LONG
	LOCAL nBlock, nUnit, nSBType, nPage AS LONG
	LOCAL nMin, nMax AS LONG
	LOCAL dwType AS DWORD
	LOCAL lSlider AS LOGIC


	oSB := SELF:ScrollBar
	liRetVal:= SELF:OldPosition
    lSlider     := IsInstanceOf(oSB, #Slider)
	strucScrollInfo:cbSize := _SIZEOF(_winSCROLLINFO)


	IF oSB != NULL_OBJECT
		oRange  := oSB:Range
		nBlock  := oSB:BlockSize
		nUnit   := oSB:UnitSize
		hHandle := oSB:Handle()
		nSBType := oSB:__Type
		IF hHandle != NULL_PTR
			strucScrollInfo:fMask  := SIF_ALL //_OR(SIF_PAGE, SIF_POS, SIF_TRACKPOS)
			GetScrollInfo(hHandle, nSBType, @strucScrollInfo )
	      nPage   := INT(_CAST, strucScrollInfo:nPage)
		ENDIF
		nMin   := oRange:Min
		nMax   := oRange:Max
	ELSE
		nBlock  := 10
		nUnit   := 1
		hHandle := PTR(_CAST, lParam)
		IF (hHandle != 0)
			nSBType := SB_CTL
		ELSE
			hHandle := oWindow:Owner:Handle()
			IF (uMsg == WM_HSCROLL)
				nSBType := SB_HORZ
			ELSE
				nSBType := SB_VERT
			ENDIF
		ENDIF
		strucScrollInfo:fMask := SIF_ALL // _OR(SIF_RANGE, SIF_PAGE, SIF_POS, SIF_TRACKPOS)
		GetScrollInfo(hHandle, nSBType, @strucScrollInfo )
		nMin    := strucScrollInfo:nMin
		nMax    := strucScrollInfo:nMax
	ENDIF


	dwType := LoWord(wParam)
    //RvdH 081105 Sliders need to be handled differently
    IF lSlider
    	SWITCH dwType
    	CASE SB_ThumbTrack
        CASE SB_ThumbPosition
    		liRetVal := SHORT(_CAST, HiWord(wParam))  // Note that the high word is signed !
    	CASE SB_LINEDOWN
    		IF (liRetVal + nUnit + nPage) > nMax
    			liRetVal := nMax
    		ELSE
    			liRetVal += nUnit
    		ENDIF
    	CASE SB_LINEUP
    		IF (liRetVal - nUnit) < nMin
    			liRetVal := nMin
    		ELSE
    			liRetVal -= nUnit
    		ENDIF
    	CASE SB_PAGEDOWN
    		IF (liRetVal + nBlock + nPage) > nMax
    			liRetVal := nMax
    		ELSE
    			liRetVal += nBlock
    		ENDIF
    	CASE SB_PAGEUP
    		IF (liRetVal - nBlock) < nMin
    			liRetVal := nMin
    		ELSE
    			liRetVal -= nBlock
    		ENDIF
    	END SWITCH
    ELSE
    	SWITCH dwType


    	CASE SB_THUMBPOSITION
        CASE SB_THUMBTRACK
    		liRetVal := strucScrollInfo:nTrackPos //nPos
    	CASE SB_LINEDOWN
    		IF strucScrollInfo:nPage > 1L
    		   nMax -= LONG(_CAST,strucScrollInfo:nPage) - 1L
    		ENDIF
    		IF liRetVal + nUnit > nMax
    			liRetVal := nMax
    		ELSE
    			liRetVal += nUnit
    		ENDIF
    	CASE SB_LINEUP
    		IF (liRetVal - nUnit) < nMin
    			liRetVal := nMin
    		ELSE
    			liRetVal -= nUnit
    		ENDIF
    	CASE SB_PAGEDOWN
    		IF strucScrollInfo:nPage > 1L
    		   nMax -= LONG(_CAST,strucScrollInfo:nPage) - 1L
    		ENDIF
    		IF liRetVal + nBlock > nMax
    			liRetVal := nMax
    		ELSE
    			liRetVal += nBlock
    		ENDIF
    	CASE SB_PAGEUP
    		IF (liRetVal - nBlock) < nMin
    			liRetVal := nMin
    		ELSE
    			liRetVal -= nBlock
    		ENDIF
    	END SWITCH
    ENDIF
	RETURN liRetVal


/// <include file="Gui.xml" path="doc/ScrollEvent.ScrollBar/*" />
ACCESS ScrollBar AS Scrollbar STRICT




	IF (lParam == 0) .AND. IsMethod(oWindow, #ENABLEVERTICALSCROLL)
		IF (uMsg == WM_HSCROLL)
			RETURN oWindow:EnableHorizontalScroll()
		ELSE
			RETURN oWindow:EnableVerticalScroll()
		ENDIF
	ENDIF


	RETURN (ScrollBar) __WCGetControlByHandle(PTR(_CAST,lParam))


/// <include file="Gui.xml" path="doc/ScrollEvent.ScrollBarID/*" />
ACCESS ScrollBarID AS LONGINT STRICT
	LOCAL myScrollbar AS ScrollBar




	myScrollbar := SELF:ScrollBar
	IF myScrollbar!=NULL_OBJECT
		RETURN myScrollBar:ControlID
	ENDIF


	RETURN 0


/// <include file="Gui.xml" path="doc/ScrollEvent.TYPE/*" />
ACCESS TYPE AS LONGINT STRICT
	LOCAL dwType AS DWORD




	dwType:=LoWord(wParam)
	DO CASE
	CASE dwType==SB_ThumbPosition
		RETURN ScrollEnd
	CASE dwType==SB_PageDown
		RETURN BlockIncrement
	CASE dwType==SB_LineDown
		RETURN UnitIncrement
	CASE dwType==SB_ThumbTrack
		RETURN ScrollThumbDrag
	CASE dwType==SB_LineUp
		RETURN UnitDecrement
	CASE dwType==SB_PageUp
		RETURN BlockDecrement
	CASE dwType==SB_Top
		RETURN ScrollToTopLeft
	CASE dwType==SB_Bottom
		RETURN ScrollToBottomRight
	ENDCASE


	RETURN ScrollEnd


END CLASS


/// <include file="Gui.xml" path="doc/SliderEvent/*" />
CLASS SliderEvent INHERIT ScrollEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/SliderEvent.ctor/*" />
	constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/SliderEvent.IsWindowScroll/*" />
ACCESS IsWindowScroll AS LOGIC STRICT


	RETURN FALSE


/// <include file="Gui.xml" path="doc/SliderEvent.Slider/*" />
ACCESS Slider AS OBJECT STRICT


	RETURN __WCGetControlByHandle(PTR(_CAST,lParam))


END CLASS


/// <include file="Gui.xml" path="doc/SpinnerEvent/*" />
CLASS SpinnerEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/SpinnerEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/SpinnerEvent.OldPosition/*" />
ACCESS OldPosition AS LONGINT STRICT
	LOCAL oS AS Spinner
	//LOCAL dwTemp AS DWORD
    LOCAL lOk   AS LOGIC
    LOCAL lRetVal AS LONG
	oS := SELF:Spinner
	IF (oS != NULL_OBJECT)
		RETURN oS:Position
	ENDIF
	IF (lParam != 0)
	    //RvdH 081212 Use 32 bit version to get full range of values
		//dwTemp := LoWord(DWORD(SendMessage(PTR(_CAST, lParam), UDM_GetPos, 0, 0)))
		//IF HiWord(dwTemp) == 0
		//	RETURN LoWord(dwTemp)
		//ENDIF
        lRetVal := 	SendMessage(oS:Handle(), UDM_GETPOS32, 0, LONG(_CAST, @lOk))
        IF (lOk)
            RETURN lRetVal
        ENDIF


	ENDIF


	RETURN 0


/// <include file="Gui.xml" path="doc/SpinnerEvent.OldValue/*" />
ACCESS OldValue AS LONGINT STRICT
RETURN SELF:OldPosition


/// <include file="Gui.xml" path="doc/SpinnerEvent.Position/*" />
ACCESS Position AS LONGINT STRICT
	LOCAL oS AS Spinner
	LOCAL oRange AS OBJECT
	LOCAL liRetVal, wUnit AS LONG
	//LOCAL dwTemp AS DWORD
	LOCAL iLow, iHigh AS LONG
   LOCAL wLow, wHigh   AS WORD


	oS := SELF:Spinner
	liRetVal := SELF:OldPosition


	IF (oS != NULL_OBJECT)
		oRange := oS:Range
		wUnit  := oS:UnitSize
	ELSE
		wUnit := 1
		// RvdH 081212 Use 32 bit version to get whole range
		//dwTemp := DWORD(SendMessage(PTR(_CAST, lParam), UDM_GETRANGE, 0, 0))
		//oRange := Range{LoWord(dwTemp), HiWord(dwTemp)}
		SendMessage(oS:Handle(), UDM_GETRANGE32, DWORD(_CAST, @iLow), LONG(_CAST, @iHigh))
		oRange := Range{iLow, iHigh}
	ENDIF


   wLow    := LoWord(wParam)
   wHigh   := HiWord(wParam)


	SWITCH wLow
	CASE SB_LINEDOWN
		IF (liRetVal + wUnit) > oRange:Max
			liRetVal := oRange:Max
		ELSE
			liRetVal += wUnit
		ENDIF


	CASE SB_LINEUP
		IF (liRetVal - wUnit) < oRange:Min
			liRetVal := oRange:Min
		ELSE
			liRetVal -= wUnit
		ENDIF
	CASE SB_THUMBPOSITION
	    liRetVal := SHORT(_CAST,wHigh) // Note that the high word is signed !
	CASE SB_THUMBTRACK
	    liRetVal := SHORT(_CAST,wHigh) // Note that the high word is signed !
	END SWITCH


	RETURN LONGINT(liRetVal)


/// <include file="Gui.xml" path="doc/SpinnerEvent.Spinner/*" />
ACCESS Spinner AS OBJECT STRICT
	LOCAL oS AS OBJECT
	oS :=  __WCGetControlByHandle(PTR(_CAST, lParam))
	IF IsInstanceOf(oS, #Spinner)
	    RETURN oS
	ENDIF
	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/SpinnerEvent.SpinnerID/*" />
ACCESS SpinnerID AS LONGINT STRICT
	LOCAL oS AS Spinner
	oS := SELF:Spinner
	IF (oS != NULL_OBJECT)
		RETURN oS:ControlID
	ENDIF


	RETURN 0


/// <include file="Gui.xml" path="doc/SpinnerEvent.Type/*" />
ACCESS Type AS LONGINT STRICT
	LOCAL wType AS WORD
	wType := LoWord(wParam)
	SWITCH wType
	CASE SB_LINEDOWN
		RETURN UnitIncrement
	CASE SB_LINEUP
		RETURN UnitDecrement
	CASE SB_TOP
		RETURN ScrollToTopLeft
	CASE SB_BOTTOM
		RETURN ScrollToBottomRight
	END SWITCH


	RETURN ScrollEnd


/// <include file="Gui.xml" path="doc/SpinnerEvent.Value/*" />
ACCESS Value AS LONGINT STRICT






	RETURN SELF:Position


END CLASS


/// <include file="Gui.xml" path="doc/SysLinkSelectEvent/*" />
CLASS SysLinkSelectEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/SysLinkSelectEvent.ID/*" />
	ACCESS ID AS STRING STRICT
	LOCAL DIM szUrl[MAX_LINKID_TEXT] AS BYTE
	LOCAL nml AS _winNMLink
	nml := PTR(_CAST, lParam)


	WideCharToMultiByte(CP_ACP, 0, @nml:item:szID[1], -1, PSZ(_CAST, @szUrl[1]), MAX_LINKID_TEXT, NULL_PTR, NULL_PTR)
	RETURN Psz2String(@szUrl[1])


/// <include file="Gui.xml" path="doc/SysLinkSelectEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/SysLinkSelectEvent.LinkIndex/*" />
ACCESS LinkIndex AS LONGINT STRICT
	LOCAL nml AS _winNMLink
	nml := PTR(_CAST, lParam)
	RETURN nml:item:iLink


/// <include file="Gui.xml" path="doc/SysLinkSelectEvent.URL/*" />
ACCESS URL AS STRING STRICT
	LOCAL DIM szUrl[L_MAX_URL_LENGTH] AS BYTE
	LOCAL nml AS _winNMLink
	nml := PTR(_CAST, lParam)


	WideCharToMultiByte(CP_ACP, 0, @nml:item:szUrl[1], -1, PSZ(_CAST, @szUrl[1]), L_MAX_URL_LENGTH, NULL_PTR, NULL_PTR)
	RETURN Psz2String(@szUrl[1])


END CLASS


/// <include file="Gui.xml" path="doc/TreeViewDeleteEvent/*" />
CLASS TreeViewDeleteEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/TreeViewDeleteEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/TreeViewDeleteEvent.TreeViewItem/*" />
ACCESS TreeViewItem AS TreeViewItem STRICT
	LOCAL strucTreeView AS _winNM_TreeView
	LOCAL oTreeView AS TreeView






	strucTreeView := PTR(_CAST, lParam)
	oTreeView 		:= OBJECT(SELF:Control)


	RETURN oTreeView:GetItemAttributes(strucTreeView:itemOld:hItem)


END CLASS


/// <include file="Gui.xml" path="doc/TreeViewDragEvent/*" />
CLASS TreeViewDragEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/TreeViewDragEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/TreeViewDragEvent.IsLeftButton/*" />
ACCESS IsLeftButton AS LOGIC STRICT






	RETURN (SELF:NotifyCode == TVN_BEGINDRAG)


/// <include file="Gui.xml" path="doc/TreeViewDragEvent.IsRightButton/*" />
ACCESS IsRightButton AS LOGIC STRICT




	RETURN (SELF:NotifyCode == TVN_BEGINRDRAG)


/// <include file="Gui.xml" path="doc/TreeViewDragEvent.Position/*" />
ACCESS Position AS Point STRICT
	LOCAL strucTreeView AS _winNM_TREEVIEW
	LOCAL oPoint AS Point






	strucTreeView := PTR(_CAST, SELF:lParam)


	oPoint := Point{strucTreeView:ptDrag:x, strucTreeView:ptDrag:y}
	oPoint := __WCConvertPoint(SELF:Control, oPoint)


	RETURN oPoint


/// <include file="Gui.xml" path="doc/TreeViewDragEvent.TreeViewItem/*" />
ACCESS TreeViewItem AS TreeViewItem STRICT
	LOCAL strucTreeView AS _winNM_TREEVIEW
	LOCAL oTreeView AS TreeView






	strucTreeView := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)


	RETURN oTreeView:GetItemAttributes(strucTreeView:itemNew:hItem)


END CLASS


/// <include file="Gui.xml" path="doc/TreeViewEditEvent/*" />
CLASS TreeViewEditEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/TreeViewEditEvent.EditBeginning/*" />
	ACCESS EditBeginning AS LOGIC STRICT




	RETURN (SELF:NotifyCode == TVN_BEGINLABELEDIT)


/// <include file="Gui.xml" path="doc/TreeViewEditEvent.EditEnding/*" />
ACCESS EditEnding AS LOGIC STRICT




	RETURN (SELF:NotifyCode == TVN_ENDLABELEDIT)


/// <include file="Gui.xml" path="doc/TreeViewEditEvent.EditText/*" />
ACCESS EditText  AS STRING STRICT
	LOCAL strucDispInfo AS _winTV_DispInfo






	strucDispInfo := PTR(_CAST, SELF:lParam)


	IF SELF:EditEnding
		RETURN Psz2String(strucDispInfo:item:pszText)
	ENDIF
	RETURN NULL_STRING


/// <include file="Gui.xml" path="doc/TreeViewEditEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/TreeViewEditEvent.TreeViewItem/*" />
ACCESS TreeViewItem AS TreeViewItem STRICT
	LOCAL strucDispInfo AS _winTV_DispInfo
	LOCAL oTreeView AS TreeView






	strucDispInfo := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)


	RETURN oTreeView:GetItemAttributes(strucDispInfo:item:hItem)


END CLASS


/// <include file="Gui.xml" path="doc/TreeViewExpandedEvent/*" />
CLASS TreeViewExpandedEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/TreeViewExpandedEvent.Collapsed/*" />
	ACCESS Collapsed AS LOGIC STRICT
	LOCAL strucTreeView AS _winNM_TreeView






	strucTreeView := PTR(_CAST, SELF:lParam)


	RETURN (strucTreeView:action == TVE_COLLAPSE)


/// <include file="Gui.xml" path="doc/TreeViewExpandedEvent.Expanded/*" />
ACCESS Expanded  AS LOGIC STRICT
	LOCAL strucTreeView AS _winNM_TreeView






	strucTreeView := PTR(_CAST, SELF:lParam)


	RETURN (strucTreeView:action == TVE_EXPAND)


/// <include file="Gui.xml" path="doc/TreeViewExpandedEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/TreeViewExpandedEvent.TreeViewItem/*" />
ACCESS TreeViewItem AS TreeViewItem STRICT
	LOCAL strucTreeView AS _winNM_TreeView
	LOCAL oTreeView AS TreeView






	strucTreeView := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)


	RETURN oTreeView:GetItemAttributes(strucTreeView:itemNew:hItem)


END CLASS


/// <include file="Gui.xml" path="doc/TreeViewExpandingEvent/*" />
CLASS TreeViewExpandingEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/TreeViewExpandingEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/TreeViewExpandingEvent.TreeViewItem/*" />
ACCESS TreeViewItem AS TreeViewItem STRICT
	LOCAL strucTreeView AS _winNM_TreeView
	LOCAL oTreeView AS TreeView






	strucTreeView := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)


	RETURN oTreeView:GetItemAttributes(strucTreeView:itemNew:hItem)


END CLASS


/// <include file="Gui.xml" path="doc/TreeViewKeyEvent/*" />
CLASS TreeViewKeyEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/TreeViewKeyEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/TreeViewKeyEvent.KeyCode/*" />
ACCESS KeyCode AS LONGINT STRICT
	LOCAL strucKeyDown AS _winTV_KeyDown






	strucKeyDown := PTR(_CAST, SELF:LParam)
	RETURN strucKeyDown:wVKey


END CLASS


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent/*" />
CLASS TreeViewMouseEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	PROTECT nX AS LONGINT
	PROTECT nY AS LONGINT
/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.ButtonID/*" />
	ACCESS ButtonID AS LONGINT STRICT
	LOCAL dwNotifyCode AS DWORD






	dwNotifyCode := SELF:NotifyCode


	IF (dwNotifyCode == NM_CLICK) .OR. (dwNotifyCode == NM_DBLCLK)
		RETURN BUTTONLEFT
	ENDIF


	IF (dwNotifyCode == NM_RCLICK) .OR. (dwNotifyCode == NM_RDBLCLK)
		RETURN BUTTONRIGHT
	ENDIF


	RETURN 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.ctor/*" />
CONSTRUCTOR(oControlNotifyEvent)
	LOCAL strucPoint IS _winPoint






	SUPER(oControlNotifyEvent)


	GetCursorPos(@strucPoint)
	ScreenToClient(SELF:Control:Handle(), @strucPoint)
	nX := strucPoint:x
	nY := strucPoint:y


	RETURN


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.IsLeftButton/*" />
ACCESS IsLeftButton AS LOGIC STRICT
	//RvdH 061214 Added type (was untyped)
	LOCAL dwNotifyCode  AS DWORD






	dwNotifyCode := SELF:NotifyCode
	IF (dwNotifyCode == NM_CLICK) .OR. (dwNotifyCode == NM_DBLCLK)
		RETURN TRUE
	ENDIF


	RETURN FALSE


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.IsRightButton/*" />
ACCESS IsRightButton  AS LOGIC STRICT
	//RvdH 061214 Added type (was untyped)
	LOCAL dwNotifyCode AS DWORD






	dwNotifyCode := SELF:NotifyCode


	RETURN ((dwNotifyCode == NM_RCLICK) .OR. (dwNotifyCode == NM_RDBLCLK))


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointAboveClientArea/*" />
ACCESS PointAboveClientArea  AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_ABOVE) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointBelowClientArea/*" />
ACCESS PointBelowClientArea  AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_BELOW) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointLeftOfClientArea/*" />
ACCESS PointLeftOfClientArea  AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_TOLEFT) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointNowhere/*" />
ACCESS PointNowhere  AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_NOWHERE) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointOnItem/*" />
ACCESS PointOnItem  AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEM) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointOnItemButton/*" />
ACCESS PointOnItemButton  AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMBUTTON) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointOnItemImage/*" />
ACCESS PointOnItemImage  AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMICON) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointOnItemIndent/*" />
ACCESS PointOnItemIndent  AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMINDENT) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointOnItemLabel/*" />
ACCESS PointOnItemLabel  AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMLABEL) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointOnItemRight/*" />
ACCESS PointOnItemRight AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMRIGHT) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointOnItemStateImage/*" />
ACCESS PointOnItemStateImage  AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMSTATEICON) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.PointRightOfClientArea/*" />
ACCESS PointRightOfClientArea  AS LOGIC STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo






	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)


	RETURN _AND(strucHitTestInfo:flags, TVHT_TORIGHT) != 0


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.Position/*" />
ACCESS Position AS Point STRICT






	RETURN __WCConvertPoint(SELF:Control, Point{nX, nY})


/// <include file="Gui.xml" path="doc/TreeViewMouseEvent.TreeViewItem/*" />
ACCESS TreeViewItem AS TreeViewItem STRICT
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo
	LOCAL oTreeView AS TreeView






	oTreeView := OBJECT(SELF:Control)
	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(oTreeView:Handle(), @strucHitTestInfo)


	RETURN oTreeView:GetItemAttributes(strucHitTestInfo:hItem)


END CLASS


/// <include file="Gui.xml" path="doc/TreeViewSelectionEvent/*" />
CLASS TreeViewSelectionEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
/// <include file="Gui.xml" path="doc/TreeViewSelectionEvent.ctor/*" />
constructor(args params usual[])
    super(args)
return



/// <include file="Gui.xml" path="doc/TreeViewSelectionEvent.KeyBoardAction/*" />
ACCESS KeyBoardAction AS LOGIC STRICT
	LOCAL strucTreeView AS _winNM_TreeView






	strucTreeView := PTR(_CAST, SELF:lParam)


	RETURN (strucTreeView:action == TVC_BYKEYBOARD)


/// <include file="Gui.xml" path="doc/TreeViewSelectionEvent.MouseAction/*" />
ACCESS MouseAction AS LOGIC STRICT
	LOCAL strucTreeView AS _winNM_TreeView






	strucTreeView := PTR(_CAST, SELF:lParam)


	RETURN (strucTreeView:action == TVC_BYMOUSE)


/// <include file="Gui.xml" path="doc/TreeViewSelectionEvent.NewTreeViewItem/*" />
ACCESS NewTreeViewItem AS TreeViewItem STRICT
	LOCAL strucTreeView AS _winNM_TreeView
	LOCAL oTreeView AS TreeView






	strucTreeView := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)


	RETURN oTreeView:GetItemAttributes(strucTreeView:itemNew:hItem)


/// <include file="Gui.xml" path="doc/TreeViewSelectionEvent.OldTreeViewItem/*" />
ACCESS OldTreeViewItem AS TreeViewItem STRICT
	LOCAL strucTreeView AS _winNM_TreeView
	LOCAL oTreeView AS TreeView






	strucTreeView := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)


	RETURN oTreeView:GetItemAttributes(strucTreeView:itemOld:hItem)


/// <include file="Gui.xml" path="doc/TreeViewSelectionEvent.SelectionChanged/*" />
ACCESS SelectionChanged  AS LOGIC STRICT




	RETURN (SELF:NotifyCode == TVN_SELCHANGEDA)


/// <include file="Gui.xml" path="doc/TreeViewSelectionEvent.SelectionChanging/*" />
ACCESS SelectionChanging  AS LOGIC STRICT




	RETURN (SELF:NotifyCode == TVN_SELCHANGINGA)


/// <include file="Gui.xml" path="doc/TreeViewSelectionEvent.UnknownAction/*" />
ACCESS UnknownAction  AS LOGIC STRICT
	LOCAL strucTreeView AS _winNM_TreeView






	strucTreeView := PTR(_CAST, SELF:lParam)
	RETURN (strucTreeView:action == TVC_UNKNOWN)


END CLASS


 /// <exclude />
PROCEDURE __InitClassPointer _INIT3
RETURN
