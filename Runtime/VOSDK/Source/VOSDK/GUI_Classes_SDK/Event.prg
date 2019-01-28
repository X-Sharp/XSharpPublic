STATIC DEFINE SYSTEM_KEYCODE := 0x20000000

CLASS AppCommandEvent INHERIT @@Event
	//PP-030904
	//RvdH 061218 Declared properties for performance
	ACCESS Command AS DWORD STRICT 
	RETURN Get_AppCommand_lParam(DWORD(_CAST,SELF:lParam))

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS IsControl AS LOGIC STRICT 
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_CONTROL) > 0

ACCESS IsDeviceKey AS LOGIC STRICT 
	RETURN Get_Device_lParam(DWORD(_CAST,SELF:lParam)) == FAPPCOMMAND_KEY

ACCESS IsDeviceMouse AS LOGIC STRICT 
	RETURN Get_Device_lParam(DWORD(_CAST,SELF:lParam)) == FAPPCOMMAND_MOUSE

ACCESS IsDeviceOEM AS LOGIC STRICT 
	RETURN Get_Device_lParam(DWORD(_CAST,SELF:lParam)) == FAPPCOMMAND_OEM

ACCESS IsLeftButton AS LOGIC STRICT 
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_LBUTTON) > 0

ACCESS IsMiddleButton AS LOGIC STRICT 
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_MBUTTON) > 0

ACCESS IsRightButton AS LOGIC STRICT 
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_RBUTTON) > 0

ACCESS IsShift AS LOGIC STRICT 
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_SHIFT) > 0

ACCESS IsXButton1 AS LOGIC STRICT 
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_XBUTTON1) > 0

ACCESS IsXButton2 AS LOGIC STRICT 
	RETURN _AND(WORD(Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_XBUTTON2) > 0

ACCESS oTarget AS OBJECT STRICT 
	RETURN __WCGetObjectByHandle(PTR(_CAST,SELF:wParam))


END CLASS

CLASS ComboBoxExEndEditEvent INHERIT ControlNotifyEvent
	//SE-060519              
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS IsChanged AS LOGIC STRICT 
	//SE-060519
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT

	

	sNMCBEENDEDIT := PTR(_CAST, SELF:lParam)
	RETURN sNMCBEENDEDIT:fChanged

ACCESS NewSelection AS LONGINT STRICT 
	//SE-060519
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT

	

	sNMCBEENDEDIT := PTR(_CAST, SELF:lParam)
	RETURN sNMCBEENDEDIT:iNewSelection + 1l

ACCESS TextValue AS STRING STRICT 
	//SE-060519
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT

	

	sNMCBEENDEDIT := PTR(_CAST, SELF:lParam)
	IF sNMCBEENDEDIT:szText[1] = 0
		RETURN NULL_STRING
	ENDIF
   RETURN Psz2String(PSZ(_CAST, @sNMCBEENDEDIT:szText[1]))

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

CLASS ControlEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance

ACCESS Control AS Control STRICT 
	RETURN __WCGetControlByHandle(lParam)

ACCESS ControlID AS LONGINT STRICT 
	RETURN LoWord(wParam)

ACCESS Description AS STRING STRICT 
	LOCAL oHL AS HyperLabel

	
	oHL:=SELF:HyperLabel
	IF oHL!=NULL_OBJECT
		RETURN oHL:Description
	ENDIF
	RETURN NULL_STRING

ACCESS HelpContext AS STRING STRICT 
	LOCAL oHL AS HyperLabel

	
	oHL:=SELF:HyperLabel

	IF oHL!=NULL_OBJECT
		RETURN oHL:HelpContext
	ENDIF

	RETURN NULL_STRING

ACCESS HyperLabel AS HyperLabel STRICT 
	LOCAL oControl AS Control

	

	oControl:=SELF:Control
	IF oControl!=NULL_OBJECT
		RETURN oControl:HyperLabel
	ENDIF

	RETURN NULL_OBJECT

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS Name AS STRING STRICT 
	LOCAL oHL AS HyperLabel

	

	oHL:=SELF:HyperLabel
	IF oHL!=NULL_OBJECT
		RETURN oHL:Name
	ENDIF

	RETURN NULL_STRING

ACCESS NameSym AS SYMBOL STRICT 
	LOCAL oHL AS HyperLabel

	

	oHL:=SELF:HyperLabel
	IF oHL!=NULL_OBJECT
		RETURN oHL:NameSym
	ENDIF

	RETURN NULL_SYMBOL

END CLASS

CLASS ControlFocusChangeEvent INHERIT FocusChangeEvent  
	//RvdH 061218 Declared properties for performance
	ACCESS Control AS CONTROL STRICT 
  RETURN SELF:Window

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 
END CLASS

CLASS ControlNotifyEvent INHERIT ControlEvent
	//RvdH 061218 Declared properties for performance
ACCESS Control AS Control STRICT 
	LOCAL strucNotify AS _winNMHDR
	strucNotify := PTR(_CAST, SELF:LParam)
	RETURN __WCGetControlByHandle(strucNotify:hwndFrom)

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS NotifyCode AS DWORD STRICT 
	LOCAL strucNotify AS _winNMHDR

	

	strucNotify := PTR(_CAST, SELF:lParam)

	RETURN  strucNotify:_code

END CLASS

CLASS DateTimeSelectionEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS SelectedDate AS DATE STRICT 
	LOCAL sc AS _winNMDATETIMECHANGE

	sc := PTR(_CAST, lParam)
	RETURN ConDate(sc:st:wYear, sc:st:wMonth, sc:st:wDay)

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

CLASS DragEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	PROTECT oControl AS Control
	ACCESS Control AS OBJECT STRICT 
	

	RETURN SELF:oControl

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
		RETURN PCALL(gpfnDragQueryFile, wParam, 0Xffffffff, NULL_PSZ, 0)
	ENDIF

	RETURN 0

METHOD FileName(nfile) 
	LOCAL dwSize AS DWORD
	LOCAL pszBuf AS PSZ
	LOCAL cBuf AS STRING

	

	IF !IsLong(nFile)
		WCError{#FileName,#DragEvent,__WCSTypeError,nfile,1}:@@Throw()
	ENDIF

	nfile--

	IF ((uMsg == WM_DROPFILES) .OR. ((uMsg == WM_QUERYDROPOBJECT) .AND. (wParam != 0))) .AND. __LoadShellDll()
		dwSize := PCALL(gpfnDragQueryFile, PTR(_CAST, wParam), nfile, NULL_PSZ, 0) + 1
		IF (dwSize > 0)
			pszBuf := MemAlloc(dwSize)
			PCALL(gpfnDragQueryFile, PTR(_CAST, wParam), nfile, pszBuf, dwSize)
			cBuf := Psz2String(pszBuf)
			MemFree(pszBuf)
		ENDIF
	ENDIF

	RETURN cBuf

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
	

	SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)

	IF !IsPtr(_hWnd)
		IF IsInstanceOfUsual(_uMsg, #Control)
			SELF:oControl := _uMsg
		ENDIF
	ENDIF

	RETURN 

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

CLASS EditFocusChangeEvent INHERIT ControlEvent 
	//RvdH 061218 Declared properties for performance
	ACCESS GotFocus AS LOGIC STRICT 
	LOCAL dwHiWord AS DWORD

	dwHiWord:=HiWord(wParam)

	RETURN (dwHiWord==EN_SetFocus) .OR. (dwHiWord==CBN_SetFocus)

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 
END CLASS

CLASS @@Event //inherit object
	//RvdH 061218 Declared properties for performance
	EXPORT hWnd 	AS PTR
	EXPORT uMsg 	AS DWORD
	EXPORT wParam 	AS DWORD
	EXPORT lParam 	AS LONGINT
	EXPORT oWindow AS OBJECT 
	ACCESS Handle AS PTR STRICT 
	

	RETURN hWnd

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
	LOCAL oEvent AS @@Event
	

	//super:Init()
	IF IsPtr(_hWnd)
		hWnd := _hWnd
		uMsg := _uMsg
		wParam := _wParam
		lParam := _lParam
		oWindow := _oWindow
	ELSE
		oEvent := _hWnd

		hWnd := oEvent:hWnd
		uMsg := oEvent:uMsg
		wParam := oEvent:wParam
		lParam := oEvent:lParam
		oWindow := oEvent:oWindow
	ENDIF

	RETURN 

ACCESS Message AS DWORD STRICT 
	
	RETURN uMsg

ACCESS Window AS OBJECT STRICT 
	

	IF (oWindow == NULL_OBJECT)
		RETURN __WCGetObjectByHandle(hWnd)
	ENDIF
	RETURN oWindow


END CLASS

CLASS ExposeEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
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

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 
END CLASS

CLASS FocusChangeEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	ACCESS GotFocus AS LOGIC STRICT 
	
	RETURN uMsg == WM_SETFOCUS

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 
END CLASS

CLASS HelpRequestEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	ACCESS HelpContext AS STRING STRICT 
	LOCAL oHL AS HyperLabel

	

	oHL:=SELF:HyperLabel

	IF oHL!=NULL_OBJECT
		RETURN oHL:HelpContext
	ENDIF

	RETURN NULL_STRING

ACCESS @@HelpInfo AS PTR STRICT 
	//SE-060522
	

	IF uMsg = WM_HELP
		RETURN PTR(_CAST, lParam)
   ENDIF

	RETURN NULL_PTR

ACCESS HelpType AS DWORD STRICT 
	

	IF uMsg = WM_HELP
		RETURN HELPINFO
   ENDIF

	RETURN wParam

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
   			 IF IsInstanceOf(oObject, #Control)
   				 oHL := oObject:Hyperlabel
   				 IF oHL != NULL_OBJECT .AND. oHL:HelpContext == NULL_STRING
   				    IF IsInstanceOf(oObject, #TabControl)
   				       oObject := oObject:CurrentPage
   				    ELSE
                      oObject := oObject:Owner
   				    ENDIF
   				 ENDIF
   	       ENDIF
   			 IF IsInstanceOf(oObject,#Window)
   				 IF IsInstanceOf(oObject,#__DocApp) .OR.;
   					 IsInstanceOf(oObject,#__WndApp) .OR.;
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

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

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

ACCESS Position AS POINT STRICT 
	LOCAL strucPoint IS _WinPoint

	

	GetCursorPos(@strucPoint)
	ScreenToClient(oWindow:Handle(), @strucPoint)

	RETURN __WCConvertPoint(oWindow,Point{strucPoint:X,strucPoint:Y})

ACCESS WindowRegion() AS LONGINT STRICT 
	

	IF wParam == HelpWindow
		RETURN lParam
	ENDIF

	RETURN RegionUnknown

END CLASS

CLASS KeyEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
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

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS KeyCode AS DWORD STRICT 
	

	IF (uMsg != WM_CHAR)
		RETURN wParam
	ENDIF
	RETURN MapVirtualKey(HiWord(_AND(DWORD(_CAST, lParam), 0x00FF0000U)), 1)

ACCESS RepeatCount AS LONGINT STRICT 
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD	
	RETURN LoWord(dw)

ACCESS System AS LOGIC STRICT 
	
	RETURN _AND(lParam, SYSTEM_KEYCODE) != 0

END CLASS

CLASS ListViewColumnClickEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS ListViewColumn AS ListViewCOlumn STRICT 
	LOCAL strucListView AS _winNM_ListView
	LOCAL oListView		AS ListView
	oListView := OBJECT(SELF:Control)
	

	strucListView := PTR(_CAST, SELF:lParam)
	RETURN oListView:__GetColumnFromIndex(DWORD(strucListView:iSubItem + 1))

END CLASS

CLASS ListViewDeleteEvent INHERIT ListViewItemEvent

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 
END CLASS

CLASS ListViewDragEvent INHERIT ListViewItemEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS IsLeftButton AS LOGIC STRICT 
	

	IF SELF:NotifyCode == LVN_BEGINRDRAG
		RETURN FALSE
	ENDIF

	RETURN TRUE

ACCESS IsRightButton AS LOGIC STRICT 
	

	IF SELF:NotifyCode == LVN_BEGINRDRAG
		RETURN TRUE
	ENDIF

	RETURN FALSE

ACCESS Position AS POINT STRICT 
	LOCAL strucListView AS _winNM_ListView
	LOCAL oPoint AS Point

	

	strucListView := PTR(_CAST, SELF:lParam)
	oPoint := Point{strucListView:ptAction:x, strucListView:ptAction:y}

	RETURN __WCConvertPoint(SELF:Control, oPoint)

END CLASS

CLASS ListViewEditEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	ACCESS EditBeginning AS LOGIC STRICT 
	

	RETURN (SELF:NotifyCode == LVN_BEGINLABELEDIT)

ACCESS EditEnding AS LOGIC STRICT 
	

	RETURN (SELF:NotifyCode == LVN_ENDLABELEDIT)

ACCESS EditText AS STRING STRICT 
	LOCAL strucDispInfo AS _winLV_DispInfo

	

	strucDispInfo := PTR(_CAST, SELF:lParam)

	IF SELF:EditEnding
		RETURN Psz2String(strucDispInfo:item:pszText)
	ENDIF
	RETURN NULL_STRING

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS ListViewItem 
	LOCAL strucDispInfo AS _winLV_DispInfo
	LOCAL oListView		AS ListView
	oListView := OBJECT(SELF:Control)

	

	strucDispInfo := PTR(_CAST, SELF:lParam)
	RETURN oListView:GetItemAttributes(strucDispInfo:item:iItem + 1)

END CLASS

CLASS ListViewItemEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS ListViewItem AS ListViewItem STRICT 
	LOCAL oListView AS ListView
	LOCAL strucListView AS _winNM_ListView

	

	strucListView := PTR(_CAST, SELF:lParam)
	oListView := OBJECT(SELF:Control)

	RETURN oListView:GetItemAttributes(strucListView:iItem + 1)


END CLASS

CLASS ListViewKeyEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS KeyCode AS LONGINT STRICT 
	LOCAL strucKeyDown AS _winLV_KeyDown

	

	strucKeyDown := PTR(_CAST, SELF:LParam)
	RETURN strucKeyDown:wVKey

END CLASS

CLASS ListViewMouseEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	PROTECT nX AS INT
	PROTECT nY AS INT
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

CONSTRUCTOR(oControlNotifyEvent) 
	LOCAL strucPoint IS _winPoint

	

	SUPER(oControlNotifyEvent)

	GetCursorPos(@strucPoint)
	ScreenToClient(SELF:Control:Handle(), @strucPoint)
	nX := strucPoint:x
	nY := strucPoint:y

	RETURN 

ACCESS IsLeftButton AS LOGIC STRICT 
	//RvdH 061214 Added type (was untyped)
	LOCAL dwNotifyCode AS DWORD

	

	dwNotifyCode := SELF:NotifyCode
	IF dwNotifyCode == NM_CLICK .OR. dwNotifyCode == NM_DBLCLK
		RETURN TRUE
	ENDIF

	RETURN FALSE

ACCESS IsRightButton AS LOGIC STRICT 
	//RvdH 061214 Added type (was untyped)
	LOCAL dwNotifyCode AS DWORD

	

	dwNotifyCode := SELF:NotifyCode
	IF dwNotifyCode == NM_RCLICK .OR. dwNotifyCode == NM_RDBLCLK
		RETURN TRUE
	ENDIF

	RETURN FALSE

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

ACCESS PointAboveClientArea AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, LVHT_ABOVE) != 0

ACCESS PointBelowClientArea AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, LVHT_BELOW) != 0

ACCESS PointLeftOfClientArea AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, LVHT_TOLEFT) != 0

ACCESS PointNowhere AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, LVHT_NOWHERE) != 0

ACCESS PointOnItem AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, LVHT_ONITEM) != 0

ACCESS PointOnItemImage AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, LVHT_ONITEMICON) != 0

ACCESS PointOnItemLabel AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, LVHT_ONITEMLABEL) != 0

ACCESS PointOnItemStateImage AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, LVHT_ONITEMSTATEICON) != 0

ACCESS PointRightOfClientArea AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	ListView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, LVHT_TORIGHT) != 0

ACCESS Position AS POINT STRICT 

	

	RETURN __WCConvertPoint(SELF:Control, Point{nX, nY})

END CLASS

CLASS MenuCommandEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	PROTECT oMenu AS Menu

	METHOD __SetMenu(oParam AS OBJECT) AS MenuCommandEvent STRICT 
	//PP-030828 Strong typing
	LOCAL oParent AS Window
	LOCAL oObject AS OBJECT

	

	IF uMsg == WM_SYSCOMMAND .AND. IsInstanceOf(oWindow, #AppWindow)
		omenu := oWindow:__SysMenu
	ELSE
		// Was Menu Passed?
		IF IsInstanceOfUsual(oParam, #Menu)
			oMenu := oParam
			// Window was passed
		ELSEIF (oParam:ContextMenu != NULL_OBJECT) .AND. (oParam:ContextMenu:HyperLabel(SELF:ItemID) != NULL_OBJECT)
			oMenu := oParam:ContextMenu
		ELSE
			oMenu := oParam:Menu
			// PRAAN02: Make sure the window is not a control
			IF !IsInstanceOf(oParam, #Control)
				oParent := oParam
				DO WHILE (oMenu == NULL_OBJECT) .AND. IsInstanceOf(oObject := oParent:Owner, #Window)
					oParent := oObject
					IF (oParent:ContextMenu != NULL_OBJECT) .AND. (oParent:ContextMenu:HyperLabel(SELF:ItemID) != NULL_OBJECT)
						oMenu := oParent:ContextMenu
					ELSE
						oMenu := oParent:Menu
					ENDIF
				ENDDO
			ENDIF
		ENDIF
	ENDIF

	RETURN SELF

METHOD AsString() AS STRING STRICT 
	LOCAL pszBuffer AS PSZ
	LOCAL cString AS STRING
	LOCAL liLength AS LONGINT

	

	pszBuffer := MemAlloc(256)
	liLength := GetMenuString(SELF:Menu:Handle(), wParam, pszBuffer, 256, MF_BYCOMMAND)

	IF (liLength != 0)
		cString := Psz2String(pszBuffer)
	ENDIF

	MemFree(pszBuffer)

	RETURN cString

ACCESS HyperLabel AS HyperLabel STRICT 
	LOCAL oHL AS HyperLabel

	

	IF IsInstanceOf(oMenu, #Menu)
		oHL := oMenu:HyperLabel(SELF:ItemID)
	ENDIF

	RETURN oHL

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS ItemID AS LONGINT STRICT 
	

	RETURN LoWord(wParam)

ACCESS Menu AS MENU STRICT 
	

	RETURN oMenu

ACCESS Name AS STRING STRICT 
	LOCAL oHL AS HyperLabel

	

	oHL := SELF:HyperLabel

	IF IsInstanceOf(oHL,#HyperLabel)
		RETURN oHL:Name
	ENDIF

	RETURN NULL_STRING

ACCESS NameSym AS SYMBOL STRICT 
	LOCAL oHL AS HyperLabel

	

	oHL := SELF:HyperLabel
	IF IsInstanceOf(oHL,#HyperLabel)
		RETURN oHL:NameSym
	ENDIF

	RETURN NULL_SYMBOL

END CLASS

CLASS MenuInitEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
	

	SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)

	RETURN 

ACCESS Menu AS Menu STRICT 
	

	RETURN __WCGetMenuByHandle(PTR(_CAST, wParam))

END CLASS

CLASS MenuSelectEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
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

ACCESS HyperLabel AS HyperLabel STRICT 
	LOCAL oHyperLabel AS HyperLabel

	

	IF (SELF:Menu != NULL_OBJECT)
		oHyperLabel := SELF:Menu:HyperLabel(SELF:ItemID)
	ENDIF

	RETURN oHyperLabel

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
	

	SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)

	RETURN 

ACCESS ItemID AS LONGINT STRICT 
	
	RETURN LoWord(wParam)

ACCESS Menu AS Menu STRICT 
	LOCAL oMenu AS Menu

	oMenu := __WCGetMenuByHandle(PTR(_CAST, lParam))

	IF (oMenu == NULL_OBJECT) .AND. (_AND(HiWord(wParam), MF_SYSMENU) == 0) .AND. IsInstanceOf(oWindow, #AppWindow)
		oMenu := oWindow:__SysMenu
	ENDIF

	RETURN oMenu

ACCESS Name AS STRING STRICT 
	LOCAL retVal AS STRING

	

	IF SELF:HyperLabel != NULL_OBJECT
		retVal := SELF:HyperLabel:Name
	ENDIF

	RETURN retVal

ACCESS NameSym AS SYMBOL STRICT 
	LOCAL retVal AS SYMBOL

	

	IF SELF:HyperLabel != NULL_OBJECT
		retVal := SELF:HyperLabel:NameSym
	ENDIF

	RETURN retVal

END CLASS

CLASS MinMaxInfoEvent INHERIT @@Event                          
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS MaxPosition AS POINT STRICT 
	LOCAL sMinMax AS _WINMINMAXINFO

	sMinMax := PTR(_CAST, lParam)
	RETURN Point{sMinMax:ptMaxPosition:X, sMinMax:ptMaxPosition:Y}

ASSIGN MaxPosition(oPoint AS point)  STRICT 
	LOCAL oMaxPos  AS Point
	LOCAL sMinMax  AS _WINMINMAXINFO

	oMaxPos  := oPoint
	sMinMax  := PTR(_CAST, lParam)
	sMinMax:ptMaxPosition:X := oMaxPos:X
	sMinMax:ptMaxPosition:Y := oMaxPos:Y
	RETURN 

ACCESS MaxSize AS Dimension STRICT 
	LOCAL sMinMax AS _WINMINMAXINFO

	sMinMax := PTR(_CAST, lParam)
	RETURN Dimension{sMinMax:ptMaxSize:X, sMinMax:ptMaxSize:Y}

ASSIGN MaxSize(oSize AS Dimension)  STRICT 
	LOCAL oMaxSize AS Dimension
	LOCAL sMinMax  AS _WINMINMAXINFO

	oMaxSize := oSize
	sMinMax  := PTR(_CAST, lParam)
	sMinMax:ptMaxSize:X := oMaxSize:Width
	sMinMax:ptMaxSize:Y := oMaxSize:Height
	RETURN 

ACCESS MaxTrackSize AS Dimension STRICT 
	LOCAL sMinMax AS _WINMINMAXINFO

	sMinMax := PTR(_CAST, lParam)
	RETURN Dimension{sMinMax:ptMaxTrackSize:X, sMinMax:ptMaxTrackSize:Y}

ASSIGN MaxTrackSize(oSize AS Dimension)  STRICT 
	LOCAL oMaxSize AS Dimension
	LOCAL sMinMax  AS _WINMINMAXINFO

	oMaxSize := oSize
	sMinMax  := PTR(_CAST, lParam)
	sMinMax:ptMaxTrackSize:X := oMaxSize:Width
	sMinMax:ptMaxTrackSize:Y := oMaxSize:Height
	RETURN 

ACCESS MinTrackSize AS Dimension STRICT 
	LOCAL sMinMax AS _WINMINMAXINFO

	sMinMax := PTR(_CAST, lParam)
	RETURN Dimension{sMinMax:ptMinTrackSize:X, sMinMax:ptMinTrackSize:Y}

ASSIGN MinTrackSize(oSize AS Dimension)  STRICT 
	LOCAL oMinSize AS Dimension
	LOCAL sMinMax  AS _WINMINMAXINFO

	oMinSize := oSize
	sMinMax  := PTR(_CAST, lParam)
	sMinMax:ptMinTrackSize:X := oMinSize:Width
	sMinMax:ptMinTrackSize:Y := oMinSize:Height
	RETURN 

END CLASS

CLASS MonthCalSelectionEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	ACCESS Explicit AS LOGIC STRICT 
	RETURN (SELF:NotifyCode == MCN_SELECT)

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS Selection AS DateRange STRICT 
	LOCAL sc AS _winNMSELCHANGE

	sc := PTR(_CAST, lParam)
	RETURN DateRange{ConDate(sc:stSelStart:wYear, sc:stSelStart:wMonth, sc:stSelStart:wDay), ConDate(sc:stSelEnd:wYear, sc:stSelEnd:wMonth, sc:stSelEnd:wDay)}

END CLASS

CLASS MouseEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	ACCESS ButtonID AS LONGINT STRICT 
	LOCAL retVal AS LONGINT

	

	DO CASE
	CASE uMsg == WM_LBUTTONDOWN .OR.;
		uMsg == WM_LBUTTONUP .OR.;
		uMsg == WM_LBUTTONDBLCLK
		retVal := ButtonLeft
	CASE uMsg == WM_MBUTTONDOWN .OR.;
		uMsg == WM_MBUTTONUP .OR.;
		uMsg == WM_MBUTTONDBLCLK
		retVal := ButtonMiddle
	CASE uMsg == WM_RBUTTONDOWN .OR.;
		uMsg == WM_RBUTTONUP .OR.;
		uMsg == WM_RBUTTONDBLCLK
		retVal := ButtonRight
	CASE uMsg == WM_XBUTTONDOWN .OR.;
		uMsg == WM_XBUTTONUP .OR.;
		uMsg == WM_XBUTTONDBLCLK
		IF _AND(HiWord(wParam), XButton1) == XButton1
			retVal := ButtonX1
		ELSE
			retVal := ButtonX2
		ENDIF
	CASE uMsg == WM_MOUSEMOVE
		retVal := LONGINT(_CAST, _AND(wParam, 0x001FU))
	OTHERWISE
		retVal := 0
	ENDCASE

	RETURN retVal

ACCESS Height AS LONGINT STRICT 
	
	RETURN HiWord(DWORD(lParam))

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS IsControlButton AS LOGIC STRICT 
	
	RETURN _AND(wParam, ButtonControl) == ButtonControl

ACCESS IsLeftButton AS LOGIC STRICT 
	LOCAL retVal AS LOGIC

	

	DO CASE
	CASE uMsg == WM_LBUTTONUP .OR. uMsg == WM_LBUTTONDOWN .OR. uMsg == WM_LBUTTONDBLCLK
		retVal := TRUE
	CASE uMsg == WM_MOUSEMOVE
		retVal := _AND(wParam, ButtonLeft) == ButtonLeft
	OTHERWISE
		retVal := FALSE
	ENDCASE

	RETURN retVal

ACCESS IsMiddleButton AS LOGIC STRICT 
	LOCAL retVal AS LOGIC

	

	DO CASE
	CASE uMsg == WM_MBUTTONUP .OR. uMsg == WM_MBUTTONDOWN .OR. uMsg == WM_MBUTTONDBLCLK
		retVal := TRUE
	CASE uMsg == WM_MOUSEMOVE
		retVal := _AND(wParam, ButtonMiddle) == ButtonMiddle
	OTHERWISE
		retVal := FALSE
	ENDCASE

	RETURN retVal

ACCESS IsRightButton AS LOGIC STRICT 
	LOCAL retVal AS LOGIC

	

	DO CASE
	CASE uMsg == WM_RBUTTONUP .OR. uMsg == WM_RBUTTONDOWN .OR. uMsg == WM_RBUTTONDBLCLK
		retVal := TRUE
	CASE uMsg == WM_MOUSEMOVE
		retVal := _AND(wParam, ButtonRight) == ButtonRight
	OTHERWISE
		retVal := FALSE
	ENDCASE

	RETURN retVal

ACCESS IsShiftButton AS LOGIC STRICT 
	
	RETURN _AND(wParam, ButtonShift) == ButtonShift

ACCESS IsXButton1 AS LOGIC STRICT 
	//PP-030904
	LOCAL retVal AS LOGIC

	

	DO CASE
	CASE uMsg == WM_XBUTTONUP .OR. uMsg == WM_XBUTTONDOWN .OR. uMsg == WM_XBUTTONDBLCLK
		IF _AND(HiWord(wParam), XButton1) == XButton1
			retVal := TRUE
		ENDIF
	CASE uMsg == WM_MOUSEMOVE
		retVal := _AND(wParam, ButtonX1) == ButtonX1
	OTHERWISE
		retVal := FALSE
	ENDCASE

	RETURN retVal

ACCESS IsXButton2 AS LOGIC STRICT 
	//PP-030904
	LOCAL retVal AS LOGIC

	

	DO CASE
	CASE uMsg == WM_XBUTTONUP .OR. uMsg == WM_XBUTTONDOWN .OR. uMsg == WM_XBUTTONDBLCLK
		IF _AND(HiWord(wParam), XButton2) == XButton2
			retVal := TRUE
		ENDIF
	CASE uMsg == WM_MOUSEMOVE
		retVal := _AND(wParam, ButtonX2) == ButtonX2
	OTHERWISE
		retVal := FALSE
	ENDCASE

	RETURN retVal


ACCESS Position AS POINT STRICT 
   //SE-080520 
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD	
	// In 1.0 there was a problem if the window was a control.
	// This should not be the case in 2.0, but you never really know!
	RETURN __WCConvertPoint(oWindow, Point{SHORT(_CAST, LoWord(dw)), SHORT(_CAST, HiWord(dw))})

ACCESS Size AS Dimension STRICT 
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD	
	RETURN Dimension{LoWord(dw), HiWord(dw)}

ACCESS Width AS LONGINT STRICT 
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD	
	RETURN LoWord(dw)

END CLASS

CLASS MoveEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS Origin AS Point STRICT 
  //SE-080520
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD	
	RETURN __WCConvertPoint(oWindow, Point{SHORT(_CAST, LoWord(dw)), SHORT(_CAST, HiWord(dw))})


END CLASS

CLASS PrinterErrorEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	ACCESS ErrorType AS DWORD STRICT 
	
	RETURN wParam

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 
END CLASS

CLASS PrinterExposeEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	ACCESS ExposedArea AS OBJECT STRICT 
	

	RETURN oWindow

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS PageNo AS DWORD STRICT 
	

	RETURN wParam

END CLASS

CLASS ResizeEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	ACCESS Height AS LONGINT STRICT 
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD	
	RETURN HiWord(dw)

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS Size AS Dimension STRICT 
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD	
	RETURN Dimension{LoWord(dw), HiWord(dw)}

ACCESS Width AS LONGINT STRICT 
	LOCAL dw := DWORD(_CAST,lParam) AS DWORD	
	RETURN LoWord(dw)

END CLASS

CLASS RichEditProtectEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS Selection AS Selection STRICT 
	//PP-030910
	LOCAL strucENProtect AS _winENProtected

	

	strucENProtect := PTR(_CAST, lParam)

	RETURN Selection{strucENProtect:chrg:cpMin + 1, strucENProtect:chrg:cpMax + 1}


ACCESS SelectionRange AS Range STRICT 
	LOCAL strucENProtect AS _winENProtected

	

	strucENProtect := PTR(_CAST, lParam)

	RETURN Range{strucENProtect:chrg:cpMin + 1, strucENProtect:chrg:cpMax + 1}

END CLASS

CLASS RichEditSelectionEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS Selection AS Selection STRICT 
	//PP-030910
	LOCAL strucSelChange AS _winSelChange

	

	strucSelChange := PTR(_CAST, lParam)
	RETURN Selection{strucSelChange:chrg:cpMin + 1, strucSelChange:chrg:cpMax + 1}

ACCESS SelectionRange AS Range STRICT 
	LOCAL strucSelChange AS _winSelChange

	

	strucSelChange := PTR(_CAST, lParam)
	RETURN Range{strucSelChange:chrg:cpMin + 1, strucSelChange:chrg:cpMax + 1}

ACCESS SelectionType AS LONGINT STRICT 
	LOCAL strucSelChange AS _winSelChange

	

	strucSelChange := PTR(_CAST, lParam)
	RETURN strucSelChange:seltyp

END CLASS

CLASS ScrollEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS IsWindowScroll AS LOGIC STRICT 
	

	RETURN (lParam == 0)

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
	LOCAL fOk AS LOGIC
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
			fOk     := GetScrollInfo(hHandle, nSBType, @strucScrollInfo )
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
		fOk     := GetScrollInfo(hHandle, nSBType, @strucScrollInfo )
		nMin    := strucScrollInfo:nMin 
		nMax    := strucScrollInfo:nMax 
	ENDIF

	dwType := LoWord(wParam)
    //RvdH 081105 Sliders need to be handled differently
    IF lSlider
    	DO CASE
    	CASE (dwType == SB_ThumbTrack) .OR. (dwType == SB_ThumbPosition)
    		liRetVal := SHORT(_CAST, HiWord(wParam))  // Note that the high word is signed !
    	CASE (dwType == SB_LINEDOWN) // SB_LINERIGHT == SB_LINEDOWN
    		IF (liRetVal + nUnit + nPage) > nMax
    			liRetVal := nMax 
    		ELSE
    			liRetVal += nUnit
    		ENDIF
    	CASE (dwType == SB_LINEUP)  // SB_LINELEFT == SB_LINEUP
    		IF (liRetVal - nUnit) < nMin
    			liRetVal := nMin
    		ELSE
    			liRetVal -= nUnit
    		ENDIF
    	CASE (dwType == SB_PAGEDOWN)   // SB_PAGERIGHT == SB_PAGEDOWN
    		IF (liRetVal + nBlock + nPage) > nMax
    			liRetVal := nMax
    		ELSE
    			liRetVal += nBlock
    		ENDIF
    	CASE (dwType == SB_PAGEUP)    // SB_PAGELEFT == SB_PAGEUP
    		IF (liRetVal - nBlock) < nMin
    			liRetVal := nMin
    		ELSE
    			liRetVal -= nBlock
    		ENDIF
    	ENDCASE
    ELSE
    	DO CASE
    	//SE-070421
    	CASE (dwType == SB_THUMBPOSITION) .OR. (dwType == SB_THUMBTRACK)
    		liRetVal := strucScrollInfo:nTrackPos //nPos
    	//CASE (dwType == SB_ThumbTrack)
    		//liRetVal := strucScrollInfo.nTrackPos
    	CASE (dwType == SB_LINEDOWN)   // SB_LINERIGHT == SB_LINEDOWN
    		IF strucScrollInfo:nPage > 1L
    		   nMax -= LONG(_CAST,strucScrollInfo:nPage) - 1L
    		ENDIF
    		IF liRetVal + nUnit > nMax
    			liRetVal := nMax
    		ELSE
    			liRetVal += nUnit
    		ENDIF
    	CASE (dwType == SB_LINEUP)    // SB_LINELEFT == SB_LINEUP
    		IF (liRetVal - nUnit) < nMin
    			liRetVal := nMin
    		ELSE
    			liRetVal -= nUnit
    		ENDIF
    	CASE (dwType == SB_PAGEDOWN)   // SB_PAGERIGHT == SB_PAGEDOWN
    		IF strucScrollInfo:nPage > 1L
    		   nMax -= LONG(_CAST,strucScrollInfo:nPage) - 1L
    		ENDIF
    		IF liRetVal + nBlock > nMax
    			liRetVal := nMax
    		ELSE
    			liRetVal += nBlock
    		ENDIF
    	CASE (dwType == SB_PAGEUP)    // SB_PAGELEFT == SB_PAGEUP
    		IF (liRetVal - nBlock) < nMin
    			liRetVal := nMin
    		ELSE
    			liRetVal -= nBlock
    		ENDIF
    	ENDCASE
    ENDIF
	RETURN liRetVal

ACCESS ScrollBar AS Scrollbar STRICT 
	

	IF (lParam == 0) .AND. IsMethod(oWindow, #ENABLEVERTICALSCROLL)
		IF (uMsg == WM_HSCROLL)
			RETURN oWindow:EnableHorizontalScroll()
		ELSE
			RETURN oWindow:EnableVerticalScroll()
		ENDIF
	ENDIF

	RETURN (ScrollBar) __WCGetControlByHandle(PTR(_CAST,lParam))

ACCESS ScrollBarID AS LONGINT STRICT 
	LOCAL myScrollbar AS ScrollBar
	

	myScrollbar := SELF:ScrollBar
	IF myScrollbar!=NULL_OBJECT
		RETURN myScrollBar:ControlID
	ENDIF

	RETURN 0

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

CLASS SliderEvent INHERIT ScrollEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS IsWindowScroll AS LOGIC STRICT 
	
	RETURN FALSE

ACCESS Slider AS OBJECT STRICT 
	
	RETURN __WCGetControlByHandle(PTR(_CAST,lParam))

END CLASS

CLASS SpinnerEvent INHERIT @@Event
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

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

ACCESS OldValue AS LONGINT STRICT 
RETURN SELF:OldPosition

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

	DO CASE
	CASE (wLow == SB_LINEDOWN)
		IF (liRetVal + wUnit) > oRange:Max
			liRetVal := oRange:Max
		ELSE
			liRetVal += wUnit
		ENDIF

	CASE (wLow == SB_LINEUP)
		IF (liRetVal - wUnit) < oRange:Min
			liRetVal := oRange:Min
		ELSE
			liRetVal -= wUnit
		ENDIF
	CASE (wLow == SB_THUMBPOSITION)
	    liRetVal := SHORT(_CAST,wHigh) // Note that the high word is signed !
	CASE (wLow == SB_THUMBTRACK)
	    liRetVal := SHORT(_CAST,wHigh) // Note that the high word is signed !
	ENDCASE

	RETURN LONGINT(liRetVal)

ACCESS Spinner AS OBJECT STRICT 
	LOCAL oS AS OBJECT
	oS :=  __WCGetControlByHandle(PTR(_CAST, lParam))
	IF IsInstanceOf(oS, #Spinner)
	    RETURN oS
	ENDIF
	RETURN NULL_OBJECT 

ACCESS SpinnerID AS LONGINT STRICT 
	LOCAL oS AS Spinner
	oS := SELF:Spinner
	IF (oS != NULL_OBJECT)
		RETURN oS:ControlID
	ENDIF

	RETURN 0

ACCESS Type AS LONGINT STRICT 
	LOCAL wType AS WORD
	wType := LoWord(wParam)
	DO CASE
	CASE (wType == SB_LINEDOWN)
		RETURN UnitIncrement
	CASE (wType == SB_LINEUP)
		RETURN UnitDecrement
	CASE (wType == SB_TOP)
		RETURN ScrollToTopLeft
	CASE (wType == SB_BOTTOM)
		RETURN ScrollToBottomRight
	ENDCASE

	RETURN ScrollEnd

ACCESS Value AS LONGINT STRICT 

	

	RETURN SELF:Position

END CLASS

CLASS SysLinkSelectEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	ACCESS ID AS STRING STRICT 
	LOCAL DIM szUrl[MAX_LINKID_TEXT] AS BYTE
	LOCAL nml AS _winNMLink
	nml := PTR(_CAST, lParam)

	WideCharToMultiByte(CP_ACP, 0, @nml:item:szID[1], -1, PSZ(_CAST, @szUrl[1]), MAX_LINKID_TEXT, NULL_PTR, NULL_PTR)
	RETURN Psz2String(@szUrl[1])

CONSTRUCTOR(oControlNotifyEvent) 
	

	SUPER(oControlNotifyEvent)

	RETURN 

ACCESS LinkIndex AS LONGINT STRICT 
	LOCAL nml AS _winNMLink
	nml := PTR(_CAST, lParam)
	RETURN nml:item:iLink

ACCESS URL AS STRING STRICT 
	LOCAL DIM szUrl[L_MAX_URL_LENGTH] AS BYTE
	LOCAL nml AS _winNMLink
	nml := PTR(_CAST, lParam)

	WideCharToMultiByte(CP_ACP, 0, @nml:item:szUrl[1], -1, PSZ(_CAST, @szUrl[1]), L_MAX_URL_LENGTH, NULL_PTR, NULL_PTR)
	RETURN Psz2String(@szUrl[1])

END CLASS

CLASS TreeViewDeleteEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS TreeViewItem AS TreeViewItem STRICT 
	LOCAL strucTreeView AS _winNM_TreeView
	LOCAL oTreeView AS TreeView

	

	strucTreeView := PTR(_CAST, lParam)
	oTreeView 		:= OBJECT(SELF:Control)

	RETURN oTreeView:GetItemAttributes(strucTreeView:itemOld:hItem)

END CLASS

CLASS TreeViewDragEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS IsLeftButton AS LOGIC STRICT 

	

	RETURN (SELF:NotifyCode == TVN_BEGINDRAG)

ACCESS IsRightButton AS LOGIC STRICT  
	

	RETURN (SELF:NotifyCode == TVN_BEGINRDRAG)

ACCESS Position AS Point STRICT  
	LOCAL strucTreeView AS _winNM_TREEVIEW
	LOCAL oPoint AS Point

	

	strucTreeView := PTR(_CAST, SELF:lParam)

	oPoint := Point{strucTreeView:ptDrag:x, strucTreeView:ptDrag:y}
	oPoint := __WCConvertPoint(SELF:Control, oPoint)

	RETURN oPoint

ACCESS TreeViewItem AS TreeViewItem STRICT 
	LOCAL strucTreeView AS _winNM_TREEVIEW
	LOCAL oTreeView AS TreeView

	

	strucTreeView := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)

	RETURN oTreeView:GetItemAttributes(strucTreeView:itemNew:hItem)

END CLASS

CLASS TreeViewEditEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	ACCESS EditBeginning AS LOGIC STRICT 
	

	RETURN (SELF:NotifyCode == TVN_BEGINLABELEDIT)

ACCESS EditEnding AS LOGIC STRICT 
	

	RETURN (SELF:NotifyCode == TVN_ENDLABELEDIT)

ACCESS EditText  AS STRING STRICT 
	LOCAL strucDispInfo AS _winTV_DispInfo

	

	strucDispInfo := PTR(_CAST, SELF:lParam)

	IF SELF:EditEnding
		RETURN Psz2String(strucDispInfo:item:pszText)
	ENDIF
	RETURN NULL_STRING

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS TreeViewItem AS TreeViewItem STRICT 
	LOCAL strucDispInfo AS _winTV_DispInfo
	LOCAL oTreeView AS TreeView

	

	strucDispInfo := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)

	RETURN oTreeView:GetItemAttributes(strucDispInfo:item:hItem)

END CLASS

CLASS TreeViewExpandedEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	ACCESS Collapsed AS LOGIC STRICT 
	LOCAL strucTreeView AS _winNM_TreeView

	

	strucTreeView := PTR(_CAST, SELF:lParam)

	RETURN (strucTreeView:action == TVE_COLLAPSE)

ACCESS Expanded  AS LOGIC STRICT 
	LOCAL strucTreeView AS _winNM_TreeView

	

	strucTreeView := PTR(_CAST, SELF:lParam)

	RETURN (strucTreeView:action == TVE_EXPAND)

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS TreeViewItem AS TreeViewItem STRICT 
	LOCAL strucTreeView AS _winNM_TreeView
	LOCAL oTreeView AS TreeView

	

	strucTreeView := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)

	RETURN oTreeView:GetItemAttributes(strucTreeView:itemNew:hItem)

END CLASS

CLASS TreeViewExpandingEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
   CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS TreeViewItem AS TreeViewItem STRICT 
	LOCAL strucTreeView AS _winNM_TreeView
	LOCAL oTreeView AS TreeView

	

	strucTreeView := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)

	RETURN oTreeView:GetItemAttributes(strucTreeView:itemNew:hItem)

END CLASS

CLASS TreeViewKeyEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS KeyCode AS LONGINT STRICT 
	LOCAL strucKeyDown AS _winTV_KeyDown

	

	strucKeyDown := PTR(_CAST, SELF:LParam)
	RETURN strucKeyDown:wVKey

END CLASS

CLASS TreeViewMouseEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	PROTECT nX AS LONGINT
	PROTECT nY AS LONGINT
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

CONSTRUCTOR(oControlNotifyEvent) 
	LOCAL strucPoint IS _winPoint

	

	SUPER(oControlNotifyEvent)

	GetCursorPos(@strucPoint)
	ScreenToClient(SELF:Control:Handle(), @strucPoint)
	nX := strucPoint:x
	nY := strucPoint:y

	RETURN 

ACCESS IsLeftButton AS LOGIC STRICT 
	//RvdH 061214 Added type (was untyped)
	LOCAL dwNotifyCode  AS DWORD

	

	dwNotifyCode := SELF:NotifyCode
	IF (dwNotifyCode == NM_CLICK) .OR. (dwNotifyCode == NM_DBLCLK)
		RETURN TRUE
	ENDIF

	RETURN FALSE

ACCESS IsRightButton  AS LOGIC STRICT 
	//RvdH 061214 Added type (was untyped)
	LOCAL dwNotifyCode AS DWORD

	

	dwNotifyCode := SELF:NotifyCode

	RETURN ((dwNotifyCode == NM_RCLICK) .OR. (dwNotifyCode == NM_RDBLCLK))

ACCESS PointAboveClientArea  AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_ABOVE) != 0

ACCESS PointBelowClientArea  AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_BELOW) != 0

ACCESS PointLeftOfClientArea  AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_TOLEFT) != 0

ACCESS PointNowhere  AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_NOWHERE) != 0

ACCESS PointOnItem  AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEM) != 0

ACCESS PointOnItemButton  AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMBUTTON) != 0

ACCESS PointOnItemImage  AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMICON) != 0

ACCESS PointOnItemIndent  AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMINDENT) != 0

ACCESS PointOnItemLabel  AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMLABEL) != 0

ACCESS PointOnItemRight AS LOGIC STRICT  
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMRIGHT) != 0

ACCESS PointOnItemStateImage  AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_ONITEMSTATEICON) != 0

ACCESS PointRightOfClientArea  AS LOGIC STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(SELF:Control:Handle(), @strucHitTestInfo)

	RETURN _AND(strucHitTestInfo:flags, TVHT_TORIGHT) != 0

ACCESS Position AS Point STRICT 

	

	RETURN __WCConvertPoint(SELF:Control, Point{nX, nY})

ACCESS TreeViewItem AS TreeViewItem STRICT 
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo
	LOCAL oTreeView AS TreeView

	

	oTreeView := OBJECT(SELF:Control)
	strucHitTestInfo:pt:x := nX
	strucHitTestInfo:pt:y := nY
	TreeView_HitTest(oTreeView:Handle(), @strucHitTestInfo)

	RETURN oTreeView:GetItemAttributes(strucHitTestInfo:hItem)

END CLASS

CLASS TreeViewSelectionEvent INHERIT ControlNotifyEvent
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
    
    SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS KeyBoardAction AS LOGIC STRICT 
	LOCAL strucTreeView AS _winNM_TreeView

	

	strucTreeView := PTR(_CAST, SELF:lParam)

	RETURN (strucTreeView:action == TVC_BYKEYBOARD)

ACCESS MouseAction AS LOGIC STRICT 
	LOCAL strucTreeView AS _winNM_TreeView

	

	strucTreeView := PTR(_CAST, SELF:lParam)

	RETURN (strucTreeView:action == TVC_BYMOUSE)

ACCESS NewTreeViewItem AS TreeViewItem STRICT 
	LOCAL strucTreeView AS _winNM_TreeView
	LOCAL oTreeView AS TreeView

	

	strucTreeView := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)

	RETURN oTreeView:GetItemAttributes(strucTreeView:itemNew:hItem)

ACCESS OldTreeViewItem AS TreeViewItem STRICT 
	LOCAL strucTreeView AS _winNM_TreeView
	LOCAL oTreeView AS TreeView

	

	strucTreeView := PTR(_CAST, SELF:lParam)
	oTreeView := OBJECT(SELF:Control)

	RETURN oTreeView:GetItemAttributes(strucTreeView:itemOld:hItem)

ACCESS SelectionChanged  AS LOGIC STRICT 
	

	RETURN (SELF:NotifyCode == TVN_SELCHANGEDA)

ACCESS SelectionChanging  AS LOGIC STRICT 
	

	RETURN (SELF:NotifyCode == TVN_SELCHANGINGA)

ACCESS UnknownAction  AS LOGIC STRICT 
	LOCAL strucTreeView AS _winNM_TreeView

	

	strucTreeView := PTR(_CAST, SELF:lParam)
	RETURN (strucTreeView:action == TVC_UNKNOWN)

END CLASS

PROCEDURE __InitClassPointer _INIT3
RETURN
