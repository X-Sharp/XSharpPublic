#ifdef __VULCAN__
   #using System.Runtime.InteropServices
#endif

CLASS DialogWindow INHERIT Window
	//RvdH 060627 Changed Hidden Ivars to Protect 
	PROTECT oResourceID 	AS ResourceID
	PROTECT bModal 			AS LOGIC
	PROTECT nResult 			AS SHORTINT
	PROTECT lShown 			AS LOGIC
	PROTECT aRadioGroups 	AS ARRAY
	PROTECT lClipperKeys 	AS LOGIC
	EXPORT  __lpfnOldDlgProc AS PTR
	PROTECT oLastFocus AS Control
	
	#ifdef __VULCAN__
	   HIDDEN WCDragListDialogProcDelegate AS __WCDragListDialogProcDelegate 
	   HIDDEN WCDialogProcDelegate         AS __WCDialogProcDelegate
	#endif

METHOD __Close(oEvent AS @@event) AS VOID STRICT 
	//PP-030828 Strong typing
	
	
	SELF:Close(oEvent)
	
	SELF:Destroy()
	SELF:EventReturnValue := 1L
	
	RETURN
	

METHOD __SetupDataControl(oDC AS Control) AS DialogWindow STRICT 
	//PP-030828 Strong typing
	
	IF IsInstanceOfUsual(oDC, #RadioButtonGroup)
		AAdd(aRadioGroups, oDC)
	ENDIF
	RETURN SELF
	

METHOD __SubClassForDragList() AS PTR STRICT 
	//PP-030828 Strong typing
	IF (__lpfnOldDlgProc == NULL_PTR)
#ifdef __VULCAN__
      IF WCDragListDialogProcDelegate == NULL
         WCDragListDialogProcDelegate := __WCDragListDialogProcDelegate{ NULL, @__WCDragListDialogProc() }
      ENDIF   
		__lpfnOldDlgProc := PTR(_CAST, SetWindowLong(hWnd, GWL_WNDPROC, System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) WCDragListDialogProcDelegate ) ) )
#else
		__lpfnOldDlgProc := PTR(_CAST, SetWindowLong(hWnd, GWL_WNDPROC, LONGINT(_CAST, @__WCDragListDialogProc())))
#endif		
	ENDIF
	
	RETURN __lpfnOldDlgProc
	

METHOD Activate(oEvent) 
	
	
	WCAppSetDialogWindow(SELF:Handle())
	
	RETURN SUPER:Activate(oEvent)
	
METHOD Active() 
	RETURN lShown
	
METHOD ButtonClick(oControlEvent)  
	LOCAL oButton AS OBJECT
	LOCAL dwI, dwCount AS DWORD
	LOCAL oRBG AS RadioButtonGroup 
	LOCAL oCE := oControlEvent	AS ControlEvent
	
	
	oButton := oCE:Control
	IF IsInstanceOf(oButton, #Button)
		oButton:Modified := TRUE // assume its modified
		IF IsInstanceOf(oButton, #RadioButton)
			//SE-060526
			dwCount := ALen(aRadioGroups)
			FOR dwI := 1 UPTO dwCount
				oRBG := aRadioGroups[dwI]
				IF oRBG:__IsElement(OBJECT(_CAST,oButton))
					oRBG:__SetOn(OBJECT(_CAST,oButton))
					oButton:__Update()
					oButton := oRBG
					EXIT
				ENDIF
			NEXT  //Vulcan.NET-Transporter: dwI
		ENDIF
		oButton:__Update() // Update radio button group
	ENDIF
	RETURN 0
	

METHOD ChangeFont(New_Font, lRescale) 
	LOCAL oOldFont AS Font
	LOCAL hDCtmp AS PTR
	LOCAL hfont AS PTR
	LOCAL hOldFont AS PTR
	LOCAL lMenu AS LOGIC
	LOCAL liDBUnits AS LONGINT
	LOCAL liWidth AS LONGINT
	LOCAL liHeight AS LONGINT
	LOCAL p0 AS Point
	LOCAL p1 AS Point
	LOCAL p2 AS Point
	LOCAL p3 AS Point
	LOCAL wXChars AS DWORD
	LOCAL wYChars AS DWORD
//	LOCAL Xbase AS DWORD
//	LOCAL Ybase AS DWORD
	//LOCAL oDlgPoint AS Point
	LOCAL DlgRect IS _winRect
	LOCAL strucPoint IS _winPoint
	LOCAL TextMetric IS _winTextMetric
	LOCAL dialogInfo AS __WCDialog_VARS
	LOCAL oDlgDimesion AS Dimension
	//LOCAL X0 AS INT
	LOCAL Y0 AS INT
	//RvdH 090610 Make sure hWnd is valid or else all windows on the desktop will be changed by this...
	//            This happens for example if you do a ChangeFont after a call to Show() for a modal dialog..
	IF hWnd == NULL_PTR
	   RETURN NULL_OBJECT
	ENDIF
	
	IF !IsNil(New_Font) .AND. !IsInstanceOf(New_Font, #Font)
		WCError{#ChangeFont, #DialogWindow, __WCStypeError, New_Font,1}:Throw()
	ENDIF
	
	IF !IsNil(lRescale) .AND. !IsLogic(lRescale)
		WCError{#ChangeFont, #DialogWindow, __WCStypeError, lRescale,2}:Throw()
	ENDIF
	oOldFont := SELF:Font
	
	IF IsNil(lRescale) .OR. !lRescale
		SUPER:Font:= New_Font
		RETURN oOldFont
	ENDIF
	
	IF (hDC != NULL_PTR)
		DCfontNeeded := FALSE
		DCfontInUse := TRUE
		// self:__GetDC() // needed ????
	ENDIF
	
	dialogInfo := MemAlloc(_SIZEOF(__WCDialog_VARS))
	dialogInfo:hDlg := hWnd
	
	hDCtmp := GetDC(hWnd)
	
	IF (New_Font != NULL_OBJECT)
		New_Font:Create(FALSE, hDCtmp)
		dialogInfo:hFont := New_Font:Handle()
		hfont := New_Font:handle()
	ELSE
		hfont := GetStockObject(DEFAULT_GUI_FONT)
		IF (hfont == NULL_PTR)
			hfont := GetStockObject(SYSTEM_FONT)
		ENDIF
	ENDIF
	
	hOldFont := SelectObject(hDCtmp, hfont)
	GetTextMetrics(hDCtmp, @TextMetric)
	SelectObject(hDCtmp, hOldFont)
	ReleaseDC(hWnd, hDCtmp)
	dialogInfo:fontX := TextMetric:tmAveCharWidth
	dialogInfo:fontY := TextMetric:tmHeight
	liDBUnits := GetDialogBaseUnits()
	dialogInfo:Xbase := LoWord(DWORD(liDBUnits))
	dialogInfo:Ybase := HiWord(DWORD(liDBUnits))
//	Xbase := DWORD(TextMetric:tmAveCharWidth)
//	Ybase := DWORD(TextMetric:tmHeight)
	
#ifdef __VULCAN__
   LOCAL SetChildFontProcDelegate AS __SetChildFontProcDelegate
   SetChildFontProcDelegate := __SetChildFontProcDelegate{ NULL, @__SetChildFontProc() }
   EnumChildWindows( hWnd, System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) SetChildFontProcDelegate ), LONGINT(_CAST,dialogInfo) )
   GC.KeepAlive( SetChildFontProcDelegate )
#else	
	EnumChildWindows(hwnd, @__SetChildFontProc(), LONGINT(_CAST,dialogInfo))
#endif
	
	GetClientRect(hWnd, @DlgRect)
	liWidth := DlgRect:Right - DlgRect:Left
	liWidth := DlgRect:Right
	liHeight := DlgRect:Bottom - DlgRect:Top
	liHeight := DlgRect:Bottom
	liDBUnits := GetDialogBaseUnits()
	wXChars := DWORD(liWidth)/LoWord(DWORD(liDBUnits))
	wYChars := DWORD(liHeight)/HiWord(DWORD(liDBUnits))
	IF (wYChars == 0)
		wYchars := 1
	ENDIF
	// keep bottom left in screen co-ordinates
	p1 :=Point{DlgRect:Left,DlgRect:Top}
	strucPoint:X := p1:X
	strucPoint:Y := p1:Y
	ClientToScreen(hwnd, @strucPoint)
	p1:X := strucPoint:X
	p1:Y := strucPoint:Y
	
	p0 :=Point{DlgRect:Right, DlgRect:Bottom}
	strucPoint:X := p0:X
	strucPoint:Y := p0:Y
	ClientToScreen(hwnd, @strucPoint)
	p0:X := strucPoint:X
	p0:Y := strucPoint:Y
	//X0 := p1:X
	Y0 := P0:y
	
	// adjust width/height (client co-ordinates )
	DlgRect:Right := TextMetric:tmAveCharWidth * LONGINT(_CAST, wxchars)
	DlgRect:Bottom := TextMetric:tmHeight * LONGINT(_CAST, wychars)
	IF (GetMenu(hWnd)!=0)
		lMenu := TRUE
	ELSE
		lMenu := FALSE
	ENDIF
	
	AdjustWindowRect(@DlgRect, DWORD(_CAST, GetWindowLong(hWnd,GWL_STYLE)), lMenu)
	// convert to screen co-ordinates
	p2 := Point{DlgRect:Left, DlgRect:Top}
	strucPoint:X := p2:X
	strucPoint:Y := p2:Y
	ClientToScreen(hwnd, @strucPoint)
	p2:X := strucPoint:X
	p2:Y := strucPoint:Y
	
	p3 := Point{DlgRect:Right, DlgRect:Bottom}
	strucPoint:X := p3:X
	strucPoint:Y := p3:Y
	ClientToScreen(hwnd, @strucPoint)
	p3:X := strucPoint:X
	p3:Y := strucPoint:Y
	
	y0:= (Y0 - Abs(p3:Y - p2:Y))
	//oDlgPoint := Point{X0, Y0+10}
	oDlgDimesion := Dimension{Abs(p3:X-p2:X), Abs(p3:Y-p2:Y)}
	// WCMoveWindow(self, self:Origin /*oDlgPoint*/, oDlgDimesion, true)
	WCMoveWindow(SELF, SELF:Origin, oDlgDimesion, TRUE)
	// reshow controls hidden during rescaling
#ifdef __VULCAN__
   LOCAL ShowControlProcDelegate AS __ShowControlProcDelegate
   ShowControlProcDelegate := __ShowControlProcDelegate{ NULL, @__ShowControlProc() }
   EnumChildWindows( hWnd, System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) ShowControlProcDelegate ), LONGINT(_CAST,hWnd) )
   GC.KeepAlive( ShowControlProcDelegate )
#else	
	EnumChildWindows(hWnd, @__ShowControlProc(), LONGINT(_CAST,hWnd))
#endif	
	
	RETURN oOldFont
	

ACCESS ClipperKeys() 
	
	
	RETURN lClipperKeys
	

ASSIGN ClipperKeys(lNewValue) 
	
	
	lClipperKeys := lNewValue
	
	RETURN 
	

METHOD ControlFocusChange(oControlFocusChangeEvent) 
	//PP-040519 S.Ebert
	LOCAL oCFCE := oControlFocusChangeEvent AS ControlFocusChangeEvent
	
	IF oCFCE:GotFocus
		SELF:LastFocus := oCFCE:Control
		WCAppSetDialogWindow(SELF:Handle())
	ENDIF
	
	RETURN NIL

METHOD DeActivate(oEvent) 
	RETURN SUPER:Deactivate(oEvent)
	

METHOD Default(oEvent) 
	SELF:EventReturnValue := 0
	
	RETURN SELF
	

METHOD Destroy() 
	IF (WCAppGetDialogWindow() == SELF:Handle()) //FdW// 20061202
		WCAppSetDialogWindow(NULL_PTR)
	ENDIF //FdW// 20061202
	IF bModal
		SELF:ShowModal(FALSE)
	ENDIF
	
	SUPER:Destroy()
	
	RETURN NIL
	

METHOD EditChange(oControlEvent) 
	//404@TR001 new method
	/* // ??? why was this needed ????
	local oCurrentControl as object
	
	oCurrentControl := oControlEvent:Control
	if IsInstanceOfUsual(oCurrentControl, #ListBox)
	oCurrentControl : Modified := .T.
	endif
	*/
	
	RETURN SUPER:EditChange(oControlEvent)
	

METHOD EditFocusChange(oEditFocusChangeEvent) 
	LOCAL uRetCode AS USUAL
	LOCAL oEFCE AS EditFocusChangeEvent
	oEFCE := oEditFocusChangeEvent 
	uRetCode := SUPER:EditFocusChange(oEFCE)
	
	IF !oEFCE:GotFocus
		IF oEFCE:Control != NULL_OBJECT
			oEFCE:Control:__Update()
		ENDIF
	ENDIF
	
	RETURN uRetCode
	

METHOD EndDialog(iResult) 
	DEFAULT(@iResult, 0)
	SendMessage(SELF:handle(), WM_CLOSE, 0, 0)
	//SELF:Destroy()
	nResult := iResult
	
	IF IsInstanceOf(oParent, #Window)
		SetFocus(oParent:Handle())
	ENDIF
	
    //080820 return iResult in stead of NIL.
    RETURN iResult
	

METHOD ExecModal() 
	//PP-040101 S Ebert contribution
	
	oApp:Exec(EXECNORMAL, SELF)
	RETURN SELF
	

METHOD HelpRequest(oHelpRequestEvent) 
	LOCAL cHelpContext AS STRING
	LOCAL oHRE AS HelpRequestEvent
	
	
	IF IsInstanceOfUsual(oHelpRequestEvent, #HelpRequestEvent) ;
			.AND. SELF:HelpDisplay != NULL_OBJECT
		// .AND. self:CurrentView == #FormView ;
		oHRE := oHelpRequestEvent 		
		cHelpContext := oHRE:HelpContext
		IF NULL_STRING != cHelpContext
			SELF:HelpDisplay:Show(cHelpContext, oHRE:HelpInfo)
		ELSE
			SUPER:HelpRequest(oHRE)
		ENDIF
	ENDIF
	RETURN SELF
	

CONSTRUCTOR(oOwner, xResourceID, lModal) 
   LOCAL pszBuffer   AS PTR
   LOCAL hHandle     AS PTR
   LOCAL hRSCHandle  AS PTR
   LOCAL lIsChild    AS LOGIC
   LOCAL oOldOwner   AS OBJECT
   LOCAL oResId      AS ResourceID
   LOCAL pDialogProc AS PTR
	
	IF IsInstanceOfUsual(oOwner, #App)
		oOwner := NIL
	ENDIF
	
	IF !IsNil(oOwner) .AND. !IsInstanceOfUsual(oOwner, #Window) .AND. !IsInstanceOfUsual(oOwner, #ToolBar) .AND. !IsPtr(oOwner)
		WCError{#Init,#DialogWindow,__WCSTypeError,oOwner,1}:Throw()
	ENDIF
	
	// should (might) change (separate class for TabDialogs) depending on how we implement DataDialog
	IF !IsInstanceOf(SELF, #__DDImp) .AND. IsInstanceOfUsual(oOwner, #DataWindow) .AND. !IsInstanceOf(SELF, #FormDialogWindow)
		oOldOwner := oOwner
		oOwner := oOwner:__GetFormSurface()
	ENDIF
	
	SUPER(oOwner)
	
	IF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID) .OR. IsString(xResourceID)
		oResourceID := ResourceID{xResourceID}
	ELSEIF IsInstanceOfUsual(xResourceID, #ResourceID)
		oResourceID := xResourceID
	ENDIF
	
	DEFAULT(@lModal, TRUE)
   bModal := lModal
	
	IF !IsNil(oParent)
		IF IsInstanceOf(SELF, #__DDImp)
			IF IsInstanceOf(oParent:Owner, #Window)
				hHandle := oParent:Owner:Handle()
			ELSE
				hHandle := NULL_PTR
			ENDIF
		ELSE
			hHandle := oParent:Handle()
		ENDIF
	ENDIF
#ifdef __VULCAN__
   IF WCDialogProcDelegate == NULL
      WCDialogProcDelegate := __WCDialogProcDelegate{ NULL, @__WCDialogProc() }
   ENDIF	
	pDialogProc := System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate((System.Delegate) WCDialogProcDelegate )
#else	
	pDialogProc := @__WCDialogProc()
#endif	
	
    IF IsPtr(xResourceID)
        //SE-070906 Support of dialog creation from memory dialog templates 
        //xResourceID is a pointer to global memory which contains the memory dialog template.
        hRSCHandle := xResourceID 
        hWnd := CreateDialogIndirectParam(_GetInst(), hRSCHandle, hHandle, pDialogProc, LONGINT(_CAST,ptrSelfPtr))
    ELSE
        IF IsInstanceOfUsual(xResourceID, #ResourceID)
            oResId := xResourceID 
        ELSEIF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID) .OR. IsString(xResourceID)
            oResId := ResourceID{xResourceID} 
        ENDIF                   
        
        IF oResId != NULL_OBJECT
            hRSCHandle := oResId:Handle()
            
            // MessageBox(0, "CreateDialog, hinst : "+AsSTring(hRSCHAndle)+", add: "+AsString(oResourceID:Address()),"create", 0)
            hWnd := CreateDialogParam(hRSCHandle, PTR(_CAST, oResId:Address()), hHandle, pDialogProc, LONGINT(_CAST,ptrSelfPtr))
        ENDIF   
    ENDIF
    //     IF !IsNil(xResourceID)
    //         hRSCHandle := oResourceID:Handle()
    //     ELSE
    //         hRSCHandle := _GetInst()
    //     ENDIF
	
	
	// MessageBox(0, "CreateDialog, hinst : "+AsSTring(hRSCHAndle)+", add: "+AsString(oResourceID:Address()),"create", 0)
	
	IF !IsWindow(hwnd)
		WCError{#Init,#DialogWindow,__WCSCreateDlgFailed}:Throw()
	ENDIF
	
	lIsChild := (_AND(WS_CHILD, GetWindowLong(hwnd, GWL_STYLE)) != 0)
	IF !lIsChild .AND. (oOldOwner != NULL_OBJECT)
		oParent := oOldOwner
	ENDIF
	
    pszBuffer := MemAlloc(79)
    IF (GetWindowText(hWnd, pszBuffer,78) > 0)
        SELF:Caption := Psz2String(pszBuffer)
    ENDIF
    MemFree(pszBuffer)
	 SetWindowPos( hwnd, 0, 0,0,0,0,;
		_OR(SWP_NOSIZE,SWP_NOZORDER,SWP_NOMOVE,SWP_NOREPOSITION,SWP_NOACTIVATE,SWP_DRAWFRAME))
	
	aRadioGroups := {}
	   
    IF (SELF:oParent != NULL_OBJECT .AND. IsAccess(SELF:oParent, #HELPDISPLAY ))
        SELF:HelpDisplay := oParent:HelpDisplay
    ENDIF
	RETURN 
	

ACCESS IsModal 
	
	
	RETURN bModal
	

ACCESS LastFocus 
	//PP-040419 S.Ebert
	
	RETURN oLastFocus
	

ASSIGN LastFocus (oControl) 
	//PP-040419 S.Ebert
	
	
	IF _AND(GetWindowLong(hWnd, GWL_STYLE), WS_CHILD) =  WS_CHILD
		IF IsAssign(oParent, #LastFocus)
			IVarPut(oParent, #LastFocus, oControl)
		ENDIF
	ENDIF
	RETURN oLastFocus := oControl
	
	

METHOD ListBoxClick(oControlEvent) 
	LOCAL oListBox := NULL_OBJECT AS OBJECT
	LOCAL oCE AS ControlEvent
	oCE := oControlEvent
	oListBox := oCE:Control
	IF IsInstanceOf(oListBox, #ListBox)
		oListBox : Modified := TRUE // assume its modified
		oListBox:__Update()
	ENDIF
	RETURN SELF
	

METHOD ListBoxSelect(oControlEvent) 
	LOCAL oListBox := NULL_OBJECT AS OBJECT
	LOCAL oCE AS ControlEvent
	oCE := oControlEvent
	oListBox := oCE:Control
	IF IsInstanceOf(oListBox, #ListBox)
		oListBox:Modified := TRUE // assume its modified
		oListBox:__SetText(oListBox:CurrentItem)
		oListBox:__Update()
	ENDIF
	RETURN SELF
	

ACCESS Owner 
	IF IsNil(oParent)
		RETURN oApp
	ENDIF
	RETURN oParent
	

METHOD PostShowDialog() 
	RETURN NIL
	

ACCESS Result() 
	RETURN nResult
	

METHOD Show(kShowState) 
	SUPER:Show(kShowState)
	
	//if !IsInstanceOf(self, #DataDialog) .and. !IsInstanceOf(self, #__DDImp)
	// if !IsInstanceOf(self, #__DDImp)
	// oApp:SetDialogWindow(self:Handle())
	// endif
	
	IF bModal
		SELF:ShowModal(TRUE)
	ELSE
		SELF:PostShowDialog()
	ENDIF
	
    //080820 return nResult in stead of NIL.
	RETURN nResult
	

METHOD ShowModal(lActive) 
	// This class does not use DialogBoxParam() to create a modal dialog.
	// Instead it mimics modal behaviour with the ShowModal() method. This
	// method is called to turn it on and off.
	LOCAL hHandle AS PTR
	LOCAL oDlg AS OBJECT
	LOCAL rc IS _winRECT
	
	DEFAULT(@lActive, TRUE)
	
	//hHandle := GetFocus()
	hHandle := GetActiveWindow()
	IF (hHandle == 0) .OR. !(hHandle == hWnd .OR. IsChild(hWnd, hHandle))
		SetFocus(hWnd)
	ENDIF
	
	hHandle := hWnd
	IF (lActive)
		DO WHILE (hHandle := GetParent(hHandle)) != NULL_PTR
			GetClientRect(hHandle, @rc)
			IF !IsRectEmpty(@rc)
				EnableWindow(hHandle, FALSE)
			ENDIF
		ENDDO
	ELSE
		DO WHILE (hHandle := GetParent(hHandle)) != NULL_PTR
			EnableWindow(hHandle, TRUE)
			oDlg := __WCGetWindowByHandle(hHandle)
			IF (IsInstanceOf(oDlg, #DialogWindow) .AND. IVarGet(oDlg, #IsModal))
				EXIT
			ENDIF
		ENDDO
	ENDIF
	
	lShown := lActive
	
	IF lShown .AND. (oApp != NULL_OBJECT)
		oApp:Exec(EXECWHILEEVENT)
		SELF:PostShowDialog()
		//PP-040101 replaced oApp:Exec(...) with call to SELF:ExecModal()
		SELF:ExecModal()
	ENDIF
	RETURN SELF
	
	
END CLASS

_DLL FUNCTION CreateDialogParamW(hInstance AS PTR, lpTemplateName AS PSZ,;
		hWndParent AS PTR, lpDialogFunc AS PTR,;
		dwInitParam AS LONG) AS PTR PASCAL:USER32.CreateDialogParamW
	
#ifdef __VULCAN__
   DELEGATE __SetChildFontProcDelegate( hWnd AS PTR, lParam AS LONGINT ) AS LOGIC
#endif	
	
FUNCTION __SetChildFontProc (hWnd AS PTR, lParam AS LONGINT) AS LOGIC /* WINCALL */
	//LIUHO01@12/21/95: callBack function for ChangeFont Method
	LOCAL ptrBuf AS PTR
	LOCAL cBuf AS STRING
	LOCAL dv AS __WCDialog_VARS
	LOCAL CtrlRect1 IS _winRect
	LOCAL CtrlRect2 IS _winRect
	LOCAL CtrlRect IS _winRect
	LOCAL struPoint IS _winPoint
	LOCAL p0 AS Point
	LOCAL p1 AS Point
	LOCAL X0 AS INT
	LOCAL Y0 AS INT
	LOCAL newWidth AS INT
	LOCAL newHeight AS INT
	
	dv := PTR(_CAST, lParam)
	// Is this a direct child of the dialog
	IF (GetParent(hWnd) != dv:hDlg)
		RETURN TRUE
	ENDIF
	ptrBuf := MemAlloc(50)
	GetClassName(hWnd, ptrBuf, 50)
	cBuf:=Psz2String(ptrBuf)
	ShowWindow (hWnd, 0) // hide control
	
	IF (cBuf =="ComboBox")
		GetWindowRect(hWnd, @CtrlRect2)
		SendMessage(hWnd,CB_GETDROPPEDCONTROLRECT, 0, LONGINT(_CAST, @CtrlRect1))
		UnionRect(@CtrlRect, @CtrlRect1,@CtrlRect2)
	ELSE
		GetWindowRect(hWnd, @CtrlRect)
	ENDIF
	
	p1:= Point{CtrlRect:Left, CtrlRect:Top}
	struPoint:X := p1:X
	struPoint:y := p1:Y
	ScreenToClient(dv:hDlg, @struPoint)
	p1:X:= struPoint:X
	p1:Y:= struPoint:Y
	
	p0:= Point{CtrlRect:Right, CtrlRect:Bottom}
	struPoint:X := p0:X
	struPoint:y := p0:Y
	ScreenToClient(dv:hDlg, @struPoint)
	p0:X:= struPoint:X
	p0:Y:= struPoint:Y
	
	X0 := (p1:X * dv:fontX) / dv:Xbase
	Y0 := (p0:Y * dv:fontY) / dv:Ybase
	newWidth := (Abs(CtrlRect:Right - CtrlRect:Left) * (dv:fontX))/dv:Xbase
	newHeight := (Abs(CtrlRect:Bottom - CtrlRect:Top) * (dv:fontY))/dv:Ybase
	y0 := y0-newHeight
	
	SendMessage(hWnd, WM_SETFONT, DWORD(_CAST, dv:hFont), 1)
	SetWindowPos(hWnd,NULL, x0, Y0,newWidth, newHeight, _OR(SWP_NOZORDER , SWP_NOACTIVATE))
	MemFree(ptrBuf)
	RETURN TRUE // continue enumerating
#ifdef __VULCAN__
   INTERNAL DELEGATE __ShowControlProcDelegate( hWnd AS PTR, lParam AS LONGINT ) AS LOGIC
#endif
	
FUNCTION __ShowControlProc (hWnd AS PTR, lParam AS LONGINT) AS LOGIC /* WINCALL */
	IF GetParent(hWnd) == DWORD(_CAST,lParam)
		ShowWindow (hWnd, 1)
	ENDIF
	// Request callbacks until no more childwindows found
	RETURN TRUE
	
#ifdef __VULCAN__
   INTERNAL DELEGATE __WCDialogProcDelegate(hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LONGINT
#endif	
	
FUNCTION __WCDialogProc(hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LONGINT /* WINCALL */
	LOCAL lRetValue := 0L AS LONGINT
	LOCAL oDialogWindow AS DialogWindow
	
	IF uMsg == WM_INITDIALOG
		SetWindowLong(hWnd, DWL_User, lparam)
      oDialogWindow := __WcSelfptr2Object(PTR(_CAST, lParam))
      IF (oDialogWindow != NULL_OBJECT)
		   oDialogWindow:SetHandle(hWnd)
		   IF !IsInstanceOf(oDialogWindow, #__DDImp)
			   WCAppSetDialogWindow(hwnd)
		   ENDIF
      ENDIF
	ELSE
	   oDialogWindow := (DialogWindow) __WCGetWindowByHandle(hWnd)
		IF (oDialogWindow != NULL_OBJECT)
			lRetValue := oDialogWindow:Dispatch(@@Event{ hWnd, uMsg, wParam, lParam, oDialogWindow})
			// 2.5b this allows passing of EventReturnvalues
			SetWindowLong(hwnd, DWL_MSGRESULT, oDialogWindow:EventReturnValue)
		ENDIF
	ENDIF
	
	RETURN lRetValue

#ifdef __VULCAN__
   DELEGATE __WCDragListDialogProcDelegate( hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT ) AS LONGINT
#endif
	
	// used to subclass dialogs with draglist, in order to correctly return values
FUNCTION __WCDragListDialogProc(hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LONGINT /* WINCALL */
	LOCAL oDialogWindow AS DialogWindow
   oDialogWindow := (DialogWindow) __WcGetWindowByHandle(hWnd)
	IF (oDialogWindow != NULL_OBJECT) .AND. (oDialogWindow:__lpfnOldDlgProc != NULL_PTR)
		IF (uMsg == gdwDragListMsg)
			oDialogWindow:__HandleListItemDrag(@@Event{ hWnd, uMsg, wParam, lParam, oDialogWindow})
			RETURN oDialogWindow:EventReturnValue
		ELSE
			RETURN CallWindowProc(oDialogWindow:__lpfnOldDlgProc, hWnd, uMsg, wParam, lParam)
		ENDIF
	ENDIF
	
	RETURN DefWindowProc(hWnd, uMsg, wParam, lParam)
