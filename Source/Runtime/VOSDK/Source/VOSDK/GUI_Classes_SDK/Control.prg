#pragma options ("enforceself", on)
/// <include file="Gui.xml" path="doc/Control/*" />
CLASS Control INHERIT VObject
	PROTECT hWnd AS PTR
	PROTECT oParent AS OBJECT // PRAAN02@12/05/95: Changed to protect for ToolBar's benefit


	// The surface window that owns the control. This is the same as oParent except
	// when oParent is a data window. Then oFormSurface is the DataWindow's dialog
	PROTECT oFormSurface AS OBJECT
	PROTECT wId AS LONGINT   //RvdH 070205 Changed from WORD to LONG
	PROTECT lDataAware AS LOGIC
	PROTECT lIsDestroyed AS LOGIC
	PROTECT oContextMenu AS Menu


	PROTECT oSize AS Dimension
	PROTECT oOrigin AS Point
	// protect oSpinner as Spinner
	PROTECT cWindowName AS STRING
	PROTECT cClassName AS STRING
	PROTECT dwStyle AS DWORD
	PROTECT dwExStyle AS DWORD
	PROTECT cCaption AS STRING
	PROTECT uValue AS USUAL
	PROTECT oFieldSpec AS OBJECT
	PROTECT lChanged AS LOGIC
	EXPORT __lpfnDefaultProc AS PTR
	PROTECT __lModified AS LOGIC
	PROTECT __oHyperLabel AS HyperLabel
	PROTECT lExplicitHL AS LOGIC
	PROTECT oServer AS OBJECT
	PROTECT lBaseServer AS LOGIC
	PROTECT siDataField AS LONGINT      //RvdH 041129 Changed from SHORT to LONG
	PROTECT symDataField AS SYMBOL
	PROTECT oHlStatus AS HyperLabel
	// PROTECT oTempBrush as Brush
	PROTECT oControlBackground AS Brush
	// protect __oBackground as Brush
	PROTECT lExplicitFS AS LOGIC
	PROTECT oDataField AS DataField
	PROTECT uGetSetOwner AS USUAL
	PROTECT cbGetSetBlock AS CODEBLOCK
	//PROTECT lRetVal AS LOGIC //not used
	PROTECT dwTimerCount AS INT
	PROTECT dwTimerInterval AS INT
	PROTECT lTimerRegistered AS LOGIC
	//PROTECT oCurrentHelp AS HelpDisplay //not used
	PROTECT sToolTipText AS STRING
	PROTECT lUseHLForToolTip AS LOGIC
	//PROTECT hwndToolTip AS PTR //not used


	EXPORT EventReturnValue AS LONGINT
   EXPORT __ControlWindow AS ControlWindow


	//PP-030828 Strong typing
 /// <exclude />
	METHOD __AddTool(oControl AS Control) AS LOGIC STRICT
	//PP-030828 Strong typing
	RETURN oParent:__AddTool(oControl)




 /// <exclude />
ACCESS __ClassName AS STRING STRICT
	//PP-030828 Strong typing
	RETURN cClassName




 /// <exclude />
ASSIGN __ClassName(cNewName AS STRING)  STRICT
	//PP-030828 Strong typing
	RETURN cClassName:=cNewName




 /// <exclude />
ACCESS __DataField AS DataField STRICT
	//PP-030828 Strong typing
	RETURN oDataField




 /// <exclude />
METHOD __EnsureVisibity() AS LOGIC STRICT
	LOCAL rCtl, rParent IS _winRECT
	LOCAL oPoint AS Point
	LOCAL lVScrollPos, lHScrollPos AS LONGINT
	LOCAL iXCorrect AS INT
	LOCAL hFFrame AS PTR
	IF (oParent:__FormWindow == NULL_OBJECT)
		RETURN FALSE
	ENDIF


	hFFrame := oParent:__FormWindow:Handle()
	IF (hFFrame == NULL_PTR)
		RETURN FALSE
	ENDIF


	GetWindowRect(SELF:Handle(), @rCtl)
	GetClientRect(hFFrame, @rParent)
#ifdef __VULCAN__
	ClientToScreen(hFFrame, (_winPOINT PTR) @rParent)
#else
	ClientToScreen(hFFrame, @rParent)
#endif
	ClientToScreen(hFFrame, (_winPOINT PTR) @(rParent:right))


	IF (rCtl:left < rParent:left) .OR. (rCtl:right > rParent:right) .OR.;
			(rCtl:top < rParent:top) .OR. (rCtl:bottom > rParent:bottom)
		oPoint := oFormSurface:Origin


		lHScrollPos := GetScrollPos(hFFrame, SB_HORZ)
		lVScrollPos := GetScrollPos(hFFrame, SB_VERT)


		IF (rCtl:left < rParent:left)
			oPoint:X += (rParent:left - rCtl:left)
			SetScrollPos(hFFrame, SB_HORZ, lHScrollPos - (rParent:left - rCtl:left), TRUE)
		ELSEIF (rCtl:right > rParent:right)
			iXCorrect := Min((rCtl:right - rParent:right), (rCtl:left - rParent:left))
			oPoint:X -= iXCorrect
			SetScrollPos(hFFrame, SB_HORZ, lHScrollPos + iXCorrect, TRUE)
		ENDIF


		IF (rCtl:top < rParent:top)
			oPoint:Y -= (rParent:top - rCtl:top)
			SetScrollPos(hFFrame, SB_VERT, lVScrollPos - (rParent:top - rCtl:top), TRUE)
		ELSEIF (rCtl:bottom > rParent:bottom)
			oPoint:Y += (rCtl:bottom - rParent:bottom)
			SetScrollPos(hFFrame, SB_VERT, lVScrollPos + (rCtl:bottom - rParent:bottom), TRUE)
		ENDIF


		oFormSurface:Origin := oPoint
	ENDIF


	RETURN TRUE




 /// <exclude />
ACCESS __FormSurface AS OBJECT STRICT
	//PP-030828 Strong typing
	RETURN oFormSurface




 /// <exclude />
METHOD __Gather() AS LOGIC STRICT
	//PP-030828 Strong typing
	// <XXX> Assumes that Validation completed successfully and uValue contains
	// converted value.
	LOCAL lReturn:=TRUE AS LOGIC




	// Get value from control if necessary
	SELF:__Update()


	IF lChanged
		IF IsInstanceOfUsual(oServer, #DataServer)
			IF lBaseServer // if not subclassing
				oServer:FIELDPUT(siDataField,SELF:Value) //use FieldPut
			ELSEIF (symDataField != NULL_SYMBOL) //else use assigns
				IVarPut(oServer, symDataField ,SELF:Value)
			ELSE
				IVarPut(oServer, SELF:NameSym ,SELF:Value)
			ENDIF
			lReturn := (oServer:Status == NIL)
		ELSEIF cbGetSetBlock != NULL_CODEBLOCK
			IF (uGetSetOwner == NIL)
				Eval(cbGetSetBlock, SELF:Value)
			ELSE
				Eval(cbGetSetBlock, uGetSetOwner, SELF:Value)
			ENDIF
		ENDIF
	ENDIF
	RETURN lReturn




 /// <exclude />
ACCESS __GetDataFldPos AS LONGINT STRICT
	//PP-030828 Strong typing


	RETURN siDataField




 /// <exclude />
METHOD __GetDispInfo(oCtrlNotifyEvent AS ControlNotifyEvent) AS VOID STRICT
	//SE-060518
	//processes LVN_GETDISPINFO, TVN_GETDISPINFO, CBEN_GETDISPINFO, TBN_GETDISPINFO, HDN_GETDISPINFO
	//see Window:ControlNotify()
	RETURN


 /// <exclude />
ACCESS __Parent AS OBJECT STRICT
	//PP-030828 Strong typing


	RETURN oParent




 /// <exclude />
METHOD __Scatter() AS Control STRICT
	//PP-030828 Strong typing
	//
	// The __scatter method sends data to the control from the server
	//
	LOCAL newValue AS USUAL
	LOCAL cFSValType AS STRING


	IF IsInstanceOfUsual(oServer, #DataServer)
		IF lBaseServer // if not subclassing
			NewValue := oServer:FIELDGET(symDataField) //use fieldget
		ELSEIF symDataField != NULL_SYMBOL //else use Access
			NewValue := IVarGet(oServer, symDataField)
		ELSE
			NewValue := IVarGet(oServer, SELF:NameSym)
		ENDIF
		IF IsString(newValue)
			SELF:__Value:=RTrim(newValue)
		ELSE
			SELF:__Value:=newValue
		ENDIF
		oHLStatus := NULL_OBJECT
		SELF:ValueChanged := FALSE
	ELSEIF SELF:Value == NIL .AND. oFieldSpec IS FieldSpec VAR oFs
		cFSValType := oFs:ValType
		DO CASE
		CASE (cFSValType == "C") .OR. (cFSValType == "M")
			SELF:Value := NULL_STRING
		CASE cFSValType == "D"
			SELF:Value := NULL_DATE
		CASE cFSValType == "N"
			SELF:Value := 0
		CASE cFSValType == "L"
			SELF:Value := FALSE
		ENDCASE //438@AJP001 END
	ENDIF


	RETURN SELF




 /// <exclude />
METHOD __SetColors(_hDC AS PTR) AS PTR STRICT
	// 	//PP-030828 Strong typing
	//
	//
	// 	IF (oControlBackground != NULL_OBJECT)
	// 		RETURN oControlBackground:Handle()
	// 	ENDIF
	// 	RETURN NULL_PTR


	//SE-050804 from S Ebert
	//Correct ControlBackground for all Controls like FixedIcon for excample.
	//Without this change transparent icons don't support textured backgrounds.
	LOCAL hBr           AS PTR
	LOCAL strucLogBrush IS _WinLogBrush


	IF (oControlBackground != NULL_OBJECT)
		hbr := oControlBackground:Handle()
		GetObject(hBr, _SIZEOF(_WinLogBrush), @strucLogBrush)


		IF strucLogBrush:lbSTyle = BS_HOLLOW
			SetBkMode(_hdc, TRANSPARENT)
			IF IsThemeEnabled()
				DrawThemeParentBackground(hWnd, _hDC, NULL_PTR)
			ENDIF
		ELSEIF strucLogBrush:lbSTyle = BS_SOLID
			SetBkColor(_hDC, strucLogBrush:lbColor)
		ELSE
			IF ! (SELF IS Edit) .AND. ! (SELF IS BaseListBox)
				SetBkMode(_hdc, TRANSPARENT)
			ENDIF
			oControlBackground:__SetBrushOrg(_hdc, hwnd)
		ENDIF
	ELSE
		IF _AND(GetWindowLong(hWnd, GWL_EXSTYLE), WS_EX_TRANSPARENT)>0
			hbr := GetStockObject(HOLLOW_BRUSH)
			IF IsThemeEnabled()
				DrawThemeParentBackground(hWnd, _hDC, NULL_PTR)
			ELSE
				SetBkMode(_hdc, TRANSPARENT)
			ENDIF
		ENDIF
	ENDIF


	RETURN hBr


 /// <exclude />
METHOD __Timer() AS Control STRICT
	//PP-030828 Strong typing


	dwTimerCount := dwTimerCount - 1
	IF (dwTimerCount == 0)
		SELF:Timer()
		dwTimerCount:=dwTimerInterval
		IF (dwTimerCount == 0)
			__WCUnregisterTimer(SELF)
			lTimerRegistered:=FALSE
		ENDIF
	ENDIF


	RETURN SELF




 /// <exclude />
METHOD __ToolTipHandle() AS PTR STRICT
	//PP-030828 Strong typing
	RETURN oParent:__ToolTipHandle()


 /// <exclude />
METHOD __Unlink(oDataServer := NIL AS USUAL) AS Control STRICT
	//PP-030828 Strong typing
	// <XXX> - need to put in code to tidy up existing server and write out any
	// outstanding information here - iff control contains unvalidated changes


	IF SELF:Modified
		SELF:__Update()
		IF SELF:PerformValidations(uValue)
			SELF:__Gather()
		ENDIF
	ENDIF


	//PP-030909 Bug BF 021206. See datawin:__unlink()
	DEFAULT(@oDataServer,oServer)


	// Do actual unlinking
	IF IsNil(oDataServer)
		uGetSetOwner:= NIL
		cbGetSetBlock:= NULL_CODEBLOCK
		//PP-040410 next lines were incorrectly assigning NIL
		oDataField:= NULL_OBJECT
		oServer:= NULL_OBJECT
	ELSE
		IF !IsInstanceOfUsual(oDataServer,#DataServer)
			WCError{#__Unlink,#Control,__WCSTypeError,oDataServer,1}:Throw()
		ENDIF
		IF (oDataServer == oServer)
			uGetSetOwner:= NIL
			cbGetSetBlock:= NULL_CODEBLOCK
			//PP-040410 next lines were incorrectly assigning NIL
			oDataField := NULL_OBJECT
			oServer := NULL_OBJECT
		ENDIF
	ENDIF
	RETURN SELF


 /// <exclude />
METHOD __Update() AS Control STRICT
	//PP-030828 Strong typing
	LOCAL cText
	LOCAL uOldValue




	IF SELF:Modified
		cText := SELF:TextValue
		uOldValue := uValue
		IF oFieldSpec IS FieldSpec VAR oFs
			uValue := oFs:Val(cText)


			// If theres a picture clause we need to reformat the data at this point
			//RvdH 060608 optimized: Picture is a string
			//IF ((!IsNil(oFieldSpec:Picture)) .AND. (!Empty(oFieldSpec:Picture)))
			IF SLen(oFs:Picture) > 0
				SELF:TextValue := oFs:Transform(uValue)
			ENDIF
		ELSE
			uValue := cText
		ENDIF
		SELF:Modified := FALSE


		SELF:lChanged := !(uOldValue == uValue)
	ENDIF
	RETURN SELF


 /// <exclude />
ASSIGN __Value(uNewValue AS USUAL)  STRICT
	//PP-030828 Strong typing
	LOCAL uTemp AS USUAL


	IF uNewValue == NIL
		uValue := NIL
		SELF:TextValue := ""
		oHlStatus := NULL_OBJECT
		SELF:Modified := FALSE
		SELF:lChanged := TRUE
	ELSEIF IsInstanceOfUsual(oFieldSpec, #FieldSpec)
		uTemp := oFieldSpec:Transform(uNewValue)
		IF IsNil(uTemp)
			oHLStatus := oFieldSpec:Status
			WCError{#__Value,#Control,__WCSTypeError}:Throw()
		ELSE
			SELF:uValue := oFieldSpec:Val(uTemp)
			SELF:TextValue := uTemp
			oHLStatus := NULL_OBJECT
			SELF:Modified := FALSE
			SELF:lChanged := TRUE
		ENDIF
	ELSE
		uTemp := AsString(uNewValue)
		SELF:uValue := uNewValue
		SELF:TextValue := uTemp
		oHLStatus := NULL_OBJECT
		SELF:Modified := FALSE
		SELF:lChanged := TRUE
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/Control.Activate/*" />
METHOD Activate(oEvent)


	RETURN NIL


/// <include file="Gui.xml" path="doc/Control.AsString/*" />
METHOD AsString()




	IF !IsNil(SELF:uvalue)
		RETURN AsString(SELF:uvalue)
	ENDIF
	RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption


/// <include file="Gui.xml" path="doc/Control.Background/*" />
ACCESS Background


	RETURN oControlBackground


/// <include file="Gui.xml" path="doc/Control.Background/*" />
ASSIGN Background(oNewBrush)


	IF !IsNil(oNewBrush)
		IF !IsInstanceOfUsual(oNewBrush,#Brush)
			WCError{#Background,#Control,__WCSTypeError,oNewBrush,1}:Throw()
		ENDIF
	ENDIF


	oControlBackground := oNewBrush


	//2.5c otherwise Desktop invalidation!
	IF (hWnd != NULL_PTR)
		InvalidateRect(hWnd, NULL, TRUE)
	ENDIF
	// UpdateWindow(hWnd)


	RETURN






/// <include file="Gui.xml" path="doc/Control.Caption/*" />
ACCESS Caption
	RETURN cCaption




/// <include file="Gui.xml" path="doc/Control.Caption/*" />
ASSIGN Caption(cstring)
	cCaption := cstring


	RETURN




/// <include file="Gui.xml" path="doc/Control.ContextMenu/*" />
ACCESS ContextMenu
	RETURN oContextMenu




/// <include file="Gui.xml" path="doc/Control.ContextMenu/*" />
ASSIGN ContextMenu(oNewMenu)
	RETURN (oContextMenu := oNewMenu)




/// <include file="Gui.xml" path="doc/Control.ControlID/*" />
ACCESS ControlID
	//RvdH 070326 make sure we return a WORD like in the old classes
	//RETURN SELF:wId
	RETURN LoWord(DWORD(_CAST,SELF:wId))




/// <include file="Gui.xml" path="doc/Control.Create/*" />
METHOD Create()
	LOCAL oDevPoint AS Point
	LOCAL hInst AS PTR
	LOCAL hDefFont AS PTR
	LOCAL hOwner AS PTR




	// Sabo 01/15/1996


	// don't create twice
	IF (hwnd != NULL) .AND. IsWindow(hWnd)
		RETURN hwnd
	ENDIF


	IF !lIsDestroyed
		//if(cClassName == "_VOOLEContainer") .or. (cClassName == "_VOBmpContainer") // ?? needed ??
		// hInst := 0x400000
		//else
		hInst := _GetInst()
		//endif


		IF (hWnd == NULL_PTR) .AND. (oOrigin != NULL_OBJECT) .AND. (oSize != NULL_OBJECT)
			oDevPoint := Point{oOrigin:X, oOrigin:Y}
			IF WCGetCoordinateSystem() == WCCartesianCoordinates
				oDevPoint:Y := oDevPoint:Y + SELF:Size:Height
			ENDIF
			oDevPoint := __WCConvertPoint(oFormSurface, oDevPoint)
			hOwner := oFormSurface:Handle()
			IF oFormSurface IS DataBrowser VAR oBrow
				hOwner := oBrow:__ContainerChildWnd
			ENDIF


			hWnd:=CreateWindowEx(dwExStyle, String2Psz(cClassName), String2Psz(cWindowName), dwStyle,;
				oDevPoint:x, oDevPoint:y, oSize:Width, oSize:Height,;
				hOwner, wID, hInst, NULL_PTR)
			// 2.5b e.g. ComboBoxEx has Unicode set if created on the fly
			IF (hwnd != NULL_PTR)
				SendMessage(hwnd, CCM_SETUNICODEFORMAT, 0, 0)
			ENDIF
		ENDIF


		// Sabo 01/15/1996
		IF (hWnd != NULL_PTR)
			hDefFont := GetStockObject(DEFAULT_GUI_FONT)
			IF (hDefFont != NULL_PTR)
				SendMessage(hWnd, WM_SETFONT, DWORD(_CAST, hDefFont), MAKELPARAM(1, 0))
			ENDIF


			__lpfnDefaultProc := PTR(_CAST, GetWindowLong(hWnd, GWL_WNDPROC))
			SetWindowLong(hWnd, GWL_WNDPROC, LONGINT(_CAST, Get__WCControlProcPtr()))


			oSize := NULL_OBJECT
			oOrigin := NULL_OBJECT
			__WCRegisterControl(SELF) //register after we get the handle
		ELSE
			// !!! Maybe we should remove this before shipping !!!
			WCError{#Create,#Control,__WCSCreateCtlFailed,SELF}:Throw()
		ENDIF
	ENDIF


	RETURN hWnd


/// <include file="Gui.xml" path="doc/Control.DataField/*" />
ACCESS DataField
	// DHer: 18/12/2008
RETURN SELF:symDataField


/// <include file="Gui.xml" path="doc/Control.Deactivate/*" />
METHOD Deactivate(oEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.Default/*" />
METHOD DEFAULT(oEvent)
	LOCAL oEvt := oEvent AS @@event
	SELF:EventReturnValue := CallWindowProc(SELF:__lpfnDefaultProc, hWnd, oEvt:umsg, oEvt:wParam, oEvt:lParam)
	RETURN 1l




/// <include file="Gui.xml" path="doc/Control.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




	//SE-070501
	//__WCUnregisterMenu(oContextMenu)




	IF (hWnd != NULL_PTR)
		IF !(SELF IS WindowScrollBar)
			IF lTimerRegistered
				__WCUnregisterTimer(SELF)
			ENDIF
            IF lDataAware .AND. oParent IS DataWindow VAR oData
				oData:__UnregisterDataControl( SELF)
			ENDIF
			__WCUnregisterControl(hWnd)
			IF (__lpfnDefaultProc != NULL_PTR)
				SetWindowLong(hWnd, GWL_WNDPROC, LONGINT(_CAST, __lpfnDefaultProc))
			ENDIF
			IF IsWindow(hWnd)
				DestroyWindow(hWnd)
			ENDIF
		ENDIF


		IF !InCollect()
			__lpfnDefaultProc := NULL_PTR
			hWnd := NULL_PTR
			oContextMenu := NULL_OBJECT
			UnregisterAxit(SELF)
			lTimerRegistered:=FALSE
		ENDIF
	ENDIF




	SUPER:Destroy()


	lIsDestroyed := TRUE


	RETURN SELF




/// <include file="Gui.xml" path="doc/Control.Disable/*" />
METHOD Disable()




	IF SELF:ValidateControl()
		EnableWindow(SELF:Handle(),FALSE)
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/Control.DisableTheme/*" />
METHOD DisableTheme()
#ifdef __VULCAN
	//PP-030910
	RETURN SetWindowTheme(SELF:handle(),"","")
#else
	//DHer-110622 : VO doesn't handle String2W("") correctly in SetWindowTheme()
	RETURN SetWindowTheme(SELF:handle()," "," ")
#endif




/// <include file="Gui.xml" path="doc/Control.Dispatch/*" />
METHOD Dispatch(oEvent)
	LOCAL oEvt := oEvent AS @@event
	LOCAL msg AS DWORD
	LOCAL struMsg IS _winMSG
	LOCAL uRet AS USUAL


	// local hPar as ptr


	// (return value != 0) => message was handled internally
	// => DON'T call default handler
	// => return EventReturnValue to caller




	SELF:EventReturnValue := 0L
	msg := oEvt:uMsg


	SWITCH msg
	CASE WM_ERASEBKGND
		IF IsMethod(SELF, #PaintBackground)
			IF Send(SELF, #PaintBackground, PTR(_CAST, oEvt:wParam))
				SELF:EventReturnValue := 1L
				RETURN 1l
			ENDIF
		ENDIF


	CASE WM_PAINT
		//uRet := SELF:Expose(__ObjectCastClassPtr(oEvt, __pCExposeEvent))
		uRet := SELF:Expose(ExposeEvent{oEvt})


	CASE WM_DRAWITEM
		//PP-031006 owner draw support, thanks to SEbert
		//Used for owner drawn menus or controls
		RETURN __Dispatch_DrawItem(oEvt, SELF)


	CASE WM_MEASUREITEM
		//PP-031006 owner draw support
		//Used for owner drawn menus or controls
		RETURN __Dispatch_MeasureItem(oEvt, SELF)


	CASE WM_MENUCHAR
		//PP-031006 owner draw support
		//Used for owner drawn menus or controls
		RETURN __Dispatch_MenuChar(oEvt, SELF)
		/*
		CASE msg == WM_NOTIFY
		//SE-060523 message forwarding
		IF oFormSurface != Null_Object
		oEvt:hWnd    := oFormSurface:Handle()
		oEvt:oWindow := oFormSurface
		oFormSurface:Dispatch(oEvt)
		uRet := oFormSurface:EventReturnValue
		ENDIF
		*/
	CASE WM_WCHELP
		//oParent:HelpRequest(__ObjectCastClassPtr(oEvt, __pCHelpRequestEvent))
		oParent:HelpRequest(HelpRequestEvent{oEvt})


	CASE WM_ACTIVATE
		IF LoWord(oEvt:wParam) != 0
			uRet := SELF:Activate(oEvt)
		ELSE
			uRet := SELF:Deactivate(oEvt)
		ENDIF


		// WM_COMMAND trigged by (popup) menu
	CASE WM_COMMAND
        IF (HiWord(oEvt:wParam) == 0) .AND. (oEvt:lParam == 0L)
		    IF (oContextMenu != NULL_OBJECT)
			    oParent:__PreMenuCommand(MenuCommandEvent{oEvt}:__SetMenu(oContextMenu))
			    RETURN 1L
		    ENDIF
        ENDIF
		//PP-040421 Improved focus handling
	CASE WM_SETFOCUS
    CASE WM_KILLFOCUS
		//uRet := SELF:FocusChange(__ObjectCastClassPtr(oEvt, __pCFocusChangeEvent))
		uRet := SELF:FocusChange(FocusChangeEvent{oEvt})
		/*
		CASE msg == WM_SETFOCUS
		IF IsInstanceOf(oParent, #DataWindow) .and. IVarGet(oParent, #AutoScroll)
		SELF:__EnsureVisibity()
		ENDIF
		// hPar := GetParent(hwnd)
		// this is done in FocusChange anyway
		// if IsInstanceOf(__WCGetWindowByHandle(hPar), #DialogWindow)
		// oApp:SetDialogWindow(hPar)
		// endif
		SELF:FocusChange(__ObjectCastClassPtr(oEvt, __pCFocusChangeEvent))


		CASE msg == WM_KILLFOCUS
		IF IsInstanceOf(oParent,#DataWindow)
		oParent:__DoValidate(SELF)
		ENDIF
		SELF:FocusChange(__ObjectCastClassPtr(oEvt, __pCFocusChangeEvent))
		*/
	CASE WM_HSCROLL
		//uRet := SELF:HorizontalScroll(__ObjectCastClassPtr(oEvt, __pCScrollEvent))
		uRet := SELF:HorizontalScroll(ScrollEvent{oEvt})


	CASE WM_KEYUP
		//uRet := SELF:KeyUp(__ObjectCastClassPtr(oEvt, __pCKeyEvent))
		uRet := SELF:KeyUp(KeyEvent{oEvt})
		//PP-031221 WMoore SLE/Button issue - don't return 1L
		// RETURN (SELF:EventReturnValue := 1L)


	CASE WM_KEYDOWN
		//uRet := SELF:KeyDown(__ObjectCastClassPtr(oEvt, __pCKeyEvent))
		uRet := SELF:KeyDown(KeyEvent{oEvt})




	CASE WM_LBUTTONDBLCLK
    CASE WM_RBUTTONDBLCLK
    CASE WM_MBUTTONDBLCLK
    CASE WM_XBUTTONDBLCLK
		//PP-030904 Xbutton
		//uRet := SELF:MouseButtonDoubleClick(__ObjectCastClassPtr(oEvt, __pCMouseEvent))
		uRet := SELF:MouseButtonDoubleClick(MouseEvent{oEvt})


	CASE WM_LBUTTONDOWN
	CASE WM_RBUTTONDOWN
	CASE WM_MBUTTONDOWN
	CASE WM_XBUTTONDOWN
		//PP-030904 Xbutton
		//uRet := SELF:MouseButtonDown(__ObjectCastClassPtr(oEvt, __pCMouseEvent))
		uRet := SELF:MouseButtonDown(MouseEvent{oEvt})


	CASE WM_LBUTTONUP
	CASE WM_RBUTTONUP
	CASE WM_MBUTTONUP
	CASE WM_XBUTTONUP
		//PP-030904 Xbutton
		//uRet := SELF:MouseButtonUp(__ObjectCastClassPtr(oEvt, __pCMouseEvent))
		uRet := SELF:MouseButtonUp(MouseEvent{oEvt})


	CASE WM_VSCROLL
		//uRet := SELF:VerticalScroll(__ObjectCastClassPtr(oEvt, __pCScrollEvent))
		uRet := SELF:VerticalScroll(ScrollEvent{oEvt})




	CASE WM_MOUSEMOVE
		IF IsMethod(oFormSurface, #__ToolTipHandle) .AND. (oFormSurface:__ToolTipHandle() != NULL_PTR)
			struMsg:hwnd := hwnd
			struMsg:message := WM_MOUSEMOVE
			struMsg:wParam := oEvt:wParam
			struMsg:lParam := oEvt:lParam
			GetCursorPos(@struMsg:pt)
			struMsg:time := DWORD(GetMessageTime())
			SendMessage(oFormSurface:__ToolTipHandle(), TTM_RELAYEVENT, 0, LONGINT(_CAST, @struMsg))
		ENDIF


		IF oEvt:wParam != 0
			//uRet := SELF:MouseDrag(__ObjectCastClassPtr(oEvt, __pCMouseEvent))
			uRet := SELF:MouseDrag(MouseEvent{oEvt})
		ELSE
			//uRet := SELF:MouseMove(__ObjectCastClassPtr(oEvt, __pCMouseEvent))
			uRet := SELF:MouseMove(MouseEvent{oEvt})
		ENDIF


	CASE WM_MOVE
		uRet := SELF:Move(MoveEvent{oEvt})


	CASE WM_SIZE
		uRet := SELF:Resize(ResizeEvent{oEvt})


	CASE WM_DROPFILES
		IF IsMethod(oParent, #Drop)
			oParent:Drop(DragEvent{oEvt, SELF})
		ENDIF
		IF __LoadShellDll()
			DragFinish( PTR(_CAST, oEvt:wParam))
		ENDIF
		RETURN 1L


	CASE WM_INITMENU
    CASE WM_INITMENUPOPUP
		uRet := SendMessage(oParent:Handle(), msg, oEvt:wParam, oEvt:lParam)
		//if IsMethod(oParent, #MenuInit)
		//oParent:MenuInit(__ObjectCastClassPtr(oEvt, __pCMenuInitEvent))
		//endif


	CASE WM_MENUSELECT
		uRet := SendMessage(oParent:Handle(), msg, oEvt:wParam, oEvt:lParam)
		//if (oEvent:lParam != 0) .and. IsMethod(oParent, #MenuSelect)
		//oParent:MenuSelect(__ObjectCastClassPtr(oEvt, __pCMenuSelectEvent))
		//endif


		//RvdH 050809 Removed next line
		//CASE msg == WM_CLOSE
		// __WCUnregisterControl(self)


		//PP-040425 improved context menu support, following code replaced with subsequent code
		// 	CASE msg == WM_CONTEXTMENU
		// 		IF (oContextMenu != NULL_OBJECT)
		// 			RETURN (SELF:EventReturnValue := 1L)
		// 		ENDIF


		//PP-040410 improved context menu support
	CASE WM_CONTEXTMENU
		IF (oContextMenu != NULL_OBJECT)
			oContextMenu:ShowAsPopup(SELF, oEvt:lParam)
			RETURN (SELF:EventReturnValue := 1L)
		ENDIF


	CASE WM_THEMECHANGED //SE-060526
		VerifyThemeState()
		InvalidateRect(hWnd, NULL_PTR, TRUE)


	END SWITCH


	IF IsLong(uRet)
		SELF:EventReturnValue := uRet
		RETURN 1l
	ENDIF


	RETURN SELF:EventReturnValue




/// <include file="Gui.xml" path="doc/Control.Drop/*" />
METHOD Drop(oDragEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.Enable/*" />
METHOD Enable()


	IF SELF:ValidateControl()
		EnableWindow(SELF:Handle(),TRUE)
	ENDIF
	RETURN SELF




/// <include file="Gui.xml" path="doc/Control.Expose/*" />
METHOD Expose(oExposeEvent)


	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.FieldSpec/*" />
ACCESS FieldSpec


	RETURN oFieldSpec




/// <include file="Gui.xml" path="doc/Control.FieldSpec/*" />
ASSIGN FieldSpec(oDSAssign)
	// Assign method for FieldSpec
	//
	// This may change existing formatting or validation so an
	// update may be required if the data control is already linked to a server
	//


	IF !IsInstanceOfUsual(oDSAssign,#FieldSpec)
		WCError{#FieldSpec,#Control,__WCSTypeError,oDSAssign,1}:Throw()
	ENDIF


	oFieldSpec := oDSAssign
	lExplicitFS := TRUE


	// We need to update the control if is visible and connected to the server
	IF oServer != NIL .AND. oDataField!=NIL .OR. cbGetSetBlock != NULL_CODEBLOCK
		IF SELF:IsVisible()
			// If its modified update it to server
			// update it from server
			SELF:__Scatter()
		ENDIF
	ENDIF
	RETURN




/// <include file="Gui.xml" path="doc/Control.FocusChange/*" />
METHOD FocusChange(oFocusChangeEvent)
	//PP-040518 Update from S Ebert






	IF oParent IS DataWindow VAR oData
		RETURN oData:ControlFocusChange(ControlFocusChangeEvent{oFocusChangeEvent})
	ELSEIF oParent IS DialogWindow VAR oDlg
		RETURN oDlg:ControlFocusChange(ControlFocusChangeEvent{oFocusChangeEvent})
    ELSEIF oParent IS __FormFrame VAR oFF //It's a browser of a DataWindow
        LOCAL oFCE := oFocusChangeEvent AS FocusChangeEvent
		IF oFCE:GotFocus
			oFF:DataWindow:LastFocus := SELF
		ENDIF
	ENDIF


	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.GetExStyle/*" />
METHOD GetExStyle() AS LONG
	// Retrieves the complete window ex-styles bitmask for a control
	RETURN GetWindowLong( SELF:Handle(), GWL_EXSTYLE )




/// <include file="Gui.xml" path="doc/Control.GetStyle/*" />
METHOD GetStyle() AS LONG
	// Retrieves the complete window styles bitmask for a control
	RETURN GetWindowLong( SELF:Handle(), GWL_STYLE )






/// <include file="Gui.xml" path="doc/Control.Handle/*" />
METHOD Handle(uType) AS PTR


	IF !IsNil(uType) .AND. !IsLong(uType)
		WCError{#Handle,#Control,__WCSTypeError,uType,1}:Throw()
	ENDIF
	IF (hWnd == NULL_PTR )
		SELF:Create()
	ENDIF


	RETURN hWnd


/// <include file="Gui.xml" path="doc/Control.HandleAsDword/*" />
METHOD HandleAsDword()
	RETURN DWORD(_CAST,SELF:Handle())


/// <include file="Gui.xml" path="doc/Control.HasBorder/*" />
METHOD HasBorder()
LOCAL lBorder		AS LOGIC
LOCAL nStyle		AS LONG


	// DHer: 18/12/2008
	nStyle := GetWindowLong(SELF:hWnd,GWL_EXSTYLE)
	IF _AND(nStyle,WS_EX_DLGMODALFRAME)>0
		lBorder := TRUE
	ELSEIF _AND(nStyle,WS_EX_WINDOWEDGE)>0
		lBorder := TRUE
	ELSEIF _AND(nStyle,WS_EX_STATICEDGE)>0
		lBorder := TRUE
	ELSEIF _AND(nStyle,WS_EX_CLIENTEDGE)>0
		lBorder := TRUE
	ELSE
		nStyle := GetWindowLong(SELF:hWnd,GWL_STYLE)
		IF _AND(nStyle,WS_BORDER)>0
			lBorder := TRUE
		ENDIF
	ENDIF


RETURN lBorder


/// <include file="Gui.xml" path="doc/Control.Hide/*" />
METHOD Hide()




	IF IsWindow(hWnd)
		ShowWindow(hWnd, SW_HIDE)
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/Control.HorizontalScroll/*" />
METHOD HorizontalScroll(oScrollEvent)




	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.HyperLabel/*" />
ACCESS HyperLabel




	RETURN __oHyperLabel




/// <include file="Gui.xml" path="doc/Control.HyperLabel/*" />
ASSIGN HyperLabel(oNewHL)
	// Assign method for HyperLabel




	IF IsInstanceOfUsual(oNewHL, #HyperLabel)
		__oHyperLabel := oNewHL
		lExplicitHL := TRUE
		SELF:Caption := oNewHL:Caption
	ELSEIF oNewHL != NIL
		WCError{#HyperLabel,#Control,__WCSTypeError,oNewHL,1}:Throw()
	ELSE
		//PP-040410 next line was incorrectly assigning NIL
		__oHyperLabel := NULL_OBJECT
		lExplicitHL := FALSE
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/Control.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cRegClass, kStyle, lDataAware)
	SUPER()


	IF IsObject(oOwner)
		oParent := oOwner
	ELSEIF IsPtr(oOwner)
		oParent := __ForeignWindow{oOwner}
	ELSE
		WCError{#Init,#Control,__WCSTypeError,oOwner,1}:Throw()
	ENDIF


	IF oParent IS DataWindow VAR oDW
		oFormSurface 	:= oDw:__GetFormSurface()
	ELSE
		oFormSurface 	:= oParent
	ENDIF


	IF IsLong(xID)
		IF IsString(oPoint) .AND. IsNil(oDimension)
			xID := ResourceID{ xID }
		ENDIF
	ELSE
		IF !IsInstanceOfUsual(xID,#ResourceID)
			WCError{#Init,#Control,__WCSTypeError,xID,2}:Throw()
		ENDIF
	ENDIF






	IF oFormSurface IS DialogWindow .AND. ;
			(IsInstanceOfUsual(xID,#ResourceID) .OR. (IsLong(xID) .AND.;
			IsNil(oDimension) .AND. IsNil(oPoint)))


		IF IsInstanceOfUsual(xID,#ResourceID)
			wId 		:= xID:ID
		ELSE
			wId 		:= xID
		ENDIF


		dwStyle := WS_CHILD
		IF !IsNil(kStyle)
			dwStyle := _OR(dwStyle, DWORD(kStyle))
		ENDIF


		hWnd := GetDlgItem(oFormSurface:Handle(), wId)


		__WCRegisterControl(SELF) //register after we get the handle
		SetWindowLong(hWnd, GWL_STYLE, _OR(GetWindowLong(hwnd, GWL_STYLE), LONGINT(_CAST, dwStyle)))
		SetWindowLong(hWnd, GWL_EXSTYLE, LONGINT(_CAST, _AND(GetWindowLong(hwnd, GWL_EXSTYLE), _NOT(WS_EX_NOPARENTNOTIFY))))


		__lpfnDefaultProc := PTR(_CAST, GetWindowLong(hWnd, GWL_WNDPROC))
		SetWindowLong(hWnd, GWL_WNDPROC, LONGINT(_CAST, Get__WCControlProcPtr()))


		IF !IsNil(oPoint)
			SELF:Origin := oPoint
		ENDIF
		IF !IsNil(oDimension)
			SELF:Size := oDimension
		ENDIF
	ELSEIF (oFormSurface IS Window .OR. oFormSurface IS Control) .AND. IsLong(xID)
		IF !IsInstanceOfUsual(oPoint,#Point)
			WCError{#Init,#Control,__WCSTypeError,oPoint,3}:Throw()
		ENDIF
		IF !IsInstanceOfUsual(oDimension,#Dimension)
			WCError{#Init,#Control,__WCSTypeError,oDimension,4}:Throw()
		ENDIF


		IF !IsNil(cRegClass) .AND. !IsString(cRegClass)
			WCError{#Init,#Control,__WCSTypeError,cRegClass,5}:Throw()
		ENDIF


		oSize 	    := Dimension{oDimension:Width, oDimension:Height}
		oOrigin 	:= Point{oPoint:x, oPoint:y}
		wId 		:= xID
		dwStyle 	:= WS_CHILD
		IF !IsNil(kStyle)
			dwStyle := _OR(dwStyle, DWORD(kStyle))
		ENDIF
		cClassName := cRegClass
		SWITCH cClassName:ToLower()
        CASE "edit"
        CASE "listbox"
        CASE "sysanimate32"
        CASE "systreeview32"
        CASE "syslistview32"
        CASE "msctls_hotkey32"
			dwExStyle := _OR(dwExStyle, DWORD(_CAST, WS_EX_CLIENTEDGE))
		CASE "msctls_progress32"
			dwExStyle := _OR(dwExStyle, DWORD(_CAST, WS_EX_CLIENTEDGE), DWORD(_CAST, WS_EX_STATICEDGE))
		END SWITCH
	ELSE
		WCError{#Init,#Control,__WCSTypeError}:Throw()
	ENDIF


	IF IsLogic(lDataAware)
		SELF:lDataAware:=lDataAware
	ENDIF


	IF SELF:lDataAware .AND.;
			!(SELF IS DataBrowser) .AND.;
			IsMethod(oParent, #__SetUpDataControl)
		oParent:__SetupDataControl(SELF)
	ELSEIF oParent IS DataWindow VAR oDW2
		oDw2:__SetupNonDataControl(SELF)
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/Control.IsDestroyed/*" />
ACCESS IsDestroyed
	// DHer: 18/12/2008
RETURN SELF:lIsDestroyed


/// <include file="Gui.xml" path="doc/Control.IsDisabled/*" />
ACCESS IsDisabled AS LOGIC
	// Determines wether the control is disabled
	// TRUE if the control is disabled, FALSE if not
	RETURN SELF:IsStyle( WS_DISABLED )


/// <include file="Gui.xml" path="doc/Control.IsEditable/*" />
ACCESS IsEditable
   LOCAL lEditable	AS LOGIC
	// DHer: 18/12/2008
	lEditable := FALSE
	IF !SELF:lIsDestroyed
		IF IsWindowVisible(SELF:Handle())
			IF IsWindowEnabled(SELF:Handle())
				IF SELF IS EDIT .OR.  SELF IS BASELISTBOX .OR. ;
						SELF IS MONTHCALENDAR .OR. ;
						SELF IS DATETIMEPICKER .OR. ;
						SELF IS IPADDRESS
					lEditable := TRUE
				ENDIF
				IF SELF IS EDIT VAR oEdit
					IF oEdit:ReadOnly
						lEditable := FALSE
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDIF
RETURN lEditable


/// <include file="Gui.xml" path="doc/Control.IsEnabled/*" />
METHOD IsEnabled()
	//PP-030319 added method
	RETURN (_AND(GetWindowLong(SELF:Handle(),GWL_STYLE),WS_DISABLED)==0)


/// <include file="Gui.xml" path="doc/Control.IsExStyle/*" />
METHOD IsExStyle( nStyle AS LONG) AS LOGIC
	// Determines wether a certain windows ex-style is set for this control
	RETURN _And( LONG( SELF:GetExStyle() ), nStyle  ) != 0


/// <include file="Gui.xml" path="doc/Control.IsReadOnly/*" />
METHOD IsReadOnly()
	//PP-030319 added method
	RETURN IIF(SELF IS Edit, (_AND(GetWindowLong(SELF:Handle(),GWL_STYLE),ES_READONLY)!=0), FALSE)




/// <include file="Gui.xml" path="doc/Control.IsStyle/*" />
METHOD IsStyle( nStyle AS LONG ) AS LOGIC
	// Determines wether a certain windows style is set for this control
	RETURN _And( SELF:GetStyle() , nStyle  ) != 0




/// <include file="Gui.xml" path="doc/Control.IsVisible/*" />
METHOD IsVisible
	LOCAL style AS LONGINT


	style := GetWindowLong(SELF:Handle(), GWL_STYLE)
	RETURN _AND(style, WS_Visible) != 0




/// <include file="Gui.xml" path="doc/Control.KeyDown/*" />
METHOD KeyDown(oKeyEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.KeyUp/*" />
METHOD KeyUp(oKeyEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.LinkDF/*" />
METHOD LinkDF(oDS, siDF)
	LOCAL tmpDF AS OBJECT
	LOCAL symClassName AS SYMBOL


	IF !IsNumeric(siDF)
		siDF:=oDS:FieldPos(siDF)
	ENDIF


	IF !IsInstanceOfUsual(oDS,#DataServer)
		WCError{#LinkDF,#Control,__WCSTypeError,oDS,1}:Throw()
	ENDIF


	IF (!IsNil(oServer) .AND. (oDS!=oServer))
		SELF:__Unlink()
	ENDIF


	oServer 	 := oDS
	siDataField  := siDF
	symDataField := oServer:FieldSym(siDataField)
	IF IsMethod(oServer, #IsBaseField)
       lBaseServer := Send(oServer, #IsBaseField, symDataField)
    ELSE
    	symClassName := ClassName(oServer)
	    lBaseServer  := symClassName==#DBServer .OR. symClassName==#SQLSelect .OR. symClassName==#SQLTable .OR. symClassName==#JDataServer
	ENDIF


	// Propogate data field if no explicit one
	IF ((tmpDF := oServer:DataField(siDataField)) != NULL_OBJECT)
		oDataField := tmpDF


		IF !lExplicitFS
			// propogate field spec if no explicit one
			SELF:FieldSpec := SELF:oDataField:FieldSpec


			// propogate field spec
			IF !lExplicitHL
				// CHECK if NameSym and hyperlabel are same
				IF !IsNil(SELF:oDataField:HyperLabel) .AND. (oDataField:NameSym == oDataField:HyperLabel:NameSym)
					__oHyperLabel := oDataField:HyperLabel
				ELSE
					//RvdH 060608 optimized: cCaption is a string
					//__oHyperLabel := HyperLabel { oDataField:NameSym, iif(!Empty(cCaption), cCaption, oDataField:Name) }
					__oHyperLabel := HyperLabel { oDataField:NameSym, IIF(SLen(cCaption)>0, cCaption, oDataField:Name) }
				ENDIF
			ENDIF
		ENDIF
	ENDIF


	uGetSetOwner := NIL
	cbGetSetBlock := NULL_CODEBLOCK


	RETURN SELF




/// <include file="Gui.xml" path="doc/Control.MenuInit/*" />
METHOD MenuInit(oMenuInitEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.MenuSelect/*" />
METHOD MenuSelect(oMenuSelectEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.Modified/*" />
ACCESS Modified
	RETURN __lModified




/// <include file="Gui.xml" path="doc/Control.Modified/*" />
ASSIGN Modified(lChangedFlag)
	IF IsLogic(lChangedFlag)
		__lModified := lChangedFlag
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/Control.MouseButtonDoubleClick/*" />
METHOD MouseButtonDoubleClick(oMouseEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.MouseButtonDown/*" />
METHOD MouseButtonDown(oMouseEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.MouseButtonUp/*" />
METHOD MouseButtonUp(oMouseEvent)
	//PP-040410 This is handled better in dispatch
	//
	// 	IF (oMouseEvent:ButtonID == BUTTONRIGHT) .and. (oContextMenu != NULL_OBJECT)
	// 		oContextMenu:ShowAsPopup(SELF)
	// 	ENDIF


	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.MouseDrag/*" />
METHOD MouseDrag(oMouseEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.MouseMove/*" />
METHOD MouseMove(oMouseEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.Move/*" />
METHOD Move(oMoveEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.Name/*" />
ACCESS Name
	IF (__oHyperLabel != NULL_OBJECT)
		RETURN __oHyperLabel:Name
	ENDIF


	RETURN NULL_STRING




/// <include file="Gui.xml" path="doc/Control.NameSym/*" />
ACCESS NameSym
	IF (__oHyperLabel != NULL_OBJECT)
		RETURN __oHyperLabel:NameSym
	ENDIF


	RETURN NULL_SYMBOL




/// <include file="Gui.xml" path="doc/Control.Origin/*" />
ACCESS Origin
	IF (hWnd == NULL_PTR .AND. oOrigin != NULL_OBJECT)
		RETURN oOrigin
	ELSE
		RETURN __WCGetOrigin(SELF)
	ENDIF


/// <include file="Gui.xml" path="doc/Control.Origin/*" />
ASSIGN Origin(oPoint)
	IF !IsInstanceOfUsual(oPoint, #Point)
		WCError{#Origin,#Control,__WCSTypeError,oPoint,1}:Throw()
	ENDIF


	IF (hWnd == NULL_PTR)
		SELF:oOrigin := Point{oPoint:X, oPoint:y}
	ELSE
		WCMoveWindow(SELF, oPoint, SELF:Size, TRUE)
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/Control.OverRide/*" />
METHOD OverRide(lEnable)
	//local hHandle as ptr
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.Owner/*" />
ACCESS Owner
	RETURN SELF:oParent




/// <include file="Gui.xml" path="doc/Control.OwnerAlignment/*" />
ASSIGN OwnerAlignment(iNewVal)
	IF !(oFormSurface IS Window) .OR. ! (oParent IS Window)
		RETURN OA_NO
	ELSEIF oFormSurface IS Window VAR oWin
		oWin:__AddAlign(SELF, iNewVal)
	ELSE
		oParent:__AddAlign(SELF, iNewVal)
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/Control.PerformValidations/*" />
METHOD PerformValidations() CLIPPER
	// PerformValidations
	// Perform validations for Control against supplied parameter
	// if it has a data spec, otherwise just return true




	IF oFieldSpec IS FieldSpec VAR oFS
		IF !oFS:PerformValidations(uValue,SELF)
			oHLStatus := oFS:Status
			RETURN FALSE
			// elseif !oDataField:PerformValidations(uValue)
			// oHLStatus:=oDataField:Status
			// return false
		ENDIF
	ENDIF
	oHLStatus := NULL_OBJECT


	RETURN TRUE




/// <include file="Gui.xml" path="doc/Control.RegisterTimer/*" />
METHOD RegisterTimer(nInterval,lOneTime)
	IF !IsLong(nInterval)
		WCError{#RegisterTimer,#Control,__WCSTypeError,nInterval,1}:Throw()
	ENDIF


	IF !IsNil(lOneTime)
		IF !IsLogic(lOneTime)
			WCError{#RegisterTimer,#Control,__WCSTypeError,lOneTime,2}:Throw()
		ENDIF
		IF lOneTime
			dwTimerInterval:=0
		ELSE
			dwTimerInterval := nInterval
		ENDIF
	ELSE
		dwTimerInterval := nInterval
	ENDIF


	IF (nInterval > 0)
		dwTimerCount := nInterval
		IF !lTimerRegistered
			__WCRegisterTimer(SELF)
			lTimerRegistered := TRUE
		ENDIF
	ELSE
		__WCUnregisterTimer(SELF)
		lTimerRegistered := FALSE
	ENDIF


	RETURN SELF


/// <include file="Gui.xml" path="doc/Control.ReadOnly/*" />
ACCESS ReadOnly
	RETURN SELF:IsEnabled()


/// <include file="Gui.xml" path="doc/Control.ReadOnly/*" />
ASSIGN ReadOnly(lNewValue)
   IF (lNewValue)
      SELF:Disable()
   ELSE
      SELF:Enable()
   ENDIF
	RETURN






/// <include file="Gui.xml" path="doc/Control.RePaint/*" />
METHOD RePaint()
	IF hWnd != NULL_PTR
		InvalidateRect(hWnd, NULL_PTR, TRUE)
	ENDIF
	RETURN SELF


/// <include file="Gui.xml" path="doc/Control.Resize/*" />
METHOD Resize(oResizeEvent)
	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.RestoreUpdate/*" />
METHOD RestoreUpdate()




	SendMessage(SELF:Handle(), WM_SETREDRAW, 1, 0)
	RETURN SELF




/// <include file="Gui.xml" path="doc/Control.Server/*" />
ACCESS Server




	RETURN oServer




/// <include file="Gui.xml" path="doc/Control.SetExStyle/*" />
METHOD SetExStyle(kExStyle, lEnable)
	// 607@001 RJ 02-09-96 - set control's extended style
	LOCAL dwTemp AS DWORD






	IF !IsLong(kExStyle)
		WCError{#SetExStyle,#Control,__WCSTypeError,kExStyle,}:Throw()
	ENDIF


	IF IsNil(lEnable) .OR. !IsLogic(lEnable)
		lEnable := TRUE
	ENDIF


	IF hWnd == NULL_PTR
		IF lEnable
			dwExStyle := _OR(DWORD(kExStyle), dwExStyle)
		ELSE
			dwExStyle := _AND(dwExStyle, _NOT(DWORD(kExStyle)))
		ENDIF
	ELSE
		dwTemp := DWORD(_CAST, GetWindowLong(hWnd, GWL_EXSTYLE))
		IF lEnable
			dwTemp := _OR(DWORD(kExStyle), dwTemp)
		ELSE
			dwTemp := _AND(dwTemp, _NOT(DWORD(kExStyle)))
		ENDIF


		SetWindowLong(hWnd, GWL_EXSTYLE, LONGINT(_CAST, dwTemp))
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/Control.SetFocus/*" />
METHOD SetFocus()




	IF SELF:ValidateControl()
		// 2.5b was #DIALOG !
		IF oParent IS DialogWindow
			SendMessage(oParent:Handle(), WM_NEXTDLGCTL, DWORD(_CAST, SELF:Handle()), 1L)
		ELSE
			SetFocus(SELF:Handle())
		ENDIF
	ENDIF


	RETURN SELF


/// <include file="Gui.xml" path="doc/Control.HasStyle/*" />
METHOD HasStyle(kStyle AS LONG)
   LOCAL liStyle	AS LONG
	// DHer: 18/12/2008


	liStyle := GetWindowLong(SELF:hWnd,GWL_STYLE)


	RETURN _AND(liStyle,kStyle) != 0


/// <include file="Gui.xml" path="doc/Control.SetParent/*" />
METHOD SetParent( hWndNewParent AS PTR)  AS VOID
	SetParent( SELF:Handle() , hWndNewParent  )
	RETURN


/// <include file="Gui.xml" path="doc/Control.Style/*" />
ACCESS Style
	// DHer: 18/12/2008
	RETURN GetWindowLong(SELF:hWnd,GWL_STYLE)


/// <include file="Gui.xml" path="doc/Control.SetStyle/*" />
METHOD SetStyle(kStyle, lEnable)
	LOCAL dwTemp AS DWORD


	IF !IsLong(kStyle)
		WCError{#SetStyle,#Control,__WCSTypeError,kStyle,}:Throw()
	ENDIF


	IF IsNil(lEnable) .OR. !IsLogic(lEnable)
		lEnable := TRUE
	ENDIF


	IF (hWnd == NULL_PTR)
		IF lEnable
			dwStyle := _OR(DWORD(kStyle), dwStyle)
		ELSE
			dwStyle := _AND(dwStyle, _NOT(DWORD(kStyle)))
		ENDIF
	ELSE
		dwTemp := DWORD(_CAST, GetWindowLong(hWnd, GWL_STYLE))


		IF lEnable
			dwTemp := _OR(DWORD(kStyle), dwTemp)
		ELSE
			dwTemp := _AND(dwTemp, _NOT(DWORD(kStyle)))
		ENDIF


		SetWindowLong(hWnd, GWL_STYLE, LONGINT(_CAST, dwTemp))
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/Control.Show/*" />
METHOD Show()


	IF (hWnd == NULL_PTR)
		SELF:Create()
	ENDIF


	IF IsWindow(hWnd)
		ShowWindow(hWnd, SW_SHOWNORMAL)
	ENDIF


	RETURN NIL




/// <include file="Gui.xml" path="doc/Control.Size/*" />
ACCESS Size
	LOCAL strucRect IS _WinRect






	IF (hWnd == NULL_PTR)
		RETURN oSize
	ENDIF
	GetWindowRect(hwnd,@strucRect)


	RETURN Dimension{strucRect:Right-strucRect:Left,strucRect:Bottom-strucRect:Top}




/// <include file="Gui.xml" path="doc/Control.Size/*" />
ASSIGN Size(oDimension)


	IF IsInstanceOfUsual(oDimension, #BoundingBox)
		oDimension := oDimension:Size
	ENDIF


	IF !IsInstanceOfUsual(oDimension,#Dimension)
		WCError{#Size,#Control,__WCSTypeError,oDimension,1}:Throw()
	ENDIF


	IF (hWnd == NULL_PTR)
		oSize	:= Dimension{oDimension:Width, oDimension:Height}
	ELSE
		WCMoveWindow(SELF, SELF:Origin, oDimension, TRUE)
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/Control.Status/*" />
ACCESS Status


	RETURN oHLStatus

/// <include file="Gui.xml" path="doc/Control.Status/*" />
ASSIGN Status(oStatus)
    //SE-081122 null_object is a valid parameter now
    IF IsObject(oStatus) .AND. (IsInstanceOf(oStatus, #HyperLabel) .OR. oStatus == NULL_OBJECT)
         RETURN oHlStatus := oStatus
    ENDIF
    WCError{#Status,#Control,__WCSTypeError,oStatus,1}:Throw()
    RETURN oStatus




/// <include file="Gui.xml" path="doc/Control.SuspendUpdate/*" />
METHOD SuspendUpdate()




	SendMessage(SELF:Handle(), WM_SETREDRAW, 0 ,0)
	RETURN SELF




/// <include file="Gui.xml" path="doc/Control.TextValue/*" />
ACCESS TextValue




	RETURN ""




/// <include file="Gui.xml" path="doc/Control.TextValue/*" />
ASSIGN TextValue (cNewText)




	RETURN




/// <include file="Gui.xml" path="doc/Control.Timer/*" />
METHOD Timer()




	RETURN SELF




/// <include file="Gui.xml" path="doc/Control.ToolTipText/*" />
ACCESS ToolTipText




	RETURN sToolTipText




/// <include file="Gui.xml" path="doc/Control.ToolTipText/*" />
ASSIGN ToolTipText(sNewText)




	oFormSurface:__AddTool(SELF)


	RETURN (sToolTipText := sNewText)




/// <include file="Gui.xml" path="doc/Control.UseHLForToolTip/*" />
ACCESS UseHLForToolTip




	RETURN lUseHLForToolTip




/// <include file="Gui.xml" path="doc/Control.UseHLForToolTip/*" />
ASSIGN UseHLForToolTip(lNewValue)




	oFormSurface:__AddTool(SELF)


	RETURN (lUseHLForToolTip := lNewValue)




/// <include file="Gui.xml" path="doc/Control.ValidateControl/*" />
METHOD ValidateControl()




	IF (hWnd == NULL_PTR)
		SELF:Create()
	ENDIF


	RETURN (hWnd != NULL_PTR) .AND. IsWindow(hWnd)




/// <include file="Gui.xml" path="doc/Control.Value/*" />
ACCESS Value
	// returns last valid value
	// value is only updated on leaving the field




	RETURN uValue




/// <include file="Gui.xml" path="doc/Control.Value/*" />
ASSIGN Value(uNewValue)
	//
	// Value assignment
	// Note : value does not have to be same as what is displayed
	//
	LOCAL cOldValue AS STRING

	cOldValue := AsString(uValue)
	// !!! should be result of FIELDGET and located after FIELDPUT !!!
	SELF:__Value := uNewValue //Update the control
	IF IsInstanceOfUsual(oServer, #DataServer)
		IF lBaseServer // if not subclassing
			oServer:FIELDPUT(siDataField,SELF:uValue) //update the DataServer
		ELSEIF symDataField != NULL_SYMBOL
			IVarPut(oServer, symDataField ,SELF:uValue)
		ELSE
			IVarPut(oServer, SELF:NameSym, SELF:uValue)
		ENDIF
	ENDIF
	SELF:ValueChanged := !(cOldValue == AsString(uValue))


	RETURN




/// <include file="Gui.xml" path="doc/Control.ValueChanged/*" />
ACCESS ValueChanged




	RETURN lChanged




/// <include file="Gui.xml" path="doc/Control.ValueChanged/*" />
ASSIGN ValueChanged(lFlag)




	IF !IsLogic(lFlag)
		WCError{#ValueChanged,#Control,__WCSTypeError,lFlag,1}:Throw()
	ENDIF


	RETURN SELF:lChanged := lFlag




/// <include file="Gui.xml" path="doc/Control.VerticalScroll/*" />
METHOD VerticalScroll(oScrollEvent)




	RETURN NIL


END CLASS


#ifdef __VULCAN__
    /// <exclude/>
   DELEGATE __WCControlProcDelegate( hWnd AS PTR, umsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LONGINT
    /// <exclude/>
   FUNCTION Get__WCControlProcPtr() AS PTR
      STATIC LOCAL WCControlProcDelegate AS __WCControlProcDelegate
      IF WCControlProcDelegate == NULL
         WCControlProcDelegate := __WCControlProcDelegate{ NULL, @__WCControlProc() }
      ENDIF
      RETURN System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) WCControlProcDelegate )


#else
/// <include file="Gui.xml" path="doc/Get__WCControlProcPtr/*" />
   FUNCTION Get__WCControlProcPtr() AS PTR
      RETURN @__WCControlProc()
#endif


 /// <exclude />
FUNCTION __WCControlProc(hWnd AS PTR, umsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LONGINT /* WINCALL */




	LOCAL oControl AS Control
    LOCAL oWindow  AS ControlWindow
	LOCAL liRetVal AS LONGINT
    LOCAL liEventReturnValue AS LONG
	LOCAL pDefProc AS PTR


    oControl := __WCGetControlByHandle(hWnd)


    IF (oControl != NULL_OBJECT)
         IF oControl:__ControlWindow != NULL_OBJECT
            oWindow := oControl:__ControlWindow
            liRetVal := oWindow:Dispatch(@@Event{hWnd, umsg, wParam, lParam, oControl})
            liEventReturnValue := oWindow:EventReturnValue
         ELSE
            liRetVal := oControl:Dispatch(@@Event{hWnd, umsg, wParam, lParam, oControl})
            liEventReturnValue := oControl:EventReturnValue
         ENDIF
		IF (liRetVal != 0)
             liRetVal := liEventReturnValue
		ELSE
            pDefProc := oControl:__lpfnDefaultProc
		    liRetVal := CallWindowProc(pDefProc, hWnd, umsg, wParam, lParam)
		ENDIF
		IF (umsg == WM_DESTROY)
			__WCUnregisterControl(hWnd)
		ENDIF
	ENDIF


	RETURN liRetVal
