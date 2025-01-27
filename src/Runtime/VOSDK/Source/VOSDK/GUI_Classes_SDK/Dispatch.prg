
PARTIAL CLASS Window


/// <include file="Gui.xml" path="doc/Window.Dispatch/*" />
METHOD Dispatch(oEvent)
	LOCAL msg IS _winMSG
	LOCAL uMsg AS DWORD
	LOCAL dwHiWord AS DWORD
	LOCAL dwLParam AS DWORD
	LOCAL oTempEvent AS OBJECT
	LOCAL oChild AS OBJECT
	LOCAL ptrBuf AS PTR
	LOCAL cBuf AS STRING
	LOCAL strucPS IS _winPAINTSTRUCT
	LOCAL _hdc AS PTR
	LOCAL _Handle AS PTR
    LOCAL oControl AS Control
	LOCAL hBr AS PTR
	LOCAL lHelpEnable AS LOGIC
	LOCAL lclient AS LOGIC
	LOCAL oObject AS OBJECT
	LOCAL oEvt AS @@event
	LOCAL strHelpInfo AS _winHELPINFO
	LOCAL oMenuHL AS HyperLabel
//	LOCAL wp AS DWORD




	oEvt := oEvent
	SELF:EventReturnValue := 0L
	uMsg := oEvt:uMsg
//	wp   := oEvt:wParam


	SWITCH uMsg
	CASE WM_CTLCOLORBTN
	CASE WM_CTLCOLOREDIT
	CASE WM_CTLCOLORLISTBOX
	CASE WM_CTLCOLORSTATIC
	CASE WM_CTLCOLORSCROLLBAR


		_Handle := PTR(_CAST, oEvt:lParam)
		_hdc := PTR(_CAST, oEvt:wParam)


        oControl := __WCGetControlByHandle(_Handle)


        IF (oControl == NULL_OBJECT)
            oControl := __WCGetControlByHandle(GetParent(_Handle))
            IF !IsInstanceOf(oControl, #ComboBox)
                oControl := NULL_OBJECT
            ENDIF
        ENDIF


		IF (oControl != NULL_OBJECT) .AND. IsMethod(oControl, #__SetColors)
			hBr := oControl:__SetColors(_hdc)
			IF (hBR != NULL_PTR)
				RETURN (SELF:EventreturnValue := (LONGINT(_CAST, hbr)))
			ENDIF
		ELSE
			//PP-031129
			//For not generated controls with transparent style
			IF _AND(GetWindowLong(_Handle, GWL_EXSTYLE), WS_EX_TRANSPARENT)>0
				hbr := GetStockObject(HOLLOW_BRUSH)
				SetBkMode(_hdc, TRANSPARENT)
				IF IsThemeEnabled()
					DrawThemeParentBackground(_Handle, _hDC, NULL_PTR)
				ENDIF
				RETURN (SELF:EventreturnValue := (LONGINT(_CAST, hbr)))
			ENDIF
		ENDIF


	CASE WM_NOTIFY
		SELF:ControlNotify(ControlNotifyEvent{oEvt})
		RETURN SELF:EventReturnValue


	CASE WM_ERASEBKGND
		//PP-031129 added call to DrawBackGround
		_hdc := PTR(_CAST, oEvt:wParam)
		IF (SELF:background != NULL_OBJECT)
			SELF:PaintBackground(_hdc)
			SELF:EventReturnValue := 1L
		ELSEIF SELF:DrawBackground(_hdc, SELF)
			SELF:EventReturnValue := 1L
		ELSE
			SELF:Default(oEvt)
		ENDIF
		RETURN SELF:EventReturnValue


	CASE WM_PAINT
		hDCPaint := BeginPaint(hWnd, @strucPS)
		strucPaintRect := @strucPS:rcPaint
		SELF:ExPose(ExposeEvent{oEvt})
		EndPaint(hWnd, @strucPS)
		hDCPaint := NULL_PTR
		strucPaintRect := NULL_PTR
		RETURN SELF:EventReturnValue


	CASE WM_DRAWITEM
		//PP-030319 owner draw support, thanks to SEbert
		//Used for owner drawn menus or controls
		__Dispatch_DrawItem(oEvt, SELF)
		RETURN SELF:EventReturnValue


	CASE WM_MEASUREITEM
		//PP-030319 owner draw support
		//Used for owner drawn menus or controls
		__Dispatch_MeasureItem(oEvt, SELF)
		RETURN SELF:EventReturnValue


		// Liuho01 05-01-96 Application receives drop notification in this message
	CASE WM_DROPFILES
    CASE WM_QUERYDROPOBJECT
	CASE WM_DRAGSELECT
		oObject := SELF:DragDropClient
		IF (oObject != NULL_OBJECT)
			oObject:Dispatch(oEvt)
		ENDIF
		RETURN SELF:EventReturnValue


	CASE WM_WCHELP
		SELF:__EnableHelpCursor(FALSE)
		SELF:HelpRequest(HelpRequestEvent{oEvt})
		RETURN SELF:EventReturnValue


	CASE WM_HELP
		//SE-060522 S.Ebert
		IF ! lHelpOn
			SELF:HelpRequest(HelpRequestEvent{oEvt})
			IF SELF:EventReturnValue = 1L
				RETURN 1L
			ENDIF
		ENDIF
		strHelpInfo := PTR(_CAST, oEvt:lParam)
		IF strHelpInfo:iContextType = HELPINFO_MENUITEM .AND. SELF:Menu != NULL_OBJECT
			oMenuHL := SELF:Menu:HyperLabel(strHelpInfo:iCtrlId)
			IF (oMenuHL != NULL_OBJECT) .AND. !Empty(oMenuHL:HelpContext) .AND. (SELF:HelpDisplay != NULL_OBJECT)
				SELF:HelpDisplay:Show(oMenuHL:HelpContext)
			ENDIF
			RETURN SELF:EventReturnValue
		ENDIF


	CASE WM_ACTIVATE
		IF LoWord(oEvt:wParam) != WA_INACTIVE    //FdW//20061202 // LoWord() added
			IF lHelpOn .AND. (oApp != NULL_OBJECT)
				IF lHelpCursorOn
					oApp:__SetHelpWind(hWnd,HM_MOUSE)
				ELSE
					oApp:__SetHelpWind(hWnd,HM_GENERAL)
				ENDIF
			ENDIF
			SELF:__AssociateAccel(TRUE)
			SELF:Activate(oEvt)
		ELSE
			SELF:__AssociateAccel(FALSE)
			SELF:DeActivate(oEvt)
		ENDIF
		RETURN SELF:EventReturnValue


	CASE WM_SETFOCUS
    CASE WM_KILLFOCUS
		// if __lFilterFocusMsg
		// 	self:EventReturnValue := 1L
		// endif
		IF !IsWindow(PTR(_CAST, oEvt:wParam))
			SELF:EventReturnValue := 1L
		ENDIF
		IF (oCursor != NULL_OBJECT)
			oCursor:__Update((uMsg == WM_SETFOCUS))
		ENDIF
		SELF:FocusChange(FocusChangeEvent{oEvt})
		RETURN SELF:EventReturnValue


	CASE WM_APPCOMMAND
		//PP-030904
		// If AppCommand returns TRUE it means the command has been processed so 1 is returned
		// so that no further action is taken.
		SELF:EventReturnValue := IIF(SELF:AppCommand(AppCommandEvent{oEvt}),1,0)
		RETURN SELF:EventReturnValue


	CASE WM_LBUTTONDOWN
    CASE WM_RBUTTONDOWN
    CASE WM_MBUTTONDOWN
    CASE WM_XBUTTONDOWN
		//PP-030904 Xbutton
		SELF:MouseButtonDown(MouseEvent{oEvt})
		RETURN SELF:EventReturnValue


	CASE WM_LBUTTONUP
		IF lFileDragging
			IF IsMethod(SELF, #MouseDrag)
				SELF:MouseDrag(MouseEvent{oEvt})
			ELSEIF IsMethod(oParent, #MouseDrag)
				oParent:MouseDrag(MouseEvent{oEvt})
			ENDIF
			// if IsMethod(self, #MouseDrag)
			// oObject := oParent
			// else
			// oObject := self
			// endif
			lFileDragging := FALSE
			ReleaseCapture()
		ENDIF
        	IF SELF IS __FormFrame //SE-111104 for DataWindow
			    SELF:Owner:MouseButtonUp(MouseEvent{oEvt})
	        ELSE
        	   SELF:MouseButtonUp(MouseEvent{oEvt})
	        ENDIF
		RETURN SELF:EventReturnValue


	CASE WM_RBUTTONUP
    CASE WM_MBUTTONUP
    CASE WM_XBUTTONUP
		//PP-030904 Xbutton
		SELF:MouseButtonUp(MouseEvent{oEvt})
		RETURN SELF:EventReturnValue


	CASE WM_LBUTTONDBLCLK
    CASE WM_RBUTTONDBLCLK
    CASE WM_MBUTTONDBLCLK
    CASE WM_XBUTTONDBLCLK
		//PP-030904 Xbutton
		SELF:MouseButtonDoubleClick(MouseEvent{oEvt})
		RETURN SELF:EventReturnValue


	CASE WM_CHAR
		SELF:KeyDown(KeyEvent{oEvt})
		RETURN SELF:EventReturnValue


	CASE WM_HSCROLL
    CASE WM_VSCROLL
		dwLParam := DWORD(_CAST,oEvt:lParam)
		IF dwLParam != 0
			ptrBuf := MemAlloc(50)
			GetClassName(dwLParam,ptrBuf,50)
			cBuf:=Psz2String(ptrBuf)
			MemFree(ptrBuf)
		ENDIF
		IF (cBuf == UpDown_Class)
			oTempEvent := SpinnerEvent{oEvt}
			IF (uMsg == WM_HSCROLL)
				SELF:HorizontalSpin(oTempEvent)
			ELSE
				SELF:VerticalSpin(oTempEvent)
			ENDIF
		ELSEIF (cBuf == TrackBar_Class)
			oTempEvent := SliderEvent{oEvt}
			IF (uMsg == WM_HSCROLL)
				SELF:HorizontalSlide(oTempEvent)
			ELSE
				SELF:VerticalSlide(oTempEvent)
			ENDIF
		ELSE
            oTempEvent := ScrollEvent{oEvt}
            //	oScrollBar := oTempEvent:ScrollBar


			//if (sbi && CV_RunTime::ScrollBar_GetIgnore(sbi)
			// && (CV_RunTime::Event_wParam(e) == SB_ENDSCROLL))
			// CV_RunTime::ScrollBar_SetIgnore(sbi, 0)
			// ((pDW)pRequestor) -> _Default(e)
			//endif


			IF (uMsg == WM_HSCROLL)
				SELF:HorizontalScroll(oTempEvent)
			ELSE
				SELF:VerticalScroll(oTempEvent)
			ENDIF


			// Required for Windows 3.x onwards
			//if (sbi && CV_RunTime ::Event_wParam(e) == SB_THUMBPOSITION)
			// CV_RunTime::ScrollBar_SetIgnore(sbi, 1)
			//endif
		ENDIF
		RETURN SELF:EventReturnValue


	CASE WM_COMMAND


		dwLParam := DWORD(_CAST, oEvt:lParam)


		IF (dwLParam != 0)
			IF lHelpCursorOn
				SELF:__EnableHelpCursor(FALSE)
			ELSE
				oChild :=__WCGetObjectByHandle(dwLParam)
				// temp hack for ReBar/ToolBar
				IF (oChild == NULL_OBJECT)
					oChild :=__WCGetObjectByHandle(GetParent(PTR(_CAST, oEvt:lParam)))
				ENDIF
				dwHiWord := HiWord(oEvt:wParam)


				DO CASE
				CASE oChild IS Button
					oTempEvent := ControlEvent{oEvt}


					SWITCH dwHiWord
					CASE BN_CLICKED
						IF oChild IS PushButton


							// force focus to button to update values
							oChild:SetFocus(TRUE)
							IF !SELF:__CommandFromEvent(oTempEvent)
								SELF:ButtonClick(oTempEvent)
							ELSE
								SELF:EventReturnValue := 1L
							ENDIF
						ELSE
							SELF:ButtonClick(oTempEvent)
						ENDIF


					CASE BN_DOUBLECLICKED
						SELF:ButtonDoubleClick(oTempEvent)
					OTHERWISE
						SELF:Default(oEvt)
					END SWITCH


				CASE oChild IS BaseListBox
					IF oChild IS ComboBox VAR combo
						SWITCH dwHiWord
						CASE CBN_DBLCLK
							SELF:ListBoxClick(ControlEvent{oEvt})
						CASE CBN_EDITCHANGE
							//PP-030923 Tell the combobox that an edit change occurred
							combo:__EditChange()
							SELF:EditChange(ControlEvent{oEvt})


						CASE CBN_KILLFOCUS
                        CASE CBN_SETFOCUS
							SELF:EditFocusChange(EditFocusChangeEvent{oEvt})
						CASE CBN_SELCHANGE
							SELF:ListBoxSelect(ControlEvent{oEvt})
						OTHERWISE
							SELF:Default(oEvt)
						END SWITCH
					ELSE
						SWITCH dwHiWord
						CASE LBN_DBLCLK
							SELF:ListBoxClick(ControlEvent{oEvt})
						CASE LBN_SELCHANGE
							SELF:ListBoxSelect(ControlEvent{oEvt})
						OTHERWISE
							SELF:Default(oEvt)
						END SWITCH
					ENDIF


				CASE (oChild IS Edit VAR oEdit .AND. !oEdit:__NoNotify) .OR. oChild IS IPAddress
					dwHiWord:=HiWord(oEvt:wParam)


					SWITCH dwHiWord
					CASE EN_CHANGE
						SELF:EditChange(ControlEvent{oEvt})
					CASE EN_HSCROLL
                    CASE EN_VSCROLL
						SELF:EditScroll(ControlEvent{oEvt})
					CASE EN_KILLFOCUS
                    CASE EN_SETFOCUS
						SELF:EditFocusChange(EditFocusChangeEvent{oEvt})
						// this is needed because the IPAddress control has nested edits, whose WM_SETFOCUS we don't get
						IF oChild IS IPAddress VAR ip
							SendMessage(ip:Handle(), IIF(dwHiWord==EN_SETFOCUS, WM_SETFOCUS, WM_KILLFOCUS), 0, 0)
						ENDIF
						//					RvdH 050816 Unreachable code
						//					CASE dwHiWord == EN_KILLFOCUS .or. dwHiWord == EN_SETFOCUS
						//						SELF:EditFocusChange(__ObjectCastClassPtr(oEvt, __pCEditFocusChangeEvent))
						//						// this is needed because the IPAddress control has nested edits, whose WM_SETFOCUS we don't get
						//						IF IsInstanceOf(oChild,#IPAddress) .and. IsInstanceOf(SELF, #DataWindow)
						//							Send(SELF, #__ControlFocusChange, oChild, FocusChangeEvent{hwnd, IIf(dwHiWord==EN_SETFOCUS, WM_SETFOCUS, WM_KILLFOCUS), 0, 0})
						//						ENDIF
					OTHERWISE
						SELF:Default(oEvt)
					END SWITCH


				CASE oChild IS ToolBar VAR oTb
                    oObject := oTb:Owner
                    local lPassObject as logic
                    lPassObject := oObject is Window
                    if oObject is ShellWindow var oShell .and. oShell:ChildToolBarLocation == TBL_SHELL
                        lPassObject := false
                    endif
					if lPassObject
						SELF:__PreMenuCommand(MenuCommandEvent{oEvt}:__SetMenu(oObject))
					ELSE
						SELF:__PreMenuCommand(MenuCommandEvent{oEvt}:__SetMenu(SELF))
					ENDIF


					RETURN SELF:EventReturnValue


					//PP-031115 ACN_START/ACN_STOP were in ControlNotify - should be here as WM_COMMAND
				CASE oChild IS AnimationControl
					oTempEvent := ControlEvent{oEvt}


					IF dwHiWord = ACN_START
						SELF:AnimationStart(oTempEvent)
					ELSEIF dwHiWord = ACN_STOP
						SELF:AnimationStop(oTempEvent)
					ENDIF


				OTHERWISE
					SELF:Default(oEvt)
				ENDCASE


				RETURN SELF:EventReturnValue
			ENDIF
		ELSEIF (dwLParam == 0) //Menu or Accel
			IF !lHelpOn .OR. !SELF:__HelpFilter(oEvt)
				SELF:EventReturnValue := 1L
				SELF:__PreMenuCommand(MenuCommandEvent{oEvt}:__SetMenu(SELF))
				SELF:EventReturnValue := 0
				RETURN SELF:EventReturnValue
			ENDIF
		ELSE
			SELF:Default(oEvt)
			RETURN SELF:EventReturnValue
		ENDIF


	CASE WM_SETCURSOR
		IF (oEvt:wParam) != (DWORD(_CAST, hWnd))
			lclient := FALSE
		ELSE
			lclient := TRUE
		ENDIF
		IF lHelpOn
			IF lhelpcursorOn
				lHelpEnable := TRUE
			ELSE
				lHelpEnable := FALSE
			ENDIF
		ELSE
			lHelpEnable := FALSE
		ENDIF
		SELF:__HandlePointer(oEvt, lHelpEnable, lclient)
		RETURN SELF:EventReturnValue


	CASE WM_DESTROY
		SetMenu(hWnd, 0)
		// SetAccelerator(Null_Ptr, Null_Ptr)
		// ReleaseCapture()
	   __WCSelfPtrFree(ptrSelfPtr)
		SetWindowLong(hwnd, DWL_USER, 0L)
		IF !InCollect()
			UnregisterAxit(SELF)
			ptrSelfPtr:=NULL_PTR
		ENDIF


		SELF:__ReleaseDC()
		/*WCDCDelete(self)
		ReleaseDC(hWnd,hDC)*/
		RETURN SELF:EventReturnValue


	CASE WM_INITMENU
    CASE WM_INITMENUPOPUP
		SELF:MenuInit(MenuInitEvent{oEvt})
		RETURN SELF:EventReturnValue


	CASE WM_KEYDOWN
    CASE WM_SYSKEYDOWN
		IF ! PeekMessage(@msg, hWnd, WM_CHAR, WM_CHAR, PM_NOREMOVE) //  == FALSE
			SELF:KeyDown(KeyEvent{oEvt})
			RETURN SELF:EventReturnValue
		ENDIF


	CASE WM_KEYUP
    CASE WM_SYSKEYUP
		SELF:KeyUp(KeyEvent{oEvt})
		SELF:EventReturnValue := 1
		RETURN SELF:EventReturnValue


	CASE WM_MENUSELECT
		IF (oEvt:lParam != 0)
			SELF:MenuSelect(MenuSelectEvent{oEvt})
			RETURN SELF:EventReturnValue
		ENDIF


	CASE WM_MOUSEMOVE
		//PP-040427 Issue 12909 Following was testing oEvt:wParam == MK_LBUTTON
		// which allows for the only left button press without ctrl/shift - wParam is different values if ctrl/shift pressed
		IF _AND(oEvt:wParam,MK_LBUTTON) > 0
			lFileDragging := TRUE
			SetCapture(SELF:Handle())
			IF IsMethod(SELF, #MouseDrag)
				SELF:MouseDrag(MouseEvent{oEvt})
			ELSEIF IsMethod(oParent, #MouseDrag)
				oParent:MouseDrag(MouseEvent{oEvt})
			ENDIF
		ELSE
			SELF:MouseMove(MouseEvent{oEvt})
		ENDIF


		RETURN SELF:EventReturnValue


	CASE WM_MOVE
		SELF:Move(MoveEvent{oEvt})
		RETURN SELF:EventReturnValue


	CASE WM_SIZE
		DCInitialized := FALSE
		SELF:Resize(ResizeEvent{oEvt})
		RETURN SELF:EventReturnValue


	CASE WM_CLOSE
		IF SELF:QueryClose(oEvt)
			SELF:__Close(oEvt)
		ENDIF
		RETURN SELF:EventReturnValue


	CASE TRAY_ICON_MSG
		//PP-030902
		SWITCH oEvt:lParam
		CASE WM_LBUTTONDOWN
			SELF:TrayIconClicked(oEvt:wParam, FALSE, FALSE)
		CASE WM_LBUTTONDBLCLK
			SELF:TrayIconClicked(oEvt:wParam, FALSE, TRUE)
		CASE WM_RBUTTONDOWN
			SELF:TrayIconClicked(oEvt:wParam, TRUE, FALSE)
		CASE WM_RBUTTONDBLCLK
			SELF:TrayIconClicked(oEvt:wParam, TRUE, TRUE)


		CASE NIN_BALLOONSHOW
			SELF:TrayIconBalloonShown(oEvt:wParam)


		CASE NIN_BALLOONTIMEOUT
			SELF:TrayIconBalloonTimeOut(oEvt:wParam)


		CASE NIN_BALLOONUSERCLICK
			SELF:TrayIconBalloonClicked(oEvt:wParam)


		END SWITCH


	CASE WM_MENUCHAR
		//PP-030319 owner draw support
		//Used for owner drawn menus or controls
		__Dispatch_MenuChar(oEvt, SELF)
		RETURN SELF:EventReturnValue


		//PP-040425 New improved context menu support, following replaced with subsequent code
		// 	CASE (uMsg == WM_CONTEXTMENU)
		// 		IF IsInstanceOf(SELF, #ShellWindow) .and. (oContextMenu != NULL_OBJECT)
		// 			oContextMenu:ShowAsPopup(SELF)
		// 		ENDIF
		// 		EventReturnValue := IIf(oContextMenu == NULL_OBJECT, 0, 1)
		// 		RETURN SELF:EventReturnValue
		//
	CASE WM_CONTEXTMENU
		//PP-040410 New improved context menu support
		oObject :=__WCGetObjectByHandle(PTR(_CAST,oEvt:wPARAM))
		IF oObject != NULL_OBJECT
			IF oObject:Contextmenu != NULL_OBJECT
				oObject:Contextmenu:ShowAsPopup(oObject, oEvt:lParam)
				RETURN (SELF:EventReturnValue := 1L)
			ENDIF
		ENDIF


	CASE WM_GETMINMAXINFO
		//PP-040410
		SELF:MinMaxInfo(MinMaxInfoEvent{oEvt})


    CASE WM_TIMER
         //SE-081122 Systemtimer event
         SELF:Timer(oEvt)
         RETURN SELF:EventReturnValue


    END SWITCH
    DO CASE
	CASE (uMsg == gdwDragListMsg)
		SELF:__HandleListItemDrag(oEvt)
		RETURN SELF:EventReturnValue


        // 081212 suggestion from Sven
    CASE uMsg == WM_SysCommand .AND. oEvt:wParam < 0x0000F000
        //SE-081122 SystemMenu call
        SELF:__PreMenuCommand(MenuCommandEvent{oEvt}:__SetMenu(SELF))
        RETURN SELF:EventReturnValue


    CASE uMsg >= WM_APP
         IF IsMethod(SELF, #AppMessage)
             Send(SELF, #AppMessage, oEvt)
             RETURN SELF:EventReturnValue
         ENDIF
	OTHERWISE
		IF IsMethod(SELF, #DispatchUnknown)
			Send(SELF, #DispatchUnknown, oEvt)
			RETURN SELF:EventReturnValue
		ENDIF


	ENDCASE


	SELF:Default(oEvt)


	RETURN SELF:EventReturnValue


END CLASS


 /// <exclude />
FUNCTION __Dispatch_DrawItem(oEvent AS @@event, oWindow AS OBJECT) AS LONGINT STRICT
	LOCAL struDrawItem AS _winDRAWITEMSTRUCT
	LOCAL oControl AS OBJECT


	struDrawItem := PTR(_CAST, oEvent:lParam)


	oControl :=__WCGetObjectByHandle(struDrawItem:hwnditem)
	IF oControl != NULL_OBJECT .AND. IsMethod(oControl, #ODDrawItem)
		Send(oControl, #ODDrawItem, oEvent)
		oWindow:EventReturnValue := 1L
		RETURN 1L
	ENDIF
	IF oWindow IS Window VAR oWin
		oWin:Default(oEvent)
	ENDIF
	RETURN 0L


 /// <exclude />
FUNCTION __Dispatch_MeasureItem(oEvent AS @@event, oWindow AS OBJECT) AS LONGINT STRICT
	LOCAL struMeasureItem AS _winMEASUREITEMSTRUCT
	LOCAL oControl AS OBJECT
	LOCAL hFromHwnd AS PTR


	IF oEvent:wParam = 0 //wParam includes the CtlID
		//message was send by a menu, the owner drwan menu must set
		//menu handle to struMeasureItem.ItemData
		struMeasureItem := PTR(_CAST, oEvent:lParam)
		hFromHwnd       := PTR(_CAST, struMeasureItem:ItemData)
	ELSE
		//message was send by a control, wParam includes the CtlID
		hFromHwnd := GetDlgItem(oEvent:hWnd, INT(_CAST,oEvent:wParam))
	ENDIF
	oControl :=__WCGetObjectByHandle (hFromHwnd)
	IF oControl != NULL_OBJECT .AND. IsMethod(oControl, #ODMeasureItem)
		Send(oControl, #ODMeasureItem, oEvent)
		oWindow:EventReturnValue := 1L
		RETURN 1L
	ENDIF
	IF oWindow IS Window VAR oWin
		oWin:Default(oEvent)
	ENDIF
	RETURN 0L


 /// <exclude />
FUNCTION __Dispatch_MenuChar(oEvent AS @@event, oWindow AS OBJECT) AS LONGINT STRICT
	//PP-040317 Issue 12743
	LOCAL oControl AS OBJECT
	LOCAL lRetVal AS LONGINT


	oControl :=__WCGetObjectByHandle(PTR(_CAST, oEvent:lParam))
	IF oControl != NULL_OBJECT .AND. IsMethod(oControl, #ODMenuChar)
		lRetVal := Send(oControl, #ODMenuChar, oEvent)
		IF lRetVal >= 0
			oWindow:EventReturnvalue := lRetVal
			RETURN 1L
		ENDIF
	ENDIF


	IF oWindow IS Window VAR oWin
		oWindow:Default(oEvent)
	ENDIF


	RETURN 0L


