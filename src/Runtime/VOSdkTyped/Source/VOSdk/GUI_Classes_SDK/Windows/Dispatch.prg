//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



PARTIAL CLASS Window

/// <include file="Gui.xml" path="doc/Window.Dispatch/*" />
	METHOD Dispatch(oEvent AS @@Event)
		RETURN SELF:EventReturnValue

	//LOCAL msg IS _winMSG
	//LOCAL uMsg AS DWORD
	//LOCAL dwHiWord AS DWORD
	//LOCAL dwLParam AS DWORD
	//LOCAL oTempEvent AS OBJECT
	//LOCAL oChild AS OBJECT
	//LOCAL oScrollBar AS OBJECT
	//LOCAL ptrBuf AS PTR
	//LOCAL cBuf AS STRING
	//LOCAL strucPS IS _winPAINTSTRUCT
	//LOCAL _hdc AS PTR
	//LOCAL _Handle AS PTR
 //   LOCAL oControl AS Control
	//LOCAL hBr AS PTR
	//LOCAL lHelpEnable AS LOGIC
	//LOCAL lclient AS LOGIC
	//LOCAL oObject AS OBJECT
	//LOCAL oEvt AS @@event
	//LOCAL strHelpInfo AS _winHELPINFO
	//LOCAL oMenuHL AS HyperLabel
	//LOCAL wp AS DWORD


	//oEvt := oEvent
	//SELF:EventReturnValue := 0L
	//uMsg := oEvt:uMsg
	//wp   := oEvt:wParam




	//CASE (uMsg == WM_WCHELP)
	//	SELF:__EnableHelpCursor(FALSE)
	//	SELF:HelpRequest(HelpRequestEvent{oEvt})
	//	RETURN SELF:EventReturnValue

	//CASE (uMsg == WM_HELP)
	//	//SE-060522 S.Ebert
	//	IF ! lHelpOn
	//		SELF:HelpRequest(HelpRequestEvent{oEvt})
	//		IF SELF:EventReturnValue = 1l
	//			RETURN 1l
	//		ENDIF
	//	ENDIF
	//	strHelpInfo := PTR(_CAST, oEvt:lParam)
	//	IF strHelpInfo:iContextType = HELPINFO_MENUITEM .and. SELF:Menu != NULL_OBJECT
	//		oMenuHL := SELF:Menu:HyperLabel(strHelpInfo:iCtrlId)
	//		IF (oMenuHL != NULL_OBJECT) .and. !Empty(oMenuHL:HelpContext) .and. (SELF:HelpDisplay != NULL_OBJECT)
	//			SELF:HelpDisplay:Show(oMenuHL:HelpContext)
	//		ENDIF
	//		RETURN SELF:EventReturnValue
	//	ENDIF

	//CASE (uMsg == WM_ACTIVATE)
	//	IF LoWord(oEvt:wParam) != WA_INACTIVE    //FdW//20061202 // LoWord() added
	//		IF lHelpOn .and. (oApp != NULL_OBJECT)
	//			IF lHelpCursorOn
	//				oApp:__SetHelpWind(hWnd,HM_MOUSE)
	//			ELSE
	//				oApp:__SetHelpWind(hWnd,HM_GENERAL)
	//			ENDIF
	//		ENDIF
	//		SELF:__AssociateAccel(TRUE)
	//		SELF:Activate(oEvt)
	//	ELSE
	//		SELF:__AssociateAccel(FALSE)
	//		SELF:DeActivate(oEvt)
	//	ENDIF
	//	RETURN SELF:EventReturnValue


	//CASE (uMsg == WM_LBUTTONDOWN) .or. (uMsg == WM_RBUTTONDOWN) .or. (uMsg == WM_MBUTTONDOWN) .or. ;
	//		uMsg == WM_XBUTTONDOWN
	//	//PP-030904 Xbutton
	//	SELF:MouseButtonDown(MouseEvent{oEvt})
	//	RETURN SELF:EventReturnValue

	//CASE (uMsg == WM_HSCROLL) .or. (uMsg == WM_VSCROLL)
	//	dwLParam := DWORD(_CAST,oEvt:lParam)
	//	IF dwLParam != 0
	//		ptrBuf := MemAlloc(50)
	//		GetClassName(dwLParam,ptrBuf,50)
	//		cBuf:=Psz2String(ptrBuf)
	//		MemFree(ptrBuf)
	//	ENDIF
	//	IF (cBuf == UpDown_Class)
	//		oTempEvent := SpinnerEvent{oEvt}
	//		IF (uMsg == WM_HSCROLL)
	//			SELF:HorizontalSpin(oTempEvent)
	//		ELSE
	//			SELF:VerticalSpin(oTempEvent)
	//		ENDIF
	//	ELSEIF (cBuf == TrackBar_Class)
	//		oTempEvent := SliderEvent{oEvt}
	//		IF (uMsg == WM_HSCROLL)
	//			SELF:HorizontalSlide(oTempEvent)
	//		ELSE
	//			SELF:VerticalSlide(oTempEvent)
	//		ENDIF
	//	ELSE
	//		oTempEvent := ScrollEvent{oEvt}
	//		oScrollBar := oTempEvent:ScrollBar

	//		//if (sbi .and. CV_RunTime::ScrollBar_GetIgnore(sbi)
	//		// .and. (CV_RunTime::Event_wParam(e) == SB_ENDSCROLL))
	//		// CV_RunTime::ScrollBar_SetIgnore(sbi, 0)
	//		// ((pDW)pRequestor) -> _Default(e)
	//		//endif

	//		IF (uMsg == WM_HSCROLL)
	//			SELF:HorizontalScroll(oTempEvent)
	//		ELSE
	//			SELF:VerticalScroll(oTempEvent)
	//		ENDIF

	//		// Required for Windows 3.x onwards
	//		//if (sbi .and. CV_RunTime ::Event_wParam(e) == SB_THUMBPOSITION)
	//		// CV_RunTime::ScrollBar_SetIgnore(sbi, 1)
	//		//endif
	//	ENDIF
	//	RETURN SELF:EventReturnValue

	//CASE (uMsg == WM_COMMAND)

	//	dwLParam := DWORD(_CAST, oEvt:lParam)

	//	IF (dwLParam != 0)
	//		IF lHelpCursorOn
	//			SELF:__EnableHelpCursor(FALSE)
	//		ELSE
	//			oChild :=__WCGetObjectByHandle(dwLParam)
	//			// temp hack for ReBar/ToolBar
	//			IF (oChild == NULL_OBJECT)
	//				oChild :=__WCGetObjectByHandle(GetParent(PTR(_CAST, oEvt:lParam)))
	//			ENDIF
	//			dwHiWord := HiWord(oEvt:wParam)

	//				CASE (dwHiWord == BN_DOUBLECLICKED)
	//					SELF:ButtonDoubleClick(oTempEvent)
	//				OTHERWISE
	//					SELF:Default(oEvt)
	//				ENDCASE

	//			CASE IsInstanceOf(oChild, #BaseListBox)
	//				IF IsInstanceOf(oChild, #ComboBox)
	//					DO CASE
	//					CASE dwHiWord == CBN_DBLCLK
	//						SELF:ListBoxClick(ControlEvent{oEvt})
	//					CASE dwHiWord == CBN_EDITCHANGE
	//						//PP-030923 Tell the combobox that an edit change occurred
	//						oChild:__EditChange()
	//						SELF:EditChange(ControlEvent{oEvt})

	//					CASE dwHiWord == CBN_KILLFOCUS .or.;
	//							dwHiWord == CBN_SETFOCUS
	//						SELF:EditFocusChange(EditFocusChangeEvent{oEvt})
	//					CASE dwHiWord == CBN_SELCHANGE
	//						SELF:ListBoxSelect(ControlEvent{oEvt})
	//					OTHERWISE
	//						SELF:Default(oEvt)
	//					ENDCASE
	//				ELSE
	//					DO CASE
	//					CASE dwHiWord == LBN_DBLCLK
	//						SELF:ListBoxClick(ControlEvent{oEvt})
	//					CASE dwHiWord == LBN_SELCHANGE
	//						SELF:ListBoxSelect(ControlEvent{oEvt})
	//					OTHERWISE
	//						SELF:Default(oEvt)
	//					ENDCASE
	//				ENDIF

	//			CASE (IsInstanceOf(oChild,#Edit) .and. !oChild:__NoNotify) .or. IsInstanceOf(oChild,#IPAddress)
	//				dwHiWord:=HiWord(oEvt:wParam)

	//				DO CASE
	//				CASE dwHiWord == EN_CHANGE
	//					SELF:EditChange(ControlEvent{oEvt})
	//				CASE dwHiWord == EN_HSCROLL .or.;
	//						dwHiWord == EN_VSCROLL
	//					SELF:EditScroll(ControlEvent{oEvt})
	//				CASE dwHiWord == EN_KILLFOCUS .or. dwHiWord == EN_SETFOCUS
	//					SELF:EditFocusChange(EditFocusChangeEvent{oEvt})
	//					// this is needed because the IPAddress control has nested edits, whose WM_SETFOCUS we don't get
	//					IF IsInstanceOf(oChild,#IPAddress)
	//						SendMessage(oChild:Handle(), IIf(dwHiWord==EN_SETFOCUS, WM_SETFOCUS, WM_KILLFOCUS), 0, 0)
	//					ENDIF
	//				OTHERWISE
	//					SELF:Default(oEvt)
	//				ENDCASE

	//			CASE IsInstanceOf(oChild, #ToolBar)
	//				oObject := oChild:Owner
	//				IF IsInstanceOfUsual(oChild:Owner, #Window) .and. !(IsInstanceOfUsual(oObject, #ShellWindow) .and. (IVarGet(oObject, #ChildToolbarLocation) == TBL_SHELL))
	//					SELF:__PreMenuCommand(MenuCommandEvent{oEvt}:__SetMenu(oObject))
	//				ELSE
	//					SELF:__PreMenuCommand(MenuCommandEvent{oEvt}:__SetMenu(SELF))
	//				ENDIF

	//				RETURN SELF:EventReturnValue

	//				//PP-031115 ACN_START/ACN_STOP were in ControlNotify - should be here as WM_COMMAND
	//			CASE IsInstanceOf(oChild, #AnimationControl)
	//				oTempEvent := ControlEvent{oEvt}

	//				IF dwHiWord = ACN_START
	//					SELF:AnimationStart(oTempEvent)
	//				ELSEIF dwHiWord = ACN_STOP
	//					SELF:AnimationStop(oTempEvent)
	//				ENDIF

	//			OTHERWISE
	//				SELF:Default(oEvt)
	//			ENDCASE

	//			RETURN SELF:EventReturnValue
	//		ENDIF
	//	ELSEIF (dwLParam == 0) //Menu or Accel
	//		IF !lHelpOn .or. !SELF:__HelpFilter(oEvt)
	//			SELF:EventReturnValue := 1L
	//			SELF:__PreMenuCommand(MenuCommandEvent{oEvt}:__SetMenu(SELF))
	//			SELF:EventReturnValue := 0
	//			RETURN SELF:EventReturnValue
	//		ENDIF
	//	ELSE
	//		SELF:Default(oEvt)
	//		RETURN SELF:EventReturnValue
	//	ENDIF

	//CASE (uMsg == WM_SETCURSOR)
	//	IF (oEvt:wParam) != (DWORD(_CAST, hWnd))
	//		lclient := FALSE
	//	ELSE
	//		lclient := TRUE
	//	ENDIF
	//	IF lHelpOn
	//		IF lhelpcursorOn
	//			lHelpEnable := TRUE
	//		ELSE
	//			lHelpEnable := FALSE
	//		ENDIF
	//	ELSE
	//		lHelpEnable := FALSE
	//	ENDIF
	//	SELF:__HandlePointer(oEvt, lHelpEnable, lclient)
	//	RETURN SELF:EventReturnValue

	//CASE (uMsg == WM_DESTROY)
	//	SetMenu(hWnd, 0)
	//	// SetAccelerator(Null_Ptr, Null_Ptr)
	//	// ReleaseCapture()
	//   __WCSelfPtrFree(ptrSelfPtr)
	//		SetWindowLong(hwnd, DWL_USER, 0L)
	//	IF !InCollect()
	//		UnRegisterAxit(SELF)
	//		ptrSelfPtr:=Null_Ptr
	//	ENDIF

	//	SELF:__ReleaseDC()
	//	/*WCDCDelete(self)
	//	ReleaseDC(hWnd,hDC)*/
	//	RETURN SELF:EventReturnValue

	//CASE (uMsg == WM_INITMENU) .or. (uMsg == WM_INITMENUPOPUP)
	//	SELF:MenuInit(MenuInitEvent{oEvt})
	//	RETURN SELF:EventReturnValue

	//CASE (uMsg == WM_KEYDOWN) .or. (uMsg == WM_SYSKEYDOWN)
	//	IF ! PeekMessage(@msg, hWnd, WM_CHAR, WM_CHAR, PM_NOREMOVE) //  == FALSE
	//		SELF:KeyDown(KeyEvent{oEvt})
	//		RETURN SELF:EventReturnValue
	//	ENDIF

	//CASE (uMsg == WM_KEYUP) .or. (uMsg == WM_SYSKEYUP)
	//	SELF:KeyUp(KeyEvent{oEvt})
	//	SELF:EventReturnValue := 1
	//	RETURN SELF:EventReturnValue

	//CASE (uMsg == WM_MENUSELECT)
	//	IF (oEvt:lParam != 0)
	//		SELF:MenuSelect(MenuSelectEvent{oEvt})
	//		RETURN SELF:EventReturnValue
	//	ENDIF

	//CASE (uMsg == WM_MOUSEMOVE)
	//	//PP-040427 Issue 12909 Following was testing oEvt:wParam == MK_LBUTTON
	//	// which allows for the only left button press without ctrl/shift - wParam is different values if ctrl/shift pressed
	//	IF _and(oEvt:wParam,MK_LBUTTON) > 0
	//		lFileDragging := TRUE
	//		SetCapture(SELF:Handle())
	//		IF IsMethod(SELF, #MouseDrag)
	//			SELF:MouseDrag(MouseEvent{oEvt})
	//		ELSEIF IsMethod(oParent, #MouseDrag)
	//			oParent:MouseDrag(MouseEvent{oEvt})
	//		ENDIF
	//	ELSE
	//		SELF:MouseMove(MouseEvent{oEvt})
	//	ENDIF

	//	RETURN SELF:EventReturnValue

	//CASE (uMsg == WM_MOVE)
	//	SELF:Move(MoveEvent{oEvt})
	//	RETURN SELF:EventReturnValue

	//CASE (uMsg == WM_SIZE)
	//	DCInitialized := FALSE
	//	SELF:Resize(ResizeEvent{oEvt})
	//	RETURN SELF:EventReturnValue

	//CASE (uMsg == WM_CLOSE)
	//	IF SELF:QueryClose(oEvt)
	//		SELF:__Close(oEvt)
	//	ENDIF
	//	RETURN SELF:EventReturnValue

	//CASE (uMsg == gdwDragListMsg)
	//	SELF:__HandleListItemDrag(oEvt)
	//	RETURN SELF:EventReturnValue


	//CASE uMsg == WM_MENUCHAR
	//	//PP-030319 owner draw support
	//	//Used for owner drawn menus or controls
	//	__Dispatch_MenuChar(oEvt, SELF)
	//	RETURN SELF:EventReturnValue

	//	//PP-040425 New improved context menu support, following replaced with subsequent code
	//	// 	CASE (uMsg == WM_CONTEXTMENU)
	//	// 		IF IsInstanceOf(SELF, #ShellWindow) .and. (oContextMenu != NULL_OBJECT)
	//	// 			oContextMenu:ShowAsPopup(SELF)
	//	// 		ENDIF
	//	// 		EventReturnValue := IIf(oContextMenu == NULL_OBJECT, 0, 1)
	//	// 		RETURN SELF:EventReturnValue
	//	//
	//CASE (uMsg == WM_CONTEXTMENU)
	//	//PP-040410 New improved context menu support
	//	oObject :=__WCGetObjectByHandle(PTR(_CAST,oEvt:wPARAM))
	//	IF oObject != NULL_OBJECT
	//		IF oObject:Contextmenu != NULL_OBJECT
	//			oObject:Contextmenu:ShowAsPopup(oObject, oEvt:lParam)
	//			RETURN (SELF:EventReturnValue := 1l)
	//		ENDIF
	//	ENDIF


 //       // 081212 suggestion from Sven
 //   CASE uMsg == WM_SysCommand .AND. oEvt:wParam < 0x0000F000
 //       //SE-081122 SystemMenu call
 //       SELF:__PreMenuCommand(MenuCommandEvent{oEvt}:__SetMenu(SELF))
 //       RETURN SELF:EventReturnValue

 //   CASE uMsg == WM_TIMER
 //        //SE-081122 Systemtimer event
 //        SELF:Timer(oEvt)
 //        RETURN SELF:EventReturnValue

 //   CASE uMsg >= WM_APP
 //        IF IsMethod(SELF, #AppMessage)
 //            Send(SELF, #AppMessage, oEvt)
 //            RETURN SELF:EventReturnValue
 //        ENDIF
	//OTHERWISE
	//	IF IsMethod(SELF, #DispatchUnknown)
	//		Send(SELF, #DispatchUnknown, oEvt)
	//		RETURN SELF:EventReturnValue
	//	ENDIF

	//ENDCASE


	//SELF:Default(oEvt)

	//RETURN SELF:EventReturnValue

END CLASS


