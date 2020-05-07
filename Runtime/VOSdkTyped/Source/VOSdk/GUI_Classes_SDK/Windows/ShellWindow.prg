

STATIC DEFINE __WCMdiFirstChildID := 0x8001
USING SWF := System.Windows.Forms

CLASS ShellWindow INHERIT AppWindow
	PROTECT oWndClient	AS SWF.MdiClient
	PROTECT lOpened		AS LOGIC
	PROTECT lUsingChildMenu AS LOGIC
	PROTECT oActualMenu	AS XSharp.VO.Menu
	PROTECT oSavedTB	AS XSharp.VO.ToolBar
	PROTECT iChildTBLoc AS INT
	//PRIVATE oNMDI AS NativeMDIclient
	
	METHOD enableScrollBars(enable AS LOGIC) AS VOID
	//SELF:oNMDI:hideScrollbars := !enable
	RETURN

	ACCESS __IsClientValid AS LOGIC STRICT
		RETURN SELF:oWndClient != NULL_OBJECT .and. ! SELF:oWndClient:IsDisposed


	METHOD __CreateForm() AS VOForm STRICT
		LOCAL oShell AS VOShellForm
		oShell := GuiFactory.Instance:CreateShellWindow(SELF)
		SELF:lOpened := TRUE
		IF oApp != NULL_OBJECT
			oApp:__WindowCount += 1
		ENDIF
		oShell:WindowState := System.Windows.Forms.FormWindowState.Maximized
		FOREACH oCtrl AS System.Windows.Forms.Control IN oShell:Controls
			IF oCtrl:GetType() == typeof(System.Windows.Forms.MdiClient)
				SELF:oWndClient := (System.Windows.Forms.MdiClient) oCtrl
				//oNMDI := NativeMDIclient{(System.Windows.Forms.MdiClient)oCtrl}
			ENDIF
		NEXT
		RETURN oShell

	ACCESS __Shell AS VOShellForm
		RETURN (VOShellForm) __Form
	
	ACCESS __ActualMenu AS XSharp.VO.Menu STRICT 
		//PP-030828 Strong typing
		RETURN oActualMenu
	
	[Obsolete];
	METHOD __AdjustClient() AS LOGIC STRICT 
		RETURN TRUE
	

	//[Obsolete];
	//METHOD __AssociateAccel(lSwitch AS LOGIC) AS Window STRICT 
		// Todo
		//LOCAL oWndChild AS PTR
		//LOCAL oWindow AS OBJECT
		
		
		
		//oWndChild := PTR(_CAST, SendMessage(oWndClient, WM_MDIGETACTIVE, 0, 0))
		//IF (oWndChild != NULL_PTR)
		//	oWindow := __WCGetWindowByHandle(oWndChild)
		//	IF (oWindow != NULL_OBJECT)
		//		Send(oWindow, #__AssociateAccel, lSwitch)
		//		RETURN SELF
		//	ENDIF
		//ENDIF
		
		//RETURN SUPER:__AssociateAccel(lSwitch)
	

	METHOD __RemoveChildToolBar() AS XSharp.VO.ToolBar STRICT 
		
		IF (iChildTBLoc == TBL_SHELL)
			IF (oSavedTB != NULL_OBJECT)
				SELF:ToolBar := oSavedTB
			ENDIF
		ELSEIF (iChildTBLoc == TBL_SHELLBAND)
			// if (oSavedTB != NULL_OBJECT)
			// SendMessage(oToolBar:Handle(), RB_DELETEBAND, 1, 0)
			//endif
		ENDIF
		
		RETURN oToolBar
	
	

	METHOD __SetChildToolBar(oChildToolbar AS XSharp.VO.ToolBar) AS XSharp.VO.ToolBar STRICT 
		LOCAL oTB AS XSharp.VO.ToolBar
		IF oChildToolbar != NULL_OBJECT  //SE-050926
			
			IF (iChildTBLoc == TBL_SHELL)
				
				IF (oToolBar != oChildToolbar)
					oTB := oToolBar
					
					oToolBar := oChildToolbar
					oToolBar:__SetParent(SELF)
					oToolBar:Show()
					IF (oTB != NULL_OBJECT)
						oTB:Hide()
					ENDIF
				ENDIF
			ELSEIF (iChildTBLoc == TBL_SHELLBAND)
				// TBD
				// oChildToolBar:__SetParent(oToolBar)
				// oChildToolBar:Show()
				// oToolBar:AddBand(#__CHILDTBBAND, oChildToolBar, 1, 50, 25)
			ENDIF
			
		ENDIF
		
		RETURN oToolBar
	
	
	METHOD Arrange(liStyle := ArrangeCascade AS LONG) 
		IF SELF:__IsValid
			DO CASE
			CASE liStyle == ARRANGEASICONS
				oWnd:LayoutMdi(SWF.MdiLayout.ArrangeIcons)
			CASE liStyle == ARRANGECASCADE
				oWnd:LayoutMdi(SWF.MdiLayout.Cascade)
			CASE liStyle == ARRANGETILEVERTICAL
				oWnd:LayoutMdi(SWF.MdiLayout.TileVertical)
			CASE liStyle == ARRANGETILEHORIZONTAL
				oWnd:LayoutMdi(SWF.MdiLayout.TileHorizontal)
			ENDCASE
		ENDIF	
		RETURN SELF
	

	ACCESS ChildToolBarLocation AS LONG
		RETURN iChildTBLoc

	ASSIGN ChildToolBarLocation(iNewLoc AS LONG) 
		iChildTBLoc := iNewLoc
	

	METHOD CloseAllChildren() 
		LOCAL aForms AS System.Windows.Forms.Form[]
		IF SELF:__IsValid
			aForms := oWnd:MdiChildren
			FOREACH IMPLIED oForm IN aForms
				oForm:Close()
			NEXT
		ENDIF
		RETURN NIL
	
	

	METHOD Destroy() AS USUAL CLIPPER
		// was at the end !!!
		SUPER:Destroy()
		
		// Tests if this is the last TopAppWindow
		IF (oApp != NULL_OBJECT)
			oApp:__WindowCount := oApp:__WindowCount - 1
			IF (oApp:__WindowCount <= 0)
				oApp:Quit()
			ENDIF
		ENDIF
		
		
		RETURN SELF
	
	//METHOD Dispatch(oEvent) 
	//	//PP-040601 S.Ebert (WM_CONTEXTMENU)
	//	LOCAL oEvt := oEvent AS @@Event
	//	LOCAL uMsg AS DWORD
	//	LOCAL wParam AS DWORD
	//	LOCAL wParamLow AS DWORD
	//	LOCAL lParam AS LONGINT
	//	LOCAL lRetVal AS LOGIC
	//	LOCAL lclient AS LOGIC
	//	LOCAL lHelpEnable AS LOGIC
	//	LOCAL oWndChild AS PTR
	//	LOCAL hMDIChild AS PTR
	//	LOCAL oDocApp AS Window
	
	
	
	//	uMsg := oEvt:uMsg
	
	//	DO CASE
	//		CASE (uMsg == WM_MENUSELECT) .OR.;
	//		(uMsg == WM_INITMENU) .OR.;
	//		(uMsg == WM_INITMENUPOPUP)
	
	//			oWndChild := PTR(_CAST, SendMessage(oWndClient, WM_MDIGetActive, 0, 0))
	//		SendMessage(oWndChild, oEvt:uMsg, oEvt:wParam, oEvt:lParam)
	
	//	CASE uMsg == WM_COMMAND
	//		// If the WM_COMMAND is not a help message (id < 0xFFFD)
	//		// and not a WINDOW menu option (id >= MDI_FIRSTCHILDID)
	//		// process it.
	
	//		wParam := oEvt:wParam
	//		wParamLow := LoWord(wParam)
	
	//		IF (wParamLow >= __WCMdiFirstChildID) //.and. wParamLow<ID_FirstSystemID
	//			IF (wParamLow < ID_FIRSTWCHELPID)
	//				SELF:Default(oEvt)
	//				RETURN SELF:EventReturnValue
	//			ENDIF
	//		ENDIF
	
	//		lParam := oEvt:lParam
	//		IF (HiWord(wParam) <= 1) //accelerator or menu
	//			IF !lHelpOn .OR. !SELF:__HelpFilter(oEvt)
	//				IF (wParamLow >= 0x0000F000)
	//					SELF:Default(oEvt)
	//					RETURN SELF:EventReturnValue
	//				ENDIF
	
	//				oWndChild := PTR(_CAST,SendMessage(oWndClient,WM_MDIGetActive,0,0))
	
	//				IF (oWndChild != 0)
	//					SendMessage(oWndChild, WM_COMMAND, wParam, lParam)
	//					RETURN SELF:EventReturnValue //Assume child handled menu
	//				ENDIF
	//			ELSE
	//				SUPER:Dispatch(oEvt)
	//				RETURN SELF:EventReturnValue
	//			ENDIF
	//		ENDIF
	
	
	//	CASE (uMsg == WM_SETCURSOR)
	//		IF (oEvt:wParam) != (DWORD(_CAST, oWndClient:Handle))
	//			lclient := FALSE
	//		ELSE
	//			lclient := TRUE
	//		ENDIF
	//		IF lHelpOn
	//			IF lhelpcursorOn
	//				lHelpEnable := TRUE
	//			ELSE
	//				lHelpEnable := FALSE
	//			ENDIF
	//		ELSE
	//			lHelpEnable := FALSE
	//		ENDIF
	//		SELF:__HandlePointer(oEvt, lHelpCursorOn, lclient)
	//		RETURN SELF:EventReturnValue
	
	//	CASE (uMsg == WM_WCHELP)
	//		SELF:__EnableHelpCursor(FALSE)
	//		//SELF:HelpRequest(__ObjectCastClassPtr(oEvt, __pCHelpRequestEvent))
	//		SELF:HelpRequest(HelpRequestEvent{oEvt})
	//		RETURN SELF:EventReturnValue
	

	//	ENDCASE
	
	//	SUPER:Dispatch(oEvt)
	
	//	RETURN SELF:EventReturnValue

	METHOD EnableOleStatusMessages(lEnable) 
		// Also Empty in the GUI Classes
		RETURN FALSE

	METHOD GetActiveChild() AS Window Strict
		LOCAL oActive AS Window
		LOCAL oForm AS System.Windows.Forms.Form
		LOCAL oVewaForm AS VOAppForm
		IF SELF:__IsValid
			IF oWnd:ActiveMdiChild != NULL_OBJECT 
				oForm := oWnd:ActiveMdiChild
				IF oForm is VOAppForm
					oVewaForm := (VOAppForm) oForm
					oActive := oVewaForm:Window
				ENDIF
			ENDIF
		ENDIF		
		RETURN oActive
	
	METHOD SetActiveChild(oForm AS VOAppForm) AS VOID STRICT
		LOCAL oMdi AS System.Windows.Forms.MdiClient
		oMdi := SELF:oWndClient
		IF oMdi:Controls:Contains(oForm)
			oMdi:Controls:SetChildIndex(oForm,0)
		ENDIF

	METHOD GetPreviousChildForm(oChild AS VOForm) AS VOForm STRICT
		LOCAL oMdi AS System.Windows.Forms.MdiClient
		LOCAL i, nstartindex, nCount AS INT
		IF SELF:__IsClientValid
			oMdi := SELF:oWndClient
			IF oMdi:Controls:Contains(oChild)
				// Letztes Form ist auf Position 0, deswegen Addition statt Subtraktion
				nstartindex := oMdi:Controls:IndexOf(oChild)
				nCount := oMdi:Controls:Count
				IF nCount > 1
					// Modulo-Ring, endet wenn gültiges Form gefunden (Return in Schleife) oder Startindex erreicht
					i := (nstartindex + 1) % nCount
					DO WHILE i!=nstartindex
						IF oMdi:Controls[i] IS VoForm VAR oRet
							IF oRet:Visible
								RETURN oRet
							ENDIF
						ENDIF
						i := (i + 1) % nCount
					ENDDO
				ENDIF
			ENDIF
		ENDIF
		RETURN NULL_OBJECT



	METHOD Handle(nWhich) AS IntPtr  CLIPPER
		IF IsNumeric(nWhich) .and. nWhich == 4 .and. SELF:__IsClientValid
			RETURN SELF:oWndClient:Handle
		ELSE
			RETURN SUPER:Handle()
		ENDIF			
	

	//METHOD HelpRequest(oHelpRequestEvent) 
	//	SUPER:HelpRequest(oHelpRequestEvent)
	//	RETURN NIL
	
	CONSTRUCTOR(oOwner) 
		LOCAL oScreen AS System.Windows.Forms.Screen
		SUPER(oOwner)
		SELF:EnableStatusBar(TRUE)
		oScreen := System.Windows.Forms.Screen.PrimaryScreen
		SELF:Size := Dimension{oScreen:WorkingArea:Width/2, oScreen:WorkingArea:Height/2}
		RETURN 
	

	ASSIGN Menu(oNewMenu  AS XSharp.VO.Menu) 
		LOCAL nAuto as LONG
		SUPER:Menu := oNewMenu
		nAuto := oNewMenu:GetAutoUpdate()
		IF nAuto >= 0 .and. nAuto < oNewMenu:__Menu:MenuItems:Count
			oNewMenu:__Menu:MenuItems[nAuto]:MdiList := TRUE
		ENDIF
		
		RETURN 
	

	METHOD OnOleStatusMessage(cMsgString) 
		IF SELF:StatusBar != NULL_OBJECT
			SELF:StatusBar:MenuText := cMsgString
		ENDIF
		RETURN SELF
	

	METHOD Resize(oResizeEvent) 
		// Toolbar is resized automatically		
		RETURN SUPER:Resize(oResizeEvent)
	

	ASSIGN StatusBar(oNewBar As StatusBar) 
		SUPER:StatusBar := oNewBar
		RETURN 
	
	ASSIGN ToolBar(oNewToolBar As XSharp.VO.ToolBar) 
		oSavedTB := oNewToolBar
		SUPER:ToolBar := oNewToolBar
	
END CLASS



#region defines
DEFINE TBL_CHILD := 0
DEFINE TBL_SHELL := 1
DEFINE TBL_SHELLBAND := 2
DEFINE __WCShellWindowClass := "ShellWindow"
#endregion
