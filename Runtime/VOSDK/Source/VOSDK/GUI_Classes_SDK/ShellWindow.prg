STATIC DEFINE __WCMdiFirstChildID := 0x8001

CLASS ShellWindow INHERIT AppWindow
	PROTECT hWndClient AS PTR
	PROTECT lOpened AS LOGIC
	PROTECT lUsingChildMenu AS LOGIC
	PROTECT oActualMenu AS Menu
	PROTECT oSavedTB AS ToolBar
	PROTECT iChildTBLoc AS INT
	
	//PP-030828 Strong typing
	ACCESS __ActualMenu AS Menu STRICT 
	//PP-030828 Strong typing
	
	
	RETURN oActualMenu
	

METHOD __AdjustClient() AS LOGIC STRICT 
	//PP-030828 Strong typing
	LOCAL strucClientRect IS _winRect
	LOCAL oToolBar AS ToolBar
	LOCAL oStatusBar AS StatusBar
	LOCAL lToolBarValid := TRUE AS LOGIC
	LOCAL lStatusBarValid := TRUE AS LOGIC
	
	
	
	oToolBar := SELF:ToolBar
	oStatusBar := SELF:StatusBar
	
	IF (oToolBar == NULL_OBJECT)
		lToolBarValid := FALSE
	ELSEIF !oToolBar:IsVisible()
		lToolBarValid := FALSE
	ENDIF
	
	IF (oStatusBar == NULL_OBJECT)
		lStatusBarValid := FALSE
	ELSEIF !oStatusBar:IsVisible()
		lStatusBarValid := FALSE
	ENDIF
	
	//if !lToolBarValid .and. !lStatusBarValid
	// return false
	//endif
	
	GetClientRect(SELF:Handle(), @strucClientRect)
	
	IF lStatusBarValid
		IF oStatusBar:__IsTopAligned
			strucClientRect:top += oStatusBar:Size:Height
		ELSE
			strucClientRect:bottom -= oStatusBar:Size:Height
		ENDIF
	ENDIF
	
	IF lToolBarValid
		IF oToolBar:__IsTopAligned
			strucClientRect:top += oToolBar:Size:Height
		ELSE
			strucClientRect:bottom -= oToolBar:Size:Height
		ENDIF
	ENDIF
	
	MoveWindow(SELF:Handle(4), strucClientRect:left, strucClientRect:top, ;
		strucClientRect:right - strucClientRect:left, ;
		strucClientRect:bottom - strucClientRect:top, TRUE)
	
	RETURN TRUE
	

METHOD __AssociateAccel(lSwitch AS LOGIC) AS Window STRICT 
	//PP-030828 Strong typing
	LOCAL hWndChild AS PTR
	LOCAL oWindow AS OBJECT
	
	
	
	hWndChild := PTR(_CAST, SendMessage(hWndClient, WM_MDIGETACTIVE, 0, 0))
	IF (hWndChild != NULL_PTR)
		oWindow := __WCGetWindowByHandle(hWndChild)
		IF (oWindow != NULL_OBJECT)
			Send(oWindow, #__AssociateAccel, lSwitch)
			RETURN SELF
		ENDIF
	ENDIF
	
	RETURN SUPER:__AssociateAccel(lSwitch)
	

METHOD __RemoveChildToolBar() AS ToolBar STRICT 
	//PP-030828 Strong typing
	
	
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
	
	

METHOD __SetChildToolBar(oChildToolbar AS ToolBar) AS ToolBar STRICT 
	//PP-030828 Strong typing
	LOCAL oTB AS ToolBar
	
	
	
	IF oChildToolbar != NULL_OBJECT  //SE-050926
		
		IF (iChildTBLoc == TBL_SHELL)
			
			IF (oToolBar != oChildToolBar)
				oTB := oToolBar
				
				oToolbar := oChildToolBar
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
	

METHOD __UseChildMenu(oMenu AS Menu) AS ShellWindow STRICT 
	//PP-030828 Strong typing
	//PP-040927 Update. S.Ebert
	LOCAL hMenu AS PTR
	LOCAL liMenuPos AS LONGINT
	LOCAL oShellMenu AS Menu
	
	IF (oMenu != NULL_OBJECT)
		lUsingChildMenu := TRUE
		hMenu := oMenu:Handle()
		IF (oMenu != oActualMenu)
			SendMessage(hwndClient, WM_MDISETMENU, DWORD(_CAST, hMenu), LONGINT(_CAST, GetSubMenu(hMenu, oMenu:GetAutoUpdate())))
		ENDIF
		DrawMenuBar(hWnd)
		oActualMenu := oMenu
		IF (iChildTBLoc != TBL_CHILD)
			SELF:__SetChildToolBar(oActualMenu:ToolBar)
		ENDIF
	ELSEIF lUsingChildMenu
		lUsingChildMenu := FALSE
		oShellMenu := SELF:Menu
		IF (oShellMenu != NULL_OBJECT)
			liMenuPos := oShellMenu:GetAutoUpdate()
			hMenu := oShellMenu:Handle()
			oActualMenu := oShellMenu
			SendMessage(hwndClient, WM_MDISETMENU, DWORD(_CAST, hMenu), LONGINT(_CAST, GetSubMenu(hMenu,liMenuPos)))
		ELSE
			oActualMenu := NULL_OBJECT
			SetMenu(hWnd, 0)
		ENDIF
		DrawMenuBar(hWnd)
		IF (iChildTBLoc != TBL_CHILD)
			SELF:__RemoveChildToolBar()
		ENDIF
	ENDIF
	
	SendMessage(hwndClient, WM_MDIREFRESHMENU, 0, 0)
	RETURN SELF
	

METHOD Arrange(kArrangeStyle) 
	LOCAL liStyle AS LONGINT
	
	
	IF IsNil(kArrangeStyle)
		liStyle := ArrangeCascade
	ELSE
		IF !IsLong(kArrangeStyle)
			WCError{#Arrange,#ShellWindow,__WCSTypeError,kArrangeStyle,1}:@@Throw()
		ENDIF
		liStyle := kArrangeStyle
	ENDIF
	
	DO CASE
	CASE liStyle == ARRANGEASICONS
		SendMessage(hWndClient,WM_MDIICONARRANGE, 0, 0)
	CASE liStyle == ARRANGECASCADE
		SendMessage(hWndClient,WM_MDICASCADE, MDITILE_SKIPDISABLED, 0)
	CASE liStyle == ARRANGETILEVERTICAL
		SendMessage(hWndClient,WM_MDITILE, MDITILE_SKIPDISABLED, 0)
	CASE liStyle == ARRANGETILEHORIZONTAL
		SendMessage(hWndClient,WM_MDITILE, _OR(MDITILE_SKIPDISABLED,MDITILE_HORIZONTAL), 0)
	ENDCASE
	
	RETURN SELF
	
	

ACCESS ChildToolBarLocation 
	RETURN iChildTBLoc
	

ASSIGN ChildToolBarLocation(iNewLoc) 
	RETURN (iChildTBLoc := iNewLoc)
	

METHOD CloseAllChildren() 
	LOCAL hChild, hNextChild AS PTR
	
	
	
	hChild := GetWindow(SELF:Handle(4), GW_CHILD)
	
	WHILE (hChild != NULL_PTR)
		hNextChild := GetWindow(hChild, GW_HWNDNEXT)
		SendMessage(hChild, WM_CLOSE, 0, 0)
		hChild := hNextChild
	END
	
	RETURN NIL
	

METHOD Default(oEvent) 
	LOCAL oEvt := oEvent AS @@Event
	
	
	
	SELF:EventReturnValue := DefFrameProc(oEvt:hWnd, hwndClient, oEvt:uMsg, oEvt:wParam, oEvt:lParam)
	
	RETURN SELF
	

METHOD Destroy() 
	
	
	// was at the end !!!
	SUPER:Destroy()
	
	// Tests if this is the last TopAppWindow
	IF (oApp != NULL_OBJECT)
		oApp:__WindowCount := oApp:__WindowCount - 1
	ENDIF
	IF (oApp:__WindowCount == 0)
		PostQuitMessage(0)
	ENDIF
	
	RETURN SELF
	

METHOD Dispatch(oEvent) 
	//PP-040601 S.Ebert (WM_CONTEXTMENU)
	LOCAL oEvt := oEvent AS @@Event
	LOCAL uMsg AS DWORD
	LOCAL wParam AS DWORD
	LOCAL wParamLow AS DWORD
	LOCAL lParam AS LONGINT
	LOCAL lRetVal AS LOGIC
	LOCAL lclient AS LOGIC
	LOCAL lHelpEnable AS LOGIC
	LOCAL hWndChild AS PTR
	LOCAL hMDIChild AS PTR
	LOCAL oDocApp AS Window
	
	
	
	uMsg := oEvt:uMsg
	
	DO CASE
	CASE (uMsg == WM_MENUSELECT) .OR.;
			(uMsg == WM_INITMENU) .OR.;
			(uMsg == WM_INITMENUPOPUP)
		
		hWndChild := PTR(_CAST, SendMessage(hWndClient, WM_MDIGetActive, 0, 0))
		SendMessage(hWndChild, oEvt:uMsg, oEvt:wParam, oEvt:lParam)
		
	CASE uMsg == WM_COMMAND
		// If the WM_COMMAND is not a help message (id < 0xFFFD)
		// and not a WINDOW menu option (id >= MDI_FIRSTCHILDID)
		// process it.
		
		wParam := oEvt:wParam
		wParamLow := LoWord(wParam)
		
		IF (wParamLow >= __WCMdiFirstChildID) //.and. wParamLow<ID_FirstSystemID
			IF (wParamLow < ID_FIRSTWCHELPID)
				SELF:Default(oEvt)
				RETURN SELF:EventReturnValue
			ENDIF
		ENDIF
		
		lParam := oEvt:lParam
		IF (HiWord(wParam) <= 1) //accelerator or menu
			IF !lHelpOn .OR. !SELF:__HelpFilter(oEvt)
				IF (wParamLow >= 0x0000F000)
					SELF:Default(oEvt)
					RETURN SELF:EventReturnValue
				ENDIF
				
				hWndChild := PTR(_CAST,SendMessage(hWndClient,WM_MDIGetActive,0,0))
				
				IF (hWndChild != 0)
					SendMessage(hWndChild, WM_COMMAND, wParam, lParam)
					RETURN SELF:EventReturnValue //Assume child handled menu
				ENDIF
			ELSE
				SUPER:Dispatch(oEvt)
				RETURN SELF:EventReturnValue
			ENDIF
		ENDIF
		
	CASE (uMsg == WM_CREATE)
		// If this is a top app window
		// If not opened
		// Tag it open
		// Increment the window count
		IF !lOpened
			lOpened := TRUE
			IF (oApp != NULL_OBJECT)
				oApp:__WindowCount += 1
			ENDIF
		ENDIF
		
	CASE (uMsg == WM_SETCURSOR)
		IF (oEvt:wParam) != (DWORD(_CAST, hwndClient))
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
		#warning There was a typo there ("lHelpCursorOn" instead of "lHelpEnable"), right?
//		SELF:__HandlePointer(oEvt, lHelpCursorOn, lclient)
		SELF:__HandlePointer(oEvt, lHelpEnable, lclient)
		RETURN SELF:EventReturnValue
		
	CASE (uMsg == WM_WCHELP)
		SELF:__EnableHelpCursor(FALSE)
		//SELF:HelpRequest(__ObjectCastClassPtr(oEvt, __pCHelpRequestEvent))
		SELF:HelpRequest(HelpRequestEvent{oEvt})
		RETURN SELF:EventReturnValue
		
		// case uMsg == WM_NCLBUTTONDBLCLK
		
		/* if (CV_RunTime :: Event_wParam ( e ) == HTMENU )
		{
		dword dwActive=SendMessage(hwndClient,WM_MDIGETACTIVE, 0, 0);
			
		// if it is maximized, look if the click is inside the system menu
		
		if (HiWord(dwActive))
		{
		RECT rc;
			// use windows SDK functions for rectangle to avoid all
		// tricks with vertical coordinates
		
		::GetWindowRect(hEContext,&rc);
			
		// Calculate system menu rectangle
		
		rc.left+=::GetSystemMetrics(SM_CXBORDER);
			rc.top+=(::GetSystemMetrics(SM_CYBORDER)+::GetSystemMetrics(SM_CYCAPTION));
			rc.right=rc.left+::GetSystemMetrics(SM_CXSIZE);
			rc.bottom=rc.top+::GetSystemMetrics(SM_CYSIZE);
			
		if (::PtInRect(&rc,MAKEPOINT(CV_RunTime :: Event_lParam( e ))))
		SendMessage(LoWord(dwActive),WM_SYSCOMMAND,SC_CLOSE,CV_RunTime :: Event_lParam( e ));
			}
		}
		Default(e);
			break;
			*/
	CASE (uMsg == WM_QUERYENDSESSION) .OR. (uMsg == WM_CLOSE)
		IF (GetWindow(hWndClient,GW_Child) != 0)
#ifdef __VULCAN__
         LOCAL WCQueryCloseEnumFuncDelegate AS __WCQueryCloseEnumFuncDelegate
         WCQueryCloseEnumFuncDelegate := __WCQueryCloseEnumFuncDelegate{ NULL, @__WCQueryCloseEnumFunc() }
			lRetVal := EnumChildWindows( hWndClient, System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) WCQueryCloseEnumFuncDelegate ), 0 )
         GC.KeepAlive( WCQueryCloseEnumFuncDelegate ) 
#else		
			lRetVal := EnumChildWindows(hWndClient, @__WCQueryCloseEnumFunc(), 0)
#endif			
			IF !lRetVal
				RETURN 0L
			ENDIF
		ENDIF
		
	CASE (uMsg == WM_SIZE)
		SUPER:Dispatch(oEvt)
		IF SELF:__AdjustClient()
			IF IsClass(#OleObject)
				hMDIChild := PTR(_CAST, SendMessage(SELF:Handle(4), WM_MDIGETACTIVE, 0, 0L))
				IF (hMDIChild != NULL_PTR)
					oDocApp := __WCGetWindowByHandle(hMDIChild)
					IF (IsInstanceOf(oDocApp, #__DocApp)) .AND.;
							(IsInstanceOf(oDocApp:owner, #DataWindow))
						//PP-040915 Issue 12961
						IF IsMethod(oDocApp:Owner, #__UpdateActiveObject)
							Send(oDocApp:Owner, #__UpdateActiveObject)
						ENDIF
					ENDIF
				ENDIF
			ENDIF
			RETURN 0L
		ENDIF
		RETURN SELF:EventReturnValue
		
	CASE oEvt:uMsg == WM_CONTEXTMENU
		//In a Shellwindow this message becomes created in the MDIClient Window
		//and __WCGetWindowByHandle() returns a Null_Object in this case, so
		//shell contextmenu is not shown.
		IF oEvt:wPARAM = DWORD(_CAST, hWndClient)
			oEvt:wPARAM := DWORD(_CAST, hWnd)
		ENDIF
		
	ENDCASE
	
	SUPER:Dispatch(oEvt)
	
	RETURN SELF:EventReturnValue
	

METHOD EnableOleStatusMessages(lEnable) 
	// RvdH 030815 Moved method from Ole classes
#ifdef __VULCAN__
   // TODO
#else	
	LOCAL pRegister AS  _VOOLERegisterStatusCallback  PTR
	pRegister := GetProcAddress(GetModuleHandle(String2Psz("VO28ORUN.DLL")), String2Psz("_VOOLERegisterStatusCallback"))
	IF pRegister != NULL_PTR
		IF IsNil(lEnable)
			lEnable := TRUE
		ENDIF
		
		IF (lEnable)
			PCALL(pRegister, @__OleStatusCallback())
		ELSE
			PCALL(pRegister, NULL_PTR)
		ENDIF
		RETURN lEnable
	ENDIF
#endif	
	RETURN FALSE

METHOD GetActiveChild() 
	LOCAL hActive AS PTR
	LOCAL oActive AS Window
	
	
	
	hActive := PTR(_CAST, SendMessage(SELF:Handle(4), WM_MDIGETACTIVE, 0, 0L))
	oActive := __WCGetWindowByHandle(hActive)
	
	IF IsInstanceOf(oActive, #__DocApp)
		oActive := oActive:Owner
	ENDIF
	
	RETURN oActive
	

METHOD Handle(nHandleType) AS PTR
	
	
	IF !IsNil(nHandleType)
		IF !IsLong(nHandleType)
			WCError{#Handle,#ShellWindow,__WCSTypeError,nHandleType,1}:@@Throw()
		ENDIF
		IF (nHandleType == 4)
			RETURN hwndClient
		ENDIF
	ENDIF
	
	RETURN hWnd
	

METHOD HelpRequest(oHelpRequestEvent) 
	LOCAL cHelpContext AS STRING
	
	
	IF IsInstanceOfUsual(oHelpRequestEvent, #HelpRequestEvent) ;
			.AND. SELF:HelpDisplay!=NULL_OBJECT;
			.AND. oHelpRequestEvent:Helptype == HELPCONTROL
		IF oHelpRequestEvent:ItemID == 3244
			IF SELF:Hyperlabel!=NULL_OBJECT ;
					.AND. !((cHelpContext:=SELF:Hyperlabel:HelpContext) == NULL_STRING)
				SELF:HelpDisplay:Show(cHelpContext)
			ELSE
				SELF:HelpDisplay:Show("Window_WindowCanvas")
			ENDIF
		ENDIF
	ELSE
		SUPER:HelpRequest(oHelpRequestEvent)
	ENDIF
	
	RETURN NIL
	

CONSTRUCTOR(oOwner) 
	LOCAL strucClientCreate IS _WinClientCreateStruct
	
	
	
	SUPER(oOwner)
	
	IF __WCRegisterShellWindow(_GetInst())
		hWnd := CreateWindowEx (0, PSZ(_CAST, __WCShellWindowClass), PSZ(_CAST," "),;
			_OR(WS_OverlappedWindow,WS_CLIPCHILDREN),;
			CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, 0, 0, _GetInst(), ptrSelfPtr)
		
		strucClientCreate:idFirstChild := __WCMdiFirstChildID
		
		hwndClient := CreateWindowEx(WS_EX_CLIENTEDGE, PSZ(_CAST,"MDICLIENT"), NULL_PTR,;
			_OR(WS_CHILD,WS_CLIPCHILDREN,WS_CLIPSIBLINGS,MDIS_AllChildStyles),;
			0, 0, 0, 0, hWnd, 0x2000, _GetInst(), @strucClientCreate)
		
		IF (oApp != NULL_OBJECT)
			oApp:SetMdiClientWindow(hwndClient)
		ENDIF
		
		ShowWindow(hwndClient, SW_SHOW)
		
		SendMessage(hwndClient, WM_MDICASCADE, 0, 0)
		
		//self:EnableSystemMenu()
		//self:EnableBorder()
		//self:EnableMinBox()
		//self:EnableMaxBox()
	ENDIF
	
	SELF:EnableStatusBar(TRUE)
	
	RETURN 
	

ASSIGN Menu(oNewMenu) 
	LOCAL oRetMenu AS Menu
	LOCAL hMenu AS PTR
	
	
	
	oRetMenu := (SUPER:Menu := oNewMenu)
	
	IF oRetMenu != NULL_OBJECT
		hMenu := oRetMenu:Handle()
		SendMessage(hwndClient, WM_MDISETMENU, 0, LONGINT(_CAST, GetSubMenu(hMenu, oRetMenu:GetAutoUpdate())) )
		SendMessage(hwndClient, WM_MDIREFRESHMENU, 0, 0)
	ENDIF
	
	RETURN 
	
	

METHOD OnOleStatusMessage(cMsgString) 
	// RvdH 030815 Moved method from Ole classes
	IF SELF:StatusBar != NULL_OBJECT
		SELF:StatusBar:MenuText := cMsgString
	ENDIF
	RETURN SELF
	

METHOD Resize(oResizeEvent) 
	LOCAL oResEvt := oResizeEvent AS ResizeEvent
	
	
	
	IF IsInstanceOf(SELF:ToolBar, #ToolBar)
		SendMessage(SELF:ToolBar:Handle(), WM_SIZE, oResEvt:wParam, oResEvt:lParam)
	ENDIF
	
	RETURN NIL
	

ASSIGN StatusBar(oNewBar) 
    SUPER:StatusBar := oNewBar
	SELF:__AdjustClient()
	RETURN 
	

ASSIGN ToolBar(oNewToolBar) 
	oSavedTB := oNewToolBar
	
	RETURN (SUPER:ToolBar := oNewToolBar)
	

METHOD ToolBarHeightChanged(oControlNotifyEvent) 
	
	
	SUPER:ToolBarHeightChanged(oControlNotifyEvent)
	
	SELF:__AdjustClient()
	RETURN SELF
END CLASS

STATIC FUNCTION __WCRegisterShellWindow(hInst AS PTR) AS LOGIC
	STATIC LOCAL lretVal AS LOGIC
	LOCAL wc IS _WINWNDCLASS
	
	//PP-031129 From S Ebert. Changed wc.hbrBackground
	//Otherwise under XP and theme suport on, you see a toolbar,
	//which is not a client of a rebar with a white background.
	//The reason for this is the transparency flag of toolbar which
	//is needed for XP. So you see the background color of the owner window.
	
	IF !lretVal
		// 2.5c. REDRAW removed - lead to flicker!
		wc:style := CS_DBLCLKS //, CS_HREDRAW, CS_VREDRAW) //, CS_CLASSDC)
#ifdef __VULCAN__
      STATIC LOCAL WCShellWndProcDelegate AS __WCShellWndProcDelegate
      IF WCShellWndProcDelegate == NULL
         WCShellWndProcDelegate := __WCShellWndProcDelegate{ NULL, @__WCShellWndProc() }
      ENDIF
		wc:lpfnWndProc := System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) WCShellWndProcDelegate )
#else		
		wc:lpfnWndProc := PTR(_CAST, @__WCShellWndProc())
#endif		
		wc:hInstance := hInst
		wc:hIcon := LoadIcon(0, IDI_APPLICATION)
		wc:hCursor := LoadCursor(0, IDC_ARROW)
		wc:hbrBackground := (COLOR_3DFACE+1) //(COLOR_WINDOW + 1)
		wc:lpszClassName := PSZ(_CAST,__WCShellWindowClass)
		wc:cbWndExtra := 12
		
		lretVal := (RegisterClass(@wc)!=0)
	ENDIF
	
	RETURN lretVal
	
STATIC FUNCTION _VOOLERegisterStatusCallback(pCallBackFunc AS PTR) AS LOGIC STRICT
	RETURN FALSE
	
#ifdef __VULCAN__	
   DELEGATE __WCQueryCloseEnumFuncDelegate( hWnd AS PTR, lParam AS LONGINT ) AS LONGINT
#endif
	
FUNCTION __WCQueryCloseEnumFunc(hWnd AS PTR, lParam AS LONGINT) AS LONGINT /* WINCALL */
	RETURN SendMessage(hWnd, WM_QueryEndSession, 0, 0)
	
#ifdef __VULCAN__
   DELEGATE __WCShellWndProcDelegate( hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT ) AS LONGINT
#endif	
	
FUNCTION __WCShellWndProc(hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LONGINT /* WINCALL */
	LOCAL oWindow AS Window
	LOCAL strucCreateStruct AS _WinCreateStruct
	
	
	IF (uMsg == WM_CREATE)
		strucCreateStruct:=PTR(_CAST,lParam)
		SetWindowLong(hWnd, DWL_User, LONGINT(_CAST,strucCreateStruct:lpCreateParams))
	ENDIF
	
	oWindow := __WCGetWindowByHandle(hWnd)
	IF (oWindow != NULL_OBJECT)
		RETURN oWindow:Dispatch(@@Event{hWnd, uMsg, wParam, lParam, oWindow})
	ENDIF
	RETURN DefFrameProc(hWnd, NULL_PTR, uMsg, wParam, lParam)



#region defines
DEFINE TBL_CHILD := 0
DEFINE TBL_SHELL := 1
DEFINE TBL_SHELLBAND := 2
DEFINE __WCShellWindowClass := "ShellWindow"
#endregion
