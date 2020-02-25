CLASS __DocApp INHERIT AppWindow
	PROTECT oShellWin AS ShellWindow

METHOD __AssociateAccel(lSwitch AS LOGIC) AS Window STRICT 
	//PP-030828 Strong typing
	

	IF lSwitch .AND. (oParent:Accelerator != NULL_OBJECT)
		//SetAccelerator(hWnd, oParent:Accelerator:Handle())
		//the menu belong to the shell, otherwise Accels of disabled items are still working
		SetAccelerator(oShellWin:Handle(), oParent:Accelerator:Handle())
	ELSEIF (oShellWin:Accelerator != NULL_OBJECT)
		SetAccelerator(oShellWin:Handle(), oShellWin:Accelerator:Handle())
	ELSE
		SetAccelerator(NULL_PTR, NULL_PTR)
	ENDIF

	RETURN SELF

METHOD __DestroyWnd() AS LOGIC STRICT 
	//PP-030828 Strong typing
	

	IF (hwnd != NULL_PTR)
		SendMessage(oShellWin:Handle(4), WM_MDIDESTROY, DWORD(_CAST, hWnd), 0)
	ENDIF
	RETURN TRUE

METHOD Default(oEvent) 
	LOCAL oEvt := oEvent AS @@Event

	

	oParent:EventReturnValue := DefMDIChildProc(oEvt:hWnd, oEvt:uMsg, oEvt:wParam, oEvt:lParam)

	RETURN SELF

METHOD Dispatch(oEvent) 
	LOCAL oEvt := oEvent AS @@Event
	LOCAL uMsg AS DWORD

	

	oParent:EventReturnValue := 0L
	uMsg := oEvt:uMsg

	IF (oParent != NULL_OBJECT)
		oEvt:oWindow := oParent
	ENDIF

	SWITCH uMsg
	CASE WM_MDIACTIVATE
		IF PTR(_CAST, oEvt:lParam) == hWnd // if activate
			IF hWnd == PTR(_CAST, SendMessage(oShellWin:Handle(4), WM_MDIGETACTIVE, 0, 0))
				oShellWin:__UseChildMenu(oParent:menu)
			ENDIF
			SELF:__AssociateAccel(TRUE)
			oParent:Activate(oEvt)
		ELSE // else deactivate
			IF (oEvt:lParam == 0)
				oShellWin:__UseChildMenu(NULL_OBJECT)
				SELF:__AssociateAccel(FALSE)
			ENDIF
			oParent:Deactivate(oEvt)
			RETURN oParent:EventReturnValue
		ENDIF

	CASE WM_SIZE
		IF (oStatusBar != NULL_OBJECT)
			SendMessage(oStatusBar:Handle(), oEvt:uMsg, oEvt:wParam, oEvt:lParam)
			oStatusBar:__BuildItems()
		ENDIF
		//oParent:Resize(__ObjectCastClassPtr(oEvt, __pCResizeEvent))
		oParent:Resize(ResizeEvent{oEvt})
		RETURN oParent:EventReturnValue

	CASE WM_QUERYENDSESSION
		oParent:EventReturnValue := oParent:QueryClose(oEvt)
		RETURN oParent:EventReturnValue

	CASE WM_CREATE
		hWnd := oEvt:hWnd
		SELF:Owner:__Imp := SELF
		SELF:Owner:SetHandle(hWnd)
		oParent:Dispatch(oEvt)
		RETURN oParent:EventReturnValue

	OTHERWISE
		oParent:Dispatch(oEvt)
		RETURN oParent:EventReturnValue
	END SWITCH

	SELF:Default(oEvt)
	RETURN oParent:EventReturnValue

CONSTRUCTOR(oChildApp) 
	

	SUPER(oChildApp)
	oShellWin := oChildApp:Owner

	IF __WCRegisterDocAppWindow(_GetInst())
		hwnd := CreateMDIWindow(String2Psz(__WCDocAppWindowClass), NULL_PSZ,;
			_OR(WS_CHILD,WS_CLIPSIBLINGS,WS_SYSMENU,WS_CLIPCHILDREN,WS_CAPTION,WS_THICKFRAME),;
			CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, ;
			PTR(_CAST,oShellWin:Handle(4)), _GetInst(), LONGINT(_CAST, ptrSelfPtr))
	ENDIF

	RETURN 

ASSIGN Menu(oNewMenu) 
	

	IF IsWindowVisible(hWnd)
		oShellWin:__UseChildMenu(oNewMenu)
	ENDIF

	RETURN 

METHOD Show(kShowState) 
	LOCAL hCurrent AS PTR

	

	SUPER:Show(kShowState)
	hCurrent := PTR(_CAST, SendMessage(oShellWin:Handle(4), DWORD(WM_MDIGETACTIVE), 0, 0))

	IF (hCurrent == hWnd) .AND. !IsWindowVisible(hWnd) .AND. (SELF:Menu != NULL_OBJECT)
		// Show after a hide
		oShellWin:__UseChildMenu(SELF:Menu)
	ENDIF

	SendMessage(oShellWin:Handle(4), WM_MDIACTIVATE, DWORD(_CAST, hWnd), 0 )
	RETURN SELF

ACCESS ToolBar 
	IF (oShellWin:ChildToolBarLocation == TBL_CHILD)
		RETURN SUPER:Toolbar
	ENDIF
	RETURN oShellWin:ToolBar

ASSIGN ToolBar(oNewToolBar) 

	IF (oShellWin:ChildToolBarLocation == TBL_CHILD)
		RETURN SUPER:Toolbar := oNewToolBar
	ENDIF
	RETURN 

END CLASS

STATIC FUNCTION __WCRegisterDocAppWindow(hInst AS PTR) AS LOGIC
	STATIC LOCAL lretVal AS LOGIC
	LOCAL wc IS _WINWNDCLASS

	//PP-031129 From S Ebert. Changed wc.hbrBackground
	//Otherwise under XP and theme suport on, you see a toolbar,
	//which is not a client of a rebar with a white background.
	//The reason for this is the transparency flag of toolbar which
	//is needed for XP. So you see the background color of the owner window.
	IF !lretVal
		// 2.5c REDRAW_FLAGS removed - lead to flicker !!!
		wc:style := CS_DBLCLKS //, CS_HREDRAW, CS_VREDRAW, // CS_CLASSDC)
#ifdef __VULCAN__
      STATIC LOCAL WCDocAppWndProcDelegate AS __WCDocAppWndProcDelegate
      IF WCDocAppWndProcDelegate == NULL
         WCDocAppWndProcDelegate := __WCDocAppWndProcDelegate{ NULL, @__WCDocAppWndProc() }
      ENDIF
		wc:lpfnWndProc := System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) WCDocAppWndProcDelegate )
#else		
		wc:lpfnWndProc := PTR(_CAST, @__WCDocAppWndProc())
#endif		
		wc:hInstance := hInst
		wc:hIcon := LoadIcon(0, IDI_APPLICATION)
		wc:hCursor := LoadCursor(0, IDC_Arrow)
		wc:hbrBackground := (COLOR_3DFACE+1) //(COLOR_WINDOW + 1)
		wc:lpszClassName := PSZ(_CAST, __WCDocAppWindowClass)
		wc:cbWndExtra := 12

		lretVal := (RegisterClass(@wc)!=0)
	ENDIF

	RETURN lretVal

#ifdef __VULCAN__
   DELEGATE __WCDocAppWndProcDelegate( hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT ) AS LONGINT
#endif

FUNCTION __WCDocAppWndProc(hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LONGINT /* WINCALL */
	LOCAL oWindow AS Window
	LOCAL strucCreateStruct AS _WinCreateStruct
	LOCAL strucMDICreateStruct AS _WinMDICreateStruct


	IF uMsg == WM_Create
		strucCreateStruct := PTR(_CAST, lParam)
		strucMDICreateStruct := PTR(_CAST, strucCreateStruct:lpCreateParams)
		SetWindowLong(hWnd, DWL_User, LONGINT(_CAST,strucMDICreateStruct:lParam))
	ENDIF

	oWindow := __WCGetWindowByHandle(hWnd) 

	IF (oWindow != NULL_OBJECT)
		RETURN oWindow:Dispatch(@@Event{ hwnd, uMsg, wParam, lParam, oWindow}     )
	ENDIF
	RETURN DefMDIChildProc(hWnd, uMsg, wParam, lParam)



#region defines
DEFINE __WCDocAppWindowClass := "DocAppWindow"
#endregion
