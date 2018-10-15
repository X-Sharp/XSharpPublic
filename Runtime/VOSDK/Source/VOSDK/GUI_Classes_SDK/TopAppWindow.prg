PARTIAL CLASS TopAppWindow INHERIT AppWindow
	//PP-030828 Strong typing
	METHOD __ResizeChild() AS TopAppWindow STRICT 
	//PP-030828 Strong typing
	//PP-290404 Update S.Ebert
	LOCAL hChild AS PTR
	LOCAL rect IS _WINRECT
	LOCAL xPoint AS INT
	LOCAL yPoint AS INT
	LOCAL DimX AS LONGINT
	LOCAL DimY AS LONGINT
	LOCAL oTB AS ToolBar
	LOCAL oSB AS StatusBar
	LOCAL oChild AS OBJECT

	

	hchild := GetWindow(hWnd, GW_CHILD)

	oTB := SELF:ToolBar
	oSB := SELF:StatusBar

	DO WHILE hChild != Null_Ptr
		oChild :=__WCGetObjectByHandle(hChild)
		//IF oChild != NULL_OBJECT
			//EXIT
		//ENDIF
		//oChild := __WCGetControlByHandle(hChild)
		IF oChild = NULL_OBJECT .or. (oChild != oTB .and. oChild != oSB)
			EXIT
		ENDIF
		hchild := GetWindow(hchild, GW_HWNDNEXT)
	ENDDO

	IF oChild != NULL_OBJECT
		GetClientRect(hWnd, @rect)

		DimX := rect:right
		DimY := rect:bottom

		xPoint := 0
		yPoint := 0

		IF (oTB != NULL_OBJECT)
			yPoint := oTB:Size:Height
			DimY -= yPoint
		ENDIF

		IF (oSB != NULL_OBJECT)
			DimY -= oSB:Size:Height
		ENDIF

		MoveWindow(hChild, xPoint, yPoint, DimX, DimY, TRUE)
	ENDIF

	RETURN SELF

CONSTRUCTOR(oOwner) 
	

	SUPER(oOwner)

	IF __WCRegisterTopAppWindow(_GetInst())
		hWnd := CreateWindowEx(0, String2Psz( __WCTopAppWindowClass), String2Psz(" "),;
			_Or(_Or(WS_CLIPCHILDREN, WS_OVERLAPPED),WS_THICKFRAME),;
			CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,;
			0, 0, _GetInst(), ptrSelfPtr)
	ENDIF
	SELF:EnableSystemMenu()
	SELF:EnableBorder()
	SELF:EnableMinBox()
	SELF:EnableMaxBox()

	RETURN 

METHOD Resize(oResizeEvent) 

   SUPER:Resize(oResizeEvent)
   SELF:__ResizeChild()
   RETURN SELF

METHOD ToolBarHeightChanged() 
   SELF:__ResizeChild()
   RETURN SELF
END CLASS

STATIC FUNCTION __WCRegisterTopAppWindow(hInst AS PTR) AS LOGIC
	STATIC LOCAL lretVal AS LOGIC
	LOCAL wc IS _WINWNDCLASS

	IF !lretVal
		// 2.5c REDRAW flags removed - leas to flicker
		wc:style := CS_DBLCLKS //,CS_HREDRAW, CS_VREDRAW) //, CS_CLASSDC)
#ifdef __VULCAN__
      STATIC LOCAL WCTopAppWndProcDelegate AS __WCTopAppWndProcDelegate
      IF WCTopAppWndProcDelegate == NULL
         WCTopAppWndProcDelegate := __WCTopAppWndProcDelegate{ NULL, @__WCTopAppWndProc() }
      ENDIF
		wc:lpfnWndProc := System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) WCTopAppWndProcDelegate )
#else		
		wc:lpfnWndProc := PTR(_CAST, @__WCTopAppWndProc())
#endif		
		wc:hInstance := hInst
		wc:hIcon := LoadIcon(0, IDI_APPLICATION)
		wc:hCursor := LoadCursor(0, IDC_ARROW)
		wc:hbrBackground := (COLOR_APPWORKSPACE /*COLOR_WINDOW*/ + 1)
		wc:lpszClassName := PSZ(_CAST,__WCTopAppWindowClass)
		wc:cbWndExtra := 12

		lretVal := (RegisterClass(@wc) != 0)
	ENDIF

	RETURN lretVal
	
#ifdef __VULCAN__
   DELEGATE __WCTopAppWndProcDelegate( hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT ) AS LONGINT
#endif	

FUNCTION __WCTopAppWndProc(hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LONGINT /* WINCALL */
	LOCAL oWindow AS Window
	LOCAL strucCreateStruct AS _WinCreateStruct


	IF uMsg == WM_DESTROY
		PostQuitMessage(0)
		RETURN 0
	ELSE
		IF uMsg == WM_CREATE
			strucCreateStruct:=PTR(_CAST,lParam)
			SetWindowLong(hWnd, DWL_User, LONGINT(_CAST, strucCreateStruct:lpCreateParams))
		ENDIF
		oWindow := __WCGetWindowByHandle(hWnd)

		IF oWindow != NULL_OBJECT
			RETURN oWindow:Dispatch(@@Event{hWnd, uMsg, wParam, lParam, oWindow})
		ELSE
			RETURN DefWindowProc(hWnd, uMsg, wParam, lParam)
		ENDIF
	ENDIF



#region defines
DEFINE __WCTopAppWindowClass := "TopAppWindow"
#endregion
