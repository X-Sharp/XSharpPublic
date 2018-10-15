PARTIAL CLASS __SplitView INHERIT Control
	PROTECT	dwDeferPaintCount	AS DWORD
	PROTECT	oPanes			 	AS Dimension
	PROTECT	oBackgroundBrush	AS Brush
	PROTECT	oBarBrush		 	AS Brush
	PROTECT	oBarFrameBrush		AS Brush

ACCESS Background 
	
	RETURN oBackgroundBrush

ASSIGN Background(oBrush) 
	

	SELF:ChangeBackground(oBrush, SPLTCOLOR_WINDOW)
	RETURN 

METHOD ChangeBackground(oBrush, kWhere) 
	LOCAL dwNewColor	AS DWORD

	

	IF !IsInstanceOfUsual(oBrush, #Brush)
		WCError{#ChangeBackground, #__SplitView, __WCSTypeError, oBrush, 1}:@@Throw()
	ENDIF
	IF !IsNil(kWhere)
		IF !IsLong(kWhere)
			WCError{#ChangeBackground, #__SplitView, __WCSTypeError, kWhere, 2}:@@Throw()
		ENDIF
	ENDIF

	// change the particular color using the supplied brush,
	// or the appropriate system color if the brush is null
	DO CASE
	CASE kWhere == SPLTCOLOR_WINDOW
		oBackgroundBrush := oBrush
		IF oBackgroundBrush == NULL_OBJECT
			dwNewColor := GetSysColor(COLOR_APPWORKSPACE)
		ENDIF

	CASE kWhere == SPLTCOLOR_BAR
		oBarBrush := oBrush
		IF oBarBrush == NULL_OBJECT
			IF _And(GetVersion(), 0x80000000) != 0
				// Use Windows95 constants
				dwNewColor := GetSysColor(COLOR_3DFACE)
			ELSE
				// Use WindowsNT constants
				dwNewColor := GetSysColor(COLOR_BTNFACE)
			ENDIF
		ENDIF

	CASE kWhere == SPLTCOLOR_BARFRAME
		oBarFrameBrush := oBrush
		IF oBarFrameBrush == NULL_OBJECT
			IF _And(GetVersion(), 0x80000000) != 0
				// Use Windows95 constants
				dwNewColor := GetSysColor(COLOR_3DFACE)
			ELSE
				// Use WindowsNT constants
				dwNewColor := GetSysColor(COLOR_BTNFACE)
			ENDIF
		ENDIF
	END CASE

	IF oBrush != NULL_OBJECT
		dwNewColor := __WCGetBrushColor(oBrush)
	ENDIF
	PCALL(gpfnSpltColorSet, SELF:Handle(), kWhere, INT(_CAST, dwNewColor))

	RETURN SELF

METHOD Create() 
	//PP-031129 Flicker removal
	LOCAL hOwner	 AS PTR
	LOCAL oDevPoint AS Point
	LOCAL hInst AS PTR

	

	hInst := _GetInst()

	IF (hWnd == NULL_PTR)
		IF (WCGetCoordinateSystem() == WCCartesianCoordinates)
			oOrigin:Y := oOrigin:Y + SELF:Size:Height
		ENDIF
		oDevPoint:=__WCConvertPoint(oFormSurface, Point{oOrigin:X,oOrigin:Y})
		hOwner := oFormSurface:Handle()

		// remove the WS_EX_CLIENTEDGE style
		SELF:SetExStyle(WS_EX_CLIENTEDGE, FALSE)

		hWnd := CreateWindowEx(dwExStyle,;
			String2Psz(SELF:__ClassName), String2Psz(cWindowName),;
			dwStyle, oDevPoint:X, oDevPoint:Y,;
			oSize:Width, oSize:Height,;
			hOwner, wID, hInst, NULL_PTR)

		IF hWnd != NULL_PTR
			__lpfnDefaultProc := GetWindowLong(hWnd, GWL_WNDPROC)
			SetWindowLong(hWnd, GWL_WNDPROC, LONGINT(_CAST, Get__WCControlProcPtr()))

			//Suppresses flicker in SplitWindow Control
			SetClassStyle(hWnd, _OR(CS_VREDRAW,CS_HREDRAW), FALSE)

			oSize := NULL_OBJECT
			oOrigin := NULL_OBJECT
			__WCRegisterControl(SELF) //register after we get the handle
		ENDIF
	ENDIF

	// set the various visual elements of the control
	IF hWnd != NULL_PTR
		IF _And(GetVersion(), 0x80000000) != 0
			// use Windows95 constants to set colors
			PCALL(gpfnSpltColorSet, hWnd, SPLTCOLOR_3DHIGH, INT(_CAST, GetSysColor(COLOR_3DHILIGHT)))
			PCALL(gpfnSpltColorSet, hWnd, SPLTCOLOR_3DSHADOW, INT(_CAST, GetSysColor(COLOR_3DSHADOW)))
			PCALL(gpfnSpltColorSet, hWnd, SPLTCOLOR_BARFRAME, INT(_CAST, GetSysColor(COLOR_3DFACE)))
		ELSE
			// use WindowsNT constants to set colors
			PCALL(gpfnSpltColorSet, hWnd, SPLTCOLOR_3DSHADOW, INT(_CAST, GetSysColor(COLOR_BTNSHADOW)))
			PCALL(gpfnSpltColorSet, hWnd, SPLTCOLOR_BARFRAME, INT(_CAST, GetSysColor(COLOR_BTNFACE)))
		ENDIF
	ENDIF

	RETURN hWnd

ACCESS deferPaintCount 
	// DHer: 18/12/2008
RETURN SELF:dwDeferPaintCount

METHOD Destroy() 
	//SE-060519
	LOCAL liPane  AS LONGINT
   LOCAL liCount AS LONGINT
	LOCAL oPane   AS OBJECT

	

	IF (oPanes != NULL_OBJECT)
		liCount := oPanes:Width * oPanes:Height
		FOR liPane := 1 TO liCount
			oPane := SELF:GetPaneClient(liPane)
			IF (oPane != NULL_OBJECT) .and. IsMethod(oPane, #Destroy)
				Send(oPane, #Destroy)
			ENDIF
		NEXT  // liPane
	ENDIF

	// if not in garbage collection, clean up instance variables
	IF ! InCollect()
		oPanes := NULL_OBJECT
		oBackgroundBrush := NULL_OBJECT
		oBarBrush := NULL_OBJECT
		oBarFrameBrush := NULL_OBJECT
	ENDIF

	SUPER:Destroy()

	RETURN SELF

METHOD Dispatch(oEvent) 
	LOCAL oSize 	AS Dimension
	LOCAL lResize 	AS LOGIC
	LOCAL oEvt := oEvent AS @@Event

	IF (oEvt:Message == WM_CAPTURECHANGED)
		oSize := SELF:GetPaneSize(1)
		IF (oSize != NULL_OBJECT .and. oSize:Height <= 1)
			oSize:Height := 2
			lResize := TRUE
		ENDIF
		IF (oSize != NULL_OBJECT .and. oSize:Width <= 1)
			oSize:Width := 2
			lResize := TRUE
		ENDIF
		IF (lResize)
			SELF:SetPaneSize(oSize, 1)
		ENDIF
	ENDIF

	RETURN SUPER:Dispatch(oEvt)

METHOD GetAllPaneClients(aChildren) 
	//SE-060519
	LOCAL aPanes  AS ARRAY
	LOCAL liPane  AS LONGINT
   LOCAL liCount AS LONGINT
	LOCAL oPane   AS OBJECT

	IF IsArray(aChildren)
		aPanes := aChildren
	ELSE
		aPanes := {}
	ENDIF

   IF oPanes != Null_Object
	   liCount := oPanes:Width * oPanes:Height - 1l

		FOR liPane := 0 UPTO liCount
			 oPane :=__WCGetObjectByHandle(PCALL(gpfnSpltPaneAssocGet, SELF:Handle(), liPane))
			 IF IsInstanceOf(oPane, #__WindApp)
				 oPane := IVarGet(oPane, #Owner)
			 ENDIF
			 IF oPane != NULL_OBJECT
				 AAdd(aPanes, oPane)
			 ENDIF
		NEXT  // dwPane
	ENDIF

	RETURN aPanes

METHOD GetPaneClient(nPane) 
	LOCAL oRet AS OBJECT
	

	// if nPane is a valid pane number, return the pane's client object
	IF (nPane > 0) .and. ((oPanes != NULL_OBJECT) .and. nPane <= (oPanes:Width * oPanes:Height))
		oRet :=__WCGetObjectByHandle(PCALL(gpfnSpltPaneAssocGet, SELF:Handle(), nPane - 1))
		IF IsInstanceOf(oRet, #__WindApp)
			oRet := IVarGet(oRet, #Owner)
		ENDIF
	ENDIF

	RETURN oRet

METHOD GetPaneSize(nPane) 
	LOCAL strucSize	IS _winSize

	

	// if nPane is a valid pane number, return the pane's dimension	object
	IF (nPane > 0) .and. ((oPanes != NULL_OBJECT) .and. nPane <= (oPanes:Width * oPanes:Height))
		IF PCALL(gpfnSpltPaneExtGet, SELF:Handle(), nPane - 1, @strucSize)
			RETURN Dimension{strucSize:cx, strucSize:cy}
		ENDIF
	ENDIF

	RETURN NULL_OBJECT

METHOD Hide(nPane) 
	

	// if nPane is supplied, hide the appropriate pane;
	// otherwise, hide all the panes
	IF IsNil(nPane)
		PCALL(gpfnSpltPaneShow, SELF:Handle(), 0, SPS_HIDEALLPANES)
	ELSE
		PCALL(gpfnSpltPaneShow, SELF:Handle(), nPane - 1, SPS_HIDEPANE)
	ENDIF

	RETURN SELF

ACCESS HorizontalAlign 
	

	RETURN _And(SWS_HALIGN, PCALL(gpfnSpltStyleGet, SELF:Handle())) == 1

ACCESS HorizontalDrag 
	

	RETURN _And(SWS_NOHORZDRAG, PCALL(gpfnSpltStyleGet, SELF:Handle())) == 0

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lHorizontalDrag, lVerticalDrag, kAlignment) 
	LOCAL oWin AS Window

	

	__LoadSplitWindowDLL() //SE-060520

	// the owner must be a window
	IF !IsInstanceOfUsual(oOwner, #Window)
		WCError{#Init, #__SplitView, __WCSTypeError, oOwner, 1}:@@Throw()
	ENDIF
	oWin := oOwner

	// the actual owner on a form will be the FormDialog
	IF IsInstanceOf(oWin, #DataWindow) 
		// oWin := IVarGet(oWin, #__FormWindow)
		oWin := Send(oWin, #__GetFormSurface)
	ELSEIF IsInstanceOf(oWin, #__FormFrame)
		oWin := Send(oWin, #GetDialogWindow)
	ENDIF

	SUPER(oWin, xID, oPoint, oDimension, CASPLIT_CLASS, _Or(WS_VISIBLE, WS_CHILD))

	// set drag and alignment attributes
	IF !lHorizontalDrag
		SELF:SetStyle(SWS_NOHORZDRAG)
	ENDIF
	IF !lVerticalDrag
		SELF:SetStyle(SWS_NOVERTDRAG)
	ENDIF
	SELF:SetStyle(kAlignment)

	RETURN 

ACCESS Layout 
	

	RETURN oPanes

ASSIGN Layout(oDimension) 
   //SE-060520 S. Ebert
	//Corrects a problem which occurs if you define Layout and the window height or width is 0.
   //Also a SplitWindow with SPLIT_HORZALIGN style is correctly displayed now in this case.
	LOCAL sRect    IS _winRect
	LOCAL sSize    IS _winSize
	LOCAL liWidth  AS LONGINT
	LOCAL liHeight AS LONGINT
	LOCAL liPane   AS LONGINT
	LOCAL liCount  AS LONGINT
	LOCAL dwFlags  AS DWORD
	LOCAL hSplit   AS PTR

	

	// store the dimensions of the window and create the
	// array that will store the pane client objects
	oPanes := oDimension

	liWidth  := oPanes:Width
   liHeight := oPanes:Height
   liCount  := liWidth * liHeight - 1l

   hSplit := SELF:Handle()

   //Initialize layout
   PCALL(gpfnSpltPaneShow, hSplit, 0, SPS_HIDEALLPANES)
   PCALL(gpfnSpltLayout, hSplit, oPanes:Height, oPanes:Width)

   //show and initialize each pane
   FOR liPane := 0 UPTO liCount
   	 PCALL(gpfnSpltPaneShow, hSplit, liPane, SPS_SHOWPANE)
   NEXT  // liPane

   //force the same size for each pane
   liWidth  *= 50l
   liHeight *= 50l

	GetClientRect(hSplit, @sRect)

   dwFlags  := _OR(SWP_NOMOVE, SWP_NOZORDER, SWP_NOACTIVATE)
   SetWindowPos(hSplit, 0, 0, 0, liWidth, liHeight, dwFlags)

   sSize:cx := 51l
	sSize:cy := 51l
   FOR liPane := 0 UPTO liCount
       PCALL(gpfnSpltPaneExtSet, hSplit, liPane, @sSize)
   NEXT  // liPane

   SetWindowPos(hSplit, 0, 0, 0, sRect:Right, sRect:bottom, dwFlags)

   RETURN 

METHOD RestoreUpdate() 

	

	// decrement the deferred paint count
	IF (dwDeferPaintCount != 0)
		--dwDeferPaintCount
	ENDIF

	// if the deferred paint count is 0, end deferred painting
	IF (dwDeferPaintCount == 0)
		PCALL(gpfnSpltEndDeferPaint, SELF:Handle(), TRUE)
	ENDIF

	RETURN SELF

METHOD SetPaneClient(oWindow, nPane) 
	LOCAL dwStyles AS DWORD
	

	//PP-030910 Bug 99
	//PP-030916 // Check oWindow is a Window before changing style
	//PP-030923 move SetWindowLong call, so only done if oWindow is a window
	IF IsInstanceOfUsual(oWindow,#Window)
      //SE-050113 S. Ebert
     //avoids the side effect, that after a call of SetPaneClient() a formerly been set
      //WS_CLIPCHILDREN style is reset.
		dwStyles := DWORD(_CAST, GetWindowLong(oWindow:handle(), GWL_STYLE))
		dwStyles := _And(dwStyles,_Not(WS_POPUP))
		dwStyles := _Or(dwStyles,  WS_CHILD)
		SetWindowLong(oWindow:handle(), GWL_STYLE, LONGINT(_CAST, dwStyles))

	ENDIF


	// store the client in the array, and connect it to the pane
	IF IsInstanceOfUsual(oWindow, #Window) .or. IsInstanceOfUsual(oWindow, #Control)
		PCALL(gpfnSpltPaneAssocSet, SELF:Handle(), oWindow:Handle(), nPane - 1)
	ELSEIF (oWindow == NULL_OBJECT)
		PCALL(gpfnSpltPaneAssocSet, SELF:Handle(), NULL_PTR, nPane - 1)
	ENDIF

	RETURN NIL

METHOD SetPaneSize(oDimension, nPane) 
	LOCAL strucSize IS _winSize

	

	// if nPane is valid, return the size of the pane
	IF nPane > 0 .and. nPane <= (oPanes:Width * oPanes:Height)
		//SE-050910 S. Ebert
		//Added the offset off 1 to the horizontal and vertical dimension.
		//Without this offset, the result of __SplitView:GetPaneSize() is decremented by one in
		//the horizontal and the vertical dimension.
		//Pane size calculations becomes more accurate with this fix and it avoids the sometimes
      //missing pixels of panes.
		strucSize:cx := oDimension:Width  + 1
		strucSize:cy := oDimension:Height + 1
		RETURN PCALL(gpfnSpltPaneExtSet, SELF:Handle(), nPane - 1, @strucSize)
	ENDIF

	RETURN FALSE

METHOD Show(nPane) 
	

	// if nPane is supplied, show the appropriate pane;
	// otherwise, show all the panes
	IF IsNil(nPane)
		PCALL(gpfnSpltPaneShow, SELF:Handle(), 0, SPS_SHOWALLPANES)
	ELSE
		PCALL(gpfnSpltPaneShow, SELF:Handle(), nPane - 1, SPS_SHOWPANE)
	ENDIF

	RETURN SELF

ACCESS SplitBarBackground 
	

	RETURN oBarBrush

ASSIGN SplitBarBackground(oBrush) 
	

	SELF:ChangeBackground(oBrush, SPLTCOLOR_BAR)
	RETURN 

ACCESS SplitBarFrameBackground 
	

	RETURN oBarFrameBrush

ASSIGN SplitBarFrameBackground(oBrush) 
	

	SELF:ChangeBackground(oBrush, SPLTCOLOR_BARFRAME)
	RETURN 

METHOD SuspendUpdate() 
	

	// if the deferred paint count is 0, begin deferred painting
	IF (dwDeferPaintCount == 0)
		PCALL(gpfnSpltDeferPaint, SELF:Handle())
	ENDIF
	dwDeferPaintCount := dwDeferPaintCount + 1 //++

	RETURN NIL

ACCESS VerticalAlign 
	

	RETURN _And(SWS_VALIGN, PCALL(gpfnSpltStyleGet, SELF:Handle())) == 1

ACCESS VerticalDrag 
	

	RETURN _And(SWS_NOVERTDRAG, PCALL(gpfnSpltStyleGet, SELF:Handle())) == 0

END CLASS

PARTIAL CLASS SplitWindow INHERIT ChildAppWindow
	PROTECT oSplitView	AS __SplitView
	PROTECT lInDestroy AS LOGIC

	//PP-030828 Strong typing
	METHOD __ResizeSplitView() AS VOID STRICT 
	//SE-050822 S. Ebert
	//Corrects a two pixel horizontal sizing mismatch.
	LOCAL rect   IS _WINRECT
	LOCAL yPoint AS INT
	LOCAL oTB    AS ToolBar
	LOCAL oSB    AS StatusBar

	IF oSplitView != NULL_OBJECT

		GetClientRect(hWnd, @rect)

		yPoint := 0

		IF (oTB := SELF:ToolBar) != NULL_OBJECT
			yPoint := oTB:Size:Height
			rect:bottom -= yPoint
		ENDIF

		IF (oSB := SELF:StatusBar) != NULL_OBJECT
			rect:bottom -= oSB:Size:Height
		ENDIF

      SetWindowPos(oSplitView:Handle(), 0, 0, yPoint, rect:right, rect:bottom, _OR(SWP_NOZORDER, SWP_NOACTIVATE))

	ENDIF

	RETURN
// 	//PP-030828 Strong typing
//
// 	//PP-030910 See below
// 	LOCAL oBar			 AS Control
// 	LOCAL oClientSize   AS Dimension
// 	//	LOCAL oCanvasArea AS BoundingBox
// 	LOCAL nOffsetTop	 AS INT
// 	LOCAL nOffsetBottom	AS INT
//
// 	
//
// 	// resize the split view control to the canvas area of
// 	// the window, adjusting for the tool bar and status bar
//
// 	IF (oSplitView != NULL_OBJECT)
// 		oBar := SELF:ToolBar
// 		IF (oBar != NULL_OBJECT)
// 			//PP-030910
// 			// IF !IsInstanceOf(oBar:owner, #ShellWindow) .or. (IVarGet(oBar:Owner, #ChildToolBarLocation) != TBL_SHELL)
// 			nOffsetTop := oBar:Size:Height
// 			// ENDIF
// 		ENDIF
//
// 		oBar := SELF:StatusBar
// 		IF (oBar != NULL_OBJECT)
// 			nOffsetBottom := oBar:Size:Height
// 		ENDIF
//
// 		//PP-030910 From S Ebert bug 78 code replace that commented below
// 		oClientSize := SELF:CanvasArea:Size
//
// 		oClientSize:Width  += 2
// 		oClientSize:Height -= nOffsetTop + nOffSetBottom
//
// 		WCMoveWindow(oSplitView, Point{0, nOffsetBottom}, oClientSize, TRUE)
// 		/*
// 			oClientSize := SELF:CanvasArea:Size
//
// 			oSplitView:Origin := Point{0, nOffsetBottom}
//
// 			// correction for Bug in SplitWindow (!?)
// 			oCanvasArea:Width += 2
//
// 			oSplitView:Size := Dimension{oCanvasArea:Width, oCanvasArea:Height - nOffsetTop - nOffSetBottom}
// 		*/
// 	ENDIF
// 	RETURN

ACCESS __SplitView AS __SplitView STRICT 
	//PP-030828 Strong typing
	

	RETURN oSplitView

ACCESS Background 
	
	IF oSplitView != NULL_OBJECT
		RETURN oSplitView:Background
	ENDIF
	RETURN NULL_OBJECT

ASSIGN Background(oBrush) 
	
	RETURN oSplitView:Background := oBrush

METHOD ChangeBackground(oBrush, kWhere) 
	
	RETURN oSplitView:ChangeBackground(oBrush, kWhere)

METHOD Destroy() 
	

	// if not in garbage collection, destroy the split view control
	lInDestroy := TRUE
	IF !InCollect()
		IF oSplitView != NULL_OBJECT
			oSplitView:Destroy()
			oSplitView := NULL_OBJECT
		ENDIF
	ENDIF
	SUPER:Destroy()

	RETURN SELF

METHOD Dispatch(oEvent) 
	LOCAL oEvt := oEvent AS @@Event
	LOCAL oPane AS OBJECT

	IF (oEvt:uMsg == WM_NCACTIVATE) .and. LOGIC(_CAST, oEvt:wParam)
		InvalidateRect(hwnd, NULL_PTR, TRUE)
	ELSEIF (oEvt:uMsg == WM_SETFOCUS) .and. !lInDestroy .and. (oSplitView != NULL_OBJECT)
		oPane := oSplitView:GetPaneClient(1)
		IF (oPane != NULL_OBJECT) .and. IsWindow(oPane:Handle()) .and. !IsInstanceOf(oPane, #DialogWindow)
			SetFocus(Send(oPane, #Handle))
		ENDIF
	ENDIF

	RETURN SUPER:Dispatch(oEvt)

METHOD EnableStatusBar(lEnable) 
	

	SUPER:EnableStatusBar(lEnable)
	SELF:__ResizeSplitView()

	RETURN SELF:StatusBar

METHOD GetAllChildren() 
	//RvdH 060519 Added, so all pane clients are also returned
	//SE-060520
	LOCAL aChildren AS ARRAY

	aChildren := SUPER:GetAllChildren()
	IF oSplitView != NULL_OBJECT
	   aChildren := oSplitView:GetAllPaneClients(aChildren)
	ENDIF

	RETURN aChildren

METHOD GetPaneClient(nPane) 
	

	RETURN oSplitView:GetPaneClient(nPane)

METHOD GetPaneSize(nPane) 
	

	RETURN oSplitView:GetPaneSize(nPane)

METHOD HidePane(nPane) 
	

	SELF:oSplitView:Hide(nPane)
	RETURN SELF


ACCESS HorizontalAlign 
	

	RETURN oSplitView:HorizontalAlign

ACCESS HorizontalDrag 
	

	RETURN oSplitView:HorizontalDrag

CONSTRUCTOR(oOwner, lHorizontalDrag, lVerticalDrag, kAlignment) 
	LOCAL oObject		AS OBJECT
	LOCAL oBar			AS Control
	LOCAL oPoint		AS Point
	LOCAL oDimension	AS Dimension
	LOCAL nOffsetTop	AS INT
	LOCAL nOffsetBottom	AS INT

	

	IF IsObject(oOwner)
		oObject := oOwner
		IF IsInstanceOf(oObject, #App)
			SUPER(NIL, FALSE)
		ELSEIF IsInstanceOf(oObject, #ShellWindow)
			SUPER(oOwner, TRUE)
		ELSEIF IsInstanceOf(oObject, #DataWindow)
			SUPER(oObject:__GetFormSurface())
		ELSEIF IsInstanceOf(oObject, #ChildAppWindow)
			SUPER(oOwner)
		ELSEIF IsInstanceOf(oObject, #TopAppWindow)
			SUPER(oOwner)
		ELSEIF IsInstanceOf(oOBject, #DialogWindow)
			SUPER(oOwner)
		ELSEIF IsInstanceOf(oObject, #Window)
			WCError{#Init, #SplitWindow, __WCSTypeError, oOwner, 1}:@@Throw()
		ELSE
			SUPER(oOwner)
		ENDIF
	ELSE
		SUPER(oOwner)
	ENDIF

	oBar := SELF:ToolBar
	IF oBar != NULL_OBJECT
		nOffsetTop := oBar:Size:Height
	ENDIF

	oBar := SELF:StatusBar
	IF oBar != NULL_OBJECT
		nOffsetBottom := oBar:Size:Height
	ENDIF

	oDimension := Dimension{SELF:CanvasArea:Width, SELF:CanvasArea:Height - nOffsetTop}
	oPoint := Point{0, nOffsetBottom}

	// set up drag and alignment options

	Default(@lHorizontalDrag, FALSE)
	Default(@lVerticalDrag, TRUE)
	Default(@kAlignment, SPLIT_VERTALIGN )

	oSplitView := __SplitView{SELF, 1000, oPoint, oDimension, lHorizontalDrag, lVerticalDrag, kAlignment}
	oSplitView:Show()

	RETURN 

ACCESS Layout 
	

	RETURN oSplitView:Layout

ASSIGN Layout(oDimension) 
   //SE-060520
	RETURN oSplitView:Layout := oDimension

METHOD Resize(oResizeEvent) 
	

	SUPER:Resize(oResizeEvent)
	SELF:__ResizeSplitView()

	RETURN NIL

METHOD RestoreUpdate 
	

	RETURN oSplitView:RestoreUpdate()

METHOD SetPaneClient(oWindow, nPane) 
	

	RETURN oSplitView:SetPaneClient(oWindow, nPane)

METHOD SetPaneSize(oDimension, nPane) 
	//SE-060518
	

	RETURN oSplitView:SetPaneSize(oDimension, nPane)

METHOD Show(nShowState, nPane) 
	

	SUPER:Show(nShowState)
	SELF:__ResizeSplitView()
	//SE-050822 S. Ebert
   //This fix avoids the display of all hidden panes after a call of SplitWindow:Show()
   //with nPane = NIL. This behaviour was very irritating.
   //The parameter nPane in the Show method is nowhere documented.
   //IMHO I would delete this parameter and the below code, but this needs a new repository.
	IF IsNumeric(nPane)
		IF nPane <= 0
			nPane := NIL
		ENDIF
	   oSplitView:Show(nPane)
	ENDIF

	RETURN SELF

METHOD ShowPane(nPane) 
	

	SELF:oSplitView:Show(nPane)
	RETURN SELF

ACCESS SplitBarBackground 
	

	RETURN oSplitView:SplitBarBackground

ASSIGN SplitBarBackground(oBrush) 
	

	RETURN oSplitView:SplitBarBackground := oBrush

ACCESS SplitBarFrameBackground 
	

	RETURN oSplitView:SplitBarFrameBackground

ASSIGN SplitBarFrameBackground(oBrush) 
	

	RETURN oSplitView:SplitBarFrameBackground := oBrush

METHOD SuspendUpdate 
	

	RETURN oSplitView:SuspendUpdate()

ASSIGN ToolBar(oNewToolBar) 
	

	SUPER:Toolbar := oNewToolBar
	SELF:__ResizeSplitView()

	RETURN 

METHOD ToolBarHeightChanged(oControlNotifyEvent) 
   SELF:__ResizeSplitView()
   RETURN SELF

ACCESS VerticalAlign 
	

	RETURN oSplitView:VerticalAlign

ACCESS VerticalDrag 
	

	RETURN oSplitView:VerticalDrag
END CLASS

STATIC GLOBAL glSplitDllLoaded := FALSE AS LOGIC

//function declarations
STATIC GLOBAL gpfnSpltColorSet AS TSpltColorSet PTR
STATIC GLOBAL gpfnSpltDeferPaint AS TSpltDeferPaint PTR
STATIC GLOBAL gpfnSpltEndDeferPaint AS TSpltEndDeferPaint PTR

STATIC GLOBAL gpfnSpltLayout AS TSpltLayout PTR
STATIC GLOBAL gpfnSpltPaneAssocGet AS TSpltPaneAssocGet PTR
STATIC GLOBAL gpfnSpltPaneAssocSet AS TSpltPaneAssocSet PTR
STATIC GLOBAL gpfnSpltPaneExtGet AS TSpltPaneExtGet PTR
STATIC GLOBAL gpfnSpltPaneExtSet AS TSpltPaneExtSet PTR
STATIC GLOBAL gpfnSpltPaneShow AS TSpltPaneShow PTR
STATIC GLOBAL gpfnSpltStyleGet AS TSpltStyleGet PTR
STATIC FUNCTION TSpltColorSet(hWnd AS PTR, iColor AS DWORD, cr AS INT) AS INT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TSpltDeferPaint(hWnd AS PTR) AS VOID STRICT
	RETURN
STATIC FUNCTION TSpltEndDeferPaint(hWnd AS PTR, bUpdate AS LOGIC) AS VOID STRICT
	RETURN

STATIC FUNCTION TSpltLayout(hWnd AS PTR, nRows AS INT, nCols AS INT) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TSpltPaneAssocGet(hWnd AS PTR, iPane AS INT) AS PTR STRICT
	//SYSTEM
	RETURN NULL_PTR

STATIC FUNCTION TSpltPaneAssocSet(hWnd AS PTR, hWndAssoc AS PTR, iPane AS INT) AS PTR STRICT
	//SYSTEM
	RETURN NULL_PTR

STATIC FUNCTION TSpltPaneExtGet(hWnd AS PTR, iPane AS INT, lpExt AS _winSize) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TSpltPaneExtSet(hWnd AS PTR, iPane AS INT, lpExt AS _winSize) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TSpltPaneShow(hWnd AS PTR, iPane AS INT, uCode AS DWORD) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TSpltStyleGet(hWnd AS PTR) AS DWORD STRICT
	//SYSTEM
	RETURN 0

FUNCTION __LoadSplitWindowDLL()
	LOCAL hDll AS PTR
	LOCAL rsFormat AS ResourceString

	IF glSplitDllLoaded
		RETURN TRUE
	ENDIF

	hDll := LoadLibrary(PSZ(_CAST, "CATO3SPL.DLL"))
	IF (hDll == NULL_PTR)
		rsFormat := ResourceString{__WCSLoadLibraryError}
		WCError{#LoadSplitWindowDLL, #SplitWindow, VO_Sprintf(rsFormat:value, "CATO3SPL.DLL"),,,FALSE}:@@Throw()
		RETURN FALSE
	ENDIF

	gpfnSpltColorSet 		:= GetProcAddress(hDll, PSZ(_CAST, "SpltColorSet"))
	gpfnSpltPaneAssocGet := GetProcAddress(hDll, PSZ(_CAST, "SpltPaneAssocGet"))
	gpfnSpltPaneAssocSet := GetProcAddress(hDll, PSZ(_CAST, "SpltPaneAssocSet"))
	gpfnSpltPaneExtGet 	:= GetProcAddress(hDll, PSZ(_CAST, "SpltPaneExtGet"))
	gpfnSpltPaneExtSet 	:= GetProcAddress(hDll, PSZ(_CAST, "SpltPaneExtSet"))
	gpfnSpltPaneShow 		:= GetProcAddress(hDll, PSZ(_CAST, "SpltPaneShow"))
	gpfnSpltStyleGet 		:= GetProcAddress(hDll, PSZ(_CAST, "SpltStyleGet"))
	gpfnSpltLayout 		:= GetProcAddress(hDll, PSZ(_CAST, "SpltLayout"))
	gpfnSpltDeferPaint 	:= GetProcAddress(hDll, PSZ(_CAST, "SpltDeferPaint"))
	gpfnSpltEndDeferPaint := GetProcAddress(hDll, PSZ(_CAST, "SpltEndDeferPaint"))

	RETURN (glSplitDllLoaded := TRUE)
