USING SWF := System.Windows.Forms


CLASS SplitWindow INHERIT ChildAppWindow
	PROTECT oSplitView	AS SplitView
	PROTECT lInDestroy AS LOGIC

	METHOD __ResizeSplitView() AS VOID STRICT 
		//Corrects a two pixel horizontal sizing mismatch.
		//LOCAL rect   IS _WINRECT
		//LOCAL yPoint AS INT
		//LOCAL oTB    AS ToolBar
		//LOCAL oSB    AS StatusBar

		//IF oSplitView != NULL_OBJECT

		//	GetClientRect(hWnd, @rect)

		//	yPoint := 0

		//	IF (oTB := SELF:ToolBar) != NULL_OBJECT
		//		yPoint := oTB:Size:Height
		//		rect:bottom -= yPoint
		//	ENDIF

		//	IF (oSB := SELF:StatusBar) != NULL_OBJECT
		//		rect:bottom -= oSB:Size:Height
		//	ENDIF

		//	SetWindowPos(oSplitView:Handle(), 0, 0, yPoint, rect:right, rect:bottom, _OR(SWP_NOZORDER, SWP_NOACTIVATE))

		//ENDIF

		RETURN


	PROPERTY __SplitView AS SplitView GET oSplitView

	ACCESS Background AS Brush
		IF oSplitView != NULL_OBJECT
			RETURN oSplitView:Background
		ENDIF
		RETURN NULL_OBJECT

	ASSIGN Background(oBrush AS Brush) 
		
		oSplitView:Background := oBrush

	METHOD ChangeBackground(oBrush, kWhere) 
		
		oSplitView:ChangeBackground(oBrush, kWhere)
		RETURN SELF

	METHOD Destroy() AS USUAL CLIPPER
		

		// if not in garbage collection, destroy the split view control
		lInDestroy := TRUE
		IF oSplitView != NULL_OBJECT
			oSplitView:Destroy()
			oSplitView := NULL_OBJECT
		ENDIF
		SUPER:Destroy()

		RETURN SELF

	METHOD Dispatch(oEvent) 
		//LOCAL oEvt := oEvent AS @@Event
		//LOCAL oPane AS OBJECT

		//IF (oEvt:uMsg == WM_NCACTIVATE) .and. LOGIC(_CAST, oEvt:wParam)
		//	InvalidateRect(hwnd, NULL_PTR, TRUE)
		//ELSEIF (oEvt:uMsg == WM_SETFOCUS) .and. !lInDestroy .and. (oSplitView != NULL_OBJECT)
		//	oPane := oSplitView:GetPaneClient(1)
		//	IF (oPane != NULL_OBJECT) .and. IsWindow(oPane:Handle()) .and. !IsInstanceOf(oPane, #DialogWindow)
		//		SetFocus(Send(oPane, #Handle))
		//	ENDIF
		//ENDIF

		RETURN SUPER:Dispatch(oEvent)

	METHOD EnableStatusBar(lEnable as Logic)  as StatusBar
		

		SUPER:EnableStatusBar(lEnable)
		SELF:__ResizeSplitView()

		RETURN SELF:StatusBar

	METHOD GetAllChildren() AS ARRAY STRICT
		//RvdH 060519 Added, so all pane clients are also returned
		//SE-060520
		LOCAL aChildren AS ARRAY

		aChildren := SUPER:GetAllChildren()
		IF oSplitView != NULL_OBJECT
			aChildren := oSplitView:GetAllPaneClients(aChildren)
		ENDIF

		RETURN aChildren

	METHOD GetPaneClient(nPane AS LONG)  AS OBJECT
		RETURN oSplitView:GetPaneClient(nPane)

	METHOD GetPaneSize(nPane AS LONG) AS Dimension
		RETURN oSplitView:GetPaneSize(nPane)

	METHOD HidePane(nPane AS LONG)  AS VOID
		SELF:oSplitView:HidePane(nPane)
		RETURN 


	ACCESS HorizontalAlign AS LOGIC
		RETURN oSplitView:HorizontalAlign

    ACCESS HorizontalDrag  AS LOGIC
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

		DEFAULT(@lHorizontalDrag,   FALSE)
		DEFAULT(@lVerticalDrag,     TRUE)
		Default(@kAlignment, SPLIT_VERTALIGN )

		oSplitView := SplitView{SELF, 1000, oPoint, oDimension, lHorizontalDrag, lVerticalDrag, kAlignment}
		oSplitView:Show()

		RETURN 

	ACCESS Layout AS Dimension
		RETURN oSplitView:Layout

	ASSIGN Layout(oDimension AS Dimension) 
		oSplitView:Layout := oDimension

	METHOD Resize(oResizeEvent) 

		SUPER:Resize(oResizeEvent)
		SELF:__ResizeSplitView()

		RETURN NIL

	METHOD RestoreUpdate AS VOID
		oSplitView:RestoreUpdate()
		RETURN 

	METHOD SetPaneClient(oWindow AS IGuiObject, nPane AS LONG)  AS VOID
		oSplitView:SetPaneClient(oWindow, nPane)

	METHOD SetPaneSize(oDimension AS Dimension, nPane AS LONG) AS VOID
		oSplitView:SetPaneSize(oDimension, nPane)

    METHOD Show() AS VOID STRICT
        SELF:Show(SW_NORMAL, -1)
        
	METHOD Show(nShowState AS LONG, nPane AS LONG) AS VOID

		SUPER:Show(nShowState)
		SELF:__ResizeSplitView()
	    oSplitView:ShowPane(nPane)

		RETURN 

	METHOD ShowPane(nPane AS LONG)  AS VOID
		SELF:oSplitView:ShowPane(nPane)
		RETURN 

	ACCESS SplitBarBackground AS Brush
		RETURN oSplitView:SplitBarBackground

	ASSIGN SplitBarBackground(oBrush AS Brush) 
		oSplitView:SplitBarBackground := oBrush

	ACCESS SplitBarFrameBackground AS Brush
		RETURN oSplitView:SplitBarFrameBackground

	ASSIGN SplitBarFrameBackground(oBrush AS Brush) 
		 oSplitView:SplitBarFrameBackground := oBrush

	METHOD SuspendUpdate AS VOID
		oSplitView:SuspendUpdate()
		RETURN  

	ASSIGN ToolBar(oNewToolBar as Toolbar) 
		SUPER:Toolbar := oNewToolBar
		SELF:__ResizeSplitView()

		RETURN 

	METHOD ToolBarHeightChanged(oControlNotifyEvent) 
		SELF:__ResizeSplitView()
		RETURN SELF

	ACCESS VerticalAlign AS LOGIC
		RETURN oSplitView:VerticalAlign

	ACCESS VerticalDrag  AS LOGIC
		RETURN oSplitView:VerticalDrag

END CLASS


