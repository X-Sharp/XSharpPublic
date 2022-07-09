USING SWF := System.Windows.Forms


/// <include file="Gui.xml" path="doc/SplitWindow/*" />
CLASS SplitWindow INHERIT ChildAppWindow
	PROTECT oSplitView	AS SplitView

 /// <exclude />
	METHOD __ResizeSplitView() AS VOID STRICT
        VAR oc := oSplitView:__Control
        IF SELF:IsVisible()
            SELF:__Form:SuspendLayout()
            IF SELF:ToolBar != NULL_OBJECT
                VAR nTb := SELF:ToolBar:Size:Height
                oc:Location := Point{0, nTb}
                oc:Size     := Dimension{SELF:Size:Width, SELF:Size:Height - nTb}
            ELSE
                oc:Location := Point{0, 0}
                oc:Size     := SELF:Size
            ENDIF
            SELF:__Form:ResumeLayout(TRUE)
        ENDIF
 		RETURN


 /// <exclude />
	PROPERTY __SplitView AS SplitView GET oSplitView

/// <include file="Gui.xml" path="doc/SplitWindow.Background/*" />
	ACCESS Background AS Brush
		IF oSplitView != NULL_OBJECT
			RETURN oSplitView:Background
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/SplitWindow.Background/*" />
	ASSIGN Background(oBrush AS Brush)
		oSplitView:Background := oBrush
        RETURN

/// <include file="Gui.xml" path="doc/SplitWindow.ChangeBackground/*" />
	METHOD ChangeBackground(oBrush, kWhere)
		oSplitView:ChangeBackground(oBrush, kWhere)
		RETURN SELF

/// <include file="Gui.xml" path="doc/SplitWindow.Destroy/*" />
	METHOD Destroy() AS USUAL
		// if not in garbage collection, destroy the split view control
		IF oSplitView != NULL_OBJECT
			oSplitView:Destroy()
			oSplitView := NULL_OBJECT
		ENDIF
		SUPER:Destroy()

		RETURN SELF

/// <include file="Gui.xml" path="doc/SplitWindow.Dispatch/*" />
	METHOD Dispatch(oEvent AS @@Event)
        LOCAL oEvt := oEvent AS Event
		IF (oEvt:uMsg == WM_SETFOCUS) .AND. oSplitView != NULL_OBJECT
			VAR oPane := oSplitView:GetPaneClient(1)
			IF (oPane != NULL_OBJECT) .AND.  ! (oPane IS DialogWindow)
				oPane:SetFocus()
			ENDIF
		ENDIF
		RETURN SUPER:Dispatch(oEvent)

/// <include file="Gui.xml" path="doc/SplitWindow.EnableStatusBar/*" />
	METHOD EnableStatusBar(lEnable AS LOGIC)  AS StatusBar
		SUPER:EnableStatusBar(lEnable)
		SELF:__ResizeSplitView()
		RETURN SELF:StatusBar

/// <include file="Gui.xml" path="doc/SplitWindow.GetAllChildren/*" />
	METHOD GetAllChildren() AS ARRAY STRICT
		LOCAL aChildren AS ARRAY
		aChildren := SUPER:GetAllChildren()
		IF oSplitView != NULL_OBJECT
			aChildren := oSplitView:GetAllPaneClients(aChildren)
		ENDIF
		RETURN aChildren

/// <include file="Gui.xml" path="doc/SplitWindow.GetPaneClient/*" />
	METHOD GetPaneClient(nPane AS LONG)  AS OBJECT
		RETURN oSplitView:GetPaneClient(nPane)

/// <include file="Gui.xml" path="doc/SplitWindow.GetPaneSize/*" />
	METHOD GetPaneSize(nPane AS LONG) AS Dimension
		RETURN oSplitView:GetPaneSize(nPane)

/// <include file="Gui.xml" path="doc/SplitWindow.HidePane/*" />
	METHOD HidePane(nPane AS LONG)  AS VOID
		SELF:oSplitView:HidePane(nPane)
		RETURN

/// <include file="Gui.xml" path="doc/SplitWindow.HorizontalAlign/*" />
	ACCESS HorizontalAlign AS LOGIC
		RETURN oSplitView:HorizontalAlign

/// <include file="Gui.xml" path="doc/SplitWindow.HorizontalDrag/*" />
    ACCESS HorizontalDrag  AS LOGIC
		RETURN oSplitView:HorizontalDrag

/// <include file="Gui.xml" path="doc/SplitWindow.ctor/*" />
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
        oSplitView:__Control:Anchor := System.Windows.Forms.AnchorStyles.Bottom+System.Windows.Forms.AnchorStyles.Left+System.Windows.Forms.AnchorStyles.Right+System.Windows.Forms.AnchorStyles.Top
        oSplitView:__Control:Location := Point{0,0}
        oSplitView:__Control:Size     := SELF:Size
		RETURN

/// <include file="Gui.xml" path="doc/SplitWindow.Layout/*" />
	ACCESS Layout AS Dimension
		RETURN oSplitView:Layout

/// <include file="Gui.xml" path="doc/SplitWindow.Layout/*" />
	ASSIGN Layout(oDimension AS Dimension)
		oSplitView:Layout := oDimension

/// <include file="Gui.xml" path="doc/SplitWindow.Resize/*" />
	METHOD Resize(oResizeEvent)
		SUPER:Resize(oResizeEvent)
		RETURN NIL

/// <include file="Gui.xml" path="doc/SplitWindow.RestoreUpdate/*" />
	METHOD RestoreUpdate AS VOID
		oSplitView:RestoreUpdate()
		RETURN

/// <include file="Gui.xml" path="doc/SplitWindow.SetPaneClient/*" />
	METHOD SetPaneClient(oWindow AS IGuiObject, nPane AS LONG)  AS VOID
		oSplitView:SetPaneClient(oWindow, nPane)

/// <include file="Gui.xml" path="doc/SplitWindow.SetPaneSize/*" />
	METHOD SetPaneSize(oDimension AS Dimension, nPane AS LONG) AS VOID
		oSplitView:SetPaneSize(oDimension, nPane)

/// <include file="Gui.xml" path="doc/SplitWindow.Show/*" />
    METHOD Show() AS VOID STRICT
        SELF:Show(SW_NORMAL, -1)

/// <include file="Gui.xml" path="doc/SplitWindow.Show/*" />
	METHOD Show(nShowState AS LONG, nPane AS LONG) AS VOID
		SUPER:Show(nShowState)
	    oSplitView:ShowPane(nPane)
		RETURN

/// <include file="Gui.xml" path="doc/SplitWindow.ShowPane/*" />
	METHOD ShowPane(nPane AS LONG)  AS VOID
		SELF:oSplitView:ShowPane(nPane)
		RETURN

/// <include file="Gui.xml" path="doc/SplitWindow.SplitBarBackground/*" />
	ACCESS SplitBarBackground AS Brush
		RETURN oSplitView:SplitBarBackground

/// <include file="Gui.xml" path="doc/SplitWindow.SplitBarBackground/*" />
	ASSIGN SplitBarBackground(oBrush AS Brush)
		oSplitView:SplitBarBackground := oBrush

/// <include file="Gui.xml" path="doc/SplitWindow.SplitBarFrameBackground/*" />
	ACCESS SplitBarFrameBackground AS Brush
		RETURN oSplitView:SplitBarFrameBackground

/// <include file="Gui.xml" path="doc/SplitWindow.SplitBarFrameBackground/*" />
	ASSIGN SplitBarFrameBackground(oBrush AS Brush)
		 oSplitView:SplitBarFrameBackground := oBrush

/// <include file="Gui.xml" path="doc/SplitWindow.SuspendUpdate/*" />
	METHOD SuspendUpdate AS VOID
		oSplitView:SuspendUpdate()
		RETURN

/// <include file="Gui.xml" path="doc/SplitWindow.ToolBar/*" />
	ASSIGN ToolBar(oNewToolBar as Toolbar)
		SUPER:Toolbar := oNewToolBar
        SELF:__ResizeSplitView()
		RETURN

/// <include file="Gui.xml" path="doc/SplitWindow.ToolBarHeightChanged/*" />
	METHOD ToolBarHeightChanged(oControlNotifyEvent AS ControlNotifyEvent)
        SELF:__ResizeSplitView()
		RETURN SELF

/// <include file="Gui.xml" path="doc/SplitWindow.VerticalAlign/*" />
	ACCESS VerticalAlign AS LOGIC
		RETURN oSplitView:VerticalAlign

/// <include file="Gui.xml" path="doc/SplitWindow.VerticalDrag/*" />
	ACCESS VerticalDrag  AS LOGIC
		RETURN oSplitView:VerticalDrag

END CLASS


