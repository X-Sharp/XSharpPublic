//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using SWF := System.Windows.Forms


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


    /// <include file="Gui.xml" path="doc/SplitWindow.Background/*" />
    PROPERTY Background AS Brush GET oSplitView:Background SET oSplitView:Background := value

    /// <include file="Gui.xml" path="doc/SplitWindow.ChangeBackground/*" />
    METHOD ChangeBackground(oBrush, kWhere)
       oSplitView:ChangeBackground(oBrush, kWhere)
       RETURN SELF

    /// <include file="Gui.xml" path="doc/SplitWindow.Destroy/*" />
    METHOD Destroy() AS USUAL CLIPPER
        if oSplitView != null_object
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
    METHOD HidePane(nPane AS LONG)  AS VOID => SELF:oSplitView:HidePane(nPane)

    /// <include file="Gui.xml" path="doc/SplitWindow.HorizontalAlign/*" />
    PROPERTY HorizontalAlign AS LOGIC GET oSplitView:HorizontalAlign

    /// <include file="Gui.xml" path="doc/SplitWindow.HorizontalDrag/*" />
    PROPERTY HorizontalDrag  AS LOGIC GET oSplitView:HorizontalDrag

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
            SWITCH oObject
            CASE App
                SUPER(NIL, FALSE)
            CASE ShellWindow
                SUPER(oOwner, TRUE)
            case oDW as DataWindow
                super(oDW:__GetFormSurface())
            CASE ChildAppWindow
            CASE TopAppWindow
            CASE DialogWindow
                SUPER(oOwner)
            CASE Window
                WCError{#Init, #SplitWindow, __WCSTypeError, oOwner, 1}:Throw()
            OTHERWISE
                SUPER(oOwner)
            END SWITCH
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

        DEFAULT( REF lHorizontalDrag,   FALSE)
        DEFAULT( REF lVerticalDrag,     TRUE)
        DEFAULT( REF kAlignment, SPLIT_VERTALIGN )

        oSplitView := SplitView{SELF, 1000, oPoint, oDimension, lHorizontalDrag, lVerticalDrag, kAlignment}
        oSplitView:Show()
        oSplitView:__Control:Anchor := System.Windows.Forms.AnchorStyles.Bottom+System.Windows.Forms.AnchorStyles.Left+System.Windows.Forms.AnchorStyles.Right+System.Windows.Forms.AnchorStyles.Top
        oSplitView:__Control:Location := Point{0,0}
        oSplitView:__Control:Size     := SELF:Size
        RETURN

    /// <include file="Gui.xml" path="doc/SplitWindow.Layout/*" />
    PROPERTY Layout AS Dimension GET oSplitView:Layout SET oSplitView:Layout := value

    /// <include file="Gui.xml" path="doc/SplitWindow.Resize/*" />
    METHOD Resize(oResizeEvent as ResizeEvent) AS USUAL
        SUPER:Resize(oResizeEvent)
        RETURN SELF

    /// <include file="Gui.xml" path="doc/SplitWindow.RestoreUpdate/*" />
    METHOD RestoreUpdate AS VOID STRICT => oSplitView:RestoreUpdate()

    /// <include file="Gui.xml" path="doc/SplitWindow.SetPaneClient/*" />
    METHOD SetPaneClient(oWindow AS IGuiObject, nPane AS LONG)  AS VOID => oSplitView:SetPaneClient(oWindow, nPane)

    /// <include file="Gui.xml" path="doc/SplitWindow.SetPaneSize/*" />
    METHOD SetPaneSize(oDimension AS Dimension, nPane AS LONG) AS VOID => oSplitView:SetPaneSize(oDimension, nPane)

    /// <include file="Gui.xml" path="doc/SplitWindow.Show/*" />
    method Show(nShowState, nPane ) as void clipper
        default (ref nShowState, SW_NORMAL)
        default (ref nPane, -1)
        SUPER:Show(nShowState)
        oSplitView:ShowPane(nPane)
        RETURN

    /// <include file="Gui.xml" path="doc/SplitWindow.ShowPane/*" />
    METHOD ShowPane(nPane AS LONG)  AS VOID => SELF:oSplitView:ShowPane(nPane)

    /// <include file="Gui.xml" path="doc/SplitWindow.SplitBarBackground/*" />
    PROPERTY SplitBarBackground AS Brush GET  oSplitView:SplitBarBackground SET oSplitView:SplitBarBackground := value

    /// <include file="Gui.xml" path="doc/SplitWindow.SplitBarFrameBackground/*" />
    PROPERTY SplitBarFrameBackground AS Brush GET oSplitView:SplitBarFrameBackground SET oSplitView:SplitBarFrameBackground := value

    /// <include file="Gui.xml" path="doc/SplitWindow.SuspendUpdate/*" />
    METHOD SuspendUpdate AS VOID STRICT => oSplitView:SuspendUpdate()

    /// <include file="Gui.xml" path="doc/SplitWindow.ToolBar/*" />
    ASSIGN ToolBar(oNewToolBar as Toolbar)
        SUPER:Toolbar := oNewToolBar
        SELF:__ResizeSplitView()
        RETURN

    /// <include file="Gui.xml" path="doc/SplitWindow.ToolBarHeightChanged/*" />
    METHOD ToolBarHeightChanged(oControlNotifyEvent AS ControlNotifyEvent) AS USUAL
        SELF:__ResizeSplitView()
  	 RETURN SELF

    /// <include file="Gui.xml" path="doc/SplitWindow.VerticalAlign/*" />
    PROPERTY VerticalAlign AS LOGIC GET oSplitView:VerticalAlign

    /// <include file="Gui.xml" path="doc/SplitWindow.VerticalDrag/*" />
    PROPERTY VerticalDrag  AS LOGIC GET oSplitView:VerticalDrag

END CLASS



