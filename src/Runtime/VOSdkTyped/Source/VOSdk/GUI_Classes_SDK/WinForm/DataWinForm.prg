//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// DataForm.prg

USING SWF := System.Windows.Forms
USING System.Drawing
USING VOSDK := XSharp.VO.SDK
#pragma warnings (9043, disable) // Controls is ambiguous

#define INSIDEFORMBORDER 5
/*
For MDI Child windows this looks like:
.--------------------------------------------
| Toolbar Area                              |
.--------------------------------------------
| SurfacePanel                   |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|.................................          |
|FramePanel                                 |
|                                           |
.--------------------------------------------
| Statusbar Area                            |
.--------------------------------------------


Subwindows consist of:

.--------------------------------------------
| SurfacePanel                   |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|.................................          |
|FramePanel                                 |
.--------------------------------------------

When the surface is smaller than the framepanel, then that panel will fill the window
When the srface is bigger than the Framepanel, then that panel will show the scrollbars

When the datawindow is in browse view, then the browser has the size of the frame panel and no
scrollbars are shown on the framepanel


*/


CLASS VODataForm INHERIT VOChildAppForm
#region Fields
    PROTECT oSurfacePanel	AS VoSurfacePanel		// This is the place where the controls are drawn
    protect oFramePanel		as VoFramePanel			// This is a panel on which the Surface sits. When the window is too small this panel will show the scrollbars
    PROTECT oToolBar		AS VOToolBar
    PROTECT oStatusBar		AS VOStatusBar
    PROTECT oResDlg			AS ResourceDialog
    PROTECT lInBrowseView   AS LOGIC
    PROTECT oDataBrowser	AS SWF.Control
#endregion
#region Properties
    PROPERTY Origin			AS VOSDK.Point      GET SELF:Location SET SELF:Location := Value
    PROPERTY Surface		AS VoSurfacePanel	GET oSurfacePanel
    property Frame          as VOFramePanel	    get oFramePanel
    PROPERTY AutoLayout	    AS LOGIC AUTO       GET SET

    PROPERTY DataBrowser AS SWF.Control
        GET
            RETURN oDataBrowser
        END GET
        SET
            IF oDataBrowser != Value
                IF oDataBrowser != NULL_OBJECT
                    oDataBrowser:GotFocus   -= OnBrowserGotFocus
                    oDataBrowser:LostFocus  -= OnBrowserLostFocus
                ENDIF
                oDataBrowser := Value
                IF oDataBrowser != NULL_OBJECT
                    oFramePanel:Controls:Add(oDataBrowser)
                    oDataBrowser:Visible := FALSE
                    oDataBrowser:Dock := SWF.DockStyle.Fill
                    oDataBrowser:BringToFront()
                    oDataBrowser:GotFocus   += OnBrowserGotFocus
                    oDataBrowser:LostFocus  += OnBrowserLostFocus
                ENDIF
            ENDIF
            SELF:AdjustSizes()
        END SET
    END PROPERTY

    PROPERTY DataWindow AS VOSDK.DataWindow GET (DataWindow) Window


    PROPERTY StatusBar AS VOStatusBar
        GET
            RETURN oStatusBar
        END GET
        SET
            oStatusBar := Value
            IF oStatusBar != NULL_OBJECT
                SELF:Controls:Add((SWF.Control) oStatusBar)
                oStatusBar:Dock := SWF.DockStyle.Bottom
                oStatusBar:TabIndex := 3
                SELF:PerformLayout()
            ENDIF
        END SET
    END PROPERTY

    PROPERTY SubForm	AS LOGIC	AUTO GET SET

    PROPERTY ToolBar AS VOToolBar
        GET
            RETURN oToolBar
        END GET
        SET
            IF Value != NULL_OBJECT
                oToolBar := Value
                SELF:Controls:Add(oToolBar)
                oToolBar:Dock := SWF.DockStyle.Top
                oToolBar:TabIndex := 1
                SELF:PerformLayout()
                SELF:AdjustSizes()
                SELF:AdjustFrameSize()
            ENDIF
        END SET
    END PROPERTY
#endregion



#region Winforms event handlers
    PRIVATE METHOD OnBrowserGotFocus(sender AS OBJECT, e AS EventArgs) AS VOID
        IF SELF:SubForm .AND. SELF:Window != NULL_OBJECT
            SELF:Window:Activate(Event{})
        ENDIF

    PRIVATE METHOD OnBrowserLostFocus(sender AS OBJECT, e AS EventArgs) AS VOID
        IF SELF:SubForm .AND. SELF:Window != NULL_OBJECT
            SELF:Window:Deactivate(Event{})
        ENDIF
#endregion



    CONSTRUCTOR(oWindow AS Window, oOwner AS SWF.Form, oRes AS ResourceDialog)
        SUPER(oWindow, oOwner)
        SELF:oResDlg := oRes
        SELF:Text := "DataWinForm"
        // Form owns the frame
        oFramePanel := GuiFactory.Instance:CreateFramePanel(SELF, oWindow)
        SELF:Controls:Add((SWF.Control) oFramePanel)
        // Frame owns the surface
        oSurfacePanel := GuiFactory.Instance:CreateSurfacePanel(oWindow)
        oFramePanel:Controls:Add(oSurfacePanel)
        IF oRes != NULL_OBJECT
            oSurfacePanel:Size			:= oRes:Size
            oSurfacePanel:MinimumSize   := oRes:Size
        ELSE
            oSurfacePanel:Size          := SELF:ClientRectangle:Size
        ENDIF
        oFramePanel:Size := oSurfacePanel:Size
        SELF:AdjustFrameSize()

#region Winforms Method overrides
    OVERRIDE PROTECT METHOD OnShown(e AS EventArgs) AS VOID STRICT
        FOREACH oC AS SWF.Control IN SELF:Surface:Controls
            IF oC IS VOButton VAR oVOB
                IF oVOB:DefaultButton
                    SELF:AcceptButton := (SWF.IButtonControl) oVOB
                ENDIF
            ENDIF
        NEXT
        SUPER:OnShown(e)
        SELF:SelectFirstControl()
    PROTECTED METHOD SelectFirstControl() AS SWF.Control STRICT
        VAR oFirst := SELF:GetFirstEditableControl()
        IF oFirst != NULL_OBJECT
            oFirst:Select()
        ENDIF
        RETURN oFirst
    OVERRIDE PROTECT METHOD OnSizeChanged(e AS EventArgs ) AS VOID
        SUPER:OnSizeChanged(e)
        IF SELF:Visible
            SELF:AdjustSizes()
        ENDIF
        RETURN

#endregion
#region Other methods to preproduce the VO DataWIndow behavior


    METHOD AdjustSizes() AS VOID STRICT
        LOCAL oSize AS Size
        // This is called when resizing and/or when toolbar or statusbar is shown
        // it recalculates the size of the framepanel
        // DebOut(__FUNCTION__)
        SELF:SuspendLayout()
        IF ! SELF:SubForm
            oSize := SELF:ClientRectangle:Size
            IF oToolBar != NULL_OBJECT
                oSize:Height -= oToolBar:Height
                SELF:oFramePanel:Location := System.Drawing.Point{0,oToolBar:Height}
            ELSE
                SELF:oFramePanel:Location := System.Drawing.Point{0,0}
            ENDIF
            IF oStatusBar != NULL_OBJECT //.and. oStatusBar:Visible
                oSize:Height -= oStatusBar:Height
            ENDIF
            IF oFramePanel:Size:Width != oSize:Width .OR. oFramePanel:Size:Height != oSize:Height
                oFramePanel:Size := oSize
            ENDIF
        ENDIF
        IF lInBrowseView
            oDataBrowser:Dock := SWF.DockStyle.Fill
            oFramePanel:AutoScroll := FALSE
        ELSE
            oFramePanel:AutoScroll := TRUE
        ENDIF
        SELF:ResumeLayout()
        RETURN

    METHOD AdjustFrameSize() AS VOID STRICT
        // Adjust outer window so it fits just around the surface
        LOCAL oOuterSize AS Size
        LOCAL oInnerSize AS Size
        LOCAL nBordX, nBordY AS LONG
        LOCAL oLoc       AS System.Drawing.Point
        SELF:SuspendLayout()
        //DebOut(__FUNCTION__)
        IF SELF:SubForm
            NOP
        ELSE
            oLoc       := SELF:Location
            oOuterSize := SELF:Size
            oInnerSize := SELF:ClientRectangle:Size
            nBordX     := oOuterSize:Width - oInnerSize:Width
            nBordY     := oOuterSize:Height - oInnerSize:Height
            // Calculate wanted outer size based on Surface Size
            //oSurfacePanel:MinimumSize := System.Drawing.Size.Empty
            oSurfacePanel:AutoSizeMode := SWF.AutoSizeMode.GrowAndShrink
            oSurfacePanel:PerformLayout()
            oInnerSize := oSurfacePanel:Size
            SELF:oFramePanel:Size := oInnerSize
            oOuterSize:Width := oInnerSize:Width + nBordX + INSIDEFORMBORDER
            oOuterSize:Height := oInnerSize:Height + nBordY + INSIDEFORMBORDER
            IF oToolBar != NULL_OBJECT //.and. oToolBar:Visible

                oOuterSize:Height += oToolBar:Height
            ENDIF
            IF oStatusBar != NULL_OBJECT //.and. oStatusBar:Visible
                oOuterSize:Height += oStatusBar:Height
            ENDIF
            SELF:Size := oOuterSize
            SELF:Location := oLoc
        ENDIF
        SELF:ResumeLayout()
        RETURN

    METHOD ChangeFormSize(oDim AS Size) AS VOID
        //DebOut(__FUNCTION__)
        SELF:Size := oDim
        RETURN


    METHOD CreateSubForm(nResourceID AS LONG, oResDlg AS ResourceDialog) AS LOGIC
        LOCAL oItem AS ResourceDialogItem
        SELF:SubForm := TRUE
        oItem := oResDlg:GetDlgItem(nResourceID)
        IF oItem != NULL_OBJECT
            SELF:oFramePanel:Location	 := oItem:Origin
            SELF:oFramePanel:Size		 := oItem:Size
            SELF:oFramePanel:DefinedSize := oItem:Size
            SELF:oFramePanel:Visible	 := TRUE
            SELF:oFramePanel:AutoScroll  := FALSE
            SELF:oFramePanel:IsSubForm   := TRUE
            IF oItem:TabIndex > 0
                SELF:oFramePanel:TabIndex	:= oItem:TabIndex
            ENDIF
        ENDIF
        RETURN TRUE

    METHOD ResetMinSize() AS VOID
        RETURN


    METHOD HideSubForm () AS VOID STRICT
        SELF:Frame:Visible	:= FALSE
        RETURN


    METHOD ShowSubForm () AS VOID STRICT
        SELF:oFramePanel:Visible	 := TRUE
        IF SELF:lInBrowseView
            SELF:oDataBrowser:Visible := TRUE
            SELF:oDataBrowser:Dock 	:= SWF.DockStyle.Fill
            SELF:oFramePanel:Size 	:= SELF:oFramePanel:DefinedSize
        ELSE
            SELF:oSurfacePanel:Visible := TRUE
        ENDIF
        SELF:Frame:Prepare()
        RETURN

    METHOD SetFocusToForm ( ) AS VOID STRICT
        IF SELF:lInBrowseView
            oDataBrowser:Focus()
        ELSEIF (oSurfacePanel  != NULL_OBJECT)
            oSurfacePanel:Focus()
        ENDIF
        RETURN

    METHOD ViewAs(lBrowse AS LOGIC) AS VOID
        oFramePanel:SuspendLayout()
        IF lBrowse
            IF (oDataBrowser== NULL_OBJECT)
                RETURN
            ENDIF
            oSurfacePanel:Visible := FALSE
            IF oFramePanel:Controls:Contains(oSurfacePanel)
                oFramePanel:Controls:Remove(oSurfacePanel)
            ENDIF
            oFramePanel:Controls:Add(oDataBrowser)
            oFramePanel:AutoScroll := FALSE
            oDataBrowser:Visible := TRUE
            oDataBrowser:Dock 	 := SWF.DockStyle.Fill
            oDataBrowser:Focus()
        ELSE
            // Show the dialog and hide the GBRowse
            IF (oDataBrowser != NULL_OBJECT)
                oDataBrowser:Visible := FALSE
                IF oFramePanel:Controls:Contains(oDataBrowser)
                    oFramePanel:Controls:Remove(oDataBrowser)
                ENDIF
            ENDIF
            oFramePanel:Controls:Add(oSurfacePanel)
            oFramePanel:AutoScroll := TRUE
            oSurfacePanel:Visible := TRUE

        ENDIF
        oFramePanel:Prepare()
        oFramePanel:ResumeLayout(TRUE)
        SELF:lInBrowseView := lBrowse
        SELF:AdjustSizes()
        RETURN

    PUBLIC METHOD SuspendRedraw AS VOID
        //DebOut(__FUNCTION__)
        IF SELF:lInBrowseView
            GuiWin32.SendMessage(SELF:Frame:Handle, WM_SETREDRAW, 0, 0)
        ELSE
            GuiWin32.SendMessage(SELF:Surface:Handle, WM_SETREDRAW, 0, 0)
        ENDIF

    PUBLIC METHOD ResumeRedraw AS VOID
        //DebOut(__FUNCTION__)
        IF SELF:lInBrowseView
            GuiWin32.SendMessage(SELF:Frame:Handle, WM_SETREDRAW, 1, 0)
        ELSE
            GuiWin32.SendMessage(SELF:Surface:Handle, WM_SETREDRAW, 1, 0)
        ENDIF
        SELF:Refresh()

#endregion

END CLASS
