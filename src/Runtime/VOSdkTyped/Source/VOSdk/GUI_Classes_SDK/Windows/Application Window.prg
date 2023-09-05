//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Windows.Forms
/// <include file="Gui.xml" path="doc/AppWindow/*" />
CLASS AppWindow INHERIT Window
    PROTECT oVertScroll AS WindowVerticalScrollBar
    PROTECT oHorzScroll AS WindowHorizontalScrollBar
    PROTECT oStatusBar AS StatusBar
    PROTECT lQuitOnClose AS LOGIC

    /// <exclude />
    METHOD __CreateForm() AS VOForm STRICT
        RETURN GuiFactory.Instance:CreateAppWindow(SELF)

    /// <exclude />
    method __StatusMessageFromEvent(oEvent as MenuSelectEvent, nType as longint) as void strict
        LOCAL oHL AS HyperLabel

        oHL := oEvent:HyperLabel
        IF (oHL != NULL_OBJECT)
            SELF:@@StatusMessage(oHL, nType)
        ENDIF
        RETURN

    /// <inheritdoc />
    METHOD Activate(oEvent  AS Event) as USUAL
        SUPER:Activate(oEvent)
        IF SELF:__Form:IsMdiChild
            VAR oShell  := SELF:__Form:ParentForm
            oSHell:Menu := SELF:__Form:Menu
        ENDIF
        RETURN SELF



    /// <include file="Gui.xml" path="doc/AppWindow.Default/*" />
    METHOD Default(oEvent AS Event) as USUAL
        RETURN SELF


    /// <include file="Gui.xml" path="doc/AppWindow.Destroy/*" />
    METHOD Destroy() AS USUAL CLIPPER

        IF (oVertScroll != NULL_OBJECT)
            oVertScroll:Destroy()
            oVertScroll := NULL_OBJECT
        ENDIF
        IF oHorzScroll != NULL_OBJECT
            oHorzScroll:Destroy()
            oHorzScroll := NULL_OBJECT
        ENDIF
        IF oStatusBar != NULL_OBJECT
            oStatusBar:Destroy()
            oStatusBar := NULL_OBJECT
        ENDIF

        IF SELF:lQuitOnClose .and. oApp != NULL_OBJECT
            lQuitOnClose := FALSE
            oApp:Quit()
        ENDIF
        SUPER:Destroy()
        RETURN SELF

    /// <include file="Gui.xml" path="doc/AppWindow.Dispatch/*" />
    METHOD Dispatch(oEvt AS @@Event)
        //Todo Dispatch ?
        //LOCAL oEvt := oEvent AS @@Event
        //LOCAL dwMsg AS DWORD

        //LOCAL lHelpEnable AS LOGIC


        //dwMsg := oEvt:Message


        //CASE (dwMsg == WM_SETCURSOR)
        //	IF lHelpOn
        //		IF lHelpcursorOn
        //			lHelpEnable := TRUE
        //		ELSE
        //			lHelpEnable := FALSE
        //		ENDIF
        //	ELSE
        //		lHelpEnable := FALSE
        //	ENDIF
        //	// last par. changed from true (bug report l. atkins)
        //	SELF:__HandlePointer(oEvt, lHelpEnable, FALSE)
        //	RETURN SELF:EventReturnValue


        //CASE (dwMsg == WM_NCLBUTTONDOWN)
        //	IF lHelpOn
        //		IF lHelpcursorOn
        //			RETURN SELF:EventReturnValue
        //		ENDIF
        //	ENDIF

        //CASE (dwMsg == WM_LBUTTONDOWN)
        //	// ***********************************
        //	// Ignore if we're in Help Cursor mode
        //	// ***********************************
        //	IF !lHelpOn .OR. !lHelpcursorOn
        //		SELF:MouseButtonDown(MouseEvent{oEvt})
        //		//SELF:MouseButtonDown(__ObjectCastClassPtr(oEvt, __pCMouseEvent))
        //	ENDIF
        //	RETURN SELF:EventReturnValue

        //ENDCASE


        RETURN SUPER:Dispatch(oEvt)

    /// <inheritdoc />
    METHOD MenuSelect(oEvent AS MenuSelectEvent) AS USUAL
        SELF:__StatusMessageFromEvent(oEvent, MESSAGEMENU)
        RETURN SELF

    /// <include file="Gui.xml" path="doc/AppWindow.EnableBorder/*" />
    METHOD EnableBorder(kBorderStyle)

        DEFAULT( REF kBorderStyle, WINDOWSIZINGBORDER)
        IF SELF:__IsValid
            DO CASE
            CASE kBorderStyle == WINDOWNOBORDER
                SELF:oWnd:FormBorderStyle := FormBorderStyle.None
            CASE kBorderStyle == WINDOWNONSIZINGBORDER
                SELF:oWnd:FormBorderStyle := FormBorderStyle.FixedSingle
            CASE kBorderStyle == WINDOWSIZINGBORDER
                SELF:oWnd:FormBorderStyle := FormBorderStyle.Sizable
            END CASE
        ENDIF
        RETURN NIL


    /// <include file="Gui.xml" path="doc/AppWindow.EnableHorizontalScroll/*" />
    METHOD EnableHorizontalScroll(lEnable := TRUE AS LOGIC) AS VOID
        IF oWnd IS VOAppForm VAR oAppForm
            oAppForm:EnableHorizontalScroll(lEnable)
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/AppWindow.EnableMaxBox/*" />
    METHOD EnableMaxBox(lEnable := TRUE AS LOGIC) AS VOID
        IF SELF:__IsValid
            SELF:oWnd:MaximizeBox := lEnable
        ENDIF
        RETURN


    /// <include file="Gui.xml" path="doc/AppWindow.EnableMinBox/*" />
    METHOD EnableMinBox(lEnable:= TRUE AS LOGIC) AS VOID
        IF SELF:__IsValid
            SELF:oWnd:MinimizeBox := lEnable
        ENDIF
        RETURN
    /// <include file="Gui.xml" path="doc/AppWindow.EnableOleDropTarget/*" />
    METHOD EnableOleDropTarget(lEnable:= TRUE AS LOGIC) AS VOID
        //Todo EnableOleDropTarget
        RETURN

    /// <include file="Gui.xml" path="doc/AppWindow.EnableStatusBar/*" />
    METHOD EnableStatusBar(lEnable:= TRUE AS LOGIC) AS StatusBar

        IF lEnable
            IF (SELF:StatusBar == NULL_OBJECT)
                SELF:StatusBar := StatusBar{SELF}
            ENDIF
            SELF:StatusBar:DisplayMessage()
            SELF:StatusBar:Create()
        ELSEIF SELF:StatusBar != NULL_OBJECT
            SELF:StatusBar:Destroy()
            SELF:StatusBar := NULL_OBJECT
        ENDIF

        RETURN SELF:StatusBar


    /// <include file="Gui.xml" path="doc/AppWindow.EnableSystemMenu/*" />
    METHOD EnableSystemMenu(lEnable:= TRUE AS LOGIC) AS VOID
        IF SELF:__IsValid
            oWnd:ControlBox := lEnable
        ENDIF
        RETURN


    /// <include file="Gui.xml" path="doc/AppWindow.EnableToolBar/*" />
    METHOD EnableToolBar(lEnable:= TRUE AS LOGIC) AS VOID
        IF (SELF:ToolBar != NULL_OBJECT)
            IF lEnable
                SELF:ToolBar:Show()
            ELSE
                SELF:ToolBar:Hide()
            ENDIF
        ENDIF

        RETURN


    /// <include file="Gui.xml" path="doc/AppWindow.EnableVerticalScroll/*" />
    METHOD EnableVerticalScroll(lEnable:= TRUE AS LOGIC) AS VOID
        IF SELF:__IsValid .AND. oWnd IS VOAppForm VAR oAppForm
            oAppForm:EnableVerticalScroll(lEnable)
        ENDIF
        RETURN



    /// <include file="Gui.xml" path="doc/AppWindow.EndWindow/*" />
    METHOD EndWindow() AS USUAL STRICT
        RETURN SELF:EndWindow(FALSE)


    /// <include file="Gui.xml" path="doc/AppWindow.EndWindow/*" />
    METHOD EndWindow(lSendMsg AS LOGIC) AS USUAL
        IF SELF:__IsValid
            IF lSendMsg
                oWnd:Close()
            ELSE
                GuiWin32.PostMessage(SELF:Handle(), WM_CLOSE, 0,0)
            ENDIF
        ENDIF
        RETURN SELF

    /// <include file="Gui.xml" path="doc/AppWindow.ErrorMessage/*" />
    METHOD ErrorMessage(uText AS STRING) AS VOID
        ErrorBox{SELF, uText}
        RETURN


    /// <include file="Gui.xml" path="doc/AppWindow.ctor/*" />
    CONSTRUCTOR(oOwner)
        SUPER(oOwner)
        RETURN

    /// <include file="Gui.xml" path="doc/AppWindow.OLEDragEnter/*" />
    METHOD OLEDragEnter(oOleDragEvent)
        //Also empty in GUI Classes
        RETURN TRUE

    /// <include file="Gui.xml" path="doc/AppWindow.OLEDragLeave/*" />
    METHOD OLEDragLeave(oOleDragEvent)
        //Also empty in GUI Classes
        RETURN TRUE

    /// <include file="Gui.xml" path="doc/AppWindow.OLEDragOver/*" />
    METHOD OLEDragOver(oOleDragEvent)
        //Also empty in GUI Classes
        RETURN TRUE

    /// <include file="Gui.xml" path="doc/AppWindow.OLEDrop/*" />
    METHOD OLEDrop(oOleDragEvent)
        //Also empty in GUI Classes
        RETURN TRUE


    /// <include file="Gui.xml" path="doc/AppWindow.OLEInPlaceActivate/*" />
    METHOD OLEInPlaceActivate()
        //Also empty in GUI Classes
        RETURN NIL

    /// <include file="Gui.xml" path="doc/AppWindow.OLEInPlaceDeactivate/*" />
    METHOD OLEInPlaceDeactivate()
        //Also empty in GUI Classes
        RETURN NIL

    /// <include file="Gui.xml" path="doc/AppWindow.QuitOnClose/*" />
    PROPERTY QuitOnClose AS LOGIC GET lQuitOnClose SET lQuitOnClose := Value

    /// <include file="Gui.xml" path="doc/AppWindow.ReportException/*" />
    METHOD ReportException(oRQ)
        //Also empty in GUI Classes
        RETURN NIL

    /// <include file="Gui.xml" path="doc/AppWindow.ReportNotification/*" />
    METHOD ReportNotification(oRQ)
        //Also empty in GUI Classes
        RETURN NIL

    /// <include file="Gui.xml" path="doc/AppWindow.Show/*" />
    METHOD Show(nShowState) AS VOID CLIPPER
        IF oStatusBar != NULL_OBJECT
            oStatusBar:Show()
        ENDIF
        SUPER:Show(nShowState)
        RETURN

    /// <include file="Gui.xml" path="doc/AppWindow.StatusBar/*" />
    PROPERTY StatusBar AS StatusBar GET oStatusBar SET oStatusBar := value

    /// <include file="Gui.xml" path="doc/AppWindow.StatusMessage/*" />
    METHOD StatusMessage(oHL, nType)
        LOCAL Message AS STRING
        LOCAL oStatBar AS StatusBar
        LOCAL oOwner AS OBJECT

        DEFAULT( REF nType, MESSAGETRANSIENT)

        IF SELF:StatusBar != NULL_OBJECT
            oStatBar := SELF:StatusBar
        ELSE
            oOwner := SELF:Owner
            if oOwner is AppWindow var oWin
                oStatBar := oWin:StatusBar
            ENDIF
        ENDIF

        IF oStatBar != NULL_OBJECT
            DO CASE
            case oHL is HyperLabel var oHyperLabel
                Message := oHyperLabel:Description
            CASE IsString(oHL)
                Message := oHL
            OTHERWISE
                Message := ""
            ENDCASE

            oStatBar:SetMessage(Message, nType)
        ENDIF
        RETURN NIL

    PROPERTY HorizontalScrollBar AS WindowHorizontalScrollBar GET oHorzScroll
    PROPERTY VerticalScrollBar  as WindowVerticalScrollBar GET oVertScroll


    /// <include file="Gui.xml" path="doc/AppWindow.WarningMessage/*" />
    METHOD WarningMessage(aPlace1, aPlace2)
        RETURN WarningBox{SELF, aPlace1, aPlace2}:Show()

END CLASS


