//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Reflection
USING SWF := System.Windows.Forms
/// <include file="Gui.xml" path="doc/Control/*" />
[XSharp.Internal.TypesChanged];
CLASS Control INHERIT VObject IMPLEMENTS IGuiObject, ITimer
    protect oCtrl                as SWF.Control
    protect oParent              as IControlParent

        // The surface window that owns the control. This is the same as oParent except
        // when oParent                                   is a data window. Then oFormSurface is the DataWindow's dialog
    PROTECT oFormSurface         AS IControlParent
    PROTECT wId                  AS LONGINT
    PROTECT lDataAware           AS LOGIC
    PROTECT lIsDestroyed         AS LOGIC
    PROTECT oContextMenu         AS Menu

    PROTECT oSize                AS Dimension
    PROTECT oOrigin              AS Point
    PROTECT cWindowName          AS STRING
    PROTECT cClassName           AS STRING
    PROTECT dwStyle              AS LONG
    PROTECT dwExStyle            AS LONG
    PROTECT cCaption             AS STRING
    PROTECT uValue               AS USUAL
    PROTECT oFieldSpec           AS FieldSpec
    PROTECT lChanged             AS LOGIC
    PROTECT __lModified          AS LOGIC
    PROTECT oHyperLabel          AS HyperLabel
    PROTECT lExplicitHL          AS LOGIC
    PROTECT oServer              AS DataServer
    PROTECT lBaseServer          AS LOGIC
    PROTECT siDataField          AS LONGINT
    PROTECT symDataField         AS SYMBOL
    PROTECT oHLStatus            AS HyperLabel
    PROTECT oControlBackground   AS Brush
    PROTECT lExplicitFS          AS LOGIC
    PROTECT oDataField           AS DataField
    PROTECT uGetSetOwner         AS USUAL
    PROTECT cbGetSetBlock        AS CODEBLOCK
    PROTECT dwTimerCount         AS INT
    PROTECT dwTimerInterval      AS INT
    PROTECT lTimerRegistered     AS LOGIC
    PROTECT sToolTipText         AS STRING
    PROTECT lUseHLForToolTip     AS LOGIC
    PROTECT oControlWindow		 AS ControlWindow

    PROPERTY EventReturnValue      AS LONGINT AUTO

    PROPERTY __IsValid		AS LOGIC GET oCtrl != NULL_OBJECT .and. ! oCtrl:IsDisposed
    PROPERTY ControlType    AS Controltype GET ControlType.Control

    PROPERTY __Handle as IntPtr GET SELF:Handle()

    assign Parent(oP as Window)
        SELF:oParent := oP

    PROPERTY __ControlWindow AS ControlWindow
    GET
        RETURN oControlWindow
    END GET
    SET
        oControlWindow := value
        IF SELF:oParent == NULL_OBJECT
            oParent := value
        ENDIF
    END SET
    END PROPERTY

    METHOD OnHandleCreated(o AS OBJECT, e AS EventArgs) AS VOID
        IF oFormSurface != NULL_OBJECT
            oFormSurface:__AddTool(SELF)
        ENDIF
        lIsDestroyed := FALSE
        RETURN

    METHOD OnHandleDestroyed(o AS OBJECT, e AS EventArgs) AS VOID
        lIsDestroyed := TRUE
        RETURN

    method __CreateControl(liStyle as long, liExStyle as long) as SWF.Control
        RETURN GuiFactory.Instance:CreateControl(SELF:ControlType, SELF, liStyle, liExStyle)

    METHOD OnControlCreated(oC AS IVOControl) AS VOID
        RETURN

    property __Control as SWF.Control get oCtrl

    METHOD __AddTool(oControl AS Control) AS LOGIC STRICT
        RETURN oParent:__AddTool(oControl)



    /// <exclude />
    PROPERTY __DataField AS DataField GET oDataField

    METHOD __EnsureVisibity() AS LOGIC STRICT
        //Todo __EnsureVisibity
        /*
        LOCAL rCtl, rParent IS _winRECT
        LOCAL oPoint AS Point
        LOCAL lVScrollPos, lHScrollPos AS LONGINT
        LOCAL iXCorrect AS INT
        LOCAL hFFrame AS PTR
        IF (oParent:__FormWindow == NULL_OBJECT)
        RETURN FALSE
        ENDIF

        hFFrame := oParent:__FormWindow:Handle()
        IF (hFFrame == NULL_PTR)
        RETURN FALSE
        ENDIF

        GetWindowRect(SELF:Handle(), @rCtl)
        GetClientRect(hFFrame, @rParent)
        #ifdef __VULCAN__
        ClientToScreen(hFFrame, (_winPOINT PTR) @rParent)
        #else
        ClientToScreen(hFFrame, @rParent)
        #endif
        ClientToScreen(hFFrame, (_winPOINT PTR) @(rParent:right))

        IF (rCtl:left < rParent:left) .OR. (rCtl:right > rParent:right) .OR.;
        (rCtl:top < rParent:top) .OR. (rCtl:bottom > rParent:bottom)
        oPoint := oFormSurface:Origin

        lHScrollPos := GetScrollPos(hFFrame, SB_HORZ)
        lVScrollPos := GetScrollPos(hFFrame, SB_VERT)

        IF (rCtl:left < rParent:left)
        oPoint:X += (rParent:left - rCtl:left)
        SetScrollPos(hFFrame, SB_HORZ, lHScrollPos - (rParent:left - rCtl:left), TRUE)
        ELSEIF (rCtl:right > rParent:right)
        iXCorrect := Math.Min((rCtl:right - rParent:right), (rCtl:left - rParent:left))
        oPoint:X -= iXCorrect
        SetScrollPos(hFFrame, SB_HORZ, lHScrollPos + iXCorrect, TRUE)
        ENDIF

        IF (rCtl:top < rParent:top)
        oPoint:Y -= (rParent:top - rCtl:top)
        SetScrollPos(hFFrame, SB_VERT, lVScrollPos - (rParent:top - rCtl:top), TRUE)
        ELSEIF (rCtl:bottom > rParent:bottom)
        oPoint:Y += (rCtl:bottom - rParent:bottom)
        SetScrollPos(hFFrame, SB_VERT, lVScrollPos + (rCtl:bottom - rParent:bottom), TRUE)
        ENDIF

        oFormSurface:Origin := oPoint
        ENDIF
        */
        RETURN TRUE


    /// <exclude />
    PROPERTY __FormSurface AS OBJECT GET oFormSurface


    /// <exclude />
    METHOD __Gather() AS LOGIC STRICT
        LOCAL lReturn:=TRUE AS LOGIC
        // Get value from control if necessary
        SELF:__Update()

        IF lChanged
            IF oServer != NULL_OBJECT
                IF lBaseServer // if not subclassing
                    oServer:FIELDPUT(siDataField,SELF:Value) //use FieldPut
                ELSEIF (symDataField != NULL_SYMBOL) //else use assigns
                    IVarPut(oServer, symDataField ,SELF:Value)
                ELSE
                    IVarPut(oServer, SELF:NameSym ,SELF:Value)
                ENDIF
                lReturn := (oServer:Status == NIL)
                IF lReturn
                    SELF:Modified := FALSE
                    SELF:ValueChanged := FALSE
                ENDIF
            ELSEIF cbGetSetBlock != NULL_CODEBLOCK
                IF (uGetSetOwner == NIL)
                    Eval(cbGetSetBlock, SELF:Value)
                ELSE
                    Eval(cbGetSetBlock, uGetSetOwner, SELF:Value)
                ENDIF
            ENDIF
        ENDIF
        RETURN lReturn


    /// <exclude />
    PROPERTY __GetDataFldPos AS LONGINT GET siDataField
    /// <exclude />
    [Obsolete];
    METHOD __GetDispInfo(oCtrlNotifyEvent AS ControlNotifyEvent) AS VOID STRICT
        //processes LVN_GETDISPINFO, TVN_GETDISPINFO, CBEN_GETDISPINFO, TBN_GETDISPINFO, HDN_GETDISPINFO
        //see Window:ControlNotify()
        RETURN

    /// <exclude />
    property __Parent as Window get (Window) oParent


    /// <exclude />
    METHOD __Scatter() AS Control STRICT
        LOCAL newValue AS USUAL
        LOCAL cFSValType AS STRING

        IF oServer != NULL_OBJECT
            IF lBaseServer // if not subclassing
                newValue := oServer:FIELDGET(symDataField) //use fieldget
            ELSEIF symDataField != NULL_SYMBOL //else use Access
                newValue := IVarGet(oServer, symDataField)
            ELSE
                newValue := IVarGet(oServer, SELF:NameSym)
            ENDIF
            IF IsString(newValue)
                SELF:__Value:=RTrim(newValue)
            ELSE
                SELF:__Value:=newValue
            ENDIF
            oHLStatus := NULL_OBJECT
            SELF:ValueChanged := FALSE
        ELSEIF (SELF:Value == NIL) .AND. oFieldSpec != NULL_OBJECT
            cFSValType := oFieldSpec:ValType
            SWITCH cFSValType
            CASE "C"
            CASE "M"
                SELF:Value := NULL_STRING
            CASE "D"
                SELF:Value := NULL_DATE
            CASE "N"
                SELF:Value := 0
            CASE "L"
                SELF:Value := FALSE
            END SWITCH
        ENDIF

        RETURN SELF

    /// <exclude />
    [Obsolete];
    METHOD __SetColors(_hDC AS IntPtr) AS IntPtr STRICT
        RETURN IntPtr.Zero

    /// <exclude />
    METHOD __Timer() AS VOID STRICT
        dwTimerCount := dwTimerCount - 1
        IF (dwTimerCount == 0)
            SELF:Timer()
            dwTimerCount:=dwTimerInterval
            IF (dwTimerCount == 0)
                WC.UnregisterTimer(SELF)
                lTimerRegistered:=FALSE
            ENDIF
        ENDIF

        RETURN


        [Obsolete];
    METHOD __ToolTipHandle() AS IntPtr STRICT
        // Todo __EnsureVisibity
        //	RETURN oParent:__ToolTipHandle()
        RETURN IntPtr.Zero

    /// <exclude />
    METHOD __Unlink(oDataServer := NULL_OBJECT AS DataServer) AS Control STRICT

        IF SELF:Modified
            SELF:__Update()
            IF SELF:PerformValidations(uValue)
                SELF:__Gather()
            ENDIF
        ENDIF

        IF oDataServer == NULL_OBJECT
            oDataServer := SELF:oServer
        ENDIF

        // Do actual unlinking
        IF oDataServer == NULL_OBJECT .or. ;
            (oDataServer == SELF:oServer)
            SELF:uGetSetOwner:= NIL
            SELF:cbGetSetBlock:= NULL_CODEBLOCK
            SELF:oDataField:= NULL_OBJECT
            SELF:oServer:= NULL_OBJECT
        ENDIF
        RETURN SELF

    /// <exclude />
    METHOD __Update() AS VOID STRICT
        LOCAL cText AS STRING
        LOCAL uOldValue AS USUAL

        IF SELF:Modified
            cText := SELF:TextValue
            uOldValue := uValue
            IF oFieldSpec != NULL_OBJECT
                uValue := oFieldSpec:Val(cText)
                IF SLen(oFieldSpec:Picture) > 0
                    SELF:TextValue := oFieldSpec:Transform(uValue)
                ENDIF
            ELSE
                uValue := cText
            ENDIF
            SELF:Modified := FALSE

            SELF:lChanged := !(uOldValue == uValue)
        ENDIF
        RETURN

    /// <exclude />
    PROPERTY __Value AS USUAL
        SET
            LOCAL uTemp AS USUAL

            IF value == NIL
                uValue := NIL
                SELF:TextValue := ""
                SELF:oHLStatus := NULL_OBJECT
                SELF:Modified := FALSE
                SELF:lChanged := TRUE
            ELSEIF oFieldSpec != NULL_OBJECT
                uTemp := oFieldSpec:Transform(value)
                IF IsNil(uTemp)
                    oHLStatus := oFieldSpec:Status
                    WCError{#__Value,#Control,__WCSTypeError}:Throw()
                ELSE
                    SELF:uValue := oFieldSpec:Val(uTemp)
                    SELF:TextValue := uTemp
                    SELF:oHLStatus := NULL_OBJECT
                    SELF:Modified := FALSE
                    SELF:lChanged := TRUE
                ENDIF
            ELSE
                SELF:uValue := value
                SELF:TextValue := AsString(value)
                SELF:oHLStatus := NULL_OBJECT
                SELF:Modified := FALSE
                SELF:lChanged := TRUE
            ENDIF

        END SET
    END PROPERTY


    /// <include file="Gui.xml" path="doc/Control.Activate/*" />
    METHOD Activate(oEvent  AS Event ) AS VOID
        // Also empty in GUI Classes
        RETURN

    METHOD AddChild(oC AS OBJECT) AS VOID STRICT
        IF SELF:__IsValid
            DO CASE
            CASE oC IS Control VAR oCtrl
                SELF:oCtrl:Controls:Add((System.Windows.Forms.Control) oCtrl:__Control)
            CASE oC IS DataWindow VAR oDW
                SELF:oCtrl:Controls:Add((System.Windows.Forms.Control) oDW:__Frame)
            CASE oC IS Window VAR oWin
                SELF:oCtrl:Controls:Add((System.Windows.Forms.Control) oWin:__Surface)
            ENDCASE
        ENDIF
        RETURN

    METHOD AsString() as string strict
        IF !IsNil(SELF:uValue)
            RETURN AsString(SELF:uValue)
        ENDIF
        RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption

    /// <include file="Gui.xml" path="doc/Control.Background/*" />
    PROPERTY Background AS Brush
    GET
        RETURN oControlBackground
    end get
    set
        oControlBackground := value
        IF SELF:__IsValid .and. value != NULL_OBJECT
            oCtrl:BackColor := value:Color
        ENDIF
        RETURN
    end set
    end property
    METHOD BringToFront() AS VOID CLIPPER
        IF SELF:ValidateControl()
            oCtrl:BringToFront()
        ENDIF
    /// <include file="Gui.xml" path="doc/Control.Caption/*" />
    PROPERTY Caption AS STRING GET cCaption SET cCaption := Value

    /// <include file="Gui.xml" path="doc/Control.ContextMenu/*" />
    PROPERTY ContextMenu AS Menu
        GET
            RETURN oContextMenu
        END GET
        SET
            IF SELF:__IsValid
                oContextMenu := VALUE
                IF oCtrl IS System.Windows.Forms.Control VAR oC
                    IF oContextMenu != NULL_OBJECT
                        oC:ContextMenu := oContextMenu:__Menu:AsContextMenu()
                    ELSE
                        oC:ContextMenu := NULL_OBJECT
                    ENDIF
                ENDIF
            ENDIF
        END SET
    END PROPERTY

    property SWFControl as IVOControl get oCtrl astype IVOControl
    /// <include file="Gui.xml" path="doc/Control.ControlID/*" />
    PROPERTY ControlID AS LONG GET SELF:wId

    METHOD CreateWindowEx(dwExStyle AS LONG, sCaption AS STRING, dwStyle AS LONG,;
            nX as long, nY as long, nWidth as long, nHeight as long, oOwner as Window) as SWF.Control
        oCtrl  := 	SELF:__CreateControl(dwStyle , dwExStyle )

        IF oCtrl != NULL_OBJECT
            // Determine where to add the control
            // Some controls are added to the surface, others to the form
            oCtrl:HandleDestroyed += OnHandleDestroyed
            oCtrl:HandleCreated   += OnHandleCreated

            IF !STRING.IsNullOrEmpty(sCaption)
                oCtrl:Text := sCaption
            ENDIF
            IF oOwner != NULL_OBJECT
                IF oCtrl IS VOStatusBar VAR oSB
                    oOwner:__Form:AddControl(oSB)
                ELSEIF oCtrl IS VOToolBar VAR oTB
                    oOwner:__Form:AddControl(oTB)
                ELSEIF oCtrl IS VODataGridView VAR oGrid
                    oOwner:__Form:AddControl(oGrid)
                    IF oOwner IS DataWindow
                        LOCAL oDataForm AS VODataForm
                        oDataForm := (VODataForm) oOwner:__Form
                        oDataForm:DataBrowser := oGrid
                    ENDIF
                ELSEIF oOwner:__HasSurface
                    oOwner:__Surface:AddControl(oCtrl)
                ELSE
                    oOwner:__Form:AddControl(oCtrl)
                ENDIF
            ENDIF
            oCtrl:Location := Point{nX, nY}
            oCtrl:Size     := Dimension{nWidth, nHeight}
        ENDIF
        RETURN oCtrl

    /// <include file="Gui.xml" path="doc/Control.Create/*" />
    method Create() as SWF.Control strict
        LOCAL oDlgItem AS ResourceDialogItem
        LOCAL oFont AS System.Drawing.Font
        LOCAL oDevPoint as Point
        LOCAL oOwner AS OBJECT
        LOCAL nTabIndex AS LONG
        // don't create twice
        IF SELF:__IsValid
            RETURN oCtrl
        ENDIF

        IF !lIsDestroyed

            IF (oCtrl == NULL_OBJECT)
                // See if the control has been defined in a native resource. When so, then copy some of the properties
                // from the resource, such as position, size, styles and caption
                IF oFormSurface != NULL_OBJECT
                    oDlgItem := SELF:oFormSurface:GetDlgItem(SELF:wId)
                ENDIF
                IF oDlgItem != NULL_OBJECT
                    oOrigin := oDlgItem:Origin
                    oSize   := oDlgItem:Size
                    IF STRING.IsNullOrEmpty(cClassName)
                        cClassName := oDlgItem:WindowClassName
                    ENDIF
                    dwStyle		:= oDlgItem:Style
                    dwExStyle	:= oDlgItem:ExStyle
                    cCaption	:= oDlgItem:Caption
                    oDevPoint	:= oDlgItem:Origin
                    nTabIndex   := oDlgItem:TabIndex
                ELSE
                    nTabIndex   := -1
                    oDevPoint := Point{oOrigin:X, oOrigin:Y}
                    IF WC.CoordinateSystem == WC.CartesianCoordinates
                        oDevPoint:Y := oDevPoint:Y - oSize:Height
                        oDevPoint := WC.ConvertPoint(oFormSurface, oDevPoint)
                    ENDIF
                ENDIF
                oOwner := oParent
                DO WHILE oOwner != NULL_OBJECT .AND. ! (oOwner IS Window)
                    IF IsAccess(oOwner, #Owner)
                        oOwner := IVarGet(oOwner, #Owner)
                    ELSE
                        oOwner := NULL_OBJECT
                    ENDIF
                ENDDO
                oCtrl:= SELF:CreateWindowEx(dwExStyle, cWindowName, dwStyle, oDevPoint:X, oDevPoint:Y, oSize:Width, oSize:Height, (Window) oOwner)
                IF oCtrl != NULL_OBJECT .and. nTabIndex > 0
                    oCtrl:TabIndex := nTabIndex
                ENDIF
            ENDIF

            IF (oCtrl != NULL_OBJECT)
                oFont := System.Drawing.SystemFonts.DefaultFont
                oCtrl:Font := oFont

            ELSE
                // !!! Maybe we should remove this before shipping !!!
                WCError{#Create,#Control,__WCSCreateCtlFailed,SELF}:Throw()
            ENDIF
        ENDIF
        return oCtrl

    /// <include file="Gui.xml" path="doc/Control.DataField/*" />
    PROPERTY DataField  AS SYMBOL GET SELF:symDataField

    /// <include file="Gui.xml" path="doc/Control.Deactivate/*" />
    METHOD Deactivate(oEvent AS @@Event) AS VOID
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.Default/*" />
    METHOD Default(oEvent AS Event) AS VOID
        //Todo Default
        // LOCAL oEvt := oEvent AS @@event
        // SELF:EventReturnValue := CallWindowProc(SELF:__lpfnDefaultProc, oCtrl, oEvt:umsg, oEvt:wParam, oEvt:lParam)

        RETURN

    /// <include file="Gui.xml" path="doc/Control.Destroy/*" />
    METHOD Destroy() AS USUAL CLIPPER

        IF oCtrl != NULL_OBJECT
            IF ! oCtrl:IsDisposed
                // Prevent calling Dispose in a background thread
                IF SWF.Application.MessageLoop .AND. oCtrl IS System.Windows.Forms.Control VAR oC
                    oC:Dispose()
                ENDIF
            ENDIF
            oCtrl := NULL_OBJECT
        ENDIF
        oContextMenu := NULL_OBJECT
        UnregisterAxit(SELF)
        lTimerRegistered:=FALSE
        SUPER:Destroy()
        lIsDestroyed := TRUE
        RETURN SELF


    /// <include file="Gui.xml" path="doc/Control.Disable/*" />
    METHOD Disable() AS VOID STRICT
        IF SELF:__IsValid
            oCtrl:Enabled := FALSE
            oCtrl:Invalidate()
        ENDIF
        SELF:SetStyle(WS_DISABLED, TRUE)

        RETURN


    /// <include file="Gui.xml" path="doc/Control.DisableTheme/*" />
    METHOD DisableTheme() AS VOID STRICT
        //Todo DisableTheme
        //RETURN SetWindowTheme(SELF:handle(),"","")

    /// <include file="Gui.xml" path="doc/Control.Dispatch/*" />
    METHOD Dispatch(oEvent AS @@Event)
        /*
        LOCAL oEvt := oEvent AS @@event
        LOCAL msg AS DWORD
        LOCAL uRet AS USUAL


        SELF:EventReturnValue := 0L
        msg := oEvt:uMsg

        DO CASE
        CASE msg == WM_ERASEBKGND
        //RH PaintBackground is defined in our patch file
        IF SELF:PaintBackground(PTR(_CAST, oEvt:wParam))
        SELF:EventReturnValue := 1L
        RETURN 1L
        ENDIF

        CASE msg == WM_PAINT
        uRet := SELF:Expose(ExposeEvent{oEvt})

        CASE msg == WM_DRAWITEM
        //PP-031006 owner draw support, thanks to SEbert
        //Used for owner drawn menus or controls
        RETURN __Dispatch_DrawItem(oEvt, SELF)

        CASE msg == WM_MEASUREITEM
        //PP-031006 owner draw support
        //Used for owner drawn menus or controls
        RETURN __Dispatch_MeasureItem(oEvt, SELF)

        CASE msg == WM_MENUCHAR
        //PP-031006 owner draw support
        //Used for owner drawn menus or controls
        RETURN __Dispatch_MenuChar(oEvt, SELF)
        CASE msg == WM_WCHELP
        oParent:HelpRequest(HelpRequestEvent{oEvt})

        CASE msg == WM_ACTIVATE
        IF LoWord(oEvt:wParam) != 0
        uRet := SELF:Activate(oEvt)
        ELSE
        uRet := SELF:Deactivate(oEvt)
        ENDIF

        // WM_COMMAND trigged by (popup) menu
        CASE (msg == WM_COMMAND) .AND. (HiWord(oEvt:wParam) == 0) .AND. (oEvt:lParam == 0L)
        IF (oContextMenu != NULL_OBJECT)
        oParent:__PreMenuCommand(MenuCommandEvent{oEvt}:__SetMenu(oContextMenu))
        RETURN 1L
        ENDIF

        //PP-040421 Improved focus handling
        CASE msg == WM_SETFOCUS .OR. msg == WM_KILLFOCUS
        uRet := SELF:FocusChange(FocusChangeEvent{oEvt})
        CASE msg == WM_HSCROLL
        uRet := SELF:HorizontalScroll(ScrollEvent{oEvt})

        CASE msg == WM_KEYUP
        uRet := SELF:KeyUp(KeyEvent{oEvt})

        CASE msg == WM_KEYDOWN
        uRet := SELF:KeyDown(KeyEvent{oEvt})


        CASE msg == WM_LBUTTONDBLCLK .OR.;
        msg == WM_RBUTTONDBLCLK .OR.;
        msg == WM_MBUTTONDBLCLK .OR. ;
        msg == WM_XBUTTONDBLCLK
        uRet := SELF:MouseButtonDoubleClick(MouseEvent{oEvt})

        CASE msg == WM_LBUTTONDOWN .OR.;
        msg == WM_RBUTTONDOWN .OR.;
        msg == WM_MBUTTONDOWN .OR. ;
        msg == WM_XBUTTONDOWN
        uRet := SELF:MouseButtonDown(MouseEvent{oEvt})

        CASE msg == WM_LBUTTONUP .OR.;
        msg == WM_RBUTTONUP .OR.;
        msg == WM_MBUTTONUP .OR. ;
        msg == WM_XBUTTONUP
        uRet := SELF:MouseButtonUp(MouseEvent{oEvt})

        CASE msg == WM_VSCROLL
        uRet := SELF:VerticalScroll(ScrollEvent{oEvt})


        CASE msg == WM_MOUSEMOVE
        //IF IsMethod(oFormSurface, #__ToolTipHandle) .AND. (oFormSurface:__ToolTipHandle() != NULL_PTR)
        //	struMsg:oCtrl := oCtrl
        //	struMsg:message := WM_MOUSEMOVE
        //	struMsg:wParam := oEvt:wParam
        //	struMsg:lParam := oEvt:lParam
        //	GetCursorPos(@struMsg:pt)
        //	struMsg:time := DWORD(GetMessageTime())
        //	SendMessage(oFormSurface:__ToolTipHandle(), TTM_RELAYEVENT, 0, LONGINT(_CAST, @struMsg))
        //ENDIF

        IF oEvt:wParam != 0
        uRet := SELF:MouseDrag(MouseEvent{oEvt})
        ELSE
        uRet := SELF:MouseMove(MouseEvent{oEvt})
        ENDIF

        CASE msg == WM_MOVE
        uRet := SELF:Move(MoveEvent{oEvt})

        CASE msg == WM_SIZE
        uRet := SELF:Resize(ResizeEvent{oEvt})

        CASE (msg == WM_DROPFILES)
        IF IsMethod(oParent, #Drop)
        oParent:Drop(DragEvent{oEvt, SELF})
        ENDIF
        IF __LoadShellDll()
        PCALL(gpfnDragFinish, PTR(_CAST, oEvt:wParam))
        ENDIF
        RETURN 1L

        CASE (msg == WM_INITMENU) .OR. (msg == WM_INITMENUPOPUP)
        uRet := SendMessage(oParent:Handle(), msg, oEvt:wParam, oEvt:lParam)
        //if IsMethod(oParent, #MenuInit)
        //oParent:MenuInit(__ObjectCastClassPtr(oEvt, __pCMenuInitEvent))
        //endif

        CASE msg == WM_MENUSELECT
        uRet := SendMessage(oParent:Handle(), msg, oEvt:wParam, oEvt:lParam)
        //if (oEvent:lParam != 0) .and. IsMethod(oParent, #MenuSelect)
        //oParent:MenuSelect(__ObjectCastClassPtr(oEvt, __pCMenuSelectEvent))
        //endif

        //RvdH 050809 Removed next line
        //CASE msg == WM_CLOSE
        // __WCUnregisterControl(self)

        //PP-040425 improved context menu support, following code replaced with subsequent code
        // 	CASE msg == WM_CONTEXTMENU
        // 		IF (oContextMenu != NULL_OBJECT)
        // 			RETURN (SELF:EventReturnValue := 1L)
        // 		ENDIF

        //PP-040410 improved context menu support
        CASE msg == WM_CONTEXTMENU
        IF (oContextMenu != NULL_OBJECT)
        oContextMenu:ShowAsPopup(SELF, oEvt:lParam)
        RETURN (SELF:EventReturnValue := 1L)
        ENDIF

        CASE msg == WM_THEMECHANGED //SE-060526
        VerifyThemeState()
        InvalidateRect(oCtrl, NULL_PTR, TRUE)

        ENDCASE

        IF IsLong(uRet)
        SELF:EventReturnValue := uRet
        RETURN 1L
        ENDIF
        */
        RETURN SELF:EventReturnValue


    METHOD Drop(oDragEvent as DragEvent) AS VOID
        // Also empty in GUI Classes
        RETURN


    /// <include file="Gui.xml" path="doc/Control.Enable/*" />
    METHOD Enable() AS VOID STRICT
        IF SELF:ValidateControl()
            oCtrl:Enabled := TRUE
            oCtrl:Invalidate()
        ENDIF
        SELF:SetStyle(WS_DISABLED, FALSE)
        RETURN


    /// <include file="Gui.xml" path="doc/Control.Expose/*" />
    METHOD Expose(oExposeEvent as ExposeEvent) AS VOID
        // Also empty in GUI Classes
        RETURN


    PROPERTY FieldSpec AS FieldSpec
    GET
        RETURN oFieldSpec
    END GET
    SET
        oFieldSpec := value
        lExplicitFS := TRUE

        // We need to update the control if is visible and connected to the server
        IF oServer != NIL .AND. oDataField!=NIL .OR. cbGetSetBlock != NULL_CODEBLOCK
            IF SELF:IsVisible()
                // If its modified update it to server
                // update it from server
                SELF:__Scatter()
            ENDIF
        ENDIF
        RETURN
    end set
    end property

    /// <include file="Gui.xml" path="doc/Control.FocusChange/*" />
    METHOD FocusChange(oFocusChangeEvent AS FocusChangeEvent ) AS VOID
        IF oParent != NULL_OBJECT
            oParent:ControlFocusChange(ControlFocusChangeEvent{oFocusChangeEvent, SELF})
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/Control.Handle/*" />
    METHOD Handle() AS IntPtr CLIPPER
        IF (oCtrl == NULL_OBJECT )
            SELF:Create()
        ENDIF
        IF SELF:__IsValid
            RETURN oCtrl:Handle
        ENDIF
        RETURN Intptr.Zero

    PROTECTED PROPERTY hWnd AS IntPtr GET SELF:Handle()

    /// <include file="Gui.xml" path="doc/Control.HasBorder/*" />
    METHOD HasBorder() AS LOGIC STRICT
        LOCAL lBorder		AS LOGIC
        LOCAL nStyle		AS LONG

        nStyle := GuiWin32.GetWindowExStyle(SELF:hWnd)
        IF _AND(nStyle,WS_EX_DLGMODALFRAME)>0
            lBorder := TRUE
        ELSEIF _AND(nStyle,WS_EX_WINDOWEDGE)>0
            lBorder := TRUE
        ELSEIF _AND(nStyle,WS_EX_STATICEDGE)>0
            lBorder := TRUE
        ELSEIF _AND(nStyle,WS_EX_CLIENTEDGE)>0
            lBorder := TRUE
        ELSE
            nStyle := GuiWin32.GetWindowStyle(SELF:hWnd)
            IF _AND(nStyle,WS_BORDER)>0
                lBorder := TRUE
            ENDIF
        ENDIF
        RETURN lBorder

    /// <include file="Gui.xml" path="doc/Control.Hide/*" />
    METHOD Hide() AS VOID STRICT
        IF SELF:__IsValid
            oCtrl:Visible := FALSE
        ENDIF
        RETURN


    /// <include file="Gui.xml" path="doc/Control.HorizontalScroll/*" />
    METHOD HorizontalScroll(oScrollEvent as ScrollEvent) AS VOID
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.HyperLabel/*" />
    property HyperLabel AS HyperLabel
    GET
        RETURN oHyperLabel
    end get
    set
        oHyperLabel := value
        lExplicitHL := TRUE
        IF value != NULL_OBJECT
            SELF:Caption := value:Caption
            IF SELF:ValidateControl()
                oCtrl:AccessibleDescription := value:Description
                oCtrl:AccessibleName        := value:Name
            ENDIF
            IF SELF:UseHLForToolTip
                SELF:__AddTool(SELF)
            ENDIF
        ENDIF
    end set
    end property


    /// <include file="Gui.xml" path="doc/Control.ctor/*" />
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cRegClass, kStyle, lDataAware)
        super()
        if oOwner != null_object
            local oOwnerObject := oOwner as OBJECT
            if oOwnerObject is SWF.Control var ownerControl
                SELF:oCtrl    := ownerControl
                SELF:oOrigin  := oCtrl:Location
                SELF:oSize	  := oCtrl:Size
                IF oOwnerObject IS IVOControlProperties VAR oWrapper
                    oWrapper:SetOwner(SELF)
                ENDIF
                RETURN
            ENDIF
        ENDIF

        if oPoint is Point var oPt
            self:oOrigin 	:= Point{oPt:x, oPt:y}
        ENDIF
        if oDimension is Dimension var oDim
            self:oSize 		:= Dimension{oDim:Width, oDim:Height}
        ENDIF
        dwStyle := WS_CHILD
        IF !IsNil(kStyle)
            dwStyle := _OR(dwStyle, (LONG) kStyle)
        ENDIF

        oParent := oOwner


        IF IsObject(oOwner)
            oParent := oOwner
        ELSEIF IsPtr(oOwner)
            oParent := __ForeignWindow{oOwner}
        ELSE
            WCError{#Init,#Control,__WCSTypeError,oOwner,1}:Throw()
        ENDIF

        oFormSurface := oParent
        if IsLong(xID)
            IF IsString(oPoint) .AND. IsNil(oDimension)
                xID := ResourceID{ xID }
            ELSE
                wID := xID
            ENDIF
        ELSE
            IF !(xID IS ResourceID)
                WCError{#Init,#Control,__WCSTypeError,xID,2}:Throw()
            ENDIF
        ENDIF



        IF (oFormSurface IS DialogWindow .OR. oFormSurface IS DataWindow)  .AND. ;
                ((xID is ResourceID) .or. (IsLong(xID) .and.;
                oDimension == null .and. oPoint== null))

            IF xID IS ResourceID var resId
                wId 		:= resId:ID
            ELSE
                wId 		:= xID
            ENDIF

            dwStyle := WS_CHILD
            IF !IsNil(kStyle)
                dwStyle := _OR(dwStyle, (LONG) kStyle)
            ENDIF

            //oCtrl := GetDlgItem(oFormSurface:Handle(), wId)

            //__WCRegisterControl(SELF) //register after we get the handle
            //SetWindowStyle(oCtrl, _OR(GetWindowStyle(oCtrl), LONGINT(_CAST, dwStyle)))
            //SetWindowExStyle(oCtrl, LONGINT(_CAST, _AND(GetWindowExStyle(oCtrl), _NOT(WS_EX_NOPARENTNOTIFY))))

            //__lpfnDefaultProc := PTR(_CAST, GetWindowLong(oCtrl, GWL_WNDPROC))
            //SetWindowLong(oCtrl, GWL_WNDPROC, LONGINT(_CAST, Get__WCControlProcPtr()))

        elseif (oFormSurface is Window .or. oFormSurface is Control) .and. IsLong(xID)

            IF !IsNil(cRegClass) .AND. !IsString(cRegClass)
                WCError{#Init,#Control,__WCSTypeError,cRegClass,5}:Throw()
            ENDIF

            cClassName := cRegClass

            IF (cClassName == "Edit" .OR. cClassName == "ListBox" .OR. cClassName == "SysAnimate32" .OR.;
                    cClassName == "SysTreeView32" .OR. cClassName == "SysListView32" .OR. cClassName == "msctls_hotkey32")
                dwExStyle := _OR(dwExStyle, (LONG) WS_EX_CLIENTEDGE)
            ENDIF
            IF cClassName == "msctls_progress32"
                dwExStyle := _OR(dwExStyle, (LONG) WS_EX_CLIENTEDGE, (LONG) WS_EX_STATICEDGE)
            ENDIF
        ELSE
            WCError{#Init,#Control,__WCSTypeError}:Throw()
        ENDIF

        IF IsLogic(lDataAware)
            SELF:lDataAware:=lDataAware
        ENDIF

        IF SELF:lDataAware .AND. !(SELF IS DataBrowser) .AND. IsMethod(oParent, #__SetUpDataControl)
            SELF:Create()
            oParent:__SetupDataControl(SELF)
        ELSEIF oParent IS DataWindow VAR oDw
            SELF:Create()
            oDw:__SetupNonDataControl(SELF)
        ELSE
            SELF:Create()
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/Control.IsDestroyed/*" />
    PROPERTY IsDestroyed AS LOGIC GET SELF:lIsDestroyed

    /// <include file="Gui.xml" path="doc/Control.IsDisabled/*" />
    PROPERTY IsDisabled AS LOGIC
        GET
            RETURN SELF:__IsValid  .and. !oCtrl:Enabled
        END GET
        SET
            IF value
                SELF:Disable()
            ELSE
                SELF:Enable()
            ENDIF
            RETURN
        END SET
    END PROPERTY


    /// <include file="Gui.xml" path="doc/Control.IsEditable/*" />
    ACCESS IsEditable AS LOGIC
        LOCAL lEditable	AS LOGIC
        lEditable := FALSE
        IF SELF:__IsValid
            IF oCtrl:Visible
                IF oCtrl:Enabled
                    IF  SELF IS BASELISTBOX .OR. ;
                            SELF IS MONTHCALENDAR .OR. ;
                            SELF IS DATETIMEPICKER .OR. ;
                            SELF IS IPADDRESS
                        lEditable := TRUE
                    ENDIF
                    IF SELF IS Edit VAR oEdit
                        IF oEdit:ReadOnly
                            lEditable := FALSE
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
        RETURN lEditable

    /// <include file="Gui.xml" path="doc/Control.IsEnabled/*" />
    METHOD IsEnabled() AS LOGIC CLIPPER
        IF SELF:__IsValid
            RETURN oCtrl:Enabled
        ENDIF
        RETURN FALSE

    PROPERTY IsHidden  AS LOGIC
        GET
            RETURN SELF:__IsValid  .and. !oCtrl:Visible
        END GET
        SET
            IF SELF:__IsValid
                oCtrl:Visible := ! Value
            ENDIF
            RETURN
        END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/Control.IsReadOnly/*" />
    METHOD IsReadOnly() AS LOGIC STRICT
        IF SELF:__IsValid .AND. SELF IS Edit
            return ((SWF.TextBox) self:oCtrl):ReadOnly
        ELSE
            RETURN FALSE
        ENDIF

    /// <include file="Gui.xml" path="doc/Control.IsVisible/*" />
    METHOD IsVisible
        RETURN SELF:__IsValid .and. oCtrl:Visible

    /// <include file="Gui.xml" path="doc/Control.KeyDown/*" />
    METHOD KeyDown(oKeyEvent as KeyEvent) AS VOID
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.KeyUp/*" />
    METHOD KeyUp(oKeyEvent as KeyEvent) AS VOID
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.LinkDF/*" />
    METHOD LinkDF(oDS AS DataServer, siDF as usual) as void
        LOCAL tmpDF AS OBJECT
        LOCAL symClassName AS SYMBOL

        IF !IsNumeric(siDF)
            siDF:=oDS:FieldPos(siDF)
        ENDIF

         IF (!IsNil(oServer) .AND. (oDS!=oServer))
            SELF:__Unlink()
        ENDIF

        oServer 	 := oDS
        siDataField  := siDF
        symDataField := oServer:FieldSym(siDataField)
        IF IsMethod(oServer, #IsBaseField)
            lBaseServer := Send(oServer, #IsBaseField, symDataField)
        ELSE
            symClassName := ClassName(oServer)
            lBaseServer  := symClassName==#DBServer .OR. symClassName==#SQLSelect .OR. symClassName==#SQLTable .OR. symClassName==#JDataServer
        ENDIF

        // Propagate data field if no explicit one
        IF ((tmpDF := oServer:DataField(siDataField)) != NULL_OBJECT)
            oDataField := tmpDF

            IF !lExplicitFS
                // propogate field spec if no explicit one
                SELF:FieldSpec := SELF:oDataField:FieldSpec

                // propogate field spec
                IF !lExplicitHL
                    // CHECK if NameSym and hyperlabel are same
                    IF !IsNil(SELF:oDataField:HyperLabel) .AND. (oDataField:NameSym == oDataField:HyperLabel:NameSym)
                        oHyperLabel := oDataField:HyperLabel
                    ELSE
                        //RvdH 060608 optimized: cCaption is a string
                        //oHyperLabel := HyperLabel { oDataField:NameSym, iif(!Empty(cCaption), cCaption, oDataField:Name) }
                        oHyperLabel := HyperLabel { oDataField:NameSym, IIF(SLen(cCaption)>0, cCaption, oDataField:Name) }
                    ENDIF
                ENDIF
            ENDIF
        ENDIF

        uGetSetOwner := NIL
        cbGetSetBlock := NULL_CODEBLOCK

        return


    /// <include file="Gui.xml" path="doc/Control.MenuInit/*" />
    METHOD MenuInit(oMenuInitEvent as MenuInitEvent) AS VOID
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.MenuSelect/*" />
    METHOD MenuSelect(oMenuSelectEvent as MenuSelectEvent) AS VOID
        //Also empty in GUI Classes
        RETURN
    /// <include file="Gui.xml" path="doc/Control.Modified/*" />
    property Modified AS LOGIC GET __lModified SET __lModified := value

    /// <include file="Gui.xml" path="doc/Control.MouseButtonDoubleClick/*" />
    METHOD MouseButtonDoubleClick(oMouseEvent as MouseEvent ) AS VOID
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.MouseButtonDown/*" />
    method MouseButtonDown(oMouseEvent as MouseEvent) as void
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.MouseButtonUp/*" />
    METHOD MouseButtonUp(oMouseEvent as MouseEvent) AS VOID
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.MouseDrag/*" />
    METHOD MouseDrag(oMouseEvent as MouseEvent) as void
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.MouseMove/*" />
    METHOD MouseMove(oMouseEvent as MouseEvent) as void
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.Move/*" />
    METHOD Move(oMoveEvent as MoveEvent) as void
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.Name/*" />
    ACCESS Name  AS STRING
        IF (SELF:HyperLabel  != NULL_OBJECT)
            RETURN SELF:HyperLabel :Name
        ENDIF

        RETURN NULL_STRING

    /// <include file="Gui.xml" path="doc/Control.NameSym/*" />
    ACCESS NameSym AS SYMBOL
        IF (SELF:HyperLabel != NULL_OBJECT)
            RETURN SELF:HyperLabel:NameSym
        ENDIF
        RETURN NULL_SYMBOL

    ACCESS __OwnerOffSet AS Point
        LOCAL oCtrlOwner AS SWF.Control
        LOCAL oPoint AS Point
        LOCAL oPoint2 AS Point
        oPoint := Point{}
        IF SELF:oCtrl != NULL_OBJECT
            oCtrlOwner 	:= SELF:oCtrl:Parent
            DO WHILE oCtrlOwner != NULL
                IF oCtrlOwner is VOSurfacePanel
                    EXIT
                ENDIF
                oPoint2 := oCtrlOwner:Location
                oPoint:X += oPoint2:X
                oPoint:Y += oPoint2:Y
                oCtrlOwner := oCtrlOwner:Parent
            ENDDO
        ENDIF
        RETURN oPoint

    /// <include file="Gui.xml" path="doc/Control.Origin/*" />
    PROPERTY Origin AS Point
        GET
            IF (oCtrl == NULL_OBJECT .AND. oOrigin != NULL_OBJECT)
                RETURN oOrigin
            ELSE
                LOCAL oPoint AS Point
                oPoint := WC.GetOrigin(SELF)
                oPoint := oPoint + SELF:__OwnerOffSet
                RETURN oPoint
            ENDIF
        END GET
        SET
            IF (oCtrl == NULL_OBJECT)
                SELF:oOrigin := Point{value:X, value:Y}
            elseif oCtrl is IVOUIObject var oGui
                value := value - SELF:__OwnerOffSet
                WC.MoveWindow(oGui, value, true)
            ENDIF
        END SET
    END PROPERTY
    /// <include file="Gui.xml" path="doc/Control.OverRide/*" />
    METHOD OverRide(lEnable AS LOGIC) AS VOID PASCAL
        //Also empty in GUI Classes
        RETURN

    /// <include file="Gui.xml" path="doc/Control.Owner/*" />
    access Owner as object
        return self:oParent

    STATIC METHOD OwnerAlignmentHandledByWinForms(oC AS IVOControl, iNewType AS USUAL) AS LOGIC
        LOCAL lStandard AS LOGIC
        LOCAL iType AS DWORD
        lStandard := TRUE
        IF IsNumeric(iNewType)
            iType := (DWORD) iNewType
        ENDIF
        IF IsLong(iNewType)
            SWITCH iType
            CASE OA_PX_Y
                oC:Anchor := SWF.AnchorStyles.Bottom
            CASE OA_X
                oC:Anchor := SWF.AnchorStyles.Top | SWF.AnchorStyles.Right
            CASE OA_Y
                oC:Anchor := SWF.AnchorStyles.Left | SWF.AnchorStyles.Bottom
            CASE OA_X_Y
                oC:Anchor := SWF.AnchorStyles.Bottom | SWF.AnchorStyles.Right
            CASE OA_WIDTH
                oC:Anchor := SWF.AnchorStyles.Top | SWF.AnchorStyles.Left | SWF.AnchorStyles.Right
            CASE OA_HEIGHT
                oC:Anchor := SWF.AnchorStyles.Top | SWF.AnchorStyles.Left | SWF.AnchorStyles.Bottom
            CASE OA_WIDTH_HEIGHT
                oC:Anchor := SWF.AnchorStyles.Top | SWF.AnchorStyles.Left | SWF.AnchorStyles.Bottom| SWF.AnchorStyles.Right
            CASE OA_FULL_SIZE
                oC:Dock := SWF.DockStyle.Fill
            CASE OA_LEFT_AUTOSIZE
                oC:Dock := SWF.DockStyle.Left
            CASE OA_RIGHT_AUTOSIZE
                oC:Dock := SWF.DockStyle.Right
            CASE OA_TOP_AUTOSIZE
                oC:Dock := SWF.DockStyle.Top
            CASE OA_BOTTOM_AUTOSIZE
                oC:Dock := SWF.DockStyle.Bottom
            OTHERWISE
                lStandard := FALSE
            END SWITCH
        ELSE
            lStandard := FALSE
        ENDIF
        RETURN lStandard

    /// <include file="Gui.xml" path="doc/Control.OwnerAlignment/*" />
    ASSIGN OwnerAlignment(iNewType AS USUAL)

        if oCtrl is IVOControl var oVC .and. ! Control.OwnerAlignmentHandledByWinForms(oVC, iNewType)
            IF oFormSurface IS Window
                oFormSurface:__AddAlign(SELF, iNewType)
            ELSE
                oParent:__AddAlign(SELF, iNewType)
            ENDIF
        ENDIF
        RETURN

    METHOD PaintBackGround() CLIPPER
        RETURN FALSE

    /// <include file="Gui.xml" path="doc/Control.PerformValidations/*" />
    METHOD PerformValidations() CLIPPER
        // Perform validations for Control against supplied parameter
        // if it has a data spec, otherwise just return true
        oHLStatus := NULL_OBJECT
        IF oFieldSpec != NULL_OBJECT
            IF !oFieldSpec:PerformValidations(uValue)
                oHLStatus := oFieldSpec:Status
                RETURN FALSE
                // elseif !oDataField:PerformValidations(uValue)
                // oHLStatus:=oDataField:Status
                // return false
            ENDIF
        ENDIF
        RETURN TRUE

    /// <include file="Gui.xml" path="doc/Control.RegisterTimer/*" />
    METHOD RegisterTimer(nInterval,lOneTime)
        IF !IsLong(nInterval)
            WCError{#RegisterTimer,#Control,__WCSTypeError,nInterval,1}:Throw()
        ENDIF

        IF !IsNil(lOneTime)
            IF !IsLogic(lOneTime)
                WCError{#RegisterTimer,#Control,__WCSTypeError,lOneTime,2}:Throw()
            ENDIF
            IF lOneTime
                dwTimerInterval:=0
            ELSE
                dwTimerInterval := nInterval
            ENDIF
        ELSE
            dwTimerInterval := nInterval
        ENDIF

        IF (nInterval > 0)
            dwTimerCount := nInterval
            IF !lTimerRegistered
                WC.RegisterTimer(SELF)
                lTimerRegistered := TRUE
            ENDIF
        ELSE
            WC.UnregisterTimer(SELF)
            lTimerRegistered := FALSE
        ENDIF

        RETURN SELF

    /// <include file="Gui.xml" path="doc/Control.ReadOnly/*" />
    PROPERTY ReadOnly AS LOGIC
    GET
        RETURN SELF:IsEnabled()
    END GET
    SET
        IF (value)
            SELF:Disable()
        ELSE
            SELF:Enable()
        ENDIF
    END SET
    END PROPERTY


    /// <include file="Gui.xml" path="doc/Control.RePaint/*" />
    METHOD RePaint() AS VOID STRICT
        IF SELF:__IsValid
            oCtrl:Invalidate()
        ENDIF
        RETURN


    /// <include file="Gui.xml" path="doc/Control.Resize/*" />
    METHOD Resize(oResizeEvent as ResizeEvent) AS VOID
        RETURN

    /// <include file="Gui.xml" path="doc/Control.RestoreUpdate/*" />
    METHOD RestoreUpdate() AS VOID STRICT
        IF SELF:__IsValid
            SELF:oCtrl:ResumeLayout()
        ENDIF
        RETURN

    METHOD SendToBack() AS VOID CLIPPER
        IF SELF:ValidateControl()
            oCtrl:SendToBack()
        ENDIF

    /// <include file="Gui.xml" path="doc/Control.Server/*" />
    PROPERTY Server AS DataServer GET oServer

    /// <include file="Gui.xml" path="doc/Control.SetExStyle/*" />
    METHOD SetExStyle(kExStyle AS LONG, lEnable := TRUE AS LOGIC) AS VOID STRICT
        LOCAL liTemp AS LONG

        IF oCtrl == NULL_OBJECT
            IF lEnable
                dwExStyle := _OR(kExStyle, dwExStyle)
            ELSE
                dwExStyle := _AND(dwExStyle, _NOT(kExStyle))
            ENDIF
        ELSE
            VAR iCtrl := (IVOControlProperties) oCtrl
            liTemp := GuiWin32.GetWindowExStyle(hWnd)
            iCtrl:ControlProperties:SetExStyle(kExStyle, lEnable)
            IF lEnable
                liTemp := _OR(kExStyle, liTemp)
            ELSE
                liTemp := _AND(liTemp, _NOT(kExStyle))
            ENDIF
            GuiWin32.SetWindowExStyle(hWnd, liTemp)
        ENDIF

        RETURN


    /// <include file="Gui.xml" path="doc/Control.SetFocus/*" />
    METHOD SetFocus() AS VOID STRICT

        IF SELF:ValidateControl()
            oCtrl:Focus()
        ENDIF
        RETURN

    PROPERTY HasFocus AS LOGIC GET SELF:__IsValid .and. oCtrl:Focused

    /// <include file="Gui.xml" path="doc/Control.HasStyle/*" />
    METHOD HasStyle(kStyle AS LONG)
        LOCAL liStyle	AS LONG
        liStyle := GuiWin32.GetWindowStyle(SELF:hWnd)
        RETURN _AND(liStyle,kStyle) != 0

    /// <include file="Gui.xml" path="doc/Control.Style/*" />
    PROPERTY Style AS LONG GET GuiWin32.GetWindowStyle(SELF:hWnd)

    /// <include file="Gui.xml" path="doc/Control.SetStyle/*" />
    METHOD SetStyle(kStyle AS LONG, lEnable := TRUE AS LOGIC) AS VOID
        LOCAL liTemp as LONG
        IF (oCtrl == NULL_OBJECT .or. ! oCtrl:IsHandleCreated)
            IF lEnable
                dwStyle := _OR( kStyle, dwStyle)
            ELSE
                dwStyle := _AND(dwStyle, _NOT(kStyle))
            ENDIF
        elseif oCtrl is IVOControlProperties var oProps
            liTemp := GuiWin32.GetWindowStyle(self:hWnd)
            oProps:ControlProperties:SetStyle(kStyle, lEnable)
            IF lEnable
                liTemp := _OR(kStyle, liTemp)
            ELSE
                liTemp := _AND(liTemp, _NOT(kStyle))
            ENDIF
            dwStyle := liTemp // without this line it does not work for custom drawn labels
            GuiWin32.SetWindowStyle(hWnd, liTemp)
        ENDIF

        RETURN


    /// <include file="Gui.xml" path="doc/Control.Show/*" />
    METHOD Show( ) AS VOID CLIPPER

        IF (oCtrl == NULL_OBJECT)
            SELF:Create()
        ENDIF
        IF SELF:__IsValid
            oCtrl:Show()
        ENDIF

        RETURN


    METHOD ShowToolTip() AS VOID STRICT
        IF SELF:oFormSurface != NULL_OBJECT
            SELF:oFormSurface:__ShowToolTip(SELF)
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/Control.Size/*" />
    PROPERTY Size  AS Dimension
    GET
        LOCAL oResult AS Dimension
        IF SELF:ValidateControl()
            oResult := oCtrl:Size
        ELSE
            oResult := SELF:oSize
        ENDIF
        RETURN oResult
    END GET
    SET
        IF (oCtrl == NULL_OBJECT)
            oSize		:= value:Clone()
        ELSE
            SELF:oCtrl:Size := value
        ENDIF

        RETURN
    END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/Control.Status/*" />
    PROPERTY Status AS HyperLabel GET SELF:oHLStatus SET oHLStatus := value

    /// <include file="Gui.xml" path="doc/Control.SuspendUpdate/*" />
    METHOD SuspendUpdate() AS VOID STRICT
        IF SELF:__IsValid
            SELF:oCtrl:SuspendLayout()
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/Control.TextValue/*" />
    PROPERTY TextValue AS STRING GET "" SET

    /// <include file="Gui.xml" path="doc/Control.Timer/*" />
    METHOD Timer()  CLIPPER
        //Also empty in GUI Classes
        RETURN NIL


    /// <include file="Gui.xml" path="doc/Control.ToolTipText/*" />
    PROPERTY ToolTipText AS STRING GET sToolTipText SET sToolTipText := value

    /// <include file="Gui.xml" path="doc/Control.UseHLForToolTip/*" />
    PROPERTY UseHLForToolTip AS LOGIC GET lUseHLForToolTip SET lUseHLForToolTip := value

    /// <include file="Gui.xml" path="doc/Control.ValidateControl/*" />
    METHOD ValidateControl() AS LOGIC STRICT
        IF (oCtrl == NULL_OBJECT)
            SELF:Create()
        ENDIF
        RETURN SELF:__IsValid


    /// <include file="Gui.xml" path="doc/Control.Value/*" />
    PROPERTY Value AS USUAL
    GET
        // returns last valid value
        // value is only updated on leaving the field
        RETURN uValue
    END GET
    SET
        //
        // Value assignment
        // Note : value does not have to be same as what is displayed
        //
        LOCAL cOldValue AS STRING
        cOldValue := AsString(uValue)
        // !!! should be result of FIELDGET and located after FIELDPUT !!!

        SELF:__Value := value //Update the control
        IF oServer != NULL_OBJECT
            IF lBaseServer // if not subclassing
                oServer:FIELDPUT(siDataField,SELF:uValue) //update the DataServer
            ELSEIF symDataField != NULL_SYMBOL
                IVarPut(oServer, symDataField ,SELF:uValue)
            ELSE
                IVarPut(oServer, SELF:NameSym, SELF:uValue)
            ENDIF
        ENDIF
        SELF:ValueChanged := !(cOldValue == AsString(uValue))

    END SET
    END PROPERTY

    PROPERTY ValueChanged AS LOGIC GET lChanged SET SELF:lChanged := VALUE


    /// <include file="Gui.xml" path="doc/Control.VerticalScroll/*" />
    METHOD VerticalScroll(oScrollEvent)
        //Also empty in GUI Classes
        RETURN NIL


END CLASS
