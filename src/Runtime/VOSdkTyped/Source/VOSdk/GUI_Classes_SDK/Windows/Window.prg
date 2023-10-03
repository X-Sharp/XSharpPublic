//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Generic
using VOSDK := XSharp.VO.SDK
using System.Linq

/// <include file="Gui.xml" path="doc/Window/*" />
partial class Window inherit @@EventContext implements IGuiObject, IControlParent, ITimer
    protect oParent as object
    protect oWnd as VOForm
    protect dwStyle as long
    protect dwExStyle as long

    protect cCaption as string
    protect oMenu as Menu
    protect oIcon as Icon
    protect oIconSmall as Icon
    protect oContextMenu as Menu
    protect oAccelerator as Accelerator
    protect oFont as Font
    protect oPointer as Pointer
    protect oBackground as Brush
    protect oForeground as Brush
    protect oHyperLabel as HyperLabel
    protect lRetVal as logic
    protect oOrigin as Point
    protect oPen as Pen
    protect oCursor as VOSDK.Cursor
    protect oDragDropClient as DragDropClient
        //PROTECT oDragDropServer AS DragDropServer
    protect oToolBar as ToolBar

    protect DCInitialized as logic
    protect DCPenInUse as logic
    protect DCPenNeeded as logic
    protect DCBrushInUse as logic
    protect DCBrushNeeded as logic
    protect DCFontInUse as logic
    protect DCFontNeeded as logic
    protect hDC as IntPtr
    protect hDCPaint as IntPtr

    protect oCurrentHelp as HelpDisplay
    protect lHelpOn as logic
    protect lHelpCursorOn as logic
    protect lHelpMenu as logic
        //PROTECT lFileDragging AS LOGIC
    protect dwTimerCount as int
    protect dwTimerInterval as int
    protect lTimerRegistered as logic

        //PROTECT lDragActive AS LOGIC
        //PROTECT oDragImageList AS VOSDK.ImageList
    protect hDragSingleCursor as IntPtr
    protect hDragMultipCursor as IntPtr
    protect aAlignes			as array
    protect aDelayedAlignes		as List< Tuple <IGUIObject, usual> >
    protect lDelayAlignment		as logic
    protect lAutomated			as logic

    protect oMinSize as Dimension
    protect oTrayIcon as VOTrayIcon


    property EventReturnValue as longint auto

    protect oResourceDialog as ResourceDialog	// Class that holds the decoded version of the Win32 resource
    property ResourceDialog as ResourceDialog get oResourceDialog
    property IsClosing as logic auto

    property __Handle as IntPtr get self:Handle()

    class VOAlignElement
        property Control as IGuiObject auto
        property Type as usual auto
        property Left as long auto
        property Top as long auto
        property Width as long auto
        property Height as long auto
        constructor(oC as IGuiObject, uType as usual, nLeft as long, nTop as long, nWidth as long, nHeight as long)
            Control  := oC
            Type    := uType
            Left    := nLeft
            Top     := nTop
            Width   := nWidth
            Height  := nHeight
            return
        constructor() strict
            return
    end class

    method __CreateForm() as VOForm strict
        return GuiFactory.Instance:CreateWindow(self)

    property __Form as VOForm get oWnd

    property TopMost as logic
        get
            return self:__Form:TopMost
        end get
        set
            if value
                self:__Form:mdiParent := null
            endif
            self:__Form:TopMost := value
        end set
    end property


    property __IsValid as logic get self:oWnd != null_object .and. ! self:oWnd:IsDisposed

    property __HasSurface as logic get self:__Surface != null

    property __Surface as IVOPanel
        get
            if self:__IsValid
                local implied aList := oWnd:GetAllControls()
                foreach oC as object in aList
                    if oc is IVOPanel
                        return oC
                    endif
                next
            endif
            return null
        end get
    end property
    method __SetupDataControl(oDC as VOSDK.Control) as void
        return
    method __AddControl (oCtrl as IVOControl) as void
        return
    /// <exclude />
    method __AddAlign(oControl as IGUIObject, iType as usual) as logic strict
        local dwI, dwCount as dword
        local lDelete      as logic
        local lOldAlign    as logic
        local lFound	   as logic
        if IsNil(iType)
            iType := OA_TOP
        endif
        if self:lDelayAlignment
            aDelayedAlignes:Add( Tuple<IGUIObject, usual> {oControl, iType})
            return true
        endif

        lOldAlign := ! IsPtr(iType) .and. (int) iType <= OA_FULL_SIZE

        //PP-031129 Additional owner alignment options
        if ! lOldAlign
            //Null_Object calls SetAlignStartSize() in init mode,
            //means that only the first call sets the AlignStartSize.
            self:SetAlignStartSize(null_object)
        endif

        lDelete := IsLong(iType) .and. (int) iType = OA_NO

        dwCount := ALen(aAlignes)
        lFound  := false
        for dwI := 1 upto dwCount
            var element := (VOAlignElement) aAlignes[dwI]
            if element:Control == oControl
                lFound  := true
                if lDelete
                    ADel(aAlignes, dwI)
                    aSize(aAlignes, Alen(aAlignes)-1)
                else
                    element:Type := iType
                endif
                dwI := 0
                exit
            endif
        next


        if !lFound
            // Add Control to the array
            local oRect as System.Drawing.Rectangle
            if oControl is Window
                local oForm as System.Windows.Forms.Form
                oForm 			:= ((VOSDK.Window) oControl):__Form
                oRect 			:= oForm:ClientRectangle
                oRect:Location 	:= oControl:Origin
                oRect:Size     	:= oForm:Size
            elseif oControl is VOSDK.Control var oC .and. oC:__Control is IVOControl var oCtrl
                oRect			:= oCtrl:DisplayRectangle
                oRect:Location  := oCtrl:Location
            endif
            AAdd(aAlignes,VOAlignElement{oControl, iType, oRect:left, oRect:top, oRect:Width, oRect:Height})
        endif

        return true

    /// <exclude />
    method __AddTool(oControl as VOSDK.Control) as logic strict
        //LOCAL cMessage AS STRING
        //SELF:EnableToolTips(TRUE)
        //cMessage := oControl:ToolTipText
        //IF STRING.IsNullOrEmpty(cMessage) .and. oControl:UseHLForToolTip
        //	IF oControl:HyperLabel != NULL_OBJECT
        //		cMessage := oControl:HyperLabel:Description
        //	ENDIF
        //ENDIF
        //IF typeof(VOPanel):Isassignablefrom(SELF:__Surface:GetType())
        //	((VOPanel) SELF:__Surface):RegisterTooltip(oControl:__Control, cMessage)
        //ENDIF

        return true

    method __ShowToolTip(oControl as VOSDK.Control) as void strict
        local cMessage as string
        cMessage := oControl:ToolTipText
        if STRING.IsNullOrEmpty(cMessage) .and. oControl:UseHLForToolTip
            if oControl:HyperLabel != null_object
                cMessage := oControl:HyperLabel:Description
            endif
        endif
        if self:__Surface is VOPanel var oPanel
            oPanel:ShowToolTip((System.Windows.Forms.Control) oControl:__Control, cMessage)
        endif

    method __AlignControl(	oCtl as IVOUIObject, nX as long, nY as long, nW as long, nH as long) as void
        oCtl:Location := System.Drawing.Point{nX, nY}
        oCtl:Size     := System.Drawing.Size{nW, nH}
        return


    /// <exclude />
    method __AlignControls() as Window strict

        //PP-030828 Strong typing
        //PP-031129 Additional owner alignment options
        //PP-040914 Update from S Ebert
        //SE-070920 Support for factor and divisor and enhanced proportional alignment.
        //          Factor/Divisor-Mode is switched on, if the alignment parameter which
        //          was assigned to Control:OwnerAlignment is a PTR (pointer) value.
        //          The type PTR is used here to recognize the Factor/Divisor-Mode.
        //          Then the PTR value becomes casted to a DWORD. The 32Bit value is
        //          divided in to 8 4bit values, each can represent a value between 0 and 15
        //          (0x0 and 0xF). The 8 4bit values are ordered in this way:
        //
        //          Pos    8      7      6      5       4       3      2      1
        //                  Height        Width         Y-Position     X-Position
        //          0x  Factor,Divisor Factor,Divisor Factor,Divisor Factor,Divisor
        //
        //          If you use a 32hex OwnerAlignment value you can write it like here
        //
        //          Control:OwnerAlignment := PTR(_CAST, 0x00001111) (same as OA_X_Y)
        //
        //          The hex value has the following format:

        //          0x00001111
        //            HHWWYYXX   (H=Height, W=Width, Y=Y-Position, X=X-Position)
        //            FDFDFDFD   (F=Factor, D=Divisor)
        //
        //          a Value of 00 means no alignment for X,Y,With or Height depending on its
        //          position in the 32 bit hex value.
        //
        //          IF you use the Factor/Divisor-Mode you can't use the proportinal alignment
        //          simultaneously for this control (window).
        local oRect		 as System.Drawing.Rectangle
        local oCtl       as IVOUIObject
        local uType      as usual
        local dwType     as dword
        local liMulDiv   as long
        local oOwnerSize as Dimension
        // Original Sizes
        local iWinRefWidth		as int
        local iWinRefHeight		as int
        local iCtlRefWidth      as int
        local iCtlRefHeight     as int
        // New Window Size
        local iWinWidth			as int
        local iWinHeight		as int
        // New Control Size & Position
        local iCtlX				as int
        local iCtlY				as int
        local iCtlWidth			as int
        local iCtlHeight		as int

        local pB                    as byte ptr
        local oOwnerOffSet			as Point

        local dwI, dwCount  as dword
        local oResize    as object

        if self:lDelayAlignment
            return self
        endif


        dwCount := ALen(aAlignes)

        if dwCount < 1 .or. oWnd == null_object .or. oWnd:WindowState == system.Windows.Forms.FormWindowState.Minimized
            return  self
        endif

        oRect   := self:__Surface:ClientRectangle
        iWinWidth  := oRect:Width
        iWinHeight := oRect:Height
        var element := (VOAlignElement) aAlignes[1]
        if element:Control == null_object
            oOwnerSize	:= element:Type
            if oOwnerSize != null_object
                iWinRefWidth  := oOwnerSize:Width
                iWinRefHeight := oOwnerSize:Height
            else
                iWinRefWidth  := iWinWidth
                iWinRefHeight := iWinHeight
            endif
            dwI := 2
        else
            dwI := 1
        endif
        if iWinRefWidth == iWinWidth .and. iWinRefHeight == iWinHeight
            return self
        endif

        oWnd:SuspendRedraw()
        // First element in aAlignes has the window and its original size
        // 1 = Window
        // 2 = Size (Dimension object)
        // Each other Element in aAlignes has the following values
        // 1   = Control/ Window
        // 2   = Type, Pointer or LONG
        // 3,4 = Original X,Y
        // 5,6 = Original Width/Height
        // 5,6 = Original Width/Height
        do while dwI <= dwCount
            element := (VOAlignElement) aAlignes[dwI]
            oResize := element:Control
            if oResize is DataWindow
                oCtl	:= ((DataWindow) oResize):__Frame
                oOwnerOffSet := Point{0,0}
            elseif oResize is Window
                oCtl	:= ((Window) oResize):__Form
                oOwnerOffSet := Point{0,0}
            else
                oCtl	:= ((Control) oResize):__Control astype IVOUIObject
                oOwnerOffSet := ((Control) oResize):__OwnerOffSet
            endif
            uType   := element:Type
            dwType  := dword(_cast, uType)
            if !oCtl:IsDisposed
                if IsPtr(uType) .or. dwType > OA_FULL_SIZE // 10
                    iCtlX      := element:Left
                    iCtlY      := element:Top
                    iCtlWidth  := iCtlRefWidth  := element:Width
                    iCtlHeight := iCtlRefHeight := element:Height

                    if IsPtr(uType)
                        pB := (byte ptr) @dwType
                        if (liMulDiv := pB[1]) > 0
                            iCtlX := GuiWin32.MulDiv(iWinWidth , liMulDiv>>4, (long) _and(liMulDiv, 0XF))
                        endif
                        if (liMulDiv := pB[2]) > 0
                            iCtlY := GuiWin32.MulDiv(iWinHeight , liMulDiv>>4, (long)  _and(liMulDiv, 0XF))
                        endif
                        if (liMulDiv := pB[3]) > 0
                            iCtlWidth := GuiWin32.MulDiv(iWinWidth , liMulDiv>>4,  (long) _and(liMulDiv, 0XF))
                        endif
                        if (liMulDiv := pB[4]) > 0
                            iCtlHeight := GuiWin32.MulDiv(iWinHeight , liMulDiv>>4,  (long) _and(liMulDiv, 0XF))
                        endif
                    else
                        // Width
                        if _and(dwType, OA_PWIDTH) = OA_PWIDTH
                            iCtlWidth := GuiWin32.MulDiv(iWinWidth, iCtlRefWidth, iWinRefWidth)
                        elseif _and(dwType, OA_WIDTH) = OA_WIDTH
                            iCtlWidth += iWinWidth - iWinRefWidth
                        endif
                        // Height
                        if _and(dwType, OA_PHEIGHT) = OA_PHEIGHT
                            iCtlHeight := GuiWin32.MulDiv(iWinHeight, iCtlRefHeight, iWinRefHeight)
                        elseif _and(dwType, OA_HEIGHT) = OA_HEIGHT
                            iCtlHeight += iWinHeight - iWinRefHeight
                        endif
                        // X-Position
                        if _and(dwType, OA_PX) = OA_PX
                            if _and(dwType, OA_PWIDTH) = OA_PWIDTH
                                iCtlX := GuiWin32.MulDiv(iWinWidth, iCtlX, iWinRefWidth)
                            else
                                iCtlX := GuiWin32.MulDiv(iWinWidth, iCtlX + iCtlRefWidth/2, iWinRefWidth) - iCtlWidth/2
                            endif
                        elseif _and(dwType, OA_X) = OA_X
                            iCtlX += iWinWidth - iWinRefWidth
                        endif
                        // Y- position
                        if _and(dwType, OA_PY) = OA_PY
                            if _and(dwType, OA_PHEIGHT) = OA_PHEIGHT
                                iCtlY := GuiWin32.MulDiv(iWinHeight, iCtlY, iWinRefHeight)
                            else
                                iCtlY := GuiWin32.MulDiv(iWinHeight, iCtlY + iCtlRefHeight/2, iWinRefHeight) - iCtlHeight/2
                            endif
                        elseif _and(dwType, OA_Y) = OA_Y
                            iCtlY += iWinHeight - iWinRefHeight
                        endif

                    endif
                    // Since we are nesting controls in our GUI Classes we need to subtract the Offset of the owner
                    iCtlX -= oOwnerOffSet:X
                    iCtlY -= oOwnerOffSet:Y
                    self:__AlignControl(oCtl,iCtlX, iCtlY, iCtlWidth, iCtlHeight)
                else
#region Simple Alignment
                    oRect		:= oCtl:ClientRectangle
                    iCtlWidth	:= oRect:Width
                    iCtlHeight	:= oRect:Height
                    iCtlX		:= oRect:Left
                    iCtlY		:= oRect:Top
                    switch dwType
                    case OA_TOP
                        iCtlY      := 0
                    case OA_LEFT
                        iCtlX      := 0
                    case OA_BOTTOM
                        iCtlY      := iWinHeight-iCtlHeight
                    case OA_RIGHT
                        iCtlX      := iWinWidth-iCtlWidth
                    case OA_TOP_AUTOSIZE
                        iCtlX      := 0
                        iCtlY      := 0
                        iCtlWidth  := iWinWidth
                    case OA_LEFT_AUTOSIZE
                        iCtlX      := 0
                        iCtlY      := 0
                        iCtlHeight := iWinHeight
                    case OA_BOTTOM_AUTOSIZE
                        iCtlX      := 0
                        iCtlY      := iWinHeight-iCtlHeight
                        iCtlWidth  := iWinWidth
                    case OA_RIGHT_AUTOSIZE
                        iCtlX      := iWinWidth-iCtlWidth
                        iCtlY      := 0
                        iCtlHeight := iWinHeight
                    case OA_CENTER
                        iCtlX      := (iWinWidth / 2)- (iCtlWidth / 2)
                        iCtlY      := (iWinHeight / 2)- (iCtlHeight / 2)

                    case OA_FULL_SIZE
                        iCtlX      := 0
                        iCtlY      := 0
                        iCtlWidth  := iWinWidth
                        iCtlHeight := iWinHeight
                    end switch
                    // Since we are nesting controls in our GUI Classes we need to subtract the Offset of the owner
                    iCtlX -= oOwnerOffSet:X
                    iCtlY -= oOwnerOffSet:Y
                    self:__AlignControl(oCtl,iCtlX, iCtlY, iCtlWidth, iCtlHeight)
#endregion
                endif
            endif
            ++dwI
        enddo
        oWnd:ResumeRedraw()
        return self


    /// <exclude />
    method __AssociateAccel(lSwitch as logic) as Window strict
        //IF lSwitch .AND. (oAccelerator != NULL_OBJECT)
        //	SetAccelerator(oWnd, oAccelerator:Handle())
        //ELSE //if !lswitch
        //	SetAccelerator(NULL_PTR, NULL_PTR)
        //ENDIF

        return self


    /// <exclude />
    method __Close(oEvent as @@Event) as void strict
        self:IsClosing := true
        self:Close(oEvent)
        self:Destroy()
        return

    /// <exclude />
    method __CommandFromEvent(oEvent as INamedEvent) as logic strict
        local symNameSym as symbol
        local oWindow as Window
        local oReport as object

        symNameSym := oEvent:NameSym
        oWindow := self
        if oWindow is ShellWindow var oShell
            oWindow := oShell:GetActiveChild()
            if oWindow == null_object
                oWindow := self
            endif
        endif


        do while true
            if IsMethod(oWindow, symNameSym)
                Send(oWindow, symNameSym)
                return true
            endif
            if oWindow:Owner is Window var oWin
                oWindow := oWin
            else
                exit
            endif
        enddo

        if IsClassOf(symNameSym, #Window)
            local o as Window
            local oOwner as Window
            oOwner:=self
            if oOwner is ChildAppWindow
                do while oOwner:Owner is Window
                    oOwner:= oOwner:Owner
                enddo
            endif
            o := (Window) CreateInstance(symNameSym, oOwner)
            o:Show()
            return true
        elseif IsClassOf(symNameSym, #ReportQueue)
            oReport := CreateInstance(symNameSym, self)
            if (oReport != null_object)
                Send(oReport,#Show)
            endif
            return true
        endif

        return false


    /// <exclude />
    method __CreateSelfBitmap() as IntPtr strict
        // Todo __CreateSelfBitmap
        local hDIB as ptr
        //LOCAL hBitmap AS PTR
        //LOCAL hBitmapOld AS PTR
        //LOCAL _hDC AS PTR
        //LOCAL hMemDC AS PTR
        //LOCAL rc IS _winRECT
        //LOCAL x, y AS INT



        //_hDC := GetWindowDC(oWnd)

        //IF (_hDC != NULL_PTR)
        //	hMemDC := CreateCompatibleDC(hDC)
        //	IF (hMemDC != NULL_PTR)
        //		GetWindowRect(oWnd, @rc)
        //		x := rc:right - rc:left
        //		y := rc:bottom - rc:top

        //		hBitmap := CreateCompatibleBitmap(_hDC, x, y)

        //		IF (hBitmap != NULL_PTR)
        //			hBitmapOld := SelectObject(hMemDC, hBitmap)
        //			PatBlt(hMemDC, 0, 0, x, y, WHITENESS)
        //			BitBlt(hMemDC, 0, 0, x, y, _hDC, 0, 0, SRCCOPY)
        //			SelectObject(hMemDC, hBitmapOld)
        //		ENDIF

        //		DeleteDC(hMemDC)
        //	ENDIF

        //	ReleaseDC(oWnd, _hDC)
        //ENDIF

        //IF (hBitmap != NULL_PTR)
        //	hDIB := __WCDIBFromBitmap(hBitmap)
        //	DeleteObject(hBitmap)
        //ENDIF

        return hDIB


    /// <exclude />
    property __Cursor as VOSDK.Cursor get oCursor set oCursor := value

        [Obsolete];
    method __DestroyChildren() as void strict
        return

        [Obsolete];
    method __DestroyWnd() as logic strict
        return true

    /// <exclude />
    method __EnableHelpCursor(lEnabled as logic) as Window strict
        // Todo __EnableHelpCursor
        //LOCAL strucPoint IS _WinPoint
        //LOCAL x, y AS LONGINT
        //LOCAL liHitArea AS LONGINT
        //LOCAL oWndCursor AS PTR
        //LOCAL oWndChildCursor AS PTR



    //// lHelpMenu := lEnabled
    //IF lEnabled
    //	IF !lHelpCursorOn
    //		lHelpCursorOn := TRUE
    //		IF (oApp != NULL_OBJECT)
    //			SetCursor(oApp:__HelpCursor)
    //		ENDIF
    //		ShowCursor(TRUE)
    //		IF (GetActiveWindow() == oWnd) .AND. (oApp != NULL_OBJECT)
    //			oApp:__SetHelpWind(oWnd, HM_MOUSE)
    //		ENDIF
    //	ENDIF
    //	// elseif lHelpCursorOn
    //ELSE
    //	lHelpCursorOn := FALSE
    //	IF IsWindowVisible(oWnd) .AND. (GetCapture() == 0)
    //		GetCursorPos(@strucPoint)

    //		// Locate Window or ChildWindow currently containing cursor.
    //		// This is needed for passing to WM_SETCURSOR
    //		x := strucPoint:x
    //		y := strucPoint:y
    //		oWndCursor := WindowFromPoint( x, y ) // TopAppWindow containing point.
    //		strucPoint:x := x
    //		strucPoint:y := y
    //		oWndChildCursor := ChildWindowFromPoint(oWndCursor, x, y)
    //		strucPoint:x := x
    //		strucPoint:y := y
    //		IF oWndChildCursor != 0 .AND. IsWindowVisible(oWndChildCursor)
    //			oWndCursor := oWndChildCursor
    //		ENDIF
    //		liHitArea := SendMessage(oWnd, WM_NCHITTEST, 0, LONGINT(_CAST,@strucPoint))
    //		SendMessage(oWnd, WM_SETCURSOR, DWORD(_CAST,oWndCursor), _OR(liHitArea,LONGINT(WM_MOUSEMOVE) << 16))
    //	ENDIF
    //	IF (GetActiveWindow() == oWnd) .AND. (oApp != NULL_OBJECT)//was commented out ??? Liuho01 03-29-96 ???
    //		oApp:__SetHelpWind(oWnd, HM_GENERAL)
    //	ENDIF
    //ENDIF
    return self


    /// <exclude />
    method __FilterHelpCursor(wArea as longint) as logic strict
        local lTemp as logic

        do case
        case wArea==HTCAPTION
            lTemp := true
        case wArea==HTCLIENT
            lTemp := true
        case wArea==HTREDUCE
            lTemp := true
        case wArea==HTZOOM
            lTemp := true
        case wArea==HTSYSMENU
            lTemp := true
        case wArea==HTBOTTOM .or. ;
                wArea==HTBOTTOMLEFT .or. ;
                wArea==HTBOTTOMRIGHT .or. ;
                wArea==HTTOP .or. ;
                wArea==HTLEFT .or. ;
                wArea==HTRIGHT .or. ;
                wArea==HTTOPLEFT .or. ;
                wArea==HTTOPRIGHT
            lTemp := true
        case wArea==HTNowhere
            lTemp := true
        otherwise
            lTemp := false
        endcase

        return lTemp



    /// <exclude />
    method __GetDC() as IntPtr strict
        // Todo ? __GetDC
        //LOCAL hFont AS PTR
        //LOCAL strucLogPen IS _WinLogPen
        //LOCAL strucLogBrush IS _WinLogBrush
        //LOCAL r IS _WinRECT



        //IF SELF:Handle(4) == NULL_PTR
        //	//PaintInfo = 0
        //	RETURN NULL_PTR
        //ENDIF

        //IF (hDC == NULL_PTR)
        //	WCDCAdd(SELF)
        //	SELF:hDC := GetDC(SELF:Handle(4))
        //	DCInitialized := DCFontInUse := DCPenInUse := DCBrushInUse := FALSE
        //	// else
        //	// WCDCTop(self)
        //ENDIF

        //IF (hDC != NULL_PTR)
        //	IF !DCInitialized
        //		IF (WC.CoordinateSystem == WC.CartesianCoordinates)
        //			SetMapMode(hDC, MM_TEXT)
        //			SetMapMode(hDC, MM_ANISOTROPIC)

        //			GetClientRect(SELF:Handle(4), @r)
        //			SetViewportExtEx(hDC, r:right, r:bottom, NULL_PTR) // device coords
        //			SetWindowExtEx(hDC, r:right, -r:bottom, NULL_PTR) // logical coords used by GDI
        //			SetViewportOrgEx(hDC, 0, r:bottom-1, NULL_PTR)
        //		ENDIF

        //		DCInitialized := TRUE
        //		__WCLogicalBackgroundBrush(SELF,@strucLogBrush)
        //		SetBkColor(hDC, strucLogBrush:lbColor)
        //		IF strucLogBrush:lbStyle == BS_HOLLOW
        //			SetBkMode(hDC, TRANSPARENT)
        //		ELSE
        //			SetBkMode(hDC, OPAQUE)
        //		ENDIF
        //	ENDIF

        //	IF DCFontNeeded .AND. !DCFontInUse
        //		IF oFont != NULL_OBJECT
        //			oFont:Create(FALSE, hDC)
        //			hFont := oFont:Handle()
        //		ELSE
        //			hFont := GetStockObject(System_Font)
        //		ENDIF
        //		SelectObject (hDC, hFont)
        //		DCFontInUse := TRUE
        //		DCFontNeeded := FALSE
        //		SetTextAlign(hDC, _OR(TA_LEFT,TA_BOTTOM))
        //	ENDIF

        //	IF DCPenNeeded .AND. !DCPenInUse
        //		IF oPen != NULL_OBJECT
        //			SelectObject ( hDC, oPen:Handle())
        //			__WCLogicalPen(oPen, @strucLogPen)
        //			SetTextColor(hDC, strucLogPen:lopnColor)
        //		ELSE
        //			// Stock Object BLACK_PEN is default pen
        //			SelectObject(hDC, GetStockObject(BLACK_PEN))
        //			SetTextColor(hDC, GetSysColor(COLOR_WINDOWTEXT))
        //		ENDIF
        //		DCPenInUse := TRUE
        //		DCPenNeeded := FALSE
        //	ENDIF

        //	IF DCBrushNeeded .AND. !DCBrushInUse
        //		IF oForeground != NULL_OBJECT
        //			SelectObject(hDC, oForeground:Handle())
        //		ELSE
        //			// Stock Object Black_Brush is default brush
        //			SelectObject(hDC, GetStockObject(BLACK_BRUSH))
        //		ENDIF
        //		DCBrushInUse := TRUE
        //		DCBrushNeeded := FALSE
        //	ENDIF
        //ENDIF

        return hDC


    /// <exclude />
    method __GetMyOleObjects as array strict
        local aMyControls as array
        local aObjects		as array
        local iLen			as dword
        local i				as dword
        aMyControls := self:GetAllChildren()
        aObjects    := {}
        iLen := ALen(aMyControls)
        for i:=1 to iLen
            if IsInstanceOf(aMyControls[i], #OleObject)
                AAdd(aObjects, aMyControls[i])
            endif
        next
        return aObjects



        //METHOD __GetPaintRect() AS _WINRECT STRICT
        //	//PP-030828 Strong typing


        //	RETURN strucPaintRect



    /// <exclude />
    method __HandleListItemDrag(oEvent as @@Event) as Window strict
        // needed ?
        //LOCAL dli AS _winDRAGLISTINFO
        //STATIC LOCAL iBeginDragItem AS INT
        //LOCAL iCurItem AS INT
        //LOCAL sItemText AS STRING
        //LOCAL uItemVal AS USUAL
        //LOCAL oLB AS ListBox
    ////RvdH 070713 Fixed ItemDrag problem
    //dli := PTR(_CAST, oEvent:lParam)
    //DO CASE
    //CASE (dli:uNotification == DL_BEGINDRAG)
    //	iBeginDragItem := LBItemFromPt(dli:oWnd, @dli:ptCursor, TRUE)
    //	DrawInsert(SELF:Handle(), dli:oWnd, iBeginDragItem)
    //	SELF:EventReturnValue := 1L
    //CASE (dli:uNotification == DL_DRAGGING)
    //	iCurItem := LBItemFromPt(dli:oWnd, @dli:ptCursor, TRUE)
    //	DrawInsert(SELF:handle(), dli:oWnd, iCurItem)
    //	SELF:EventReturnValue := DL_MOVECURSOR
    //CASE (dli:uNotification == DL_DROPPED)
    //	iCurItem := LBItemFromPt(dli:oWnd, @dli:ptCursor, TRUE)
    //	DrawInsert(SELF:handle(), dli:oWnd, -1)
    //	oLB := (LIstBox) __WCGetControlByHandle(dli:oWnd)
    //	IF (oLB != NULL_OBJECT) .AND. (iCurItem != -1) .AND. (iCurItem != iBeginDragItem)
    //		sItemText := oLB:GetItem(iBeginDragItem+1)
    //		uItemVal := oLB:GetItemValue(iBeginDragItem+1)
    //		oLB:DeleteItem(iBeginDragItem+1)
    //		IF (iBeginDragItem < iCurItem)
    //			iCurItem --
    //		ENDIF
    //		oLB:AddItem(sItemText, iCurItem+1, uItemVal)
    //		oLB:CurrentItemNo := iCurItem+1
    //	ENDIF
    //ENDCASE
    return self


    /// <exclude />
    method __HandlePointer(oEvent as @@Event, lHelp as logic, lClient as logic) as Window strict
        // needed ?
        //LOCAL oObject AS OBJECT
        //LOCAL lParam AS LONG
        //LOCAL hHandle AS PTR



        //IF lHelp
        //	IF (oApp != NULL_OBJECT)
        //		SetCursor(oApp:__HelpCursor)
        //	ENDIF
        //	lParam := oEvent:lParam
        //	IF HiWord(DWORD(_CAST,lParam)) == WM_LBUTTONDOWN
        //		hHandle := PTR(_CAST, oEvent:wParam)
        //		IF hHandle == oWnd
        //			oObject:=SELF
        //		ELSE
        //			oObject :=__WCGetObjectByHandle(hHandle)
        //			IF IsInstanceOf(oObject, #__FormDialogWindow)
        //				oObject := oObject:Owner:DataWindow
        //			ENDIF
        //		ENDIF

        //		// $$$
        //		/*
        //		do while (IsInstanceOf(oObject, #Control) .or. IsInstanceOf(oObject, #Window))
        //		if (oObject:HyperLabel != NULL_OBJECT) .and. (NULL_STRING != oObject:HyperLabel:HelpContext)
        //		exit
        //		endif
        //		oObject := oObject:Owner
        //		enddo
        //		*/

        //		IF (oObject == NULL_OBJECT) .OR. IsInstanceOf(oObject, #App)
        //			oObject:=SELF
        //		ENDIF
        //		SELF:__ProcessHelpCursor(oObject, _AND(lParam, 0xFFFF))
        //	ENDIF
        //	SELF:EventReturnValue := 1L
        //ELSE
        //	IF (oPointer != NULL_OBJECT) .AND.;
        //	(LoWord(DWORD(_CAST,oEvent:lParam)) == HTCLIENT) .AND.;
        //	((oWnd == oEvent:wParam) .OR. lClient)
        //		SetCursor(oPointer:Handle())
        //		SELF:EventReturnValue := 1L
        //	ELSE
        //		SELF:Default(oEvent)
        //	ENDIF
        //ENDIF
        return self


    /// <exclude />
    method __HelpFilter(oEvent as @@Event) as logic strict
        local wParam as dword



        wParam := LoWord(oEvent:wParam)
        if (wParam >= ID_FIRSTWCHELPID)
            do case
            case (wParam == ID_WCHELP)
                self:__ProcessHelpCursor(self, HTNowhere)
            case (wParam == ID_WCHELPON)
                self:__EnableHelpCursor(true)
            case (wParam == ID_WCHELPOFF)
                self:__EnableHelpCursor(false)
            endcase
            return true
        endif

        return false


    /// <exclude />
    method __InCursorHelpMode() as logic strict
        //PP-030828 Strong typing
        return lHelpOn .and. lHelpCursorOn


    /// <exclude />
    property __Parent as object get oParent


    method PreMenuCommand(oMenuCommandEvent as MenuCommandEvent) as logic
        return false
    /// <exclude />
    method __PreMenuCommand(oMenuCommandEvent as MenuCommandEvent) as usual

        if self:PreMenuCommand( oMenuCommandEvent)
            return self
        endif

        if ! self:__CommandFromEvent(oMenuCommandEvent)
            return self:MenuCommand(oMenuCommandEvent)
        endif
        return self


        [Obsolete];
    method __PrePreMenuCommand() as void strict
        return


    /// <exclude />
    method __ProcessHelp(oEvent as @@Event) as logic strict
        // Todo __ProcessHelp
        //LOCAL oObject AS OBJECT
        //LOCAL oHL AS HyperLabel
        //LOCAL sHelpInfo AS _winHelpInfo
        //LOCAL hTemp AS PTR
        //LOCAL cKey AS STRING

        //IF oCurrentHelp != NULL_OBJECT .AND. ! lHelpOn
        //	sHelpInfo := PTR(_CAST, oEvent:lParam)

        //	hTemp := sHelpInfo:hItemHandle

        //	IF (oObject :=__WCGetObjectByHandle(hTemp)) == NULL_OBJECT
        //		hTemp   := PTR(_CAST, GetWindowLong(hTemp, GWL_oWndPARENT))
        //		oObject :=__WCGetObjectByHandle(hTemp)
        //	ENDIF

        //	IF oObject != NULL_OBJECT
        //		IF sHelpInfo:iContextType == HELPINFO_WINDOW
        //			IF IsInstanceOf(oObject, #Control)
        //				oHL := ((Control)oObject):HyperLabel
        //				IF oHL != NULL_OBJECT .AND. oHL:HelpContext == NULL_STRING
        //					IF IsInstanceOf(oObject, #TabControl)
        //						oObject := ((TabControl)oObject):CurrentPage
        //					ELSE
        //						oObject := oObject:Owner
        //					ENDIF
        //				ENDIF
        //			ENDIF
        //			IF IsInstanceOf(oObject,#Window)
        //				IF IsInstanceOf(oObject,#__DocApp) .OR.;
        //				IsInstanceOf(oObject,#__WndApp) .OR.;
        //				IsInstanceOf(oObject,#__FormDialogWindow)
        //					oObject := oObject:Owner
        //				ENDIF
        //				IF IsInstanceOf(oObject,#__FormFrame)
        //					oObject := ((__FormFrame)oObject):DataWindow
        //				ENDIF
        //				oHL := oObject:HyperLabel
        //			ENDIF
        //		ELSE
        //			IF IsInstanceOf(oObject, #Menu)
        //				oHL := ((Menu)oObject):HyperLabel(sHelpInfo:iCtrlId)
        //			ENDIF
        //		ENDIF

        //		IF oHL != NULL_OBJECT
        //			cKEY := oHL:HelpContext
        //		ENDIF
        //		IF cKey == NULL_STRING .AND. IsInstanceOf(oObject, #ShellWindow)
        //			cKey := "HelpContents"
        //		ENDIF

        //		oCurrentHelp:Show(cKey, sHelpInfo)
        //		SELF:EventReturnValue := 1l
        //		RETURN TRUE
        //	ENDIF
        //ENDIF

        return false


    /// <exclude />
    method __ProcessHelpCursor(oWin as Window, wArea as longint) as logic strict
        local liTemp as longint
        local hTemp as ptr

        hTemp:=oWin:Handle()
        // is it a window or child window
        // if hTemp==oWnd .or. !IsInstanceOf(oWin,#Printer)
        if oWin is Window
            do case
            case wArea==HTCAPTION
                liTemp := RegionCaption
            case wArea==HTCLIENT
                liTemp := RegionCanvas
            case wArea==HTREDUCE
                liTemp := RegionMinBox
            case wArea==HTZOOM
                liTemp := RegionMaxBox
            case wArea==HTSYSMENU
                liTemp := RegionSystemMenuBox
            case wArea==HTBOTTOM .or. ;
                    wArea==HTBOTTOMLEFT .or. ;
                    wArea==HTBOTTOMRIGHT .or. ;
                    wArea==HTTOP .or. ;
                    wArea==HTLEFT .or. ;
                    wArea==HTRIGHT .or. ;
                    wArea==HTTOPLEFT .or. ;
                    wArea==HTTOPRIGHT
                liTemp := RegionBorder
            case wArea==HTMENU
                liTemp := RegionMenuBar
            case wArea == HTCLOSE
                liTemp := RegionClose
            case wArea==HTNowhere
                liTemp := RegionUnknown
            otherwise
                return false
            endcase
            GuiWin32.PostMessage(hTemp, WM_WCHelp, HelpWindow, liTemp)
        else
            GuiWin32.PostMessage(hTemp, WM_WCHELP, HelpControl, longint(_cast,hTemp))
        endif

        return true

    /// <exclude />

    [Obsolete];
    method __ProcessToolTip(oControlNotifyEvent as object) as void strict
        // Handled by windows forms
        return

    method __ReadResource(oResourceID as ResourceID, oOwner as object) as logic
        IF oResourceID != NULL_OBJECT
            oResourceDialog := ResourceDialog.FromCache(oResourceID:Handle(), oResourceID:Name)
            IF oResourceDialog == NULL
                oResourceDialog := ResourceDialog{oResourceID:Handle(), oResourceID:Name, oOwner}
            ENDIF
        endif
        if oResourceDialog != null_object
            self:Caption	:= oResourceDialog:Caption
            self:dwStyle	:= oResourceDialog:Style
            self:dwExStyle  := oResourceDialog:ExStyle
        endif
        return oResourceDialog != null_object

    /// <exclude />
    method __ReleaseDC() as void strict
        // needed ?
        //IF (hDC != NULL_PTR)
        //	WCDCDelete(SELF)
        //	ReleaseDC(SELF:Handle(4), hDC)
        //	hDC := NULL_PTR
        //ENDIF
        return


    /// <exclude />
    method __SetBrushNeeded(lNew as logic) as logic strict
        DCBrushNeeded := lNew
        return lNew


    /// <exclude />
    method __SetDCInitialized(lNew as logic) as logic strict
        DCInitialized := lNew
        return lNew


    /// <exclude />
    method __SetFont(oNewFont as Font) as Font strict
        oFont := oNewFont
        self:__SetFont()
        return oFont

    method __SetFont() as void strict
        if self:oFont != null_object .and. self:oWnd != null_object
            oWnd:Font := self:oFont:__Font
        endif
        return

    /// <exclude />
    method __SetPenNeeded(lNew as logic) as logic strict
        DCPenNeeded := lNew
        return lNew

        [Obsolete];
    method __SetSelfAssoAccel(lSwitch as logic) as void strict
        return

        [Obsolete];
    method __SetSelfMenu() as void strict
        return

    /// <exclude />
    method __Timer() as void strict

        dwTimerCount := dwTimerCount - 1
        if (dwTimerCount == 0)
            self:Timer()
            dwTimerCount := dwTimerInterval
            if (dwTimerCount == 0)
                WC.UnregisterTimer(self)
                lTimerRegistered := false
            endif
        endif

        return

    /// <exclude />

    [Obsolete];
    method __ToolTipHandle() as IntPtr strict
        // Tooltips are handled in the Panel class
        return IntPtr.Zero


    /// <exclude />
    method __UpdateTrayIcon(dwNIM as dword,oIcon as VOTrayIcon,dwID as dword,sToolTip as string)

        if oTrayIcon == null_object
            oTrayIcon:= VOTrayIcon{self, dwID}
        endif
        oTrayIcon:Text := sToolTip
        if oIcon != null_object
            oTrayIcon:Image := oIcon:Image
        endif
        oTrayIcon:Show()
        return 	self

    /// <include file="Gui.xml" path="doc/Window.Accelerator/*" />
    property Accelerator as Accelerator
        get => oAccelerator
        set => oAccelerator := value
    END PROPERTY



    /// <include file="Gui.xml" path="doc/Window.Activate/*" />
    method Activate(oEvent  as Event) as usual
        return self:Default(oEvent)
    /// <include file="Gui.xml" path="doc/Window.AddTrayIcon/*" />
    method AddTrayIcon(oTrayIcon, dwID, sToolTip)
        //PP-030902
        return self:__UpdateTrayIcon(NIM_ADD,oTrayIcon,dwID,sToolTip)


    /// <include file="Gui.xml" path="doc/Window.Animate/*" />
    method Animate(nTime as dword,nFlags as dword) as logic

        local dwTime,dwFlags as dword
        // Determine Windows version
        dwTime	:= nTime
        dwFlags := nFlags
        GuiWin32.AnimateWindow(self:Handle(),dwTime,dwFlags)
        return true

    /// <include file="Gui.xml" path="doc/Window.AnimationStart/*" />
    method AnimationStart(oControlEvent as ControlEvent) as usual
        return self:Default(oControlEvent)
    /// <include file="Gui.xml" path="doc/Window.AnimationStop/*" />
    method AnimationStop(oControlEvent as ControlEvent)as usual
        return self:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.AppCommand/*" />
    method AppCommand(oACEvent as AppCommandEvent) as logic
        // FALSE means the message has not been processed, so it is passed on to windows so other default behaviour can occur
        return false


    /// <include file="Gui.xml" path="doc/Window.Automated/*" />
    property Automated as logic
        get
            return lAutomated
        end get
        set
            if value != self:lAutomated
                self:lAutomated := value
                //#ifdef USE_OLEOBJECT
                //	IF SELF:lAutomated
                //		_VOOLERegisterAutomationObject(PTR(_CAST, SELF), NULL_PSZ, 1, FALSE)
                //		//PCALL(gpfnOLERegisterAutomationObject, PTR(_CAST, SELF), NULL_PSZ, 1, FALSE)
                //	ELSE
                //		_VOOLEUnRegisterAutomationObject(PTR(_CAST, SELF))
                //		//PCALL(gpfnOLEUnRegisterAutomationObject, PTR(_CAST, SELF))
                //	ENDIF
                //#endif
            endif
        end set
    end property


    /// <include file="Gui.xml" path="doc/Window.Background/*" />
    property Background  as Brush
        get
            return oBackground
        end get
        set


            oBackground := value
            if oBackGround != null_object .and. oBackground:Color != null_object
                self:__Surface:BackColor := 	oBackground:Color
            else
                // Need to add a paint handler
                nop
            endif
            return
        end set
    end property

    /// <include file="Gui.xml" path="doc/Window.ButtonClick/*" />
    method ButtonClick(oControlEvent as ControlEvent) as usual
        return self:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.ButtonDoubleClick/*" />
    method ButtonDoubleClick(oControlEvent as ControlEvent) as usual
        return self:Default(oControlEvent)
    /// <include file="Gui.xml" path="doc/Window.CanvasArea/*" />
    property CanvasArea  as BoundingBox get __Form:ClientRectangle


    /// <include file="Gui.xml" path="doc/Window.CanvasErase/*" />
    method CanvasErase() as void
        return


    /// <include file="Gui.xml" path="doc/Window.Caption/*" />
    property Caption as string
        get
            return cCaption
        end get
        set
            cCaption := value
            if self:__IsValid
                oWnd:Text := value
            endif
            return
        end set
    end property

    /// <include file="Gui.xml" path="doc/Window.Center/*" />
    method Center() as void
        if self:__IsValid
            self:__Form:Center()
        endif
        return


    /// <include file="Gui.xml" path="doc/Window.Close/*" />
    method Close(oEvent as Event) as usual
        if self:__IsValid .and. ! self:IsClosing
            self:IsClosing := true
            if oWnd:IsAttached
                oWnd:Close()
            endif
        endif
        return nil

    /// <include file="Gui.xml" path="doc/Window.ComboBoxExEndEdit/*" />
    method ComboBoxExEndEdit(oComboBoxExEndEditEvent as ComboBoxExEndEditEvent) as usual
        return nil

    /// <include file="Gui.xml" path="doc/Window.ComboBoxExNotify/*" />
    method ComboBoxExNotify(oControlNotifyEvent) as usual
        //Todo ComboBoxExNotify
        //LOCAL oCNE AS ControlNotifyEvent
        //oCNE := oControlNotifyEvent
        //IF oCNE:NotifyCode = CBEN_ENDEDIT
        //	SELF:ComboBoxExEndEdit(ComboBoxExEndEditEvent{oCNE})
        //ENDIF
        return nil

    /// <include file="Gui.xml" path="doc/Window.ContextMenu/*" />
    property ContextMenu as Menu
        get
            return oContextMenu
        end get
        set
            oContextMenu := value
            if self:__IsValid
                if value == null_object
                    oWnd:ContextMenu := null_object
                else
                    oWnd:ContextMenu := value:__Menu:AsContextMenu()

                endif
            endif
        end set
    end property
    method ContextMenuShow(oPos as Point) as void
        if self:__IsValid .and. oWnd:ContextMenu != null_object
            oWnd:ContextMenu:Show(oWnd, oPos)
        endif
        return

    /// <include file="Gui.xml" path="doc/Window.ControlFocusChange/*" />
    method ControlFocusChange(oControlFocusChangeEvent as ControlFocusChangeEvent) as usual
        return self:Default(oControlFocusChangeEvent)


    /// <include file="Gui.xml" path="doc/Window.ControlNotify/*" />
    method ControlNotify(oControlNotifyEvent as ControlNotifyEvent) as usual
        // Handling Window
        /*LOCAL oTargetWnd AS Window
        LOCAL nCode AS DWORD
        LOCAL lParam AS LONGINT

        // TCN_SELCHANGE, TCN_SELCHANGING, TCN_KEYDOWN
        LOCAL oControl 	AS OBJECT
        LOCAL oEvt			AS ControlNotifyEvent
        LOCAL strucNotify AS _winNMHDR


        //RvdH 061218 Copied some of the code from ControlNotify for performance reasons
        oEvt 			:= oControlNotifyEvent
        strucNotify := PTR(_CAST, oEvt:lParam)

        nCode 	:= strucNotify:_code
        lParam 	:= oEvt:lParam
        oControl :=__WCGetObjectByHandle(strucNotify:oWndFrom)

        IF (IsInstanceOf(SELF, #__FormDialogWindow) .AND. IsInstanceOf(oParent, #__FormFrame))
        oTargetWnd := ((__FormFrame)oParent):DataWindow
        oTargetWnd:EventReturnValue := 0
        ELSEIF (IsInstanceOf(SELF, #__FormFrame))
        oTargetWnd := ((__FormFrame)SELF):DataWindow
        oTargetWnd:EventReturnValue := 0
        ELSE
        oTargetWnd := SELF
        ENDIF

        DO CASE

        CASE nCode = NM_CUSTOMDRAW
        //PP-030319 Call control's CustomDraw method to handle notification. Thanks to S Ebert
        IF oControl != NULL_OBJECT .AND. IsMethod(oControl, #CustomDraw)
        oTargetWnd:EventReturnValue := Send(oControl, #CustomDraw, lParam)
        ENDIF

        CASE nCode == LVN_ODCACHEHINT
        //PP-031115
        IF IsMethod(oControl,  #__CacheHint)
        Send(oControl, #__CacheHint, oEvt)
        ENDIF

        //SE-060519
        CASE nCode = LVN_GETDISPINFO  .OR. nCode = TVN_GETDISPINFO .OR.;
        nCode = CBEN_GETDISPINFO .OR. nCode = TBN_GETDISPINFO .OR. nCode = HDN_GETDISPINFOA
        IF (oControl != NULL_OBJECT)
        oControl:__GetDispInfo(oEvt)
        ENDIF

        CASE nCode == EN_PROTECTED
        oTargetWnd:RichEditProtected(RichEditProtectEvent{oEvt})

        CASE nCode == EN_SELCHANGE
        oTargetWnd:RichEditSelectionChange(RichEditSelectionEvent{oEvt})

        CASE nCode == EN_STOPNOUNDO
        oTargetWnd:RichEditUndoLost(oEvt)

        CASE nCode == LVN_BEGINDRAG .OR. nCode == LVN_BEGINRDRAG
        IF IVarGet(oControl, #DragDropEnabled)
        oTargetWnd:ListViewItemDrag(ListViewDragEvent{oEvt})
        ENDIF

        CASE nCode == LVN_BEGINLABELEDIT .OR. nCode == LVN_ENDLABELEDIT
        oTargetWnd:ListViewItemEdit(ListViewEditEvent{oEvt})

        CASE nCode == LVN_COLUMNCLICK
        oTargetWnd:ListViewColumnClick(ListViewColumnClickEvent{oEvt})

        CASE nCode == LVN_DELETEITEM
        oTargetWnd:ListViewItemDelete(ListViewDeleteEvent{oEvt})

        CASE nCode == LVN_KEYDOWN
        oTargetWnd:ListViewKeyDown(ListViewKeyEvent{oEvt})

        CASE nCode == LVN_ITEMCHANGING
        oTargetWnd:ListViewItemChanging(ListViewItemEvent{oEvt})

        CASE nCode == LVN_ITEMCHANGED
        //PP-031115
        IF IsInstanceOfUsual(oControl, #DataListView)
        ((DataListView) oControl):__ItemChanged( oEvt)
        ENDIF
        oTargetWnd:ListViewItemChanged( ListViewItemEvent{oEvt})

        CASE nCode == LVN_ODFINDITEM
        //PP-030319 Corrected setting of return value. Fixes problem finding item in DLV on DW. Thanks to S Ebert
        //PP-031115
        IF IsMethod(oControl,  #__FindItem)
        oTargetWnd:EventReturnValue := Send(oControl, #__FindItem, oEvt)
        ENDIF

        CASE nCode == NM_CLICK .OR. nCode == NM_RCLICK
        IF IsInstanceOf(oControl, #TreeView)
        oTargetWnd:TreeViewMouseButtonDown( TreeViewMouseEvent{oEvt})
        ELSEIF IsInstanceOf(oControl, #ListView)
        oTargetWnd:ListViewMouseButtonDown( ListViewMouseEvent{oEvt})
        ELSEIF IsInstanceOf(oControl, #SysLink)
        oTargetWnd:SysLinkSelect( SysLinkSelectEvent{oEvt})
        ENDIF

        CASE nCode == NM_DBLCLK .OR. nCode == NM_RDBLCLK
        IF IsInstanceOf(oControl, #TreeView)
        oTargetWnd:TreeViewMouseButtonDoubleClick(TreeViewMouseEvent{oEvt})
        ELSEIF IsInstanceOf(oControl, #ListView)
        oTargetWnd:ListViewMouseButtonDoubleClick(ListViewMouseEvent{oEvt})
        ENDIF

        CASE nCode == TCN_SELCHANGE
        IF IsInstanceOf(oControl, #TabControl)
        // !!! was in wrong order in 730 !!!
        ((TabControl)oControl):__FocusPage( TabCtrl_GetCurSel(oControl:Handle()))
        oTargetWnd:TabSelect(oEvt)
        ENDIF

        CASE nCode == TCN_SELCHANGING
        oTargetWnd:TabSelectionChanging(oEvt)

        CASE nCode == TCN_KEYDOWN
        oTargetWnd:TabKeyDown(oEvt)

        CASE nCode == TTN_NEEDTEXT // is identical to TTN_GETDISPINFO
        //PP-030909 Move this CASE to a separate method
        SELF:__ProcessToolTip(oEvt)

        CASE nCode == TVN_BEGINDRAG .OR. nCode == TVN_BEGINRDRAG
        IF IVarGet(oControl, #DragDropEnabled)
        oTargetWnd:TreeViewItemDrag(TreeViewDragEvent{oEvt})
        ENDIF

        CASE nCode == TVN_BEGINLABELEDIT .OR. nCode == TVN_ENDLABELEDIT
        oTargetWnd:TreeViewItemEdit(TreeViewEditEvent{oEvt})

        CASE nCode == TVN_DELETEITEM
        oTargetWnd:TreeViewItemDelete( TreeViewDeleteEvent{oEvt})

        CASE nCode == TVN_ITEMEXPANDED
        oTargetWnd:TreeViewItemExpanded( TreeViewExpandedEvent{oEvt})

        CASE nCode == TVN_ITEMEXPANDING
        oTargetWnd:TreeViewItemExpanding( TreeViewExpandingEvent{oEvt})

        CASE nCode == TVN_KEYDOWN
        oTargetWnd:TreeViewKeyDown( TreeViewKeyEvent{oEvt})

        CASE nCode == TVN_SELCHANGEDA
        oTargetWnd:TreeViewSelectionChanged( TreeViewSelectionEvent{oEvt})

        CASE nCode == TVN_SELCHANGINGA
        oTargetWnd:TreeViewSelectionChanging( TreeViewSelectionEvent{oEvt})

        CASE (nCode == MCN_SELECT) .OR. (nCode == MCN_SELCHANGE)
        oTargetWnd:MonthCalSelectionChanged( MonthCalSelectionEvent{oEvt})

        CASE (nCode == DTN_DATETIMECHANGE)
        oTargetWnd:DateTimeSelectionChanged( DateTimeSelectionEvent{oEvt})

        CASE (nCode == RBN_HEIGHTCHANGE)
        oTargetWnd:ToolBarHeightChanged( oEvt)

        //SE-060519
        CASE nCode >= CBEN_LAST .AND. nCode <= CBEN_FIRST
        oTargetWnd:ComboBoxExNotify( oEvt)

        OTHERWISE
        //PP-040504 Forwards control's parent notify messages back to the control. Thanks to S Ebert
        IF oControl != NULL_OBJECT .AND. IsMethod(oControl, #ParentNotify)
        oTargetWnd:EventReturnValue := Send(oControl, #ParentNotify, nCode, lParam)
        ENDIF

        END CASE

        IF (oTargetWnd != SELF)
        SELF:EventReturnValue := oTargetWnd:EventReturnValue
        ENDIF

        IF (SELF:EventReturnValue == 0)
        RETURN SELF:Default(oEvt)
        ENDIF*/
        return nil

    /// <include file="Gui.xml" path="doc/Window.DateTimeSelectionChanged/*" />
    method DateTimeSelectionChanged(oDateTimeSelectionEvent as DateTimeSelectionEvent) as usual
        //Sets the modified flag only, if the DTP-Control becomes changed.
        //In ComCtrl32 DLL below version 6, this eventhandler becomes called twice
        //if you change the date with the calender control.
        //With this change the workaround in the DateTimePicker:__Update() method
        //is not longer needed. The old workaround worked not correct in all cases.

        local oDTPicker as DateTimePicker
        local cText     as string
        local cOldValue  as string
        oDTPicker := (DateTimePicker) oDateTimeSelectionEvent:Control
        cOldValue := oDTPicker:AsString()
        cText := oDTPicker:TextValue
        // Convert the DatePicker empty date to VEWA empty date
        if cText == "01.01.1753"
            cText := "  .  .    "
        endif
        if oDTPicker:FieldSpec != null_object
            cText := AsString(oDTPicker:FieldSpec:Val(cText))
        endif
        if ! cOldValue == cText
            oDTPicker:Modified := true
            if IsInstanceOf(oDTPicker:Owner, #DataWindow)
                ((DataWindow) oDTPicker:Owner):__DoValidate(oDTPicker)
            endif
        endif
        if oDTPicker:NullFormat .and. oDTPicker:SelectedDate != null_date
            // Re-assign the value so the format gets set
            oDTPicker:SelectedDate := oDTPicker:SelectedDate
        elseif !oDTPicker:NullFormat .and. oDTPicker:SelectedDate == null_date
            // Reassign the selected Date so the format gets set
            oDTPicker:SelectedDate := null_date
        endif



        return 0l

    /// <include file="Gui.xml" path="doc/Window.DeActivate/*" />
    method DeActivate(oEvent as Event)  as usual
        self:DeactivateAllOLEObjects()
        return self:Default(oEvent)


    /// <include file="Gui.xml" path="doc/Window.DeactivateAllOLEObjects/*" />
    method DeactivateAllOLEObjects(oExcept) as usual
#ifdef USE_OLEOBJECTS
        //RvdH 041123 Added method at Window Level
        //				  Also removed lNeedUpdate
        //LOCAL i 	  		AS DWORD
        //LOCAL oOLE	  		AS OBJECT
        //LOCAL aObjects 		AS ARRAY
        //LOCAL oException  	AS OleObject
        //IF IsObject(oExcept)
        //	oException := oExcept
        //ENDIF
        //aObjects := SELF:__GetMyOleObjects()
        //FOR i:= 1 TO ALen(aObjects)
        //	oOLE := aObjects[i]
        //	IF oOle <> oException
        //		oOLE:Deactivate()
        //		//IF oOLE:IsInPlaceActive

        //		//ENDIF
        //	ENDIF
        //NEXT
#endif
        return self



    /// <include file="Gui.xml" path="doc/Window.Default/*" />
    method Default(oEvent as Event) as usual
        self:EventReturnValue := 1L
        return self

    /// <include file="Gui.xml" path="doc/Window.DeleteTrayIcon/*" />
    method DeleteTrayIcon(dwID)  as usual
        DEFAULT(ref dwID, 0)
        if oTrayIcon != null_object
            if oTrayIcon:ID == dwID
                oTrayIcon:Destroy()
            endif
        endif
        return true

    /// <include file="Gui.xml" path="doc/Window.Destroy/*" />
    method Destroy() as usual clipper
        if lAutomated
            self:Automated := false
        endif

        //__WCUnregisterMenu(oMenu)
        //__WCUnregisterMenu(oContextMenu)

        if self:__IsValid
            if oWnd:IsAttached
                oWnd:Dispose()
            endif

            if (oToolBar != null_object)
                oToolBar:Destroy()
                oToolBar := null_object
            endif

            oMenu := null_object
            oIcon := null_object
            oIconSmall := null_object
            oContextMenu := null_object
            oWnd := null_object
            hDC := null_ptr
            oAccelerator := null_object
            oFont := null_object
            oPointer := null_object
            oBackground := null_object
            oForeground := null_object
            lTimerRegistered := false
        endif

        super:Destroy()
        UnregisterAxit(self)
        //Gc.Collect()
        return nil


    /// <include file="Gui.xml" path="doc/Window.Disable/*" />
    method Disable()  as void
        if self:__IsValid
            oWnd:Enabled := false
        endif
        return


        //ACCESS DragDropClient AS DragDropClient
        //	RETURN oDragDropClient


        //ACCESS DragDropServer AS DragDropServer
        //	RETURN oDragDropServer


        //ACCESS DragImageList AS VOSDK.ImageList
        //	RETURN oDragImageList


    /// <include file="Gui.xml" path="doc/Window.Draw/*" />
    method Draw(oDrawObject) as usual
        local cnt, i as dword
        local oDraw as DrawObject
        local aDraw as array

        if (oWnd == null_object) .and. !IsInstanceOf(self, #Printer)
            return self
        endif

        if !IsArray(oDrawObject)
            if !(oDrawObject is DrawObject)
                WCError{#Draw,#Window,__WCSTypeError,oDrawObject,1}:Throw()
            endif
            oDraw := oDrawObject
            oDraw:__SetWindow(self)
            oDraw:Draw()
        else
            aDraw := oDrawObject
            cnt := ALen(aDraw)
            for i:=1 to cnt
                if !(aDraw[i] is DrawObject)
                    WCError{#Draw,#Window,__WCSTypeError,oDrawObject[i],1}:Throw()
                endif
                oDraw := aDraw[i]
                oDraw:__SetWindow(self)
                oDraw:Draw()
            next
        endif

        return self


    /// <include file="Gui.xml" path="doc/Window.DrawBackground/*" />
    method DrawBackground(hdc, oWindow) as usual
        return false


    /// <include file="Gui.xml" path="doc/Window.Drop/*" />
    method Drop(oDragEvent as DragEvent ) as usual
        return nil

    /// <include file="Gui.xml" path="doc/Window.EditChange/*" />
    method EditChange(oControlEvent as ControlEvent) as usual
        return self:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.EditFocusChange/*" />
    method EditFocusChange(oEditFocusChangeEvent as EditFocusChangeEvent) as usual
        return self:Default(oEditFocusChangeEvent)
    /// <include file="Gui.xml" path="doc/Window.EditScroll/*" />
    method EditScroll(oControlEvent as ControlEvent) as usual
        return self:Default(oControlEvent)
    /// <include file="Gui.xml" path="doc/Window.Enable/*" />
    method Enable()  as void
        if self:__IsValid
            oWnd:Enabled := true
        endif
        return

    /// <include file="Gui.xml" path="doc/Window.EnableCloseBox/*" />
    method EnableCloseBox(uValue := true as logic) as void
        local hBox as IntPtr
        if self:__IsValid
            hBox := GuiWin32.GetSystemMenu(oWnd:Handle,false)
        endif
        if hBox != IntPtr.Zero
            if uValue
                GuiWin32.EnableMenuItem(hBox,SC_CLOSE,MF_ENABLED)
            else
                GuiWin32.EnableMenuItem(hBox,SC_CLOSE,_or(MF_GRAYED,MF_BYCOMMAND))
            endif
        endif

        return

    /// <include file="Gui.xml" path="doc/Window.EnableDragDropClient/*" />
    method EnableDragDropClient(lEnable := true as logic)  as usual
        if self:__IsValid
            self:__Form:AllowDrop := lEnable
            if lEnable
                self:oDragDropClient := DragDropClient{self}
            else
                if self:oDragDropClient != null_object
                    self:oDragDropClient:Destroy()
                    self:oDragDropClient:= null_object
                endif
            endif
        endif
        return self

    /// <include file="Gui.xml" path="doc/Window.EnableDragDropServer/*" />

    [Obsolete];
    method EnableDragDropServer(lEnable := true as logic) as void
        //IF lEnable
        //	IF (oDragDropServer == NULL_OBJECT)
        //		oDragDropServer := DragDropServer{SELF}
        //	ENDIF
        //ELSEIF (oDragDropServer != NULL_OBJECT)
        //	oDragDropServer:Destroy()
        //	oDragDropServer := NULL_OBJECT
        //ENDIF

        return


    /// <include file="Gui.xml" path="doc/Window.EnableHelp/*" />
    method EnableHelp(lEnable as logic, oHelpDisplay as HelpDisplay) as void

        if lHelpOn
            // Disable previous Help
            lHelpOn := false
            self:__EnableHelpCursor(false)
            if (oApp != null_object)
                oApp:__SetHelpWind(null_object, HM_NONE)
            endif
        endif

        if lEnable
            // Enable New Help
            oCurrentHelp := oHelpDisplay
            if oCurrentHelp = null_object .or. ! oCurrentHelp:Win32Processing
                lHelpOn := true
                //IF (GuiWin32.GetActiveWindow() == oWnd:Handle) .AND. (oApp != NULL_OBJECT)
                if (oApp != null_object)
                    oApp:__SetHelpWind(oWnd, HM_GENERAL)
                endif
            endif
        else
            oCurrentHelp := null_object
        endif

        return


    /// <include file="Gui.xml" path="doc/Window.EnableHelpButton/*" />
    method EnableHelpButton() as void strict
        if self:__IsValid
            oWnd:HelpButton := true
        endif
        return


    /// <include file="Gui.xml" path="doc/Window.EnableHelpCursor/*" />
    method EnableHelpCursor() as void strict
        if lHelpOn
            self:__EnableHelpCursor(true)
        else
            GuiWin32.PostMessage(self:Handle(), WM_SYSCOMMAND, SC_CONTEXTHELP, 0)
        endif

        return


    /// <include file="Gui.xml" path="doc/Window.EnableThemeDialogTexture/*" />
    method EnableThemeDialogTexture(dwStyle as dword) as void
        // Todo EnableThemeDialogTexture

        //RETURN EnableThemeDialogTexture(SELF,dwStyle)
        return

    /// <include file="Gui.xml" path="doc/Window.EnableToolTips/*" />
    method EnableToolTips(lEnable := true as logic) as void
        // Todo EnableToolTips

        if lEnable
            if self:__Surface is VOPanel var panel
                panel:EnableToolTips(lEnable)
            endif
        endif
        return


    /// <include file="Gui.xml" path="doc/Window.Expose/*" />
    method Expose(oExposeEvent as ExposeEvent) as usual
        return self:Default(oExposeEvent)

    /// <include file="Gui.xml" path="doc/Window.FocusChange/*" />
    method FocusChange(oFocusChangeEvent as FocusChangeEvent) as usual
        return self:Default(oFocusChangeEvent)
    /// <include file="Gui.xml" path="doc/Window.Font/*" />
    property Font as VOSDK.Font
        get
            return oFont
        end get
        set

            oFont := value
            self:__SetFont()
        end set
    end property


    /// <include file="Gui.xml" path="doc/Window.Foreground/*" />
    property Foreground as Brush
        get
            return oForeground
        end get
        set
            oForeground := value
            DCBrushInUse := false
            DCBrushNeeded := true
            // this forces the new object to be selected into the current DC and allows proper releasing of the old one
            self:__GetDC()

        end set
    end property


    /// <include file="Gui.xml" path="doc/Window.GetAllChildren/*" />
    method GetAllChildren() as array strict
        local aRet as array
        aRet := {}
        // Add all MDI Children
        if self:__IsValid .and. __Form:IsMDIContainer
            foreach form as System.Windows.Forms.Form in __Form:MdiChildren
                if form is VOForm
                    local oVoForm as VOForm
                    oVoForm := (VOForm)  form
                    AADD(aRet,oVoForm:Window)
                endif
            next
        endif
        // Add all subwindows on the surface
        if self:__Surface != null_object
            foreach var control in self:__Surface:Controls
                if control is VOForm
                    local oVoForm as VOForm
                    oVoForm := (VOForm)  control
                    AADD(aRet,(usual) oVoForm:Window)
                elseif control is VOMenu
                    // Skip
                    nop
                elseif control is VOToolbar
                    // Skip
                    nop
                elseif control is VOStatusBar
                    // Skip
                    nop
                elseif control is VOLabel
                    // Skip
                    nop
                elseif control is IVOControl
                    var oControl := control astype IVOControlProperties
                    aRet:Add(oControl:Control)
                    // Get the children of the group boxes also in this list
                    if oControl is VOGroupBox
                        local aGroupChildren as IList<IVOControl>
                        var oGroup :=  control astype VOGroupBox
                        aGroupChildren := oGroup:getAllChildren(null)
                        foreach oc as IVOCOntrolProperties in aGroupChildren
                            AAdd(aRet,oC:Control)
                        next
                    endif
                endif
            next
        endif
        return aRet


    method GetDlgItem(nItem as long) as ResourceDialogItem
        // For a Window based on a resource, this returns the control from the resource
        if oResourceDialog != null_object
            return oResourceDialog:GetDlgItem(nItem)
        endif
        return null_object

    /// <include file="Gui.xml" path="doc/Window.GetStyle/*" />
    method GetStyle() as long
        if self:__IsValid
            return GuiWin32.GetWindowStyle(oWnd:Handle)
        endif
        return self:dwStyle

    /// <include file="Gui.xml" path="doc/Window.GetExStyle/*" />
    method GetExStyle as long
        if self:__IsValid
            return GuiWin32.GetWindowExStyle(oWnd:Handle)
        endif
        return self:dwExStyle


    /// <include file="Gui.xml" path="doc/Window.Handle/*" />
    method Handle() as IntPtr  clipper
        local hWin as Intptr
        if self:__IsValid
            hWin := oWnd:Handle
            return hWin
        endif
        return IntPtr.Zero

    /// <include file="Gui.xml" path="doc/Window.HasExStyle/*" />
    method HasExStyle(kStyle as long) as logic
        local liStyle	as long
        liStyle := self:GetExStyle()
        return _and(liStyle,kStyle) != 0

    /// <include file="Gui.xml" path="doc/Window.HasStyle/*" />
    method HasStyle(kStyle as long) as logic
        local liStyle	as long
        liStyle := self:GetStyle()
        return _and(liStyle,kStyle) != 0

    /// <include file="Gui.xml" path="doc/Window.HelpDisplay/*" />
    property HelpDisplay as HelpDisplay get oCurrentHelp set self:EnableHelp(true, value)

    /// <include file="Gui.xml" path="doc/Window.HelpRequest/*" />
    method HelpRequest(oHelpRequestEvent as HelpRequestEvent) as usual
        local dwType as long
        local oHRE as HelpRequestEvent
        local oP as object

        oP := self:Owner
        do while IsInstanceOf(oP, #Window)
            ((Window) oP):__EnableHelpCursor(false)
            oP := ((Window) oP):Owner
        enddo

        if IsInstanceOfUsual(oHelpRequestEvent, #HelpRequestEvent)
            if self:HelpDisplay==null_object
                if IsMethod(self:oParent, #HelpRequest)
                    Send(self:oParent, #HelpRequest, oHelpRequestEvent)
                endif
            else
                oHRE := oHelpRequestEvent
                dwType := oHRE:HelpType
                do case
                case dwType == HELPMENU
                    nop
                case dwType == HELPCONTROL
                    if ! empty(oHRE:HelpContext)
                        oCurrentHelp:Show(oHRE:HelpContext)
                    elseif ! empty(oHRE:HyperLabel:Name)
                        oCurrentHelp:Show(oHRE:HyperLabel:Name)
                    else
                        oCurrentHelp:Show("Control_"+AsString(oHRE:Control:ControlID))
                    endif
                case dwType == HELPWINDOW
                    if ! empty(oHRE:HelpContext)
                        oCurrentHelp:Show(oHRE:HelpContext)
                    else
                        oCurrentHelp:Show("HelpIndex")
                    endif
                case dwType == HELPINFO
                    nop
                endcase
            endif
        endif


        return nil


    /// <include file="Gui.xml" path="doc/Window.Hide/*" />
    method Hide() as void strict
        if self:__IsValid
            oWnd:Visible := false
        endif

        return


    /// <include file="Gui.xml" path="doc/Window.HorizontalScroll/*" />
    method HorizontalScroll(oScrollEvent as ScrollEvent)  as usual
        local oScrollBar as ScrollBar
        local oEvt	:= oScrollEvent as ScrollEvent

        oScrollBar := oEvt:ScrollBar
        if (oScrollBar != null_object)
            oScrollBar:ThumbPosition := oEvt:Position
        endif

        return self:Default(oEvt)


    /// <include file="Gui.xml" path="doc/Window.HorizontalSlide/*" />
    method HorizontalSlide(oSliderEvent as SliderEvent)  as usual
        local oSlider as Slider
        oSlider := oSliderEvent:Slider
        if (oSlider != null_object)
            oSlider:ThumbPosition := oSliderEvent:Position
        endif
        return self:Default(oSliderEvent)


    /// <include file="Gui.xml" path="doc/Window.HorizontalSpin/*" />
    method HorizontalSpin(oSpinnerEvent as SpinnerEvent)  as usual
        local oSpinner as Spinner
        oSpinner := oSpinnerEvent:Spinner
        if (oSpinner != null_object)
            oSpinner:Position := oSpinnerEvent:Position
        endif

        return self:Default(oSpinnerEvent)


    /// <include file="Gui.xml" path="doc/Window.HyperLabel/*" />
    property HyperLabel as HyperLabel
        get
            return oHyperLabel
        end get
        set
            oHyperLabel := value
            self:@@StatusMessage(value, MESSAGEPERMANENT)
        end set
    end property


    /// <include file="Gui.xml" path="doc/Window.Icon/*" />
    property Icon as Icon
        get
            return oIcon
        end get
        set
            oIcon := value
            if self:__IsValid
                self:__Form:Icon := oIcon
            endif
        end set
    end property


    /// <include file="Gui.xml" path="doc/Window.IconSm/*" />
    property IconSm as Icon
        get
            return oIconSmall
        end get
        set

            oIconSmall := value
            if self:__IsValid
                self:__Form:SmallIcon := oIconSmall
            endif
        end set
    end property


    /// <include file="Gui.xml" path="doc/Window.ctor/*" />
    constructor(oOwner)


        super()
        if oOwner != null_object
            if oOwner is System.Windows.Forms.Form
                oParent := __ForeignWindow{oOwner}
            else
                oParent := oOwner
            endif
        endif
        oOrigin := Point{0,0}

        aAlignes		:= {}
        aDelayedAlignes := List< Tuple <IGUIObject, usual> >{}
        self:lDelayAlignment := true
        oFont := System.Drawing.SystemFonts.DefaultFont

        //PP-030910
        self:SetBackgroundBrush()
        oWnd  := (VoForm) self:__CreateForm()
        if oWnd != null_object
            oWnd:Visible := false
            self:__SetFont()

        endif
        return


    /// <include file="Gui.xml" path="doc/Window.IsEnabled/*" />
    method IsEnabled()  as logic strict
        if oWnd != null_object
            return oWnd:Enabled
        endif
        return false

    /// <include file="Gui.xml" path="doc/Window.IsIconic/*" />
    method IsIconic() as logic strict
        if oWnd != null_object
            return oWnd:WindowState == System.Windows.Forms.FormWindowState.Minimized
        endif
        return false


    /// <include file="Gui.xml" path="doc/Window.IsVisible/*" />
    method IsVisible()  as logic strict
        if self:__IsValid
            return oWnd:Visible
        endif
        return false

    /// <include file="Gui.xml" path="doc/Window.IsZoomed/*" />
    method IsZoomed() as logic strict
        if self:__IsValid
            return oWnd:WindowState == System.Windows.Forms.FormWindowState.Maximized
        endif
        return false

    /// <include file="Gui.xml" path="doc/Window.KeyDown/*" />
    method KeyDown(oKeyEvent as KeyEvent) as usual
        return self:Default(oKeyEvent)

    /// <include file="Gui.xml" path="doc/Window.KeyUp/*" />
    method KeyUp(oKeyEvent as KeyEvent) as usual
        return self:Default(oKeyEvent)
    /// <include file="Gui.xml" path="doc/Window.LineTo/*" />
    method LineTo(oPoint as Point) as usual
        //Todo
        //LOCAL dwLen, i AS DWORD
        //LOCAL oPT AS Point
        //LOCAL aPT AS ARRAY

        //IF (oWnd != NULL_PTR) .OR. IsInstanceOf(SELF, #Printer)
        //	DCPenNeeded := TRUE
        //	IF (SELF:__GetDC() != NULL_PTR)
        //		IF !IsArray(oPoint)
        //			LineTo(hDC, oPoint:x, oPoint:y)
        //		ELSE
        //			aPT := oPoint
        //			dwLen := ALen(aPT)
        //			FOR i:=1 UPTO dwLen
        //				oPT := aPT[i]
        //				LineTo(hDC, oPT:X, oPT:Y)
        //			NEXT
        //		ENDIF
        //	ENDIF
        //ENDIF

        return oPoint


    /// <include file="Gui.xml" path="doc/Window.ListBoxClick/*" />
    method ListBoxClick(oControlEvent as ControlEvent) as usual
        return self:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.ListBoxSelect/*" />
    method ListBoxSelect(oControlEvent  as ControlEvent) as usual
        return self:Default(oControlEvent)
    /// <include file="Gui.xml" path="doc/Window.ListViewColumnClick/*" />
    method ListViewColumnClick(oListViewColumnClickEvent as ListViewColumnClickEvent) as usual
        return self:Default(oListViewColumnClickEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewItemChanged/*" />
    method ListViewItemChanged(oListViewItemEvent as ListViewItemEvent) as usual
        return self:Default(oListViewItemEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewItemChanging/*" />
    method ListViewItemChanging(oListViewItemEvent as ListViewItemEvent) as usual
        return self:Default(oListViewItemEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewItemDelete/*" />
    method ListViewItemDelete(oListViewDeleteEvent as ListViewDeleteEvent)  as usual
        return self:Default(oListViewDeleteEvent)
    /// <include file="Gui.xml" path="doc/Window.ListViewItemDrag/*" />
    method ListViewItemDrag(oListViewDragEvent as ListViewDragEvent)  as usual
        // Todo
        //LOCAL oControl AS ListView
        //LOCAL oPoint AS Point
        //LOCAL oEvt :=oListViewDragEvent AS ListViewDragEvent

        //oControl := OBJECT(oEvt:Control)

        //IF oControl:DragDropEnabled
        //	IF oControl:DragImageList == NULL_OBJECT
        //		oDragImageList := oControl:__CreateDragImageList(oEvt:ListViewItem:ItemIndex)
        //		oDragImageList:BeginDrag(1)
        //	ELSE
        //		oDragImageList := oControl:DragImageList
        //		oDragImageList:BeginDrag(oEvt:ListViewItem:ImageIndex)
        //	ENDIF
        //	oPoint := oEvt:Position
        //	oPoint:X += oControl:Origin:X
        //	oPoint:Y += oControl:Origin:Y
        //	oDragImageList:DragEnter(oPoint, SELF)
        //	lDragActive := TRUE
        //	ShowCursor(FALSE)
        //	SetCapture(SELF:Handle())
        //ENDIF

        return self:Default(oListViewDragEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewItemEdit/*" />
    method ListViewItemEdit(oListViewEditEvent as ListViewEditEvent) as usual
        return self:Default(oListViewEditEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewKeyDown/*" />
    method ListViewKeyDown(oListViewKeyEvent as ListViewKeyEvent) as usual
        return self:Default(oListViewKeyEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewMouseButtonDoubleClick/*" />
    method ListViewMouseButtonDoubleClick(oListViewMouseEvent as ListViewMouseEvent) as usual
        return self:Default(oListViewMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewMouseButtonDown/*" />
    method ListViewMouseButtonDown(oListViewMouseEvent as ListViewMouseEvent) as usual
        return self:Default(oListViewMouseEvent)
    /// <include file="Gui.xml" path="doc/Window.Menu/*" />
    property Menu as Menu
        get
            return oMenu
        end get
        set
            oMenu := value
            oMenu:__Owner := self
            if self:__IsValid
                self:__Form:Menu := oMenu:__Menu
            endif
            foreach oItem as VOMenuItem in value:__Menu:MenuItems
                oItem:MergeType := System.Windows.Forms.MenuMerge.Remove
            next

            if (oMenu == null_object)
                self:Accelerator := null_object
                self:ToolBar := null_object
            else
                self:Accelerator := oMenu:Accelerator
                self:ToolBar := oMenu:ToolBar
                oMenu:SetShortCuts(self:Accelerator)
            endif
        end set
    end property


    /// <include file="Gui.xml" path="doc/Window.MenuCommand/*" />
    method MenuCommand(oMenuCommandEvent as MenuCommandEvent) as usual
        return self:Default(oMenuCommandEvent)

    /// <include file="Gui.xml" path="doc/Window.MenuInit/*" />
    method MenuInit(oMenuInitEvent as MenuInitEvent) as usual
        return self:Default(oMenuInitEvent)
    /// <include file="Gui.xml" path="doc/Window.MenuSelect/*" />
    method MenuSelect(oMenuSelectEvent as MenuSelectEvent) as usual
        return self:Default(oMenuSelectEvent)

    /// <include file="Gui.xml" path="doc/Window.MinMaxInfo/*" />
    method MinMaxInfo(oMinMaxInfoEvent as MinMaxInfoEvent) as void
        if oMinSize != null_object
            oMinMaxInfoEvent:MinTrackSize := oMinSize
        endif
        return


    /// <include file="Gui.xml" path="doc/Window.MinSize/*" />
    property MinSize as Dimension get oMinSize set oMinSize := value

    /// <include file="Gui.xml" path="doc/Window.ModifyTrayIcon/*" />
    method ModifyTrayIcon(oTrayIcon as VOTrayIcon, dwID as dword, sToolTip as string) as usual
        //PP-030902
        return self:__UpdateTrayIcon(NIM_MODIFY,oTrayIcon,dwID,sToolTip)

    /// <include file="Gui.xml" path="doc/Window.MonthCalSelectionChanged/*" />
    method MonthCalSelectionChanged(oMonthCalSelectionEvent as MonthCalSelectionEvent) as usual
        // Todo
        //LOCAL oMonthCal AS MonthCalendar
        //Local oMonthCalSelectionEvent as MonthCalSelectionEvent
        //oMonthCalSelectionEvent := _oMonthCalSelectionEvent
        //oMonthCal := (MonthCalendar) oMonthCalSelectionEvent:Control
        //oMonthCal:Modified := TRUE
        //IF oMonthCalSelectionEvent:Explicit
        //	oMonthCal:SetFocus()
        //ENDIF

        //IF IsInstanceOf(oMonthCal:Owner, #DataWindow)
        //	((DataWindow) oMonthCal:Owner):__DoValidate(oMonthCal)
        //ENDIF

        return self:Default(oMonthCalSelectionEvent)



    /// <include file="Gui.xml" path="doc/Window.MouseButtonDoubleClick/*" />
    method MouseButtonDoubleClick(oMouseEvent as MouseEvent) as usual
        return self:Default(oMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.MouseButtonDown/*" />
    method MouseButtonDown(oMouseEvent  as MouseEvent) as usual
        return self:Default(oMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.MouseButtonUp/*" />
    method MouseButtonUp(oMouseEvent as MouseEvent)  as usual
        // Todo MouseButtonUp

        //IF lDragActive
        //	ReleaseCapture()
        //	ShowCursor(TRUE)
        //	oDragImageList:EndDrag()
        //	oDragImageList := NULL_OBJECT
        //	lDragActive := FALSE
        //ENDIF


        return self:Default(oMouseEvent)


    /// <include file="Gui.xml" path="doc/Window.MouseDrag/*" />
    method MouseDrag(oMouseEvent as MouseEvent)  as usual
        // Todo MouseDrag
        //LOCAL oEvt := oMouseEvent AS MouseEvent

        //IF lDragActive
        //	oDragImageList:DragMove(oEvt:Position)
        //	//ImageList_DragMove(oMouseEvent:Position:Y, oMouseEvent:Position:Y)
        //ENDIF
        return self:Default(oMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.MouseButtonUp/*" />
    method MouseMove(oMouseEvent as MouseEvent) as usual
        return self:Default(oMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.MouseTrapOff/*" />
    method MouseTrapOff() as void strict
        // Todo MouseTrapOff
        //ReleaseCapture()
        return


    /// <include file="Gui.xml" path="doc/Window.MouseTrapOn/*" />
    method MouseTrapOn()  as void strict
        // Todo MouseTrapOn
        //SetCapture(SELF:Handle(0))
        return


    /// <include file="Gui.xml" path="doc/Window.Move/*" />
    method Move(oMoveEvent as MoveEvent)as usual
        return self:Default(oMoveEvent)
    /// <include file="Gui.xml" path="doc/Window.MoveTo/*" />
    method MoveTo(oPoint as Point)  as Point
        // Todo
        //LOCAL winPoint IS _winPoint  // dcaton 070316 was _winsize
        //IF (oWnd != NULL_PTR) .OR. IsInstanceOf(SELF, #Printer)
        //	DCPenNeeded := TRUE
        //	IF (SELF:__GetDC() != NULL_PTR)
        //		MoveToEx(hDC,oPoint:X,oPoint:Y, @winPoint)
        //		RETURN Point{winPoint:x,winPoint:y}
        //	ENDIF
        //ENDIF

        return Point{0, 0}

    property NameSym as symbol
    get
        if self:HyperLabel != null_object
            return self:HyperLabel:NameSym
        endif
        return null_symbol
    end get
    end property

    /// <include file="Gui.xml" path="doc/Window.Origin/*" />
    property Origin as Point
        get
            return WC.GetOrigin(self)
        end get
        set
            if self:__IsValid
                oWnd:Location := value
            else
                WC.MoveWindow(oWnd, value, true)
            endif
        end set
    end property

    /// <include file="Gui.xml" path="doc/Window.Owner/*" />
    property Owner as object
        get
            return oParent
        end get
        set
            if value is Window var  oWindow
                self:oParent := oWindow
                if self:__IsValid
                    oWnd:Owner := ((Window) oWindow):__Form
                endif
            endif
            return
        end set
    end property


    /// <include file="Gui.xml" path="doc/Window.OwnerAlignment/*" />
    property OwnerAlignment as usual
    set
        local oFormWindow  	as object
        local oWindow		as WINDOW
        oFormWindow := self:Owner
        if IsInstanceOf(oFormWindow, #Window)
            oWindow := oFormWindow
            oWindow:__AddAlign(self, value)
        endif
        return
    end set
    end property

    /// <include file="Gui.xml" path="doc/Window.PaintBackground/*" />
    method PaintBackground(hDC) as usual
        // Todo PaintBackground
        //LOCAL strRect IS _winRECT
        //LOCAL _hdc AS PTR
        //LOCAL _handle AS PTR
        //LOCAL hBrush AS PTR



        //_handle := SELF:Handle(4)
        //IF IsPtr(hDC)
        //	_hdc := hDC
        //ELSE
        //	_hdc := GetDC(_handle)
        //ENDIF

        //IF (oBackground == NULL_OBJECT)
        //	hBrush := GetClassLong(_handle, GCL_HBRBACKGROUND)
        //	IF (hBrush == NULL_PTR)
        //		hBrush := GetSysColorBrush(COLOR_WINDOW)
        //	ENDIF
        //ELSE
        //	hBrush := oBackground:Handle()
        //	oBackground:__SetBrushOrg(_hdc, _handle)
        //ENDIF

        //GetClientRect(_handle, @strRect)
        //FillRect(_hdc, @strRect, hBrush)

        //IF !IsPtr(hDC)
        //	ReleaseDC(_handle, _hdc)
        //ENDIF

        return nil


    /// <include file="Gui.xml" path="doc/Window.PaintBoundingBox/*" />
    method PaintBoundingBox(oBoundingBox as BoundingBox, kPaintMode as long) as void
        //Todo PaintBoundingBox
        //LOCAL hBrush AS PTR
        //LOCAL r IS _WinRect




        //IF oForeground == NULL_OBJECT
        //	hBrush:= GetStockObject(BLACK_BRUSH)
        //ELSE
        //	hBrush := oForeground:Handle()
        //ENDIF

        //SELF:__GetDC()

        //r:Left := oBoundingBox:Origin:X
        //r:Top := oBoundingBox:Origin:Y
        //r:Right := oBoundingBox:Extent:X
        //r:Bottom := oBoundingBox:Extent:Y

        //DO CASE
        //CASE kPaintMode == PAINTFRAME
        //	FrameRect(hdc, @r, hBrush)
        //CASE kPaintMode == PAINTINVERT
        //	InvertRect(hdc, @r)
        //OTHERWISE
        //	FillRect(hdc, @r, hBrush)
        //ENDCASE

        return


    /// <include file="Gui.xml" path="doc/Window.Pen/*" />
    property Pen as Pen
        get
            return oPen
        end get
        set
            self:oPen := value
            DCPenNeeded := true
            DCPenInUse := false
            // this forces the new object to be selected into the current DC and allows proper releasing of the old one
            self:__GetDC()
        end set
    end property


    /// <include file="Gui.xml" path="doc/Window.Pointer/*" />
    property Pointer as Pointer
        get
            if self:__Form != null_object
                oPointer := self:__Form:Cursor
            endif
            if oPointer == null_object
                oPointer := Pointer{}
            endif
            return oPointer
        end get
        set
            oPointer := value
            if self:__IsValid
                self:__Form:Cursor := oPointer
            endif
        end set
    end property

    /// <include file="Gui.xml" path="doc/Window.PostInit/*" />
    method PostInit() as usual clipper
        return self

    /// <include file="Gui.xml" path="doc/Window.PreInit/*" />
    method PreInit() as usual clipper
        return self



    /// <include file="Gui.xml" path="doc/Window.Print/*" />
    method Print(oDevice := null_object as PrintingDevice) as logic
        local lRet as logic
        // Todo  Print
        //LOCAL hDIB AS PTR
        //LOCAL cDevice AS STRING
        //LOCAL cDriver AS STRING
        //LOCAL cPort AS STRING
        //LOCAL ptrDevMode AS PTR
        //LOCAL oPrintingDev AS PrintingDevice
        //LOCAL hDCPrinter AS PTR
        //LOCAL rc IS _winRECT
        //LOCAL DocInfo IS _winDOCINFO

        //IF !IsNil(oDevice)
        //	IF !IsInstanceOfUsual(oDevice, #PrintingDevice)
        //		WCError{#Init,#Printer,__WCSTypeError,oDevice,2}:Throw()
        //	ENDIF
        //	oPrintingDev := oDevice
        //ELSE
        //	oPrintingDev := PrintingDevice{}
        //ENDIF

        //hDIB := SELF:__CreateSelfBitmap()

        //IF (hDIB != NULL_PTR)
        //	cDevice 		:= oPrintingDev:Device
        //	cDriver 		:= oPrintingDev:Driver
        //	cPort 		:= oPrintingDev:Port
        //	ptrDevMode 	:= oPrintingDev:GetDevMode()

        //	hDCPrinter := CreateDC(String2Psz(cDriver), String2Psz(cDevice), String2Psz(cPort), ptrDevMode)

        //	IF (hDCPrinter != NULL_PTR)
        //		MemSet(@DocInfo, 0, _SIZEOF(_winDOCINFO))
        //		DocInfo:cbSize := _SIZEOF(_winDOCINFO)
        //		DocInfo:lpszDocName := String2Psz( "XSharp Print Job")

        //		StartDoc(hDCPrinter, @DocInfo)
        //		StartPage(hDCPrinter)

        //		SetMapMode(hDCPrinter, MM_TEXT)
        //		__WCGetPictureCoordinates(oWnd, hDCPrinter, @rc)
        //		lRet := __WCStretchDibBlt(hDCPrinter, rc:left, rc:top, rc:right - rc:left, rc:bottom - rc:top, hDib)

        //		EndPage(hDCPrinter)
        //		EndDoc(hDCPrinter)
        //	ENDIF

        //	GlobalFree(hDIB)
        //ENDIF

        return lRet


    /// <include file="Gui.xml" path="doc/Window.QueryClose/*" />
    /// <include file="Gui.xml" path="doc/Window.QueryClose/*" />
    method QueryClose(oEvent as Event) as logic
        return true
    /// <include file="Gui.xml" path="doc/Window.RegisterTimer/*" />
    method RegisterTimer(nInterval, lOneTime)

        if !IsLong(nInterval)
            WCError{#RegisterTimer,#Window,__WCSTypeError,nInterval,1}:Throw()
        endif

        if !IsNil(lOneTime)
            if !IsLogic(lOneTime)
                WCError{#RegisterTimer,#Window,__WCSTypeError,lOneTime,2}:Throw()
            endif
            if lOneTime
                dwTimerInterval := 0
            else
                dwTimerInterval := nInterval
            endif
        else
            dwTimerInterval := nInterval
        endif

        if (nInterval > 0)
            dwTimerCount:=nInterval
            if !lTimerRegistered
                WC.RegisterTimer(self)
                lTimerRegistered := true
            endif
        else
            WC.UnregisterTimer(self)
            lTimerRegistered := false
        endif

        return self


    /// <include file="Gui.xml" path="doc/Window.RePaint/*" />
    method RePaint() as void strict
        if self:__IsValid
            self:oWnd:Invalidate(true)
        endif
        return


    /// <include file="Gui.xml" path="doc/Window.RepaintBoundingBox/*" />
    method RepaintBoundingBox(oBoundingBox as BoundingBox) as void strict
        if self:__IsValid
            self:oWnd:Invalidate( (System.Drawing.Rectangle) oBoundingBox)
        endif
        return

    /// <include file="Gui.xml" path="doc/Window.Resize/*" />
    method Resize(oResizeEvent as ResizeEvent)  as usual
        var uRet := self:@@Default(oResizeEvent)
        self:__AlignControls()

        return uRet
    /// <include file="Gui.xml" path="doc/Window.RichEditProtected/*" />
    method RichEditProtected(oRichEditProtectEvent as RichEditProtectEvent)  as usual
        return self:Default(oRichEditProtectEvent)

    /// <include file="Gui.xml" path="doc/Window.RichEditSelectionChange/*" />
    method RichEditSelectionChange(oRichEditSelectionEvent as RichEditSelectionEvent)  as usual
        return self:Default(oRichEditSelectionEvent)

    /// <include file="Gui.xml" path="doc/Window.RichEditUndoLost/*" />
    method RichEditUndoLost(oControlNotifyEvent as ControlNotifyEvent)  as usual
        return self:Default(oControlNotifyEvent)
    /// <include file="Gui.xml" path="doc/Window.Scroll/*" />
    method Scroll(oDimension, oBoundingBox, lClip) as usual
        // Todo Scroll
        //LOCAL oBB AS BoundingBox
        //LOCAL strucRectScroll IS _WinRect
        //LOCAL strucRectClip AS _WinRect
        //LOCAL oPoint AS Point

        //IF !IsInstanceOfUsual(oDimension,#Dimension)
        //	WCError{#Scroll,#Window,__WCSTypeError,oDimension,1}:Throw()
        //ENDIF
        //IF !IsNil(oBoundingBox)
        //	IF !IsInstanceOfUsual(oBoundingBox,#BoundingBox)
        //		WCError{#Scroll,#Window,__WCSTypeError,oBoundingBox,2}:Throw()
        //	ENDIF
        //	oBB:=oBoundingBox
        //ELSE
        //	oBB:=SELF:CanvasArea
        //ENDIF

        //oPoint:=__WCConvertPoint(SELF,oBB:Origin)
        //strucRectScroll:Left:=oPoint:X
        //strucRectScroll:Bottom:=oPoint:Y
        //oPoint:=__WCConvertPoint(SELF, Point{oBB:Right,oBB:Top} )
        //strucRectScroll:Right:=oPoint:X
        //strucRectScroll:Top:=oPoint:Y

        //IF !IsNil(lClip)
        //	IF !IsLogic(lClip)
        //		WCError{#Scroll,#Window,__WCSTypeError,lClip,3}:Throw()
        //	ENDIF
        //	IF lClip
        //		strucRectClip:=@strucRectScroll
        //		//strucRectClip:=Ptr(_cast,strucRectScroll)
        //	ENDIF
        //ELSE
        //	strucRectClip:=@strucRectScroll
        //	//strucRectClip:=Ptr(_cast,strucRectScroll)
        //ENDIF

        //ScrollWindow( oWnd, oDimension:Width, - oDimension:Height, @strucRectScroll, strucRectClip )

        return self


    /// <include file="Gui.xml" path="doc/Window.SetAlignStartSize/*" />
    method SetAlignStartSize(oSize as Dimension) as void

        if ALen(aAlignes) = 0 .or. ((VOAlignElement)aAlignes[1]):Control != null_object
            AAdd(aAlignes, VOAlignElement{})
            if ALen(aAlignes) > 1
                AIns(aAlignes, 1)
            endif
            aAlignes[1] := VOAlignElement{}
        endif
        if oSize == null_object
            oSize := (Dimension) oWnd:ClientRectangle
        endif
        ((VOAlignElement)aAlignes[1]):Type := oSize
        return


    /// <include file="Gui.xml" path="doc/Window.SetBackgroundBrush/*" />
    method SetBackgroundBrush(dwNew := COLOR_BTNSHADOW as dword) as usual
        // TOdo SetBackgroundBrush
        //DEFAULT( REF dwNew,COLOR_3DSHADOW)

        //SetClassLong(SELF:handle(), GCL_HBRBACKGROUND, dwNew)
        return self


    /// <include file="Gui.xml" path="doc/Window.SetExStyle/*" />
    method SetExStyle(dwSetStyle as long, lEnable:= true as logic) as long
        local iWnd as IVOForm

        if (oWnd != null_object)
            iWnd := (IVOForm) (object) oWnd
            dwExStyle := GuiWin32.GetWindowExStyle(oWnd:Handle)

            if lEnable
                dwExStyle := _or(dwExStyle, dwSetStyle)
                iWnd:Properties:ExStyle |= dwSetStyle
            else
                dwExStyle := _and(dwExStyle, _not(dwSetStyle))
                iWnd:Properties:NotExStyle |= dwSetStyle
            endif

            GuiWin32.SetWindowExStyle(oWnd:Handle, dwSetStyle)
        endif

        return dwExStyle


    /// <include file="Gui.xml" path="doc/Window.SetFocus/*" />
    method SetFocus() as void strict
        if self:__IsValid  .and. oWnd:Visible
            oWnd:Focus()
        endif
        return


    /// <include file="Gui.xml" path="doc/Window.SetHandle/*" />
    method SetHandle(hNewWnd as VOForm) as object
        oWnd := hNewWnd
        return oWnd


    /// <include file="Gui.xml" path="doc/Window.SetStyle/*" />
    method SetStyle(dwSetStyle as long, lEnable := true as logic) as long
        if lEnable
            dwStyle := _or(dwStyle, dwSetStyle)
        else
            dwStyle := _and(dwStyle, _not(dwSetStyle))
        endif
        return dwStyle

    method __GetStartPosFromShowState(kSHowState as long) as System.Windows.Forms.FormStartPosition
        local startPos as System.Windows.Forms.FormStartPosition
        if (kSHowState == SHOWZOOMED)
            oWnd:WindowState := System.Windows.Forms.FormWindowState.Maximized
        elseif (kSHowState == SHOWICONIZED)
            oWnd:WindowState :=System.Windows.Forms.FormWindowState.Minimized
            //ELSEIF kShowState == SHOWINACTIVE
            //
        elseif kSHowState == SHOWCENTERED
            if  IsInstanceOf(self, #dialogWindow)
                startPos := System.Windows.Forms.FormStartPosition.CenterScreen
            else
                startPos := System.Windows.Forms.FormStartPosition.CenterParent
            endif
        else
            if self:Origin:X != 0 .or. self:Origin:Y != 0
                startPos := System.Windows.Forms.FormStartPosition.Manual
            elseif IsInstanceOf(self, #dialogWindow)
                startPos := System.Windows.Forms.FormStartPosition.CenterScreen
            else
                startPos := System.Windows.Forms.FormStartPosition.WindowsDefaultLocation
            endif
        endif
        return startPos

    method SuspendLayout as void strict
        if self:__IsValid
            self:oWnd:SuspendLayout()
        endif
        return

    method ResumeLayout as void strict
        if self:__IsValid
            self:oWnd:ResumeLayout()
        endif
        return

    method OnMdiChildActivated(s as object, e as EventArgs) as void
        self:Activate(@@Event{})


    method Show(kShowState ) as void clipper
        default(ref kShowState, SHOWNORMAL)
        if self:__IsValid
            oWnd:SuspendLayout()
            if (null_string != cCaption)
                oWnd:Text := cCaption
            endif
            oWnd:WindowState := System.Windows.Forms.FormWindowState.Normal
            if self:__Form:isMdiChild
                local form as System.Windows.Forms.Form
                form := self:__Form:MdiParent
                self:__Form:MdiChildActivate += OnMdiChildActivated
                if form != null
                    form := form:ActiveMdiChild
                    if form != null .and. form:WindowState == System.Windows.Forms.FormWindowState.Maximized
                        form:WindowState := System.Windows.Forms.FormWindowState.Normal
                    endif
                endif
            endif
            oWnd:StartPosition := self:__GetStartPosFromShowState(kShowState)
            //oWnd:Visible := TRUE
            // check if visible
            if oWnd:Parent != null_object
                local implied oPoint := oWnd:Location
                local lChanged as logic
                if oWnd:Location:Y + oWnd:Size:Height > oWnd:Parent:Height
                    oPoint:Y := (oWnd:Parent:Height - oWnd:Height) /2
                    lChanged := true
                endif
                if oWnd:Location:X + oWnd:Size:Width > oWnd:Parent:Width .or. lChanged
                    oPoint:X := (oWnd:Parent:Width- oWnd:Width) /2
                    lChanged := true
                endif
                if oWnd:Location:X < 0 .or. oPoint:X < 0
                    oPoint:X := 0
                    lChanged := true
                endif
                if oWnd:Location:Y < 0 .or. oPoint:Y < 0
                    oPoint:Y := 0
                    lChanged := true
                endif
                if lChanged
                    oWnd:Location := oPoint
                endif
            endif
            self:lDelayAlignment := false
            foreach var element in aDelayedAlignes
                self:__AddAlign(element:Item1, element:Item2)
            next
            aDelayedAlignes:Clear()
            self:__AlignControls()
            // Counters gray areas when first showing a window
            oWnd:SuspendReDraw()
            if kShowState == SHOWCENTERED .and. oWnd:Parent == null_object
                oWnd:Center()
            endif
            oWnd:ResumeReDraw()
            oWnd:ResumeLayout()
            oWnd:Show()
            oWnd:Focus()

        endif
        return
    /// <include file="Gui.xml" path="doc/Window.ShowBalloonTrayTip/*" />

    method ShowBalloonTrayTip(oTrayIcon as VOTrayIcon,dwID := 1 as dword,sHeading := "" as string,sToolTip := "" as string,dwTimeOut := 1000 as long,dwInfo := NIIF_NONE as long) as usual
        if oTrayIcon == null_object
            self:__UpdateTrayIcon(0, oTrayIcon, dwID, sToolTip)
        endif
        if oTrayIcon != null_object
            oTrayIcon:ShowBalloonTip(dwTimeOut, sHeading, sToolTip, dwInfo)
        endif
        return nil

    /// <include file="Gui.xml" path="doc/Window.Size/*" />
    property Size as Dimension
        get
            local oSize as Dimension
            if self:__IsValid
                oSize := oWnd:Size
            else
                oSize := Dimension{}
            endif
            return oSize
        end get
        set
            if self:__IsValid
                oWnd:Size := value
            endif
        end set
    end property


    /// <include file="Gui.xml" path="doc/Window.SizeText/*" />
    method SizeText(cTextToSize as string) as Dimension
        local oDim as Dimension
        if self:oFont != null_object
            oDim := System.Windows.Forms.TextRenderer.MeasureText(cTextToSize, self:oFont)
        else
            oDim := System.Windows.Forms.TextRenderer.MeasureText(cTextToSize, System.Drawing.SystemFonts.DefaultFont)
        endif
        return oDim


    /// <include file="Gui.xml" path="doc/Window.StatusMessage/*" />
    method StatusMessage(oHL, ntype)
        return nil

    /// <include file="Gui.xml" path="doc/Window.SysLinkSelect/*" />
    method SysLinkSelect(oSysLinkSelectEvent as SysLinkSelectEvent)  as usual
        // TOdo SysLinkSelect
        //LOCAL li IS _winLITEM
        //LOCAL i AS INT
        //LOCAL oEvt := oSysLinkSelectEvent AS SysLinkSelectEvent

        //IF SLen(oEvt:URL) > 0
        //	ShellExecute(NULL_PTR, String2Psz("open"), String2Psz(oEvt:URL), NULL, NULL_PTR, SW_SHOW)

        //	li:mask := _OR(LIF_ITEMINDEX, LIF_STATE)
        //	li:iLink := oEvt:LinkIndex
        //	li:stateMask := LIS_VISITED
        //	li:state := LIS_VISITED
        //	i:=SendMessage(oEvt:Control:Handle(), LM_SETITEM, 0, LONGINT(_CAST, @li))
        //ENDIF
        return self:Default(oSysLinkSelectEvent)

    /// <include file="Gui.xml" path="doc/Window.TabKeyDown/*" />
    method TabKeyDown(oControlNotifyEvent as ControlNotifyEvent)  as usual
        return self:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.TabSelect/*" />
    method TabSelect(oControlNotifyEvent as ControlNotifyEvent)  as usual
        return self:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.TabSelectionChanging/*" />
    method TabSelectionChanging(oControlNotifyEvent as ControlNotifyEvent)  as usual
        return self:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.TextColor/*" />
    property TextColor as Pen get oPen set self:Pen:= value


    /// <include file="Gui.xml" path="doc/Window.TextPrint/*" />
    method TextPrint(cText as string, oPoint as Point) as void
        // Todo TextPrint
        //LOCAL strucLogBrush IS _WinLogBrush
        //LOCAL iOldMode, iNewMode AS INT
        //LOCAL dwOldBack AS DWORD
        //LOCAL lUsingBrush AS LOGIC



        //DCFontNeeded := TRUE
        //DCPenNeeded := TRUE

        //IF (SELF:__GetDC() != NULL_PTR)
        //	iNewMode := TRANSPARENT

        //	IF oForeground != NULL_OBJECT
        //		__WCLogicalBrush(oForeground, @strucLogBrush)
        //		IF strucLogBrush:lbStyle != BS_HOLLOW
        //			dwOldBack := SetBkColor(hDC, strucLogBrush:lbColor)
        //			iNewMode := OPAQUE
        //			lUsingBrush := TRUE
        //		ENDIF
        //	ENDIF
        //	iOldMode := SetBkMode(hDC, PTR(_CAST, iNewMode))
        //	TextOut(hDC, oPoint:x, oPoint:y, String2Psz(cText), INT(_CAST, SLen(cText)))
        //	SetBkMode(hDC, PTR(_CAST, iOldMode))
        //	IF lUsingBrush
        //		SetBkColor(hDC, dwOldBack)
        //	ENDIF
        //ENDIF

        return


    /// <include file="Gui.xml" path="doc/Window.Timer/*" />
    method Timer() as usual clipper
        return self


    /// <include file="Gui.xml" path="doc/Window.ToolBar/*" />
    property ToolBar as ToolBar
        get
            return oToolBar
        end get
        set

            if value != null_object
                if (self:Menu != null_object) .and. (self:Menu:ToolBar != value)
                    self:Menu:ToolBar := null_object
                endif

                if (oToolBar != null_object) .and. (oToolBar != value)
                    oToolBar:Destroy()
                endif

                oToolBar := value

                if oToolBar != null_object
                    oToolBar:__SetParent(self)
                    oToolBar:Show()
                endif
            endif
        end set
    end property



    /// <include file="Gui.xml" path="doc/Window.ToolBarHeightChanged/*" />
    method ToolBarHeightChanged(oControlNotifyEvent as ControlNotifyEvent)  as usual
        return self:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.ToTop/*" />
    method ToTop() as void
        if self:__IsValid
            oWnd:BringToFront()
        endif
        return

    /// <include file="Gui.xml" path="doc/Window.TrayIconBalloonClicked/*" />
    method TrayIconBalloonClicked(dwID as dword) as usual
        return self

    /// <include file="Gui.xml" path="doc/Window.TrayIconBalloonShown/*" />
    method TrayIconBalloonShown(dwID as dword) as usual
        return self



    /// <include file="Gui.xml" path="doc/Window.TrayIconBalloonTimeOut/*" />
    method TrayIconBalloonTimeOut(dwID as dword) as usual
        return self

    /// <include file="Gui.xml" path="doc/Window.TrayIconClicked/*" />
    method TrayIconClicked(dwID as dword, lRightButton as logic, lDoubleClick as logic)  as usual
        return self
    /// <include file="Gui.xml" path="doc/Window.TreeViewItemDelete/*" />
    method TreeViewItemDelete(oTreeViewDeleteEvent as TreeViewDeleteEvent)  as usual
        return self:Default(oTreeViewDeleteEvent)
    /// <include file="Gui.xml" path="doc/Window.TreeViewItemDrag/*" />
    method TreeViewItemDrag(oTreeViewDragEvent as TreeViewDragEvent) as usual
        //LOCAL oControl AS TreeView
        //LOCAL oPoint AS Point
        //LOCAL oEvt := oTreeViewDragEvent AS TreeViewDragEvent



        //oControl := OBJECT(oEvt:Control)

        //IF oControl:DragDropEnabled
        //	IF oControl:DragImageList == NULL_OBJECT
        //		oDragImageList := oControl:__CreateDragImageList(oEvt:TreeViewItem:NameSym)
        //		oDragImageList:BeginDrag(1)
        //	ELSE
        //		oDragImageList := oControl:DragImageList
        //		oDragImageList:BeginDrag(oEvt:TreeViewItem:ImageIndex)
        //	ENDIF
        //	oPoint := oEvt:Position
        //	oPoint:X += oControl:Origin:X
        //	oPoint:Y += oControl:Origin:Y
        //	//PP-030505 Bug:126
        //	oDragImageList:DragEnter(oPoint, SELF)
        //	lDragActive := TRUE
        //	ShowCursor(FALSE)
        //	SetCapture(SELF:Handle())
        return self:Default(oTreeViewDragEvent)
    /// <include file="Gui.xml" path="doc/Window.TreeViewItemEdit/*" />
    method TreeViewItemEdit(oTreeViewEditEvent as TreeViewEditEvent) as usual
        return self:Default(oTreeViewEditEvent)


    /// <include file="Gui.xml" path="doc/Window.TreeViewItemExpanded/*" />
    method TreeViewItemExpanded(oTreeViewExpandedEvent as TreeViewExpandedEvent) as usual
        return self:Default(oTreeViewExpandedEvent)
    /// <include file="Gui.xml" path="doc/Window.TreeViewItemExpanding/*" />
    method TreeViewItemExpanding(oTreeViewExpandingEvent as TreeViewExpandingEvent) as usual
        return self:Default(oTreeViewExpandingEvent)

    /// <include file="Gui.xml" path="doc/Window.TreeViewKeyDown/*" />
    method TreeViewKeyDown(oTreeViewKeyEvent as TreeViewKeyEvent) as usual
        return self:Default(oTreeViewKeyEvent)
    /// <include file="Gui.xml" path="doc/Window.TreeViewMouseButtonDoubleClick/*" />
    method TreeViewMouseButtonDoubleClick(oTreeViewMouseEvent as TreeViewMouseEvent) as usual
        return self:Default(oTreeViewMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.TreeViewMouseButtonDown/*" />
    method TreeViewMouseButtonDown(oTreeViewMouseEvent as TreeViewMouseEvent) as usual
        return self:Default(oTreeViewMouseEvent)
        //ENDIF


    /// <include file="Gui.xml" path="doc/Window.TreeViewSelectionChanged/*" />
    method TreeViewSelectionChanged(oTreeViewSelectionEvent as TreeViewSelectionEvent) as usual
        return self:Default(oTreeViewSelectionEvent)


    /// <include file="Gui.xml" path="doc/Window.TreeViewSelectionChanging/*" />
    method TreeViewSelectionChanging(oTreeViewSelectionEvent as TreeViewSelectionEvent) as usual
        return self:Default(oTreeViewSelectionEvent)
    /// <include file="Gui.xml" path="doc/Window.Update/*" />
    method Update()  as void strict
        if self:__IsValid
            self:oWnd:Update()
        endif

        return

    /// <include file="Gui.xml" path="doc/Window.VerticalScroll/*" />
    method VerticalScroll(oScrollEvent as ScrollEvent) as usual
        local oScrollBar as ScrollBar

        oScrollBar := oScrollEvent:ScrollBar
        if (oScrollBar != null_object)
            oScrollBar:ThumbPosition:=oScrollEvent:Position
        endif

        return self:Default(oScrollEvent)

    /// <include file="Gui.xml" path="doc/Window.VerticalSlide/*" />
    method VerticalSlide(oSliderEvent as SliderEvent) as usual
        local oSlider as Slider
        oSlider := oSliderEvent:Slider
        if (oSlider != null_object)
            oSlider:ThumbPosition := oSliderEvent:Position
        endif
        return self:Default(oSliderEvent)

    /// <include file="Gui.xml" path="doc/Window.VerticalSpin/*" />
    method VerticalSpin(oSpinnerEvent as SpinnerEvent) as usual
        local oSpinner as Spinner

        oSpinner := oSpinnerEvent:Spinner
        if (oSpinner != null_object)
            oSpinner:Position:=oSpinnerEvent:Position
        endif

        return self:Default(oSpinnerEvent)

    /// <include file="Gui.xml" path="doc/Window.WindowArea/*" />
    property WindowArea as BoundingBox get (BoundingBox) __Form:Bounds

end class


/// <exclude/>

class __ForeignWindow inherit Window

    constructor(oWndSelf)
        super()
        oWnd := oWndSelf
        return

end class



#region defines
define OA_LEFT          := 1
define OA_LEFT_AUTOSIZE     := 5
define OA_NO            := 0
define OA_RIGHT         := 3
define OA_RIGHT_AUTOSIZE    := 7
define OA_TOP           := 2
define OA_TOP_AUTOSIZE      := 6
define OA_BOTTOM        := 4
define OA_BOTTOM_AUTOSIZE   := 8
define OA_CENTER        := 9
define OA_FULL_SIZE         := 10
define OA_Height                := 0b0000000110000000
define OA_PHeight               := 0b0000001110000000
define OA_Width                 := 0b0000010010000000
define OA_PWidth                := 0b0000110010000000
define OA_WIDTH_HEIGHT          := OA_Width | OA_HEIGHT
define OA_WIDTH_PHEIGHT         := OA_Width | OA_PHEIGHT
define OA_PWIDTH_HEIGHT         := OA_PWidth | OA_HEIGHT
define OA_PWIDTH_PHEIGHT        := OA_PWidth | OA_PHEIGHT
define OA_HEIGHT_WIDTH          := OA_WIDTH_HEIGHT
define OA_HEIGHT_PWIDTH         := OA_PWIDTH_HEIGHT
define OA_PHEIGHT_WIDTH         := OA_WIDTH_PHEIGHT
define OA_PHEIGHT_PWIDTH        := OA_PWIDTH_PHEIGHT
define OA_X                     := 0b0100000010000000
define OA_PX                    := 0b1100000010000000
define OA_Y                     := 0b0001000010000000
define OA_PY                    := 0b0011000010000000
define OA_PX_HEIGHT             := OA_PX | OA_Height
define OA_PX_PHEIGHT            := OA_PX | OA_PHeight
define OA_PX_WIDTH              := OA_PX | OA_Width
define OA_PX_PWIDTH             := OA_PX | OA_PWidth
define OA_PX_PWIDTH_HEIGHT      := OA_PX | OA_PWidth | OA_HEIGHT
define OA_PX_WIDTH_HEIGHT       := OA_PX | OA_Width | OA_HEIGHT
define OA_PX_PWIDTH_PHEIGHT     := OA_PX | OA_PWidth | OA_PHEIGHT
define OA_PY_HEIGHT             := OA_PY | OA_Height
define OA_PY_PHEIGHT            := OA_PY | OA_PHeight
define OA_PY_WIDTH          	:= OA_PY | OA_Width
define OA_PY_PWIDTH             := OA_PY | OA_PWidth
define OA_PY_WIDTH_HEIGHT       := OA_PY | OA_Width  | OA_HEIGHT
define OA_PY_WIDTH_PHEIGHT      := OA_PY | OA_Width  | OA_PHEIGHT
define OA_PY_PWIDTH_HEIGHT      := OA_PY | OA_PWidth | OA_HEIGHT
define OA_PY_PWIDTH_PHEIGHT     := OA_PY | OA_PWidth | OA_PHEIGHT
define OA_PX_PY         		:= OA_PX | OA_PY
define OA_PX_PY_HEIGHT          := OA_PX | OA_PY | OA_Height
define OA_PX_PY_PHEIGHT         := OA_PX | OA_PY | OA_PHeight
define OA_PX_PY_PWIDTH          := OA_PX | OA_PY | OA_PWidth
define OA_PX_PY_PWIDTH_PHEIGHT  := OA_PX | OA_PY | OA_PWidth | OA_PHEIGHT
define OA_PX_PY_WIDTH           := OA_PX | OA_PY | OA_Width
define OA_PX_PY_WIDTH_HEIGHT    := OA_PX | OA_PY | OA_WIDTH | OA_HEIGHT
define OA_PX_Y                  := OA_PX | OA_Y
define OA_PX_Y_PWIDTH           := OA_PX | OA_Y | OA_PWidth
define OA_PX_Y_PHEIGHT          := OA_PX | OA_Y | OA_PHeight
define OA_X_Y                   := OA_X | OA_Y
define OA_X_HEIGHT              := OA_X | OA_Height
define OA_X_PHEIGHT             := OA_X | OA_PHeight
define OA_X_WIDTH               := OA_X | OA_Width
define OA_X_PWIDTH              := OA_X | OA_PWidth
define OA_X_PY          		:= OA_X | OA_PY
define OA_X_PY_PHEIGHT          := OA_X | OA_PY | OA_PHeight
define OA_Y_PWIDTH              := OA_Y | OA_PWidth
define OA_Y_WIDTH               := OA_Y | OA_Width
define TRAY_ICON_MSG := WM_APP + 1
define TRAYTIP_LENGTH_SHELLORIGINAL := 64
define TRAYTIP_LENGTH_SHELL5 := 128
#endregion


