


using System.Collections.Generic
USING VOSDK := XSharp.VO.SDK


/// <include file="Gui.xml" path="doc/Window/*" />
PARTIAL CLASS Window INHERIT @@EventContext IMPLEMENTS IGuiObject, IControlParent, ITimer
	PROTECT oParent AS OBJECT
	PROTECT oWnd AS VOForm
	PROTECT dwStyle AS LONG
	PROTECT dwExStyle AS LONG

	PROTECT cCaption AS STRING
	PROTECT oMenu AS Menu
	PROTECT oIcon AS Icon
	PROTECT oIconSmall AS Icon
	PROTECT oContextMenu AS Menu
	PROTECT oAccelerator AS Accelerator
	PROTECT oFont AS Font
	PROTECT oPointer AS Pointer
	PROTECT oBackground AS Brush
	PROTECT oForeground AS Brush
	PROTECT oHyperLabel AS HyperLabel
	PROTECT lRetVal AS LOGIC
	PROTECT oOrigin AS Point
	PROTECT oPen AS Pen
	PROTECT oCursor AS VOSDK.Cursor
	PROTECT oDragDropClient AS DragDropClient
	//PROTECT oDragDropServer AS DragDropServer
	PROTECT oToolBar AS ToolBar

	PROTECT DCInitialized AS LOGIC
	PROTECT DCPenInUse AS LOGIC
	PROTECT DCPenNeeded AS LOGIC
	PROTECT DCBrushInUse AS LOGIC
	PROTECT DCBrushNeeded AS LOGIC
	PROTECT DCFontInUse AS LOGIC
	PROTECT DCFontNeeded AS LOGIC
	PROTECT hDC AS IntPtr
	PROTECT hDCPaint AS IntPtr

	PROTECT oCurrentHelp AS HelpDisplay
	PROTECT lHelpOn AS LOGIC
	PROTECT lHelpCursorOn AS LOGIC
	PROTECT lHelpMenu AS LOGIC
	//PROTECT lFileDragging AS LOGIC
	PROTECT dwTimerCount AS INT
	PROTECT dwTimerInterval AS INT
	PROTECT lTimerRegistered AS LOGIC

	//PROTECT lDragActive AS LOGIC
	//PROTECT oDragImageList AS VOSDK.ImageList
	PROTECT hDragSingleCursor AS IntPtr
	PROTECT hDragMultipCursor AS IntPtr
	PROTECT aAlignes			AS ARRAY
	PROTECT aDelayedAlignes		AS ARRAY
	PROTECT lDelayAlignment		as LOGIC
	PROTECT lAutomated			AS LOGIC

	PROTECT oMinSize AS Dimension
	PROTECT oTrayIcon AS VOTrayIcon


	PROPERTY EventReturnValue AS LONGINT AUTO

	PROTECT oResourceDialog AS ResourceDialog	// Class that holds the decoded version of the Win32 resource
	PROPERTY ResourceDialog AS ResourceDialog GET oResourceDialog
	PROPERTY IsClosing AS LOGIC AUTO

    PROPERTY __Handle as IntPtr GET SELF:Handle()

	METHOD __CreateForm() AS VOForm STRICT
		RETURN GuiFactory.Instance:CreateWindow(SELF)

	ACCESS __Form AS VOForm STRICT
		RETURN oWnd

	ASSIGN TopMost(value AS LOGIC)
		IF value
			SELF:__Form:mdiParent := NULL
		ENDIF
		SELF:__Form:TopMost := Value
	ACCESS TopMost AS LOGIC
		RETURN SELF:__Form:TopMost
	ACCESS __IsValid AS LOGIC STRICT
		RETURN SELF:oWnd != NULL_OBJECT .and. ! SELF:oWnd:IsDisposed

	ACCESS __HasSurface AS LOGIC
		RETURN SELF:__Surface != oWnd

	ACCESS __Surface AS IVOControlContainer STRICT
		IF SELF:__IsValid
			LOCAL IMPLIED aList := oWnd:GetAllControls()
			FOREACH oC AS OBJECT IN aList
				IF oc IS IVOPanel
					RETURN oC
				ENDIF
			NEXT
		ENDIF
		RETURN oWnd

	METHOD __SetupDataControl(oDC AS VOSDK.Control) AS VOID
		RETURN
 /// <exclude />
	METHOD __AddAlign(oControl AS IGUIObject, iType AS USUAL) AS LOGIC STRICT
		LOCAL dwI, dwCount AS DWORD
		LOCAL lDelete      AS LOGIC
		LOCAL lOldAlign    AS LOGIC
		LOCAL lFound	   AS LOGIC
		IF IsNil(iType)
			iType := OA_TOP
		ENDIF
		IF lDelayAlignment
			aadd(aDelayedAlignes, {(OBJECT)oControl, iType})
			RETURN TRUE
		ENDIF

		lOldAlign := ! IsPtr(iType) .AND. (INT) iType <= OA_FULL_SIZE

		//PP-031129 Additional owner alignment options
		IF ! lOldAlign
			//Null_Object calls SetAlignStartSize() in init mode,
			//means that only the first call sets the AlignStartSize.
			SELF:SetAlignStartSize(NULL_OBJECT)
		ENDIF

		lDelete := (IsLong(iType) .AND. (INT) iType = OA_NO)

		//SE-060525
		dwCount := ALen(aAlignes)
		lFound  := FALSE
		FOR dwI := 1 UPTO dwCount
			IF ((OBJECT)aAlignes[dwI][ 1]) == oControl
				lFound  := TRUE
				IF lDelete
					ADel(aAlignes, dwI)
					aSize(aAlignes, Alen(aAlignes)-1)
				ELSE
					aAlignes[dwI][ 2] := iType
				ENDIF
				dwI := 0
				EXIT //SE-070919 bug fix
			ENDIF
		NEXT


		IF !lFound
			// Add Control to the array
			LOCAL oRect AS System.Drawing.Rectangle
			IF IsInstanceOf(oControl, #Window)
				LOCAL oForm AS System.Windows.Forms.Form
				oForm 			:= ((VOSDK.Window) oControl):__Form
				oRect 			:= oForm:ClientRectangle
				oRect:Location 	:= oControl:Origin
				oRect:Size     	:= oForm:Size
			ELSE
				LOCAL oCtrl AS IVOControl
				oCtrl			:= ((VOSDK.Control) oControl):__Control
				oRect			:= oCtrl:DisplayRectangle
				oRect:Location  := oCtrl:Location
			ENDIF
			AAdd(aAlignes, {(OBJECT)oControl, iType, oRect:left, oRect:top, oRect:Width, oRect:Height})
		ENDIF

		//IF lOldAlign
		//	SELF:__AlignControls()
		//ENDIF

		RETURN TRUE

 /// <exclude />
	METHOD __AddTool(oControl AS VOSDK.Control) AS LOGIC STRICT
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

		RETURN TRUE

	METHOD __ShowToolTip(oControl AS VOSDK.Control) AS VOID STRICT
		LOCAL cMessage AS STRING
		cMessage := oControl:ToolTipText
		IF STRING.IsNullOrEmpty(cMessage) .and. oControl:UseHLForToolTip
			IF oControl:HyperLabel != NULL_OBJECT
				cMessage := oControl:HyperLabel:Description
			ENDIF
		ENDIF
		IF SELF:__Surface IS VOPanel VAR oPanel
			oPanel:ShowToolTip((System.Windows.Forms.Control) oControl:__Control, cMessage)
		ENDIF

	METHOD __AlignControl(	oCtl AS IVOUIObject, nX AS LONG, nY AS LONG, nW AS LONG, nH AS LONG) AS VOID
		oCtl:Location := System.Drawing.Point{nX, nY}
		oCtl:Size     := System.Drawing.Size{nW, nH}
		RETURN


 /// <exclude />
	METHOD __AlignControls() AS Window STRICT

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
		LOCAL oRect		 AS System.Drawing.Rectangle
		LOCAL oCtl       AS IVOUIObject
		LOCAL uType      AS USUAL
		LOCAL dwType     AS DWORD
		LOCAL liMulDiv   AS LONG
		LOCAL oOwnerSize AS Dimension
		// Original Sizes
		LOCAL iWinRefWidth		AS INT
		LOCAL iWinRefHeight		AS INT
		LOCAL iCtlRefWidth      AS INT
		LOCAL iCtlRefHeight     AS INT
		// New Window Size
		LOCAL iWinWidth			AS INT
		LOCAL iWinHeight		AS INT
		// New Control Size & Position
		LOCAL iCtlX				AS INT
		LOCAL iCtlY				AS INT
		LOCAL iCtlWidth			AS INT
		LOCAL iCtlHeight		AS INT

		LOCAL pB                    AS BYTE PTR
		LOCAL oOwnerOffSet			AS Point

		LOCAL dwI, dwCount  AS DWORD
		LOCAL oResize    AS OBJECT

		IF ! lDelayAlignment
			RETURN SELF
		ENDIF


		dwCount := ALen(aAlignes)

		IF dwCount < 1 .OR. oWnd == NULL_OBJECT .or. oWnd:WindowState == system.Windows.Forms.FormWindowState.Minimized
			RETURN  SELF
		ENDIF

		oRect   := SELF:__Surface:ClientRectangle
		iWinWidth  := oRect:Width
		iWinHeight := oRect:Height

		IF aAlignes[1][1] == NULL_OBJECT
			oOwnerSize	:= aAlignes[1][2]
			IF oOwnerSize != NULL_OBJECT
				iWinRefWidth  := oOwnerSize:Width
				iWinRefHeight := oOwnerSize:Height
			ELSE
				iWinRefWidth  := iWinWidth
				iWinRefHeight := iWinHeight
			ENDIF
			dwI := 2
		ELSE
			dwI := 1
		ENDIF

		IF iWinRefWidth == iWinWidth .and. iWinRefHeight == iWinHeight
			RETURN SELF
		ENDIF

		oWnd:SuspendRedraw()
		// First element in aAlignes has the window and its original size
		// 1 = Window
		// 2 = Size (Dimension object)
		// Each other Element in aAlignes has the following values
		// 1   = Control/ Window
		// 2   = Type, Pointer or LONG
		// 3,4 = Original X,Y
		// 5,6 = Original Width/Height
		DO WHILE dwI <= dwCount
			oResize := aAlignes[dwI][1]
			IF IsInstanceOf(oResize, #DataWindow)
				oCtl	:= ((DataWindow) oResize):__Frame
				oOwnerOffSet := Point{0,0}
			ELSEIF IsInstanceOf(oResize, #Window)
				oCtl	:= ((Window) oResize):__Form
				oOwnerOffSet := Point{0,0}
			ELSE
				oCtl	:= ((Control) oResize):__Control
				oOwnerOffSet := ((Control) oResize):__OwnerOffSet
			ENDIF
			uType   := aAlignes[dwI][2]
			dwType  := DWORD(_CAST, uType)
			IF !oCtl:IsDisposed
				IF IsPtr(uType) .OR. dwType > OA_FULL_SIZE // 10
					iCtlX      := aAlignes[dwI][3]
					iCtlY      := aAlignes[dwI][4]
					iCtlWidth  := iCtlRefWidth  := aAlignes[dwI][5]
					iCtlHeight := iCtlRefHeight := aAlignes[dwI][6]

					IF IsPtr(uType)
						pB := (BYTE PTR) @dwType
						IF (liMulDiv := pB[1]) > 0
							iCtlX := GuiWin32.MulDiv(iWinWidth , liMulDiv>>4, (LONG) _AND(liMulDiv, 0XF))
						ENDIF
						IF (liMulDiv := pB[2]) > 0
							iCtlY := GuiWin32.MulDiv(iWinHeight , liMulDiv>>4, (LONG)  _AND(liMulDiv, 0XF))
						ENDIF
						IF (liMulDiv := pB[3]) > 0
							iCtlWidth := GuiWin32.MulDiv(iWinWidth , liMulDiv>>4,  (LONG) _AND(liMulDiv, 0XF))
						ENDIF
						IF (liMulDiv := pB[4]) > 0
							iCtlHeight := GuiWin32.MulDiv(iWinHeight , liMulDiv>>4,  (LONG) _AND(liMulDiv, 0XF))
						ENDIF
					ELSE
						// Width
						IF _AND(dwType, OA_PWIDTH) = OA_PWIDTH
							iCtlWidth := GuiWin32.MulDiv(iWinWidth, iCtlRefWidth, iWinRefWidth)
						ELSEIF _AND(dwType, OA_WIDTH) = OA_WIDTH
							iCtlWidth += iWinWidth - iWinRefWidth
						ENDIF
						// Height
						IF _AND(dwType, OA_PHEIGHT) = OA_PHEIGHT
							iCtlHeight := GuiWin32.MulDiv(iWinHeight, iCtlRefHeight, iWinRefHeight)
						ELSEIF _AND(dwType, OA_HEIGHT) = OA_HEIGHT
							iCtlHeight += iWinHeight - iWinRefHeight
						ENDIF
						// X-Position
						IF _AND(dwType, OA_PX) = OA_PX
							IF _AND(dwType, OA_PWIDTH) = OA_PWIDTH
								iCtlX := GuiWin32.MulDiv(iWinWidth, iCtlX, iWinRefWidth)
							ELSE
								iCtlX := GuiWin32.MulDiv(iWinWidth, iCtlX + iCtlRefWidth/2, iWinRefWidth) - iCtlWidth/2
							ENDIF
						ELSEIF _AND(dwType, OA_X) = OA_X
							iCtlX += iWinWidth - iWinRefWidth
						ENDIF
						// Y- position
						IF _AND(dwType, OA_PY) = OA_PY
							IF _AND(dwType, OA_PHEIGHT) = OA_PHEIGHT
								iCtlY := GuiWin32.MulDiv(iWinHeight, iCtlY, iWinRefHeight)
							ELSE
								iCtlY := GuiWin32.MulDiv(iWinHeight, iCtlY + iCtlRefHeight/2, iWinRefHeight) - iCtlHeight/2
							ENDIF
						ELSEIF _AND(dwType, OA_Y) = OA_Y
							iCtlY += iWinHeight - iWinRefHeight
						ENDIF

					ENDIF
					// Since we are nesting controls in our GUI Classes we need to subtract the Offset of the owner
					iCtlX -= oOwnerOffSet:X
					iCtlY -= oOwnerOffSet:Y
					SELF:__AlignControl(oCtl,iCtlX, iCtlY, iCtlWidth, iCtlHeight)
				ELSE
					#region Simple Alignment
					oRect		:= oCtl:ClientRectangle
					iCtlWidth	:= oRect:Width
					iCtlHeight	:= oRect:Height
					iCtlX		:= oRect:Left
					iCtlY		:= oRect:Top
					SWITCH dwType
					CASE OA_TOP
						iCtlY      := 0
					CASE OA_LEFT
						iCtlX      := 0
					CASE OA_BOTTOM
						iCtlY      := iWinHeight-iCtlHeight
					CASE OA_RIGHT
						iCtlX      := iWinWidth-iCtlWidth
					CASE OA_TOP_AUTOSIZE
						iCtlX      := 0
						iCtlY      := 0
						iCtlWidth  := iWinWidth
					CASE OA_LEFT_AUTOSIZE
						iCtlX      := 0
						iCtlY      := 0
						iCtlHeight := iWinHeight
					CASE OA_BOTTOM_AUTOSIZE
						iCtlX      := 0
						iCtlY      := iWinHeight-iCtlHeight
						iCtlWidth  := iWinWidth
					CASE OA_RIGHT_AUTOSIZE
						iCtlX      := iWinWidth-iCtlWidth
						iCtlY      := 0
						iCtlHeight := iWinHeight
					CASE OA_CENTER
						iCtlX      := (iWinWidth / 2)- (iCtlWidth / 2)
						iCtlY      := (iWinHeight / 2)- (iCtlHeight / 2)

					CASE OA_FULL_SIZE
						iCtlX      := 0
						iCtlY      := 0
						iCtlWidth  := iWinWidth
						iCtlHeight := iWinHeight
					END SWITCH
					// Since we are nesting controls in our GUI Classes we need to subtract the Offset of the owner
					iCtlX -= oOwnerOffSet:X
					iCtlY -= oOwnerOffSet:Y
					SELF:__AlignControl(oCtl,iCtlX, iCtlY, iCtlWidth, iCtlHeight)
					#endregion
				ENDIF
			ENDIF
			++dwI
		ENDDO
		oWnd:ResumeRedraw()
		RETURN SELF


 /// <exclude />
	METHOD __AssociateAccel(lSwitch AS LOGIC) AS Window STRICT
		//IF lSwitch .AND. (oAccelerator != NULL_OBJECT)
		//	SetAccelerator(oWnd, oAccelerator:Handle())
		//ELSE //if !lswitch
		//	SetAccelerator(NULL_PTR, NULL_PTR)
		//ENDIF

		RETURN SELF


 /// <exclude />
	METHOD __Close(oEvent AS @@Event) AS VOID STRICT
		SELF:IsClosing := TRUE
		SELF:Close(oEvent)
		SELF:Destroy()
		RETURN

 /// <exclude />
	METHOD __CommandFromEvent(oEvent AS INamedEvent) AS LOGIC STRICT
		LOCAL symNameSym AS SYMBOL
		LOCAL oWindow AS OBJECT
		LOCAL oReport AS OBJECT
		LOCAL o AS OBJECT

		symNameSym := oEvent:NameSym
		oWindow := SELF
		IF oWindow IS ShellWindow
			oWindow := ((ShellWindow) oWindow):GetActiveChild()
			IF oWindow == NULL_OBJECT
				oWindow := SELF
			ENDIF
		ENDIF


		DO WHILE TRUE
			IF IsMethod(oWindow, symNameSym)
				Send(oWindow, symNameSym)
				RETURN TRUE
			ENDIF
			IF IsAccess(oWindow, #Owner) .AND. IsInstanceOfUsual(IVarGet(oWindow,#Owner), #Window)
				oWindow := IVarGet(oWindow,#Owner)
			ELSE
				EXIT
			ENDIF
		ENDDO

		IF IsClassOf(symNameSym, #Window)
			IF IsInstanceOf(SELF, #ChildAppWindow)
				oWindow:=SELF
				DO WHILE IsInstanceOf(oWindow:Owner, #Window)
					oWindow:=IVarGet(oWindow,#Owner)
				ENDDO
				o := CreateInstance(symNameSym, oWindow)
				o:show()
				// (CreateInstance(symNameSym, oWindow)):Show()
			ELSE
				o := CreateInstance(symNameSym, SELF)
				o:Show()
			ENDIF
			RETURN TRUE
		ELSEIF IsClassOf(symNameSym, #ReportQueue)
			oReport := CreateInstance(symNameSym, SELF)
			IF (oReport != NULL_OBJECT)
				Send(oReport,#Show)
			ENDIF
			RETURN TRUE
		ENDIF

		RETURN FALSE


 /// <exclude />
	METHOD __CreateSelfBitmap() AS IntPtr STRICT
		// Todo __CreateSelfBitmap
		LOCAL hDIB AS PTR
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

		RETURN hDIB


 /// <exclude />
	ACCESS __Cursor AS VOSDK.Cursor STRICT
		RETURN oCursor

 /// <exclude />
	ASSIGN __Cursor(oNewCursor AS VOSDK.Cursor)  STRICT
		oCursor:=oNewCursor

	[Obsolete];
	METHOD __DestroyChildren() AS VOID STRICT
		RETURN

	[Obsolete];
	METHOD __DestroyWnd() AS LOGIC STRICT
		RETURN TRUE

 /// <exclude />
	METHOD __EnableHelpCursor(lEnabled AS LOGIC) AS Window STRICT
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
		RETURN SELF


 /// <exclude />
	METHOD __FilterHelpCursor(wArea AS LONGINT) AS LOGIC STRICT
		LOCAL lTemp AS LOGIC

		DO CASE
		CASE wArea==HTCAPTION
			lTemp := TRUE
		CASE wArea==HTCLIENT
			lTemp := TRUE
		CASE wArea==HTREDUCE
			lTemp := TRUE
		CASE wArea==HTZOOM
			lTemp := TRUE
		CASE wArea==HTSYSMENU
			lTemp := TRUE
		CASE wArea==HTBOTTOM .OR. ;
			wArea==HTBOTTOMLEFT .OR. ;
			wArea==HTBOTTOMRIGHT .OR. ;
			wArea==HTTOP .OR. ;
			wArea==HTLEFT .OR. ;
			wArea==HTRIGHT .OR. ;
			wArea==HTTOPLEFT .OR. ;
			wArea==HTTOPRIGHT
			lTemp := TRUE
		CASE wArea==HTNowhere
			lTemp := TRUE
		OTHERWISE
			lTemp := FALSE
		ENDCASE

		RETURN lTemp



 /// <exclude />
	METHOD __GetDC() AS IntPtr STRICT
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

		RETURN hDC


 /// <exclude />
	METHOD __GetMyOleObjects AS ARRAY STRICT
		LOCAL aMyControls AS ARRAY
		LOCAL aObjects		AS ARRAY
		LOCAL iLen			AS DWORD
		LOCAL i				AS DWORD
		aMyControls := SELF:GetAllChildren()
		aObjects    := {}
		iLen := ALen(aMyControls)
		FOR i:=1 TO iLen
			IF IsInstanceOf(aMyControls[i], #OleObject)
				AAdd(aObjects, aMyControls[i])
			ENDIF
		NEXT
		RETURN aObjects



	//METHOD __GetPaintRect() AS _WINRECT STRICT
	//	//PP-030828 Strong typing


	//	RETURN strucPaintRect



 /// <exclude />
	METHOD __HandleListItemDrag(oEvent AS @@Event) AS Window STRICT
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
		RETURN SELF


 /// <exclude />
	METHOD __HandlePointer(oEvent AS @@Event, lHelp AS LOGIC, lClient AS LOGIC) AS Window STRICT
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
		RETURN SELF


 /// <exclude />
	METHOD __HelpFilter(oEvent AS @@Event) AS LOGIC STRICT
		LOCAL wParam AS DWORD



		wParam := LoWord(oEvent:wParam)
		IF (wParam >= ID_FIRSTWCHELPID)
			DO CASE
			CASE (wParam == ID_WCHELP)
				SELF:__ProcessHelpCursor(SELF, HTNowhere)
			CASE (wParam == ID_WCHELPON)
				SELF:__EnableHelpCursor(TRUE)
			CASE (wParam == ID_WCHELPOFF)
				SELF:__EnableHelpCursor(FALSE)
			ENDCASE
			RETURN TRUE
		ENDIF

		RETURN FALSE


 /// <exclude />
	METHOD __InCursorHelpMode() AS LOGIC STRICT
		//PP-030828 Strong typing
		RETURN lHelpOn .AND. lHelpCursorOn


 /// <exclude />
	ACCESS __Parent AS OBJECT STRICT
		//PP-030828 Strong typing
		RETURN oParent


 /// <exclude />
	METHOD __PreMenuCommand(oMenuCommandEvent AS MenuCommandEvent) AS USUAL STRICT

		IF SELF:PreMenuCommand( oMenuCommandEvent)
			RETURN SELF
		ENDIF

		IF ! SELF:__CommandFromEvent(oMenuCommandEvent)
			RETURN SELF:MenuCommand(oMenuCommandEvent)
		ENDIF
		RETURN SELF


	[Obsolete];
	METHOD __PrePreMenuCommand()

		RETURN SELF


 /// <exclude />
	METHOD __ProcessHelp(oEvent AS @@Event) AS LOGIC STRICT
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

		RETURN FALSE


 /// <exclude />
	METHOD __ProcessHelpCursor(oWin AS OBJECT, wArea AS LONGINT) AS LOGIC STRICT
		LOCAL liTemp AS LONGINT
		LOCAL hTemp AS PTR

		hTemp:=oWin:Handle()
		// is it a window or child window
		// if hTemp==oWnd .or. !IsInstanceOf(oWin,#Printer)
    IF oWin IS Window
			DO CASE
			CASE wArea==HTCAPTION
				liTemp := RegionCaption
			CASE wArea==HTCLIENT
				liTemp := RegionCanvas
			CASE wArea==HTREDUCE
				liTemp := RegionMinBox
			CASE wArea==HTZOOM
				liTemp := RegionMaxBox
			CASE wArea==HTSYSMENU
				liTemp := RegionSystemMenuBox
			CASE wArea==HTBOTTOM .OR. ;
				wArea==HTBOTTOMLEFT .OR. ;
				wArea==HTBOTTOMRIGHT .OR. ;
				wArea==HTTOP .OR. ;
				wArea==HTLEFT .OR. ;
				wArea==HTRIGHT .OR. ;
				wArea==HTTOPLEFT .OR. ;
				wArea==HTTOPRIGHT
				liTemp := RegionBorder
			CASE wArea==HTMENU
				liTemp := RegionMenuBar
			CASE wArea == HTCLOSE
				liTemp := RegionClose
			CASE wArea==HTNowhere
				liTemp := RegionUnknown
			OTHERWISE
				RETURN FALSE
			ENDCASE
			GuiWin32.PostMessage(hTemp, WM_WCHelp, HelpWindow, liTemp)
		ELSE
			GuiWin32.PostMessage(hTemp, WM_WCHELP, HelpControl, LONGINT(_CAST,hTemp))
		ENDIF

		RETURN TRUE

 /// <exclude />

	[Obsolete];
	METHOD __ProcessToolTip(oControlNotifyEvent AS OBJECT) AS VOID STRICT
		// Handled by windows forms
		RETURN

	METHOD __ReadResource(oResourceID AS ResourceID, oOwner as OBJECT) AS LOGIC
		IF oResourceID != NULL_OBJECT
			oResourceDialog := ResourceDialog{oResourceID:Handle(), oResourceID:Name, oOwner}
		ENDIF
		IF oResourceDialog != NULL_OBJECT
			SELF:Caption	:= oResourceDialog:Caption
			SELF:dwStyle	:= oResourceDialog:Style
			SELF:dwExStyle  := oResourceDialog:ExStyle
		ENDIF
		RETURN oResourceDialog != NULL_OBJECT

 /// <exclude />
	METHOD __ReleaseDC() AS VOID STRICT
		// needed ?
		//IF (hDC != NULL_PTR)
		//	WCDCDelete(SELF)
		//	ReleaseDC(SELF:Handle(4), hDC)
		//	hDC := NULL_PTR
		//ENDIF
		RETURN


 /// <exclude />
	METHOD __SetBrushNeeded(lNew AS LOGIC) AS LOGIC STRICT
		DCBrushNeeded := lNew
		RETURN lNew


 /// <exclude />
	METHOD __SetDCInitialized(lNew AS LOGIC) AS LOGIC STRICT
		DCInitialized := lNew
		RETURN lNew


 /// <exclude />
	METHOD __SetFont(oNewFont AS Font) AS Font STRICT
		oFont := oNewFont
		SELF:__SetFont()
		RETURN oFont

	METHOD __SetFont() AS VOID STRICT
		IF SELF:oFont != NULL_OBJECT .and. SELF:oWnd != NULL_OBJECT
			oWnd:Font := SELF:oFont:__Font
		ENDIF
		RETURN

 /// <exclude />
	METHOD __SetPenNeeded(lNew AS LOGIC) AS LOGIC STRICT
		DCPenNeeded := lNew
		RETURN lNew

	[Obsolete];
	METHOD __SetSelfAssoAccel(lSwitch AS LOGIC) AS VOID STRICT
		RETURN

	[Obsolete];
	METHOD __SetSelfMenu() AS VOID STRICT
		RETURN

 /// <exclude />
	METHOD __Timer() AS VOID STRICT

		dwTimerCount := dwTimerCount - 1
		IF (dwTimerCount == 0)
			SELF:Timer()
			dwTimerCount := dwTimerInterval
			IF (dwTimerCount == 0)
				WC.UnregisterTimer(SELF)
				lTimerRegistered := FALSE
			ENDIF
		ENDIF

		RETURN

 /// <exclude />

	[Obsolete];
	METHOD __ToolTipHandle() AS IntPtr STRICT
		// Tooltips are handled in the Panel class
		RETURN IntPtr.Zero


 /// <exclude />
	METHOD __UpdateTrayIcon(dwNIM,oIcon,dwID,sToolTip)

		DEFAULT(@dwID, 1)
		DEFAULT(@sToolTip,"")
		DEFAULT(@oIcon,NULL_OBJECT)

		IF oTrayIcon == NULL_OBJECT
			oTrayIcon:= VOTrayIcon{SELF, dwID}
		ENDIF
		oTrayIcon:Text := sToolTip
		IF oIcon != NULL_OBJECT
			oTrayIcon:Image := ((Icon) oIcon):__Icon
		ENDIF
		oTrayIcon:Show()
		RETURN 	SELF

/// <include file="Gui.xml" path="doc/Window.Accelerator/*" />
	ACCESS Accelerator AS Accelerator
		RETURN oAccelerator

/// <include file="Gui.xml" path="doc/Window.Accelerator/*" />
	ASSIGN Accelerator(oNewAccelerator AS Accelerator)
		oAccelerator := oNewAccelerator
		RETURN


/// <include file="Gui.xml" path="doc/Window.AddTrayIcon/*" />
	METHOD AddTrayIcon(oTrayIcon, dwID, sToolTip)
		//PP-030902
		RETURN SELF:__UpdateTrayIcon(NIM_ADD,oTrayIcon,dwID,sToolTip)


/// <include file="Gui.xml" path="doc/Window.Animate/*" />
	METHOD Animate(nTime,nFlags)

		LOCAL dwTime,dwFlags AS DWORD
		// Determine Windows version
		dwTime	:= nTime
		dwFlags := nFlags
		GuiWin32.AnimateWindow(SELF:Handle(),dwTime,dwFlags)
		RETURN TRUE

/// <include file="Gui.xml" path="doc/Window.Automated/*" />
	ACCESS Automated AS LOGIC
		RETURN lAutomated

/// <include file="Gui.xml" path="doc/Window.Automated/*" />
	ASSIGN Automated(lNewVal AS LOGIC)
		IF lNewVal != SELF:lAutomated
			SELF:lAutomated := lNewVal
			//#ifdef USE_OLEOBJECT
			//	IF SELF:lAutomated
			//		_VOOLERegisterAutomationObject(PTR(_CAST, SELF), NULL_PSZ, 1, FALSE)
			//		//PCALL(gpfnOLERegisterAutomationObject, PTR(_CAST, SELF), NULL_PSZ, 1, FALSE)
			//	ELSE
			//		_VOOLEUnRegisterAutomationObject(PTR(_CAST, SELF))
			//		//PCALL(gpfnOLEUnRegisterAutomationObject, PTR(_CAST, SELF))
			//	ENDIF
			//#endif
		ENDIF



/// <include file="Gui.xml" path="doc/Window.Background/*" />
	ACCESS Background  AS Brush
		RETURN oBackground


/// <include file="Gui.xml" path="doc/Window.Background/*" />
	ASSIGN Background(oNewBackground AS Brush)
		oBackground := oNewBackground
		IF oBackGround != NULL_OBJECT .and. oBackground:Color != NULL_OBJECT
			SELF:__Surface:BackColor := 	oBackground:Color
		ELSE
			// Need to add a paint handler
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/Window.CanvasArea/*" />
	ACCESS CanvasArea  AS BoundingBox
		RETURN (BoundingBox) __Form:ClientRectangle


/// <include file="Gui.xml" path="doc/Window.CanvasErase/*" />
	METHOD CanvasErase()
		RETURN NIL


/// <include file="Gui.xml" path="doc/Window.Caption/*" />
	ACCESS Caption AS STRING
		RETURN cCaption

/// <include file="Gui.xml" path="doc/Window.Caption/*" />
	ASSIGN Caption(sNewCaption AS STRING)
		cCaption := sNewCaption
		IF SELF:__IsValid
			oWnd:Text := sNewCaption
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/Window.Center/*" />
	METHOD Center()
		IF SELF:__IsValid
			SELF:__Form:Center()
		ENDIF
		RETURN NIL


/// <include file="Gui.xml" path="doc/Window.Close/*" />
	METHOD Close(oEvent)
		IF SELF:__IsValid .and. ! SELF:IsClosing
			SELF:IsClosing := TRUE
			IF oWnd:IsAttached
				oWnd:Close()
			ENDIF
		ENDIF
		RETURN NIL

/// <include file="Gui.xml" path="doc/Window.ComboBoxExNotify/*" />
	METHOD ComboBoxExNotify(oControlNotifyEvent)
		//Todo ComboBoxExNotify
		//LOCAL oCNE AS ControlNotifyEvent
		//oCNE := oControlNotifyEvent
		//IF oCNE:NotifyCode = CBEN_ENDEDIT
		//	SELF:ComboBoxExEndEdit(ComboBoxExEndEditEvent{oCNE})
		//ENDIF
		RETURN NIL

/// <include file="Gui.xml" path="doc/Window.ContextMenu/*" />
	ACCESS ContextMenu AS Menu
		RETURN oContextMenu

/// <include file="Gui.xml" path="doc/Window.ContextMenu/*" />
	ASSIGN ContextMenu(oNewMenu AS Menu)
		oContextMenu := oNewMenu
		IF SELF:__IsValid
			IF oNewMenu == NULL_OBJECT
				oWnd:ContextMenu := NULL_OBJECT
			ELSE
				oWnd:ContextMenu := oNewMenu:__Menu:AsContextMenu()

			ENDIF
		ENDIF
	METHOD ContextMenuShow(oPos AS Point) AS VOID
		IF SELF:__IsValid .and. oWnd:ContextMenu != NULL_OBJECT
			oWnd:ContextMenu:Show(oWnd, oPos)
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/Window.ControlNotify/*" />
	METHOD ControlNotify(oControlNotifyEvent)
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
		RETURN NIL

/// <include file="Gui.xml" path="doc/Window.DateTimeSelectionChanged/*" />
	METHOD DateTimeSelectionChanged(oDateTimeSelectionEvent)
		//Sets the modified flag only, if the DTP-Control becomes changed.
		//In ComCtrl32 DLL below version 6, this eventhandler becomes called twice
		//if you change the date with the calender control.
		//With this change the workaround in the DateTimePicker:__Update() method
		//is not longer needed. The old workaround worked not correct in all cases.

		LOCAL oDTPicker AS DateTimePicker
		LOCAL cText     AS STRING
		LOCAL oEvt		  AS DateTimeSelectionEvent
		LOCAL cOldValue  AS STRING
		oEvt := oDateTimeSelectionEvent
		oDTPicker := (DateTimePicker) oEvt:Control
		cOldValue := oDTPicker:AsString()
		cText := oDTPicker:TextValue
		// Convert the DatePicker empty date to VEWA empty date
		IF cText == "01.01.1753"
			cText := "  .  .    "
		ENDIF
		IF IsInstanceOfUsual(oDTPicker:FieldSpec, #FieldSpec)
			cText := AsString(oDTPicker:FieldSpec:Val(cText))
		ENDIF
		IF ! cOldValue == cText
			oDTPicker:Modified := TRUE
			IF IsInstanceOf(oDTPicker:Owner, #DataWindow)
				((DataWindow) oDTPicker:Owner):__DoValidate(oDTPicker)
			ENDIF
		ENDIF
		IF oDTPicker:NullFormat .AND. oDTPicker:SelectedDate != NULL_DATE
			// Re-assign the value so the format gets set
			oDTPicker:SelectedDate := oDTPicker:SelectedDate
		ELSEIF !oDTPicker:NullFormat .AND. oDTPicker:SelectedDate == NULL_DATE
			// Reassign the selected Date so the format gets set
			oDTPicker:SelectedDate := NULL_DATE
		ENDIF


		RETURN 0L


/// <include file="Gui.xml" path="doc/Window.DeActivate/*" />
	METHOD DeActivate(oEvent AS Event)
		SELF:DeactivateAllOLEObjects()
		RETURN SELF:Default(oEvent)


/// <include file="Gui.xml" path="doc/Window.DeactivateAllOLEObjects/*" />
	METHOD DeactivateAllOLEObjects(oExcept)
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
		RETURN SELF



/// <include file="Gui.xml" path="doc/Window.Default/*" />
	METHOD Default(oEvent AS Event)
		SELF:EventReturnValue := 1L
		RETURN SELF

/// <include file="Gui.xml" path="doc/Window.DeleteTrayIcon/*" />
	METHOD DeleteTrayIcon(dwID)
		DEFAULT(REF dwID, 0)
		IF oTrayIcon != NULL_OBJECT
			IF oTrayIcon:ID == dwID
				oTrayIcon:Destroy()
			ENDIF
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/Window.Destroy/*" />
	METHOD Destroy() AS USUAL
		IF lAutomated
			SELF:Automated := FALSE
		ENDIF

		//__WCUnregisterMenu(oMenu)
		//__WCUnregisterMenu(oContextMenu)

		IF SELF:__IsValid
			IF oWnd:IsAttached
				oWnd:Dispose()
			ENDIF

			IF (oToolBar != NULL_OBJECT)
				oToolBar:Destroy()
				oToolBar := NULL_OBJECT
			ENDIF

			oMenu := NULL_OBJECT
			oIcon := NULL_OBJECT
			oIconSmall := NULL_OBJECT
			oContextMenu := NULL_OBJECT
			oWnd := NULL_OBJECT
			hDC := NULL_PTR
			oAccelerator := NULL_OBJECT
			oFont := NULL_OBJECT
			oPointer := NULL_OBJECT
			oBackground := NULL_OBJECT
			oForeground := NULL_OBJECT
			lTimerRegistered := FALSE
		ENDIF

		SUPER:Destroy()
		UnregisterAxit(SELF)
		//Gc.Collect()
		RETURN NIL


/// <include file="Gui.xml" path="doc/Window.Disable/*" />
	METHOD Disable()  AS VOID
		IF SELF:__IsValid
			oWnd:Enabled := FALSE
		ENDIF
		RETURN


	//ACCESS DragDropClient AS DragDropClient
	//	RETURN oDragDropClient


	//ACCESS DragDropServer AS DragDropServer
	//	RETURN oDragDropServer


	//ACCESS DragImageList AS VOSDK.ImageList
	//	RETURN oDragImageList


/// <include file="Gui.xml" path="doc/Window.Draw/*" />
	METHOD Draw(oDrawObject)
		LOCAL cnt, i AS DWORD
		LOCAL oDraw as DrawObject
		LOCAL aDraw as Array

		IF (oWnd == NULL_OBJECT) .AND. !IsInstanceOf(SELF, #Printer)
			RETURN SELF
		ENDIF

		IF !IsArray(oDrawObject)
			IF !IsInstanceOfUsual(oDrawObject,#DrawObject)
				WCError{#Draw,#Window,__WCSTypeError,oDrawObject,1}:Throw()
			ENDIF
			oDraw := oDrawObject
			oDraw:__SetWindow(SELF)
			oDraw:Draw()
		ELSE
			aDraw := oDrawObject
			cnt := ALen(aDraw)
			FOR i:=1 TO cnt
				IF !IsInstanceOfUsual(aDraw[i],#DrawObject)
					WCError{#Draw,#Window,__WCSTypeError,oDrawObject[i],1}:Throw()
				ENDIF
				oDraw := aDraw[i]
				oDraw:__SetWindow(SELF)
				oDraw:Draw()
			NEXT
		ENDIF

		RETURN SELF


/// <include file="Gui.xml" path="doc/Window.DrawBackground/*" />
	METHOD DrawBackground(hdc, oWindow)
		RETURN FALSE


/// <include file="Gui.xml" path="doc/Window.Drop/*" />
	METHOD Drop(oDragEvent )
		RETURN NIL

/// <include file="Gui.xml" path="doc/Window.Enable/*" />
	METHOD Enable()  AS VOID
		IF SELF:__IsValid
			oWnd:Enabled := TRUE
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/Window.EnableCloseBox/*" />
	METHOD EnableCloseBox(uValue := TRUE AS LOGIC)
		LOCAL hBox AS IntPtr
		IF SELF:__IsValid
			hBox := GuiWin32.GetSystemMenu(oWnd:Handle,FALSE)
		ENDIF
		IF hBox != IntPtr.Zero
			IF uValue
				RETURN GuiWin32.EnableMenuItem(hBox,SC_CLOSE,MF_ENABLED)
			ELSE
				RETURN GuiWin32.EnableMenuItem(hBox,SC_CLOSE,_OR(MF_GRAYED,MF_BYCOMMAND))
			ENDIF
		ENDIF

		RETURN NIL

/// <include file="Gui.xml" path="doc/Window.EnableDragDropClient/*" />
	METHOD EnableDragDropClient(lEnable := TRUE AS LOGIC) AS VOID
        IF SELF:__IsValid
            SELF:__Form:AllowDrop := lEnable
            IF lEnable
                SELF:oDragDropClient := DragDropClient{SELF}
            ELSE
                IF SELF:oDragDropClient != NULL_OBJECT
                    SELF:oDragDropClient:Destroy()
                    SELF:oDragDropClient:= NULL_OBJECT
                ENDIF
            ENDIF
        ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/Window.EnableDragDropServer/*" />

	[Obsolete];
	METHOD EnableDragDropServer(lEnable := TRUE AS LOGIC)
		//IF lEnable
		//	IF (oDragDropServer == NULL_OBJECT)
		//		oDragDropServer := DragDropServer{SELF}
		//	ENDIF
		//ELSEIF (oDragDropServer != NULL_OBJECT)
		//	oDragDropServer:Destroy()
		//	oDragDropServer := NULL_OBJECT
		//ENDIF

		RETURN SELF


/// <include file="Gui.xml" path="doc/Window.EnableHelp/*" />
	METHOD EnableHelp(lEnable AS LOGIC, oHelpDisplay AS HelpDisplay)

		IF lHelpOn
			// Disable previous Help
			lHelpOn := FALSE
			SELF:__EnableHelpCursor(FALSE)
			IF (oApp != NULL_OBJECT)
				oApp:__SetHelpWind(NULL_OBJECT, HM_NONE)
			ENDIF
		ENDIF

		IF lEnable
			// Enable New Help
			oCurrentHelp := oHelpDisplay
			IF oCurrentHelp = NULL_OBJECT .OR. ! oCurrentHelp:Win32Processing
				lHelpOn := TRUE
				//IF (GuiWin32.GetActiveWindow() == oWnd:Handle) .AND. (oApp != NULL_OBJECT)
				IF (oApp != NULL_OBJECT)
					oApp:__SetHelpWind(oWnd, HM_GENERAL)
				ENDIF
			ENDIF
		ELSE
			oCurrentHelp := NULL_OBJECT
		ENDIF

		RETURN SELF


/// <include file="Gui.xml" path="doc/Window.EnableHelpButton/*" />
	METHOD EnableHelpButton() AS VOID STRICT
		IF SELF:__IsValid
			oWnd:HelpButton := TRUE
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/Window.EnableHelpCursor/*" />
	METHOD EnableHelpCursor() AS VOID STRICT
		IF lHelpOn
			SELF:__EnableHelpCursor(TRUE)
		ELSE
			GuiWin32.PostMessage(SELF:Handle(), WM_SYSCOMMAND, SC_CONTEXTHELP, 0)
		ENDIF

		RETURN


/// <include file="Gui.xml" path="doc/Window.EnableThemeDialogTexture/*" />
	METHOD EnableThemeDialogTexture(dwStyle)
		// Todo EnableThemeDialogTexture

		//RETURN EnableThemeDialogTexture(SELF,dwStyle)
		RETURN SELF

/// <include file="Gui.xml" path="doc/Window.EnableToolTips/*" />
	METHOD EnableToolTips(lEnable := TRUE AS LOGIC) AS VOID
		// Todo EnableToolTips

		IF lEnable
			IF SELF:__Surface IS VOPanel VAR panel
				panel:EnableToolTips(lEnable)
			ENDIF
		ENDIF
		RETURN



/// <include file="Gui.xml" path="doc/Window.Font/*" />
	ACCESS Font AS VOSDK.Font
		RETURN oFont


/// <include file="Gui.xml" path="doc/Window.Font/*" />
	ASSIGN Font(oNewFont AS VOSDK.Font)
		oFont := oNewFont
		SELF:__SetFont()
		RETURN


/// <include file="Gui.xml" path="doc/Window.Foreground/*" />
	ACCESS Foreground AS Brush
		RETURN oForeground


/// <include file="Gui.xml" path="doc/Window.Foreground/*" />
	ASSIGN Foreground(oNewForeground AS Brush)
		oForeground := oNewForeground
		DCBrushInUse := FALSE
		DCBrushNeeded := TRUE
		// this forces the new object to be selected into the current DC and allows proper releasing of the old one
		SELF:__GetDC()

		RETURN


/// <include file="Gui.xml" path="doc/Window.GetAllChildren/*" />
	METHOD GetAllChildren() AS ARRAY STRICT
		LOCAL aRet AS ARRAY
		aRet := {}
		// Add all MDI Children
		IF SELF:__IsValid .AND. __Form:IsMDIContainer
			FOREACH form AS System.Windows.Forms.Form IN __Form:MdiChildren
				IF form IS VOForm
					LOCAL oVoForm AS VOForm
					oVoForm := (VOForm)  form
					AADD(aRet,oVoForm:Window)
				ENDIF
			NEXT
		ENDIF
		// Add all subwindows on the surface
		IF SELF:__Surface != NULL_OBJECT
			FOREACH VAR control IN SELF:__Surface:Controls
				IF control IS VOForm
					LOCAL oVoForm AS VOForm
					oVoForm := (VOForm)  control
					AADD(aRet,(USUAL) oVoForm:Window)
				ELSEIF control IS VOMenu
					// Skip
					NOP
				ELSEIF control IS VOToolbar
					// Skip
					NOP
				ELSEIF control IS IVOStatusBar
					// Skip
					NOP
				ELSEIF control IS IVOLabel
					// Skip
					NOP
				ELSEIF control IS IVOControl
					VAR oControl := control ASTYPE IVOControlProperties
					aRet:Add(oControl:Control)
					// Get the children of the group boxes also in this list
					IF oControl IS IVOGroupBox
						LOCAL aGroupChildren AS IList<IVOControl>
						VAR oGroup :=  control ASTYPE IVOGroupBox
						aGroupChildren := oGroup:getAllChildren(NULL)
						FOREACH oc AS IVOCOntrolProperties IN aGroupChildren
							AAdd(aRet,oC:Control)
						NEXT
					ENDIF
				ENDIF
			NEXT
		ENDIF
		RETURN aRet


	METHOD GetDlgItem(nItem AS LONG) AS ResourceDialogItem
		// For a Window based on a resource, this returns the control from the resource
		IF oResourceDialog != NULL_OBJECT
			RETURN oResourceDialog:GetDlgItem(nItem)
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/Window.GetStyle/*" />
	METHOD GetStyle() AS LONG
		IF SELF:__IsValid
			RETURN GuiWin32.GetWindowLong(oWnd:Handle, GWL_STYLE)
		ENDIF
		RETURN SELF:dwStyle

/// <include file="Gui.xml" path="doc/Window.GetExStyle/*" />
	METHOD GetExStyle AS LONG
		IF SELF:__IsValid
			RETURN GuiWin32.GetWindowLong(oWnd:Handle, GWL_EXSTYLE)
		ENDIF
		RETURN SELF:dwExStyle


/// <include file="Gui.xml" path="doc/Window.Handle/*" />
	METHOD Handle() AS IntPtr  CLIPPER
		LOCAL hWin AS Intptr
		IF SELF:__IsValid
			hWin := oWnd:Handle
			RETURN hWin
		ENDIF
		RETURN IntPtr.Zero

/// <include file="Gui.xml" path="doc/Window.HasExStyle/*" />
	METHOD HasExStyle(kStyle AS LONG) AS LOGIC
		LOCAL liStyle	AS LONG
		liStyle := SELF:GetExStyle()
		RETURN _AND(liStyle,kStyle) != 0

/// <include file="Gui.xml" path="doc/Window.HasStyle/*" />
	METHOD HasStyle(kStyle AS LONG) AS LOGIC
		LOCAL liStyle	AS LONG
		liStyle := SELF:GetStyle()
		RETURN _AND(liStyle,kStyle) != 0

/// <include file="Gui.xml" path="doc/Window.HelpDisplay/*" />
	ACCESS HelpDisplay AS HelpDisplay
		RETURN oCurrentHelp

/// <include file="Gui.xml" path="doc/Window.HelpDisplay/*" />
	ASSIGN HelpDisplay(oHelpDisplay AS HelpDisplay)
		SELF:EnableHelp(TRUE, oHelpDisplay)

		RETURN

/// <include file="Gui.xml" path="doc/Window.HelpRequest/*" />
	METHOD HelpRequest(oHelpRequestEvent)
		LOCAL dwType AS LONG
		LOCAL oHRE AS HelpRequestEvent
		LOCAL oP AS OBJECT

		oP := SELF:Owner
		DO WHILE IsInstanceOf(oP, #Window)
			((Window) oP):__EnableHelpCursor(FALSE)
			oP := ((Window) oP):Owner
		ENDDO

		IF IsInstanceOfUsual(oHelpRequestEvent, #HelpRequestEvent)
			IF SELF:HelpDisplay==NULL_OBJECT
				IF IsMethod(SELF:oParent, #HelpRequest)
					Send(SELF:oParent, #HelpRequest, oHelpRequestEvent)
				ENDIF
			ELSE
				oHRE := oHelpRequestEvent
				dwType := oHRE:HelpType
				DO CASE
				CASE dwType == HELPMENU
				CASE dwType == HELPCONTROL
					IF ! empty(oHRE:HelpContext)
						oCurrentHelp:Show(oHRE:HelpContext)
					ELSEIF ! empty(oHRE:HyperLabel:Name)
						oCurrentHelp:Show(oHRE:HyperLabel:Name)
					ELSE
						oCurrentHelp:Show("Control_"+AsString(oHRE:Control:ControlID))
					ENDIF
				CASE dwType == HELPWINDOW
					IF ! empty(oHRE:HelpContext)
						oCurrentHelp:Show(oHRE:HelpContext)
					ELSE
						oCurrentHelp:Show("HelpIndex")
					ENDIF
				CASE dwType == HELPINFO
				ENDCASE
			ENDIF
		ENDIF


		RETURN NIL


/// <include file="Gui.xml" path="doc/Window.Hide/*" />
	METHOD Hide() AS VOID STRICT
		IF SELF:__IsValid
			oWnd:Visible := FALSE
		ENDIF

		RETURN


/// <include file="Gui.xml" path="doc/Window.HorizontalScroll/*" />
	METHOD HorizontalScroll(oScrollEvent AS ScrollEvent)
		LOCAL oScrollBar AS ScrollBar
		LOCAL oEvt	:= oScrollEvent AS ScrollEvent

		oScrollBar := oEvt:ScrollBar
		IF (oScrollBar != NULL_OBJECT)
			oScrollBar:ThumbPosition := oEvt:Position
		ENDIF

		RETURN SELF:Default(oEvt)


/// <include file="Gui.xml" path="doc/Window.HorizontalSlide/*" />
	METHOD HorizontalSlide(oSliderEvent AS SliderEvent)
		LOCAL oSlider AS Slider
		oSlider := oSliderEvent:Slider
		IF (oSlider != NULL_OBJECT)
			oSlider:ThumbPosition := oSliderEvent:Position
		ENDIF
		RETURN SELF:Default(oSliderEvent)


/// <include file="Gui.xml" path="doc/Window.HorizontalSpin/*" />
	METHOD HorizontalSpin(oSpinnerEvent AS SpinnerEvent)
		LOCAL oSpinner AS Spinner
		oSpinner := oSpinnerEvent:Spinner
		IF (oSpinner != NULL_OBJECT)
			oSpinner:Position := oSpinnerEvent:Position
		ENDIF

		RETURN SELF:Default(oSpinnerEvent)


/// <include file="Gui.xml" path="doc/Window.HyperLabel/*" />
	ACCESS HyperLabel AS HyperLabel
		RETURN oHyperLabel


/// <include file="Gui.xml" path="doc/Window.HyperLabel/*" />
	ASSIGN HyperLabel(oHL AS HyperLabel)
		IF IsInstanceOfUsual(oHL,#HyperLabel)
			oHyperLabel := oHL
			SELF:@@StatusMessage(oHL, MESSAGEPERMANENT)
		ENDIF

		RETURN


/// <include file="Gui.xml" path="doc/Window.Icon/*" />
	ACCESS Icon AS Icon
		RETURN oIcon


/// <include file="Gui.xml" path="doc/Window.Icon/*" />
	ASSIGN Icon(oNewIcon AS Icon)
		oIcon := oNewIcon
		IF SELF:__IsValid
			SELF:__Form:Icon := oIcon:__Icon
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/Window.IconSm/*" />
	ACCESS IconSm AS Icon
		RETURN oIconSmall


/// <include file="Gui.xml" path="doc/Window.IconSm/*" />
	ASSIGN IconSm(oNewIcon AS Icon)
		oIconSmall := oNewIcon
		IF SELF:__IsValid
			SELF:__Form:SmallIcon := oIconSmall:__Icon
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/Window.ctor/*" />
	CONSTRUCTOR(oOwner)


		SUPER()
		IF oOwner != NULL_OBJECT
			IF oOwner IS System.Windows.Forms.Form
				oParent := __ForeignWindow{oOwner}
			ELSE
				oParent := oOwner
			ENDIF
		ENDIF
		oOrigin := Point{0,0}

		aAlignes		:= {}
		aDelayedAlignes := {}
		lDelayAlignment := TRUE
		oFont := System.Drawing.SystemFonts.DefaultFont

		//PP-030910
		SELF:SetBackgroundBrush()
		oWnd  := (VoForm) SELF:__CreateForm()
		IF oWnd != NULL_OBJECT
			oWnd:Visible := FALSE
			SELF:__SetFont()

		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/Window.IsEnabled/*" />
	METHOD IsEnabled()  AS LOGIC CLIPPER
		IF oWnd != NULL_OBJECT
			RETURN oWnd:Enabled
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/Window.IsIconic/*" />
	METHOD IsIconic() AS LOGIC
		IF oWnd != NULL_OBJECT
			RETURN oWnd:WindowState == System.Windows.Forms.FormWindowState.Minimized
		ENDIF
		RETURN FALSE


/// <include file="Gui.xml" path="doc/Window.IsVisible/*" />
	METHOD IsVisible()  AS LOGIC
		IF SELF:__IsValid
			RETURN oWnd:Visible
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/Window.IsZoomed/*" />
	METHOD IsZoomed()
		IF SELF:__IsValid
			RETURN oWnd:WindowState == System.Windows.Forms.FormWindowState.Maximized
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/Window.LineTo/*" />
	METHOD LineTo(oPoint)
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

		RETURN SELF


/// <include file="Gui.xml" path="doc/Window.ListViewItemDrag/*" />
	METHOD ListViewItemDrag(oListViewDragEvent)
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

		RETURN SELF:Default(oListViewDragEvent)

/// <include file="Gui.xml" path="doc/Window.Menu/*" />
	ACCESS Menu AS Menu
		RETURN oMenu

/// <include file="Gui.xml" path="doc/Window.Menu/*" />
	ASSIGN Menu(oNewMenu AS Menu)
		oMenu := oNewMenu
		oMenu:__Owner := SELF
		IF SELF:__IsValid
			SELF:__Form:Menu := oMenu:__Menu
		ENDIF
		FOREACH oItem AS VOMenuItem IN oNewMenu:__Menu:MenuItems
			oItem:MergeType := System.Windows.Forms.MenuMerge.Remove
		NEXT

		IF (oMenu == NULL_OBJECT)
			SELF:Accelerator := NULL_OBJECT
			SELF:ToolBar := NULL_OBJECT
		ELSE
			SELF:Accelerator := oMenu:Accelerator
			SELF:ToolBar := oMenu:ToolBar
			oMenu:SetShortCuts(SELF:Accelerator)
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/Window.MinMaxInfo/*" />
	METHOD MinMaxInfo(oMinMaxInfoEvent)
		LOCAL oEvt := oMinMaxInfoEvent AS MinMaxInfoEvent
		IF oMinSize != NULL_OBJECT
			oEvt:MinTrackSize := oMinSize
		ENDIF
		RETURN SELF


/// <include file="Gui.xml" path="doc/Window.MinSize/*" />
	ACCESS MinSize AS Dimension
		RETURN oMinSize

/// <include file="Gui.xml" path="doc/Window.MinSize/*" />
	ASSIGN MinSize(oSize AS Dimension)
		oMinSize := oSize

/// <include file="Gui.xml" path="doc/Window.ModifyTrayIcon/*" />
	METHOD ModifyTrayIcon(oTrayIcon, dwID, sToolTip)
		//PP-030902
		RETURN SELF:__UpdateTrayIcon(NIM_MODIFY,oTrayIcon,dwID,sToolTip)

/// <include file="Gui.xml" path="doc/Window.MonthCalSelectionChanged/*" />
	METHOD MonthCalSelectionChanged(_oMonthCalSelectionEvent)
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

		RETURN SELF:Default(_oMonthCalSelectionEvent)



/// <include file="Gui.xml" path="doc/Window.MouseButtonUp/*" />
	METHOD MouseButtonUp(oMouseEvent)
		// Todo MouseButtonUp

		//IF lDragActive
		//	ReleaseCapture()
		//	ShowCursor(TRUE)
		//	oDragImageList:EndDrag()
		//	oDragImageList := NULL_OBJECT
		//	lDragActive := FALSE
		//ENDIF


		RETURN SELF:Default(oMouseEvent)


/// <include file="Gui.xml" path="doc/Window.MouseDrag/*" />
	METHOD MouseDrag(oMouseEvent)
		// Todo MouseDrag
		//LOCAL oEvt := oMouseEvent AS MouseEvent

		//IF lDragActive
		//	oDragImageList:DragMove(oEvt:Position)
		//	//ImageList_DragMove(oMouseEvent:Position:Y, oMouseEvent:Position:Y)
		//ENDIF

		RETURN SELF:Default(oMouseEvent)



/// <include file="Gui.xml" path="doc/Window.MouseTrapOff/*" />
	METHOD MouseTrapOff()
		// Todo MouseTrapOff
		//ReleaseCapture()
		RETURN NIL


/// <include file="Gui.xml" path="doc/Window.MouseTrapOn/*" />
	METHOD MouseTrapOn()
		// Todo MouseTrapOn
		//SetCapture(SELF:Handle(0))
		RETURN NIL


/// <include file="Gui.xml" path="doc/Window.MoveTo/*" />
	METHOD MoveTo(oPoint AS Point)  AS Point
		// Todo
		//LOCAL winPoint IS _winPoint  // dcaton 070316 was _winsize
		//IF (oWnd != NULL_PTR) .OR. IsInstanceOf(SELF, #Printer)
		//	DCPenNeeded := TRUE
		//	IF (SELF:__GetDC() != NULL_PTR)
		//		MoveToEx(hDC,oPoint:X,oPoint:Y, @winPoint)
		//		RETURN Point{winPoint:x,winPoint:y}
		//	ENDIF
		//ENDIF

		RETURN Point{0, 0}

	ACCESS NameSym AS SYMBOL
		IF SELF:HyperLabel != NULL_OBJECT
			RETURN SELF:HyperLabel:NameSym
		ENDIF
		RETURN NULL_SYMBOL


/// <include file="Gui.xml" path="doc/Window.Origin/*" />
	ACCESS Origin AS Point
		RETURN WC.GetOrigin(SELF)

/// <include file="Gui.xml" path="doc/Window.Origin/*" />
	ASSIGN Origin(oPoint AS Point)
		IF SELF:__IsValid
			oWnd:Location := oPoint
		ELSE
			WC.MoveWindow(oWnd, oPoint, TRUE)
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/Window.Owner/*" />
	ACCESS Owner AS OBJECT
		RETURN oParent

/// <include file="Gui.xml" path="doc/Window.Owner/*" />
	ASSIGN Owner(oWindow AS OBJECT)
		IF IsInstanceOfUsual(oWindow, #Window)
			SELF:oParent := oWindow
			IF SELF:__IsValid
				oWnd:Owner := ((Window) oWindow):__Form
			ENDIF
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/Window.OwnerAlignment/*" />
	ASSIGN OwnerAlignment(iNewVal AS USUAL)
		LOCAL oFormWindow  	AS OBJECT
		LOCAL oWindow		AS WINDOW
		oFormWindow := SELF:Owner
		IF IsInstanceOf(oFormWindow, #Window)
			oWindow := oFormWindow
			oWindow:__AddAlign(SELF, iNewVal)
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/Window.PaintBackground/*" />
	METHOD PaintBackground(hDC)
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

		RETURN NIL


/// <include file="Gui.xml" path="doc/Window.PaintBoundingBox/*" />
	METHOD PaintBoundingBox(oBoundingBox, kPaintMode)
		//Todo PaintBoundingBox
		//LOCAL hBrush AS PTR
		//LOCAL r IS _WinRect



		//IF !IsInstanceOfUsual(oBoundingBox, #BoundingBox)
		//	WCError{#PaintBoundingBox,#Window,__WCSTypeError,oBoundingBox,1}:Throw()
		//ENDIF

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

		RETURN NIL


/// <include file="Gui.xml" path="doc/Window.Pen/*" />
	ACCESS Pen as Pen
		RETURN oPen


/// <include file="Gui.xml" path="doc/Window.Pen/*" />
	ASSIGN Pen(oPen AS Pen)
		SELF:oPen := oPen
		DCPenNeeded := TRUE
		DCPenInUse := FALSE
		// this forces the new object to be selected into the current DC and allows proper releasing of the old one
		SELF:__GetDC()
		RETURN


/// <include file="Gui.xml" path="doc/Window.Pointer/*" />
	ACCESS Pointer AS Pointer
		IF SELF:__Form != NULL_OBJECT
			oPointer := SELF:__Form:Cursor
		ENDIF
		IF oPointer == NULL_OBJECT
			oPointer := Pointer{}
		ENDIF
		RETURN oPointer

/// <include file="Gui.xml" path="doc/Window.Pointer/*" />
	ASSIGN Pointer(oNewPointer AS Pointer)
		oPointer := oNewPointer
		IF SELF:__IsValid
			SELF:__Form:Cursor := oPointer
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/Window.PostInit/*" />
	METHOD PostInit() CLIPPER
		RETURN SELF

/// <include file="Gui.xml" path="doc/Window.PreInit/*" />
	METHOD PreInit()  CLIPPER
		RETURN SELF



/// <include file="Gui.xml" path="doc/Window.Print/*" />
	METHOD Print(oDevice)
		LOCAL lRet AS LOGIC
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

		RETURN lRet


/// <include file="Gui.xml" path="doc/Window.QueryClose/*" />
/// <include file="Gui.xml" path="doc/Window.RegisterTimer/*" />
	METHOD RegisterTimer(nInterval, lOneTime)

		IF !IsLong(nInterval)
			WCError{#RegisterTimer,#Window,__WCSTypeError,nInterval,1}:Throw()
		ENDIF

		IF !IsNil(lOneTime)
			IF !IsLogic(lOneTime)
				WCError{#RegisterTimer,#Window,__WCSTypeError,lOneTime,2}:Throw()
			ENDIF
			IF lOneTime
				dwTimerInterval := 0
			ELSE
				dwTimerInterval := nInterval
			ENDIF
		ELSE
			dwTimerInterval := nInterval
		ENDIF

		IF (nInterval > 0)
			dwTimerCount:=nInterval
			IF !lTimerRegistered
				WC.RegisterTimer(SELF)
				lTimerRegistered := TRUE
			ENDIF
		ELSE
			WC.UnregisterTimer(SELF)
			lTimerRegistered := FALSE
		ENDIF

		RETURN SELF


/// <include file="Gui.xml" path="doc/Window.RePaint/*" />
	METHOD RePaint() AS VOID STRICT
		IF SELF:__IsValid
			SELF:oWnd:Invalidate(TRUE)
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/Window.RepaintBoundingBox/*" />
	METHOD RepaintBoundingBox(oBoundingBox AS BoundingBox) AS VOID STRICT
		IF SELF:__IsValid
			SELF:oWnd:Invalidate( (System.Drawing.Rectangle) oBoundingBox)
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/Window.Resize/*" />
	METHOD Resize(oResizeEvent)
		LOCAL uRet AS USUAL
		uRet := SELF:@@Default(oResizeEvent)
		SELF:__AlignControls()
		RETURN uRet

/// <include file="Gui.xml" path="doc/Window.Scroll/*" />
	METHOD Scroll(oDimension, oBoundingBox, lClip)
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

		RETURN SELF


/// <include file="Gui.xml" path="doc/Window.SetAlignStartSize/*" />
	METHOD SetAlignStartSize(oSize AS Dimension) AS VOID

		IF ALen(aAlignes) = 0 .OR. aAlignes[1][1] != NULL_OBJECT
			AAdd(aAlignes, NIL)
			IF ALen(aAlignes) > 1
				AIns(aAlignes, 1)
			ENDIF
			aAlignes[1] := {NULL_OBJECT, NIL}
		ENDIF
		IF oSize == NULL_OBJECT
			oSize := (Dimension) oWnd:ClientRectangle
		ENDIF
		aAlignes[1][2] := oSize
		RETURN


/// <include file="Gui.xml" path="doc/Window.SetBackgroundBrush/*" />
	METHOD SetBackgroundBrush(dwNew)
		// TOdo SetBackgroundBrush
		//Default(@dwNew,COLOR_3DSHADOW)

		//SetClassLong(SELF:handle(), GCL_HBRBACKGROUND, dwNew)
		RETURN SELF


/// <include file="Gui.xml" path="doc/Window.SetExStyle/*" />
	METHOD SetExStyle(dwSetStyle, lEnable)
		LOCAL iWnd AS IVOForm
		DEFAULT(@lEnable, TRUE)

		IF (oWnd != NULL_OBJECT)
			iWnd := (IVOForm) (OBJECT) oWnd
			dwExStyle := GuiWin32.GetWindowLong(oWnd:Handle, GWL_EXSTYLE)

			IF lEnable
				dwExStyle := _OR(dwExStyle, LONG(_CAST, dwSetStyle))
				iWnd:Properties:ExStyle |= dwSetStyle
			ELSE
				dwExStyle := _AND(dwExStyle, _NOT(LONG(_CAST, dwSetStyle)))
				iWnd:Properties:NotExStyle |= dwSetStyle
			ENDIF

			GuiWin32.SetWindowLong(oWnd:Handle, GWL_EXSTYLE, dwSetStyle)
		ENDIF

		RETURN dwExStyle


/// <include file="Gui.xml" path="doc/Window.SetFocus/*" />
	METHOD SetFocus() AS VOID STRICT
		IF SELF:__IsValid  .AND. oWnd:Visible
			oWnd:Focus()
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/Window.SetHandle/*" />
	METHOD SetHandle(hNewWnd AS VOForm)
		oWnd := hNewWnd
		RETURN oWnd


/// <include file="Gui.xml" path="doc/Window.SetStyle/*" />
	METHOD SetStyle(dwSetStyle, lEnable)

		DEFAULT(@lEnable, TRUE)


			IF lEnable
				dwStyle := _OR(dwStyle, LONG(_CAST, dwSetStyle))

		ELSE
				dwStyle := _AND(dwStyle, _NOT(LONG(_CAST, dwSetStyle)))
			ENDIF

		RETURN dwStyle

	METHOD __GetStartPosFromShowState(kSHowState AS LONG) AS System.Windows.Forms.FormStartPosition
		LOCAL startPos AS System.Windows.Forms.FormStartPosition
		IF (kSHowState == SHOWZOOMED)
			oWnd:WindowState := System.Windows.Forms.FormWindowState.Maximized
		ELSEIF (kSHowState == SHOWICONIZED)
			oWnd:WindowState :=System.Windows.Forms.FormWindowState.Minimized
			//ELSEIF kShowState == SHOWINACTIVE
			//
		ELSEIF kSHowState == SHOWCENTERED
			IF  IsInstanceOf(SELF, #dialogWindow)
				startPos := System.Windows.Forms.FormStartPosition.CenterScreen
			ELSE
				startPos := System.Windows.Forms.FormStartPosition.CenterParent
			ENDIF
		ELSE
			IF SELF:Origin:X != 0 .or. SELF:Origin:Y != 0
				startPos := System.Windows.Forms.FormStartPosition.Manual
			ELSEIF IsInstanceOf(SELF, #dialogWindow)
				startPos := System.Windows.Forms.FormStartPosition.CenterScreen
			ELSE
				startPos := System.Windows.Forms.FormStartPosition.WindowsDefaultLocation
			ENDIF
		ENDIF
		RETURN startPos

	METHOD SuspendLayout AS VOID STRICT
		IF SELF:__IsValid
			SELF:oWnd:SuspendLayout()
		ENDIF
		RETURN

	METHOD ResumeLayout AS VOID STRICT
		IF SELF:__IsValid
			SELF:oWnd:ResumeLayout()
		ENDIF
		RETURN

	METHOD OnMdiChildActivated(s AS OBJECT, e AS EventArgs) AS VOID
		SELF:Activate(@@Event{})

    METHOD Show() AS VOID STRICT
        SELF:Show(SHOWNORMAL)

	METHOD Show(kShowState AS LONG ) AS VOID STRICT
		IF SELF:__IsValid
            oWnd:SuspendLayout()
			IF (NULL_STRING != cCaption)
				oWnd:Text := cCaption
			ENDIF
			oWnd:WindowState := System.Windows.Forms.FormWindowState.Normal
			IF SELF:__Form:isMdiChild
				LOCAL form AS System.Windows.Forms.Form
				form := SELF:__Form:MdiParent
				SELF:__Form:MdiChildActivate += OnMdiChildActivated
				IF form != NULL
					form := form:ActiveMdiChild
					IF form != NULL .AND. form:WindowState == System.Windows.Forms.FormWindowState.Maximized
						form:WindowState := System.Windows.Forms.FormWindowState.Normal
					ENDIF
				ENDIF
			ENDIF
			oWnd:StartPosition := SELF:__GetStartPosFromShowState(kShowState)
			//oWnd:Visible := TRUE
			// check if visible
			IF oWnd:Parent != NULL_OBJECT
				LOCAL IMPLIED oPoint := oWnd:Location
				LOCAL lChanged AS LOGIC
				IF oWnd:Location:Y + oWnd:Size:Height > oWnd:Parent:Height
					oPoint:Y := (oWnd:Parent:Height - oWnd:Height) /2
					lChanged := TRUE
				ENDIF
				IF oWnd:Location:X + oWnd:Size:Width > oWnd:Parent:Width .or. lChanged
					oPoint:X := (oWnd:Parent:Width- oWnd:Width) /2
					lChanged := TRUE
				ENDIF
				IF oWnd:Location:X < 0 .or. oPoint:X < 0
					oPoint:X := 0
					lChanged := TRUE
				ENDIF
				IF oWnd:Location:Y < 0 .or. oPoint:Y < 0
					oPoint:Y := 0
					lChanged := TRUE
				ENDIF
				IF lChanged
					oWnd:Location := oPoint
                ENDIF
			ENDIF
			lDelayAlignment := FALSE
			FOREACH aElement AS ARRAY IN aDelayedAlignes
				SELF:__AddAlign(aElement[1], aElement[2])
			NEXT
			aDelayedAlignes := {}
			SELF:__AlignControls()
			// Counters gray areas when first showing a window
			oWnd:SuspendReDraw()
			IF kShowState == SHOWCENTERED .AND. oWnd:Parent == NULL_OBJECT
				oWnd:Center()
			ENDIF
			oWnd:ResumeReDraw()
            oWnd:ResumeLayout()
			oWnd:Show()

		ENDIF
		RETURN
/// <include file="Gui.xml" path="doc/Window.ShowBalloonTrayTip/*" />

	METHOD ShowBalloonTrayTip(oTrayIcon,dwID,sHeading,sToolTip,dwTimeOut,dwInfo)
		DEFAULT(@dwID,1)
		DEFAULT(@sHeading,"")
		DEFAULT(@sToolTip,"")
		DEFAULT(@dwInfo,NIIF_NONE)
		DEFAULT(@oTrayIcon,NULL_OBJECT)
		DEFAULT(@dwTimeOut,10000)

		IF oTrayIcon == NULL_OBJECT
			SELF:__UpdateTrayIcon(0, oTrayIcon, dwID, sToolTip)
		ENDIF
		IF oTrayIcon != NULL_OBJECT
			oTrayIcon:ShowBalloonTip(dwTimeOut, sHeading, sToolTip, dwInfo)
		ENDIF
		RETURN NIL

/// <include file="Gui.xml" path="doc/Window.Size/*" />
	ACCESS Size AS Dimension
		LOCAL oSize AS Dimension
		IF SELF:__IsValid
			oSize := oWnd:Size
		ELSE
			oSize := Dimension{}
		ENDIF
		RETURN oSize


/// <include file="Gui.xml" path="doc/Window.Size/*" />
	ASSIGN Size(oDimension AS Dimension)
		IF SELF:__IsValid
			oWnd:Size := oDimension
		ENDIF

		RETURN


/// <include file="Gui.xml" path="doc/Window.SizeText/*" />
	METHOD SizeText(cTextToSize AS STRING) AS Dimension
		LOCAL oDim AS Dimension
		IF SELF:oFont != NULL_OBJECT
			oDim := System.Windows.Forms.TextRenderer.MeasureText(cTextToSize, SELF:oFont)
		ELSE
			oDim := System.Windows.Forms.TextRenderer.MeasureText(cTextToSize, System.Drawing.SystemFonts.DefaultFont)
		ENDIF
		RETURN oDim


/// <include file="Gui.xml" path="doc/Window.StatusMessage/*" />
	METHOD StatusMessage(oHL, ntype)
		RETURN NIL

/// <include file="Gui.xml" path="doc/Window.SysLinkSelect/*" />
	METHOD SysLinkSelect(oSysLinkSelectEvent)
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
		RETURN SELF:Default(oSysLinkSelectEvent)


/// <include file="Gui.xml" path="doc/Window.TextColor/*" />
	ACCESS TextColor as Pen
		RETURN oPen


/// <include file="Gui.xml" path="doc/Window.TextColor/*" />
	ASSIGN TextColor(oNewPen as Pen)
		SELF:Pen:= oNewPen
		RETURN


/// <include file="Gui.xml" path="doc/Window.TextPrint/*" />
	METHOD TextPrint(cText, oPoint)
		// Todo TextPrint
		//LOCAL strucLogBrush IS _WinLogBrush
		//LOCAL iOldMode, iNewMode AS INT
		//LOCAL dwOldBack AS DWORD
		//LOCAL lUsingBrush AS LOGIC



		//IF !IsString(cText)
		//	WCError{#TextPrint,#Window,__WCSTypeError,cText,1}:Throw()
		//ENDIF
		//IF !IsInstanceOfUsual(oPoint,#Point)
		//	WCError{#TextPrint,#Window,__WCSTypeError,oPoint,2}:Throw()
		//ENDIF

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

		RETURN SELF


/// <include file="Gui.xml" path="doc/Window.Timer/*" />
	METHOD Timer()  CLIPPER
		RETURN SELF


/// <include file="Gui.xml" path="doc/Window.ToolBar/*" />
	ACCESS ToolBar AS ToolBar
		RETURN oToolBar


/// <include file="Gui.xml" path="doc/Window.ToolBar/*" />
	ASSIGN ToolBar(oNewToolBar AS ToolBar)
		IF oNewToolBar != NULL_OBJECT
			IF (SELF:Menu != NULL_OBJECT) .AND. (SELF:Menu:ToolBar != oNewToolBar)
				SELF:Menu:ToolBar := NULL_OBJECT
			ENDIF

			IF (oToolBar != NULL_OBJECT) .AND. (oToolBar != oNewToolBar)
				oToolBar:Destroy()
			ENDIF

			oToolBar := oNewToolBar

			IF oToolBar != NULL_OBJECT
				oToolBar:__SetParent(SELF)
				oToolBar:Show()
			ENDIF
		ENDIF
		RETURN



/// <include file="Gui.xml" path="doc/Window.ToTop/*" />
	METHOD ToTop()
		IF SELF:__IsValid
			oWnd:BringToFront()
		ENDIF
		RETURN NIL




/// <include file="Gui.xml" path="doc/Window.TreeViewItemDrag/*" />
	METHOD TreeViewItemDrag(oTreeViewDragEvent)
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
		//ENDIF

		RETURN SELF:Default(oTreeViewDragEvent)

/// <include file="Gui.xml" path="doc/Window.Update/*" />
	METHOD Update()
		IF SELF:__IsValid
			SELF:oWnd:Update()
		ENDIF

		RETURN NIL

/// <include file="Gui.xml" path="doc/Window.VerticalScroll/*" />
	METHOD VerticalScroll(oScrollEvent AS ScrollEvent)
		LOCAL oScrollBar AS ScrollBar
		LOCAL oEvt	:= oScrollEvent AS ScrollEvent

		oScrollBar := oEvt:ScrollBar
		IF (oScrollBar != NULL_OBJECT)
			oScrollBar:ThumbPosition:=oEvt:Position
		ENDIF

		RETURN SELF:Default(oEvt)

/// <include file="Gui.xml" path="doc/Window.VerticalSlide/*" />
	METHOD VerticalSlide(oSliderEvent AS SliderEvent)
		LOCAL oSlider AS Slider
		oSlider := oSliderEvent:Slider
		IF (oSlider != NULL_OBJECT)
			oSlider:ThumbPosition := oSliderEvent:Position
		ENDIF
		RETURN SELF:Default(oSliderEvent)

/// <include file="Gui.xml" path="doc/Window.VerticalSpin/*" />
	METHOD VerticalSpin(oSpinnerEvent AS SpinnerEvent)
		LOCAL oSpinner AS Spinner

		oSpinner := oSpinnerEvent:Spinner
		IF (oSpinner != NULL_OBJECT)
			oSpinner:Position:=oSpinnerEvent:Position
		ENDIF

		RETURN SELF:Default(oSpinnerEvent)

/// <include file="Gui.xml" path="doc/Window.WindowArea/*" />
	ACCESS WindowArea AS BoundingBox
		RETURN (BoundingBox) __Form:Bounds

END CLASS


/// <exclude/>

CLASS __ForeignWindow INHERIT Window

	CONSTRUCTOR(oWndSelf)
		SUPER()
		oWnd := oWndSelf
		RETURN

END CLASS



#region defines
DEFINE OA_LEFT          := 1
DEFINE OA_LEFT_AUTOSIZE     := 5
DEFINE OA_NO            := 0
DEFINE OA_RIGHT         := 3
DEFINE OA_RIGHT_AUTOSIZE    := 7
DEFINE OA_TOP           := 2
DEFINE OA_TOP_AUTOSIZE      := 6
DEFINE OA_BOTTOM        := 4
DEFINE OA_BOTTOM_AUTOSIZE   := 8
DEFINE OA_CENTER        := 9
DEFINE OA_FULL_SIZE         := 10
DEFINE OA_Height                := 0b0000000110000000
DEFINE OA_PHeight               := 0b0000001110000000
DEFINE OA_Width                 := 0b0000010010000000
DEFINE OA_PWidth                := 0b0000110010000000
DEFINE OA_WIDTH_HEIGHT          := OA_Width | OA_HEIGHT
DEFINE OA_WIDTH_PHEIGHT         := OA_Width | OA_PHEIGHT
DEFINE OA_PWIDTH_HEIGHT         := OA_PWidth | OA_HEIGHT
DEFINE OA_PWIDTH_PHEIGHT        := OA_PWidth | OA_PHEIGHT
DEFINE OA_HEIGHT_WIDTH          := OA_WIDTH_HEIGHT
DEFINE OA_HEIGHT_PWIDTH         := OA_PWIDTH_HEIGHT
DEFINE OA_PHEIGHT_WIDTH         := OA_WIDTH_PHEIGHT
DEFINE OA_PHEIGHT_PWIDTH        := OA_PWIDTH_PHEIGHT
DEFINE OA_X                     := 0b0100000010000000
DEFINE OA_PX                    := 0b1100000010000000
DEFINE OA_Y                     := 0b0001000010000000
DEFINE OA_PY                    := 0b0011000010000000
DEFINE OA_PX_HEIGHT             := OA_PX | OA_Height
DEFINE OA_PX_PHEIGHT            := OA_PX | OA_PHeight
DEFINE OA_PX_WIDTH              := OA_PX | OA_Width
DEFINE OA_PX_PWIDTH             := OA_PX | OA_PWidth
DEFINE OA_PX_PWIDTH_HEIGHT      := OA_PX | OA_PWidth | OA_HEIGHT
DEFINE OA_PX_WIDTH_HEIGHT       := OA_PX | OA_Width | OA_HEIGHT
DEFINE OA_PX_PWIDTH_PHEIGHT     := OA_PX | OA_PWidth | OA_PHEIGHT
DEFINE OA_PY_HEIGHT             := OA_PY | OA_Height
DEFINE OA_PY_PHEIGHT            := OA_PY | OA_PHeight
DEFINE OA_PY_WIDTH          	:= OA_PY | OA_Width
DEFINE OA_PY_PWIDTH             := OA_PY | OA_PWidth
DEFINE OA_PY_WIDTH_HEIGHT       := OA_PY | OA_Width  | OA_HEIGHT
DEFINE OA_PY_WIDTH_PHEIGHT      := OA_PY | OA_Width  | OA_PHEIGHT
DEFINE OA_PY_PWIDTH_HEIGHT      := OA_PY | OA_PWidth | OA_HEIGHT
DEFINE OA_PY_PWIDTH_PHEIGHT     := OA_PY | OA_PWidth | OA_PHEIGHT
DEFINE OA_PX_PY         		:= OA_PX | OA_PY
DEFINE OA_PX_PY_HEIGHT          := OA_PX | OA_PY | OA_Height
DEFINE OA_PX_PY_PHEIGHT         := OA_PX | OA_PY | OA_PHeight
DEFINE OA_PX_PY_PWIDTH          := OA_PX | OA_PY | OA_PWidth
DEFINE OA_PX_PY_PWIDTH_PHEIGHT  := OA_PX | OA_PY | OA_PWidth | OA_PHEIGHT
DEFINE OA_PX_PY_WIDTH           := OA_PX | OA_PY | OA_Width
DEFINE OA_PX_PY_WIDTH_HEIGHT    := OA_PX | OA_PY | OA_WIDTH | OA_HEIGHT
DEFINE OA_PX_Y                  := OA_PX | OA_Y
DEFINE OA_PX_Y_PWIDTH           := OA_PX | OA_Y | OA_PWidth
DEFINE OA_PX_Y_PHEIGHT          := OA_PX | OA_Y | OA_PHeight
DEFINE OA_X_Y                   := OA_X | OA_Y
DEFINE OA_X_HEIGHT              := OA_X | OA_Height
DEFINE OA_X_PHEIGHT             := OA_X | OA_PHeight
DEFINE OA_X_WIDTH               := OA_X | OA_Width
DEFINE OA_X_PWIDTH              := OA_X | OA_PWidth
DEFINE OA_X_PY          		:= OA_X | OA_PY
DEFINE OA_X_PY_PHEIGHT          := OA_X | OA_PY | OA_PHeight
DEFINE OA_Y_PWIDTH              := OA_Y | OA_PWidth
DEFINE OA_Y_WIDTH               := OA_Y | OA_Width
DEFINE TRAY_ICON_MSG := WM_APP + 1
DEFINE TRAYTIP_LENGTH_SHELLORIGINAL := 64
DEFINE TRAYTIP_LENGTH_SHELL5 := 128
#endregion
