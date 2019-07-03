CLASS __FormFrame INHERIT ChildAppWindow
	PROTECT lAutoLayout AS LOGIC
	PROTECT lSubForm AS LOGIC
	PROTECT lSBCreateSubform AS LOGIC
	PROTECT lViewingGbrowse AS LOGIC
	PROTECT lAllowScroll AS LOGIC
	PROTECT liHPos AS LONGINT
	PROTECT liVPos AS LONGINT
	// This is the minimum size of the FormDialog. This may differ from the
	// actual size of the dialog because the size of the dialog may be increased
	// to fit the FormFrame. If the FormFrame is smaller than the Dialog then
	// ScrollBars need to be added
	PROTECT liMinX, liMinY AS LONGINT

	PROTECT oFormDlg AS __FormDialogWindow
	PROTECT oBrowser AS Control
	PROTECT oSubFrame AS FixedText  // Not used
	PROTECT oVScroll AS WindowVerticalScrollBar
	PROTECT oHScroll AS WindowHorizontalScrollBar
	PROTECT oDataWin AS DataWindow

	//PP-030828 Strong typing
	METHOD __CommandFromEvent(oEvent AS OBJECT) AS LOGIC STRICT
	//PP-030828 Strong typing


	RETURN SELF:Owner:__CommandFromEvent(oEvent)

METHOD __DoValidate(oControl AS Control) AS __FormFrame STRICT
	//PP-030828 Strong typing
	LOCAL oDW AS OBJECT



	IF !IsNil(oControl)
		oDW := oControl:Owner
		IF IsInstanceOf(oDW, #DataWindow)
			oDW:__DoValidate(oControl)
		ENDIF
	ENDIF

	RETURN SELF

METHOD __ResizeParent() AS __FormFrame STRICT
	LOCAL liXInc, liYInc, liX, liY, liNewWidth, liNewHeight AS LONGINT
	LOCAL r IS _WinRect
	LOCAL oBB AS BoundingBox
	LOCAL liBorderX, liBorderY AS LONGINT
	LOCAL hWndParent AS PTR
	LOCAL hWndApp AS PTR
	LOCAL liToolBarVOffset AS LONGINT
	LOCAL liStatusBarVOffset AS LONGINT
	LOCAL oTB AS Toolbar



	// If this form is not a subform try to resize its parent to hold the form
	IF !lSubForm //.and. (!IsLong(uResIdId) .or. !uResIdId==-1)
		hWndParent := oParent:Handle()
		hWndApp := GetParent(hWndParent) //HoParent:Owner:Handle()

		oTB := oParent:Toolbar
		IF (oTB != NULL_OBJECT) .AND. (oTB:Owner == oParent) //.and. (lIgnoreVisibility .or. IsWindowVisible(oParent:Toolbar:Handle()))
			liToolBarVOffset := oTB:Size:Height
		ENDIF

		IF (oParent:StatusBar != NULL_OBJECT) //.and. (lIgnoreVisibility .or. IsWindowVisible(oParent:Statusbar:Handle()))
			liStatusBarVOffset := oParent:StatusBar:Size:Height
		ENDIF

		GetClientRect(hWndParent, @r)

		liXInc := liMinX - r:Right
		liYInc := liMinY - r:Bottom

		GetWindowRect(hWndParent, @r)
		ScreenToClient(hWndApp, (_winPOINT PTR) @r:left)
		ScreenToClient(hWndApp, (_winPOINT PTR) @r:right)

		r:Right += liXInc
		r:Bottom += liYInc

		liNewHeight := r:Bottom - r:Top
		liNewWidth := r:Right - r:Left

		liX := r:Left
		liY := r:Top

		liNewHeight += liToolBarVOffset
		liNewHeight += liStatusBarVOffset

		IF !IsInstanceOf(oParent, #DataDialog) .AND. (hwndApp != NULL_PTR)
			GetClientRect(hWndApp, @r)

			IF (liX + liNewWidth) > r:Right
				liX := r:Right - liNewWidth
				IF liX < 0
					liX := 0
					liNewWidth := r:Right
				ENDIF
			ENDIF

			IF (liY + liNewHeight) > r:Bottom
				liY := r:Bottom - liNewHeight
				IF liY < 0
					liY := 0
					liNewHeight := r:Bottom
				ENDIF
			ENDIF
		ENDIF

		//PP-030910 from S Ebert, remove flicker next line replaces commented code below
		liBorderX := liBorderY := LONGINT(GetFrameWidth(hWnd) * 2)
		/*
		// liNewHeight += liToolBarVOffset
		// liNewHeight += liStatusBarVOffset


		GetWindowRect(hWnd, @r)
		liBorderX := r.right - r.left
		liBorderY := r.bottom - r.top
		GetClientRect(hwnd, @r)
		liBorderX -= r.right
		liBorderY -= r.bottom

		liBorderX := GetSystemMetrics(SM_CXEDGE) * 2
		liBorderY := GetSystemMetrics(SM_CYEDGE) * 2
		*/
		MoveWindow(hWndParent, liX, liY, liNewWidth+liBorderX, liNewHeight+liBorderY, TRUE)

		oBB := oParent:CanvasArea
		IF (WCGetCoordinateSystem() == WCWindowsCoordinates)
			oBB:Origin:Y += liToolBarVOffSet
			oBB:Height -= liStatusBarVOffset
		ELSE
			oBB:Origin:Y += liStatusBarVOffset
			oBB:Height -= liToolBarVOffSet
		ENDIF
		SELF:ChangeFormSize(oBB:Origin, oBB:Size)
	ENDIF

	RETURN SELF

METHOD AddControl(oControl)
	LOCAL oPoint AS Point
	LOCAL oDim AS Dimension
	LOCAL liTemp AS LONGINT
	LOCAL lResize AS LOGIC



	IF !IsInstanceOfUsual(oControl,#Control)
		WCError{#AddControl,#__FormFrame,__WCSTypeError,oControl,1}:Throw()
	ENDIF

	oPoint := oControl:Origin
	oDim := oControl:Size
	//liScrollHeight := GetSystemMetrics( SM_CXHSCROLL )
	//liScrollWidth := GetSystemMetrics( SM_CXVSCROLL )

	// if !lSubForm .or. true
	liTemp := oPoint:X + oDim:Width + IIF(lAutoLayout, INSIDEFORMBORDER, 0)
	IF liTemp > liMinX
		liMinX := liTemp// + liScrollWidth
		lResize := TRUE
	ENDIF

	liTemp := oPoint:Y + oDim:Height + IIF(lAutoLayout, INSIDEFORMBORDER, 0)
	IF liTemp > liMinY
		liMinY := liTemp// + liScrollHeight
		lResize := TRUE
	ENDIF

	IF lResize
		//oFormDlg:Size:=Dimension{liMinX,liMinY}
		SELF:ChangeFormSize(SELF:origin, SELF:size)
		// Move the control if required
		IF lAutoLayout
			oControl:Origin:=oPoint
		ENDIF
	ENDIF
	// endif

	// Set the controls TABSTOP bit
	IF lAutoLayout .AND. !IsInstanceOf(oControl, #FixedText)
		oControl:SetStyle(WS_TabStop)
	ENDIF

	RETURN SELF

ACCESS AllowScroll AS LOGIC
	RETURN lAllowScroll

ASSIGN AllowScroll (lLogic AS LOGIC)

	RETURN lAllowScroll:=lLogic

ACCESS AutoLayout

	RETURN lAutoLayout

ASSIGN AutoLayout (lLogic)

	RETURN lAutoLayout:=lLogic

// TEXTBLOCK METHOD ChangeFormSize(oPoint, oDimension) CLASS __FormFrame
// 	//PP-030910 Method has been replaced with one supplied by S Ebert to improve visual aspects (Bug 78)
// 	LOCAL liHeight, liWidth AS LONG
// 	LOCAL lWantVScroll, lWantHScroll AS LOGIC
// 	LOCAL liScrollHeight, liScrollWidth AS LONG
// 	LOCAL oDlgSize, oCanvasSize AS Dimension
// 	LOCAL oDlgPos AS Point
// 	LOCAL liDlgPosX, liDlgPosY AS LONG
// 	LOCAL liMax AS LONG
// 	LOCAL lNewVScroll AS LOGIC
// 	LOCAL liBorderX, liBorderY AS LONG

// 	IF _AND(GetWindowLong(hwnd, GWL_EXSTYLE), WS_EX_CLIENTEDGE) != 0
// 		liBorderX := GetSystemMetrics(SM_CXEDGE) * 2
// 		liBorderY := GetSystemMetrics(SM_CYEDGE) * 2
// 	ENDIF

// 	liHeight := oDimension:Height
// 	liWidth := oDimension:Width
// 	oDlgSize := Dimension{liWidth-liBorderX,liHeight-liBorderY}
// 	oCanvasSize := Dimension{liWidth-liBorderX,liHeight-liBorderY}
// 	liScrollHeight := GetSystemMetrics(SM_CXHSCROLL)
// 	liScrollWidth := GetSystemMetrics(SM_CXVSCROLL)
// 	oDlgPos := oFormDlg:Origin
// 	liDlgPosX := oDlgPos:X
// 	liDlgPosY := oDlgPos:Y

// 	IF lViewingGBrowse
// 		SELF:size := oDimension
// 		SELF:Origin := oPoint
// 		IF (oBrowser != NULL_OBJECT)
// 			Send(oBrowser, #__AutoResize)
// 		ENDIF
// 		// return self
// 	ENDIF

// 	// Change the window sizes - Also see if we need scroll bars
// 	IF (liHeight < liMinY) .AND. lAllowScroll
// 		lWantVScroll := TRUE
// 	ENDIF
// 	IF (liWidth < liMinX) .AND. lAllowScroll
// 		lWantHScroll := TRUE
// 	ENDIF

// 	// If either scroll bars are not required - check that they do not
// 	// already exist and if they do delete them
// 	IF !lWantVScroll .AND. (oVScroll != NULL_OBJECT)
// 		SELF:EnableVerticalScroll(FALSE)
// 		oVScroll := NULL_OBJECT
// 	ENDIF
// 	IF !lWantHScroll .AND. (oHScroll != NULL_OBJECT)
// 		SELF:EnableHorizontalScroll(FALSE)
// 		oHScroll := NULL_OBJECT
// 	ENDIF

// 	// Create any scrollbars required and set their range
// 	IF lWantHScroll .AND. !lViewingGBrowse
// 		IF (oHScroll == NULL_OBJECT)
// 			oHScroll := SELF:EnableHorizontalScroll()
// 			oHScroll:ThumbPosition := 0
// 			oHScroll:UnitSize := liScrollHeight
// 			oHScroll:BlockSize := liScrollHeight * 2
// 			liDlgPosY -= liScrollHeight
// 		ENDIF
// 		oCanvasSize:Height := oCanvasSize:Height - liScrollHeight
// 		liMax := liMinX - liWidth
// 		IF lWantVScroll
// 			liMax += liScrollWidth
// 		ENDIF
// 		oHScroll:Range := Range{0, liMax}
// 		oHScroll:Show()
// 	ENDIF

// 	IF lWantVScroll .AND. !lViewingGBrowse
// 		IF (oVScroll == NULL_OBJECT)
// 			oVScroll := SELF:EnableVerticalScroll()
// 			oVScroll:ThumbPosition := 0
// 			oVScroll:UnitSize := liScrollWidth
// 			oVScroll:BlockSize := liScrollWidth * 2
// 			lNewVScroll := TRUE
// 		ENDIF
// 		liMax := liMinY - liHeight
// 		IF lWantHScroll
// 			liMax += liScrollHeight
// 		ENDIF
// 		oVScroll:Range := Range{0, liMax}
// 		oVScroll:Show()
// 	ENDIF

// 	// Move and size the windows correctly
// 	//SELF:Size := Dimension{liWidth, liHeight}
// 	//SELF:Origin := oPoint
// 	// 2.5c removes flicker
// 	WCMoveWindow(SELF, oPoint, Dimension{liWidth, liHeight}, TRUE)
// 	IF (oDlgSize:Height < liMinY)
// 		oDlgSize:Height := liMinY
// 	ENDIF
// 	IF (oDlgSize:Width < liMinX)
// 		oDlgSize:Width := liMinX
// 	ENDIF

// 	IF (liDlgPosX + oDlgSize:Width) < liWidth
// 		liDlgPosX := liWidth - oDlgSize:Width
// 		IF liDlgPosX > 0
// 			liDlgPosX := 0
// 			oDlgSize:Width := liWidth
// 		ENDIF
// 	ENDIF

// 	IF (liDlgPosY + oDlgSize:Height) < oCanvasSize:Height
// 		liDlgPosY := oCanvasSize:Height - oDlgSize:Height
// 		IF liDlgPosY > 0
// 			liDlgPosY := 0
// 			oDlgSize:Height := oCanvasSize:Height
// 		ENDIF
// 	ENDIF

// 	IF lNewVScroll .AND. (liDlgPosY + oDlgSize:Height) > oCanvasSize:Height
// 		liDlgPosY := oCanvasSize:Height - oDlgSize:Height
// 	ENDIF

// 	IF liDlgPosY > 0
// 		liDlgPosY := 0
// 		oDlgSize:Height := oCanvasSize:Height
// 	ENDIF

// 	IF (oVScroll != NULL_OBJECT)
// 		oVScroll:Range := Range{0, oDlgSize:Height - oCanvasSize:Height}
// 		oVScroll:ThumbPosition := oDlgSize:Height - oCanvasSize:Height + liDlgPosY
// 	ENDIF

// 	IF (oHScroll != NULL_OBJECT)
// 		oHScroll:Range := Range{0, oDlgSize:Width - oCanvasSize:Width}
// 		oHScroll:ThumbPosition := -liDlgPosX
// 	ENDIF

// 	//oFormDlg:Size := oDlgSize
// 	//oFormDlg:Origin := Point{liDlgPosX, liDlgPosY}
// 	//2.5c removes flicker
// 	WCMoveWindow(oFormDlg, Point{liDlgPosX, liDlgPosY}, oDlgSize, TRUE)

// 	RETURN SELF

METHOD ChangeFormSize(oPt, oDim, lWinCoordinates)
	//PP-030910 Method from S Ebert
	//PP-031129 Update from S Ebert
	//PP-040322 Update from S Ebert
	//SE-070421
	LOCAL liHeight, liWidth AS LONGINT
	LOCAL lWantVScroll, lWantHScroll AS LOGIC
	LOCAL liScrollHeight, liScrollWidth AS LONGINT
	LOCAL oDlgSize, oCanvasSize AS Dimension
	LOCAL liDlgPosX, liDlgPosY AS LONGINT
	LOCAL liBorderX, liBorderY AS LONGINT
	LOCAL sRect IS _WinRect
	LOCAL oDimension  AS Dimension
	LOCAL oPoint	   AS Point
	LOCAL sPoint IS _WinPoint
	IF !IsInstanceOfUsual(oDim, #Dimension)
		WCError{#ChangeFormSize,#__FormFrame,__WCSTypeError,oDim,1}:Throw()
	ENDIF
	IF !IsInstanceOfUsual(oPt, #Point)
		WCError{#ChangeFormSize,#__FormFrame,__WCSTypeError,oPt,1}:Throw()
	ENDIF
	oDimension := oDim
	oPoint	  := oPt

	liBorderX := liBorderY := LONGINT(GetFrameWidth(hWnd) * 2)

	liHeight  := oDimension:Height
	liWidth   := oDimension:Width

	oDlgSize       := Dimension{liWidth - liBorderX, liHeight - liBorderY}
	oCanvasSize    := Dimension{liWidth - liBorderX, liHeight - liBorderY}
	liScrollHeight := GetSystemMetrics(SM_CXHSCROLL)
	liScrollWidth  := GetSystemMetrics(SM_CXVSCROLL)

	GetWindowRect(oFormDlg:Handle(), @sRect)
	sPoint:x := sRect:Left
	sPoint:y := sRect:Top
	ScreenToClient(hWnd, @sPoint)  // dcaton 070328 wass using sRect, incompatible pointers
	liDlgPosX := sPoint:x
	liDlgPosY := sPoint:y

	IF IsLogic(lWinCoordinates) .AND. lWinCoordinates
		SetWindowPos(hWnd, NULL_PTR, oPoint:X, oPoint:Y, liWidth, liHeight, _OR(SWP_NOZORDER, SWP_NOACTIVATE))
	ELSE
		WCMoveWindow(SELF, Point{oPoint:X, oPoint:Y}, oDimension, TRUE)
	ENDIF

	IF lViewingGbrowse .AND. oBrowser != NULL_OBJECT
		Send(oBrowser, #__AutoResize)
	ENDIF

	IF (oDlgSize:Height < liMinY)
		oDlgSize:Height := liMinY
	ENDIF

	IF (oDlgSize:Width < liMinX)
		oDlgSize:Width := liMinX
	ENDIF

	IF lAllowScroll
		lWantVScroll := (liHeight < liMinY)
		lWantHScroll := (liWidth  < liMinX)

		IF lWantVScroll .AND. ! lWantHScroll
			lWantHScroll := (liWidth  - liScrollWidth  < liMinX)
		ENDIF
		IF lWantHScroll .AND. ! lWantVScroll
			lWantVScroll := (liHeight - liScrollHeight < liMinY)
		ENDIF
	ELSE
		lWantVScroll := lWantHScroll := FALSE
	ENDIF

	liWidth  := oDlgSize:Width
	liHeight := oDlgSize:Height

	IF lWantHScroll
		IF ! lViewingGbrowse
			IF oHScroll == NULL_OBJECT
				oHScroll := SELF:EnableHorizontalScroll()
				oHScroll:UnitSize  := liScrollHeight
				oHScroll:BlockSize := liScrollHeight * 2
				liDlgPosX := 0
			ENDIF
		ENDIF
		oCanvasSize:Height -= liScrollHeight
		liWidth := liMinX
	ELSE
		IF oHScroll != NULL_OBJECT
			SELF:EnableHorizontalScroll(FALSE)
			oHScroll := NULL_OBJECT
		ENDIF
		liDlgPosX := 0
	ENDIF

	IF lWantVScroll
		IF ! lViewingGbrowse
			IF (oVScroll == NULL_OBJECT)
				oVScroll := SELF:EnableVerticalScroll()
				oVScroll:UnitSize  := liScrollWidth
				oVScroll:BlockSize := liScrollWidth * 2
				liDlgPosY := 0
			ENDIF
		ENDIF
		oCanvasSize:Width -= liScrollWidth
		liHeight := liMinY
	ELSE
		IF oVScroll != NULL_OBJECT
			SELF:EnableVerticalScroll(FALSE)
			oVScroll := NULL_OBJECT
		ENDIF
		liDlgPosY := 0
	ENDIF

	IF (liDlgPosX + liWidth) < oCanvasSize:Width
		liDlgPosX := oCanvasSize:Width - liWidth
	ENDIF

	IF (liDlgPosY + liHeight) < oCanvasSize:Height
		liDlgPosY := oCanvasSize:Height - liHeight
	ENDIF

	IF liDlgPosX > 0
		liDlgPosX := 0
		oDlgSize:Width  := oCanvasSize:Width
	ENDIF

	IF liDlgPosY > 0
		liDlgPosY := 0
		oDlgSize:Height := oCanvasSize:Height
	ENDIF

	IF oHScroll != NULL_OBJECT
	   //SE-070421
	   oHScroll:SetInfo(Range{0, liMinX-1}, -liDlgPosX, oCanvasSize:Width)
		//oHScroll:Range := Range{0, liMinX  - oCanvasSize:Width}
		//oHScroll:ThumbPosition := -liDlgPosX
		oHScroll:Show()
	ENDIF

	IF oVScroll != NULL_OBJECT
	   //SE-070421
	   oVScroll:SetInfo(Range{0, liMinY-1}, -liDlgPosY, oCanvasSize:Height)
		//oVScroll:Range := Range{0, liMinY - oCanvasSize:Height}
		//oVScroll:ThumbPosition := -liDlgPosY
		oVScroll:Show()
	ENDIF

	SetWindowPos(oFormDlg:Handle(), NULL_PTR, liDlgPosX, liDlgPosY, oDlgSize:Width, oDlgSize:Height, _OR(SWP_NOZORDER, SWP_NOACTIVATE))

	//PP-031129 Fix for 109 removed, cause problems sizing elsewhere
	//PP-030910 Bug 109
	// InvalidateRect(oFormDlg:Handle(),NULL,TRUE)
	// UpdateWindow(oFormDlg:Handle())

	RETURN SELF

METHOD CreateSubform(nResourceID, oResID)
	LOCAL oFocus AS OBJECT
	LOCAL oNewForm AS __FormFrame
	LOCAL oSubFormDlg AS __FormDialogWindow
	LOCAL hFocus AS PTR
	LOCAL lBorder AS LOGIC
	LOCAL hSubFrameWnd AS PTR
	LOCAL sRect IS _winRect

	// DataPages don't have an associated control and will be created with nResourceID == 0
	IF (nResourceID > 0)
		//PP-041013 Change from S Ebert
		hSubFrameWnd := GetDlgItem(oFormDlg:Handle(), nResourceID)
		lBorder := (_AND(DWORD(_CAST, WS_EX_CLIENTEDGE), DWORD(_CAST, GetWindowLong(hSubFrameWnd, GWL_EXSTYLE))) > 0U) .OR.;
			(_AND(DWORD(_CAST, WS_BORDER), DWORD(_CAST, GetWindowLong(hSubFrameWnd, GWL_STYLE))) > 0U)
	ENDIF

	// First get the window that currently has focus. If this is either
	// a form dialog or a control on one we need to send it a WM_ACTIVATE
	// message after we create the subform
	hFocus := GetFocus()
	oFocus := __WCGetWindowByHandle(hFocus)
	IF oFocus!=NULL_OBJECT
		IF !IsInstanceOf(oFocus,#__FormDialogWindow)
			oFocus:=oFocus:Owner
			IF oFocus!=NULL_OBJECT .AND. !IsInstanceOf(oFocus,#__FormDialogWindow)
				oFocus:=NULL_OBJECT
			ENDIF
		ENDIF
	ENDIF

	// Create a new form object
	oNewForm := __FormFrame{oFormDlg, oResID, TRUE, lBorder, TRUE}

	oNewForm:SBCreate := TRUE

	oSubFormDlg := oNewForm:GetDialogWindow()

	// Size and position the new form according to subdata-control
	IF hSubFrameWnd != NULL_PTR
		//PP-041013 Change from S Ebert
		GetWindowRect(hSubFrameWnd, @sRect)

#ifdef __VULCAN__
   	MapWindowPoints(NULL_PTR, oFormDlg:Handle(), (_winPOINT PTR) @sRect, 2)
#else
	   MapWindowPoints(NULL_PTR, oFormDlg:Handle(), @sRect, 2)
#endif

		oNewForm:ChangeFormSize(Point{sRect:left, sRect:top}, Dimension{sRect:right-sRect:left, sRect:bottom-sRect:top}, TRUE)
		SetWindowPos(oNewForm:Handle(), hSubFrameWnd, 0, 0, 0, 0, _OR(SWP_NOSIZE, SWP_NOMOVE, SWP_NOREDRAW))
		ShowWindow(hSubFrameWnd, SW_HIDE)
	ELSE
		oNewForm:ChangeFormSize(oSubFormDlg:Origin, oSubFormDlg:Size)
	ENDIF

	// Set the subforms parent form to this one and add the new subform
	// to it's parents list
	oSubFormDlg:SetSubformParent(oFormDlg)

	// Show the new form
	// oNewForm:Show()

	// Reactivate previously active form
	IF (oFocus != NULL_OBJECT)
		SetFocus(hFocus)
		SendMessage(oFocus:Handle(), WM_ACTIVATE, WA_ACTIVE, 0 )
	ENDIF

	RETURN oNewForm

ACCESS DataWindow


	IF oDataWin != NULL_OBJECT
		RETURN oDataWin
	ELSEIF IsInstanceOf(oParent, #DataWindow)
		RETURN oParent
	ENDIF
	RETURN NULL_OBJECT

ASSIGN DataWindow(oNewDW)


	oDataWin := oNewDW
	//if (oImp != NULL_OBJECT) .and. IsMethod(oImp, #SetOwner)
	// Send(oImp, #SetOwner, oNewDW)
	//endif

	RETURN
//
// METHOD DeleteSubform ( oFormFrame ) CLASS __FormFrame
// 	//PP-041014 Method unused, to be removed next repository build
// 	RETURN SELF

METHOD Destroy()


	IF (oFormDlg != NULL_OBJECT)
		IF (WCAppGetDialogWindow() == oFormDlg:Handle())
			WCAppSetDialogWindow(NULL_PTR)
		ENDIF
		IF !InCollect()
			oFormDlg := NULL_OBJECT
			oBrowser := NULL_OBJECT
			oSubFrame := NULL_OBJECT  // Not used
			oVScroll := NULL_OBJECT
			oHScroll := NULL_OBJECT
		ENDIF
	ENDIF
	SUPER:Destroy()

	RETURN SELF

METHOD FocusChange(oFocusChangeEvent)

	LOCAL oFCE := oFocusChangeEvent AS FocusChangeEvent
	IF oFCE:GotFocus
		SELF:SetFocusToForm()
	ENDIF

	RETURN SUPER:FocusChange(oFCE)

METHOD GetDialogWindow()


	RETURN oFormDlg

ACCESS HelpDisplay
	LOCAL oDW AS DataWindow



	oDW := SELF:DataWindow
	IF (oDW != NULL_OBJECT)
		RETURN oDW:HelpDisplay
	ENDIF

	RETURN NULL_OBJECT

METHOD HelpRequest(oHelpRequestEvent)
	LOCAL cHelpContext AS STRING


	IF IsInstanceOfUsual(oHelpRequestEvent, #HelpRequestEvent) ;
		.AND. SELF:HelpDisplay!=NULL_OBJECT;
		.AND. oHelpRequestEvent:Helptype==HELPCONTROL
		IF oHelpRequestEvent:ItemID==3244
			IF SELF:Hyperlabel!=NULL_OBJECT ;
				.and. (NULL_STRING != (cHelpContext:=SELF:Hyperlabel:HelpContext))
				SELF:HelpDisplay:Show(cHelpContext)
			ELSE
				SELF:HelpDisplay:Show("Window_WindowCanvas")
			ENDIF
		ENDIF
	ELSE
		IF (SELF:DataWindow != NULL_OBJECT)
			SELF:DataWindow:HelpRequest(oHelpRequestEvent)
			//  We need to set the event return value to prevent the from being called twice.
			SELF:EventReturnValue := SELF:DataWindow:EventReturnValue
		ELSE
			SUPER:HelpRequest(oHelpRequestEvent)
		ENDIF
	ENDIF

	RETURN NIL

METHOD HorizontalScroll(oScrollEvent)
   //SE-070421
	LOCAL oPoint AS Point
	LOCAL oSE := oScrollEvent AS ScrollEvent

	oPoint 	:= oFormDlg:Origin
	oPoint:X := -oSE:Position
	oFormDlg:Origin := oPoint
	oParent:HorizontalScroll(oSE)
	RETURN SELF

   /*
   LOCAL oPoint AS Point
	LOCAL iPos AS INT
	LOCAL iOldPos AS INT
	LOCAL oSE := oScrollEvent AS ScrollEvent

	oPoint 	:= oFormDlg:Origin
	iPos 		:= oSE:Position
	iOldPos 	:= oSE:OldPosition
	oPoint:X := oPoint:X - iPos + oSE:OldPosition
	oFormDlg:Origin := oPoint
	oParent:HorizontalScroll(oSE)
	RETURN SELF
   */

CONSTRUCTOR(oOwner, oResID, lScroll, lBorder, lIsSub)
	LOCAL uResIdId AS USUAL
	LOCAL dwSubStyle AS DWORD
	LOCAL symFDialog AS SYMBOL



	IF IsNil(lScroll)
		lAllowScroll := TRUE
	ELSE
		lAllowScroll := lScroll
	ENDIF

	IF IsNil(lIsSub)
		lSubForm := FALSE
	ELSE
		lSubForm := lIsSub
	ENDIF

	SUPER(oOwner, FALSE)

	//PP-031129 Bug 109
	SELF:SetStyle(WS_CLIPSIBLINGS,TRUE)
	SELF:SetStyle(WS_CLIPCHILDREN,FALSE)

	IF !IsNil(lBorder) .AND. lBorder
		SELF:EnableBorder(WindowNonSizingBorder)
	ENDIF

	//PP-030627
	//PP-030505
	symFDialog := oOwner:symFormDialog
	IF symFDialog == NULL_SYMBOL
		symFDialog := #__FormDialogWindow
	ENDIF
	uResIdId:=oResID:ID
	IF IsLong(uResIdId) .AND. (uResIdId == -1)
		oFormDlg := CreateInstance(symFDialog,SELF, ResourceID{"IDD_DEFDLG", _GetInst()})
	ELSE
		oFormDlg := CreateInstance(symFDialog,SELF, oResId)
	ENDIF

	// !!!!
	dwSubStyle := DWORD(_CAST, GetWindowLong(oFormDlg:Handle(), GWL_STYLE))
	IF (_AND(dwSubStyle, DWORD(_CAST, DS_CONTROL)) > 0)
		dwSubStyle := DWORD(GetWindowLong(SELF:Handle(), GWL_EXSTYLE))
		SetWindowLong(SELF:Handle(), GWL_EXSTYLE, LONGINT(_CAST, _OR(dwSubStyle, DWORD(_CAST, WS_EX_CONTROLPARENT))))

		IF IsInstanceOf(oOwner, #__FormDialogWindow)
			dwSubStyle := DWORD(GetWindowLong(oOwner:Handle(), GWL_EXSTYLE))
			SetWindowLong(oOwner:Handle(), GWL_EXSTYLE, LONGINT(_CAST, _OR(dwSubStyle, DWORD(_CAST, WS_EX_CONTROLPARENT))))
		ENDIF
	ENDIF
	// !!!

	oFormDlg:Origin := Point{0,0}
	oFormDlg:Show()

	liMinX := Max(oFormDlg:Size:Width, IIF(!lSubForm, MIN_DATAWINDOWWIDTH, 0))
	liMinY := oFormDlg:Size:Height

	SELF:__ResizeParent()
	RETURN


ACCESS IsSubform

	RETURN lSubform

ASSIGN IsSubform(l)


	RETURN lSubform:=l

METHOD MouseDrag(oEvent)
	LOCAL oObject AS OBJECT



	oObject:= oParent
	//oObject:MouseDrag(__ObjectCastClassPtr(oEvent, __pCMouseEvent))
	oObject:MouseDrag(MouseEvent{oEvent})

	//	oParent:Owner:Dispatch(oEvent)
	RETURN SELF

ACCESS Owner


	IF SELF:dataWindow != NULL_OBJECT
		RETURN SELF:DataWindow
	ENDIF
	RETURN SUPER:Owner

METHOD ResetMinSize()
	liMinX := IIF(!lSubForm, MIN_DATAWINDOWWIDTH, 0)
	liMinY := 2
	RETURN SELF

ACCESS SBCreate


	RETURN lSBCreateSubform

ASSIGN SBCreate(lSBCreate)


	RETURN lSBCreateSubform:=lSBCreate

METHOD SetFocusToForm ( )
	LOCAL hWndCtrl AS PTR


	// If focus is not returning to any specific control then set
	// to the last active control (if any)
	IF (oBrowser != NULL_OBJECT) .AND. lViewingGBrowse
		SetFocus(oBrowser:Handle())
		RETURN SELF
	ENDIF

	// Send WM_ACTIVATE message to dialog to set the currently active dialog
	IF (oFormDlg == NULL_OBJECT)
		RETURN SELF
	ENDIF
	//oFormDlg:SetFocus()

	hWndCtrl := GetFocus()
	IF (hWndCtrl == NULL_PTR) .OR. (GetParent(hWndCtrl) != oFormDlg:Handle())
		IF !oFormDlg:SetFocusToPrev()
			oFormDlg:SetFocus()
		ENDIF
	ENDIF

	RETURN SELF

METHOD SetGBrowse(oDataBrowser)
	//PP-040515 Update S.Ebert


	oBrowser := oDataBrowser
	IF oContextMenu != NULL_OBJECT
		oBrowser:ContextMenu := oContextMenu
	ENDIF
	RETURN SELF

METHOD VerticalScroll (oScrollEvent)
   //SE-070421
	LOCAL oPoint AS Point
	LOCAL oSE := oScrollEvent AS ScrollEvent

	oPoint := oFormDlg:Origin

	SetWindowPos(oFormDlg:Handle(), NULL_PTR, oPoint:X, -oSE:Position, 0, 0, _OR(SWP_NOSIZE, SWP_NOZORDER, SWP_NOACTIVATE))

	oParent:VerticalScroll(oSE)

	RETURN SELF

/*
	LOCAL oPoint AS Point
	LOCAL iPos AS INT
	LOCAL iOldPos AS INT
	LOCAL oSE := oScrollEvent AS ScrollEvent

	oPoint 		:= oFormDlg:Origin
	iPos 			:= oSE:Position
	iOldPos 		:= oSE:OldPosition
	//RvdH 070321 Take Coordinate system into account.
	IF WCGetCoordinateSystem()
		oPoint:Y 	:= oPoint:Y + iPos - oSE:OldPosition
	ELSE
		oPoint:Y 	:= oPoint:Y - (iPos - oSE:OldPosition)
	ENDIF
	oFormDlg:Origin := oPoint

	oParent:VerticalScroll(oSE)

	RETURN SELF
*/

METHOD ViewAs(lBrowse)
	LOCAL liTemp AS LONGINT

	// If we want to view the GBRowse
	IF lBrowse
		IF (oBrowser== NULL_OBJECT) //.or. lViewingGBrowse
			RETURN SELF
		ENDIF
		liHPos := -1
		liVPos := -1
		// Remove the scroll bars if they are there - Store the
		// Thumb position first so we can restore it later
		IF oVScroll!=NULL_OBJECT
			liVPos := oVScroll:ThumbPosition
			SELF:EnableVerticalScroll(FALSE)
			oVScroll := NULL_OBJECT
		ENDIF
		IF oHScroll!=NULL_OBJECT
			liHPos := oHScroll:ThumbPosition
			SELF:EnableHorizontalScroll(FALSE)
			oHScroll := NULL_OBJECT
		ENDIF
		// hWinPos := BeginDeferWindowPos(8)
		Send(oBrowser, #__AutoResize)
		oFormDlg:Hide()
		oBrowser:Show()
		// BringWindowToTop(oBrowser:Handle())

		// EndDeferWindowPos(hWinPos)
		lViewingGBrowse := TRUE
	ELSE
		//		hWinPos := BeginDeferWindowPos( 8 )
		// Reenable the scroll bars if required
		IF liHPos != -1
			oHScroll := SELF:EnableHorizontalScroll(TRUE)
			liTemp := GetSystemMetrics( SM_CYHSCROLL )
			oHScroll:UnitSize := liTemp
			oHScroll:BlockSize := liTemp * 2
			oHScroll:Show()
			oHScroll:ThumbPosition := liHPos
			liHPos := oHScroll:ThumbPosition
		ENDIF
		IF liVPos != -1
			oVScroll := SELF:EnableVerticalScroll(TRUE)
			oVScroll:ThumbPosition := liVPos
			liTemp := GetSystemMetrics( SM_CXVSCROLL )
			oVScroll:UnitSize := liTemp
			oVScroll:BlockSize := liTemp * 2
			oVScroll:Show()
		ENDIF
		lViewingGBrowse := FALSE
		// Show the dialog and hide the GBRowse
		IF (oBrowser != NULL_OBJECT)
			oBrowser:Hide()
		ENDIF
		oFormDlg:Show()
		// BringWindowToTop(oFormDlg:Handle())
		// Resize the dialog in case the user has resized the window
		// while the dialog was out of view
		SELF:ChangeFormSize( SELF:Origin, SELF:Size )
		//		EndDeferWindowPos( hWinPos )
	ENDIF

	RETURN SELF

END CLASS

FUNCTION GetFrameWidth(hWnd AS PTR) AS DWORD STRICT
	LOCAL sRect IS _WinRect
	LOCAL dwStyle, dwExStyle AS DWORD

	IF (hWnd == NULL_PTR)
		RETURN 0l
	ENDIF

	SetRect(@sRect,10,10,20,20)
	dwStyle   := DWORD(_CAST,GetWindowLong(hWnd, GWL_STYLE))
	dwExStyle := DWORD(_CAST,GetWindowLong(hWnd, GWL_EXSTYLE))

	AdjustWindowRectEx(@sRect, dwStyle, FALSE, dwExStyle)

	RETURN DWORD(10l - sRect:Left)

	// Calculates the width of the windowframe depending of the windowstyles.



#region defines
DEFINE INSIDEFORMBORDER := 5
DEFINE MIN_DATAWINDOWWIDTH := 50
#endregion
