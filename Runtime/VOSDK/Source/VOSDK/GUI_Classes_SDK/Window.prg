CLASS __ForeignWindow INHERIT Window
	
CONSTRUCTOR(hwndSelf)
	
#ifdef __VULCAN__
   SUPER( NULL )
   // All classes MUST call their parent constructor in .NET.
   // Since finalizer registration is implicit, we call SuppressFinalize() here
   // which has the same end result as not calling SUPER:INIT in VO.
   System.GC.SuppressFinalize( SELF )
#endif  
	
	// don't call super:init /RegisterAxit -> we're not responsible for destroying this window
	//MessageBox(0, "_Foreign_Init", "", 0)
	hWnd := hwndSelf
	RETURN 
	
	
END CLASS

PARTIAL CLASS Window INHERIT @@EventContext
	PROTECT oParent AS OBJECT
	PROTECT hWnd AS PTR
	PROTECT dwStyle AS DWORD
	
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
	PROTECT oCursor AS Cursor
	PROTECT oDragDropClient AS DragDropClient
	PROTECT oDragDropServer AS DragDropServer
	PROTECT oToolBar AS ToolBar
	PROTECT strucPaintRect AS _WINRECT
	
	PROTECT DCInitialized AS LOGIC
	PROTECT DCPenInUse AS LOGIC
	PROTECT DCPenNeeded AS LOGIC
	PROTECT DCBrushInUse AS LOGIC
	PROTECT DCBrushNeeded AS LOGIC
	PROTECT DCFontInUse AS LOGIC
	PROTECT DCFontNeeded AS LOGIC
	PROTECT hDC AS PTR
	PROTECT hDCPaint AS PTR
	
	PROTECT oCurrentHelp AS HelpDisplay
	PROTECT lHelpOn AS LOGIC
	PROTECT lHelpCursorOn AS LOGIC
	PROTECT lHelpMenu AS LOGIC
	PROTECT lFileDragging AS LOGIC
	PROTECT dwTimerCount AS INT
	PROTECT dwTimerInterval AS INT
	PROTECT lTimerRegistered AS LOGIC
	
	PROTECT lDragActive AS LOGIC
	PROTECT oDragImageList AS ImageList
	//Liuho01 05-15-96 for DragDropServer
	PROTECT hDragSingleCursor AS PTR
	PROTECT hDragMultipCursor AS PTR
	PROTECT hwndToolTip AS PTR
	PROTECT aAlignes AS ARRAY
	PROTECT lAutomated AS LOGIC
	PROTECT oMinSize AS Dimension
	
	EXPORT EventReturnValue AS LONGINT
	
	//PP-030828 Strong typing
	//RvdH 041123 Typing of OLE methods
	METHOD __AddAlign(oControl AS OBJECT, iType AS USUAL) AS Window STRICT 
	//PP-030828 Strong typing
	//PP-040513 Update from S Ebert
    //SE-070920 Update for Factor/Divisor-Mode and bugfix for overwriting OwnerAlignments
    LOCAL dwI, dwCount AS DWORD
    LOCAL sRect        IS _WINRECT
    LOCAL lDelete      AS LOGIC
    LOCAL lOldAlign    AS LOGIC

    Default(@iType, OA_TOP) 
    
    lOldAlign := ! IsPtr(iType) .AND. iType <= OA_FULL_SIZE
    
    //PP-031129 Additional owner alignment options
    IF ! lOldAlign
        //Null_Object calls SetAlignStartSize() in init mode,
        //means that only the first call sets the AlignStartSize.
        SELF:SetAlignStartSize(NULL_OBJECT)
    ENDIF
    
    lDelete := (IsLong(iType) .AND. iType = OA_NO)
    
    //SE-060525
    dwCount := ALen(aAlignes)
    FOR dwI := 1 UPTO dwCount
        IF aAlignes[dwI, 1] == oControl
            IF lDelete
                ADel(aAlignes, dwI)
                ASize(aAlignes, dwCount-1)
            ELSE
                aAlignes[dwI, 2] := iType
            ENDIF
            dwI := 0
            EXIT //SE-070919 bug fix
        ENDIF
    NEXT //dwI
    
    //SE-070920 
	                  
	IF dwI > dwCount
		AAdd(aAlignes, NIL)
	
	   GetWindowRect(oControl:Handle(), @sRect)
#ifdef __VULCAN__	
	   MapWindowPoints(NULL_PTR, SELF:Handle(4), (_winPOINT PTR) @sRect, 2)
#else
	   MapWindowPoints(NULL_PTR, SELF:Handle(4), @sRect, 2)
#endif	
   	
	   aAlignes[dwI] := {oControl, iType, sRect:left, sRect:top, sRect:right-sRect:left, sRect:bottom-sRect:top}
	ENDIF
	
	//SE-070430
    IF lOldAlign
	   SELF:__AlignControls()
	ENDIF
	
	RETURN SELF
	

METHOD __AddTool(oControl AS Control, oParent AS USUAL) AS LOGIC STRICT 
	//PP-030828 Strong typing
	LOCAL ti IS _winTOOLINFO
	LOCAL hChild, hSubChild AS PTR
	LOCAL lRet AS LOGIC
	
	
	
	Default(@oParent, SELF)
	SELF:EnableToolTips(TRUE)
	
	MemClear(@ti, _SIZEOF(_winTOOLINFO))
	ti:cbSize := _SIZEOF(_winTOOLINFO)
	//RvdH 070717 Fix Tooltip problem on control windows
	ti:uFlags := TTF_IDISHWND | TTF_SUBCLASS  
	ti:hwnd := oParent:Handle()
	ti:uId := DWORD(_CAST, oControl:Handle())
	ti:lpszText := LPSTR_TEXTCALLBACK
	
	SendMessage(hwndToolTip, TTM_DELTOOL, 0, LONGINT(_CAST, @ti))
	lRet := LOGIC(_CAST, SendMessage(hwndToolTip, TTM_ADDTOOL, 0, LONGINT(_CAST, @ti)))
	
	//RvdH 050415 Added handling for OleObjects & OleControls
	IF IsInstanceOfUsual(oControl, #Combobox) ;
			.OR. IsInstanceOfUsual(oControl, #IPAddress) ;
			.OR. IsInstanceOfUsual(oControl, #OleObject)
		ti:uFlags := _OR(TTF_IDISHWND, TTF_SUBCLASS)
		hChild := GetWindow(oControl:Handle(), GW_CHILD)
		WHILE (hChild != NULL_PTR)
			
			ti:uID := DWORD(_CAST, hChild)
			SendMessage(hwndToolTip, TTM_DELTOOL, 0, LONGINT(_CAST, @ti))
			SendMessage(hwndToolTip, TTM_ADDTOOL, 0, LONGINT(_CAST, @ti))
			hSubChild := GetWindow(hChild, GW_CHILD)
			IF hSubChild != NULL_PTR
				ti:uID := DWORD(_CAST, hSubChild)
				SendMessage(hwndToolTip, TTM_DELTOOL, 0, LONGINT(_CAST, @ti))
				SendMessage(hwndToolTip, TTM_ADDTOOL, 0, LONGINT(_CAST, @ti))
			ENDIF
			hChild := GetWindow(hChild, GW_HWNDNEXT)
		END
	ENDIF
	
	RETURN lRet
	

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
	LOCAL sRect      IS _winRECT
    LOCAL hCtl       AS PTR 
    LOCAL uType      AS USUAL
    LOCAL dwType     AS DWORD
    LOCAL liMulDiv   AS LONG
    LOCAL oOwnerSize AS Dimension
	
	LOCAL iCtlX,     iCtlY      AS INT
	LOCAL iCtlWidth, iCtlHeight AS INT
    LOCAL iCtlRefWidth          AS INT
    LOCAL iCtlRefHeight         AS INT	
	LOCAL iWidth,    iHeight    AS INT
	LOCAL iRefWidth, iRefHeight AS INT
    LOCAL pB                    AS BYTE PTR
	
	LOCAL dwI, dwCount, uFlags  AS DWORD
	LOCAL oResize    AS OBJECT
	
	dwCount := ALen(aAlignes)
	
	IF dwCount < 1 .OR. hWnd=NULL_PTR .OR. IsIconic(hWnd)
		RETURN  SELF
	ENDIF
	
	GetClientRect(hWnd, @sRect)
	iWidth  := sRect:right
	iHeight := sRect:bottom
	
	IF aAlignes[1,1] == NULL_OBJECT
		oOwnerSize := aAlignes[1,2]
		IF oOwnerSize != NULL_OBJECT
			iRefWidth  := oOwnerSize:Width
			iRefHeight := oOwnerSize:Height
		ELSE
			iRefWidth  := iWidth
			iRefHeight := iHeight
		ENDIF
		dwI := 2
	ELSE
		dwI := 1
	ENDIF
	
	uFlags := _OR(SWP_NOACTIVATE, SWP_NOZORDER, SWP_NOCOPYBITS)
	
	DO WHILE dwI <= dwCount
		oResize := aAlignes[dwI][1]
		hCtl    := oResize:HANDLE()
      uType   := aAlignes[dwI][2]
      dwType  := DWORD(_CAST, uType)
		IF IsWindow(hCtl)
         IF IsPtr(uType) .OR. dwType > OA_FULL_SIZE
				iCtlX      := aAlignes[dwI][3]
				iCtlY      := aAlignes[dwI][4]
            iCtlWidth  := iCtlRefWidth  := aAlignes[dwI][5]
            iCtlHeight := iCtlRefHeight := aAlignes[dwI][6]  
				
				    IF IsPtr(uType) //SE-070920 Factor/Divisor-Mode
                    pB := (BYTE PTR) @dwType
                    IF (liMulDiv := pB[1]) > 0
                        iCtlX += MulDiv(iWidth - iRefWidth, liMulDiv>>4, _AND(liMulDiv, 0xF))
                    ENDIF
                    IF (liMulDiv := pB[2]) > 0
                        iCtlY += MulDiv(iHeight - iRefHeight, liMulDiv>>4, _AND(liMulDiv, 0xF))
                    ENDIF 
                    IF (liMulDiv := pB[3]) > 0
                        iCtlWidth += MulDiv(iWidth - iRefWidth, liMulDiv>>4, _AND(liMulDiv, 0xF))
                    ENDIF 
                    IF (liMulDiv := pB[4]) > 0
                        iCtlHeight += MulDiv(iHeight - iRefHeight, liMulDiv>>4, _AND(liMulDiv, 0xF))
                    ENDIF 
                ELSE               
                    IF _AND(dwType, OA_WIDTH) = OA_WIDTH
                        IF _AND(dwType, OA_PWIDTH) = OA_PWIDTH
                            iCtlWidth := MulDiv(iWidth, iCtlRefWidth, iRefWidth)
                        ELSE 
                            iCtlWidth += iWidth - iRefWidth
                        ENDIF
                    ENDIF   
                    
                    IF _AND(dwType, OA_HEIGHT) = OA_HEIGHT
                        IF _AND(dwType, OA_PHEIGHT) = OA_PHEIGHT
                            iCtlHeight := MulDiv(iHeight, iCtlRefHeight, iRefHeight)
                        ELSE  
                            iCtlHeight += iHeight - iRefHeight
                        ENDIF
                    ENDIF
                    
                    IF _AND(dwType, OA_X) = OA_X
                        IF _AND(dwType, OA_PX) = OA_PX
                            IF _AND(dwType, OA_PWIDTH) = OA_PWIDTH
                                iCtlX := MulDiv(iWidth, iCtlX, iRefWidth) 
                            ELSE 
                                //SE-070919 for enhanced proportional alignment
                                iCtlX := MulDiv(iWidth, iCtlX + iCtlRefWidth/2, iRefWidth) - iCtlWidth/2
                            ENDIF   
                        ELSE 
                            iCtlX += iWidth - iRefWidth
                        ENDIF
                    ENDIF  
                    
                    IF _AND(dwType, OA_Y) = OA_Y
                        IF _AND(dwType, OA_PY) = OA_PY
                            IF _AND(dwType, OA_PHEIGHT) = OA_PHEIGHT
                                iCtlY := MulDiv(iHeight, iCtlY, iRefHeight)
                            ELSE
                                //SE-070919 for enhanced proportional alignment
                                iCtlY := MulDiv(iHeight, iCtlY + iCtlRefHeight/2, iRefHeight) - iCtlHeight/2
                            ENDIF
                        ELSE 
                            iCtlY += iHeight - iRefHeight
                        ENDIF
                    ENDIF
                ENDIF   

				IF IsInstanceOf(oResize, #Window)
					IF IsInstanceOf(oResize, #__FormFrame)
						oResize:ChangeFormSize( Point{iCtlX, iCtlY}, Dimension{iCtlWidth, iCtlHeight}, TRUE)
					ELSE
						SetWindowPos(hCtl, NULL_PTR, iCtlX, iCtlY, iCtlWidth, iCtlHeight, uFlags)
					ENDIF
				ELSE
					SetWindowPos(hCtl, NULL_PTR, iCtlX, iCtlY, iCtlWidth, iCtlHeight, uFlags)
					IF IsWindowVisible(hCtl) .AND. ! IsInstanceOf(oResize, #GroupBox)
						RedrawWindow(hCtl,NULL_PTR,NULL_PTR,_OR(RDW_INVALIDATE,RDW_NOERASE,RDW_UPDATENOW,RDW_FRAME))
					ENDIF
				ENDIF
				
			ELSE
				GetWindowRect(hCtl, @sRect)
#ifdef __VULCAN__				
				MapWindowPoints(NULL, hWnd, (_winPOINT PTR) @sRect, 2)
#else				
				MapWindowPoints(NULL, hWnd, @sRect, 2)
#endif				
				iCtlWidth  := sRect:right  - sRect:left
				iCtlHeight := sRect:bottom - sRect:top
				DO CASE
             CASE (dwType == OA_TOP)
                 SetWindowPos(hCtl, NULL_PTR, sRect:left, 0, iCtlWidth, iCtlHeight, uFlags)
             CASE (dwType == OA_LEFT)
                 SetWindowPos(hCtl, NULL_PTR, 0, sRect:top, iCtlWidth, iCtlHeight, uFlags)
             CASE (dwType == OA_BOTTOM)
                 SetWindowPos(hCtl, NULL_PTR, sRect:left, iHeight-iCtlHeight, iCtlWidth, iCtlHeight, uFlags)
             CASE (dwType == OA_RIGHT)
                 SetWindowPos(hCtl, NULL_PTR, iWidth-iCtlWidth, sRect:top, iCtlWidth, iCtlHeight, uFlags)
             CASE (dwType == OA_TOP_AUTOSIZE)
                 SetWindowPos(hCtl, NULL_PTR, 0, 0, iWidth, iCtlHeight, uFlags)
             CASE (dwType == OA_LEFT_AUTOSIZE)
                 SetWindowPos(hCtl, NULL_PTR, 0, 0, iCtlWidth, iHeight, uFlags)
             CASE (dwType == OA_BOTTOM_AUTOSIZE)
                 SetWindowPos(hCtl, NULL_PTR, 0, iHeight-iCtlHeight, iWidth, iCtlHeight, uFlags)
             CASE (dwType == OA_RIGHT_AUTOSIZE)
                 SetWindowPos(hCtl, NULL_PTR, iWidth-iCtlWidth, 0, iCtlWidth, iHeight, uFlags)
             CASE (dwType == OA_CENTER)
                 SetWindowPos(hCtl, NULL_PTR, (iWidth / 2)- (iCtlWidth / 2), (iHeight / 2)- (iCtlHeight / 2), 0, 0, _OR(uFlags, SWP_NOSIZE))
             CASE (dwType == OA_FULL_SIZE)
                 SetWindowPos(hCtl, NULL_PTR, 0, 0, iWidth, iHeight, uFlags)
             ENDCASE
			ENDIF
		ENDIF
		++dwI
	ENDDO
	RETURN SELF
	

METHOD __AssociateAccel(lSwitch AS LOGIC) AS Window STRICT 
	//PP-030828 Strong typing
	
	
	IF lSwitch .AND. (oAccelerator != NULL_OBJECT)
		SetAccelerator(hWnd, oAccelerator:Handle())
	ELSE //if !lswitch
		SetAccelerator(NULL_PTR, NULL_PTR)
	ENDIF
	
	RETURN SELF
	

METHOD __Close(oEvent AS @@Event) AS VOID STRICT 
	//PP-030828 Strong typing
	
	
	SELF:Close(oEvent)
	SELF:Destroy()
	RETURN
	

METHOD __CommandFromEvent(oEvent AS OBJECT) AS LOGIC STRICT 
	//PP-030828 Strong typing
	LOCAL symNameSym AS SYMBOL
	LOCAL oWindow AS Window
	LOCAL oReport AS OBJECT
	LOCAL o AS OBJECT
	
	
	
	symNameSym := oEvent:NameSym
	oWindow := SELF
	
	
	DO WHILE TRUE
		//RvdH 050602 Added oEvent parameter
		//RvdH 050714 And removed again. Too many complaints from VOPS subscribers
		IF IsMethod(oWindow, symNameSym)
			//Send(oWindow, symNameSym, oEvent)
			Send(oWindow, symNameSym)
			RETURN TRUE
		ENDIF
		IF IsInstanceOfUsual(oWindow:Owner, #Window)
			oWindow := oWindow:Owner
		ELSE
			EXIT
		ENDIF
	ENDDO
	
	IF IsClassOf(symNameSym, #Window)
		IF IsInstanceOf(SELF, #ChildAppWindow)
			oWindow:=SELF
			DO WHILE IsInstanceOf(oWindow:Owner, #Window)
				oWindow:=oWindow:Owner
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
			oReport:Show()
		ENDIF
		RETURN TRUE
	ENDIF
	
	RETURN FALSE
	

METHOD __CreateSelfBitmap() AS PTR STRICT 
	//PP-030828 Strong typing
	LOCAL hBitmap AS PTR
	LOCAL hBitmapOld AS PTR
	LOCAL hDIB AS PTR
	LOCAL _hDC AS PTR
	LOCAL hMemDC AS PTR
	LOCAL rc IS _winRECT
	LOCAL x, y AS INT
	
	
	
	_hDC := GetWindowDC(hWnd)
	
	IF (_hDC != NULL_PTR)
		hMemDC := CreateCompatibleDC(hDC)
		IF (hMemDC != NULL_PTR)
			GetWindowRect(hWnd, @rc)
			x := rc:right - rc:left
			y := rc:bottom - rc:top
			
			hBitmap := CreateCompatibleBitmap(_hDC, x, y)
			
			IF (hBitmap != NULL_PTR)
				hBitmapOld := SelectObject(hMemDC, hBitmap)
				PatBlt(hMemDC, 0, 0, x, y, WHITENESS)
				BitBlt(hMemDC, 0, 0, x, y, _hDC, 0, 0, SRCCOPY)
				SelectObject(hMemDC, hBitmapOld)
			ENDIF
			
			DeleteDC(hMemDC)
		ENDIF
		
		ReleaseDC(hWnd, _hDC)
	ENDIF
	
	IF (hBitmap != NULL_PTR)
		hDIB := __WCDIBFromBitmap(hBitmap)
		DeleteObject(hBitmap)
	ENDIF
	
	RETURN hDIB
	

ACCESS __Cursor AS Cursor STRICT 
	//PP-030828 Strong typing
	
	
	RETURN oCursor
	

ASSIGN __Cursor(oNewCursor AS Cursor)  STRICT 
	//PP-030828 Strong typing
	
	
	RETURN oCursor:=oNewCursor
	

METHOD __DestroyChildren() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL hChild AS PTR
	LOCAL cObj AS OBJECT
	
	
	
	IF IsInstanceOf(SELF, #SHELLWINDOW)
		hChild := GetWindow(SELF:Handle(4), GW_CHILD)
	ELSE
		hChild := GetWindow(SELF:Handle(), GW_CHILD)
	ENDIF
	
	DO WHILE (hChild != NULL_PTR)
		cObj :=__WCGetObjectByHandle(hChild) 
		
		//IF (cObj == NULL_OBJECT)
		//	cObj := __WCGetWindowByHandle(hChild) //Not a control, try windows
			IF (IsInstanceOf(cObj, #__WindApp) .OR. IsInstanceOf(cObj, #__DocApp))
				cObj := cObj:Owner
			ENDIF
		//ENDIF
		
		hchild := GetWindow(hchild, GW_HWNDNEXT)
		
		IF (cObj != NULL_OBJECT) .AND. !IsInstanceOf(cObj, #Menu)
			// Change for 2.5a. Watch for Problems!!!
			IF IsMethod(cObj, #Close)
				cObj:Close()
			ENDIF
			cObj:Destroy()
		ENDIF
	ENDDO
	RETURN
	

METHOD __DestroyWnd() AS LOGIC STRICT 
	//PP-030828 Strong typing
	
	
	IF (hwnd != NULL_PTR) .AND. IsWindow(hwnd)
		RETURN DestroyWindow(hwnd)
	ENDIF
	
	RETURN TRUE
	

METHOD __EnableHelpCursor(lEnabled AS LOGIC) AS Window STRICT 
	//PP-030828 Strong typing
	LOCAL strucPoint IS _WinPoint
	LOCAL x, y AS LONGINT
	LOCAL liHitArea AS LONGINT
	LOCAL hWndCursor AS PTR
	LOCAL hWndChildCursor AS PTR
	
	
	
	// lHelpMenu := lEnabled
	IF lEnabled
		IF !lHelpCursorOn
			lHelpCursorOn := TRUE
			IF (oApp != NULL_OBJECT)
				SetCursor(oApp:__HelpCursor)
			ENDIF
			ShowCursor(TRUE)
			IF (GetActiveWindow() == hWnd) .AND. (oApp != NULL_OBJECT)
				oApp:__SetHelpWind(hWnd, HM_MOUSE)
			ENDIF
		ENDIF
		// elseif lHelpCursorOn
	ELSE
		lHelpCursorOn := FALSE
		IF IsWindowVisible(hWnd) .AND. (GetCapture() == 0)
			GetCursorPos(@strucPoint)
			
			// Locate Window or ChildWindow currently containing cursor.
			// This is needed for passing to WM_SETCURSOR
			x := strucPoint:x
			y := strucPoint:y
			hWndCursor := WindowFromPoint( x, y ) // TopAppWindow containing point.
			strucPoint:x := x
			strucPoint:y := y
			hWndChildCursor := ChildWindowFromPoint(hWndCursor, x, y)
			strucPoint:x := x
			strucPoint:y := y
			IF hWndChildCursor != 0 .AND. IsWindowVisible(hWndChildCursor)
				hWndCursor := hWndChildCursor
			ENDIF
			liHitArea := SendMessage(hWnd, WM_NCHITTEST, 0, LONGINT(_CAST,@strucPoint))
			SendMessage(hWnd, WM_SETCURSOR, DWORD(_CAST,hWndCursor), _OR(liHitArea,LONGINT(WM_MOUSEMOVE) << 16))
		ENDIF
		IF (GetActiveWindow() == hWnd) .AND. (oApp != NULL_OBJECT)//was commented out ??? Liuho01 03-29-96 ???
			oApp:__SetHelpWind(hWnd, HM_GENERAL)
		ENDIF
	ENDIF
	RETURN SELF
	

METHOD __FilterHelpCursor(wArea AS LONGINT) AS LOGIC STRICT 
	//PP-030828 Strong typing
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
	
	

METHOD __GetDC() AS PTR STRICT 
	//PP-030828 Strong typing
	LOCAL hFont AS PTR
	LOCAL strucLogPen IS _WinLogPen
	LOCAL strucLogBrush IS _WinLogBrush
	LOCAL r IS _WinRECT
	
	
	
	IF SELF:Handle(4) == NULL_PTR
		//PaintInfo = 0
		RETURN NULL_PTR
	ENDIF
	
	IF (hDC == NULL_PTR)
		WCDCAdd(SELF)
		SELF:hDC := GetDC(SELF:Handle(4))
		DCInitialized := DCFontInUse := DCPenInUse := DCBrushInUse := FALSE
		// else
		// WCDCTop(self)
	ENDIF
	
	IF (hDC != NULL_PTR)
		IF !DCInitialized
			IF (WCGetCoordinateSystem() == WCCartesianCoordinates)
				SetMapMode(hDC, MM_TEXT)
				SetMapMode(hDC, MM_ANISOTROPIC)
				
				GetClientRect(SELF:Handle(4), @r)
				SetViewportExtEx(hDC, r:right, r:bottom, NULL_PTR) // device coords
				SetWindowExtEx(hDC, r:right, -r:bottom, NULL_PTR) // logical coords used by GDI
				SetViewportOrgEx(hDC, 0, r:bottom-1, NULL_PTR)
			ENDIF
			
			DCInitialized := TRUE
			__WCLogicalBackgroundBrush(SELF,@strucLogBrush)
			SetBkColor(hDC, strucLogBrush:lbColor)
			IF strucLogBrush:lbStyle == BS_HOLLOW
				SetBkMode(hDC, TRANSPARENT)
			ELSE
				SetBkMode(hDC, OPAQUE)
			ENDIF
		ENDIF
		
		IF DCFontNeeded .AND. !DCFontInUse
			IF oFont != NULL_OBJECT
				oFont:Create(FALSE, hDC)
				hFont := oFont:Handle()
			ELSE
				hFont := GetStockObject(System_Font)
			ENDIF
			SelectObject (hDC, hFont)
			DCFontInUse := TRUE
			DCFontNeeded := FALSE
			SetTextAlign(hDC, _OR(TA_LEFT,TA_BOTTOM))
		ENDIF
		
		IF DCPenNeeded .AND. !DCPenInUse
			IF oPen != NULL_OBJECT
				SelectObject ( hDC, oPen:Handle())
				__WCLogicalPen(oPen, @strucLogPen)
				SetTextColor(hDC, strucLogPen:lopnColor)
			ELSE
				// Stock Object BLACK_PEN is default pen
				SelectObject(hDC, GetStockObject(BLACK_PEN))
				SetTextColor(hDC, GetSysColor(COLOR_WINDOWTEXT))
			ENDIF
			DCPenInUse := TRUE
			DCPenNeeded := FALSE
		ENDIF
		
		IF DCBrushNeeded .AND. !DCBrushInUse
			IF oForeground != NULL_OBJECT
				SelectObject(hDC, oForeground:Handle())
			ELSE
				// Stock Object Black_Brush is default brush
				SelectObject(hDC, GetStockObject(BLACK_BRUSH))
			ENDIF
			DCBrushInUse := TRUE
			DCBrushNeeded := FALSE
		ENDIF
	ENDIF
	
	RETURN hDC
	

METHOD __GetMyOleObjects AS ARRAY STRICT 
	//RvdH 041123 Added to get array of children of type OleObject
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
	
	

METHOD __GetPaintRect() AS _WINRECT STRICT 
	//PP-030828 Strong typing
	
	
	RETURN strucPaintRect
	
	

METHOD __HandleListItemDrag(oEvent AS @@Event) AS Window STRICT 
	//PP-030828 Strong typing
	LOCAL dli AS _winDRAGLISTINFO
	STATIC LOCAL iBeginDragItem AS INT
	LOCAL iCurItem AS INT
	LOCAL sItemText AS STRING
	LOCAL uItemVal AS USUAL
	LOCAL oLB AS ListBox
	//RvdH 070713 Fixed ItemDrag problem
	dli := PTR(_CAST, oEvent:lParam)
	DO CASE
	CASE (dli:uNotification == DL_BEGINDRAG) 
		iBeginDragItem := LBItemFromPt(dli:hWnd, @dli:ptCursor, TRUE)
		DrawInsert(SELF:Handle(), dli:hWnd, iBeginDragItem)
		SELF:EventReturnValue := 1L
	CASE (dli:uNotification == DL_DRAGGING)                
		iCurItem := LBItemFromPt(dli:hWnd, @dli:ptCursor, TRUE)
		DrawInsert(SELF:handle(), dli:hWnd, iCurItem)
		SELF:EventReturnValue := DL_MOVECURSOR
	CASE (dli:uNotification == DL_DROPPED)                 
		iCurItem := LBItemFromPt(dli:hWnd, @dli:ptCursor, TRUE)
		DrawInsert(SELF:handle(), dli:hWnd, -1)
		oLB := (LIstBox) __WCGetControlByHandle(dli:hWnd)
		IF (oLB != NULL_OBJECT) .AND. (iCurItem != -1) .AND. (iCurItem != iBeginDragItem)
			sItemText := oLB:GetItem(iBeginDragItem+1)
			uItemVal := oLB:GetItemValue(iBeginDragItem+1)
			oLB:DeleteItem(iBeginDragItem+1)
			IF (iBeginDragItem < iCurItem)
				iCurItem --
			ENDIF
			oLB:AddItem(sItemText, iCurItem+1, uItemVal)
			oLB:CurrentItemNo := iCurItem+1
		ENDIF
	ENDCASE
	RETURN SELF
	

METHOD __HandlePointer(oEvent AS @@Event, lHelp AS LOGIC, lClient AS LOGIC) AS Window STRICT 
	//PP-030828 Strong typing
	LOCAL oObject AS OBJECT
    LOCAL lParam AS LONG
	LOCAL hHandle AS PTR
	
	
	
	IF lHelp
		IF (oApp != NULL_OBJECT)
			SetCursor(oApp:__HelpCursor)
		ENDIF
		lParam := oEvent:lParam
		IF HiWord(DWORD(_CAST,lParam)) == WM_LBUTTONDOWN
			hHandle := PTR(_CAST, oEvent:wParam)
			IF hHandle == hWnd
				oObject:=SELF
			ELSE
				oObject :=__WCGetObjectByHandle(hHandle)
				IF IsInstanceOf(oObject, #__FormDialogWindow)
					oObject := oObject:Owner:DataWindow
				ENDIF
			ENDIF
			
			// $$$
			/*
			do while (IsInstanceOf(oObject, #Control) .or. IsInstanceOf(oObject, #Window))
			if (oObject:HyperLabel != NULL_OBJECT) .and. (NULL_STRING != oObject:HyperLabel:HelpContext)
			exit
			endif
			oObject := oObject:Owner
			enddo
			*/
			
			IF (oObject == NULL_OBJECT) .OR. IsInstanceOf(oObject, #App)
				oObject:=SELF
			ENDIF
			SELF:__ProcessHelpCursor(oObject, _AND(lParam, 0xFFFF))
		ENDIF
		SELF:EventReturnValue := 1L
	ELSE
		IF (oPointer != NULL_OBJECT) .AND.;
				(LoWord(DWORD(_CAST,oEvent:lParam)) == HTCLIENT) .AND.;
				((hWnd == oEvent:wParam) .OR. lClient)
			SetCursor(oPointer:Handle())
			SELF:EventReturnValue := 1L
		ELSE
			SELF:Default(oEvent)
		ENDIF
	ENDIF
	RETURN SELF
	

METHOD __HelpFilter(oEvent AS @@Event) AS LOGIC STRICT 
	//PP-030828 Strong typing
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
	

METHOD __InCursorHelpMode() AS LOGIC STRICT 
	//PP-030828 Strong typing
	RETURN lHelpOn .AND. lHelpcursorOn
	

ACCESS __Parent AS OBJECT STRICT 
	//PP-030828 Strong typing
	
	
	RETURN oParent
	

METHOD __PreMenuCommand(oMenuCommandEvent AS @@Event) AS USUAL STRICT 
	//PP-030828 Strong typing
	
	
	//PP-030910
	//PP-031221 WMoore SLE/Button issue - removed call to _PrePreMenuCommand since it prevented proper KeyUp handling
	// SELF:__PrePreMenuCommand()
	
	IF IsMethod(SELF, #PreMenuCommand)
		IF Send(SELF, #PreMenuCommand, oMenuCommandEvent)
			RETURN SELF
		ENDIF
	ENDIF
	
	IF ! SELF:__CommandFromEvent(oMenuCommandEvent)
		RETURN SELF:MenuCommand(oMenuCommandEvent)
	ENDIF
	RETURN SELF
	

METHOD __PrePreMenuCommand() 
	//PP-030910 Temp. workaround for bug 29. Still tracking down cause of problem.
	// A short delay and clearing of message queue
	LOCAL msg  IS _WINMSG
	
	Sleep(200)
	DO WHILE PeekMessage(@msg,SELF:handle(),WM_KEYUP,WM_KEYUP,PM_REMOVE)
	ENDDO
	RETURN SELF
	

METHOD __ProcessHelp(oEvent AS @@Event) AS LOGIC STRICT 
	//SE-060519
	LOCAL oObject AS OBJECT
	LOCAL oHL AS Hyperlabel
	LOCAL sHelpInfo AS _winHelpInfo
	LOCAL hTemp AS PTR
	LOCAL cKey AS STRING
	
	IF oCurrentHelp != NULL_OBJECT .AND. ! lHelpOn
		sHelpInfo := PTR(_CAST, oEvent:lParam)
		
		hTemp := sHelpInfo:hItemHandle
		
		IF (oObject :=__WCGetObjectByHandle(hTemp)) == NULL_OBJECT
			hTemp   := PTR(_CAST, GetWindowLong(hTemp, GWL_HWNDPARENT))
			oObject :=__WCGetObjectByHandle(hTemp)
		ENDIF
		
		IF oObject != NULL_OBJECT
			IF sHelpInfo:iContextType == HELPINFO_WINDOW
				IF IsInstanceOf(oObject, #Control)
					oHL := oObject:Hyperlabel
					IF oHL != NULL_OBJECT .AND. oHL:HelpContext == NULL_STRING
						IF IsInstanceOf(oObject, #TabControl)
							oObject := oObject:CurrentPage
						ELSE
							oObject := oObject:Owner
						ENDIF
					ENDIF
				ENDIF
				IF IsInstanceOf(oObject,#Window)
					IF IsInstanceOf(oObject,#__DocApp) .OR.;
							IsInstanceOf(oObject,#__WndApp) .OR.;
							IsInstanceOf(oObject,#__FormDialogWindow)
						oObject := oObject:Owner
					ENDIF
					IF IsInstanceOf(oObject,#__FormFrame)
						oObject := oObject:DataWindow
					ENDIF
					oHL := oObject:Hyperlabel
				ENDIF
			ELSE
				IF IsInstanceOf(oObject, #Menu)
					oHL := oObject:HyperLabel(sHelpInfo:iCtrlId)
				ENDIF
			ENDIF
			
			IF oHL != NULL_OBJECT
				cKEY := oHL:HelpContext
			ENDIF
			IF cKey == NULL_STRING .AND. IsInstanceOf(oObject, #ShellWindow)
				cKey := "HelpContents"
			ENDIF
			
			oCurrentHelp:Show(cKey, sHelpInfo)
			SELF:EventReturnValue := 1l
			RETURN TRUE
		ENDIF
	ENDIF
	
	RETURN FALSE
	

METHOD __ProcessHelpCursor(oWin AS OBJECT, wArea AS LONGINT) AS LOGIC STRICT 
	//PP-030828 Strong typing
	LOCAL liTemp AS LONGINT
	LOCAL hTemp AS PTR
	
	
	
	hTemp:=oWin:Handle()
	// is it a window or child window
	// if hTemp==hWnd .or. !IsInstanceOf(oWin,#Printer)
	IF IsInstanceOf(oWin, #Window) //hTemp==hWnd
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
		PostMessage(hTemp, WM_WCHelp, HelpWindow, liTemp)
	ELSE
		PostMessage(hTemp, WM_WCHELP, HelpControl, LONGINT(_CAST,hTemp))
	ENDIF
	
	RETURN TRUE
	

METHOD __ProcessToolTip(oControlNotifyEvent AS OBJECT) AS VOID STRICT 
	//PP-030909 Fix display of toolbar tooltips under XP
	
//	LOCAL nCode     		AS DWORD
	LOCAL lParam    		AS LONGINT
	
	LOCAL oControl    	AS Control
	
	LOCAL strucToolInfo 	IS _winTOOLINFO
	LOCAL strucToolTip  	AS _winTOOLTIPTEXT
	LOCAL strucNotify   	AS _winNMHDR
	LOCAL cTipText      	AS STRING
    LOCAL oObject           AS OBJECT
	LOCAL pCursor 			IS _WinPoint
	LOCAL pTCHitTest 		IS _winTC_HitTestInfo
	LOCAL liTab 			AS LONGINT  
	LOCAL ID             AS DWORD  
	LOCAL oEvt				AS ControlNotifyEvent
	LOCAL nLen				AS DWORD
    LOCAL hControl          AS PTR 
    LOCAL oTabControl       AS TabControl

    //SE-070808
    STATIC LOCAL pTipText AS PTR 
    
    IF pTipText != NULL_PTR
        MemFree(pTipText) 
        pTipText := NULL_PTR
    ENDIF 
    
    IF oControlNotifyEvent = NULL_OBJECT
        RETURN
    ENDIF    
	oEvt 			:= oControlNotifyEvent	
//	nCode      	:= oEvt:NotifyCode
	lParam     	:= oEvt:lParam
	oControl   	:= oEvt:Control
	
	//RvdH 050415 No need to continue if we have no lParam
	IF lParam == 0
		RETURN
	ENDIF
	
	
	//PP-030930 Bug 185 see below
	GetCursorPos(@pCursor)
	
	strucNotify          := PTR(_CAST, lParam)
	strucToolInfo:cbSize := _SIZEOF(_winTOOLINFO)
	strucToolInfo:hwnd   := strucNotify:hwndFrom
	
    SendMessage(strucNotify:hwndFrom, TTM_GETCURRENTTOOL, 0, LONG(_CAST, @strucToolInfo))
    oObject := __WCGetObjectByHandle(strucToolInfo:hWnd)
    IF (oObject == NULL_OBJECT)
        oObject := __WCGetObjectByHandle(GetParent(strucToolInfo:hWnd))
	ENDIF
    IF  oObject == NULL_OBJECT
		// Is this a Window handle already ?
        oObject :=  __WCGetObjectByHandle(strucToolInfo:hWnd)
	ENDIF
	strucToolTip := PTR(_CAST, lParam)
    ID           := strucToolTip:hdr:idFrom 
    IF IsInstanceOf(oObject, #ToolBar)
        cTipText  := oObject:GetTipText(ID, #MenuItemID)
		IF SELF:Menu != NULL_OBJECT
			SELF:@@StatusMessage(SELF:Menu:Hyperlabel(ID))
		ENDIF
    ELSEIF IsMethod(oObject, #GetTipText)
        cTipText := oObject:GetTipText(ID)
	ENDIF
	//RvdH 060608 optimized
	//IF Empty(cTipText)
	IF SLen(cTipText) == 0
		IF _AND(strucToolTip:uFlags, TTF_IDISHWND) > 0
			oObject :=__WCGetObjectByHandle(ID)
            IF (oObject == NULL_OBJECT)
				oObject :=__WCGetObjectByHandle( GetParent(strucToolTip:hdr:idFrom))
                IF (oObject == NULL_OBJECT)
					oObject :=__WCGetObjectByHandle(GetParent(GetParent(ID)))
                ENDIF
				//RvdH 050415 Added handling for OleObjects & OleControls
                IF !IsInstanceOf(oObject, #Combobox) .AND. ;
                    !IsInstanceOf(oObject, #IPAddress) .AND. ;
                    !IsInstanceOf(oObject, #OleObject)
                    oObject := NULL_OBJECT
                ENDIF
			ENDIF
		ELSE
			oObject :=__WCGetObjectByHandle(GetDlgItem(SELF:Handle(), INT(_CAST, ID)))
		ENDIF
		
        IF IsInstanceOf(oObject, #Control)
            oControl := oObject  
            IF IsInstanceOf(oControl, #TabControl)   
                oTabControl  := OBJECT(_CAST, oControl)
                hControl := oControl:Handle() 
                ScreenToClient(hControl, @pCursor)
                pTCHitTest:pt:x := pCursor:x
                pTCHitTest:pt:y := pCursor:y
				liTab := SendMessage(hControl, TCM_HITTEST, 0, LONGINT(_CAST,@pTCHitTest))
				IF liTab > -1
                    cTipText := oTabControl:GetTipText(liTab)
				ENDIF
			ENDIF
			//RvdH 060608 optimized
			//IF Empty(cTipText)
			IF SLen(cTipText) == 0
            cTipText := oControl:ToolTipText
            IF SLen(cTipText) == 0 .AND. oControl:UseHLForToolTip .AND. oControl:HyperLabel != NULL_OBJECT
               cTipText := oControl:HyperLabel:Description
				ENDIF
			ENDIF
        ELSE
            // For other types (e.g. ControlWindow) check for late bound properties and call them
            IF IsAccess(oObject, #ToolTipText)
                cTipText := IVarGet(oObject, #ToolTipText)
            ENDIF
            IF SLen(cTipText) == 0 .AND. IsAccess(oObject, #UseHLForToolTip) .AND. IVarGet(oObject, #UseHLForToolTip) 
                IF IsAccess(oObject, #HyperLabel) .AND. IVarGet(oObject, #HyperLabel) != NIL
                    cTipText := oObject:HyperLabel:Description
                ENDIF
            ENDIF
        ENDIF
	ENDIF
	
    //RvdH 060608 optimized
    //SE-070808
    IF (nLen := SLen(cTipText)) == 0
        strucToolTip:lpszText := NULL_PSZ
    ELSE     
        IF nLen < 80    
            MemCopy(@strucToolTip:szText[1], String2Psz(cTipText), nLen)
            strucToolTip:szText[nLen+1] := 0
        ELSE 
            pTipText := PTR(_CAST, StringAlloc(cTipText))
            strucToolTip:lpszText := pTipText
        ENDIF   
    ENDIF 
	RETURN
	
	

METHOD __ReleaseDC() AS VOID STRICT 
	//PP-030828 Strong typing
	
	
	IF (hDC != NULL_PTR)
		WCDCDelete(SELF)
		ReleaseDC(SELF:Handle(4), hDC)
		hDC := NULL_PTR
	ENDIF
	RETURN
	

METHOD __SetBrushNeeded(lNew AS LOGIC) AS LOGIC STRICT 
	//PP-030828 Strong typing
	
	
	DCBrushNeeded := lNew
	
	RETURN lNew
	

METHOD __SetDCInitialized(lNew AS LOGIC) AS LOGIC STRICT 
	//PP-030828 Strong typing
	
	
	DCInitialized := lNew
	
	RETURN lNew
	

METHOD __SetFont(oNewFont AS Font) AS Font STRICT 
	//PP-030828 Strong typing
	
	
	oFont := oNewFont
	
	RETURN oFont
	

METHOD __SetPenNeeded(lNew AS LOGIC) AS LOGIC STRICT 
	//PP-030828 Strong typing
	
	
	DCPenNeeded := lNew
	
	RETURN lNew
	

METHOD __SetSelfAssoAccel(lSwitch AS LOGIC) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL hTemp AS PTR
	
	
	
	IF lSwitch
		IF (oAccelerator != NULL_OBJECT)
			hTemp := oAccelerator:Handle()
		ENDIF
		SetAccelerator(hWnd, hTemp)
	ELSE
		SetAccelerator(NULL_PTR, NULL_PTR)
	ENDIF
	RETURN
	

METHOD __SetSelfMenu() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL oMenu AS Menu
	LOCAL hMenu AS PTR
	
	oMenu := SELF:Menu
	IF oMenu != NULL_OBJECT
		hMenu := oMenu:Handle()
	ELSE
		hMenu := NULL_PTR
	ENDIF
	SetMenu(hWnd, hMenu)
	RETURN
	

METHOD __Timer() AS OBJECT STRICT 
	//PP-030828 Strong typing
	
	
	dwTimerCount := dwTimerCount - 1
	IF (dwTimerCount == 0)
		SELF:Timer()
		dwTimerCount := dwTimerInterval
		IF (dwTimerCount == 0)
			__WCUnregisterTimer(SELF)
			lTimerRegistered := FALSE
		ENDIF
	ENDIF
	
	RETURN SELF
	

METHOD __ToolTipHandle() AS PTR STRICT 
	//PP-030828 Strong typing
	RETURN hWndToolTip
	

METHOD __UpdateTrayIcon(dwNIM,oTrayIcon,dwID,sToolTip) 
	//PP-030902
	LOCAL NID IS _winNOTIFYICONDATA
	LOCAL dwTipLength
	
	
	
	IF ! __LoadShellDll()
		RETURN FALSE
	ENDIF
	
	Default(@dwID, 1)
	Default(@sToolTip,"")
	Default(@oTrayIcon,NULL_OBJECT)
	
	IF ! GetShellMajorVersion() >= 5
		sToolTip := StrTran(sToolTip,CRLF," ") // Line breaks not supported on older Shell
		dwTipLength := TRAYTIP_LENGTH_SHELLORIGINAL
	ELSE
		dwTipLength := TRAYTIP_LENGTH_SHELL5
	ENDIF
	
	NID:cbSize := SizeOfNotifyIconData()
	NID:hWnd := SELF:handle()
	NID:uID := dwId
	NID:uFlags := _OR(NIF_MESSAGE, NIF_ICON, NIF_TIP)
	NID:uCallbackMessage := TRAY_ICON_MSG
	IF ! oTrayIcon == NULL_OBJECT
		NID:hIcon            := oTrayIcon:Handle()
	ENDIF
	//RvdH 060608 Optimized
	//IF !Empty(sToolTip)
	IF SLen(sToolTip) > 0
#ifdef __VULCAN__	
		MemCopy(@(NID:szTip[1]), String2Psz(sToolTip), dwTipLength)
#else		
		MemCopy(@(NID:szTip[1]), PTR(_CAST, sToolTip), dwTipLength)
#endif		
		NID:szTip[dwTipLength] := 0
	ENDIF
	
	RETURN PCALL(gpfnShell_NotifyIcon, DWORD(dwNIM), @NID)
	

ACCESS Accelerator 
	
	
	RETURN oAccelerator
	

ASSIGN Accelerator(oNewAccelerator) 
	
	
	oAccelerator := oNewAccelerator
	
	IF oAccelerator == NULL_OBJECT
		SetAccelerator(hWnd, NULL_PTR)
	ELSE
		SetAccelerator(hWnd, oAccelerator:Handle())
	ENDIF
	
	RETURN 
	

METHOD Activate(oEvent) 
	
	
	RETURN SELF:Default(oEvent)
	

METHOD AddTrayIcon(oTrayIcon, dwID, sToolTip) 
	//PP-030902
	RETURN SELF:__UpdateTrayIcon(NIM_ADD,oTrayIcon,dwID,sToolTip)
	


METHOD Animate(nTime,nFlags) 
   LOCAL dwTime,dwFlags AS DWORD
   // Determine Windows version
   dwTime := nTime
   dwFlags := nFlags
   AnimateWindow(SELF:Handle(),dwTime,dwFlags)
   RETURN TRUE
   
METHOD AnimationStart(oControlEvent) 
	//PP-031115 Name of imcoming variable changed from oControlNotifyEvent to oControlEvent
	
	
	RETURN SELF:Default(oControlEvent)
	

METHOD AnimationStop(oControlEvent) 
	//PP-031115 Name of imcoming variable changed from oControlNotifyEvent to oControlEvent
	
	
	RETURN SELF:Default(oControlEvent)
	

METHOD AppCommand(oACEvent) 
	//PP-030904
	// FALSE means the message has not been processed, so it is passed on to windows so other default behaviour can occur
	RETURN FALSE
	

ACCESS Automated 
	RETURN lAutomated
	

ASSIGN Automated(lNewVal) 
	//RvdH 041128 Changed because OLE is now always loaded
	IF lNewVal != SELF:lAutomated
		// 	IF (gpfnOLERegisterAutomationObject == NULL_PTR) .or. (gpfnOLEUnRegisterAutomationObject == NULL_PTR)
		// 		WCError{#Automated,ClassName(SELF),__WCSCNoAutomation}:@@Throw()
		// 	ELSE
		SELF:lAutomated := lNewVal
#ifdef USE_OLEOBJECT
		IF SELF:lAutomated
			_VOOLERegisterAutomationObject(PTR(_CAST, SELF), NULL_PSZ, 1, FALSE)
			//PCALL(gpfnOLERegisterAutomationObject, PTR(_CAST, SELF), NULL_PSZ, 1, FALSE)
		ELSE
			_VOOLEUnRegisterAutomationObject(PTR(_CAST, SELF))
			//PCALL(gpfnOLEUnRegisterAutomationObject, PTR(_CAST, SELF))
		ENDIF
#endif		
		// 	ENDIF
	ENDIF
	
	RETURN SELF:lAutomated
	
	

ACCESS Background 
	//RvdH 050509 Always return  valid brush
	
	//IF oBackground != NULL_OBJECT
	RETURN oBackground
	//ENDIF
	//RETURN NULL_OBJECT
	

ASSIGN Background(oNewBackground) 
	LOCAL strucLogBrush IS _WinLogBrush
	
	oBackground := oNewBackground
	
	SELF:__GetDC()
	__WCLogicalBackgroundBrush(SELF,@strucLogBrush)
	SetBkColor(hDC, strucLogBrush:lbColor)
	
	IF (strucLogBrush:lbStyle == BS_HOLLOW)
		SetBkMode(hDC, TRANSPARENT)
	ELSE
		SetBkMode(hDC, OPAQUE)
	ENDIF
	
	RETURN 
	

METHOD ButtonClick(oControlEvent) 
	
	RETURN SELF:Default(oControlEvent)
	

METHOD ButtonDoubleClick(oControlEvent) 
	
	RETURN SELF:Default(oControlEvent)
	

ACCESS CanvasArea 
	LOCAL rect IS _WINRECT
	LOCAL oPoint AS Point
	LOCAL oDimension AS Dimension
	
	GetClientRect(SELF:Handle(4), @rect)
	oPoint := Point{0, 0} // GetClientRect always return a (0, 0) origin
	oDimension := Dimension{rect:right - rect:left, rect:bottom - rect:top}
	
	RETURN BoundingBox{oPoint, oDimension}
	

METHOD CanvasErase() 
	LOCAL _handle AS PTR
	LOCAL _hdc    AS PTR
	
	//PP-031129 modified to cal DrawBackground
	_handle := SELF:Handle(4)
	IF (_handle != NULL_PTR)
		_hdc := GetDC(_handle)
		IF oBackground == NULL_OBJECT
			IF ! SELF:DrawBackground(_hdc, SELF)
				SELF:PaintBackground(_hdc)
			ENDIF
		ELSE
			SELF:PaintBackground(_hdc)
		ENDIF
		ReleaseDC(_handle, _hdc)
		SELF:Repaint()
	ENDIF
	
	RETURN NIL
	

ACCESS Caption 
	
	RETURN cCaption
	

ASSIGN Caption(sNewCaption) 
	LOCAL pszCaption AS PSZ
	
	cCaption := sNewCaption
	
	IF cCaption == NULL_STRING
		SetWindowText(hWnd, NULL_PSZ)
	ELSE
		// self:SetStyle(WS_CAPTION, TRUE) // $$$ incomplatible with 1.0 $$$
		pszCaption:=StringAlloc(cCaption)
		SetWindowText(hWnd, pszCaption)
		MemFree(pszCaption)
	ENDIF
	
	RETURN 
	

METHOD Center() 
	// DHer: 18/12/2008
	LOCAL oOwner AS Window
	LOCAL oCanvas AS BoundingBox
	IF IsInstanceOfUsual(oParent,#Window) 
	   oOwner   := oParent
	   oCanvas  := oOwner:CanvasArea
		SELF:Origin := Point{oCanvas:Size:Width/2-SELF:Size:Width/2,oCanvas:Size:Height/2-SELF:Size:Height/2}
	ELSE
		SELF:Origin := Point{GetSystemMetrics(SM_CXSCREEN)/2-SELF:Size:Width/2,GetSystemMetrics(SM_CYSCREEN)/2-SELF:Size:Height/2}
	ENDIF

RETURN NIL

METHOD Close(oEvent) 
	RETURN NIL
	

METHOD ComboBoxExEndEdit(oComboBoxExEndEditEvent) 
	//SE-060519
	RETURN NIL

METHOD ComboBoxExNotify(oControlNotifyEvent) 
	//SE-060519                          
	LOCAL oCNE AS ControlNotifyEvent
	oCNE := oControlNotifyEvent 
	IF oCNE:NotifyCode = CBEN_ENDEDIT
		SELF:ComboBoxExEndEdit(ComboBoxExEndEditEvent{oCNE})
	ENDIF
	
	RETURN NIL

ACCESS ContextMenu 
	RETURN oContextMenu
	

ASSIGN ContextMenu(oNewMenu) 
	RETURN (oContextMenu := oNewMenu)
	

METHOD ControlNotify(oControlNotifyEvent) 
	// Handling Window
	LOCAL oTargetWnd AS Window
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
	oControl :=__WCGetObjectByHandle(strucNotify:hwndFrom)
	
	IF (IsInstanceOf(SELF, #__FormDialogWindow) .AND. IsInstanceOf(oParent, #__FormFrame))
		oTargetWnd := IVarGet(oParent, #DataWindow)
		oTargetWnd:EventReturnValue := 0
	ELSEIF (IsInstanceOf(SELF, #__FormFrame))
		oTargetWnd := IVarGet(SELF, #DataWindow)
		oTargetWnd:EventReturnValue := 0
	ELSE
		oTargetWnd := SELF
	ENDIF
	
	DO CASE
		//PP-031115 ACN_START/ACN_STOP need to be processed as WM_COMMAND
		//	CASE nCode == ACN_START
		//		Send(oTargetWnd, #AnimationStart, oEvt)
		
		//PP-031115 ACN_START/ACN_STOP need to be processed as WM_COMMAND
		// 	CASE nCode == ACN_STOP
		//		Send(oTargetWnd, #AnimationStop, oEvt)
		
		// case nCode == EN_DROPFILES
		// Send(oTargetWnd, #RichEditDropFiles, RichEditDropEvent{oEvt})
		
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
			Send(oControl, #__ItemChanged, oEvt)
		ENDIF
		Send(oTargetWnd, #ListViewItemChanged, ListViewItemEvent{oEvt})
		
	CASE nCode == LVN_ODFINDITEM
		//PP-030319 Corrected setting of return value. Fixes problem finding item in DLV on DW. Thanks to S Ebert
		//PP-031115
		IF IsMethod(oControl,  #__FindItem)
			oTargetWnd:EventReturnValue := Send(oControl, #__FindItem, oEvt)
		ENDIF
		
	CASE nCode == NM_CLICK .OR. nCode == NM_RCLICK
		IF IsInstanceOf(oControl, #TreeView)
			Send(oTargetWnd, #TreeViewMouseButtonDown, TreeViewMouseEvent{oEvt})
		ELSEIF IsInstanceOf(oControl, #ListView)
			Send(oTargetWnd, #ListViewMouseButtonDown, ListViewMouseEvent{oEvt})
		ELSEIF IsInstanceOf(oControl, #SysLink)
			Send(oTargetWnd, #SysLinkSelect, SysLinkSelectEvent{oEvt})
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
			Send(oControl, #__FocusPage, TabCtrl_GetCurSel(oControl:Handle()))
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
	ENDIF
	RETURN NIL

METHOD DateTimeSelectionChanged(oDateTimeSelectionEvent) 
	//SE-040929
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
	oDTPicker := OBJECT(oEvt:Control)
	cOldValue := oDTPicker:AsString()
	cText := oDTPicker:TextValue
	IF IsInstanceOfUsual(oDTPicker:FieldSpec, #FieldSpec)
		cText := AsString(oDTPicker:FieldSpec:Val(cText))
	ENDIF
	IF ! cOldValue == cText
		oDTPicker:Modified := TRUE
		IF IsInstanceOf(oDTPicker:Owner, #DataWindow)
			oDTPicker:Owner:__DoValidate(oDTPicker)
		ENDIF
	ENDIF
	IF oDTPicker:NullFormat .AND. oDTPicker:SelectedDate != NULL_DATE
        // Re-assign the value so the format gets set
        oDTPicker:SelectedDate := oDTPicker:SelectedDate
    ELSEIF !oDTPicker:NullFormat .AND. oDTPicker:SelectedDate == NULL_DATE
        // Reassign the selected Date so the format gets set
        oDTPicker:SelectedDate := NULL_DATE
    ENDIF


	RETURN 0l
	

METHOD DeActivate(oEvent) 
	//RvdH 041123 Call to DeactivateAllOLEObjects moved from DataWindow to Window
	
	SELF:DeactivateAllOLEObjects()
	RETURN SELF:Default(oEvent)
	

METHOD DeactivateAllOLEObjects(oExcept) 
#ifdef USE_OLEOBJECTS
	//RvdH 041123 Added method at Window Level
	//				  Also removed lNeedUpdate
	LOCAL i 	  		AS DWORD
	LOCAL oOLE	  		AS OBJECT
	LOCAL aObjects 		AS ARRAY
	LOCAL oException  	AS OleObject
	IF IsObject(oExcept)
		oException := oExcept
	ENDIF
	aObjects := SELF:__GetMyOleObjects()
	FOR i:= 1 TO ALen(aObjects)
		oOLE := aObjects[i]
		IF oOle <> oException
			oOLE:Deactivate()
			//IF oOLE:IsInPlaceActive
			
			//ENDIF
		ENDIF
	NEXT
#endif	
	RETURN SELF
	
	

METHOD Default(oEvent) 
	
	
	SELF:EventReturnValue := 1L
	
	RETURN SELF
	

METHOD DeleteTrayIcon(dwID) 
	LOCAL NID IS _winNOTIFYICONDATA
	
	
	
	IF !__LoadShellDll()
		RETURN FALSE
	ENDIF
	
	Default(@dwID, 1)
	
	//PP-030902
	NID:cbSize := SizeOfNotifyIconData()
	NID:hWnd := SELF:handle()
	NID:uID := dwID
	
	RETURN PCALL(gpfnShell_NotifyIcon, NIM_DELETE, @NID)
	

METHOD Destroy() 
	
	IF lAutomated
		SELF:Automated := FALSE
	ENDIF
	
	//__WCUnregisterMenu(oMenu)
	//__WCUnregisterMenu(oContextMenu)
	
	IF (hWnd != NULL_PTR)
		EnableWindow(hwnd, FALSE)
		IF !IsInstanceOf(SELF,#ControlWindow)
			SELF:__DestroyChildren()
		ENDIF
		IF lTimerRegistered
			__WCUnregisterTimer(SELF)
		ENDIF
		
		IF !InCollect()
			IF (oToolBar != NULL_OBJECT)
				oToolBar:Destroy()
				oToolBar := NULL_OBJECT
			ENDIF
			
			// Move to 2.0b
			IF IsWindow(hwndToolTip)
				DestroyWindow(hwndToolTip)
				hwndToolTip := NULL_PTR
			ENDIF
			
			oMenu := NULL_OBJECT
			oIcon := NULL_OBJECT
			oIconSmall := NULL_OBJECT
			oContextMenu := NULL_OBJECT
		ENDIF
		
      SELF:__ProcessToolTip(NULL_OBJECT) //SE-070808 Free the Tooltip buffer
		SELF:__DestroyWnd()
		
		IF !InCollect()
			hWnd := NULL_PTR
			hDC := NULL_PTR
			oAccelerator := NULL_OBJECT
			oFont := NULL_OBJECT
			oPointer := NULL_OBJECT
			oBackground := NULL_OBJECT
			oForeground := NULL_OBJECT
			lTimerRegistered := FALSE
		ENDIF
	ENDIF
	
	SUPER:Destroy()
	
	RETURN NIL
	

METHOD Disable() 
	
	EnableWindow(hWnd, FALSE)
	
	RETURN NIL
	

ACCESS DragDropClient 
	
	RETURN oDragDropClient
	

ACCESS DragDropServer 
	
	RETURN oDragDropServer
	

ACCESS DragImageList 
	
	RETURN oDragImageList
	

METHOD Draw(oDrawObject) 
	LOCAL cnt, i AS DWORD
	LOCAL oDraw AS DrawObject
	LOCAL aDraw AS ARRAY
	
	
	IF (hWnd == NULL_PTR) .AND. !IsInstanceOf(SELF, #Printer)
		RETURN SELF
	ENDIF
	
	IF !IsArray(oDrawObject)
		IF !IsInstanceOfUsual(oDrawObject,#DrawObject)
			WCError{#Draw,#Window,__WCSTypeError,oDrawObject,1}:@@Throw()
		ENDIF
		oDraw := oDrawObject
		oDraw:__SetWindow(SELF)
		oDraw:Draw()
	ELSE
	    aDraw := oDrawObject
		cnt := ALen(aDraw)
		FOR i:=1 TO cnt
			IF !IsInstanceOfUsual(aDraw[i],#DrawObject)
				WCError{#Draw,#Window,__WCSTypeError,oDrawObject[i],1}:@@Throw()
			ENDIF
			oDraw := aDraw[i]
			oDraw:__SetWindow(SELF)
			oDraw:Draw()
		NEXT
	ENDIF
	
	RETURN SELF
	

METHOD DrawBackground(hdc, oWindow) 
	//PP-031129 From S Ebert
	
	//If you draw background for yourself return TRUE
	//otherwise FALSE
	//The oWindow object is normally identical with SELF
	//except SELF is a DataWindow. In this case oWindow
	//can be the oSurface (__FormDialogWindow) object.
	//In a DataWindow you should draw background ony if
	//oWindow == oSurface, otherwise it's superfluous.
	
	RETURN FALSE
	

METHOD Drop() 
	
	
	RETURN NIL
	

METHOD EditChange(oControlEvent) 
	
	
	RETURN SELF:Default(oControlEvent)
	

METHOD EditFocusChange(oEditFocusChangeEvent) 
	
	RETURN SELF:Default(oEditFocusChangeEvent)
	

METHOD EditScroll(oControlEvent) 
	
	
	RETURN SELF:Default(oControlEvent)
	

METHOD Enable() 
	
	
	EnableWindow(hWnd, TRUE)
	
	RETURN NIL
	

METHOD EnableCloseBox(uValue) 
   LOCAL oWindow		AS Window
	// DHer: 18/12/2008
	// Zoeken achter parent met CloseBox
	oWindow := SELF
	DO WHILE oWindow <> NULL_OBJECT
		//IF _And(GetWindowLong(oWindow:Handle(),GWL_STYLE),WS_SYSMENU)=WS_SYSMENU
		IF GetSystemMenu(oWindow:Handle(),FALSE)<>NULL_PTR
			EXIT
		ELSE
			IF IsInstanceOfUsual(oWindow:Owner, #Window)
				oWindow := oWindow:Owner
			ELSE
				oWindow := NULL_OBJECT
			ENDIF
		ENDIF
	ENDDO

	IF oWindow<>NULL_OBJECT
		IF uValue 
			RETURN EnableMenuItem(GetSystemMenu(oWindow:Handle(),FALSE),SC_CLOSE,MF_ENABLED)
		ELSE
			RETURN EnableMenuItem(GetSystemMenu(oWindow:Handle(),FALSE),SC_CLOSE,_OR(MF_GRAYED,MF_BYCOMMAND))
		ENDIF
	ENDIF

RETURN NIL


METHOD EnableDragDropClient(lEnable) 
	
	
	IF !IsNil(lEnable)
		IF !IsLogic(lEnable)
			WCError{#EnableDragDropClient,#Window,__WCSTypeError,lEnable,1}:@@Throw()
		ENDIF
	ELSE
		lEnable := TRUE
	ENDIF
	
	IF lEnable
		IF (oDragDropClient == NULL_OBJECT)
			oDragDropClient := DragDropClient{SELF}
		ENDIF
	ELSEIF (oDragDropClient != NULL_OBJECT)
		oDragDropClient:Destroy()
		oDragDropClient := NULL_OBJECT
	ENDIF
	
	RETURN SELF
	

METHOD EnableDragDropServer(lEnable) 
	
	
	IF !IsNil(lEnable)
		IF !IsLogic(lEnable)
			WCError{#EnableDragDropServer,#Window,__WCSTypeError,lEnable,1}:@@Throw()
		ENDIF
	ELSE
		lEnable := TRUE
	ENDIF
	
	IF lEnable
		IF (oDragDropServer == NULL_OBJECT)
			oDragDropServer := DragDropServer{SELF}
		ENDIF
	ELSEIF (oDragDropServer != NULL_OBJECT)
		oDragDropServer:Destroy()
		oDragDropServer := NULL_OBJECT
	ENDIF
	
	RETURN SELF
	

METHOD EnableHelp(lEnable, oHelpDisplay) 
	//SE-060519
	
	
	
	IF !IsNil(lEnable)
		IF !IsLogic(lEnable)
			WCError{#EnableHelp,#Window,__WCSTypeError,lEnable,1}:@@Throw()
		ENDIF
	ENDIF
	
	IF !IsNil(oHelpDisplay)
		IF !IsInstanceOfUsual(oHelpDisplay,#HelpDisplay)
			WCError{#EnableHelp,#Window,__WCSTypeError,oHelpDisplay,2}:@@Throw()
		ENDIF
	ENDIF
	
	IF lHelpOn
		lHelpOn := FALSE
		SELF:__EnableHelpCursor(FALSE)
		IF (GetActiveWindow() == hWnd) .AND. (oApp != NULL_OBJECT)
			oApp:__SetHelpWind(0, HM_NONE)
		ENDIF
	ENDIF
	
	IF lEnable
		oCurrentHelp := oHelpDisplay
		IF oCurrentHelp = NULL_OBJECT .OR. ! oCurrentHelp:Win32Processing
			lHelpOn := TRUE
			IF (GetActiveWindow() == hWnd) .AND. (oApp != NULL_OBJECT)
				oApp:__SetHelpWind(hWnd, HM_GENERAL)
			ENDIF
		ENDIF
	ELSE
		oCurrentHelp := NULL_OBJECT
	ENDIF
	
	RETURN SELF
	

METHOD EnableHelpButton() 
	//SE-060519
	
	
	SELF:SetExStyle(WS_EX_CONTEXTHELP, TRUE)
	
	RETURN NIL
	

METHOD EnableHelpCursor() 
	//SE-060519
	
	IF lHelpOn
		SELF:__EnableHelpCursor(TRUE)
	ELSE
		PostMessage(SELF:Handle(), WM_SYSCOMMAND, SC_CONTEXTHELP, 0)
	ENDIF
	
	RETURN NIL
	

METHOD EnableThemeDialogTexture(dwStyle) 
	//PP-030909
	RETURN EnableThemeDialogTexture(SELF,dwStyle)
	

METHOD EnableToolTips(lEnable) 
	
	
	Default(@lEnable, TRUE)
	
	IF lEnable .AND. (hwndToolTip == NULL_PTR)
		
		hwndToolTip := CreateWindowEx(0, String2Psz(TOOLTIPS_CLASS), NULL_PSZ, ;
			DWORD(_CAST, _OR(WS_POPUP, TTS_ALWAYSTIP)), ;
			CW_USEDEFAULT, CW_USEDEFAULT,;
			10, 10, hWnd, NULL_PTR, ;
			_GetInst(), NULL_PTR)
		
		SendMessage(hwndToolTip, TTM_SETMAXTIPWIDTH, 0 , LONGINT((GetSystemMetrics(SM_CYFULLSCREEN) / 2)))
		SendMessage(hwndToolTip, TTM_SETDELAYTIME, TTDT_AUTOPOP, 5000)
	ELSEIF (hwndToolTip != NULL_PTR)
		SendMessage(hwndToolTip, TTM_ACTIVATE, DWORD(_CAST, lEnable), 0)
	ENDIF
	
	RETURN lEnable
	

METHOD Expose(oExposeEvent) 
	
	
	RETURN SELF:Default(oExposeEvent)
	

METHOD FocusChange(oFocusChangeEvent) 
	
	
	RETURN SELF:Default(oFocusChangeEvent)
	

ACCESS Font 
	
	
	RETURN oFont
	

ASSIGN Font(oNewFont) 
	
	
	oFont := oNewFont
	// if !hDC==0
	DCFontNeeded := TRUE
	DCFontInUse := FALSE
	// this forces the new object to be selected into the current DC and allows proper releasing of the old one
	SELF:__GetDC()
	// endif
	
	RETURN 
	

ACCESS Foreground 
	
	
	RETURN oForeground
	

ASSIGN Foreground(oNewForeground) 
	
	
	oForeground := oNewForeground
	DCBrushInUse := FALSE
	DCBrushNeeded := TRUE
	// this forces the new object to be selected into the current DC and allows proper releasing of the old one
	SELF:__GetDC()
	
	RETURN 
	

METHOD GetAllChildren() 
	LOCAL hChild AS PTR
	LOCAL oChild AS OBJECT
	LOCAL aRet AS ARRAY
	
	
	
	aRet := {}
	
	hChild := GetWindow(SELF:Handle(4), GW_CHILD)
	
	WHILE (hChild != NULL_PTR)
		oChild :=__WCGetObjectByHandle(hChild)
		IF (oChild != NULL_OBJECT)
			IF IsInstanceOf(oChild, #__DocApp)
				oChild := oChild:Owner
			ELSEIF IsInstanceOf(oChild, #__WindApp) .AND. IsInstanceOf(oChild:Owner, #__FormFrame)
				oChild := oChild:Owner:DataWindow
			ENDIF
			AAdd(aRet, oChild)
		ENDIF
		
		hChild := GetWindow(hChild, GW_HWNDNEXT)
	END
	
	RETURN aRet
	

METHOD GetStyle() 
	RETURN GetWindowLong(hWnd, GWL_STYLE)

METHOD GetExStyle
	RETURN GetWindowLong(hWnd, GWL_EXSTYLE)
	

METHOD Handle() AS PTR
	RETURN hWnd

METHOD HasExStyle(kStyle AS LONG)
   LOCAL liStyle	AS DWORD    
	// DHer: 18/12/2008
	liStyle := GetWindowLong(SELF:hWnd,GWL_EXSTYLE)
	RETURN _AND(liStyle,kStyle) != 0

METHOD HasStyle(kStyle AS LONG)
   LOCAL liStyle	AS DWORD    
	// DHer: 18/12/2008
	liStyle := GetWindowLong(SELF:hWnd,GWL_STYLE)
	RETURN _AND(liStyle,kStyle) != 0


ACCESS HelpDisplay 
	RETURN oCurrentHelp
	

ASSIGN HelpDisplay(oHelpDisplay) 
	
	
	SELF:EnableHelp(TRUE, oHelpDisplay)
	
	RETURN 
	// if !IsNil(oHelpDisplay)
	// if !IsInstanceOfUsual(oHelpDisplay,#HelpDisplay)
	// WCError{#EnableHelp,#Window,__WCSTypeError,oHelpDisplay,2}:@@Throw()
	// endif
	// endif
	
	// oCurrentHelp := oHelpDisplay
	// return self
	

METHOD HelpRequest(oHelpRequestEvent) 
	LOCAL cHelpContext AS STRING
	LOCAL dwType, dwID, i, j AS DWORD
	LOCAL pszBuf AS PSZ
	LOCAL liRegion AS LONGINT
	LOCAL bByte AS BYTE
	LOCAL hMenu AS PTR
	LOCAL oEvt	:= oHelpRequestEvent AS HelpRequestEvent
	
	
	IF oCurrentHelp != NULL_OBJECT
		cHelpContext:=oEvt:HelpContext
		IF NULL_STRING != cHelpContext
			oCurrentHelp:show(cHelpContext, oEvt:@@HELPINFO)
			SELF:EventReturnValue := 1l
		ELSE
			dwType := oEvt:HelpType
			DO CASE
			CASE (dwType == HelpMenu)
				dwID := oEvt:ItemID
				IF (dwID != 0) .AND. (dwID != 0xFFFF)
					
					// Get menu string for menu item selected
					IF (_AND(DWORD(_CAST,oEvt:lParam), 0x00010000U) != 0) //System menu?
						hMenu := GetSystemMenu(hWnd,FALSE)
					ELSE
						IF oMenu != NULL_OBJECT
							hMenu := oMenu:Handle()
						ENDIF
					ENDIF
					
					IF (hMenu != NULL_PTR)
						pszBuf := MemAlloc(40)
						GetMenuString( hMenu, oEvt:ItemID, pszBuf, 39, MF_ByCommand)
						
						// Process menu string for lookup
						bByte:=_NGet(pszBuf,0)
						IF (bByte != 0)
							// Skip leading spaces
							DO WHILE (bByte == 32) .AND. (bByte != 0)
								i++
								bByte := _NGet(pszBuf,i)
							ENDDO
							
							//Copy and translate text
							DO WHILE (bByte != 0) .AND. (bByte != 9) //Loop until end or tab
								DO CASE
								CASE IsDBCSLeadByte(bByte)
									_NPut(pszBuf, j++, bByte) //move in lead byte
									_NPut(pszBuf, j++, BYTE(_NGet(pszBuf, ++i))) //move in byte
									
								CASE (bByte == 38) // "&"
									//Don't copy it
									
								CASE (bByte == 32) // Space
									_NPut(pszBuf, j++, 95) //move in "_"
									//Position to last space in sequence
									DO WHILE (_NGet(pszBuf, i+1) == 32)
										i++
									ENDDO
									
								CASE (bByte == 46) // '.'
									IF (_NGet(pszBuf,i+1) != 46)
										_NPut(pszBuf, j++, bByte) //Only one "."
									ELSE
										DO WHILE (_NGet(pszBuf,i+1) == 46) //skip multiple "."
											i++
										ENDDO
									ENDIF
									
								OTHERWISE
									_NPut(pszBuf, j++, bByte) //move in byte
								ENDCASE
								bByte := _NGet(pszBuf, ++i)
							ENDDO
							_NPut(pszBuf, j, 0)
							oCurrentHelp:Show("[C]Menu_"+Psz2String(pszBuf))
							MemFree(pszBuf)
						ENDIF
					ENDIF
				ENDIF
				
			CASE (dwType == HelpWindow)
				liRegion:= oEvt:WindowRegion
				IF (liRegion == RegionUnknown)
					oCurrentHelp:Show("HelpIndex")
				ELSE
					IF (liRegion > 9)
						liRegion := RegionUnknown
					ENDIF
					oCurrentHelp:Show( __CavoStr( __WCSAreaIndex + liRegion ) )
				ENDIF
				
			CASE (dwType == HelpControl)
				dwID := oEvt:ItemID
				IF dwID >= 0
					//lIUHO01 02-14-97 iF control has no help context, just bring up the index help
					// window and put Control_xxx keyword. this is whtat the Class Reference guide say,
					// but in vo 1.0, does not do anything about this case. I think we should handle
					//this case like the document descript.
					oCurrentHelp:Show("Control_"+NTrim(dwID))
					// $$$ oCurrentHelp:Show("[C]Control_"+AsString(dwID))
				ENDIF
			ENDCASE
		ENDIF
	ENDIF
	
	RETURN NIL
	//return self:Default(oHelpRequestEvent)
	

METHOD Hide() 
	
	
	ShowWindow(hWnd, SW_HIDE)
	
	RETURN NIL
	

METHOD HorizontalScroll(oScrollEvent) 
	LOCAL oScrollBar AS ScrollBar
	LOCAL oEvt	:= oScrollEvent AS ScrollEvent
	
	
	
	oScrollBar := oEvt:ScrollBar
	IF (oScrollBar != NULL_OBJECT)
		oScrollBar:ThumbPosition := oEvt:Position
	ENDIF
	
	RETURN SELF:Default(oEvt)
	

METHOD HorizontalSlide(oSliderEvent) 
	//local oSlider as Slider
	
	
	
	//oSlider := oSliderEvent:Slider
	//if (oSlider != NULL_OBJECT)
	// oSlider:ThumbPosition := oSliderEvent:Position
	//endif
	
	RETURN SELF:Default(oSliderEvent)
	

METHOD HorizontalSpin(oSpinnerEvent) 
	LOCAL oSpinner AS Spinner
	LOCAL oEvt	:= oSpinnerEvent AS SpinnerEvent
	
	
	
	oSpinner := oEvt:Spinner
	IF (oSpinner != NULL_OBJECT)
		oSpinner:Position := oEvt:Position
	ENDIF
	
	RETURN SELF:Default(oEvt)
	

ACCESS HyperLabel 
	
	
	RETURN oHyperLabel
	

ASSIGN HyperLabel(oHL) 
	
	
	IF IsInstanceOfUsual(oHL,#HyperLabel)
		oHyperLabel := oHL
		SELF:@@StatusMessage(oHL, MESSAGEPERMANENT)
	ENDIF
	
	RETURN 
	

ACCESS Icon 
	
	
	RETURN oIcon
	

ASSIGN Icon(oNewIcon) 
	LOCAL hIcon AS PTR
	
	
	
	oIcon := oNewIcon
	IF (oIcon != NULL_OBJECT)
		hIcon := oIcon:Handle()
	ENDIF
	
	SendMessage(SELF:handle(), WM_SETICON, DWORD(_CAST, TRUE), LONGINT(_CAST, hIcon))
	
	RETURN 
	

ACCESS IconSm 
	
	
	RETURN oIconSmall
	

ASSIGN IconSm(oNewIcon) 
	LOCAL hIcon AS PTR
	
	
	
	oIconSmall := oNewIcon
	IF (oIconSmall != NULL_OBJECT)
		hIcon := oIconSmall:Handle()
	ENDIF
	
	SendMessage(SELF:handle(), WM_SETICON, DWORD(_CAST, FALSE), LONGINT(_CAST, hIcon))
	
	RETURN 
	

CONSTRUCTOR(oOwner) 
	
	
	SUPER()
	
	IF IsPtr(oOwner)
		oParent := __ForeignWindow{oOwner}
	ELSE
		oParent := oOwner
	ENDIF
	oOrigin := Point{0,0}
	
	aAlignes := {}
	
	//PP-030910
	SELF:SetBackgroundBrush()
	
	RETURN 
	
	
METHOD IsEnabled() 
	// DHer: 18/12/2008
RETURN IsWindowEnabled(hWnd)

METHOD IsIconic() 
	RETURN IsIconic(hWnd)
	

METHOD IsVisible() 
	RETURN IsWindowVisible(hWnd)
	

METHOD IsZoomed() 
	RETURN IsZoomed(hWnd)
	

METHOD KeyDown(oKeyEvent) 
	RETURN SELF:Default(oKeyEvent)
	

METHOD KeyUp(oKeyEvent) 
	RETURN SELF:Default(oKeyEvent)
	

METHOD LineTo(oPoint) 
    LOCAL dwLen, i AS DWORD
    LOCAL oPT AS Point                    
    LOCAL aPT AS ARRAY
    
    
    IF (hWnd != NULL_PTR) .OR. IsInstanceOf(SELF, #Printer)
        DCPenNeeded := TRUE
        IF (SELF:__GetDC() != NULL_PTR)
            IF !IsArray(oPoint)
                LineTo(hDC, oPoint:x, oPoint:y)
            ELSE      
                aPt := oPoint 
                dwLen := ALen(aPt)
                FOR i:=1 UPTO dwLen
                    oPt := aPt[i] 
                    LineTo(hDC, oPT:x, oPT:y)
                NEXT
            ENDIF
        ENDIF
    ENDIF
    
    RETURN SELF
	

METHOD ListBoxClick(oControlEvent) 
	RETURN SELF:Default(oControlEvent)
	

METHOD ListBoxSelect(oControlEvent) 
	
	
	RETURN SELF:Default(oControlEvent)
	

METHOD ListViewColumnClick(oListViewColumnClickEvent) 
	
	
	RETURN SELF:Default(oListViewColumnClickEvent)
	

METHOD ListViewItemChanged(oListViewItemEvent) 
	
	
	RETURN SELF:Default(oListViewItemEvent)
	

METHOD ListViewItemChanging(oListViewItemEvent) 
	
	
	RETURN SELF:Default(oListViewItemEvent)
	

METHOD ListViewItemDelete(oListViewDeleteEvent) 
	
	
	RETURN SELF:Default(oListViewDeleteEvent)
	

METHOD ListViewItemDrag(oListViewDragEvent) 
	LOCAL oControl AS ListView
	LOCAL oPoint AS Point 
	LOCAL oEvt :=oListViewDragEvent AS ListViewDragEvent
	
	oControl := OBJECT(oEvt:Control)
	
	IF oControl:DragDropEnabled
		IF oControl:DragImageList == NULL_OBJECT
			oDragImageList := oControl:__CreateDragImageList(oEvt:ListViewItem:ItemIndex)
			oDragImageList:BeginDrag(1)
		ELSE
			oDragImageList := oControl:DragImageList
			oDragImageList:BeginDrag(oEvt:ListViewItem:ImageIndex)
		ENDIF
		oPoint := oEvt:Position
		oPoint:X += oControl:Origin:X
		oPoint:Y += oControl:Origin:Y
		oDragImageList:DragEnter(oPoint, SELF)
		lDragActive := TRUE
		ShowCursor(FALSE)
		SetCapture(SELF:Handle())
	ENDIF
	
	RETURN SELF:Default(oEvt)
	

METHOD ListViewItemEdit(oListViewEditEvent) 
	
	
	RETURN SELF:Default(oListViewEditEvent)
	

METHOD ListViewKeyDown(oListViewKeyEvent) 
	
	
	RETURN SELF:Default(oListViewKeyEvent)
	

METHOD ListViewMouseButtonDoubleClick(oListViewMouseEvent) 
	
	
	RETURN SELF:Default(oListViewMouseEvent)
	

METHOD ListViewMouseButtonDown(oListViewMouseEvent) 
	// 	LOCAL oControl AS ListView
	
	
	
	//PP-040410 This is better handled in dispatch
	// 	IF oListViewMouseEvent:IsRightButton
	// 		oControl := oListViewMouseEvent:Control
	// 		IF oControl:ContextMenu != NULL_OBJECT
	// 			oControl:ContextMenu:ShowAsPopup(oControl)
	// 		ENDIF
	// 	ENDIF
	
	RETURN SELF:Default(oListViewMouseEvent)
	

ACCESS Menu 
	
	
	RETURN oMenu
	

ASSIGN Menu(oNewMenu) 
	//PP-031129 Changes to "correct" bug 158 removed, caused incorrect sizing elsewhere
	
	//PP-030910 Bug 158
	// LOCAL oSize AS Dimension
	// LOCAL oOrigin AS Point
	
	
	
	//PP-030910 Bug 158
	// oOrigin := SELF:Origin
	// oSize := SELF:Size
	
	oMenu := oNewMenu
	
	IF (oMenu == NULL_OBJECT)
		SELF:Accelerator := NULL_OBJECT
		SELF:ToolBar := NULL_OBJECT
		SetMenu(hWnd, 0)
	ELSE
		SELF:Accelerator := oMenu:Accelerator
		SELF:ToolBar := oMenu:ToolBar
		SetMenu(hWnd, oMenu:Handle())
	ENDIF
	
	//PP-030910 Bug 158
	// SELF:Origin := oOrigin
	// SELF:Size := oSize
	
	RETURN 
	

METHOD MenuCommand(oMenuCommandEvent) 
	
	
	RETURN SELF:Default(oMenuCommandEvent)
	

METHOD MenuInit(oMenuInitEvent) 
	
	
	RETURN SELF:Default(oMenuInitEvent)
	

METHOD MenuSelect(oMenuSelectEvent) 
	
	
	RETURN SELF:Default(oMenuSelectEvent)
	

METHOD MinMaxInfo(oMinMaxInfoEvent) 
	LOCAL oEvt := oMinMaxInfoEvent AS MinMaxInfoEvent
	//PP-040410 from S Ebert
	IF oMinSize != NULL_OBJECT
		oEvt:MinTrackSize := oMinSize
	ENDIF
	RETURN SELF
	

ACCESS MinSize 
	RETURN oMinSize
	

ASSIGN MinSize(oSize) 
	RETURN (oMinSize := oSize)
	
	

METHOD ModifyTrayIcon(oTrayIcon, dwID, sToolTip) 
	//PP-030902
	RETURN SELF:__UpdateTrayIcon(NIM_MODIFY,oTrayIcon,dwID,sToolTip)
	
	
	

METHOD MonthCalSelectionChanged(_oMonthCalSelectionEvent) 
	LOCAL oMonthCal AS MonthCalendar
	LOCAL oMonthCalSelectionEvent AS MonthCalSelectionEvent
	oMonthCalSelectionEvent := _oMonthCalSelectionEvent
	oMonthCal := (MonthCalendar) oMonthCalSelectionEvent:Control
	oMonthCal:Modified := TRUE
	IF oMonthCalSelectionEvent:Explicit
		oMonthCal:SetFocus()
	ENDIF

	IF IsInstanceOf(oMonthCal:Owner, #DataWindow)
		oMonthCal:Owner:__DoValidate(oMonthCal)
	ENDIF
	
	RETURN SELF:Default(oMonthCalSelectionEvent)
	

METHOD MouseButtonDoubleClick(oMouseEvent) 
	
	
	RETURN SELF:Default(oMouseEvent)
	

METHOD MouseButtonDown(oMouseEvent) 
	
	
	RETURN SELF:Default(oMouseEvent)
	

METHOD MouseButtonUp(oMouseEvent) 
	
	
	IF lDragActive
		ReleaseCapture()
		ShowCursor(TRUE)
		oDragImageList:EndDrag()
		oDragImageList := NULL_OBJECT
		lDragActive := FALSE
	ENDIF
	
	//PP-040410 This is better handled in dispatch
	// 	IF (oMouseEvent:ButtonID == BUTTONRIGHT) .and. (oContextMenu != NULL_OBJECT)
	// 		oContextMenu:ShowAsPopup(SELF)
	// 	ENDIF
	
	RETURN SELF:Default(oMouseEvent)
	

METHOD MouseDrag(oMouseEvent) 
	LOCAL oEvt := oMouseEvent AS MouseEvent
	
	IF lDragActive
		oDragImageList:DragMove(oEvt:Position)
		//ImageList_DragMove(oMouseEvent:Position:Y, oMouseEvent:Position:Y)
	ENDIF
	
	RETURN SELF:Default(oMouseEvent)
	

METHOD MouseMove(oMouseEvent) 
	
	
	RETURN SELF:Default(oMouseEvent)
	

METHOD MouseTrapOff() 
	
	
	ReleaseCapture()
	
	RETURN NIL
	

METHOD MouseTrapOn() 
	
	
	SetCapture(SELF:Handle(0))
	
	RETURN NIL
	

METHOD Move(oMoveEvent) 
	
	
	RETURN SELF:Default(oMoveEvent)
	

METHOD MoveTo(oPoint) 
	LOCAL winPoint IS _winPoint  // dcaton 070316 was _winsize
	IF (hWnd != NULL_PTR) .OR. IsInstanceOf(SELF, #Printer)
		DCPenNeeded := TRUE
		IF (SELF:__GetDC() != NULL_PTR)
			MoveToEx(hDC,oPoint:X,oPoint:Y, @winPoint)
			RETURN Point{winPoint:x,winPoint:y}
		ENDIF
	ENDIF
	
	RETURN Point{0, 0}
	

ACCESS Origin 
	
	
	RETURN __WCGetOrigin(SELF)
	

ASSIGN Origin(oPoint) 
	IF !IsInstanceOfUsual(oPoint, #Point)
		WCError{#Origin,#Window,__WCSTypeError,oPoint,1}:@@Throw()
	ENDIF
	SELF:oOrigin := Point{oPoint:x, oPoint:y}
	WCMoveWindow(SELF, SELF:oOrigin, SELF:Size, TRUE)
	
	RETURN 
	

ACCESS Owner 
	RETURN oParent


ASSIGN Owner(oWindow) 
	// DHer: 18/12/2008
	IF IsInstanceOfUsual(oWindow, #Window) 
	   SELF:oParent := oWindow
	   SetParent(SELF:Handle(),oWindow:Handle())
   ENDIF
RETURN 
	

ASSIGN OwnerAlignment(iNewVal) 
	//PP-040322 Assign from S Ebert
	LOCAL oFormWindow  	AS OBJECT
	LOCAL oWindow			AS WINDOW
	oFormWindow := SELF:Owner
	IF IsInstanceOf(SELF, #__FormFrame) //It's a SubDataWindow
		oFormWindow := oFormWindow:Owner
		IF IsInstanceOf(oFormWindow, #DataWindow)
			oFormWindow := oFormWindow:__GetFormSurface()
		ENDIF
	ENDIF
	IF IsInstanceOf(oFormWindow, #Window)  
		oWindow := oFormWindow
		oWindow:__AddAlign(SELF, iNewVal)
	ENDIF
	
	RETURN 
	

METHOD PaintBackground(hDC) 
	LOCAL strRect IS _winRECT
	LOCAL _hdc AS PTR
	LOCAL _handle AS PTR
	LOCAL hBrush AS PTR
	
	
	
	_handle := SELF:Handle(4)
	IF IsPtr(hDC)
		_hdc := hDC
	ELSE
		_hdc := GetDC(_handle)
	ENDIF
	
	IF (oBackground == NULL_OBJECT)
		hBrush := GetClassLong(_handle, GCL_HBRBACKGROUND)
		IF (hBrush == NULL_PTR)
			hBrush := GetSysColorBrush(COLOR_WINDOW)
		ENDIF
	ELSE
		hBrush := oBackground:Handle()
		oBackground:__SetBrushOrg(_hdc, _handle)
	ENDIF
	
	GetClientRect(_handle, @strRect)
	FillRect(_hdc, @strRect, hBrush)
	
	IF !IsPtr(hDC)
		ReleaseDC(_handle, _hdc)
	ENDIF
	
	RETURN NIL
	

METHOD PaintBoundingBox(oBoundingBox, kPaintMode) 
	LOCAL hBrush AS PTR
	LOCAL r IS _WinRect
	
	
	
	IF !IsInstanceOfUsual(oBoundingBox, #BoundingBox)
		WCError{#PaintBoundingBox,#Window,__WCSTypeError,oBoundingBox,1}:@@Throw()
	ENDIF
	
	IF oForeground == NULL_OBJECT
		hBrush:= GetStockObject(BLACK_BRUSH)
	ELSE
		hBrush := oForeground:Handle()
	ENDIF
	
	SELF:__GetDC()
	
	r:Left := oBoundingBox:Origin:X
	r:Top := oBoundingBox:Origin:Y
	r:Right := oBoundingBox:Extent:X
	r:Bottom := oBoundingBox:Extent:Y
	
	DO CASE
	CASE kPaintMode == PAINTFRAME
		FrameRect(hdc, @r, hBrush)
	CASE kPaintMode == PAINTINVERT
		InvertRect(hdc, @r)
	OTHERWISE
		FillRect(hdc, @r, hBrush)
	ENDCASE
	
	RETURN NIL
	

ACCESS Pen 
	
	
	RETURN oPen
	

ASSIGN Pen(oPen) 
	
	
	SELF:oPen := oPen
	DCPenNeeded := TRUE
	DCPenInUse := FALSE
	// this forces the new object to be selected into the current DC and allows proper releasing of the old one
	SELF:__GetDC()
	
	RETURN 
	

ACCESS Pointer 
	
	
	RETURN oPointer
	

ASSIGN Pointer(oNewPointer) 
	
	
	oPointer := oNewPointer
	
	IF oPointer != NULL_OBJECT
		SetCursor(oPointer:Handle())
	ELSE
		SetCursor(LoadCursor(0, IDC_ARROW))
	ENDIF
	
	RETURN 
	

METHOD PostInit() 
	RETURN SELF
	

METHOD PreInit() 
	RETURN SELF
	

METHOD Print(oDevice) 
	LOCAL hDIB AS PTR
	LOCAL lRet AS LOGIC
	LOCAL cDevice AS STRING
	LOCAL cDriver AS STRING
	LOCAL cPort AS STRING
	LOCAL ptrDevMode AS PTR
	LOCAL oPrintingDev AS PrintingDevice
	LOCAL hDCPrinter AS PTR
	LOCAL rc IS _winRECT
	LOCAL DocInfo IS _winDOCINFO
	
	IF !IsNil(oDevice)
		IF !IsInstanceOfUsual(oDevice, #PrintingDevice)
			WCError{#Init,#Printer,__WCSTypeError,oDevice,2}:@@Throw()
		ENDIF
		oPrintingDev := oDevice
	ELSE
		oPrintingDev := PrintingDevice{}
	ENDIF
	
	hDIB := SELF:__CreateSelfBitmap()
	
	IF (hDIB != NULL_PTR)
		cDevice 		:= oPrintingDev:Device
		cDriver 		:= oPrintingDev:Driver
		cPort 		:= oPrintingDev:Port
		ptrDevMode 	:= oPrintingDev:GetDevMode()
		
        hDCPrinter := CreateDC(String2Psz(cDriver), String2Psz(cDevice), String2Psz(cPort), ptrDevMode)
		
		IF (hDCPrinter != NULL_PTR)
			MemSet(@DocInfo, 0, _SIZEOF(_winDOCINFO))
			DocInfo:cbSize := _SIZEOF(_winDOCINFO)
            DocInfo:lpszDocName := String2Psz( "Visual Objects Print Job")
			
			StartDoc(hDCPrinter, @DocInfo)
			StartPage(hDCPrinter)
			
			SetMapMode(hDCPrinter, MM_TEXT)
			__WCGetPictureCoordinates(hWnd, hDCPrinter, @rc)
			lRet := __WCStretchDibBlt(hDCPrinter, rc:left, rc:top, rc:right - rc:left, rc:bottom - rc:top, hDib)
			
			EndPage(hDCPrinter)
			EndDoc(hDCPrinter)
		ENDIF
		
		GlobalFree(hDIB)
	ENDIF
	
	
	RETURN lRet
	

METHOD QueryClose(oEvent) 
	
	RETURN TRUE
	

METHOD RegisterTimer(nInterval, lOneTime) 
	
	
	IF !IsLong(nInterval)
		WCError{#RegisterTimer,#Window,__WCSTypeError,nInterval,1}:@@Throw()
	ENDIF
	
	IF !IsNil(lOneTime)
		IF !IsLogic(lOneTime)
			WCError{#RegisterTimer,#Window,__WCSTypeError,lOneTime,2}:@@Throw()
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
			__WCRegisterTimer(SELF)
			lTimerRegistered := TRUE
		ENDIF
	ELSE
		__WCUnregisterTimer(SELF)
		lTimerRegistered := FALSE
	ENDIF
	
	RETURN SELF
	

METHOD RePaint() 
	LOCAL _handle AS PTR
	
	
	
	_handle := SELF:handle(4)
	InvalidateRect(_handle, NULL_PTR, TRUE)
	
	RETURN NIL
	

METHOD RepaintBoundingBox(oBoundingBox) 
	LOCAL r IS _winRECT
	
	
	
	IF !IsNil(oBoundingBox)
		r:Left := oBoundingBox:Origin:X
		r:Top := SELF:CanvasArea:Height - oBoundingBox:Top
		r:Right := oBoundingBox:Extent:X
		r:Bottom := r:Top + oBoundingBox:Height
		InvalidateRect(SELF:handle(4), @r, TRUE)
	ENDIF
	
	RETURN NIL
	

METHOD Resize(oResizeEvent) 
	LOCAL oResEvt := oResizeEvent AS ResizeEvent
	LOCAL uRet AS USUAL
	
	
	
	DCInitialized := FALSE
	
	uRet := SELF:Default(oResEvt)
	
	IF IsInstanceOf(SELF:ToolBar, #ToolBar)
		SendMessage(SELF:ToolBar:Handle(), WM_SIZE, oResEvt:wParam, oResEvt:lParam)
	ENDIF
	
	
	SELF:__AlignControls()
	
	RETURN uRet
	

METHOD RichEditProtected(oRichEditProtectEvent) 
	
	
	SELF:EventReturnValue := 1
	RETURN 0
	

METHOD RichEditSelectionChange(oRichEditSelectionEvent) 
	
	
	RETURN SELF:Default(oRichEditSelectionEvent)
	

METHOD RichEditUndoLost(oControlNotifyEvent) 
	
	
	RETURN SELF:Default(oControlNotifyEvent)
	

METHOD Scroll(oDimension, oBoundingBox, lClip) 
	LOCAL oBB AS BoundingBox
	LOCAL strucRectScroll IS _WinRect
	LOCAL strucRectClip AS _WinRect
	LOCAL oPoint AS Point
	
	IF !IsInstanceOfUsual(oDimension,#Dimension)
		WCError{#Scroll,#Window,__WCSTypeError,oDimension,1}:@@Throw()
	ENDIF
	IF !IsNil(oBoundingBox)
		IF !IsInstanceOfUsual(oBoundingBox,#BoundingBox)
			WCError{#Scroll,#Window,__WCSTypeError,oBoundingBox,2}:@@Throw()
		ENDIF
		oBB:=oBoundingBox
	ELSE
		oBB:=SELF:CanvasArea
	ENDIF
	
	oPoint:=__WCConvertPoint(SELF,oBB:Origin)
	strucRectScroll:Left:=oPoint:X
	strucRectScroll:Bottom:=oPoint:Y
	oPoint:=__WCConvertPoint(SELF, Point{oBB:Right,oBB:Top} )
	strucRectScroll:Right:=oPoint:X
	strucRectScroll:Top:=oPoint:Y
	
	IF !IsNil(lClip)
		IF !IsLogic(lClip)
			WCError{#Scroll,#Window,__WCSTypeError,lClip,3}:@@Throw()
		ENDIF
		IF lClip
			strucRectClip:=@strucRectScroll
			//strucRectClip:=Ptr(_cast,strucRectScroll)
		ENDIF
	ELSE
		strucRectClip:=@strucRectScroll
		//strucRectClip:=Ptr(_cast,strucRectScroll)
	ENDIF
	
	ScrollWindow( hWnd, oDimension:Width, - oDimension:Height, @strucRectScroll, strucRectClip )
	
	RETURN SELF
	

METHOD SetAlignStartSize(oSize) 
	//PP-040729 Method from S Ebert
	LOCAL sRect IS _winRect
	
	//Use Null_Object for first init from Window:__AddAlign
	IF ALen(aAlignes) = 0 .OR. aAlignes[1,1] != NULL_OBJECT
		AAdd(aAlignes, NIL)
		IF ALen(aAlignes) > 1
			AIns(aAlignes, 1)
		ENDIF
		aAlignes[1] := {NULL_OBJECT, NIL}
		IF oSize == NULL_OBJECT
			oSize := NIL
		ENDIF
	ENDIF
	
	IF IsNil(oSize) .AND. ! IsObject(oSize)
		GetClientRect(SELF:Handle(4), @sRect)
		aAlignes[1,2] := Dimension{sRect:right - sRect:left, sRect:bottom - sRect:top}
	ELSEIF IsInstanceOfUsual(oSize, #Dimension)
		aAlignes[1,2] := oSize
	ENDIF
	RETURN SELF
	

METHOD SetBackgroundBrush(dwNew) 
	//PP-030910
	Default(@dwNew,COLOR_3DSHADOW)
	
	SetClassLong(SELF:handle(), GCL_HBRBACKGROUND, dwNew)
	RETURN SELF
	

METHOD SetExStyle(dwSetStyle, lEnable) 
	//PP-031129 New method to set window extended styles
	Default(@lEnable, TRUE)
	
	IF (hWnd != NULL_PTR)
		dwStyle := DWORD(_CAST, GetWindowLong(hWnd, GWL_EXSTYLE))
		
		IF lEnable
			dwStyle := _OR(dwStyle, DWORD(_CAST, dwSetStyle))
		ELSE
			dwStyle := _AND(dwStyle, _NOT(DWORD(_CAST, dwSetStyle)))
		ENDIF
		
		SetWindowLong(hWnd, GWL_EXSTYLE, LONGINT(_CAST, dwStyle))
	ENDIF
	
	RETURN dwStyle
	

METHOD SetFocus() 
	
	SetFocus(hWnd)
	RETURN SELF
	

METHOD SetHandle(hNewWnd) 
	
	hWnd := hNewWnd
	RETURN hWnd
	

METHOD SetStyle(dwSetStyle, lEnable) 
	
	
	Default(@lEnable, TRUE)
	
	IF (hWnd != NULL_PTR)
		dwStyle := DWORD(_CAST, GetWindowLong(hWnd, GWL_STYLE))
		
		IF lEnable
			dwStyle := _OR(dwStyle, DWORD(_CAST, dwSetStyle))
		ELSE
			dwStyle := _AND(dwStyle, _NOT(DWORD(_CAST, dwSetStyle)))
		ENDIF
		
		SetWindowLong(hWnd, GWL_STYLE, LONGINT(_CAST, dwStyle))
		UpdateWindow(hWnd)
	ENDIF
	
	RETURN dwStyle
	

METHOD Show(kShowState) 
	LOCAL nCmdShow AS INT
	LOCAL pszCaption AS PSZ
	LOCAL iParentX AS INT
	LOCAL iParentY AS INT
	LOCAL hParent AS PTR
	LOCAL r IS _winRECT
	
	
	
	Default(@kShowState, SHOWNORMAL)
	
	IF (NULL_STRING != cCaption)
		pszCaption := StringAlloc(cCaption)
		SetWindowText(hWnd, pszCaption)
		MemFree(pszCaption)
	ENDIF
	
	IF (kShowState == SHOWZOOMED)
		nCmdShow := SW_SHOWMAXIMIZED
	ELSEIF (kShowState == SHOWICONIZED)
		nCmdShow := SW_SHOWMINNOACTIVE
	ELSEIF (kShowState == SHOWINACTIVE)
		nCmdShow := SW_SHOWNOACTIVATE
	ELSE
		nCmdShow := SW_SHOWNORMAL
	ENDIF
	
	IF (kShowState == SHOWCENTERED)
		hParent := GetParent(SELF:Handle())
		IF (hParent != NULL_PTR)
			GetWindowRect(hParent, @r)
			iParentX := r:right - r:left
			iParentY := r:bottom - r:top
		ENDIF
		
		IF (hParent == NULL_PTR) .OR. (iParentX <= SELF:Size:Width) .OR. (iParentY <= SELF:Size:Height)
			iParentX := GetSystemMetrics(SM_CXSCREEN)
			iParentY := GetSystemMetrics(SM_CYSCREEN)
			SetRectEmpty(@r)
		ENDIF
		
		SetWindowPos(SELF:Handle(), NULL_PTR, r:left + (iParentX - SELF:Size:Width) / 2,;
			r:top + (iParentY - SELF:Size:Height) / 2, 0, 0, _OR(SWP_NOZORDER, SWP_NOSIZE, SWP_NOREDRAW))
	ENDIF
	
	ShowWindow(hWnd, nCmdShow)
	
	SELF:__AlignControls()
	
	RETURN NIL

METHOD ShowBalloonTrayTip(oTrayIcon,dwID,sHeading,sToolTip,dwTimeOut,dwInfo) 
	//PP-030902
	LOCAL nID IS _winNOTIFYICONDATA
	LOCAL uReturn AS USUAL
	//LOCAL aVersion AS ARRAY
	
	IF ! __LoadShellDll()
		RETURN FALSE
	ENDIF
	
	Default(@dwID,1)
	Default(@sHeading,"")
	Default(@sToolTip,"")
	Default(@dwInfo,NIIF_NONE)
	Default(@oTrayIcon,NULL_OBJECT)
	Default(@dwTimeOut,10000)
	
	uReturn := FALSE
	
	IF GetShellMajorVersion() >= 5
		nId:cbSize           := SizeOfNotifyIconData()
		nid:uTimeoutVersion:uTimeout := NOTIFYICON_VERSION
		PCALL(gpfnShell_NotifyIcon, NIM_SETVERSION, @NID)
		
		nId:hWnd := SELF:handle()
		NID:uID := dwId
		NID:uFlags := _OR(NIF_MESSAGE, NIF_ICON, NIF_INFO)
		NID:uCallbackMessage := TRAY_ICON_MSG
		NID:uTimeoutVersion:uTimeout := dwTimeOut
		IF ! oTrayIcon == NULL_OBJECT
			NID:hIcon := oTrayIcon:Handle()
		ENDIF
		nid:dwInfoFlags := dwInfo
		//RvdH 060608 optimized
		//IF !Empty(sToolTip)
		IF SLen(sToolTip) > 0
#ifdef __VULCAN__		
			MemCopy(@(NID:szInfo[1]), String2Psz(sToolTip), 256)
#else			
			MemCopy(@(NID:szInfo[1]), PTR(_CAST, sToolTip), 256)
#endif			
			NID:szInfo[256] := 0
		ENDIF
		//IF ! Empty(sHeading)
		IF SLen(sHeading) > 0
#ifdef __VULCAN__		
			MemCopy(@(NID:szInfoTitle[1]), String2Psz(sHeading), 64)
#else			
			MemCopy(@(NID:szInfoTitle[1]), PTR(_CAST, sHeading), 64)
#endif			
			NID:szInfoTitle[64] := 0
		ENDIF
		
		uReturn := PCALL(gpfnShell_NotifyIcon, NIM_MODIFY, @NID)
	ENDIF
	
	RETURN uReturn
	

ACCESS Size 
	LOCAL rect IS _WINRECT
	
	GetWindowRect(hWnd, @rect)
	
	RETURN Dimension{rect:right - rect:left, rect:bottom - rect:top}
	

ASSIGN Size(oDimension) 
	//RvdH 070428 In the past you could send in a BoundingBox and it worked
	//            mysteriously. Make sure we handle that as well (although it
	//				  is not documented to do so.
	IF IsInstanceOfUsual(oDimension, #BoundingBox)
		oDimension := oDimension:Size
	ENDIF
	IF !IsInstanceOfUsual(oDimension, #Dimension)
		WCError{#Size,#Window,__WCSTypeError,oDimension,1}:@@Throw()
	ENDIF

	WCMoveWindow(SELF, SELF:Origin, oDimension, TRUE)
	DCInitialized := FALSE
	
	RETURN 
	

METHOD SizeText(cTextToSize) 
	LOCAL winSize IS _winSize
	IF !IsString(cTextToSize)
		WCError{#SizeText,#Window,__WCSTypeError,cTextToSize,1}:@@Throw()
	ENDIF
	
	DCFontNeeded := TRUE
	IF SELF:__GetDC() != NULL_PTR
		GetTextExtentPoint32(hDC, String2Psz(cTextToSize), INT(_CAST, SLen(cTextToSize)), @winSize)
		RETURN Dimension{winSize:cx, winSize:cy}
	ENDIF
	
	RETURN Dimension{0,0}
	

METHOD @@StatusMessage(oHL, ntype) 
	
	
	RETURN NIL
	

METHOD SysLinkSelect(oSysLinkSelectEvent) 
	LOCAL li IS _winLITEM
	LOCAL i AS INT
	LOCAL oEvt := oSysLinkSelectEvent AS SysLinkSelectEvent
	
	IF SLen(oEvt:URL) > 0
        ShellExecute(NULL_PTR, String2Psz("open"), String2Psz(oEvt:URL), NULL, NULL_PTR, SW_SHOW)
		
		li:mask := _OR(LIF_ITEMINDEX, LIF_STATE)
		li:iLink := oEvt:LinkIndex
		li:stateMask := LIS_VISITED
		li:state := LIS_VISITED
		i:=SendMessage(oEvt:Control:Handle(), LM_SETITEM, 0, LONGINT(_CAST, @li))
	ENDIF
	RETURN SELF:Default(oEvt)
	

METHOD TabKeyDown(oControlNotifyEvent) 
	
	
	RETURN SELF:Default(oControlNotifyEvent)
	

METHOD TabSelect(oControlNotifyEvent) 
	
	
	RETURN SELF:Default(oControlNotifyEvent)
	

METHOD TabSelectionChanging(oControlNotifyEvent) 
	
	
	RETURN SELF:Default(oControlNotifyEvent)
	

ACCESS TextColor 
	
	
	RETURN oPen
	

ASSIGN TextColor(oNewPen) 
	
	
	SELF:Pen:= oNewPen
	
	RETURN 
	

METHOD TextPrint(cText, oPoint) 
	LOCAL strucLogBrush IS _WinLogBrush
	LOCAL iOldMode, iNewMode AS INT
	LOCAL dwOldBack AS DWORD
	LOCAL lUsingBrush AS LOGIC
	
	
	
	IF !IsString(cText)
		WCError{#TextPrint,#Window,__WCSTypeError,cText,1}:@@Throw()
	ENDIF
	IF !IsInstanceOfUsual(oPoint,#Point)
		WCError{#TextPrint,#Window,__WCSTypeError,oPoint,2}:@@Throw()
	ENDIF
	
	DCFontNeeded := TRUE
	DCPenNeeded := TRUE
	
	IF (SELF:__GetDC() != NULL_PTR)
		iNewMode := TRANSPARENT
		
		IF oForeground != NULL_OBJECT
			__WCLogicalBrush(oForeground, @strucLogBrush)
			IF strucLogBrush:lbStyle != BS_HOLLOW
				dwOldBack := SetBkColor(hDC, strucLogBrush:lbColor)
				iNewMode := OPAQUE
				lUsingBrush := TRUE
			ENDIF
		ENDIF
		iOldMode := SetBkMode(hDC, PTR(_CAST, iNewMode))
		TextOut(hDC, oPoint:x, oPoint:y, String2Psz(cText), INT(_CAST, SLen(cText)))
		SetBkMode(hDC, PTR(_CAST, iOldMode))
		IF lUsingBrush
			SetBkColor(hDC, dwOldBack)
		ENDIF
	ENDIF
	
	RETURN SELF
	

METHOD Timer() 
	
	
	RETURN SELF
	

ACCESS ToolBar 
	RETURN oToolBar
	

ASSIGN ToolBar(oNewToolBar) 
	
	IF (SELF:Menu != NULL_OBJECT) .AND. (SELF:menu:ToolBar != oNewToolBar)
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
	
	RETURN 
	

METHOD ToolBarHeightChanged(oControlNotifyEvent) 
	
	
	RETURN SELF:Default(oControlNotifyEvent)
	

METHOD ToTop() 
	
	
	BringWindowToTop(hWnd)
	
	RETURN NIL
	

METHOD TrayIconBalloonClicked(dwID) 
	//PP-030902
	RETURN NIL
	

METHOD TrayIconBalloonShown(dwID) 
	//PP-030902
	RETURN NIL
	

METHOD TrayIconBalloonTimeOut(dwID) 
	//PP-030902
	RETURN NIL
	

METHOD TrayIconClicked(dwID, lRightButton, lDoubleClick) 
	RETURN NIL
	

METHOD TreeViewItemDelete(oTreeViewDeleteEvent) 
	LOCAL strucTreeView AS _winNM_TreeView
	LOCAL oTreeView AS TreeView
	LOCAL oEvt := oTreeViewDeleteEvent AS TreeViewDeleteEvent
	
	strucTreeView := PTR(_CAST, oEvt:lParam)
	
	oTreeView := OBJECT(oEvt:Control)
	
	IF (oTreeView != NULL_OBJECT)
		oTreeView:__RemoveByHandle(strucTreeView:itemOld:hItem)
	ENDIF
	
	RETURN SELF:Default(oEvt)
	

METHOD TreeViewItemDrag(oTreeViewDragEvent) 
	LOCAL oControl AS TreeView
	LOCAL oPoint AS Point
	LOCAL oEvt := oTreeViewDragEvent AS TreeViewDragEvent
	
	
	
	oControl := OBJECT(oEvt:Control)
	
	IF oControl:DragDropEnabled
		IF oControl:DragImageList == NULL_OBJECT
			oDragImageList := oControl:__CreateDragImageList(oEvt:TreeViewItem:NameSym)
			oDragImageList:BeginDrag(1)
		ELSE
			oDragImageList := oControl:DragImageList
			oDragImageList:BeginDrag(oEvt:TreeViewItem:ImageIndex)
		ENDIF
		oPoint := oEvt:Position
		oPoint:X += oControl:Origin:X
		oPoint:Y += oControl:Origin:Y
		//PP-030505 Bug:126
		oDragImageList:DragEnter(oPoint, SELF)
		lDragActive := TRUE
		ShowCursor(FALSE)
		SetCapture(SELF:Handle())
	ENDIF
	
	RETURN SELF:Default(oEvt)
	
	

METHOD TreeViewItemEdit(oTreeViewEditEvent) 
	
	
	RETURN SELF:Default(oTreeViewEditEvent)
	

METHOD TreeViewItemExpanded(oTreeViewExpandedEvent) 
	
	
	RETURN SELF:Default(oTreeViewExpandedEvent)
	

METHOD TreeViewItemExpanding(oTreeViewExpandingEvent) 
	
	
	RETURN SELF:Default(oTreeViewExpandingEvent)
	

METHOD TreeViewKeyDown(oTreeViewKeyEvent) 
	
	
	RETURN SELF:Default(oTreeViewKeyEvent)
	

METHOD TreeViewMouseButtonDoubleClick(oTreeViewMouseEvent) 
	
	RETURN SELF:Default(oTreeViewMouseEvent)
	

METHOD TreeViewMouseButtonDown(oTreeViewMouseEvent) 
	// 	LOCAL oControl AS TreeView
	
	
	
	//PP-040425 This is better handled in dispatch
	// 	IF oTreeViewMouseEvent:IsRightButton
	// 		oControl := oTreeViewMouseEvent:Control
	// 		IF oControl:ContextMenu != NULL_OBJECT
	// 			oControl:ContextMenu:ShowAsPopup(oControl)
	// 		ENDIF
	// 	ENDIF
	
	RETURN SELF:Default(oTreeViewMouseEvent)
	

METHOD TreeViewSelectionChanged(oTreeViewSelectionEvent) 
	
	
	RETURN SELF:Default(oTreeViewSelectionEvent)
	

METHOD TreeViewSelectionChanging(oTreeViewSelectionEvent) 
	
	
	RETURN SELF:Default(oTreeViewSelectionEvent)
	

METHOD Update() 
	
	
	UpdateWindow(hWnd)
	
	RETURN NIL
	

METHOD VerticalScroll(oScrollEvent) 
	LOCAL oScrollBar AS ScrollBar
	LOCAL oEvt	:= oScrollEvent AS ScrollEvent
	
	oScrollBar := OBJECT(oEvt:ScrollBar)
	IF (oScrollBar != NULL_OBJECT)
		oScrollBar:ThumbPosition:=oEvt:Position
	ENDIF
	
	RETURN SELF:Default(oEvt)
	

METHOD VerticalSlide(oSliderEvent) 
	//local oSlider as Slider
	//oSlider:=oSliderEvent:Slider
	//if oSlider != NULL_OBJECT
	// oSlider:ThumbPosition:=oSliderEvent:Position
	//endif
	
	RETURN SELF:Default(oSliderEvent)
	

METHOD VerticalSpin(oSpinnerEvent) 
	LOCAL oSpinner AS Spinner
	LOCAL oEvt	:= oSpinnerEvent AS SpinnerEvent
	
	oSpinner := OBJECT(oEvt:Spinner)
	IF (oSpinner != NULL_OBJECT)
		oSpinner:Position:=oEvt:Position
	ENDIF
	
	RETURN SELF:Default(oEvt)
	

ACCESS WindowArea 
	LOCAL rect IS _WINRECT
	LOCAL oPoint AS Point
	LOCAL oDimension AS Dimension
	
	GetWindowRect(hWnd, @rect)
	oPoint := __WCConvertPoint(SELF:Owner, Point{rect:left, rect:top})
	oDimension := Dimension{rect:right - rect:left, rect:bottom - rect:top}
	
	RETURN BoundingBox{oPoint, oDimension}
END CLASS

_DLL FUNCTION AnimateWindow(HWND AS PTR, dwTime AS DWORD, dwFlags AS DWORD) AS LOGIC PASCAL:User32.AnimateWindow



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
