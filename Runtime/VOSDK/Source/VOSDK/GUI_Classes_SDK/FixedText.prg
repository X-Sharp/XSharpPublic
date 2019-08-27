CLASS FixedText INHERIT TextControl
	//PP-030915
	PROTECT _dwDrawStyle AS DWORD
	PROTECT _dwMargin AS DWORD
	EXPORT lUseDrawText AS LOGIC
	EXPORT lDrawThemeBackground AS LOGIC

METHOD __SetColors(_hDC AS PTR) AS PTR STRICT 
	//PP-031129
	IF SELF:lUseDrawText
		RETURN NULL_PTR
	ENDIF
	RETURN SUPER:__SetColors(_hDC)

METHOD __SetText(cNewText AS STRING) AS STRING STRICT 
	//PP-030915
	IF SELF:ValidateControl()
		//PP-040107
		SUPER:__SetText(cNewText)
		cCaption := cNewText
		InvalidateRect(hWnd,NULL_PTR,FALSE)
	ENDIF
	RETURN cNewText

METHOD Dispatch(oEvent) 
	//PP-040509 From S Ebert
	LOCAL oEvt := oEvent AS @@Event
	LOCAL uMsg   AS DWORD
	LOCAL sRect  IS _WinRect
	LOCAL struPS IS _WinPaintStruct
	LOCAL hDc, hFont AS PTR
	LOCAL dwDrawStyle AS DWORD
	LOCAL liSSStyle   AS LONGINT
	LOCAL hOwner AS PTR

	IF SELF:lUseDrawText
		uMsg := oEvt:uMsg
        SWITCH uMsg
        CASE WM_PAINT
        CASE WM_PRINTCLIENT
			IF uMsg == WM_PAINT
				hDC     := BeginPaint(hwnd, @struPS)
			ELSE
				hDC     := PTR(_CAST, oEvt:wParam)
			ENDIF
			GetClientRect(hWnd, @sRect)

			liSSStyle  := GetWindowLong(hWnd, GWL_STYLE)

			dwDrawStyle := _dwDrawStyle
			IF LOGIC(_CAST, _AND(liSSStyle, SS_CENTERIMAGE))
				//PP-040812 DT_VCENTER needs DT_SINGLELINE to work - see MS docs
				dwDrawStyle := _OR(dwDrawStyle, DT_VCENTER, DT_SINGLELINE)
			ENDIF
			IF LOGIC(_CAST, _AND(liSSStyle, SS_CENTER))
				dwDrawStyle := _OR(dwDrawStyle, DT_CENTER)
			ENDIF
			IF LOGIC(_CAST, _AND(liSSStyle, SS_RIGHT))
				dwDrawStyle := _OR(dwDrawStyle, DT_RIGHT)
			ENDIF
			IF LOGIC(_CAST, _AND(liSSStyle, SS_NOPREFIX))
				dwDrawStyle := _OR(dwDrawStyle, DT_NOPREFIX)
			ENDIF
			// RvdH 050602 Prevent wrapping (issue [12944] )
			IF LOGIC(_CAST, _AND(liSSStyle, SS_LEFTNOWORDWRAP))
				dwDrawStyle := _OR(dwDrawStyle, DT_SINGLELINE)
			ENDIF


			IF oControlBackground == NULL_OBJECT
				//PP-040317 Issue 12691
				IF IsThemeEnabled() .AND. SELF:lDrawThemeBackground
					DrawThemeParentBackground(hWnd, hDC, @sRect)
				ELSE
					IF _AND(GetWindowLong(hWnd, GWL_EXSTYLE), WS_EX_TRANSPARENT) = 0 // not transparent
						FillRect(hdc, @sRect, COLOR_3DSHADOW)
					ENDIF
				ENDIF
			ELSE
				oControlBackground:__SetBrushOrg(hdc, hwnd)
				FillRect(hdc, @sRect, oControlBackground:Handle())
			ENDIF

			SetBkMode(hDc, TRANSPARENT )

			IF (hFont := SendMessage(hwnd, WM_GETFONT, 0l, 0l)) != NULL_PTR
				SelectObject(hdc, hFont)
			ENDIF

			sRect:left  += LONGINT(_dwMargin)
			sRect:right -= LONGINT(_dwMargin)

			//PP-040416 Issue 12674, 12676 Fix from S Ebert
			IF LOGIC(_CAST, _AND(liSSStyle, WS_DISABLED))
				SetTextColor(hDC, GetSysColor(COLOR_3DHILIGHT))
				sRect:left   += 1
				sRect:top    += 1
				DrawText(hDc, String2Psz(cCaption), INT(SLen(cCaption)), @sRect, dwDrawStyle)
				sRect:left   -= 1
				sRect:top    -= 1
				SetTextColor(hDC, GetSysColor(COLOR_3DSHADOW))
			ELSE
				IF oTextColor != NULL_OBJECT
					SetTextColor(hDC, oTextColor:ColorRef)
				ELSE
					SetTextColor(hDC, GetSysColor(COLOR_WINDOWTEXT))
				ENDIF
			ENDIF
			DrawText(hDc, String2Psz(cCaption), INT(SLen(cCaption)), @sRect, dwDrawStyle)

			IF uMsg == WM_PAINT
				EndPaint(hWnd, @struPS)
			ENDIF
			SELF:EventReturnValue := 0l
			RETURN 1l
		CASE WM_ERASEBKGND
			SELF:EventReturnValue := 0l
			RETURN 1l
			//PP-040418 Issue 12676
		CASE WM_ENABLE
        CASE WM_SETTEXT
			hOwner := GetParent(hWnd)
			GetWindowRect(hWnd, @sRect)
#ifdef __VULCAN__	
      	MapWindowPoints(NULL_PTR, hOwner, (_winPOINT PTR) @sRect, 2)
#else	
	      MapWindowPoints(NULL_PTR, hOwner, @sRect, 2)
#endif
			InvalidateRect(hOwner, @sRect, TRUE)
		END SWITCH
	ENDIF

	RETURN SUPER:Dispatch(oEvt)


CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, lDataAware) 
	LOCAL cClass AS USUAL
	LOCAL lResID AS LOGIC

	
	//PP-030915. Text will be displayed using API DrawText(). Default style set below.
	// Standard styles will cause the appropriate draw style to be used.
	// DT_END_ELLIPSIS will put ... if text does not fit in space provided
	//PP-030923 Changed default value, removed SingleLiena and end ellipsis
	//PP-031002 Changed default draw style to DT_WORDBREAK (text was not wrapping)
	SELF:lUseDrawText := TRUE
	SELF:lDrawThemeBackground := TRUE

	//PP-040317. Issue 12806. Added DT_EXPANDTABS for default behaviour more like old fixed text
	SELF:_dwDrawStyle := _OR(DT_WORDBREAK,DT_EXPANDTABS)

	DEFAULT(@lDataAware, TRUE)
	lResID:=IsInstanceOfUsual(xID,#ResourceID)
	IF !lResID
		cClass:="Static"
	ENDIF

	SUPER(oOwner, xID, oPoint, oDimension, cClass, SS_Left, lDataAware)

	IF !lResID
		IF !IsNil(cText)
			cWindowName := cText
			SELF:Caption := cText
		ENDIF
	ENDIF

	RETURN 

ASSIGN Margin (nNewValue) 
	//PP-030915
	RETURN (_dwMargin := nNewValue)

METHOD SetDrawStyle(dwDrawStyle, lEnable) 
	//PP-030915 from S Ebert
    EnForceNumeric(@dwDrawStyle)
	IF IsLogic(lEnable)
		IF lEnable
			_dwDrawStyle :=  _OR(_dwDrawStyle, DWORD(_CAST, dwDrawStyle))
		ELSE
			_dwDrawStyle := _AND(_dwDrawStyle, _NOT(DWORD(_CAST, dwDrawStyle)))
		ENDIF
	ELSE
		_dwDrawStyle := dwDrawStyle
	ENDIF

	RETURN _dwDrawStyle

METHOD SetStandardStyle(kTextStyle) 
	LOCAL dwTempStyle, dwStyle AS DWORD
	LOCAL hHandle AS PTR

	
	IF !IsLong(kTextStyle)
		WCError{#SetStandardStyle,#FixedText,__WCSTypeError,kTextStyle,1}:Throw()
	ENDIF

	SWITCH (INT) kTextStyle
	CASE FT_LEFTALIGN
		dwTempStyle := SS_LEFT
	CASE FT_RIGHTALIGN
		dwTempStyle := SS_RIGHT
	CASE FT_CENTERED
		dwTempStyle := SS_CENTER
	END SWITCH

	hHandle := SELF:Handle()
	IF (hHandle != 0) .AND. IsWindow(hHandle)
		dwStyle := DWORD(GetWindowLong(hHandle, GWL_STYLE))
		dwStyle := _AND(dwStyle, _NOT(0x00000003U))
		dwStyle := _OR(dwStyle, dwTempStyle)
		SetWindowLong(hHandle, GWL_STYLE, LONGINT(_CAST, dwStyle))
	ENDIF

	RETURN dwStyle

END CLASS

