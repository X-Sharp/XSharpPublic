


CLASS FixedText INHERIT TextControl
	PROTECT _dwDrawStyle AS DWORD
	PROTECT _dwMargin	 AS DWORD
	PROPERTY lUseDrawText AS LOGIC AUTO
	PROPERTY lDrawThemeBackground AS LOGIC AUTO

    PROPERTY ControlType AS Controltype GET ControlType.FixedText


	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, lDataAware) 
		LOCAL cClass AS USUAL
		LOCAL lResID AS LOGIC

		
		// Text will be displayed using API DrawText(). Default style set below.
		// Standard styles will cause the appropriate draw style to be used.
		// DT_END_ELLIPSIS will put ... if text does not fit in space provided
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


	ACCESS __Label AS VOOwnerDrawnLabel
		RETURN (VOOwnerDrawnLabel) oCtrl

	[Obsolete];
	METHOD __SetColors(_hDC AS IntPtr) AS IntPtr STRICT 
		RETURN IntPtr.Zero

	METHOD __SetText(cNewText AS STRING) AS STRING STRICT 
		//PP-030915
		IF SELF:ValidateControl()
			//PP-040107
			SUPER:__SetText(cNewText)
			cCaption := cNewText
			oCtrl:Invalidate()
		ENDIF
		RETURN cNewText

	METHOD OnPaint (e AS System.Windows.Forms.PaintEventArgs) AS LOGIC
		LOCAL liSSStyle AS LONG
		LOCAL dwDrawStyle AS System.Windows.Forms.TextFormatFlags
		LOCAL oRect AS System.Drawing.Rectangle
		IF ! SELF:lUseDrawText .or. oCtrl == NULL_OBJECT
			RETURN FALSE
		ENDIF
		oRect := oCtrl:ClientRectangle
		IF _dwMargin != 0
			LOCAL oBB AS BoundingBox
			oBB := oRect
			oBB:Left += _dwMargin
			oBB:Top  += _dwMargin
			oRect := oBB
		ENDIF
		liSSStyle := SELF:dwStyle
		
		dwDrawStyle := (System.Windows.Forms.TextFormatFlags) _dwDrawStyle
		IF LOGIC(_CAST, _AND(liSSStyle, SS_CENTERIMAGE))
			//PP-040812 DT_VCENTER needs DT_SINGLELINE to work - see MS docs
			dwDrawStyle |= System.Windows.Forms.TextFormatFlags.SingleLine | System.Windows.Forms.TextFormatFlags.VerticalCenter
		ENDIF
		IF _AND(liSSStyle, SS_CENTER) != 0
			dwDrawStyle |= System.Windows.Forms.TextFormatFlags.HorizontalCenter
		ENDIF
		IF _AND(liSSStyle, SS_RIGHT) != 0
			dwDrawStyle |= System.Windows.Forms.TextFormatFlags.Right
		ENDIF
		IF _AND(liSSStyle, SS_NOPREFIX) != 0
			dwDrawStyle |= System.Windows.Forms.TextFormatFlags.NoPrefix
		ENDIF
		// RvdH 050602 Prevent wrapping (issue [12944] )
		IF _AND(liSSStyle, SS_LEFTNOWORDWRAP) != 0
			dwDrawStyle |= System.Windows.Forms.TextFormatFlags.SingleLine
		ENDIF

		dwDrawStyle |= System.Windows.Forms.TextFormatFlags.NoPadding
		
		dwDrawStyle |= System.Windows.Forms.TextFormatFlags.PreserveGraphicsTranslateTransform | ;
			System.Windows.Forms.TextFormatFlags.PreserveGraphicsClipping
		
		LOCAL oCol AS System.Drawing.Color
		IF oCtrl:Enabled
			oCol := oCtrl:ForeColor
		ELSE
			oCol := System.Drawing.SystemColors.ControlDark
		ENDIF
		System.Windows.Forms.TextRenderer.DrawText(e:Graphics, cCaption, oCtrl:Font, oRect,oCol,dwDrawStyle)
		RETURN TRUE
	

	ASSIGN Margin (nNewValue) 
		(_dwMargin := nNewValue)
		SELF:oCtrl:Margin := System.Windows.Forms.Padding{(LONG) nNewValue}


	METHOD SetDrawStyle(dwDrawStyle, lEnable ) 

		IF IsLogic(lEnable)
			IF lEnable
				_dwDrawStyle :=  _OR(_dwDrawStyle, (DWORD) dwDrawStyle)
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
			WCError{#SetStandardStyle,#FixedText,__WCSTypeError,kTextStyle,1}:@@Throw()
		ENDIF

		DO CASE
		CASE (kTextStyle == FT_LEFTALIGN)
			dwTempStyle := SS_LEFT
		CASE (kTextStyle == FT_RIGHTALIGN)
			dwTempStyle := SS_RIGHT
		CASE (kTextStyle == FT_CENTERED)
			dwTempStyle := SS_CENTER
		ENDCASE

		hHandle := SELF:Handle()
		IF oCtrl != NULL_OBJECT .and. ! oCtrl:IsDisposed
			dwStyle := DWORD(GuiWin32.GetWindowLong(hHandle, GWL_STYLE))
			dwStyle := _AND(dwStyle, _NOT(0X00000003U))
			dwStyle := _OR(dwStyle, dwTempStyle)
			GuiWin32.SetWindowLong(hHandle, GWL_STYLE, LONGINT(_CAST, dwStyle))
		ENDIF

		RETURN dwStyle

END CLASS

