/// <include file="Gui.xml" path="doc/RadioButton/*" />
CLASS RadioButton INHERIT Button
	PROTECT lSavedPressed AS LOGIC

    /// <inheritdoc />
    PROPERTY ControlType AS ControlType GET ControlType.RadioButton

    /// <inheritdoc />
	METHOD OnHandleCreated(o AS OBJECT, e AS EventArgs) AS VOID
		SUPER:OnHandleCreated(o, e)
		GuiWin32.SetWindowLong(SELF:hWnd, GWL_STYLE, dwStyle)
		GuiWin32.SetWindowLong(SELF:hWnd, GWL_EXSTYLE, dwExStyle)
		RETURN

	ACCESS __RadioButton AS IVORadioButton
		RETURN (IVORadioButton ) oCtrl

/// <include file="Gui.xml" path="doc/RadioButton.Destroy/*" />
	METHOD Destroy() AS USUAL
		IF oCtrl != NULL_OBJECT .and. !oCtrl:IsDisposed
			lSavedPressed := SELF:Pressed
		ENDIF

		RETURN SUPER:Destroy()

/// <include file="Gui.xml" path="doc/RadioButton.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, kStyle)

		SUPER(oOwner, xID, oPoint, oDimension, cText, kStyle)
		IF !IsInstanceOfUsual(xID,#ResourceID)
			SELF:SetStyle(BS_RADIOBUTTON)
		ENDIF
		SELF:ValueChanged := FALSE

		RETURN

/// <include file="Gui.xml" path="doc/RadioButton.Pressed/*" />
	ACCESS Pressed  AS LOGIC
		IF SELF:ValidateControl()
			RETURN __RadioButton:Checked
		ELSE
			RETURN lSavedPressed
		ENDIF

	ASSIGN Pressed(lPressed AS LOGIC)

		IF SELF:ValidateControl()
			__RadioButton:Checked := lPressed
			__RadioButton:TabStop := lPressed
			SELF:Value := lPressed
		ENDIF

		RETURN

	ACCESS TextValue AS STRING
		LOCAL lTicked AS LOGIC
		LOCAL cTickValue AS STRING

		lTicked := SELF:Pressed
		IF SELF:oFieldSpec != NULL
			cTickValue := oFieldSpec:Transform(lTicked)
		ELSE
			cTickValue := AsString(lTicked)
		ENDIF

		RETURN cTickValue

/// <include file="Gui.xml" path="doc/RadioButton.TextValue/*" />
	ASSIGN TextValue(cNewValue  AS STRING)
		LOCAL lOldTicked AS LOGIC
		LOCAL lTicked AS LOGIC

		lOldTicked := SELF:Pressed

		IF SELF:oFieldSpec != NULL
			lTicked := oFieldSpec:Val(cNewValue)
		ELSE
			lTicked:=Unformat(cNewValue,"","L")
		ENDIF

		IF (lTicked != lOldTicked)
			SELF:Pressed := lTicked
			SELF:Modified := TRUE
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/RadioButton.Value/*" />
	ACCESS Value
		RETURN SELF:Pressed

END CLASS

