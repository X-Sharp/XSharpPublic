//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/RadioButton/*" />
CLASS RadioButton INHERIT Button
	PROTECT lSavedPressed AS LOGIC

    /// <inheritdoc />
    PROPERTY ControlType AS ControlType GET ControlType.RadioButton

    /// <inheritdoc />
	METHOD OnHandleCreated(o AS OBJECT, e AS EventArgs) AS VOID
		SUPER:OnHandleCreated(o, e)
		GuiWin32.SetWindowStyle(SELF:hWnd, dwStyle)
		GuiWin32.SetWindowExStyle(SELF:hWnd, dwExStyle)
		RETURN

	PROPERTY __RadioButton AS VORadioButton GET (VORadioButton) oCtrl

/// <include file="Gui.xml" path="doc/RadioButton.Destroy/*" />
	METHOD Destroy() AS USUAL clipper
		IF oCtrl != NULL_OBJECT .and. !oCtrl:IsDisposed
			lSavedPressed := SELF:Pressed
		ENDIF

		RETURN SUPER:Destroy()

/// <include file="Gui.xml" path="doc/RadioButton.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, kStyle)

		SUPER(oOwner, xID, oPoint, oDimension, cText, kStyle)
		IF ! (xID IS ResourceID)
			SELF:SetStyle(BS_RADIOBUTTON)
		ENDIF
		SELF:ValueChanged := FALSE

		RETURN

/// <include file="Gui.xml" path="doc/RadioButton.Pressed/*" />
	PROPERTY Pressed  AS LOGIC
	GET
		IF SELF:ValidateControl()
			RETURN __RadioButton:Checked
		ELSE
			RETURN lSavedPressed
		ENDIF
    END GET
	SET

		IF SELF:ValidateControl()
			__RadioButton:Checked := value
			__RadioButton:TabStop := value
			SELF:Value := value
		ENDIF

		RETURN
	END SET
	END PROPERTY
/// <include file="Gui.xml" path="doc/RadioButton.TextValue/*" />
	PROPERTY TextValue AS STRING
    GET
		LOCAL lTicked AS LOGIC
		LOCAL cTickValue AS STRING

		lTicked := SELF:Pressed
		IF SELF:oFieldSpec != NULL
			cTickValue := oFieldSpec:Transform(lTicked)
		ELSE
			cTickValue := AsString(lTicked)
		ENDIF

		RETURN cTickValue
    END GET
	SET
		LOCAL lOldTicked AS LOGIC
		LOCAL lTicked AS LOGIC

		lOldTicked := SELF:Pressed

		IF SELF:oFieldSpec != NULL
			lTicked := oFieldSpec:Val(Value)
		ELSE
			lTicked:=Unformat(value,"","L")
		ENDIF

		IF (lTicked != lOldTicked)
			SELF:Pressed := lTicked
			SELF:Modified := TRUE
		ENDIF

		RETURN
	END SET
	END PROPERTY

/// <include file="Gui.xml" path="doc/RadioButton.Value/*" />
	PROPERTY Value AS USUAL GET SELF:Pressed

END CLASS

