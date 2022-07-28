/// <include file="Gui.xml" path="doc/RadioButtonGroup/*" />
CLASS RadioButtonGroup INHERIT GroupBox
	PROTECT wPressedButton AS DWORD
	PROTECT aButtons AS ARRAY
	PROTECT aValues AS ARRAY

/// <include file="Gui.xml" path="doc/RadioButtonGroup.ctor/*" />
	CONSTRUCTOR(oOwner, xID, uPoint, uDimension, cText)
		SUPER(oOwner, xID, uPoint, uDimension, cText, TRUE)
		aButtons	:= {}
		aValues		:= {}
		RETURN


 /// <exclude />
	METHOD __AlreadyHasFocus(oButton AS RadioButton) AS LOGIC STRICT
		LOCAL dwI, dwCount AS DWORD

		dwCount := ALen(aButtons)
		FOR dwI := 1 UPTO dwCount
			IF aButtons[dwI] == oButton
				EXIT
			ENDIF
		NEXT  // dwI

		RETURN (dwI == wPressedButton)

 /// <exclude />
	METHOD __IsElement(oButton AS RadioButton) AS LOGIC STRICT
		FOREACH oItem AS RadioButton IN aButtons
			IF oItem == oButton
				RETURN TRUE
			ENDIF
		NEXT  // dwI

		RETURN FALSE

 /// <exclude />
	METHOD __SetOn(oButton AS RadioButton) AS VOID STRICT
		LOCAL dwI, dwCount AS DWORD

		dwCount := ALen(aButtons)
		FOR dwI := 1 UPTO dwCount
			IF aButtons[dwI] == oButton
				__lModified := __lModified .OR. (dwI != wPressedButton)
				wPressedButton := dwI
				SELF:__Value := aValues[wPressedButton]
				RETURN
			ENDIF
		NEXT  // dwI

		RETURN

 /// <exclude />
	METHOD __Update() AS VOID STRICT
		// Update is called to resynchronise Control:Value with control state
		// For RadioButtonGroups this is done when a button is pressed
		IF SELF:Modified
			SELF:Modified := FALSE
			SELF:ValueChanged := TRUE
		ENDIF
		RETURN

 /// <exclude />
	ASSIGN __Value(uNewValue AS USUAL)  STRICT
		LOCAL dwI, dwCount AS DWORD
		LOCAL cTempVal AS STRING
		LOCAL oRB AS RadioButton

		dwCount := ALen(aButtons)
		FOR dwI := 1 UPTO dwCount
			oRB := aButtons[dwI]
			IF oRB:Pressed
				wPressedButton := dwI
				oRB:Pressed := FALSE
			ENDIF
		NEXT

		cTempVal := AllTrim(AsString(uNewValue))
		dwCount := ALen(aValues)
		FOR dwI := 1 UPTO dwCount
			IF AllTrim(AsString(aValues[dwI])) == cTempVal
				wPressedButton := dwI
				oRB := aButtons[wPressedButton]
				IF ! oRB:Pressed
					oRB:Pressed := TRUE
					EXIT
				ENDIF
			ENDIF
		NEXT  // dwI

		SELF:uValue := uNewValue

/// <include file="Gui.xml" path="doc/RadioButtonGroup.Button/*" />
	METHOD Button(nButtonPosition)
		// Return button at specified position in list
		// - Doesn't necessarily correspond to onscreen sequence
		// - returns NIL if button not found

		IF ALen(aButtons) >= nButtonPosition
			RETURN aButtons[nButtonPosition]
		ENDIF

		RETURN NIL

/// <include file="Gui.xml" path="doc/RadioButtonGroup.Buttons/*" />
	ACCESS Buttons
		RETURN SELF:aButtons

	PROTECTED EnabledButtons := System.Collections.Generic.List<RadioButton>{} AS System.Collections.Generic.List<RadioButton>
	METHOD DisableAndSave() AS VOID STRICT
		SELF:EnabledButtons:Clear()
		FOREACH oItem AS RadioButton IN aButtons
			IF oItem:__Control:Enabled
				oItem:Disable()
				SELF:EnabledButtons:Add(oItem)
			ENDIF
		NEXT
		RETURN

/// <include file="Gui.xml" path="doc/RadioButtonGroup.Disable/*" />
	METHOD Disable() AS VOID STRICT
		FOREACH oItem AS RadioButton IN aButtons
			oItem:Disable()
		NEXT
		SUPER:Disable()
		RETURN

	METHOD EnableAndRestore() AS VOID STRICT
		FOREACH oItem AS RadioButton IN SELF:EnabledButtons
			oItem:Enable()
		NEXT
		RETURN
/// <include file="Gui.xml" path="doc/RadioButtonGroup.Enable/*" />

	METHOD Enable() AS VOID STRICT
		FOREACH oItem AS RadioButton IN aButtons
			oItem:Enable()
		NEXT
		SUPER:Enable()
		RETURN

/// <include file="Gui.xml" path="doc/RadioButtonGroup.FillUsing/*" />
	METHOD FillUsing(aContents)
		LOCAL wContentsLength AS DWORD
		LOCAL wElementLength AS DWORD
		LOCAL wIndex AS DWORD
		LOCAL uElement AS USUAL
		LOCAL uButton  AS USUAL
		LOCAL oButton  AS RadioButton
		LOCAL uDefValue AS USUAL

		IF !IsArray(aContents)
			WCError{#FillUsing,#RadioButtonGroup,__WCSTypeError,aContents,1}:Throw()
		ENDIF

		wContentsLength := ALen(aContents)
		aButtons := {}
		aValues := {}
		IF wContentsLength > 0
			FOR wIndex := 1 UPTO wContentsLength
				uElement := aContents[wIndex]

				IF IsArray(uElement)
					wElementLength := ALen(uElement)
					IF wElementLength = 2
						uButton := uElement[1]
						uDefValue := uElement[2]
					ELSEIF wElementLength = 1
						uButton := uElement[1]
						uDefValue := wIndex
					ELSE
						WCError{#FillUsing,#RadioButtonGroup,__WCSTypeError,aContents,1}:Throw()
					ENDIF
				ELSE
					uButton := uElement
					uDefValue := wIndex
				ENDIF

				IF IsNumeric(uButton) .OR. IsString(uButton)
					uButton := RadioButton{(OBJECT) SELF:Owner, ResourceID{uButton}}
				ELSEIF !IsInstanceOfUsual(uButton, #RadioButton)
					WCError{#FillUsing,#RadioButtonGroup,__WCSTypeError,aContents,1}:Throw()
				ENDIF
				oButton := uButton
				AAdd(aButtons, oButton)
				AAdd(aValues, uDefValue)
			NEXT
		ENDIF
		LOCAL nX, nY AS LONG
		nX := SELF:Origin:X
		nY := SELF:Origin:Y
		SELF:oCtrl:SuspendLayout()
		FOREACH oItem AS RadioButton IN aButtons
			IF oItem:__Control:Parent != SELF:__Control
				LOCAL oPoint := oItem:Origin AS Point
				oPoint:X -= nX
				oPoint:Y -= nY
				oItem:Origin := oPoint
				SELF:oCtrl:Controls:Add((System.Windows.Forms.Control) oItem:__Control)
			ENDIF
			oItem:Show()
		NEXT
		SELF:oCtrl:ResumeLayout(TRUE)

		RETURN SELF

/// <include file="Gui.xml" path="doc/RadioButtonGroup.Hide/*" />
	METHOD Hide()  AS VOID STRICT
	//	FOREACH oItem AS RadioButton IN aButtons
	//		oItem:Visible := FALSE
	//	NEXT

		SUPER:Hide()

		RETURN


/// <include file="Gui.xml" path="doc/RadioButtonGroup.SetFocus/*" />
	METHOD SetFocus() AS VOID STRICT
		LOCAL oRB AS RadioButton
		IF wPressedButton != 0
			oRB := aButtons[wPressedButton]
			oRB:SetFocus()
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/RadioButtonGroup.Show/*" />
	METHOD Show()  AS VOID STRICT
		SUPER:Show()
	//	FOREACH oC AS System.Windows.Forms.Control IN SELF:oCtrl:Controls
	//		oC:Visible := TRUE
	//	NEXT
		RETURN

/// <include file="Gui.xml" path="doc/RadioButtonGroup.TextValue/*" />
	ACCESS TextValue AS STRING
		LOCAL cRetVal AS STRING
		LOCAL dwI, dwCount AS DWORD
		LOCAL oRB AS RadioButton
		IF wPressedButton == 0
			dwCount := ALen(aButtons)
			FOR dwI := 1 UPTO dwCount
				oRB := aButtons[dwI]
				IF oRB:Pressed
					wPressedButton := dwI
					EXIT
				ENDIF
			NEXT
		ENDIF
		IF (wPressedButton != 0)
			oRB			:= aButtons[ wPressedButton]
			cRetVal		:= oRB:Caption
			IF IsNil(cRetVal)
				cRetVal:=AsString(SELF:Value)
			ENDIF
		ENDIF

		RETURN cRetVal

/// <include file="Gui.xml" path="doc/RadioButtonGroup.TextValue/*" />
	ASSIGN TextValue(cNewText AS STRING)
		LOCAL dwI, dwCount AS DWORD
		LOCAL cText AS STRING
		LOCAL cOldValue AS STRING
		LOCAL oRB AS RadioButton

		cOldValue := AsString(uValue)
		cText := Upper(AllTrim(cNewText))

		dwCount := ALen(aButtons)
		FOR dwI := 1 UPTO dwCount
			oRB := aButtons[dwI]
			IF Upper(AllTrim(oRB:Caption)) == cText
				SELF:Value := aValues[dwI]
				SELF:ValueChanged := !(cOldValue == AsString(uValue))
				RETURN
			ENDIF
		NEXT  // dwI

		RETURN

/// <include file="Gui.xml" path="doc/RadioButtonGroup.Value/*" />
	ACCESS Value
		RETURN SUPER:Value

/// <include file="Gui.xml" path="doc/RadioButtonGroup.Value/*" />
	ASSIGN Value(uNewValue)
		SUPER:Value := uNewValue
		SELF:SetFocus()

		RETURN

/// <include file="Gui.xml" path="doc/RadioButtonGroup.Values/*" />
	ACCESS Values AS ARRAY
		RETURN aValues
END CLASS

