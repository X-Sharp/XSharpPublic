CLASS RadioButtonGroup INHERIT GroupBox
	PROTECT wPressedButton AS DWORD
	PROTECT aButtons AS ARRAY
	PROTECT aValues AS ARRAY

	//PP-030828 Strong typing
	METHOD __AlreadyHasFocus(oButton AS RadioButton) AS LOGIC STRICT 
	//SE-060526
	LOCAL dwI, dwCount AS DWORD

	dwCount := ALen(aButtons)
	FOR dwI := 1 UPTO dwCount
	   IF aButtons[dwI] == oButton
	   	EXIT
	   ENDIF
	NEXT  // dwI

   RETURN (dwI == wPressedButton)

METHOD __IsElement(oButton AS RadioButton) AS LOGIC STRICT 
	//SE-060526
	LOCAL dwI, dwCount AS DWORD

	dwCount := ALen(aButtons)
	FOR dwI := 1 UPTO dwCount
	   IF aButtons[dwI] == oButton
	   	RETURN TRUE
	   ENDIF
	NEXT  // dwI

   RETURN FALSE

METHOD __SetOn(oButton AS RadioButton) AS VOID STRICT 
	//SE-060526
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

METHOD __Update() AS Control STRICT 
	//PP-030828 Strong typing
	// Update is called to resynchronise Control:Value with control state
	// For RadioButtonGroups this is done when a button is pressed
	

	IF SELF:Modified
		SELF:Modified := FALSE
		SELF:ValueChanged := TRUE
	ENDIF
	RETURN SELF

ASSIGN __Value(uNewValue AS USUAL)  STRICT 
	LOCAL dwI, dwCount AS DWORD
    LOCAL cTempVal AS STRING
    LOCAL oRB AS RadioButton

	

	dwCount := ALen(aButtons)
	FOR dwI := 1 UPTO dwCount
	    oRb := aButtons[dwI]
	    IF oRB:Pressed
	   	    wPressedButton := dwI
	   	    oRB:Pressed := FALSE
	   	    EXIT
	    ENDIF
	NEXT  // dwI

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

	RETURN SELF:uValue := uNewValue

METHOD Button(nButtonPosition) 
	// Return button at specified position in list
	// - Doesn't necessarily correspond to onscreen sequence
	// - returns NIL if button not found

	IF ALen(aButtons) >= nButtonPosition
		RETURN aButtons[nButtonPosition]
	ENDIF

	RETURN NIL

ACCESS Buttons 
	// DHer: 18/12/2008
RETURN SELF:aButtons

METHOD Disable() 
	
   //RvdH 060608 optimized
	//IF !Empty(aButtons)
	IF ALen(aButtons) > 0
		ASend(aButtons, #Disable)
	ENDIF
	SUPER:Disable()

	RETURN SELF

METHOD Enable() 
	

   //RvdH 060608 optimized
	//IF !Empty(aButtons)
	IF ALen(aButtons) > 0
		ASend(aButtons, #Enable)
	ENDIF
	SUPER:Enable()

	RETURN SELF

METHOD FillUsing(aContents) 
	LOCAL wContentsLength AS DWORD
	LOCAL wElementLength AS DWORD
	LOCAL wIndex AS DWORD
	LOCAL uElement AS USUAL
	LOCAL uButton AS USUAL
	LOCAL uDefValue AS USUAL

	

	IF !IsArray(aContents)
		WCError{#FillUsing,#RadioButtonGroup,__WCSTypeError,aContents,1}:@@Throw()
	ENDIF

	wContentsLength := ALen(aContents)
	// ASend(aButtons,#Destroy) // ???
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
					WCError{#FillUsing,#RadioButtonGroup,__WCSTypeError,aContents,1}:@@Throw()
				ENDIF
			ELSE
				uButton := uElement
				uDefValue := wIndex
			ENDIF

			IF IsNumeric(uButton) .OR. IsString(uButton)
				uButton := RadioButton{oFormSurface, ResourceID{uButton}}
			ELSEIF !IsInstanceOfUsual(uButton, #RadioButton)
				WCError{#FillUsing,#RadioButtonGroup,__WCSTypeError,aContents,1}:@@Throw()
			ENDIF

			AAdd(aButtons, uButton)
			AAdd(aValues, uDefValue)
		NEXT
	ENDIF

	RETURN SELF

METHOD Hide() 
	

   //RvdH 060608 optimized
	//IF !Empty(aButtons)
	IF ALen(aButtons) > 0
		ASend(aButtons, #Hide)
	ENDIF
	SUPER:Hide()

	RETURN SELF

CONSTRUCTOR(oOwner, xID, uPoint, uDimension, cText) 
	

	SUPER(oOwner, xID, uPoint, uDimension, cText, TRUE)
	aButtons := {}
	aValues := {}

	RETURN 

METHOD SetFocus() 
    LOCAL oRB AS RadioButton
	IF wPressedButton != 0     
	    oRB := aButtons[wPressedButton] 
		oRB:SetFocus()
	ENDIF

	RETURN SELF

METHOD Show() 
	

   //RvdH 060608 optimized
	//IF !Empty(aButtons)
	IF ALen(aButtons) > 0
		ASend(aButtons, #Show)
	ENDIF
	SUPER:Show()

	RETURN SELF

ACCESS TextValue 
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
        NEXT //dwI
    ENDIF

    IF (wPressedButton != 0)
        oRB := aButtons[wPressedButton] 
        cRetVal := oRB:Caption
        IF IsNil(cRetVal)
            cRetVal:=AsString(SELF:value)
        ENDIF
    ENDIF

    RETURN cRetVal
    
ASSIGN TextValue(cNewText) 
	//SE-060526
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
			RETURN cNewText
	   ENDIF
	NEXT  // dwI

	RETURN 

ACCESS Value 
	RETURN SUPER:Value

ASSIGN Value(uNewValue) 
	SUPER:Value := uNewValue
	SELF:SetFocus()

	RETURN 

END CLASS

