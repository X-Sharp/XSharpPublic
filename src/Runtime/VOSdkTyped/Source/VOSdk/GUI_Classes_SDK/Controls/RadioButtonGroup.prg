//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/RadioButtonGroup/*" />

using System.Collections.Generic
CLASS RadioButtonGroup INHERIT GroupBox
    PROTECTED enabledButtons  AS List<RadioButton>
    PROTECT wPressedButton AS LONG
    PROTECT aButtons AS List<RadioButton>
    PROTECT aValues AS ARRAY

    /// <include file="Gui.xml" path="doc/RadioButtonGroup.ctor/*" />
    CONSTRUCTOR(oOwner, xID, uPoint, uDimension, cText)
        SUPER(oOwner, xID, uPoint, uDimension, cText, TRUE)
        enabledButtons := List<RadioButton>{}
        aButtons	:= List<RadioButton>{}
        aValues		:= {}
        RETURN


    /// <exclude />
    METHOD __AlreadyHasFocus(oButton AS RadioButton) AS LOGIC STRICT
        LOCAL dwI, dwCount AS LONG

        dwCount := aButtons:Count
        FOR dwI := 1 UPTO dwCount
            IF aButtons[dwI-1] == oButton
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
        LOCAL dwI, dwCount AS LONG

        dwCount := aButtons:Count
        FOR dwI := 1 UPTO dwCount
            IF aButtons[dwI-1] == oButton
                __lModified := __lModified .OR. (dwI != wPressedButton)
                wPressedButton := dwI
                SELF:__Value := aValues[wPressedButton]
                RETURN
            ENDIF
        NEXT

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
        LOCAL dwI, dwCount AS LONG
        LOCAL cTempVal AS STRING
        LOCAL oRB AS RadioButton

        dwCount := aButtons:COunt
        FOR dwI := 1 UPTO dwCount
            oRB := aButtons[dwI-1]
            IF oRB:Pressed
                wPressedButton := dwI
                oRB:Pressed    := FALSE
            ENDIF
        NEXT

        cTempVal := AllTrim(AsString(uNewValue))
        dwCount := (LONG) ALen(aValues)
        FOR dwI := 1 UPTO dwCount
            IF AllTrim(AsString(aValues[dwI])) == cTempVal
                wPressedButton := dwI
                oRB := aButtons[wPressedButton-1]
                IF ! oRB:Pressed
                    oRB:Pressed := TRUE
                    EXIT
                ENDIF
            ENDIF
        NEXT  // dwI

        SELF:uValue := uNewValue

    /// <include file="Gui.xml" path="doc/RadioButtonGroup.Button/*" />
    METHOD Button(nButtonPosition as LONG) AS RadioButton
        // Return button at specified position in list
        // - Doesn't necessarily correspond to onscreen sequence
        // - returns NIL if button not found

        IF aButtons:Count >= nButtonPosition
            RETURN aButtons[nButtonPosition-1]
        ENDIF

        RETURN NULL

    /// <include file="Gui.xml" path="doc/RadioButtonGroup.Buttons/*" />
    ACCESS Buttons AS ARRAY
        Var result := {}
        FOREACH var oButton in SELF:aButtons
            AAdd(result, oButton)
        NEXT
        return result


    METHOD DisableAndSave() AS VOID STRICT
        SELF:enabledButtons:Clear()
        FOREACH VAR oItem IN aButtons
            IF oItem:__Control:Enabled
                oItem:Disable()
                SELF:enabledButtons:Add(oItem)
            ENDIF
        NEXT
        RETURN

    /// <include file="Gui.xml" path="doc/RadioButtonGroup.Disable/*" />
    METHOD Disable() AS VOID STRICT
        FOREACH VAR oItem IN aButtons
            oItem:Disable()
        NEXT
        SUPER:Disable()
        RETURN

    METHOD EnableAndRestore() AS VOID STRICT
        FOREACH VAR oItem IN SELF:EnabledButtons
            oItem:Enable()
        NEXT
        RETURN
    /// <include file="Gui.xml" path="doc/RadioButtonGroup.Enable/*" />

    METHOD Enable() AS VOID STRICT
        FOREACH VAR oItem IN aButtons
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
        aButtons := List<RadioButton>{}
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
                ELSEIF ! (uButton IS RadioButton)
                    WCError{#FillUsing,#RadioButtonGroup,__WCSTypeError,aContents,1}:Throw()
                ENDIF
                oButton := uButton
                aButtons:Add( oButton)
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
        // No need to hide the buttons. They are hidden with their parent
        SUPER:Hide()
        RETURN

    /// <include file="Gui.xml" path="doc/RadioButtonGroup.SetFocus/*" />
    METHOD SetFocus() AS VOID STRICT
        LOCAL oRB AS RadioButton
        IF wPressedButton != 0
            oRB := aButtons[wPressedButton-1]
            oRB:SetFocus()
        ENDIF

        RETURN

    /// <include file="Gui.xml" path="doc/RadioButtonGroup.Show/*" />
    METHOD Show()  AS VOID CLIPPER
        SUPER:Show()
        // No need to show the buttons. They are hidden with their parent
        RETURN

    /// <include file="Gui.xml" path="doc/RadioButtonGroup.TextValue/*" />
    ACCESS TextValue AS STRING
        LOCAL cRetVal AS STRING
        LOCAL dwI, dwCount AS LONG
        LOCAL oRB AS RadioButton
        IF wPressedButton == 0
            dwCount := aButtons:Count
            FOR dwI := 1 UPTO dwCount
                oRB := aButtons[dwI-1]
                IF oRB:Pressed
                    wPressedButton := dwI
                    EXIT
                ENDIF
            NEXT
        ENDIF
        IF (wPressedButton != 0)
            oRB			:= aButtons[ wPressedButton-1]
            cRetVal		:= oRB:Caption
            IF IsNil(cRetVal)
                cRetVal:=AsString(SELF:Value)
            ENDIF
        ENDIF

        RETURN cRetVal

    /// <include file="Gui.xml" path="doc/RadioButtonGroup.TextValue/*" />
    ASSIGN TextValue(cNewText AS STRING)
        LOCAL dwI, dwCount AS LONG
        LOCAL cText AS STRING
        LOCAL cOldValue AS STRING
        LOCAL oRB AS RadioButton

        cOldValue := AsString(uValue)
        cText := Upper(AllTrim(cNewText))

        dwCount := aButtons:Count
        FOR dwI := 1 UPTO dwCount
            oRB := aButtons[dwI-1]
            IF Upper(AllTrim(oRB:Caption)) == cText
                SELF:Value := aValues[dwI]
                SELF:ValueChanged := !(cOldValue == AsString(uValue))
                RETURN
            ENDIF
        NEXT  // dwI

        RETURN

    /// <include file="Gui.xml" path="doc/RadioButtonGroup.Value/*" />
    ACCESS Value AS USUAL
        RETURN SUPER:Value

    /// <include file="Gui.xml" path="doc/RadioButtonGroup.Value/*" />
    ASSIGN Value(uNewValue AS USUAL)
        SUPER:Value := uNewValue
        SELF:SetFocus()

        RETURN

    /// <include file="Gui.xml" path="doc/RadioButtonGroup.Values/*" />
    PROPERTY Values AS ARRAY GET aValues
END CLASS

