USING System
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI.Tests

    /// <summary>
    /// Integration tests for TextBox InputMask functionality
    /// Tests end-to-end behavior of InputMask with TextBox control
    /// </summary>
    PUBLIC CLASS TextBoxInputMaskTests
        
        PRIVATE _textBox AS TextBox
        PRIVATE _handler AS InputMaskHandler
        
        PUBLIC CONSTRUCTOR()
            SELF:_textBox := TextBox{}
            SELF:_handler := InputMaskHandler{}
        END CONSTRUCTOR
        
        // =====================================================================
        // TEST: Set InputMask Property
        // =====================================================================
        PUBLIC METHOD TestSetInputMaskProperty() AS LOGIC
            SELF:_textBox:InputMask := "(999) 999-9999"
            
            RETURN SELF:_textBox:InputMask == "(999) 999-9999"
        END METHOD
        
        // =====================================================================
        // TEST: Handler Initialize with Pattern
        // =====================================================================
        PUBLIC METHOD TestHandlerInitializePattern() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            
            VAR pattern := SELF:_handler:Pattern
            
            RETURN pattern != NIL .AND. pattern:IsValid() .AND. _
                   pattern:DataLength == 10
        END METHOD
        
        // =====================================================================
        // TEST: Get Data Value from Masked Input
        // =====================================================================
        PUBLIC METHOD TestGetDataValueFromMasked() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            
            VAR dataValue := SELF:_handler:GetDataValue("(555) 123-4567")
            
            RETURN dataValue == "5551234567"
        END METHOD
        
        // =====================================================================
        // TEST: Initialize TextBox Shows Placeholder
        // =====================================================================
        PUBLIC METHOD TestInitializeTextBoxShowsPlaceholder() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_handler:InitializeTextBox(SELF:_textBox)
            
            RETURN SELF:_textBox:Text == "(___) ___-____" .AND. _
                   SELF:_textBox:SelectionStart == 1  // First data position after (
        END METHOD
        
        // =====================================================================
        // TEST: Move to Next Data Position
        // =====================================================================
        PUBLIC METHOD TestMoveToNextDataPosition() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "(555) ___-____"
            SELF:_textBox:SelectionStart := 1
            
            SELF:_handler:MoveToNextDataPosition(SELF:_textBox)
            
            RETURN SELF:_textBox:SelectionStart == 2
        END METHOD
        
        // =====================================================================
        // TEST: Move to Previous Data Position
        // =====================================================================
        PUBLIC METHOD TestMoveToPreviousDataPosition() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "(555) ___-____"
            SELF:_textBox:SelectionStart := 2
            
            SELF:_handler:MoveToPreviousDataPosition(SELF:_textBox)
            
            RETURN SELF:_textBox:SelectionStart == 1
        END METHOD
        
        // =====================================================================
        // TEST: Validate Character - Valid Digit
        // =====================================================================
        PUBLIC METHOD TestValidateCharacterValidDigit() AS LOGIC
            SELF:_handler:SetPattern("999")
            
            VAR args := KeyPressEventArgs{'5'}
            SELF:_textBox:Text := "___"
            SELF:_textBox:SelectionStart := 0
            
            SELF:_handler:HandleKeyPress(args, SELF:_textBox)
            
            RETURN !args:Handled  // Valid character should not be handled (blocked)
        END METHOD
        
        // =====================================================================
        // TEST: Validate Character - Invalid Letter at Digit Position
        // =====================================================================
        PUBLIC METHOD TestValidateCharacterInvalidLetter() AS LOGIC
            SELF:_handler:SetPattern("999")
            
            VAR args := KeyPressEventArgs{'A'}
            SELF:_textBox:Text := "___"
            SELF:_textBox:SelectionStart := 0
            
            SELF:_handler:HandleKeyPress(args, SELF:_textBox)
            
            RETURN args:Handled  // Invalid character should be handled (blocked)
        END METHOD
        
        // =====================================================================
        // TEST: Handle Backspace Key
        // =====================================================================
        PUBLIC METHOD TestHandleBackspaceKey() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "(555) 123-4567"
            SELF:_textBox:SelectionStart := 7  // Position after second digit
            
            SELF:_handler:HandleBackspace(SELF:_textBox)
            
            // Backspace should have cleared the previous digit
            VAR extractedData := SELF:_handler:GetDataValue(SELF:_textBox:Text)
            RETURN extractedData:Length < 10
        END METHOD
        
        // =====================================================================
        // TEST: Handle Delete Key
        // =====================================================================
        PUBLIC METHOD TestHandleDeleteKey() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "(555) 123-4567"
            SELF:_textBox:SelectionStart := 1  // On first digit
            
            SELF:_handler:HandleDelete(SELF:_textBox)
            
            // Delete should have cleared the character at cursor
            VAR extractedData := SELF:_handler:GetDataValue(SELF:_textBox:Text)
            RETURN extractedData:Length < 10
        END METHOD
        
        // =====================================================================
        // TEST: Pattern with No InputMask
        // =====================================================================
        PUBLIC METHOD TestNoInputMask() AS LOGIC
            SELF:_handler:SetPattern("")
            
            RETURN SELF:_handler:Pattern == NIL
        END METHOD
        
        // =====================================================================
        // TEST: Handle Arrow Key - Right
        // =====================================================================
        PUBLIC METHOD TestHandleArrowKeyRight() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "(555) ___-____"
            SELF:_textBox:SelectionStart := 1
            
            SELF:_handler:HandleArrowKey(SELF:_textBox, Keys.Right)
            
            RETURN SELF:_textBox:SelectionStart == 2
        END METHOD
        
        // =====================================================================
        // TEST: Handle Arrow Key - Left
        // =====================================================================
        PUBLIC METHOD TestHandleArrowKeyLeft() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "(555) ___-____"
            SELF:_textBox:SelectionStart := 2
            
            SELF:_handler:HandleArrowKey(SELF:_textBox, Keys.Left)
            
            RETURN SELF:_textBox:SelectionStart == 1
        END METHOD
        
        // =====================================================================
        // TEST: Handle Arrow Key - Home (to First Data Position)
        // =====================================================================
        PUBLIC METHOD TestHandleArrowKeyHome() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "(555) 123-4567"
            SELF:_textBox:SelectionStart := 10
            
            SELF:_handler:HandleArrowKey(SELF:_textBox, Keys.Home)
            
            RETURN SELF:_textBox:SelectionStart == 1  // First data position
        END METHOD
        
        // =====================================================================
        // TEST: Handle Arrow Key - End
        // =====================================================================
        PUBLIC METHOD TestHandleArrowKeyEnd() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "(555) 123-4567"
            SELF:_textBox:SelectionStart := 1
            
            SELF:_handler:HandleArrowKey(SELF:_textBox, Keys.End)
            
            RETURN SELF:_textBox:SelectionStart == SELF:_textBox:Text:Length
        END METHOD
        
        // =====================================================================
        // TEST: Format Value After Text Changed
        // =====================================================================
        PUBLIC METHOD TestFormatValueAfterTextChanged() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "5551234567"
            
            SELF:_handler:HandleTextChanged(SELF:_textBox)
            
            RETURN SELF:_textBox:Text == "(555) 123-4567"
        END METHOD
        
        // =====================================================================
        // TEST: Partial Entry - Still Shows Placeholders
        // =====================================================================
        PUBLIC METHOD TestPartialEntryShowsPlaceholders() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "555"
            
            SELF:_handler:HandleTextChanged(SELF:_textBox)
            
            RETURN SELF:_textBox:Text == "(555) ___-____"
        END METHOD
        
        // =====================================================================
        // TEST: Skip Literal Positions When Navigating
        // =====================================================================
        PUBLIC METHOD TestSkipLiteralsWhenNavigating() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "(555) 123-4567"
            SELF:_textBox:SelectionStart := 3  // On closing parenthesis
            
            SELF:_handler:HandleArrowKey(SELF:_textBox, Keys.Right)
            
            // Should skip the parenthesis and space to next digit
            RETURN SELF:_textBox:SelectionStart == 6
        END METHOD
        
        // =====================================================================
        // TEST: Handle Tab Key - Move to Next Data Position
        // =====================================================================
        PUBLIC METHOD TestHandleTabKeyMovesNext() AS LOGIC
            SELF:_handler:SetPattern("(999) 999-9999")
            SELF:_textBox:Text := "(555) ___-____"
            SELF:_textBox:SelectionStart := 13  // Last digit position
            
            VAR args := KeyPressEventArgs{(CHAR)9}  // Tab character
            SELF:_handler:HandleKeyPress(args, SELF:_textBox)
            
            // Tab should move to next data position (or end)
            RETURN SELF:_textBox:SelectionStart > 13
        END METHOD
        
        // =====================================================================
        // TEST: Multiple Patterns in Sequence
        // =====================================================================
        PUBLIC METHOD TestMultiplePatternsSequence() AS LOGIC
            // First pattern
            SELF:_handler:SetPattern("(999) 999-9999")
            VAR pattern1 := SELF:_handler:Pattern
            
            // Change to second pattern
            SELF:_handler:SetPattern("999-99-9999")
            VAR pattern2 := SELF:_handler:Pattern
            
            RETURN pattern1:TotalLength == 14 .AND. pattern2:TotalLength == 11
        END METHOD
        
    END CLASS

END NAMESPACE
