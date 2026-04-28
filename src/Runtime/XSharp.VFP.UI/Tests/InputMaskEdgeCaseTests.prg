USING System
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI.Tests

    /// <summary>
    /// Edge case tests for InputMask functionality
    /// Tests boundary conditions, special scenarios, and error handling
    /// </summary>
    PUBLIC CLASS InputMaskEdgeCaseTests
        
        PRIVATE _formatter AS InputMaskFormatter
        PRIVATE _validator AS InputMaskValidator
        PRIVATE _handler AS InputMaskHandler
        
        PUBLIC CONSTRUCTOR()
            SELF:_formatter := InputMaskFormatter{}
            SELF:_validator := InputMaskValidator{}
            SELF:_handler := InputMaskHandler{}
        END CONSTRUCTOR
        
        // =====================================================================
        // TEST: Very Long Pattern
        // =====================================================================
        PUBLIC METHOD TestVeryLongPattern() AS LOGIC
            VAR pattern := InputMaskPattern{}
            VAR longMask := "9999-9999-9999-9999"
            pattern:Parse(longMask)
            
            RETURN pattern:IsValid() .AND. pattern:DataLength == 16
        END METHOD
        
        // =====================================================================
        // TEST: Pattern with Only Literals
        // =====================================================================
        PUBLIC METHOD TestPatternOnlyLiterals() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("---")
            
            // Pattern with no data positions should be invalid
            RETURN !pattern:IsValid()
        END METHOD
        
        // =====================================================================
        // TEST: Empty Data Extraction
        // =====================================================================
        PUBLIC METHOD TestEmptyDataExtraction() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR extracted := SELF:_formatter:ExtractDataValue(pattern, "(___) ___-____")
            
            RETURN extracted == ""
        END METHOD
        
        // =====================================================================
        // TEST: Nil Pattern Handling
        // =====================================================================
        PUBLIC METHOD TestNilPatternHandling() AS LOGIC
            VAR validator := SELF:_validator
            
            // Should not crash with nil pattern
            RETURN !validator:IsValidCharacter(NIL, 0, "5") .AND. _
                   !validator:CanInsertAt(NIL, 0)
        END METHOD
        
        // =====================================================================
        // TEST: Out of Bounds Position
        // =====================================================================
        PUBLIC METHOD TestOutOfBoundsPosition() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            RETURN pattern:GetCharacterType(10) == "invalid" .AND. _
                   !pattern:IsLiteralPosition(10)
        END METHOD
        
        // =====================================================================
        // TEST: Negative Position Index
        // =====================================================================
        PUBLIC METHOD TestNegativePositionIndex() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            RETURN pattern:GetCharacterType(-1) == "invalid" .AND. _
                   !SELF:_validator:CanInsertAt(pattern, -1)
        END METHOD
        
        // =====================================================================
        // TEST: Format with Extra Data
        // =====================================================================
        PUBLIC METHOD TestFormatWithExtraData() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            VAR formatted := SELF:_formatter:FormatValue(pattern, "12345")
            
            // Should only format first 3 digits
            RETURN formatted == "123"
        END METHOD
        
        // =====================================================================
        // TEST: Extract from Partial Data
        // =====================================================================
        PUBLIC METHOD TestExtractFromPartialData() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR extracted := SELF:_formatter:ExtractDataValue(pattern, "(555) 12_-____")
            
            RETURN extracted == "55512"
        END METHOD
        
        // =====================================================================
        // TEST: Case Transformation with Special Characters
        // =====================================================================
        PUBLIC METHOD TestCaseTransformWithSpecialChars() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("!AAA")
            
            VAR transformed := SELF:_validator:TransformCharacter(pattern, 1, "a")
            
            RETURN transformed == "A"
        END METHOD
        
        // =====================================================================
        // TEST: Pattern at Boundary - First Position
        // =====================================================================
        PUBLIC METHOD TestPatternFirstPosition() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999)")
            
            VAR firstData := pattern:GetFirstDataPosition()
            
            RETURN firstData == 1  // First digit after opening parenthesis
        END METHOD
        
        // =====================================================================
        // TEST: Pattern at Boundary - Last Position
        // =====================================================================
        PUBLIC METHOD TestPatternLastPosition() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999)")
            
            VAR lastDigit := pattern:GetCharacterType(3)
            VAR nextAfterLast := pattern:GetNextDataPosition(3)
            
            RETURN lastDigit == "digit" .AND. nextAfterLast == -1
        END METHOD
        
        // =====================================================================
        // TEST: Consecutive Literals
        // =====================================================================
        PUBLIC METHOD TestConsecutiveLiterals() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("9--9")  // Two consecutive dashes
            
            RETURN pattern:IsLiteralPosition(1) .AND. _
                   pattern:IsLiteralPosition(2)
        END METHOD
        
        // =====================================================================
        // TEST: Only Optional Digits Pattern
        // =====================================================================
        PUBLIC METHOD TestOnlyOptionalDigits() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("###")
            
            // Optional pattern might not be "complete" even when empty
            RETURN pattern:DataLength == 3 .AND. _
                   !SELF:_validator:IsComplete(pattern, "___")
        END METHOD
        
        // =====================================================================
        // TEST: Mix of Required and Optional
        // =====================================================================
        PUBLIC METHOD TestMixRequiredOptional() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("9#9")  // Required, optional, required
            
            RETURN pattern:Positions[0]:Required .AND. _
                   !pattern:Positions[1]:Required .AND. _
                   pattern:Positions[2]:Required
        END METHOD
        
        // =====================================================================
        // TEST: Handler without Pattern
        // =====================================================================
        PUBLIC METHOD TestHandlerWithoutPattern() AS LOGIC
            VAR textBox := TextBox{}
            textBox:Text := "test"
            
            // Should not crash when pattern is nil
            SELF:_handler:HandleTextChanged(textBox)
            
            RETURN textBox:Text == "test"  // Unchanged
        END METHOD
        
        // =====================================================================
        // TEST: Recursion Prevention Flag
        // =====================================================================
        PUBLIC METHOD TestRecursionPreventionFlag() AS LOGIC
            SELF:_handler:SetPattern("999")
            
            VAR textBox := TextBox{}
            textBox:Text := "___"
            
            // Calling HandleTextChanged should set and clear the flag
            SELF:_handler:HandleTextChanged(textBox)
            
            // If recursion prevention worked, this completes without hanging
            RETURN TRUE
        END METHOD
        
        // =====================================================================
        // TEST: Empty Mask String
        // =====================================================================
        PUBLIC METHOD TestEmptyMaskString() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("")
            
            RETURN !pattern:IsValid()
        END METHOD
        
        // =====================================================================
        // TEST: Whitespace-Only Mask
        // =====================================================================
        PUBLIC METHOD TestWhitespaceOnlyMask() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("   ")
            
            // All whitespace = all literals, no data
            RETURN !pattern:IsValid()
        END METHOD
        
        // =====================================================================
        // TEST: Unicode Characters in Literal
        // =====================================================================
        PUBLIC METHOD TestUnicodeCharacterInLiteral() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("9–9")  // En-dash instead of regular dash
            
            VAR literal := pattern:GetLiteralAt(1)
            
            RETURN literal:Length > 0
        END METHOD
        
        // =====================================================================
        // TEST: Cursor Beyond Mask Length
        // =====================================================================
        PUBLIC METHOD TestCursorBeyondMaskLength() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            VAR textBox := TextBox{}
            textBox:Text := "123"
            textBox:SelectionStart := 10  // Beyond 3-char mask
            
            // Should handle gracefully
            SELF:_handler:SetPattern("999")
            SELF:_handler:HandleArrowKey(textBox, Keys.Home)
            
            RETURN textBox:SelectionStart == 0
        END METHOD
        
        // =====================================================================
        // TEST: Delete at End of Input
        // =====================================================================
        PUBLIC METHOD TestDeleteAtEndOfInput() AS LOGIC
            SELF:_handler:SetPattern("999")
            
            VAR textBox := TextBox{}
            textBox:Text := "123"
            textBox:SelectionStart := 3  // End of input
            
            SELF:_handler:HandleDelete(textBox)
            
            // Should not crash
            RETURN TRUE
        END METHOD
        
        // =====================================================================
        // TEST: Backspace at Start of Input
        // =====================================================================
        PUBLIC METHOD TestBackspaceAtStartOfInput() AS LOGIC
            SELF:_handler:SetPattern("999")
            
            VAR textBox := TextBox{}
            textBox:Text := "123"
            textBox:SelectionStart := 0  // Start of input
            
            SELF:_handler:HandleBackspace(textBox)
            
            // Should not crash
            RETURN TRUE
        END METHOD
        
    END CLASS

END NAMESPACE
