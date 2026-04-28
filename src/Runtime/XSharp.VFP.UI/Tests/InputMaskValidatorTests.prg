USING System
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI.Tests

    /// <summary>
    /// Unit tests for InputMaskValidator
    /// Tests character validation, case transformations, and completion checks
    /// </summary>
    PUBLIC CLASS InputMaskValidatorTests
        
        PRIVATE _validator AS InputMaskValidator
        
        PUBLIC CONSTRUCTOR()
            SELF:_validator := InputMaskValidator{}
        END CONSTRUCTOR
        
        // =====================================================================
        // TEST: Validate Digit Character - Valid
        // =====================================================================
        PUBLIC METHOD TestValidateDigitCharacterValid() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            RETURN SELF:_validator:IsValidCharacter(pattern, 0, "5") .AND. _
                   SELF:_validator:IsValidCharacter(pattern, 1, "0") .AND. _
                   SELF:_validator:IsValidCharacter(pattern, 2, "9")
        END METHOD
        
        // =====================================================================
        // TEST: Validate Digit Character - Invalid
        // =====================================================================
        PUBLIC METHOD TestValidateDigitCharacterInvalid() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            RETURN !SELF:_validator:IsValidCharacter(pattern, 0, "A") .AND. _
                   !SELF:_validator:IsValidCharacter(pattern, 0, "a") .AND. _
                   !SELF:_validator:IsValidCharacter(pattern, 0, "!")
        END METHOD
        
        // =====================================================================
        // TEST: Validate Letter Character - Valid
        // =====================================================================
        PUBLIC METHOD TestValidateLetterCharacterValid() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("AAA")
            
            RETURN SELF:_validator:IsValidCharacter(pattern, 0, "A") .AND. _
                   SELF:_validator:IsValidCharacter(pattern, 1, "Z") .AND. _
                   SELF:_validator:IsValidCharacter(pattern, 2, "a")
        END METHOD
        
        // =====================================================================
        // TEST: Validate Letter Character - Invalid
        // =====================================================================
        PUBLIC METHOD TestValidateLetterCharacterInvalid() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("AAA")
            
            RETURN !SELF:_validator:IsValidCharacter(pattern, 0, "5") .AND. _
                   !SELF:_validator:IsValidCharacter(pattern, 0, "!")
        END METHOD
        
        // =====================================================================
        // TEST: Validate Alphanumeric Character
        // =====================================================================
        PUBLIC METHOD TestValidateAlphanumericCharacter() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("XXX")
            
            RETURN SELF:_validator:IsValidCharacter(pattern, 0, "A") .AND. _
                   SELF:_validator:IsValidCharacter(pattern, 0, "5") .AND. _
                   !SELF:_validator:IsValidCharacter(pattern, 0, "!")
        END METHOD
        
        // =====================================================================
        // TEST: Validate Literal Position - Cannot Type
        // =====================================================================
        PUBLIC METHOD TestValidateLiteralPositionCannotType() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999)")
            
            // Literal positions cannot accept typed input
            RETURN !SELF:_validator:IsValidCharacter(pattern, 0, "(") .AND. _
                   !SELF:_validator:IsValidCharacter(pattern, 4, ")")
        END METHOD
        
        // =====================================================================
        // TEST: Transform Character - Uppercase
        // =====================================================================
        PUBLIC METHOD TestTransformCharacterUppercase() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("!AAA")
            
            VAR transformed := SELF:_validator:TransformCharacter(pattern, 1, "a")
            
            RETURN transformed == "A"
        END METHOD
        
        // =====================================================================
        // TEST: Transform Character - Lowercase Optional
        // =====================================================================
        PUBLIC METHOD TestTransformCharacterLowercaseOptional() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("aaa")
            
            VAR transformed := SELF:_validator:TransformCharacter(pattern, 0, "A")
            
            RETURN transformed == "a"
        END METHOD
        
        // =====================================================================
        // TEST: Transform Character - No Transformation Needed
        // =====================================================================
        PUBLIC METHOD TestTransformCharacterNoTransformation() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            VAR transformed := SELF:_validator:TransformCharacter(pattern, 0, "5")
            
            RETURN transformed == "5"
        END METHOD
        
        // =====================================================================
        // TEST: Can Insert At Data Position
        // =====================================================================
        PUBLIC METHOD TestCanInsertAtDataPosition() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999")
            
            RETURN SELF:_validator:CanInsertAt(pattern, 1) .AND. _  // First digit
                   SELF:_validator:CanInsertAt(pattern, 2) .AND. _  // Second digit
                   SELF:_validator:CanInsertAt(pattern, 3)          // Third digit
        END METHOD
        
        // =====================================================================
        // TEST: Cannot Insert At Literal Position
        // =====================================================================
        PUBLIC METHOD TestCannotInsertAtLiteralPosition() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999")
            
            RETURN !SELF:_validator:CanInsertAt(pattern, 0) .AND. _  // (
                   !SELF:_validator:CanInsertAt(pattern, 4)          // )
        END METHOD
        
        // =====================================================================
        // TEST: Get Next Write Position
        // =====================================================================
        PUBLIC METHOD TestGetNextWritePosition() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999")
            
            VAR nextFromParenthesis := SELF:_validator:GetNextWritePosition(pattern, 0)
            
            RETURN nextFromParenthesis == 1  // Next write position is first digit
        END METHOD
        
        // =====================================================================
        // TEST: Is Complete - All Required Fields Filled
        // =====================================================================
        PUBLIC METHOD TestIsCompleteAllFieldsFilled() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            RETURN SELF:_validator:IsComplete(pattern, "555")
        END METHOD
        
        // =====================================================================
        // TEST: Is Complete - Some Fields Empty
        // =====================================================================
        PUBLIC METHOD TestIsCompletePartialFields() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            RETURN !SELF:_validator:IsComplete(pattern, "5__") .AND. _
                   !SELF:_validator:IsComplete(pattern, "55_")
        END METHOD
        
        // =====================================================================
        // TEST: Validate Entire Value - Valid
        // =====================================================================
        PUBLIC METHOD TestValidateEntireValueValid() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR validValue := "(555) 123-4567"
            
            RETURN SELF:_validator:ValidateValue(pattern, validValue)
        END METHOD
        
        // =====================================================================
        // TEST: Validate Entire Value - Invalid Length
        // =====================================================================
        PUBLIC METHOD TestValidateEntireValueInvalidLength() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR invalidValue := "(555) 123-456"  // Too short
            
            RETURN !SELF:_validator:ValidateValue(pattern, invalidValue)
        END METHOD
        
        // =====================================================================
        // TEST: Validate Entire Value - Wrong Literal
        // =====================================================================
        PUBLIC METHOD TestValidateEntireValueWrongLiteral() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR invalidValue := "[555] 123-4567"  // Wrong parenthesis
            
            RETURN !SELF:_validator:ValidateValue(pattern, invalidValue)
        END METHOD
        
        // =====================================================================
        // TEST: Optional Field - Can Be Partial
        // =====================================================================
        PUBLIC METHOD TestOptionalFieldCanBePartial() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("###-##")  // Optional digits
            
            // Should be incomplete since not all positions filled
            RETURN !SELF:_validator:IsComplete(pattern, "__-__")
        END METHOD
        
        // =====================================================================
        // TEST: Alphanumeric Lowercase Transform
        // =====================================================================
        PUBLIC METHOD TestAlphanumericLowercaseTransform() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("xxxx")
            
            VAR transformed := SELF:_validator:TransformCharacter(pattern, 0, "A")
            
            RETURN transformed == "a"
        END METHOD
        
        // =====================================================================
        // TEST: Uppercase Modifier on Multiple Characters
        // =====================================================================
        PUBLIC METHOD TestUppercaseModifierMultiple() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("!AAA-!BBB")
            
            VAR transform1 := SELF:_validator:TransformCharacter(pattern, 1, "a")
            VAR transform2 := SELF:_validator:TransformCharacter(pattern, 5, "b")
            
            RETURN transform1 == "A" .AND. transform2 == "B"
        END METHOD
        
        // =====================================================================
        // TEST: Mixed Digits and Letters Pattern
        // =====================================================================
        PUBLIC METHOD TestMixedDigitsAndLettersPattern() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("A9A9A9")
            
            RETURN SELF:_validator:IsValidCharacter(pattern, 0, "X") .AND. _  // Letter position
                   !SELF:_validator:IsValidCharacter(pattern, 0, "5") .AND. _  // Not digit
                   SELF:_validator:IsValidCharacter(pattern, 1, "5") .AND. _   // Digit position
                   !SELF:_validator:IsValidCharacter(pattern, 1, "X")          // Not letter
        END METHOD
        
    END CLASS

END NAMESPACE
