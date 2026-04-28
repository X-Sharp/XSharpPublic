USING System
USING System.Collections.Generic
USING System.Linq

BEGIN NAMESPACE XSharp.VFP.UI.Tests

    /// <summary>
    /// Unit tests for InputMaskPattern parser
    /// Tests pattern parsing, position identification, and navigation
    /// </summary>
    PUBLIC CLASS InputMaskPatternTests
        
        // =====================================================================
        // TEST: Parse Phone Number Pattern
        // =====================================================================
        PUBLIC METHOD TestParsePhonePattern() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            RETURN pattern:MaskString == "(999) 999-9999" .AND. _
                   pattern:TotalLength == 14 .AND. _
                   pattern:DataLength == 10 .AND. _
                   pattern:Positions:Count == 14
        END METHOD
        
        // =====================================================================
        // TEST: Parse SSN Pattern
        // =====================================================================
        PUBLIC METHOD TestParseSSNPattern() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999-99-9999")
            
            RETURN pattern:DataLength == 9 .AND. _
                   pattern:TotalLength == 11 .AND. _
                   pattern:Positions:Count == 11
        END METHOD
        
        // =====================================================================
        // TEST: Parse Date Pattern
        // =====================================================================
        PUBLIC METHOD TestParseDatePattern() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("99/99/9999")
            
            RETURN pattern:DataLength == 8 .AND. _
                   pattern:TotalLength == 10 .AND. _
                   pattern:PlaceholderDisplay == "__/__/____"
        END METHOD
        
        // =====================================================================
        // TEST: Placeholder Display Generation
        // =====================================================================
        PUBLIC METHOD TestPlaceholderDisplay() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            RETURN pattern:PlaceholderDisplay == "(___) ___-____"
        END METHOD
        
        // =====================================================================
        // TEST: Get Character Type - Digit
        // =====================================================================
        PUBLIC METHOD TestGetCharacterTypeDigit() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            RETURN pattern:GetCharacterType(0) == "digit" .AND. _
                   pattern:GetCharacterType(1) == "digit" .AND. _
                   pattern:GetCharacterType(2) == "digit"
        END METHOD
        
        // =====================================================================
        // TEST: Get Character Type - Letter
        // =====================================================================
        PUBLIC METHOD TestGetCharacterTypeLetter() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("AAA")
            
            RETURN pattern:GetCharacterType(0) == "letter" .AND. _
                   pattern:GetCharacterType(1) == "letter" .AND. _
                   pattern:GetCharacterType(2) == "letter"
        END METHOD
        
        // =====================================================================
        // TEST: Get Character Type - Literal
        // =====================================================================
        PUBLIC METHOD TestGetCharacterTypeLiteral() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999)")
            
            RETURN pattern:GetCharacterType(0) == "literal" .AND. _
                   pattern:GetCharacterType(1) == "digit" .AND. _
                   pattern:GetCharacterType(4) == "literal"
        END METHOD
        
        // =====================================================================
        // TEST: Is Literal Position
        // =====================================================================
        PUBLIC METHOD TestIsLiteralPosition() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999)-9999")
            
            RETURN pattern:IsLiteralPosition(0) .AND. _      // (
                   !pattern:IsLiteralPosition(1) .AND. _     // 9
                   pattern:IsLiteralPosition(4) .AND. _      // )
                   pattern:IsLiteralPosition(5)               // -
        END METHOD
        
        // =====================================================================
        // TEST: Get Next Data Position
        // =====================================================================
        PUBLIC METHOD TestGetNextDataPosition() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR nextAfter0 := pattern:GetNextDataPosition(0)
            VAR nextAfter1 := pattern:GetNextDataPosition(1)
            VAR nextAfter4 := pattern:GetNextDataPosition(4)
            
            RETURN nextAfter0 == 1 .AND. _  // Next after ( is first digit
                   nextAfter1 == 2 .AND. _  // Next after first digit
                   nextAfter4 == 6           // Next after ) is digit
        END METHOD
        
        // =====================================================================
        // TEST: Get Previous Data Position
        // =====================================================================
        PUBLIC METHOD TestGetPreviousDataPosition() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR prevBefore5 := pattern:GetPreviousDataPosition(5)
            VAR prevBefore10 := pattern:GetPreviousDataPosition(10)
            
            RETURN prevBefore5 == 3 .AND. _  // Before space, find previous digit
                   prevBefore10 >= 0
        END METHOD
        
        // =====================================================================
        // TEST: Get First Data Position
        // =====================================================================
        PUBLIC METHOD TestGetFirstDataPosition() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            RETURN pattern:GetFirstDataPosition() == 1  // First digit is after (
        END METHOD
        
        // =====================================================================
        // TEST: Get Literal At Position
        // =====================================================================
        PUBLIC METHOD TestGetLiteralAt() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            RETURN pattern:GetLiteralAt(0) == "(" .AND. _
                   pattern:GetLiteralAt(4) == ")" .AND. _
                   pattern:GetLiteralAt(5) == " " .AND. _
                   pattern:GetLiteralAt(9) == "-"
        END METHOD
        
        // =====================================================================
        // TEST: Is Valid Pattern
        // =====================================================================
        PUBLIC METHOD TestIsValidPattern() AS LOGIC
            VAR pattern1 := InputMaskPattern{}
            pattern1:Parse("(999) 999-9999")
            
            VAR pattern2 := InputMaskPattern{}
            // pattern2 empty - not parsed
            
            RETURN pattern1:IsValid() .AND. !pattern2:IsValid()
        END METHOD
        
        // =====================================================================
        // TEST: Pattern with Uppercase Modifier
        // =====================================================================
        PUBLIC METHOD TestPatternWithUppercaseModifier() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("!AAAA")
            
            // First position should be modifier
            RETURN pattern:Positions[0]:Type == "modifier" .AND. _
                   pattern:Positions[0]:Modifier == "!" .AND. _
                   pattern:Positions[1]:Type == "letter" .AND. _
                   pattern:DataLength == 4  // Only letters count as data
        END METHOD
        
        // =====================================================================
        // TEST: Pattern with Space-Fill Modifier
        // =====================================================================
        PUBLIC METHOD TestPatternWithSpaceFillModifier() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("&999")
            
            RETURN pattern:Positions[0]:Type == "modifier" .AND. _
                   pattern:Positions[0]:Modifier == "&"
        END METHOD
        
        // =====================================================================
        // TEST: Empty Pattern
        // =====================================================================
        PUBLIC METHOD TestEmptyPattern() AS LOGIC
            VAR pattern := InputMaskPattern{}
            // No parse call
            
            RETURN !pattern:IsValid()
        END METHOD
        
        // =====================================================================
        // TEST: Optional Digit Pattern
        // =====================================================================
        PUBLIC METHOD TestOptionalDigitPattern() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("###")
            
            RETURN pattern:Positions[0]:Type == "digit" .AND. _
                   !pattern:Positions[0]:Required .AND. _
                   pattern:DataLength == 3
        END METHOD
        
        // =====================================================================
        // TEST: Mixed Pattern Types
        // =====================================================================
        PUBLIC METHOD TestMixedPatternTypes() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("!AAA-999-XXXX")
            
            // Check first letter is uppercase forced
            VAR hasLetters := pattern:Positions:Any({p => p:Type == "letter"})
            VAR hasDigits := pattern:Positions:Any({p => p:Type == "digit"})
            VAR hasAlphanumeric := pattern:Positions:Any({p => p:Type == "alphanumeric"})
            VAR hasLiteral := pattern:Positions:Any({p => p:Type == "literal"})
            
            RETURN hasLetters .AND. hasDigits .AND. hasAlphanumeric .AND. hasLiteral
        END METHOD
        
        // =====================================================================
        // TEST: Alphanumeric Pattern
        // =====================================================================
        PUBLIC METHOD TestAlphanumericPattern() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("XXXX")
            
            RETURN pattern:Positions[0]:Type == "alphanumeric" .AND. _
                   pattern:DataLength == 4
        END METHOD
        
        // =====================================================================
        // TEST: Lowercase Letter Pattern
        // =====================================================================
        PUBLIC METHOD TestLowercaseLetterPattern() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("aaaa")
            
            RETURN pattern:Positions[0]:Type == "letter" .AND. _
                   !pattern:Positions[0]:Required  // 'a' is optional
        END METHOD
        
        // =====================================================================
        // TEST: Get Next Data Position At End
        // =====================================================================
        PUBLIC METHOD TestGetNextDataPositionAtEnd() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            VAR nextAfterLast := pattern:GetNextDataPosition(2)
            
            RETURN nextAfterLast == -1  // No next position
        END METHOD
        
        // =====================================================================
        // TEST: Get Previous Data Position At Start
        // =====================================================================
        PUBLIC METHOD TestGetPreviousDataPositionAtStart() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999")
            
            VAR prevBeforeFirst := pattern:GetPreviousDataPosition(0)
            
            RETURN prevBeforeFirst == -1  // No previous position
        END METHOD
        
    END CLASS

END NAMESPACE
