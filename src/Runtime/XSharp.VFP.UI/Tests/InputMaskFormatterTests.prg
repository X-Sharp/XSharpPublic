USING System
USING System.Text

BEGIN NAMESPACE XSharp.VFP.UI.Tests

    /// <summary>
    /// Unit tests for InputMaskFormatter
    /// Tests formatting, data extraction, and placeholder generation
    /// </summary>
    PUBLIC CLASS InputMaskFormatterTests
        
        PRIVATE _formatter AS InputMaskFormatter
        
        PUBLIC CONSTRUCTOR()
            SELF:_formatter := InputMaskFormatter{}
        END CONSTRUCTOR
        
        // =====================================================================
        // TEST: Format Phone Number - Complete Data
        // =====================================================================
        PUBLIC METHOD TestFormatPhoneNumberComplete() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR formatted := SELF:_formatter:FormatValue(pattern, "5551234567")
            
            RETURN formatted == "(555) 123-4567"
        END METHOD
        
        // =====================================================================
        // TEST: Format Phone Number - Partial Data
        // =====================================================================
        PUBLIC METHOD TestFormatPhoneNumberPartial() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR formatted := SELF:_formatter:FormatValue(pattern, "555")
            
            RETURN formatted == "(555) ___-____"
        END METHOD
        
        // =====================================================================
        // TEST: Format SSN - Complete Data
        // =====================================================================
        PUBLIC METHOD TestFormatSSNComplete() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999-99-9999")
            
            VAR formatted := SELF:_formatter:FormatValue(pattern, "123456789")
            
            RETURN formatted == "123-45-6789"
        END METHOD
        
        // =====================================================================
        // TEST: Format Date - Complete Data
        // =====================================================================
        PUBLIC METHOD TestFormatDateComplete() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("99/99/9999")
            
            VAR formatted := SELF:_formatter:FormatValue(pattern, "12252024")
            
            RETURN formatted == "12/25/2024"
        END METHOD
        
        // =====================================================================
        // TEST: Format Empty Input - Show Placeholder
        // =====================================================================
        PUBLIC METHOD TestFormatEmptyInputShowsPlaceholder() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR formatted := SELF:_formatter:FormatValue(pattern, "")
            
            RETURN formatted == "(___) ___-____"
        END METHOD
        
        // =====================================================================
        // TEST: Extract Data - Phone Number
        // =====================================================================
        PUBLIC METHOD TestExtractDataPhoneNumber() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR extracted := SELF:_formatter:ExtractDataValue(pattern, "(555) 123-4567")
            
            RETURN extracted == "5551234567"
        END METHOD
        
        // =====================================================================
        // TEST: Extract Data - SSN
        // =====================================================================
        PUBLIC METHOD TestExtractDataSSN() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999-99-9999")
            
            VAR extracted := SELF:_formatter:ExtractDataValue(pattern, "123-45-6789")
            
            RETURN extracted == "123456789"
        END METHOD
        
        // =====================================================================
        // TEST: Extract Data - Partial Entry
        // =====================================================================
        PUBLIC METHOD TestExtractDataPartialEntry() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR extracted := SELF:_formatter:ExtractDataValue(pattern, "(555) ___-____")
            
            RETURN extracted == "555"
        END METHOD
        
        // =====================================================================
        // TEST: Get Placeholder Template
        // =====================================================================
        PUBLIC METHOD TestGetPlaceholderTemplate() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR placeholder := SELF:_formatter:GetPlaceholder(pattern)
            
            RETURN placeholder == "(___) ___-____"
        END METHOD
        
        // =====================================================================
        // TEST: Apply Mask At Position
        // =====================================================================
        PUBLIC METHOD TestApplyMaskAtPosition() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR result := SELF:_formatter:ApplyMaskAtPosition(pattern, 1, "5")
            
            RETURN result == "5"
        END METHOD
        
        // =====================================================================
        // TEST: Apply Mask At Position - Invalid Character
        // =====================================================================
        PUBLIC METHOD TestApplyMaskAtPositionInvalidChar() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR result := SELF:_formatter:ApplyMaskAtPosition(pattern, 1, "A")
            
            RETURN result == ""
        END METHOD
        
        // =====================================================================
        // TEST: Rebuild Formatted Value
        // =====================================================================
        PUBLIC METHOD TestRebuildFormattedValue() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR rebuilt := SELF:_formatter:RebuildFormattedValue(pattern, "(555) 123-456_")
            
            RETURN rebuilt == "(555) 123-456_"
        END METHOD
        
        // =====================================================================
        // TEST: Fill All Data
        // =====================================================================
        PUBLIC METHOD TestFillAllData() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR filled := SELF:_formatter:FillAllData(pattern, "5551234567")
            
            RETURN filled == "(555) 123-4567"
        END METHOD
        
        // =====================================================================
        // TEST: Get Max Data Length
        // =====================================================================
        PUBLIC METHOD TestGetMaxDataLength() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR maxLength := SELF:_formatter:GetMaxDataLength(pattern)
            
            RETURN maxLength == 10
        END METHOD
        
        // =====================================================================
        // TEST: Get Formatted Length
        // =====================================================================
        PUBLIC METHOD TestGetFormattedLength() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR formattedLength := SELF:_formatter:GetFormattedLength(pattern)
            
            RETURN formattedLength == 14
        END METHOD
        
        // =====================================================================
        // TEST: Format With Placeholders (Same as FormatValue)
        // =====================================================================
        PUBLIC METHOD TestFormatWithPlaceholders() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("999-99-9999")
            
            VAR formatted := SELF:_formatter:FormatWithPlaceholders(pattern, "12345")
            
            RETURN formatted == "123-45-____"
        END METHOD
        
        // =====================================================================
        // TEST: Extract Data With Spaces - Skipped
        // =====================================================================
        PUBLIC METHOD TestExtractDataSkipsSpaces() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("(999) 999-9999")
            
            VAR extracted := SELF:_formatter:ExtractDataValue(pattern, "(555)  ___-____")
            
            RETURN extracted == "555"  // Spaces in data positions are skipped
        END METHOD
        
        // =====================================================================
        // TEST: Format Letters with Case Transformation
        // =====================================================================
        PUBLIC METHOD TestFormatLettersUppercase() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("!AAA-999")
            
            VAR formatted := SELF:_formatter:FormatValue(pattern, "abc5")
            
            RETURN formatted == "ABC-5__"
        END METHOD
        
        // =====================================================================
        // TEST: Format Lowercase Letters
        // =====================================================================
        PUBLIC METHOD TestFormatLettersLowercase() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("aaa-999")
            
            VAR formatted := SELF:_formatter:FormatValue(pattern, "ABC5")
            
            RETURN formatted == "abc-5__"
        END METHOD
        
        // =====================================================================
        // TEST: Format Alphanumeric
        // =====================================================================
        PUBLIC METHOD TestFormatAlphanumeric() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("XXXX-9999")
            
            VAR formatted := SELF:_formatter:FormatValue(pattern, "AB12CD")
            
            // AB12 formatted in XXXX, then - literal, then need more data
            RETURN formatted == "AB12-CD__"
        END METHOD
        
        // =====================================================================
        // TEST: Extract From Formatted with Alphanumeric
        // =====================================================================
        PUBLIC METHOD TestExtractAlphanumeric() AS LOGIC
            VAR pattern := InputMaskPattern{}
            pattern:Parse("XXXX-9999")
            
            VAR extracted := SELF:_formatter:ExtractDataValue(pattern, "AB12-CD34")
            
            RETURN extracted == "AB12CD34"
        END METHOD
        
    END CLASS

END NAMESPACE
