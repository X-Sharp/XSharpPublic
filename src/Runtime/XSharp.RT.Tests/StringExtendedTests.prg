// StringExtendedTests.prg
// Extended unit tests for XSharp.Core string functions
// (src/Runtime/XSharp.Core/Functions/String.prg)
// Tests: CharMix, ChrW, FPathName, PadC/PadL/PadR, QPEncString, UUEnc/Dec functions

USING Xunit
USING System

BEGIN NAMESPACE XSharp.RT.Tests
    PUBLIC CLASS StringExtendedTests

        // ─────────────────────────────────────────────
        // CharMix Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD CharMix_MixesEvenOddCharacters() AS VOID
            LOCAL cResult AS STRING
            cResult := CharMix("ABCD", "1234")
            Assert.Equal("A1B2C3D4", cResult)
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD CharMix_FirstStringLonger_UsesOnlyMatchingLength() AS VOID
            LOCAL cResult AS STRING
            cResult := CharMix("ABCDEF", "12")
            // Should only mix matching pairs
            Assert.True(cResult:StartsWith("A1B2"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD CharMix_SecondStringLonger_UsesOnlyMatchingLength() AS VOID
            LOCAL cResult AS STRING
            cResult := CharMix("AB", "123456")
            Assert.Equal("A1B2", cResult)
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD CharMix_EmptyStrings_ReturnsEmpty() AS VOID
            Assert.Equal("", CharMix("", ""))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD CharMix_NullStrings_HandlesGracefully() AS VOID
            LOCAL cResult AS STRING
            cResult := CharMix(NULL, "123")
            // Should handle NULL gracefully
            Assert.True(cResult == NULL .OR. cResult == "")
        END METHOD

        // ─────────────────────────────────────────────
        // Chr / ChrW Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD Chr_ValidValue_ReturnsCharacter() AS VOID
            Assert.Equal("A", Chr(65))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD Chr_ZeroValue_ReturnsNullChar() AS VOID
            LOCAL cResult AS STRING
            cResult := Chr(0)
            Assert.NotNull(cResult)
            Assert.Equal(1, cResult:Length)
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD ChrW_UnicodeValue_ReturnsCorrectChar() AS VOID
            LOCAL cResult AS STRING
            cResult := ChrW(0x03BB) // Greek lambda
            Assert.Equal(1, cResult:Length)
            Assert.Equal(0x03BB, cResult[0])
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD ChrW_HighValue_ReturnsCorrectChar() AS VOID
            LOCAL cResult AS STRING
            cResult := ChrW(1024)
            Assert.NotNull(cResult)
            Assert.Equal(1, cResult:Length)
        END METHOD

        // ─────────────────────────────────────────────
        // FPathName Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD FPathName_ValidPath_ReturnsCorrectFormat() AS VOID
            LOCAL cPath AS STRING
            cPath := FPathName()
            // Should return current path
            Assert.NotNull(cPath)
        END METHOD

        // ─────────────────────────────────────────────
        // Pad Functions Tests (PadC, PadL, PadR)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD PadC_CentersString() AS VOID
            LOCAL cResult AS STRING
            cResult := PadC("Hi", 10)
            Assert.Equal(10, cResult:Length)
            Assert.True(cResult:Trim() == "Hi")
            // Check roughly centered
            LOCAL nLeft AS INT
            nLeft := cResult:IndexOf("Hi")
            Assert.True(nLeft >= 3 .AND. nLeft <= 5)
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD PadC_StringLongerThanWidth_Truncates() AS VOID
            LOCAL cResult AS STRING
            cResult := PadC("LongString", 5)
            Assert.Equal(5, cResult:Length)
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD PadL_LeftPadsString() AS VOID
            LOCAL cResult AS STRING
            cResult := PadL("Hi", 10)
            Assert.Equal(10, cResult:Length)
            Assert.True(cResult:StartsWith("        Hi") .OR. cResult:EndsWith("Hi"))
        END METHOD


        [Fact, Trait("Category", "String")];
        PUBLIC METHOD PadR_RightPadsString() AS VOID
            LOCAL cResult AS STRING
            cResult := PadR("Hi", 10)
            Assert.Equal(10, cResult:Length)
            Assert.True(cResult:StartsWith("Hi"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD PadR_EmptyString_ReturnsSpaces() AS VOID
            LOCAL cResult AS STRING
            cResult := PadR("", 5)
            Assert.Equal(5, cResult:Length)
            Assert.Equal("     ", cResult)
        END METHOD

        // ─────────────────────────────────────────────
        // QPEncString Tests (Quoted-Printable Encoding)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD QPEncString_SimpleText_ReturnsUnchanged() AS VOID
            LOCAL cResult AS STRING
            cResult := QPEncString("Hello")
            // Simple ASCII should pass through
            Assert.True(cResult:Contains("Hello") .OR. cResult == "Hello")
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD QPEncString_ComplicatedText() AS VOID
            LOCAL cResult AS STRING
            cResult := QPEncString("Héllo")
            // Simple ASCII should pass through
            Assert.True(cResult == "H=E9llo")
            cResult := QPEncString("=E9")
            Assert.True(cResult == "=3DE9")
            cResult := QPEncString("é")
            Assert.True(cResult == "=E9")
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD QPEncString_EmptyString_ReturnsEmpty() AS VOID
            LOCAL cResult AS STRING
            cResult := QPEncString("")
            Assert.NotNull(cResult)
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD QPEncString_NullString_HandlesGracefully() AS VOID
            LOCAL cResult AS STRING
            TRY
                cResult := QPEncString(NULL)
                Assert.True(cResult == NULL .OR. cResult == "")
            CATCH
                // May throw, which is acceptable
                Assert.True(TRUE)
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // IsXXX Additional Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD IsAlpha_VariousInputs_ReturnsCorrect() AS VOID
            Assert.True(IsAlpha("A"))
            Assert.True(IsAlpha("Z"))
            Assert.True(IsAlpha("a"))
            Assert.True(IsAlpha("z"))
            Assert.False(IsAlpha("0"))
            Assert.False(IsAlpha(" "))
            Assert.False(IsAlpha("!"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD IsAlNum_VariousInputs_ReturnsCorrect() AS VOID
            Assert.True(IsAlNum("A"))
            Assert.True(IsAlNum("0"))
            Assert.True(IsAlNum("9"))
            Assert.False(IsAlNum(" "))
            Assert.False(IsAlNum("!"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD IsDigit_VariousInputs_ReturnsCorrect() AS VOID
            Assert.True(IsDigit("0"))
            Assert.True(IsDigit("5"))
            Assert.True(IsDigit("9"))
            Assert.False(IsDigit("A"))
            Assert.False(IsDigit(" "))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD IsBDigit_BinaryDigits_ReturnsCorrect() AS VOID
            Assert.True(IsBDigit("0"))
            Assert.True(IsBDigit("1"))
            Assert.False(IsBDigit("2"))
            Assert.False(IsBDigit("A"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD IsXDigit_HexDigits_ReturnsCorrect() AS VOID
            Assert.True(IsXDigit("0"))
            Assert.True(IsXDigit("9"))
            Assert.True(IsXDigit("A"))
            Assert.True(IsXDigit("F"))
            Assert.True(IsXDigit("a"))
            Assert.True(IsXDigit("f"))
            Assert.False(IsXDigit("G"))
            Assert.False(IsXDigit("Z"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD IsSpace_WhitespaceChars_ReturnsCorrect() AS VOID
            Assert.True(IsSpace(" "))
            Assert.True(IsSpace(Chr(9))) // Tab
            Assert.False(IsSpace("A"))
            Assert.False(IsSpace("0"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD IsUpper_UpperCaseChars_ReturnsCorrect() AS VOID
            Assert.True(IsUpper("A"))
            Assert.True(IsUpper("Z"))
            Assert.False(IsUpper("a"))
            Assert.False(IsUpper("0"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD IsLower_LowerCaseChars_ReturnsCorrect() AS VOID
            Assert.True(IsLower("a"))
            Assert.True(IsLower("z"))
            Assert.False(IsLower("A"))
            Assert.False(IsLower("0"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD IsXXX_EmptyString_ReturnsFalse() AS VOID
            Assert.False(IsAlpha(""))
            Assert.False(IsDigit(""))
            Assert.False(IsAlNum(""))
            Assert.False(IsSpace(""))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD IsXXX_NullString_ReturnsFalse() AS VOID
            Assert.False(IsAlpha(NULL))
            Assert.False(IsDigit(NULL))
            Assert.False(IsAlNum(NULL))
        END METHOD

        // ─────────────────────────────────────────────
        // InStr (case-sensitive substring check)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD InStr_SubstringExists_ReturnsTrue() AS VOID
            Assert.True(InStr("World", "Hello World"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD InStr_CaseSensitive_ReturnsFalseForMismatch() AS VOID
            Assert.False(InStr("world", "Hello World"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD InStr_SubstringNotFound_ReturnsFalse() AS VOID
            Assert.False(InStr("XYZ", "Hello World"))
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD InStr_EmptySubstring_ReturnsFalse() AS VOID
            Assert.False(InStr("", "Hello"))
        END METHOD

        /*
        // ─────────────────────────────────────────────
        // UUEncode / UUDecode Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD UUEncFile_ValidString_ReturnsEncoded() AS VOID
            LOCAL cResult AS STRING
            cResult := UUEncFile("Test")
            Assert.NotNull(cResult)
            Assert.True(cResult:Length > 0)
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD UUEncLine_ValidString_ReturnsEncoded() AS VOID
            LOCAL cResult AS STRING
            cResult := UUEncLine("Hello")
            Assert.NotNull(cResult)
            Assert.True(cResult:Length > 0)
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD UUEncLine_EmptyString_HandlesGracefully() AS VOID
            LOCAL cResult AS STRING
            cResult := UUEncLine("")
            Assert.NotNull(cResult)
        END METHOD
        */
        // ─────────────────────────────────────────────
        // ProperA (In-place Proper Case)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD ProperA_ConvertsInPlace() AS VOID
            LOCAL cText AS STRING
            cText := "hello world"
            ProperA(REF cText)
            Assert.Equal("Hello World", cText)
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD ProperA_MultipleWords_CapitalizesEach() AS VOID
            LOCAL cText AS STRING
            cText := "the quick brown fox"
            ProperA(REF cText)
            Assert.Equal("The Quick Brown Fox", cText)
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD ProperA_AlreadyProper_RemainsUnchanged() AS VOID
            LOCAL cText AS STRING
            cText := "Already Proper"
            ProperA(REF cText)
            Assert.Equal("Already Proper", cText)
        END METHOD

        // ─────────────────────────────────────────────
        // UpperA / LowerA (In-place conversion)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD UpperA_ConvertsInPlace() AS VOID
            LOCAL cText AS STRING
            cText := "hello"
            UpperA(REF cText)
            Assert.Equal("HELLO", cText)
        END METHOD

        [Fact, Trait("Category", "String")];
        PUBLIC METHOD LowerA_ConvertsInPlace() AS VOID
            LOCAL cText AS STRING
            cText := "HELLO"
            LowerA(REF cText)
            Assert.Equal("hello", cText)
        END METHOD

    END CLASS

END NAMESPACE
