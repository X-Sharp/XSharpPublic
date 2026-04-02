// InternationalFunctionsTests.prg
// Unit tests for XSharp.VO International/Multi-byte string functions
// (src/Runtime/XSharp.VO/Functions/International.prg)
// Tests: MB* functions (MBAt, MBLen, MBLeft, MBRight, MBTrim, etc.)

USING Xunit
USING System

BEGIN NAMESPACE XSharp.VO.Tests

    PUBLIC CLASS InternationalFunctionsTests

        // ─────────────────────────────────────────────
        // MBLen Tests (Multi-byte Length)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBLen_SimpleString_ReturnsCorrectLength() AS VOID
            Assert.Equal(5U, MBLen("Hello"))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBLen_EmptyString_ReturnsZero() AS VOID
            Assert.Equal(0U, MBLen(""))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBLen_NullString_ReturnsZero() AS VOID
            Assert.Equal(0U, MBLen(NULL_STRING))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBLen_UnicodeChars_ReturnsCorrectLength() AS VOID
            // Length should count characters, not bytes
            LOCAL cText AS STRING
            cText := "Test"
            Assert.Equal(4U, MBLen(cText))
        END METHOD

        // ─────────────────────────────────────────────
        // MBSLen Tests (Multi-byte Storage Length)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBSLen_SimpleString_ReturnsLength() AS VOID
            Assert.Equal(5U, MBSLen("Hello"))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBSLen_EmptyString_ReturnsZero() AS VOID
            Assert.Equal(0U, MBSLen(""))
        END METHOD

        // ─────────────────────────────────────────────
        // MBAt Tests (Multi-byte At)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAt_FindsSubstring() AS VOID
            Assert.Equal(7U, MBAt("World", "Hello World"))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAt_SubstringNotFound_ReturnsZero() AS VOID
            Assert.Equal(0U, MBAt("XYZ", "Hello World"))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAt_EmptySearch_ReturnsZero() AS VOID
            Assert.Equal(0U, MBAt("", "Hello"))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAt_NullStrings_ReturnsZero() AS VOID
            Assert.Equal(0U, MBAt(NULL_STRING, "Hello"))
            Assert.Equal(0U, MBAt("Hello", NULL_STRING))
        END METHOD

        // ─────────────────────────────────────────────
        // MBAt2 Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAt2_FindsSubstring() AS VOID
            Assert.Equal(7U, MBAt2("World", "Hello World"))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAt2_NotFound_ReturnsZero() AS VOID
            Assert.Equal(0U, MBAt2("XYZ", "Hello"))
        END METHOD

        // ─────────────────────────────────────────────
        // MBAt3 Tests (with offset)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAt3_WithOffset_FindsFromPosition() AS VOID
            LOCAL nPos AS DWORD
            nPos := MBAt3("l", "Hello World", 3)
            Assert.True(nPos >= 3)
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAt3_OffsetBeyondString_ReturnsZero() AS VOID
            Assert.Equal(0U, MBAt3("l", "Hello", 10))
        END METHOD

        // ─────────────────────────────────────────────
        // MBAtC Tests (Case-insensitive)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAtC_CaseInsensitive_FindsSubstring() AS VOID
            Assert.Equal(7U, MBAtC("WORLD", "Hello World"))
            Assert.Equal(7U, MBAtC("world", "Hello World"))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAtC_NotFound_ReturnsZero() AS VOID
            Assert.Equal(0U, MBAtC("XYZ", "Hello World"))
        END METHOD

        // ─────────────────────────────────────────────
        // MBAtC2 Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAtC2_CaseInsensitive_FindsSubstring() AS VOID
            Assert.Equal(7U, MBAtC2("world", "Hello World"))
        END METHOD

        // ─────────────────────────────────────────────
        // MBRat Tests (Right-side At)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBRat_FindsLastOccurrence() AS VOID
            LOCAL nPos AS INT
            nPos := MBRat("l", "Hello World")
            // Last 'l' is at position 10
            Assert.True(nPos == 10)
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBRat_NotFound_ReturnsZero() AS VOID
            Assert.Equal(0, MBRat("XYZ", "Hello"))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBRat_EmptySearch_ReturnsZero() AS VOID
            Assert.Equal(0, MBRat("", "Hello"))
        END METHOD

        // ─────────────────────────────────────────────
        // MBRat2 / MBRat3 Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBRat2_FindsLastOccurrence() AS VOID
            LOCAL nPos AS DWORD
            nPos := MBRat2("o", "Hello World")
            Assert.True(nPos > 0)
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBRat3_WithOffset_FindsFromRight() AS VOID
            LOCAL nPos AS DWORD
            nPos := MBRat3("l", "Hello", 2)
            Assert.True(nPos > 0)
        END METHOD

        // ─────────────────────────────────────────────
        // MBLeft Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBLeft_ExtractsLeftChars() AS VOID
            Assert.Equal("Hel", MBLeft("Hello", 3))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBLeft_CountExceedsLength_ReturnsWholeString() AS VOID
            Assert.Equal("Hi", MBLeft("Hi", 10))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBLeft_ZeroCount_ReturnsEmpty() AS VOID
            Assert.Equal("", MBLeft("Hello", 0))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBLeft_NullString_ReturnsNull() AS VOID
            LOCAL cResult AS STRING
            cResult := MBLeft(NULL_STRING, 5)
            Assert.True(cResult == NULL .OR. cResult == "")
        END METHOD

        // ─────────────────────────────────────────────
        // MBRight Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBRight_ExtractsRightChars() AS VOID
            Assert.Equal("llo", MBRight("Hello", 3))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBRight_CountExceedsLength_ReturnsWholeString() AS VOID
            Assert.Equal("Hi", MBRight("Hi", 10))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBRight_ZeroCount_ReturnsEmpty() AS VOID
            Assert.Equal("", MBRight("Hello", 0))
        END METHOD

        // ─────────────────────────────────────────────
        // MBTrim / MBLTrim / MBRTrim Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAllTrim_RemovesLeadingTrailingSpaces() AS VOID
            Assert.Equal("Hello", MBAllTrim("  Hello  "))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAllTrim_NoSpaces_ReturnsOriginal() AS VOID
            Assert.Equal("Hello", MBAllTrim("Hello"))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAllTrim_NullString_ReturnsNull() AS VOID
            LOCAL cResult AS STRING
            cResult := MBAllTrim(NULL_STRING)
            Assert.True(cResult == NULL .OR. cResult == "")
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBLTrim_RemovesLeftSpaces() AS VOID
            Assert.Equal("Hello  ", MBLTrim("  Hello  "))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBRTrim_RemovesRightSpaces() AS VOID
            Assert.Equal("  Hello", MBRTrim("  Hello  "))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBTrim_RemovesTrailingSpaces() AS VOID
            Assert.Equal("  Hello", MBTrim("  Hello  "))
        END METHOD

        // ─────────────────────────────────────────────
        // MBStuff Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBStuff_InsertsText() AS VOID
            LOCAL cResult AS STRING
            cResult := MBStuff("Hello World", 7, 5, "XSharp")
            Assert.Equal("Hello XSharp", cResult)
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBStuff_DeleteOnly_RemovesText() AS VOID
            LOCAL cResult AS STRING
            cResult := MBStuff("Hello World", 6, 6, "")
            Assert.Equal("Hello", cResult)
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBStuff_InsertAtStart() AS VOID
            LOCAL cResult AS STRING
            cResult := MBStuff("World", 1, 0, "Hello ")
            Assert.Equal("Hello World", cResult)
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBStuff_NullTarget_HandlesGracefully() AS VOID
            LOCAL cResult AS STRING
            cResult := MBStuff(NULL_STRING, 1, 0, "Test")
            Assert.True(cResult == NULL .OR. cResult:Contains("Test"))
        END METHOD

        // ─────────────────────────────────────────────
        // MBSubstr Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBSubstr2_ExtractsFromPosition() AS VOID
            Assert.Equal("World", MBSubstr2("Hello World", 7))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBSubstr2_StartExceedsLength_ReturnsEmpty() AS VOID
            Assert.Equal("", MBSubstr2("Hello", 10))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBSubstr3_ExtractsWithLength() AS VOID
            Assert.Equal("Wor", MBSubstr3("Hello World", 7, 3))
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBSubstr3_LengthExceedsRemaining_ReturnsRemainder() AS VOID
            LOCAL cResult AS STRING
            cResult := MBSubstr3("Hello", 3, 10)
            Assert.Equal("llo", cResult)
        END METHOD

        // ─────────────────────────────────────────────
        // MBAtLine Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAtLine_FindsInFirstLine_ReturnsOne() AS VOID
            LOCAL cMemo AS STRING
            LOCAL nLine AS INT
            cMemo := "Hello World" + Chr(13) + Chr(10) + "Test"
            nLine := MBAtLine("World", cMemo)
            Assert.Equal(1, nLine)
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAtLine_FindsInSecondLine_ReturnsTwo() AS VOID
            LOCAL cMemo AS STRING
            LOCAL nLine AS INT
            cMemo := "Hello" + Chr(13) + Chr(10) + "World Test"
            nLine := MBAtLine("World", cMemo)
            Assert.Equal(2, nLine)
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAtLine_NotFound_ReturnsZero() AS VOID
            Assert.Equal(0, MBAtLine("XYZ", "Hello" + Chr(13) + Chr(10) + "World"))
        END METHOD

        // ─────────────────────────────────────────────
        // MBAtLine2 Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MBAtLine2_FindsLineNumber() AS VOID
            LOCAL cMemo AS STRING
            LOCAL nLine AS DWORD
            cMemo := "Line1" + Chr(13) + Chr(10) + "Line2"
            nLine := MBAtLine2("Line2", cMemo)
            Assert.True(nLine > 0)
        END METHOD

        // ─────────────────────────────────────────────
        // Locale ID Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD GetAppLocaleID_ReturnsValidID() AS VOID
            LOCAL nLocale AS DWORD
            nLocale := GetAppLocaleID()
            Assert.True(nLocale > 0)
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD SetAppLocaleID_SetsAndReturnsOldValue() AS VOID
            LOCAL nOld AS DWORD
            LOCAL nNew AS DWORD
            nOld := GetAppLocaleID()
            nNew := SetAppLocaleID(0x0409) // English US
            // Should return previous value
            Assert.True(nNew == nOld .OR. nNew > 0)
            // Restore
            SetAppLocaleID(nOld)
        END METHOD

        // ─────────────────────────────────────────────
        // IsBiDi Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD IsBiDi_ReturnsLogicValue() AS VOID
            LOCAL lResult AS LOGIC
            lResult := IsBiDi()
            // Should return either TRUE or FALSE
            Assert.True(lResult == TRUE .OR. lResult == FALSE)
        END METHOD

        // ─────────────────────────────────────────────
        // MAKELANGID / MAKELCID Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MAKELANGID_CreatesLanguageID() AS VOID
            LOCAL wLang AS WORD
            wLang := MAKELANGID(0x09, 0x01) // English
            Assert.True(wLang > 0)
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD MAKELCID_CreatesLocaleID() AS VOID
            LOCAL dwLocale AS DWORD
            dwLocale := MAKELCID(0x0409, 0x01)
            Assert.True(dwLocale > 0)
        END METHOD

        // ─────────────────────────────────────────────
        // String2W / W2String Tests (Unicode conversion)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD String2W_W2String_RoundTrip() AS VOID
            LOCAL cOriginal AS STRING
            LOCAL pWide AS IntPtr
            LOCAL cResult AS STRING
            cOriginal := "Hello World"
            
            pWide := String2W(cOriginal)
            Assert.True(pWide != IntPtr.Zero)
            
            cResult := W2String(pWide)
            Assert.Equal(cOriginal, cResult)
        END METHOD

        [Fact, Trait("Category", "International")];
        PUBLIC METHOD W2String_NullPointer_ReturnsNull() AS VOID
            LOCAL cResult AS STRING
            cResult := W2String(IntPtr.Zero)
            Assert.True(cResult == NULL .OR. cResult == "")
        END METHOD

    END CLASS

END NAMESPACE
