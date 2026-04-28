// VfpStringFunctionsTests.prg
// Unit tests for XSharp.VFP string functions
// (src/Runtime/XSharp.VFP/StringFunctions.prg)
// Requires: xunit (NuGet), XSharp.Core and XSharp.VFP references

USING Xunit
USING System.IO

BEGIN NAMESPACE XSharp.VFP.Tests

    PUBLIC CLASS VfpStringFunctionsTests

        // ─────────────────────────────────────────────
        // AddBs
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD AddBs_AppendsPathSeparator() AS VOID
            LOCAL cSep AS STRING
            cSep := Path.DirectorySeparatorChar:ToString()
            LOCAL cResult AS STRING
            cResult := AddBs("C:\Folder")
            Assert.True(cResult:EndsWith(cSep))
        END METHOD

        [Fact];
        PUBLIC METHOD AddBs_DoesNotDoubleAppend() AS VOID
            LOCAL cSep AS STRING
            cSep := Path.DirectorySeparatorChar:ToString()
            LOCAL cResult AS STRING
            cResult := AddBs("C:\Folder" + cSep)
            Assert.False(cResult:EndsWith(cSep + cSep))
        END METHOD

        [Fact];
        PUBLIC METHOD AddBs_EmptyString_ReturnsEmpty() AS VOID
            Assert.Equal("", AddBs(""))
        END METHOD

        [Fact];
        PUBLIC METHOD AddBs_NullString_ReturnsEmpty() AS VOID
            Assert.Equal("", AddBs(NULL_STRING))
        END METHOD

        // ─────────────────────────────────────────────
        // JustExt
        // ─────────────────────────────────────────────

        [Theory];
        [InlineData("C:\dir\file.txt", "txt")];
        [InlineData("C:\dir\file",     "")];
        [InlineData("",                "")];
        PUBLIC METHOD JustExt_ReturnsExtensionWithoutDot(cInput AS STRING, cExpected AS STRING) AS VOID
            Assert.Equal(cExpected, JustExt(cInput))
        END METHOD

        [Fact];
        PUBLIC METHOD JustExt_WithLeadingDotOption_IncludesDot() AS VOID
            Assert.Equal(".txt", JustExt("C:\file.txt", TRUE))
        END METHOD

        [Fact];
        PUBLIC METHOD JustExt_NullInput_ReturnsEmpty() AS VOID
            Assert.Equal("", JustExt(NULL_STRING))
        END METHOD

        // ─────────────────────────────────────────────
        // JustFName
        // ─────────────────────────────────────────────

        [Theory];
        [InlineData("C:\dir\file.txt", "file.txt")];
        [InlineData("file.txt",         "file.txt")];
        [InlineData("",                 "")];
        PUBLIC METHOD JustFName_ReturnsFilenameWithExtension(cInput AS STRING, cExpected AS STRING) AS VOID
            Assert.Equal(cExpected, JustFName(cInput))
        END METHOD

        // ─────────────────────────────────────────────
        // JustPath
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD JustPath_ReturnsDirectory() AS VOID
            LOCAL cSep AS STRING
            cSep := Path.DirectorySeparatorChar:ToString()
            LOCAL cInput AS STRING
            cInput := "C:" + cSep + "dir" + cSep + "file.txt"
            LOCAL cResult AS STRING
            cResult := JustPath(cInput)
            Assert.Equal("C:" + cSep + "dir", cResult)
        END METHOD

        [Fact];
        PUBLIC METHOD JustPath_EmptyInput_ReturnsEmpty() AS VOID
            Assert.Equal("", JustPath(""))
        END METHOD

        // ─────────────────────────────────────────────
        // JustStem
        // ─────────────────────────────────────────────

        [Theory];
        [InlineData("C:\dir\file.txt", "file")];
        [InlineData("C:\dir\file",     "file")];
        [InlineData("",                "")];
        PUBLIC METHOD JustStem_ReturnsFilenameWithoutExtension(cInput AS STRING, cExpected AS STRING) AS VOID
            Assert.Equal(cExpected, JustStem(cInput))
        END METHOD

        // ─────────────────────────────────────────────
        // ForceExt
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD ForceExt_ChangesExtension() AS VOID
            Assert.Equal("report.pdf", ForceExt("report.txt", "pdf"))
        END METHOD

        [Fact];
        PUBLIC METHOD ForceExt_EmptyInput_ReturnsEmpty() AS VOID
            Assert.Equal("", ForceExt("", "pdf"))
        END METHOD

        [Fact];
        PUBLIC METHOD ForceExt_AddsExtensionWhenMissing() AS VOID
            Assert.Equal("report.pdf", ForceExt("report", "pdf"))
        END METHOD

        // ─────────────────────────────────────────────
        // ForcePath
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD ForcePath_ReplacesPath() AS VOID
            LOCAL cSep AS STRING
            cSep := Path.DirectorySeparatorChar:ToString()
            LOCAL cResult AS STRING
            cResult := ForcePath("C:" + cSep + "old" + cSep + "file.txt", "D:" + cSep + "new")
            Assert.Equal("D:" + cSep + "new" + cSep + "file.txt", cResult)
        END METHOD

        [Fact];
        PUBLIC METHOD ForcePath_EmptyFilename_ReturnsEmpty() AS VOID
            Assert.Equal("", ForcePath("", "D:\new"))
        END METHOD

        // ─────────────────────────────────────────────
        // At
        // ─────────────────────────────────────────────

        [Theory];
        [InlineData("lo",    "Hello World", 1u, 4u)];
        [InlineData("World", "Hello World", 1u, 7u)];
        [InlineData("xyz",   "Hello World", 1u, 0u)];
        [InlineData("l",     "Hello World", 2u, 4u)];
        [InlineData("l",     "Hello World", 3u, 10u)];
        PUBLIC METHOD At_FindsSubstringPosition(cSearch AS STRING, cSource AS STRING, nOccurrence AS DWORD, nExpected AS DWORD) AS VOID
            Assert.Equal(nExpected, At(cSearch, cSource, nOccurrence))
        END METHOD

        [Fact];
        PUBLIC METHOD At_EmptySearch_ReturnsZero() AS VOID
            Assert.Equal(0u, At("", "hello"))
        END METHOD

        [Fact];
        PUBLIC METHOD At_EmptySource_ReturnsZero() AS VOID
            Assert.Equal(0u, At("hello", ""))
        END METHOD

        [Fact];
        PUBLIC METHOD At_OccurrenceExceedsCount_ReturnsZero() AS VOID
            Assert.Equal(0u, At("l", "hello", 10u))
        END METHOD

        // ─────────────────────────────────────────────
        // AtC (case-insensitive)
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD AtC_IsCaseInsensitive() AS VOID
            Assert.Equal(1u, AtC("HELLO", "hello world"))
        END METHOD

        [Fact];
        PUBLIC METHOD AtC_FindsMixedCase() AS VOID
            Assert.Equal(7u, AtC("WORLD", "hello world"))
        END METHOD

        [Fact];
        PUBLIC METHOD AtC_NotFound_ReturnsZero() AS VOID
            Assert.Equal(0u, AtC("xyz", "hello world"))
        END METHOD

        // ─────────────────────────────────────────────
        // RAt
        // ─────────────────────────────────────────────

        [Theory];
        [InlineData("a", "abracadabra", 1u, 11u)];
        [InlineData("a", "abracadabra", 2u,  8u)];
        [InlineData("a", "abracadabra", 5u,  1u)];
        [InlineData("a", "abracadabra", 6u,  0u)];  // only 5 occurrences
        PUBLIC METHOD RAt_FindsNthOccurrenceFromRight(cSearch AS STRING, cSource AS STRING, nOcc AS DWORD, nExpected AS DWORD) AS VOID
            Assert.Equal(nExpected, RAt(cSearch, cSource, nOcc))
        END METHOD

        [Fact];
        PUBLIC METHOD RAt_EmptySearch_ReturnsZero() AS VOID
            Assert.Equal(0u, RAt("", "abracadabra"))
        END METHOD

        [Fact];
        PUBLIC METHOD RAt_SingleCharMultipleOccurrences() AS VOID
            // "aaa" — 'a' at positions 1,2,3; last (1st from right) is 3
            Assert.Equal(3u, RAt("a", "aaa", 1u))
        END METHOD

        // ─────────────────────────────────────────────
        // ChrTran
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD ChrTran_TranslatesChars() AS VOID
            // Replace 'a'->'@', 'e'->'3'
            Assert.Equal("h3llo @bc", ChrTran("hello abc", "ae", "@3"))
        END METHOD

        [Fact];
        PUBLIC METHOD ChrTran_DeletesWhenReplacementIsShorter() AS VOID
            // 'a' has no replacement char => deleted
            Assert.Equal("hello bc", ChrTran("hello abc", "a", ""))
        END METHOD

        [Fact];
        PUBLIC METHOD ChrTran_NullInput_ReturnsEmpty() AS VOID
            Assert.Equal("", ChrTran(NULL_STRING, "a", "b"))
        END METHOD

        [Fact];
        PUBLIC METHOD ChrTran_NoMatchingChars_ReturnsOriginal() AS VOID
            Assert.Equal("hello", ChrTran("hello", "xyz", "123"))
        END METHOD

        [Fact];
        PUBLIC METHOD ChrTran_ReplacesMultipleOccurrences() AS VOID
            // All 'l' replaced with 'r'
            Assert.Equal("herro", ChrTran("hello", "l", "r"))
        END METHOD

        // ─────────────────────────────────────────────
        // AllTrim / LTrim / RTrim (VFP 3-argument overloads)
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD AllTrim_CustomChars_RemovesBothSides() AS VOID
            Assert.Equal("hello", AllTrim("***hello***", 0, "***"))
        END METHOD

        [Fact];
        PUBLIC METHOD AllTrim_CaseInsensitiveFlag_RemovesBothSides() AS VOID
            // Flag=1 means case-insensitive
            Assert.Equal("hello", AllTrim("AAAhelloaaa", 1, "aaa"))
        END METHOD

        [Fact];
        PUBLIC METHOD LTrim_CustomChars_RemovesLeftOnly() AS VOID
            Assert.Equal("hello***", LTrim("***hello***", 0, "***"))
        END METHOD

        [Fact];
        PUBLIC METHOD RTrim_CustomChars_RemovesRightOnly() AS VOID
            Assert.Equal("***hello", RTrim("***hello***", 0, "***"))
        END METHOD

        [Fact];
        PUBLIC METHOD AllTrim_NullInput_ReturnsNull() AS VOID
            Assert.Null(AllTrim(NULL_STRING, 0, " "))
        END METHOD

        [Fact];
        PUBLIC METHOD AllTrim_MultipleCustomCharsSequentially() AS VOID
            // Trim both "--" and "==" from each side
            Assert.Equal("hello", AllTrim("--==hello==--", 0, "--", "=="))
        END METHOD

        // ─────────────────────────────────────────────
        // StrExtract
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD StrExtract_BasicExtraction() AS VOID
            Assert.Equal("world", StrExtract("hello [world] test", "[", "]"))
        END METHOD

        [Fact];
        PUBLIC METHOD StrExtract_MissingEndDelim_ReturnsEmpty() AS VOID
            Assert.Equal("", StrExtract("hello [world test", "[", "]"))
        END METHOD

        [Fact];
        PUBLIC METHOD StrExtract_FlagNoEndDelim_ReturnsRemainder() AS VOID
            // nFlag = 2 => end delimiter not required, returns rest of string
            Assert.Equal("world test", StrExtract("hello [world test", "[", "]", 1, 2))
        END METHOD

        [Fact];
        PUBLIC METHOD StrExtract_FlagIncludeDelims() AS VOID
            // nFlag = 4 => include delimiters in result
            Assert.Equal("[world]", StrExtract("hello [world] test", "[", "]", 1, 4))
        END METHOD

        [Fact];
        PUBLIC METHOD StrExtract_SecondOccurrence() AS VOID
            Assert.Equal("two", StrExtract("[one][two][three]", "[", "]", 2))
        END METHOD

        [Fact];
        PUBLIC METHOD StrExtract_CaseInsensitiveFlag() AS VOID
            // nFlag = 1 => case-insensitive search
            Assert.Equal("world", StrExtract("hello XworldY test", "x", "y", 1, 1))
        END METHOD

        [Fact];
        PUBLIC METHOD StrExtract_EmptySearch_ReturnsEmpty() AS VOID
            Assert.Equal("", StrExtract("", "[", "]"))
        END METHOD

        [Fact];
        PUBLIC METHOD StrExtract_EmptyBeginDelim_ReturnsEmpty() AS VOID
            Assert.Equal("", StrExtract("hello [world]", "", "]"))
        END METHOD

        // ─────────────────────────────────────────────
        // Difference (Soundex-based)
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD Difference_IdenticalWords_Returns4() AS VOID
            Assert.Equal(4, Difference("Smith", "Smith"))
        END METHOD

        [Fact];
        PUBLIC METHOD Difference_EmptyString_Returns0() AS VOID
            Assert.Equal(0, Difference("", "Smith"))
        END METHOD

        [Fact];
        PUBLIC METHOD Difference_SimilarSoundex_ReturnsHighScore() AS VOID
            // "Robert" and "Rupert" share soundex characters
            LOCAL nDiff AS INT
            nDiff := Difference("Robert", "Rupert")
            Assert.True(nDiff >= 2)
        END METHOD

        [Fact];
        PUBLIC METHOD Difference_CompletelyDifferent_ReturnsLowScore() AS VOID
            // Very different words should score 0 or 1
            LOCAL nDiff AS INT
            nDiff := Difference("Smith", "Xyz")
            Assert.True(nDiff <= 1)
        END METHOD

        // ─────────────────────────────────────────────
        // Normalize
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD Normalize_ArrowOperator_ReplacedWithDot() AS VOID
            LOCAL cResult AS STRING
            cResult := Normalize("obj->Field")
            Assert.Equal("OBJ.FIELD", cResult)
        END METHOD

        [Fact];
        PUBLIC METHOD Normalize_UpperCasesKeywords() AS VOID
            LOCAL cResult AS STRING
            cResult := Normalize("x AND y")
            Assert.Contains(".AND.", cResult)
        END METHOD

        [Fact];
        PUBLIC METHOD Normalize_StringLiterals_PreservedAsIs() AS VOID
            // Content inside quotes must NOT be uppercased
            LOCAL cResult AS STRING
            cResult := Normalize('"hello world"')
            Assert.Contains("hello world", cResult)
        END METHOD

        [Fact];
        PUBLIC METHOD Normalize_EmptyString_ReturnsEmpty() AS VOID
            Assert.Equal("", Normalize(""))
        END METHOD

        [Fact];
        PUBLIC METHOD Normalize_NullString_ReturnsEmpty() AS VOID
            Assert.Equal("", Normalize(NULL_STRING))
        END METHOD

    END CLASS

END NAMESPACE

