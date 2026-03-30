//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit

BEGIN NAMESPACE XSharp.XPP.Tests

    CLASS XppStringFunctionTests

        // ---- PosUpper tests ----

        [Fact, Trait("Category", "String")];
        METHOD PosUpperBasicTests() AS VOID
            // First uppercase letter is at position 1
            Assert.Equal(1, PosUpper("Hello"))
            // First uppercase after lowercase start
            Assert.Equal(4, PosUpper("abcDef"))
            // Uppercase at end
            Assert.Equal(5, PosUpper("abcdE"))
            // Single uppercase character
            Assert.Equal(1, PosUpper("A"))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosUpperIgnoreCharsTests() AS VOID
            // Skip first 2 characters (indices 0,1) before searching; 'C' at index 2 is first uppercase
            Assert.Equal(3, PosUpper("abCDe", FALSE, 2))
            // Skip past the only uppercase -> 0
            Assert.Equal(0, PosUpper("Abcde", FALSE, 1))
            // No ignore chars (default 0)
            Assert.Equal(3, PosUpper("abCde"))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosUpperNoLetterTests() AS VOID
            // lNoLetter = TRUE: digits/punctuation also count as "match"
            Assert.Equal(1, PosUpper("1abc", TRUE))
            Assert.Equal(1, PosUpper("!Hello", TRUE))
            // lNoLetter = FALSE: non-letters are ignored; 'H' at index 1 is the first uppercase
            Assert.Equal(2, PosUpper("1Hello", FALSE))
        RETURN

        // ---- PosLower tests ----

        [Fact, Trait("Category", "String")];
        METHOD PosLowerBasicTests() AS VOID
            // First lowercase at position 1
            Assert.Equal(1, PosLower("hello"))
            // Lowercase after uppercase
            Assert.Equal(2, PosLower("Abc"))
            // Lowercase at end
            Assert.Equal(5, PosLower("ABCDe"))
            // Single lowercase character
            Assert.Equal(1, PosLower("a"))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosLowerIgnoreCharsTests() AS VOID
            // "aBcd": a=idx0, B=idx1, c=idx2, d=idx3; skip idx 0: 'B' not lower, 'c' lower -> pos 3
            Assert.Equal(3, PosLower("aBcd", FALSE, 1))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosLowerNoLetterTests() AS VOID
            // lNoLetter = TRUE: non-letters match too
            Assert.Equal(1, PosLower("1ABC", TRUE))
            Assert.Equal(1, PosLower(" ABC", TRUE))
            // lNoLetter = FALSE: 'e' at index 4 is first lowercase
            Assert.Equal(5, PosLower("ABCDe", FALSE))
        RETURN

        // ---- PosAlpha tests ----

        [Fact, Trait("Category", "String")];
        METHOD PosAlphaBasicTests() AS VOID
            // First alphabetic character at position 1
            Assert.Equal(1, PosAlpha("Hello"))
            Assert.Equal(1, PosAlpha("abc"))
            // Digits before letters
            Assert.Equal(4, PosAlpha("123a"))
            // Single letter
            Assert.Equal(1, PosAlpha("z"))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosAlphaIgnoreCharsTests() AS VOID
            // Skip first 1 character (index 0='a'); 'b' at index 1 is next alpha -> position 2
            Assert.Equal(2, PosAlpha("abc", FALSE, 1))
            // Skip two characters; 'c' at index 2 is next alpha -> position 3
            Assert.Equal(3, PosAlpha("abcd", FALSE, 2))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosAlphaNoLetterTests() AS VOID
            // lNoLetter = TRUE: non-letter characters also qualify
            Assert.Equal(1, PosAlpha("1abc", TRUE))
            Assert.Equal(1, PosAlpha("!abc", TRUE))
            // lNoLetter = FALSE: '1' is skipped; 'a' at index 1 is the first alpha -> position 2
            Assert.Equal(2, PosAlpha("1abc", FALSE))
        RETURN

        // ---- PosChar tests ----

        [Fact, Trait("Category", "String")];
        METHOD PosCharBasicTests() AS VOID
            // Replace character at position 3 with 'X'
            Assert.Equal("abXde", PosChar("abcde", "X", 3))
            // Replace first character
            Assert.Equal("Xbcde", PosChar("abcde", "X", 1))
            // Replace last character
            Assert.Equal("abcdX", PosChar("abcde", "X", 5))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosCharNumericTests() AS VOID
            // Use numeric char code instead of string: 65 = 'A'
            Assert.Equal("Abcde", PosChar("abcde", 65, 1))
            // 90 = 'Z', replace position 3
            Assert.Equal("abZde", PosChar("abcde", 90, 3))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosCharDefaultPositionTests() AS VOID
            // Default position is last character of string
            Assert.Equal("abcdX", PosChar("abcde", "X"))
        RETURN

        // ---- PosDel tests ----

        [Fact, Trait("Category", "String")];
        METHOD PosDelBasicTests() AS VOID
            // Delete 1 character at position 3
            Assert.Equal("abde", PosDel("abcde", 3, 1))
            // Delete first character
            Assert.Equal("bcde", PosDel("abcde", 1, 1))
            // Delete multiple characters
            Assert.Equal("ae", PosDel("abcde", 2, 3))
            // Delete last character
            Assert.Equal("abcd", PosDel("abcde", 5, 1))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosDelBoundaryTests() AS VOID
            // Delete more than available: removes to end of string
            Assert.Equal("ab", PosDel("abcde", 3, 10))
            // Delete zero characters: no change
            Assert.Equal("abcde", PosDel("abcde", 2, 0))
        RETURN

        // ---- PosIns tests ----

        [Fact, Trait("Category", "String")];
        METHOD PosInsBasicTests() AS VOID
            // nPosition is used directly as 0-indexed StringBuilder position, so
            // PosIns(str, ins, n) inserts ins before the (n+1)th character (1-indexed)
            // Insert at position 3: inserts before 0-indexed position 3 ('d')
            Assert.Equal("abcXXde", PosIns("abcde", "XX", 3))
            // Insert at position 1: inserts before 0-indexed position 1 ('b')
            Assert.Equal("aXXbcde", PosIns("abcde", "XX", 1))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosInsDefaultPositionTests() AS VOID
            // Default nPosition = SLen(string); inserts at that 0-indexed position = after last char
            Assert.Equal("abcdeXX", PosIns("abcde", "XX"))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosInsAppendTests() AS VOID
            // Insert after last character position
            LOCAL result AS STRING
            result := PosIns("abcde", "Z", 5)
            Assert.True(result:Contains("Z"))
        RETURN

        // ---- PosRepl tests (extended) ----

        [Fact, Trait("Category", "String")];
        METHOD PosReplBasicTests() AS VOID
            // Replace at position 2 - "xxx" replaces "bcd"
            Assert.Equal("axxxe", PosRepl("abcde", "xxx", 2))
            // Replace at position 1 (start)
            Assert.Equal("XXXde", PosRepl("abcde", "XXX", 1))
            // Single character replacement
            Assert.Equal("aXcde", PosRepl("abcde", "X", 2))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosReplDefaultPositionTests() AS VOID
            // Default position: start = length - replacement length + 1
            Assert.Equal("abcXX", PosRepl("abcde", "XX"))
        RETURN

        [Fact, Trait("Category", "String")];
        METHOD PosReplBoundaryTests() AS VOID
            // Replacement starts at position 1 even when nStartPos < 1
            LOCAL result AS STRING
            result := PosRepl("abcde", "X", 0)
            Assert.Equal("Xbcde", result)
            // Replacement that extends past end of string
            Assert.Equal("abcXX", PosRepl("abcde", "XX", 4))
        RETURN

    END CLASS

END NAMESPACE
