//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING XUnit

BEGIN NAMESPACE XSharp.Core.Tests

    CLASS NumericExtTests

        // ---- AbsInt / AbsLong tests ----

        [Fact, Trait("Category", "Numeric")];
        METHOD AbsIntTests() AS VOID
            Assert.Equal(0,   AbsInt(0))
            Assert.Equal(5,   AbsInt(5))
            Assert.Equal(5,   AbsInt(-5))
            Assert.Equal(Int32.MaxValue, AbsInt(Int32.MaxValue))
        RETURN

        [Fact, Trait("Category", "Numeric")];
        METHOD AbsLongTests() AS VOID
            Assert.Equal(0,   AbsLong(0))
            Assert.Equal(42,  AbsLong(42))
            Assert.Equal(42,  AbsLong(-42))
            Assert.Equal(Int32.MaxValue, AbsLong(Int32.MaxValue))
        RETURN

        // ---- AbsReal4 / AbsReal8 / AbsShort tests ----

        [Fact, Trait("Category", "Numeric")];
        METHOD AbsReal4Tests() AS VOID
            LOCAL r4 AS REAL4
            r4 := 3.14
            Assert.Equal(r4, AbsReal4(r4))
            Assert.Equal(r4, AbsReal4(-r4))
            Assert.Equal(0.0, AbsReal4(0.0))
        RETURN

        [Fact, Trait("Category", "Numeric")];
        METHOD AbsReal8Tests() AS VOID
            LOCAL r8 AS REAL8
            r8 := 2.718281828
            Assert.Equal(r8, AbsReal8(r8))
            Assert.Equal(r8, AbsReal8(-r8))
            Assert.Equal(0.0, AbsReal8(0.0))
        RETURN

        [Fact, Trait("Category", "Numeric")];
        METHOD AbsShortTests() AS VOID
            Assert.Equal(0,   AbsShort(0))
            Assert.Equal(100, AbsShort(100))
            Assert.Equal(100, AbsShort(-100))
            Assert.Equal((LONG) Int16.MaxValue, AbsShort(Int16.MaxValue))
        RETURN

        // ---- SwapByte tests ----

        [Fact, Trait("Category", "Numeric")];
        METHOD SwapByteTests() AS VOID
            // SwapByte swaps the high and low nibbles of the byte
            Assert.Equal((BYTE) 0x21, SwapByte(0x12))
            Assert.Equal((BYTE) 0xAB, SwapByte(0xBA))
            Assert.Equal((BYTE) 0x00, SwapByte(0x00))
            Assert.Equal((BYTE) 0xFF, SwapByte(0xFF))
            // Swapping twice returns original value
            Assert.Equal((BYTE) 0x34, SwapByte(SwapByte(0x34)))
        RETURN

        // ---- SwapWord tests ----

        [Fact, Trait("Category", "Numeric")];
        METHOD SwapWordTests() AS VOID
            // SwapWord swaps the low and high bytes of a WORD
            Assert.Equal((WORD) 0x3412, SwapWord(0x1234))
            Assert.Equal((WORD) 0x0000, SwapWord(0x0000))
            Assert.Equal((WORD) 0xFFFF, SwapWord(0xFFFF))
            Assert.Equal((WORD) 0xFF00, SwapWord(0x00FF))
            // Swapping twice returns original value
            Assert.Equal((WORD) 0xABCD, SwapWord(SwapWord(0xABCD)))
        RETURN

        // ---- SwapShort tests ----

        [Fact, Trait("Category", "Numeric")];
        METHOD SwapShortTests() AS VOID
            // SwapShort swaps the low and high bytes of a SHORT
            Assert.Equal((SHORT) 0x3412, SwapShort(0x1234))
            Assert.Equal((SHORT) 0x0000, SwapShort(0x0000))
            // Swapping twice returns original
            LOCAL si AS SHORT
            si := 0x7F00
            Assert.Equal(si, SwapShort(SwapShort(si)))
        RETURN

        // ---- SwapDWord tests ----

        [Fact, Trait("Category", "Numeric")];
        METHOD SwapDWordTests() AS VOID
            // SwapDWord swaps the low and high WORDs of a DWORD
            Assert.Equal(0x56781234U, SwapDWord(0x12345678U))
            Assert.Equal(0x00000000U, SwapDWord(0x00000000U))
            Assert.Equal(0xFFFFFFFFU, SwapDWord(0xFFFFFFFFU))
            Assert.Equal(0xFFFF0000U, SwapDWord(0x0000FFFFU))
            // Swapping twice returns original
            Assert.Equal(0xDEADBEEFU, SwapDWord(SwapDWord(0xDEADBEEFU)))
        RETURN

        // ---- SwapLong tests ----

        [Fact, Trait("Category", "Numeric")];
        METHOD SwapLongTests() AS VOID
            // SwapLong swaps the low and high WORDs of a LONG
            Assert.Equal(0x56781234, SwapLong(0x12345678))
            Assert.Equal(0x00000000, SwapLong(0x00000000))
            // Swapping twice returns original
            LOCAL li AS LONG
            li := 0x12340000
            Assert.Equal(li, SwapLong(SwapLong(li)))
        RETURN

        // ---- SwapInt64 tests ----

        [Fact, Trait("Category", "Numeric")];
        METHOD SwapInt64Tests() AS VOID
            // SwapInt64 swaps the low and high DWORDs of an INT64
            LOCAL i64 AS INT64
            i64 := 0x0000000100000002
            LOCAL swapped AS INT64
            swapped := SwapInt64(i64)
            // After swap, original high DWORD becomes low and vice versa
            Assert.NotEqual(i64, swapped)
            // Swapping twice returns original
            Assert.Equal(i64, SwapInt64(SwapInt64(i64)))
            // Zero stays zero
            Assert.Equal(0L, SwapInt64(0L))
        RETURN

        // ---- Mod tests ----

        [Fact, Trait("Category", "Numeric")];
        METHOD ModReal8Tests() AS VOID
            Assert.Equal(0.0, Mod(10.0, 5.0))
            Assert.Equal(1.0, Mod(10.0, 3.0))
            Assert.Equal(2.5, Mod(7.5, 5.0))
            Assert.Equal(0.0, Mod(0.0, 7.0))
        RETURN

        [Fact, Trait("Category", "Numeric")];
        METHOD ModLongTests() AS VOID
            LOCAL a AS LONG
            LOCAL b AS LONG
            a := 10 ; b := 5
            Assert.Equal(0L, Mod(a, b))
            a := 10 ; b := 3
            Assert.Equal(1L, Mod(a, b))
            a := 0 ; b := 7
            Assert.Equal(0L, Mod(a, b))
        RETURN

        [Fact, Trait("Category", "Numeric")];
        METHOD ModInt64Tests() AS VOID
            LOCAL a AS INT64
            LOCAL b AS INT64
            a := 10 ; b := 5
            Assert.Equal(0L, Mod(a, b))
            a := 13 ; b := 5
            Assert.Equal(3L, Mod(a, b))
        RETURN

        // ---- MakeShort tests ----

        [Fact, Trait("Category", "Numeric")];
        METHOD MakeShortTests() AS VOID
            Assert.Equal(257,  (INT) MakeShort(1, 1))
            Assert.Equal(1,    (INT) MakeShort(1, 0))
            Assert.Equal(256,  (INT) MakeShort(0, 1))
            Assert.Equal(0,    (INT) MakeShort(0, 0))
            Assert.Equal(-1,   (INT) MakeShort(255, 255))
        RETURN

    END CLASS

END NAMESPACE
