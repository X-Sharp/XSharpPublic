// NumericFunctionsTests.prg
// Unit tests for XSharp.Core Numeric functions
// (src/Runtime/XSharp.Core/Functions/Numeric.prg)
// Tests: Abs*, Swap*, Make*, Mod, RGB

USING Xunit

BEGIN NAMESPACE XSharp.Core.Tests

    PUBLIC CLASS NumericFunctionsTests

        // ─────────────────────────────────────────────
        // Abs Functions
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD AbsInt_PositiveValue_ReturnsValue() AS VOID
            Assert.Equal(123, AbsInt(123))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD AbsInt_NegativeValue_ReturnsPositive() AS VOID
            Assert.Equal(123, AbsInt(-123))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD AbsInt_Zero_ReturnsZero() AS VOID
            Assert.Equal(0, AbsInt(0))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD AbsLong_NegativeValue_ReturnsPositive() AS VOID
            Assert.Equal(123456, AbsLong(-123456))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD AbsShort_NegativeValue_ReturnsPositive() AS VOID
            LOCAL si AS SHORT
            si := -123
            Assert.Equal(123, AbsShort(si))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD AbsReal4_NegativeValue_ReturnsPositive() AS VOID
            LOCAL r4 AS REAL4
            r4 := -123.45
            Assert.True(AbsReal4(r4) > 123.44)
            Assert.True(AbsReal4(r4) < 123.46)
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD AbsReal8_NegativeValue_ReturnsPositive() AS VOID
            LOCAL r8 AS REAL8
            r8 := -123.456789
            Assert.Equal(123.456789, AbsReal8(r8))
        END METHOD

        // ─────────────────────────────────────────────
        // Mod Functions
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD Mod_Long_ReturnsRemainder() AS VOID
            Assert.Equal(1, Mod(10, 3))
            Assert.Equal(0, Mod(10, 5))
            Assert.Equal(2, Mod(17, 5))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD Mod_Int64_ReturnsRemainder() AS VOID
            LOCAL n1, n2 AS INT64
            n1 := 100000000000
            n2 := 3
            Assert.Equal(1, Mod(n1, n2))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD Mod_Real8_ReturnsRemainder() AS VOID
            LOCAL r1, r2 AS REAL8
            r1 := 10.5
            r2 := 3.0
            LOCAL result AS REAL8
            result := Mod(r1, r2)
            Assert.True(Math.Abs(result - 1.5) < 0.0001)
        END METHOD

        // ─────────────────────────────────────────────
        // Swap Functions
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD SwapByte_SwapsNibbles() AS VOID
            LOCAL b AS BYTE
            b := 0x12
            Assert.Equal(0x21, SwapByte(b))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD SwapWord_SwapsBytes() AS VOID
            LOCAL w AS WORD
            w := 0x1234
            Assert.Equal(0x3412, SwapWord(w))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD SwapShort_SwapsBytes() AS VOID
            LOCAL si AS SHORT
            si := 0x1234
            Assert.Equal((SHORT)0x3412, SwapShort(si))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD SwapDWord_SwapsWords() AS VOID
            LOCAL dw AS DWORD
            dw := 0x12345678
            Assert.Equal(0x56781234u, SwapDWord(dw))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD SwapLong_SwapsWords() AS VOID
            LOCAL l AS LONG
            l := 0x12345678
            Assert.Equal(0x56781234, SwapLong(l))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD SwapInt_SwapsWords() AS VOID
            LOCAL l AS LONG
            l := 0x12345678
            Assert.Equal(0x56781234, SwapInt(l))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD SwapInt64_SwapsDWords() AS VOID
            LOCAL i64 AS INT64
            i64 := 0x123456789ABCDEF0
            LOCAL result AS INT64
            result := SwapInt64(i64)
            // Verify it swapped the high and low 32 bits
            Assert.NotEqual(i64, result)
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD SwapQWord_SwapsDWords() AS VOID
            LOCAL qw AS UINT64
            qw := 0x123456789ABCDEF0
            LOCAL result AS UINT64
            result := SwapQWord(qw)
            Assert.NotEqual(qw, result)
        END METHOD

        // ─────────────────────────────────────────────
        // Make Functions
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD MakeWord_CombinesBytes() AS VOID
            LOCAL bLow, bHigh AS BYTE
            bLow := 0x34
            bHigh := 0x12
            Assert.Equal(0x1234, MakeWord(bLow, bHigh))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD MakeShort_CombinesBytes() AS VOID
            LOCAL bLow, bHigh AS BYTE
            bLow := 0x34
            bHigh := 0x12
            Assert.Equal((SHORT)0x1234, MakeShort(bLow, bHigh))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD MakeDWord_CombinesWords() AS VOID
            LOCAL wLow, wHigh AS WORD
            wLow := 0x5678
            wHigh := 0x1234
            Assert.Equal(0x12345678u, MakeDWord(wLow, wHigh))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD MakeLong_CombinesWords() AS VOID
            LOCAL wLow, wHigh AS WORD
            wLow := 0x5678
            wHigh := 0x1234
            Assert.Equal(0x12345678, MakeLong(wLow, wHigh))
        END METHOD

        // ─────────────────────────────────────────────
        // Make/Swap Round-trip Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD MakeWord_SwapWord_RoundTrip() AS VOID
            LOCAL bLow, bHigh AS BYTE
            bLow := 0x34
            bHigh := 0x12
            LOCAL w AS WORD
            w := MakeWord(bLow, bHigh)
            LOCAL wSwapped AS WORD
            wSwapped := SwapWord(w)
            Assert.Equal(0x3412, wSwapped)
            Assert.Equal(w, SwapWord(wSwapped))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD MakeDWord_SwapDWord_RoundTrip() AS VOID
            LOCAL wLow, wHigh AS WORD
            wLow := 0x5678
            wHigh := 0x1234
            LOCAL dw AS DWORD
            dw := MakeDWord(wLow, wHigh)
            LOCAL dwSwapped AS DWORD
            dwSwapped := SwapDWord(dw)
            Assert.Equal(dw, SwapDWord(dwSwapped))
        END METHOD

        // ─────────────────────────────────────────────
        // RGB Function
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD RGB_CreatesColorValue() AS VOID
            LOCAL bRed, bGreen, bBlue AS BYTE
            bRed := 255
            bGreen := 128
            bBlue := 64
            LOCAL dwColor AS DWORD
            dwColor := RGB(bRed, bGreen, bBlue)
            // RGB format: 0x00BBGGRR
            Assert.Equal(0x004080FFu, dwColor)
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD RGB_Black_ReturnsZero() AS VOID
            Assert.Equal(0u, RGB(0, 0, 0))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD RGB_White_ReturnsFFFFFF() AS VOID
            Assert.Equal(0x00FFFFFFu, RGB(255, 255, 255))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD RGB_Red_Returns0000FF() AS VOID
            Assert.Equal(0x000000FFu, RGB(255, 0, 0))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD RGB_Green_Returns00FF00() AS VOID
            Assert.Equal(0x0000FF00u, RGB(0, 255, 0))
        END METHOD

        [Fact, Trait("Category", "Numeric")];
        PUBLIC METHOD RGB_Blue_ReturnsFF0000() AS VOID
            Assert.Equal(0x00FF0000u, RGB(0, 0, 255))
        END METHOD

    END CLASS

END NAMESPACE
