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

#pragma warnings(165, off)  // unassigned variables
#pragma warnings(219, off)  // assigned but not used

BEGIN NAMESPACE XSharp.RT.Tests

    CLASS MiscTests

        [Fact, Trait("Category", "Misc")];
        METHOD InListStringTest() AS VOID
            LOCAL lExact AS LOGIC
            lExact := SetExact(FALSE)
            // Exact matches always found
            Assert.True(  InList("B", "A", "B", "C") )
            Assert.False( InList("D", "A", "B", "C") )
            Assert.True(  InList("A", "A") )
            Assert.False( InList("", "A", "B") )
            Assert.True(  InList("", "") )
            SetExact(lExact)
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD InListNumericTest() AS VOID
            Assert.True(  InList(2, 1, 2, 3) )
            Assert.False( InList(4, 1, 2, 3) )
            Assert.True(  InList(0, 0, 1, 2) )
            Assert.True(  InList(-1, -3, -2, -1) )
            Assert.False( InList(0, 1, 2, 3) )
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD InListExactStringTest() AS VOID
            // InListExact uses == (exact comparison)
            Assert.True(  InListExact("B", "A", "B", "C") )
            Assert.False( InListExact("D", "A", "B", "C") )
            Assert.True(  InListExact("A", "A") )
            Assert.False( InListExact("AB", "A") )  // "AB" != "A" (exact)
            Assert.False( InListExact("", "A", "B") )
            Assert.True(  InListExact("", "") )
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD InListExactNumericTest() AS VOID
            Assert.True(  InListExact(2, 1, 2, 3) )
            Assert.False( InListExact(4, 1, 2, 3) )
            Assert.True(  InListExact(0, 0, 1, 2) )
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD MaxIntTest() AS VOID
            Assert.Equal(5,  (INT) Max(3, 5))
            Assert.Equal(5,  (INT) Max(5, 3))
            Assert.Equal(5,  (INT) Max(5, 5))
            Assert.Equal(0,  (INT) Max(-5, 0))
            Assert.Equal(-3, (INT) Max(-5, -3))
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD MaxFloatTest() AS VOID
            LOCAL fDelta AS REAL8
            fDelta := SetFloatDelta()
            SetFloatDelta(0.0000001)
            Assert.True( Max(1.5, 2.5) == 2.5 )
            Assert.True( Max(2.5, 1.5) == 2.5 )
            Assert.True( Max(-1.5, -2.5) == -1.5 )
            Assert.True( Max(0.0, 0.0) == 0.0 )
            SetFloatDelta(fDelta)
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD MaxStringTest() AS VOID
            LOCAL lExact AS LOGIC
            lExact := SetExact(TRUE)
            Assert.Equal("Z", (STRING) Max("A", "Z"))
            Assert.Equal("Z", (STRING) Max("Z", "A"))
            Assert.Equal("B", (STRING) Max("A", "B"))
            Assert.Equal("A", (STRING) Max("A", "A"))
            SetExact(lExact)
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD MaxDateTest() AS VOID
            LOCAL d1 AS DATE
            LOCAL d2 AS DATE
            d1 := ConDate(2000, 1, 1)
            d2 := ConDate(2020, 6, 15)
            Assert.True( (DATE) Max(d1, d2) == d2 )
            Assert.True( (DATE) Max(d2, d1) == d2 )
            Assert.True( (DATE) Max(d1, d1) == d1 )
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD MinIntTest() AS VOID
            Assert.Equal(3,  (INT) Min(3, 5))
            Assert.Equal(3,  (INT) Min(5, 3))
            Assert.Equal(5,  (INT) Min(5, 5))
            Assert.Equal(-5, (INT) Min(-5, 0))
            Assert.Equal(-5, (INT) Min(-5, -3))
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD MinFloatTest() AS VOID
            LOCAL fDelta AS REAL8
            fDelta := SetFloatDelta()
            SetFloatDelta(0.0000001)
            Assert.True( Min(1.5, 2.5) == 1.5 )
            Assert.True( Min(2.5, 1.5) == 1.5 )
            Assert.True( Min(-1.5, -2.5) == -2.5 )
            Assert.True( Min(0.0, 0.0) == 0.0 )
            SetFloatDelta(fDelta)
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD MinStringTest() AS VOID
            LOCAL lExact AS LOGIC
            lExact := SetExact(TRUE)
            Assert.Equal("A", (STRING) Min("A", "Z"))
            Assert.Equal("A", (STRING) Min("Z", "A"))
            Assert.Equal("A", (STRING) Min("A", "B"))
            Assert.Equal("A", (STRING) Min("A", "A"))
            SetExact(lExact)
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD MinDateTest() AS VOID
            LOCAL d1 AS DATE
            LOCAL d2 AS DATE
            d1 := ConDate(2000, 1, 1)
            d2 := ConDate(2020, 6, 15)
            Assert.True( (DATE) Min(d1, d2) == d1 )
            Assert.True( (DATE) Min(d2, d1) == d1 )
            Assert.True( (DATE) Min(d1, d1) == d1 )
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD MaxMinInt64Test() AS VOID
            LOCAL n1 AS INT64
            LOCAL n2 AS INT64
            n1 := 1000000000000L
            n2 := 2000000000000L
            Assert.Equal(n2, (INT64) Max(n1, n2))
            Assert.Equal(n1, (INT64) Min(n1, n2))
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD GetRGBValuesTest() AS VOID
            // GetRValue, GetGValue, GetBValue extract color components from a DWORD
            // Color DWORD format: R in byte 0, G in byte 1, B in byte 2
            LOCAL nColor AS DWORD
            // Build a known color value: R=255, G=128, B=64
            nColor := (DWORD)255 | ((DWORD)128 << 8) | ((DWORD)64 << 16)
            Assert.Equal(255, (INT) GetRValue(nColor))
            Assert.Equal(128, (INT) GetGValue(nColor))
            Assert.Equal(64,  (INT) GetBValue(nColor))

            // Black: R=0, G=0, B=0
            nColor := 0U
            Assert.Equal(0, (INT) GetRValue(nColor))
            Assert.Equal(0, (INT) GetGValue(nColor))
            Assert.Equal(0, (INT) GetBValue(nColor))

            // White: R=255, G=255, B=255
            nColor := (DWORD)255 | ((DWORD)255 << 8) | ((DWORD)255 << 16)
            Assert.Equal(255, (INT) GetRValue(nColor))
            Assert.Equal(255, (INT) GetGValue(nColor))
            Assert.Equal(255, (INT) GetBValue(nColor))

            // Red only: R=255, G=0, B=0
            nColor := 255U
            Assert.Equal(255, (INT) GetRValue(nColor))
            Assert.Equal(0,   (INT) GetGValue(nColor))
            Assert.Equal(0,   (INT) GetBValue(nColor))

            // Green only: R=0, G=255, B=0
            nColor := (DWORD)255 << 8
            Assert.Equal(0,   (INT) GetRValue(nColor))
            Assert.Equal(255, (INT) GetGValue(nColor))
            Assert.Equal(0,   (INT) GetBValue(nColor))

            // Blue only: R=0, G=0, B=255
            nColor := (DWORD)255 << 16
            Assert.Equal(0,   (INT) GetRValue(nColor))
            Assert.Equal(0,   (INT) GetGValue(nColor))
            Assert.Equal(255, (INT) GetBValue(nColor))
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD CMYKRoundTripTest() AS VOID
            LOCAL nCMYK AS DWORD
            // Pack 4 distinct values and verify extraction
            nCMYK := CMYK(10, 20, 30, 40)
            Assert.Equal(10, (INT) GetCValue(nCMYK))
            Assert.Equal(20, (INT) GetMValue(nCMYK))
            Assert.Equal(30, (INT) GetYValue(nCMYK))
            Assert.Equal(40, (INT) GetKValue(nCMYK))

            // All zeros
            nCMYK := CMYK(0, 0, 0, 0)
            Assert.Equal(0, (INT) GetCValue(nCMYK))
            Assert.Equal(0, (INT) GetMValue(nCMYK))
            Assert.Equal(0, (INT) GetYValue(nCMYK))
            Assert.Equal(0, (INT) GetKValue(nCMYK))

            // All 255
            nCMYK := CMYK(255, 255, 255, 255)
            Assert.Equal(255, (INT) GetCValue(nCMYK))
            Assert.Equal(255, (INT) GetMValue(nCMYK))
            Assert.Equal(255, (INT) GetYValue(nCMYK))
            Assert.Equal(255, (INT) GetKValue(nCMYK))
        RETURN

        [Fact, Trait("Category", "Misc")];
        METHOD CMYKIndividualComponentsTest() AS VOID
            LOCAL nCMYK AS DWORD
            // C only
            nCMYK := CMYK(100, 0, 0, 0)
            Assert.Equal(100, (INT) GetCValue(nCMYK))
            Assert.Equal(0,   (INT) GetMValue(nCMYK))
            Assert.Equal(0,   (INT) GetYValue(nCMYK))
            Assert.Equal(0,   (INT) GetKValue(nCMYK))
            // M only
            nCMYK := CMYK(0, 100, 0, 0)
            Assert.Equal(0,   (INT) GetCValue(nCMYK))
            Assert.Equal(100, (INT) GetMValue(nCMYK))
            Assert.Equal(0,   (INT) GetYValue(nCMYK))
            Assert.Equal(0,   (INT) GetKValue(nCMYK))
            // Y only
            nCMYK := CMYK(0, 0, 100, 0)
            Assert.Equal(0,   (INT) GetCValue(nCMYK))
            Assert.Equal(0,   (INT) GetMValue(nCMYK))
            Assert.Equal(100, (INT) GetYValue(nCMYK))
            Assert.Equal(0,   (INT) GetKValue(nCMYK))
            // K only
            nCMYK := CMYK(0, 0, 0, 100)
            Assert.Equal(0,   (INT) GetCValue(nCMYK))
            Assert.Equal(0,   (INT) GetMValue(nCMYK))
            Assert.Equal(0,   (INT) GetYValue(nCMYK))
            Assert.Equal(100, (INT) GetKValue(nCMYK))
        RETURN

    END CLASS

END NAMESPACE
