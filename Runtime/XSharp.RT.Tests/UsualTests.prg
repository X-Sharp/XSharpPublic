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

#pragma warnings(219, off)  // assigned but not used
#pragma warnings(165, off)  // unassigned variables

BEGIN NAMESPACE XSharp.RT.Tests

CLASS UsualTests

    [Fact, Trait("Category", "Usual")];
    METHOD UsualDateTimeTest() AS VOID
        LOCAL now AS DateTime
        LOCAL u   AS USUAL
        now := System.DateTime.Now
        u := now
        // cannot call u:ToString directly because of late binding in other tests
        VAR s := u:ToString()
        Assert.Equal(now:ToString(),u:ToString())
        LOCAL check AS DateTime
        check := u
        Assert.Equal(now, check)
        RETURN

    [Fact, Trait("Category", "Usual")];
    METHOD UsualDecimalTests() AS VOID
        LOCAL u AS USUAL
        LOCAL l AS Decimal
        l := 1
        u := l
        Assert.Equal(UsualType(u), (DWORD) 27)
        l := u
        Assert.Equal(l,1)
        Assert.Equal(l,  (Decimal) u)
        u += 1
        Assert.Equal(2,  (Decimal) u)
        u++
        Assert.Equal(3,  (Decimal) u)
        u += 1U
        Assert.Equal(4,  (Decimal) u)
        // Note: adding a float to a usual results in a usual of type FLOAT
        // Therefore we must add the m suffix here
        u += 1.0m
        Assert.Equal(5, (Decimal) u)

        LOCAL u1, u2 AS USUAL
        u1 := 1
        u2 := 0.5m
        Assert.Equal(2m, (System.Decimal)(u1 /u2 ))
        Assert.Equal(0m, (System.Decimal)(u1 %u2 ))
        u2 := 0.25m
        Assert.Equal(4m, (System.Decimal)(u1 /u2 ))
        Assert.Equal(0m, (System.Decimal)(u1 %u2 ))
        u2 := 0.33m
        Assert.Equal(3.0303030303030303030303030303m, (System.Decimal)(u1 /u2 ))
        Assert.Equal(0.01m, (System.Decimal)(u1 %u2 ))

        RETURN

    [Fact, Trait("Category", "Usual")];
    METHOD UsualBinaryTests() AS VOID
        LOCAL u AS USUAL
        LOCAL b AS Binary
        b := 0h1234
        u := b
        Assert.Equal(TRUE, u == b)
        Assert.Equal(TRUE, b == u)
        b := u
        Assert.Equal(TRUE, u == 0h1234)
        RETURN


    [Fact, Trait("Category", "Usual")];
    METHOD UsualCurrencyTests() AS VOID
        LOCAL u AS USUAL
        LOCAL l AS Currency
        l := $1
        u := l
        Assert.Equal(UsualType(u), (DWORD) 28)
        l := u
        Assert.Equal((CURRENCY)l,(CURRENCY) 1)
        Assert.Equal(l,  (CURRENCY) u)
        u += 1
        Assert.Equal( $2,  (CURRENCY) u)
        u++
        Assert.Equal($3,  (CURRENCY) u)
        u += 1U
        Assert.Equal($4,  (CURRENCY) u)
        // Note: adding a float to a usual results in a usual of type FLOAT
        // Therefore we must add the m suffix here
        u += 1.0m
        Assert.Equal($5, (CURRENCY) u)

        LOCAL u1, u2 AS USUAL
        u1 := 1
        u2 := $0.5
        Assert.Equal($2, (CURRENCY)(u1 /u2 ))
        Assert.Equal($0, (CURRENCY)(u1 %u2 ))
        u2 := $0.25
        Assert.Equal($4, (CURRENCY)(u1 /u2 ))
        Assert.Equal($0, (CURRENCY)(u1 %u2 ))
        u2 := $0.33
        Assert.Equal($3.0303, (CURRENCY)(u1 /u2 ))
        Assert.Equal($0.01, (CURRENCY)(u1 %u2 ))

        u2 := $.5
        Assert.Equal($0.5, (CURRENCY) u2)
        u2 += $.5
        Assert.Equal($1.0, (CURRENCY) u2)
        RETURN



    [Fact, Trait("Category", "Usual")];
    METHOD UsualInt64Tests() AS VOID
        LOCAL u AS USUAL
        LOCAL l AS INT64
        u :=  1
        Assert.Equal(UsualType(u), (DWORD) LONG)
        l := u
        Assert.Equal(l,1)
        Assert.Equal(l,  (INT64) u)
        u := UInt64.MaxValue
        l := u	// NO Overflow Error
        u := "a text"
        Assert.Throws(TYPEOF(Error), { => l := u})	// Conversion Error
        RETURN

    [Fact, Trait("Category", "Usual")];
    METHOD UsualLongTests() AS VOID
        LOCAL u AS USUAL
        LOCAL l AS LONG
        u := 1
        Assert.Equal(UsualType(u), (DWORD) LONG)
        l := u
        Assert.Equal(l,1)
        Assert.Equal(l,  (LONG) u)
        u := UInt32.MaxValue
        VAR previous := RuntimeState.CompilerOptionOVF
        RuntimeState.CompilerOptionOVF := TRUE
        Assert.Throws(TYPEOF(Error), { => l := (LONG) u})	// Overflow Error
        RuntimeState.CompilerOptionOVF := previous
        u := "a text"
        Assert.Throws(TYPEOF(Error), { => l := (LONG) u})	// Conversion Error

        RETURN

    [Fact, Trait("Category", "Usual")];
    METHOD UsualReal4Tests() AS VOID
        LOCAL u AS USUAL
        LOCAL r AS REAL4
        r := 1.0
        u:= r
        Assert.Equal(UsualType(u), (DWORD) FLOAT)
        r := u
        Assert.Equal(r,(REAL4) 1.0)
        Assert.Equal(r,  (REAL4) u)
        r := System.Single.MaxValue
        u:= r
        Assert.Equal(UsualType(u), (DWORD) FLOAT)
        r := u
        Assert.Equal(r,System.Single.MaxValue)
        Assert.Equal(r,  (REAL4) u)

        RETURN

    [Fact, Trait("Category", "Usual")];
    METHOD UsualReal8Tests() AS VOID
        LOCAL u AS USUAL
        LOCAL r AS REAL8
        r := 1.0
        u:= r
        Assert.Equal(UsualType(u), (DWORD) FLOAT)
        r := u
        Assert.Equal(r,(REAL8) 1.0)
        Assert.Equal(r,  (REAL8) u)
        r := System.Double.MaxValue
        u:= r
        Assert.Equal(UsualType(u), (DWORD) FLOAT)
        r := u
        Assert.Equal(r,System.Double.MaxValue)
        Assert.Equal(r,  (REAL8) u)

        RETURN


    [Fact, Trait("Category", "Usual")];
    METHOD UsualShortTests() AS VOID
        LOCAL u AS USUAL
        LOCAL l AS LONG
        u := (USUAL) (SHORT)1
        Assert.Equal(UsualType(u), (DWORD) LONG)
        l := u
        Assert.Equal(l,1)
        Assert.Equal(l,  (SHORT) u)
        u := UInt16.MaxValue
        //Assert.Throws(TYPEOF(Error), { => l := (SHORT) u})	// Overflow Error
        u := "a text"
        Assert.Throws(TYPEOF(Error), { => l := (SHORT) u})	// Conversion Error

        RETURN

    [Fact, Trait("Category", "Usual")];
    METHOD UsualSByteTests() AS VOID
        LOCAL u AS USUAL
        LOCAL l AS SByte
        u := (SByte)1
        Assert.Equal(UsualType(u), (DWORD) LONG)
        l := u
        Assert.Equal(l,1)
        Assert.Equal(l,  (SByte) u)
        u := Byte.MaxValue
        //Assert.Throws(TYPEOF(Error), { => l := (SByte) u})	// Overflow Error
        u := "a text"
        Assert.Throws(TYPEOF(Error), { => l := (SByte) u})	// Conversion Error
        RETURN

    [Fact, Trait("Category", "Usual")];
    METHOD UsualUInt64Tests() AS VOID
        LOCAL u AS USUAL
        LOCAL d AS UINT64
        u := (UINT64) 1
        Assert.Equal(UsualType(u), (DWORD) INT64)
        d := u
        Assert.Equal(d,1U)
        Assert.Equal(d,  (UINT64) u)
        u := -1
        d := u	// NO Overflow Error
        Assert.Equal(d,  UInt64.MaxValue)
        u := "a text"
        Assert.Throws(TYPEOF(Error), { => d :=  u})	// Conversion Error
        RETURN

    [Fact, Trait("Category", "Usual")];
    METHOD UsualDwordTests() AS VOID
        LOCAL u AS USUAL
        LOCAL d AS DWORD
        u := (DWORD) 1
        Assert.Equal(UsualType(u), (DWORD) LONG)
        d := u
        Assert.Equal(d,1U)
        Assert.Equal(d,  (DWORD) u)
        u := -1
        d := u	// USUAL -> DWORD gives NO overflow error
        Assert.Equal(d,  UInt32.MaxValue)
        u := "a text"
        Assert.Throws(TYPEOF(Error), { => d :=  u})	// Conversion Error
        RETURN

    [Fact, Trait("Category", "Usual")];
    METHOD UsualwordTests() AS VOID
        LOCAL u AS USUAL
        LOCAL w AS WORD
        u :=  (WORD) 1
        Assert.Equal(UsualType(u), (DWORD) LONG)
        w := u
        Assert.Equal(w,1)
        Assert.Equal(w,  (WORD) u)
        u := -1
        //Assert.Throws(TYPEOF(Error), { => w := u})	// Overflow Error
        u := "a text"
        Assert.Throws(TYPEOF(Error), { => w := u})	// Conversion Error
        RETURN

    [Fact, Trait("Category", "Usual")];
    METHOD UsualByteTests() AS VOID
        LOCAL u AS USUAL
        LOCAL b AS BYTE
        b := 1
        u :=  b
        Assert.Equal(UsualType(u), (DWORD) LONG)
        b := u
        Assert.Equal(b,1U)
        Assert.Equal(b,  (BYTE) u)
        u :=  -1
        //Assert.Throws(TYPEOF(Error), { => b :=  u})	// Overflow Error
        u := "a text"
        Assert.Throws(TYPEOF(Error), { => b :=  u})	// Conversion Error
        RETURN

    [Fact, Trait("Category", "Usual")];
    METHOD UsualAddOperatorTests() AS VOID
        LOCAL u AS USUAL
        u :=  1
        u += 1
        Assert.Equal((INT) u, 2)
        u += (INT64) 1
        Assert.Equal((INT) u, 3)
        u += 1.0
        Assert.Equal((INT) u, 4)
        u++
        Assert.Equal((INT) u, 5)
        Assert.Throws(TYPEOF(Error), { => u += "a"})

        u := (INT64) 1
        u += (INT) 1
        Assert.Equal((INT64) u, 2)
        u += (INT64) 1
        Assert.Equal((INT64) u, 3)
        u += 1.0
        Assert.Equal((INT64) u, 4)
        u++
        Assert.Equal((INT64) u, 5)
        Assert.Throws(TYPEOF(Error), { => u += "a"})

        u :=   1.0
        u += 1
        Assert.Equal((FLOAT) u, 2.0)
        u += (INT64) 1
        Assert.Equal((FLOAT) u, 3.0)
        u += 1.0
        Assert.Equal((FLOAT) u, 4.0)
        u ++
        Assert.Equal((FLOAT) u, 5.0)
        Assert.Throws(TYPEOF(Error), { => u += "a"})

        u := "abc"
        u += "def"
        Assert.Equal((STRING) u, "abcdef")
        Assert.Throws(TYPEOF(Error), { => u += 1})

        VAR d := (DATE) datetime.Now
        u := d
        u += 1
        d += 1
        Assert.Equal((DATE) u,  d)

        LOCAL u1,u2 AS USUAL
        LOCAL dwSep, dwDigit AS DWORD
        dwSep := SetDecimalSep(Asc(","))
        dwDigit := SetDigit(5)

        u1 := 2
        u2 := 1.234
        Assert.Equal("3,234", AsString(u1 + u2))
        Assert.Equal("-0,766", AsString(u2 - u1))
        Assert.Equal("0,766", AsString(u1 - u2))

        u1 := (INT64)2
        u2 := 1.234
        Assert.Equal("3,234", AsString(u1 + u2))
        Assert.Equal("-0,766", AsString(u2 - u1))
        Assert.Equal("0,766", AsString(u1 - u2))

        u1 := 2.0000
        u2 := 1.234
        Assert.Equal("3,2340", AsString(u1 + u2))
        Assert.Equal("-0,7660", AsString(u2 - u1))
        Assert.Equal("0,7660", AsString(u1 - u2))

        SetDecimalSep(dwSep)
        SetDigit(dwDigit)


    [Fact, Trait("Category", "Usual")];
    METHOD UsualSubOperatorTests() AS VOID
        LOCAL u AS USUAL
        u := 100
        u -= 1
        Assert.Equal((INT) u, 99)
        u -= (INT64) 1
        Assert.Equal((INT) u, 98)
        u -= 1.0
        Assert.Equal((INT) u, 97)
        u--
        Assert.Equal((INT) u, 96)
        Assert.Throws(TYPEOF(Error), { => u -= "a"})

        u := (INT64) 1000
        u -= (INT) 1
        Assert.Equal((INT64) u, 999)
        u -= (INT64) 1
        Assert.Equal((INT64) u, 998)
        u -= 1.0
        Assert.Equal((INT64) u, 997)
        u--
        Assert.Equal((INT64) u, 996)
        Assert.Throws(TYPEOF(Error), { => u -= "a"})

        u := 111.1
        u -= 1
        Assert.Equal((FLOAT) u, 110.1)
        u -= (INT64) 1
        Assert.Equal((FLOAT) u, 109.1)
        u -= 1.0
        Assert.Equal((FLOAT) u, 108.1)
        u--
        Assert.Equal((FLOAT) u, 107.1)
        Assert.Throws(TYPEOF(Error), { => u -= "a"})

        u := "abc"
        u -= "def"
        Assert.Equal((STRING) u, "abcdef")
        Assert.Throws(TYPEOF(Error), { => u -= 1})

    [Fact, Trait("Category", "Usual string comparisons")];
    METHOD UsualStringComparisons() AS VOID
        LOCAL lExact := SetExact() AS LOGIC
        LOCAL u AS USUAL

        FOR LOCAL n := 1 AS INT UPTO 2
            SetExact(n == 1)
            u := "ABC"
            // do not chnage to Assert.Equal(), we need to test the operators
            Assert.False(u = "123456")
            Assert.False(u == "123456")
            Assert.True(u != "123456")
            Assert.True(u <> "123456")

            u := "123456"
            Assert.False(u = "ABC")
            Assert.False(u == "ABC")
            Assert.True(u != "ABC")
            Assert.True(u <> "ABC")
        NEXT

        SetExact(lExact)

    [Fact, Trait("Category", "Usual conversion tests")];
    METHOD UsualConversionTests() AS VOID
        LOCAL u AS USUAL
        LOCAL d AS DWORD
        u := UInt32.MaxValue
        d := SELF:UsualConversionTests_helper(u)
        Assert.Equal(UInt32.MaxValue - 1 , d)

    METHOD UsualConversionTests_helper(d AS DWORD) AS DWORD
        d --
        RETURN d

    [Fact, Trait("Category", "Usual conversion tests")];
    METHOD UsualIsTests() AS VOID
        LOCAL u AS USUAL
        u := Today()
        Assert.Equal(TRUE, IsDate(u))
        Assert.Equal(FALSE, IsDateTime(u))
        u := DateTime.Now
        Assert.Equal(TRUE, IsDate(u))
        Assert.Equal(TRUE, IsDateTime(u))
        u := 1L
        Assert.Equal(TRUE, IsLong(u))
        Assert.Equal(FALSE, IsInt64(u))
        Assert.Equal(TRUE, IsInteger(u))
        Assert.Equal(TRUE, IsNumeric(u))
        Assert.Equal(FALSE, IsFloat(u))
        Assert.Equal(FALSE, IsFractional(u))
        Assert.Equal(FALSE, IsDecimal(u))
        u := Int64.MaxValue
        Assert.Equal(FALSE, IsLong(u))
        Assert.Equal(TRUE, IsInt64(u))
        Assert.Equal(TRUE, IsInteger(u))
        Assert.Equal(TRUE, IsNumeric(u))
        Assert.Equal(FALSE, IsFloat(u))
        Assert.Equal(FALSE, IsFractional(u))
        Assert.Equal(FALSE, IsDecimal(u))
        u := FLOAT{1,1,0}
        Assert.Equal(FALSE, IsLong(u))
        Assert.Equal(FALSE, IsInt64(u))
        Assert.Equal(FALSE, IsInteger(u))
        Assert.Equal(TRUE, IsNumeric(u))
        Assert.Equal(TRUE, IsFloat(u))
        Assert.Equal(TRUE, IsFractional(u))
        Assert.Equal(FALSE, IsDecimal(u))
        u := 1m
        Assert.Equal(FALSE, IsLong(u))
        Assert.Equal(FALSE, IsInt64(u))
        Assert.Equal(FALSE, IsInteger(u))
        Assert.Equal(TRUE, IsNumeric(u))
        Assert.Equal(FALSE, IsFloat(u))
        Assert.Equal(TRUE, IsFractional(u))
        Assert.Equal(TRUE, IsDecimal(u))
        u := "abc"
        Assert.Equal(FALSE, IsLong(u))
        Assert.Equal(TRUE, IsString(u))
        Assert.Equal(FALSE, IsSymbol(u))
        u := #abc
        Assert.Equal(FALSE, IsLong(u))
        Assert.Equal(FALSE, IsString(u))
        Assert.Equal(TRUE, IsSymbol(u))
        u := {}
        Assert.Equal(FALSE, IsLong(u))
        Assert.Equal(FALSE, IsString(u))
        Assert.Equal(TRUE, IsArray(u))
        u := NIL
        Assert.Equal(FALSE, IsLong(u))
        Assert.Equal(FALSE, IsString(u))
        Assert.Equal(TRUE, IsNil(u))
        u := {|e|e+1}
        Assert.Equal(FALSE, IsLong(u))
        Assert.Equal(FALSE, IsString(u))
        Assert.Equal(TRUE, IsCodeBlock(u))
        u := MemAlloc(1)
        Assert.Equal(FALSE, IsLong(u))
        Assert.Equal(FALSE, IsString(u))
        Assert.Equal(TRUE, IsPtr(u))
        MemFree(u)

        u := Exception{}
        Assert.Equal(FALSE, IsLong(u))
        Assert.Equal(FALSE, IsString(u))
        Assert.Equal(TRUE, IsObject(u))

        RETURN
    [Fact, Trait("Category", "Usual pointer conversion tests")];
    METHOD UsualPtrConversionTests() AS VOID
        LOCAL u1 AS USUAL
        LOCAL u2 AS USUAL
        u1 := -1
        u2 := (IntPtr) (-2)
        Assert.Equal(FALSE, u1 == u2)
        Assert.Equal(TRUE, u1 != u2)
        Assert.Equal(TRUE, u1 > u2)
        Assert.Equal(TRUE, u1 >= u2)
        Assert.Equal(FALSE, u1 < u2)
        Assert.Equal(FALSE, u1 <= u2)

        Assert.Equal(FALSE, u2 == u1)
        Assert.Equal(TRUE, u2 != u1)
        Assert.Equal(FALSE, u2 > u1)
        Assert.Equal(FALSE, u2 >= u1)
        Assert.Equal(TRUE, u2 < u1)
        Assert.Equal(TRUE, u2 <= u1)

        LOCAL  i64 := -1 AS INT64
        u1 := i64
        Assert.Equal(FALSE, u1 == u2)
        Assert.Equal(TRUE, u1 != u2)
        Assert.Equal(TRUE, u1 > u2)
        Assert.Equal(TRUE, u1 >= u2)
        Assert.Equal(FALSE, u1 < u2)
        Assert.Equal(FALSE, u1 <= u2)

        Assert.Equal(FALSE, u2 == u1)
        Assert.Equal(TRUE, u2 != u1)
        Assert.Equal(FALSE, u2 > u1)
        Assert.Equal(FALSE, u2 >= u1)
        Assert.Equal(TRUE, u2 < u1)
        Assert.Equal(TRUE, u2 <= u1)

        LOCAL p AS IntPtr
        // NIL, 0, < 0 should implicitly convert to IntPtr.Zero
        u1 := -1
        p := u1
        Assert.Equal(TRUE, p == F_ERROR)
        u1 := 0
        p := u1
        Assert.Equal(TRUE, p == IntPtr.Zero)
        u1 := NIL
        p := u1
        Assert.Equal(TRUE, p == IntPtr.Zero)

        u1 := 1
        p := u1
        Assert.Equal(TRUE, p == IntPtr{1})




    [Fact, Trait("Category", "usual to string tests")];
    METHOD UsualToStringTests() AS VOID
        LOCAL cVal AS STRING
        cVal := SELF:GetNullStringInUsual()
        Assert.True(Empty(cVal))
        Assert.True(cVal == "")

    PRIVATE METHOD GetNullStringInUsual() AS USUAL
        LOCAL cRet AS STRING
        RETURN cRet

    [Fact, Trait("Category", "usual to string tests")];
    METHOD UsualShortCircuit() AS VOID
        LOCAL u1, u2 AS USUAL
        u1 := TRUE
        u2 := 1
        Assert.Equal(TRUE, u1 .and. u2)
        u2 := 0
        Assert.Equal(FALSE, u1 .and. u2)

    [Fact, Trait("Category", "usual to string tests")];
    METHOD UsualForEachTests() AS VOID
        LOCAL a AS USUAL
        LOCAL c AS STRING
        a := {1,2,3}
        c := ""
        FOREACH u AS USUAL IN a
            c += AsString(u)
        NEXT
        Assert.Equal("123", c)

    [Fact, Trait("Category", "usual mixed numeric types")];
    METHOD UsualMixedLong() AS VOID
        LOCAL u1, u2 AS USUAL
        // LHS LONG
        u1 := 1
        u2 := 2
        Assert.True(IsLong(u1+u2))
        Assert.True(IsLong(u1-u2))
        Assert.True(IsLong(u1*u2))
        Assert.True(IsLong(u1%u2))
        Assert.True(IsFloat(u1/u2))
        u2 := (INT64) 2
        Assert.True(IsInt64(u1+u2))
        Assert.True(IsInt64(u1-u2))
        Assert.True(IsInt64(u1*u2))
        Assert.True(IsInt64(u1%u2))
        Assert.True(IsFloat(u1/u2))
        u2 := 2.0
        Assert.True(IsFloat(u1+u2))
        Assert.True(IsFloat(u1-u2))
        Assert.True(IsFloat(u1*u2))
        Assert.True(IsFloat(u1%u2))
        Assert.True(IsFloat(u1/u2))

        u2 := $2.0
        Assert.True(IsCurrency(u1+u2))
        Assert.True(IsCurrency(u1-u2))
        Assert.True(IsCurrency(u1*u2))
        Assert.True(IsCurrency(u1%u2))
        Assert.True(IsCurrency(u1/u2))

        u2 := 2.0m
        Assert.True(IsDecimal(u1+u2))
        Assert.True(IsDecimal(u1-u2))
        Assert.True(IsDecimal(u1*u2))
        Assert.True(IsDecimal(u1%u2))
        Assert.True(IsDecimal(u1/u2))

    [Fact, Trait("Category", "usual mixed numeric types")];
    METHOD UsualMixedInt64() AS VOID
        LOCAL u1, u2 AS USUAL
        // LHS INT64
        u1 := (INT64) 1
        u2 := 2
        Assert.True(IsInt64(u1+u2))
        Assert.True(IsInt64(u1-u2))
        Assert.True(IsInt64(u1*u2))
        Assert.True(IsInt64(u1%u2))
        Assert.True(IsFloat(u1/u2))
        u2 := (INT64) 2
        Assert.True(IsInt64(u1+u2))
        Assert.True(IsInt64(u1-u2))
        Assert.True(IsInt64(u1*u2))
        Assert.True(IsInt64(u1%u2))
        Assert.True(IsFloat(u1/u2))
        u2 := 2.0
        Assert.True(IsFloat(u1+u2))
        Assert.True(IsFloat(u1-u2))
        Assert.True(IsFloat(u1*u2))
        Assert.True(IsFloat(u1%u2))
        Assert.True(IsFloat(u1/u2))

        u2 := $2.0
        Assert.True(IsCurrency(u1+u2))
        Assert.True(IsCurrency(u1-u2))
        Assert.True(IsCurrency(u1*u2))
        Assert.True(IsCurrency(u1%u2))
        Assert.True(IsCurrency(u1/u2))

        u2 := 2.0m
        Assert.True(IsDecimal(u1+u2))
        Assert.True(IsDecimal(u1-u2))
        Assert.True(IsDecimal(u1*u2))
        Assert.True(IsDecimal(u1%u2))
        Assert.True(IsDecimal(u1/u2))


    [Fact, Trait("Category", "usual mixed numeric types")];
    METHOD UsualMixedFloat() AS VOID
        LOCAL u1, u2 AS USUAL
        // LHS FLOAT
        u1 := 1.0
        u2 := 2
        Assert.True(IsFloat(u1+u2))
        Assert.True(IsFloat(u1-u2))
        Assert.True(IsFloat(u1*u2))
        Assert.True(IsFloat(u1%u2))
        Assert.True(IsFloat(u1/u2))
        u2 := (INT64) 2
        Assert.True(IsFloat(u1+u2))
        Assert.True(IsFloat(u1-u2))
        Assert.True(IsFloat(u1*u2))
        Assert.True(IsFloat(u1%u2))
        Assert.True(IsFloat(u1/u2))
        u2 := 2.0
        Assert.True(IsFloat(u1+u2))
        Assert.True(IsFloat(u1-u2))
        Assert.True(IsFloat(u1*u2))
        Assert.True(IsFloat(u1%u2))
        Assert.True(IsFloat(u1/u2))

        u2 := $2.0
        Assert.True(IsFloat(u1+u2))
        Assert.True(IsFloat(u1-u2))
        Assert.True(IsFloat(u1*u2))
        Assert.True(IsFloat(u1%u2))
        Assert.True(IsFloat(u1/u2))

        u2 := 2.0m
        Assert.True(IsFloat(u1+u2))
        Assert.True(IsFloat(u1-u2))
        Assert.True(IsFloat(u1*u2))
        Assert.True(IsFloat(u1%u2))
        Assert.True(IsFloat(u1/u2))

    [Fact, Trait("Category", "usual mixed numeric types")];
    METHOD UsualMixedCurrency() AS VOID
        LOCAL u1, u2 AS USUAL

        // LHS Currency
        u1 := $1.0
        u2 := 2
        Assert.True(IsCurrency(u1+u2))
        Assert.True(IsCurrency(u1-u2))
        Assert.True(IsCurrency(u1*u2))
        Assert.True(IsCurrency(u1%u2))
        Assert.True(IsCurrency(u1/u2))
        u2 := (INT64) 2
        Assert.True(IsCurrency(u1+u2))
        Assert.True(IsCurrency(u1-u2))
        Assert.True(IsCurrency(u1*u2))
        Assert.True(IsCurrency(u1%u2))
        Assert.True(IsCurrency(u1/u2))
        u2 := 2.0
        Assert.True(IsCurrency(u1+u2))
        Assert.True(IsCurrency(u1-u2))
        Assert.True(IsCurrency(u1*u2))
        Assert.True(IsCurrency(u1%u2))
        Assert.True(IsCurrency(u1/u2))

        u2 := $2.0
        Assert.True(IsCurrency(u1+u2))
        Assert.True(IsCurrency(u1-u2))
        Assert.True(IsCurrency(u1*u2))
        Assert.True(IsCurrency(u1%u2))
        Assert.True(IsCurrency(u1/u2))

        u2 := 2.0m
        Assert.True(IsCurrency(u1+u2))
        Assert.True(IsCurrency(u1-u2))
        Assert.True(IsCurrency(u1*u2))
        Assert.True(IsCurrency(u1%u2))
        Assert.True(IsCurrency(u1/u2))
    [Fact, Trait("Category", "usual mixed numeric types")];
    METHOD UsualMixedDecimal() AS VOID
        LOCAL u1, u2 AS USUAL
        // LHS Decimal
        u1 := 1.0m
        u2 := 2
        Assert.True(IsDecimal(u1+u2))
        Assert.True(IsDecimal(u1-u2))
        Assert.True(IsDecimal(u1*u2))
        Assert.True(IsDecimal(u1%u2))
        Assert.True(IsDecimal(u1/u2))
        u2 := (INT64) 2
        Assert.True(IsDecimal(u1+u2))
        Assert.True(IsDecimal(u1-u2))
        Assert.True(IsDecimal(u1*u2))
        Assert.True(IsDecimal(u1%u2))
        Assert.True(IsDecimal(u1/u2))
        u2 := 2.0
        Assert.True(IsDecimal(u1+u2))
        Assert.True(IsDecimal(u1-u2))
        Assert.True(IsDecimal(u1*u2))
        Assert.True(IsDecimal(u1%u2))
        Assert.True(IsDecimal(u1/u2))

        u2 := $2.0
        Assert.True(IsDecimal(u1+u2))
        Assert.True(IsDecimal(u1-u2))
        Assert.True(IsDecimal(u1*u2))
        Assert.True(IsDecimal(u1%u2))
        Assert.True(IsDecimal(u1/u2))

        u2 := 2.0m
        Assert.True(IsDecimal(u1+u2))
        Assert.True(IsDecimal(u1-u2))
        Assert.True(IsDecimal(u1*u2))
        Assert.True(IsDecimal(u1%u2))
        Assert.True(IsDecimal(u1/u2))


    [Fact, Trait("Category", "usual mixed numeric types")];
    METHOD NetArrayUsualConversions() AS VOID
        // https://github.com/X-Sharp/XSharpPublic/issues/876
        LOCAL u AS USUAL
        LOCAL a1 AS INT[]
        LOCAL a2 AS STRING[]

        a1 := <INT>{ 1,2,3 }
        u := a1 // OK
        a1 := u
        Assert.Equal(3, a1:Length)
        Assert.Equal(2, a1[2])

        a2 := <STRING>{ "abc","def" }
        u := a2
        a2 := u
        Assert.Equal(2, a2:Length)
        Assert.Equal("abc", a2[1])

    [Fact, Trait("Category", "usual MinMax Tests")];
    METHOD UsualMinMaxTests() AS VOID
        LOCAL u1,u2 AS USUAL
        u1 := Today()
        u2 := Today() +1
        Assert.True(Min(u1,u2) == u1)
        Assert.True(Max(u1,u2) == u2)

        u1 := "abc"
        u2 := "def"
        Assert.True(Min(u1,u2) == u1)
        Assert.True(Max(u1,u2) == u2)

        u1 := #abc
        u2 := #def
        Assert.True(Min(u1,u2) == u1)
        Assert.True(Max(u1,u2) == u2)

        u1 := DateTime.Now
        u2 := DateTime.Now:AddDays(1)
        Assert.True(Min(u1,u2) == u1)
        Assert.True(Max(u1,u2) == u2)


        u1 := Today()
        u2 := DateTime.Now:AddDays(1)
        // Datetime comparison
        Assert.True(Min(u1,u2) == u1)
        Assert.True(Max(u1,u2) == u2)

        u1 := 42.0
        u2 := 44.0
        Assert.True(Min(u1,u2) == u1)
        Assert.True(Max(u1,u2) == u2)


        u1 := 42L
        u2 := 44L
        Assert.True(Min(u1,u2) == u1)
        Assert.True(Max(u1,u2) == u2)

        u1 := 42.0m
        u2 := 44.0m
        Assert.True(Min(u1,u2) == u1)
        Assert.True(Max(u1,u2) == u2)

        u1 := $42.0
        u2 := $44.0
        Assert.True(Min(u1,u2) == u1)
        Assert.True(Max(u1,u2) == u2)

        u1 := 0h123456
        u2 := 0h123457
        Assert.True(Min(u1,u2) == u1)
        Assert.True(Max(u1,u2) == u2)



    [Fact, Trait("Category", "Usual")];
    METHOD BetweenTests() AS VOID
        LOCAL d1, d2, d3 AS decimal
        d1 := 1m
        d2 := 2m
        d3 := 4m
        Assert.False(Between(d1,d2,d3))
        d1 := 3m
        Assert.True(Between(d1,d2,d3))
        LOCAL dw1, dw2, dw3 AS DWORD
        dw1 := 1u
        dw2 := 2u
        dw3 := 4u
        Assert.False(Between(dw1,dw2,dw3))
        dw1 := 3
        Assert.True(Between(dw1,dw2,dw3))
        LOCAL l1, l2, l3 AS LONG
        l1 := 1
        l2 := 2
        l3 := 4
        Assert.False(Between(l1,l2,l3))
        l1 := 3
        Assert.True(Between(l1,l2,l3))
        RETURN

	[Fact, Trait("Category", "Usual")];
    METHOD InListTests() AS VOID
    	LOCAL lExact := SetExact() AS LOGIC

		Assert.False(InList("AB", "ABC","AD"))
		Assert.True(InList("ABC", "ABC","AD"))
		Assert.True(InList("AD", "ABC","AD"))

		SetExact(TRUE)
		Assert.False( InList("VOL", "VOLL", "FIBU") )
		Assert.False( InList("VOLL123", "VOLL", "FIBU") )
		Assert.True ( InList("VOLL", "VOLL", "FIBU") )

		SetExact(FALSE)
		Assert.False( InList("VOL", "VOLL", "FIBU") )
		Assert.True ( InList("VOLL123", "VOLL", "FIBU") )
		Assert.True ( InList("VOLL", "VOLL", "FIBU") )
		local u as usual
      u := "VOL"
      Assert.False( InList(u, "VOLL", "FIBU") )
      Assert.False( InList(1,2,3,4) )
      Assert.True( InList(1,2,3,4,1) )
      Assert.False( InList(1.0,2.0,3.0,4) )
      var oldDelta := SetFloatDelta(0.0001)
      Assert.True( InList(1.0,2.0,3.0,4,1.0000001) )
      SetFloatDelta(oldDelta)
      SetExact(lExact)
      Assert.False( InList(ToDay(), Today()+1, Today()-1, 1000.01.01))
      Assert.True( InList(ToDay(), Today()+1, Today()-1, 1000.01.01, ToDay()))
      InList("abc",1,2,Today(), 1.0)
END CLASS
END NAMESPACE // XSharp.Runtime.Tests

