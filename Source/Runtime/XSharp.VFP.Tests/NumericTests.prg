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


// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS NumericTests
		STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro

		[Fact, Trait("Category", "Numeric")];
		METHOD ConversionTests() AS VOID
            LOCAL c1 AS CURRENCY
            LOCAL c2 AS CURRENCY
            LOCAL f1 AS FLOAT
            LOCAL f2 AS FLOAT
            c1 := $1.2345
            c2 := NToM(1.2345444)
            Assert.True( c1  == c2 )
            f1 := 1.2345
            f2 := MToN($1.2345)
            Assert.True( f1  == f2 )
            Assert.Equal(1, Sign(1.2345))
            Assert.Equal(-1, Sign(-1.2345))
            Assert.Equal(0, Sign(0.0))

            c1 := $123.45
            c1 := - c1
            Assert.True(c1 == - $123.45)
            Assert.True(c1 == $0 - $123.45)
            Assert.True(c1 + $123.45 == $0.0)

            Assert.True(Integer(c1) == -123)
            Assert.True(Integer(-c1) == 123)
            Assert.True(Integer($10.25) == 10)

            LOCAL u := Val("$12345") AS USUAL
            Assert.Equal((DWORD) __UsualType.Currency, UsualType(u))


		[Fact, Trait("Category", "Numeric")];
		METHOD BitTests() AS VOID
            Assert.Equal(0h0000,      BitAnd(0h, 0hFFFF) )
            Assert.Equal(6,           (int) BitAnd(318, 7.5) )
            Assert.Equal(160,         BitAnd(-838, 0x06AC, 873445) )
            Assert.Equal(0h0000,      BitAnd(0h, 0hFFFF) )
            Assert.Equal(0h01234567,  BitAnd(0h01234567, 0h89abcdef) )
            Assert.Equal(319,         (int) BitOr(318, 7.5) )
            Assert.Equal(-1,          BitOr(-838, 0x06AC, 873445) )
            Assert.Equal(0hffff,      BitOr(0h, 0hFFFF) )
            Assert.Equal(0h89abcdef,  BitOr(0h01234567, 0h89abcdef) )
            Assert.Equal(313,         (int) BitXOr(318, 7.5) )
            Assert.Equal(-873997,     BitXOr(-838, 0x06AC, 873445) )
            Assert.Equal(0hffff,      BitXOr(0h, 0hFFFF) )
            Assert.Equal(0h88888888,  BitXOr(0h01234567, 0h89abcdef) )

            Assert.Equal(-2,          BitNot(0x01) )
            Assert.Equal(0h,          BitNot(0h) )
            Assert.Equal(0h7fff,      BitNot(0hffff, 7) )
            Assert.Equal(0h7ff8,      BitNot(0hffff, 7, 4) )

            Assert.Equal(11239297,    BitClear(0x00abff81, 15) )
            Assert.Equal(0h,          BitClear(0h) )
            Assert.Equal(0h0000,      BitClear(0hffff) )
            Assert.Equal(0hfeff,      BitClear(0hffff, 0) )
            Assert.Equal(0h00ff,      BitClear(0hffff, 0, 8) )

            Assert.Equal(-2136211583, BitSet(0x00abff81, 31) )
            Assert.Equal(0h,          BitSet(0h) )
            Assert.Equal(0hffff,      BitSet(0h0000) )
            Assert.Equal(0h0400,      BitSet(0h0000, 2) )
            Assert.Equal(0hfc03,      BitSet(0h0000, 2, 8) )

            Assert.Equal(536870911,   BitRShift(0xFFFFFFFFL, 3) )
            Assert.Equal(-8,          BitLShift(536870911, 3) )

            Assert.True(  BitTest(-1, 0) )
            Assert.False( BitTest(-2, 0) )
            Assert.False( BitTest(0hfffffffff7ffff, 35) )


	END CLASS

END NAMESPACE
