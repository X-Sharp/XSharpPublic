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
USING System.Globalization

DEFINE FTRUE  := TRUE
DEFINE FFALSE := FALSE
BEGIN NAMESPACE XSharp.RT.Tests

	CLASS VoConversionTests

		[Fact, Trait("Category", "Conversion")];
		METHOD AsStringTest() AS VOID
			LOCAL u AS USUAL
			u := "123"
			Assert.Equal("123", AsString(u))
			u := #A123
			Assert.Equal("A123", AsString(u))

			Assert.Equal("1", AsString(1))
			VAR c1 := GetRTFullPath()
			Assert.Equal(TRUE, c1:ToLower():IndexOf(".rt.dll") > 0)

			VAR n1 := GetThreadCount()
			Assert.Equal(TRUE, n1 > 1)

			LOCAL p AS PTR
			p := @p
            Assert.Equal(AsString(p), "0x" + AsHexString(p) )
            IF Intptr.Size == 4
                Assert.Equal(10, AsString(p):Length)
            ELSE
                Assert.Equal(18, AsString(p):Length)
            ENDIF
			p := NULL_PTR
			Assert.Equal("0x00000000", AsString(p))
			Assert.Equal("00000000", AsHexString(p))

		[Fact, Trait("Category", "Conversion")];
		METHOD StrTest() AS VOID
			LOCAL c AS STRING
            LOCAL f AS FLOAT
			SetDecimalSep(46)
            SetDecimal(2)
			c := Str3(12.3456,5,2)
			Assert.Equal("12.35", c)	// ROunded up
			c := Str3(12.3411,5,2)
			Assert.Equal("12.34", c)	// ROunded down
            f := FloatFormat(12.3456,10,4)
			c := Str3(f,10,5)
			Assert.Equal("  12.34560", c)	// ROunded down
			c := StrZero(12.3456,10,2)
			Assert.Equal("0000012.35", c)	// ROunded up
			c := Str3(2.49999,4,2)
			Assert.Equal("2.50", c )
			c := Str3(2.50012,4,2)
			Assert.Equal("2.50", c )

			c := Str3(70.00 - 65.01 - 4.99 , 16 , 2)
			Assert.Equal("            0.00", c )
			c := Str3(70.00 - 65.01 - 4.99 , 20 , 15)
			Assert.Equal("  -0.000000000000005", c )

			c := Str3(123456.7890/2.34, 25,15)
			Assert.Equal("    52759.311538461545000", c )
			c := Str(123456.7890/2.34, 25,15)
			Assert.Equal("    52759.311538461545000", c )

			SetDecimalSep(Asc(","))
			f := 1.2345999999
			Assert.Equal("1,2", Str(f, 30,1):Trim() )
			Assert.Equal("1,23", Str(f, 30,2):Trim() )
			Assert.Equal("1,235", Str(f, 30,3):Trim() )
			Assert.Equal("1,2346", Str(f, 30,4):Trim() )
			Assert.Equal("1,23460", Str(f, 30,5):Trim() )
			Assert.Equal("1,234600", Str(f, 30,6):Trim() )
			Assert.Equal("1,2346000", Str(f, 30,7):Trim() )
			Assert.Equal("1,23460000", Str(f, 30,8):Trim() )
			Assert.Equal("1,234600000", Str(f, 30,9):Trim() )
			Assert.Equal("1,2345999999", Str(f, 30,10):Trim() )
			Assert.Equal("1,23459999990", Str(f, 30,11):Trim() )
			Assert.Equal("1,234599999900", Str(f, 30,12):Trim() )

			Assert.Equal("00010", StrZero(10,5) )
			Assert.Equal("10", StrZero(10,2) )
			Assert.Equal("*", StrZero(10,1) )

			Assert.Equal("-0010", StrZero(-10,5) )
			Assert.Equal("-010", StrZero(-10,4) )
			Assert.Equal("-10", StrZero(-10,3) )
			Assert.Equal("**", StrZero(-10,2) )
			Assert.Equal("*", StrZero(-10,1) )

			Assert.Equal("-00123,123", StrZero(-123.123,10,3) )
			Assert.Equal("-123,123", StrZero(-123.123,8,3) )
			Assert.Equal("-00123,1", StrZero(-123.123,8,1) )
			Assert.Equal("-0123,1", StrZero(-123.123,7,1) )

			Assert.Equal("0000000001", StrZero(1.23 , 10) )
			Assert.Equal("0000000002", StrZero(1.99 , 10) )
			Assert.Equal("00013", StrZero(12.99 , 5) )
			Assert.Equal("13", StrZero(12.99 , 2) )
			Assert.Equal("*", StrZero(12.99 , 1) )
			Assert.Equal("01", StrZero(1.0, 2) )
			Assert.Equal("-0001", StrZero(-1.2345678 , 5) )
			Assert.Equal("-0002", StrZero(-1.99 , 5) )



		[Fact, Trait("Category", "Val")];
		METHOD ValTest() AS VOID
			LOCAL u AS USUAL
            SetDecimalSep('.')
            SetThousandSep(',')
            SetDecimal(2)
			u := Val("1.234")
			Assert.Equal(1.234, (FLOAT) u)
			SetDecimalSep(',')
            SetThousandSep('.')
			u := Val("1,234")
			Assert.Equal(1.234, (FLOAT) u)
            SetDecimalSep('.')
            SetThousandSep(',')
			u := Val("1.23E2")
			Assert.Equal(123.0, (FLOAT) u)
			u := Val("1.2345E2")
			Assert.Equal(123.45, (FLOAT) u)

			Assert.True(Val("0XEE") == 238)
			Assert.True(Val("0xEE") == 238)
			Assert.True(Val("0x100") == 256)
			Assert.True(Val("0x1AE") == 430)

			SetDecimalSep(',')
            SetThousandSep('.')

			u := Val("12,34")
			Assert.Equal(12.34, (FLOAT) u)
			u := Val("12.34")
			Assert.Equal(12.34, (FLOAT) u)

            SetDecimalSep('.')
            SetThousandSep(',')

			u := Val("12,34")
			Assert.Equal(12, (INT) u) // idiotic VO behavior
			u := Val("12.34")
			Assert.Equal(12.34, (FLOAT) u)


            SetDecimalSep('.')
            SetThousandSep(',')

            LOCAL cVal AS STRING
            LOCAL uRet AS USUAL

            cVal := "123456789"; uRet := Val(cVal)
			Assert.Equal(cVal, AsString(uRet))
			Assert.Equal(1, (INT) UsualType(uRet))

            cVal := "1234567890"; uRet := Val(cVal)
			Assert.Equal(cVal, AsString(uRet))
			Assert.Equal(3, (INT) UsualType(uRet))

            cVal := "- 123456789"; uRet := Val(cVal); cVal := "-123456789"
			Assert.Equal(cVal, AsString(uRet))
			Assert.Equal(3, (INT) UsualType(uRet))

            cVal := "   - 123"; uRet := Val(cVal); cVal := "-123"
			Assert.Equal(cVal, AsString(uRet))
			Assert.Equal(1, (INT) UsualType(uRet))


			Assert.Equal("123.456", AsString(Val("123.456")))
			Assert.Equal("123.456", AsString(Val("123.456")))
			Assert.Equal("123.456", AsString(Val("  123.456"  )))
			Assert.Equal("-123.4567", AsString(Val(" - 123.4567")))

			Assert.Equal( "  12345678901234567000", Str(Val("12345678901234567890"),22,-1) )
			Assert.Equal( " -12345678901234567000", Str(Val("-12345678901234567890"),22,-1) )

			Assert.Equal("255", AsString(Val("0xFF")))
			Assert.Equal("-255", AsString(Val("-0xFF")))
			Assert.Equal("-255", AsString(Val(" - 0xFF")))
			Assert.Equal("65534", AsString(Val("0xFFFE")))
			Assert.Equal("2147483648", AsString(Val(" 0X80000000")))

            SetDecimalSep(',')
            SetThousandSep('.')
			Assert.Equal("123,456", AsString(Val("  123.456"  )))
			Assert.Equal("123,456", AsString(Val("  123,456"  )))

			Assert.True(Val(NULL) == 0)

		[Fact, Trait("Category", "Val")];
		METHOD ValTests2() AS VOID
			SetDecimalSep(',')
			SetThousandSep('.')
			Assert.Equal(123, (INT) Val("123") )
			Assert.Equal(123.456, (FLOAT) Val("123,456") )

			Assert.Equal(0, (INT) Val("") )

			Assert.Equal(0, (INT) Val("abc") )
			Assert.Equal(123, (INT) Val("123abc") )
			Assert.Equal(123, (INT) Val("123abc456") )
			Assert.Equal(123, (INT) Val("123abc456,789") )
			Assert.Equal(0, (INT) Val("abc123456,789") )
			Assert.Equal(255, (INT) Val("0xFF") )
			Assert.Equal(0xFFFF, (INT) Val("0xFFFF") )
			Assert.Equal(4294967295, (INT64) Val("0xFFFFFFFF") )
			Assert.Equal(11, (INT) Val("11L11") )

			Assert.Equal(1.000, (FLOAT) Val("1,000.1") )
			Assert.Equal(1.001, (FLOAT) Val("1,001.1") )
			SetDecimalSep('.')
			SetThousandSep(',')
			Assert.Equal(1.0, (FLOAT) Val("1,000.1") )
			Assert.Equal(1.0, (FLOAT) Val("1,001.1") )
		RETURN

//		[Fact, Trait("Category", "Val")];
		#warning Need to add test back when "https://github.com/X-Sharp/XSharpPublic/issues/321" is fixed
		METHOD ValTests_fixed() AS VOID
			LOCAL deci,thou,digit,decimal,fixed_,digitfixed AS USUAL
			deci := SetDecimalSep(Asc("."))
			thou := SetThousandSep(Asc(","))
			digit := SetDigit(12)
			decimal := SetDecimal(3)
			fixed_ := SetFixed(TRUE)
			digitfixed := SetDigitFixed(TRUE)

			Assert.Equal( "*.**", Transform(12.34, "9.99") ) // "1..3", "*.**" in VO
			Assert.Equal( "1234567890", AsString(Val("1234567890")) ) // "1234567890.000", "1234567890" in VO
			Assert.Equal( "-123456789", AsString(Val("- 123456789")) ) // "-123456789.000", "-123456789" in VO
			Assert.Equal( "-123.4567" , AsString(Val(" - 123.4567")) ) // "-123.457", "-123.4567" in VO
			Assert.Equal( "  12345678901234567000", Str(Val("12345678901234567890"),22,-1) )  // "12345678901234567000.0", "  12345678901234567000" in VO
			Assert.Equal( " -12345678901234567000", Str(Val("-12345678901234567890"),22,-1) ) // "-12345678901234567000.", " -12345678901234567000" in VO
			Assert.Equal( "2147483648", AsString(Val(" 0X80000000")) ) // "2147483648.000", "2147483648" in VO

			SetDecimalSep(deci)
			SetThousandSep(thou)
			SetDigit(digit)
			SetDecimal(decimal)
			SetFixed(fixed_)
			SetDigitFixed(digitfixed)
		RETURN
		[Fact, Trait("Category", "DirtyCasts")];
        METHOD DirtyCasts() AS VOID
            Assert.Equal(1, BYTE(_CAST, FTRUE))
            Assert.Equal(0, BYTE(_CAST, FFALSE))
            Assert.Equal(255, _OR(BYTE(_CAST, FTRUE),0xFF))
            Assert.Equal(1, _AND(BYTE(_CAST, FTRUE),0xFF))
            Assert.Equal(1, BYTE(_CAST, TRUE))
            Assert.Equal(0, BYTE(_CAST, FALSE))

		[Fact, Trait("Category", "VariousConversions")];
		METHOD VariousConversions() AS VOID
			LOCAL deci,thou,digit,decimal,fixed_,digitfixed AS USUAL
			deci := SetDecimalSep(Asc(","))
			thou := SetThousandSep(Asc("."))
			digit := SetDigit()
			decimal := SetDecimal(5)
			fixed_ := SetFixed()
			digitfixed := SetDigitFixed()

			LOCAL r AS REAL8
			LOCAL u AS USUAL
			LOCAL f AS FLOAT
			r := 123.456
			u := r
			Assert.Equal( "123,45600", NTrim(u) )
			f := 12.3400000
			Assert.Equal( "12,3400000", NTrim(f) )
			Assert.Equal( "12,3400000", NTrim( AbsFloat(f)) )

			SetDecimalSep(deci)
			SetThousandSep(thou)
			SetDigit(digit)
			SetDecimal(decimal)
			SetFixed(fixed_)
			SetDigitFixed(digitfixed)
		RETURN

		[Fact, Trait("Category", "VariousConversions")];
		METHOD SystemDecimalConversions() AS VOID
			LOCAL deci,thou,decimal AS USUAL
			deci := SetDecimalSep(Asc(","))
			thou := SetThousandSep(Asc("."))
			decimal := SetDecimal(2)

			LOCAL d AS System.Decimal
			LOCAL u AS USUAL
			LOCAL f AS FLOAT

			f := 123.456
			Assert.Equal( 3, f:Decimals )
			Assert.Equal( "123,456", Str(f,-1) )

			u := 123.456
			f := u
			Assert.Equal( 3, f:Decimals )
			Assert.Equal( "123,456", Str(f,-1) )

			d := 123.456m
			f := d
			Assert.Equal( 2, f:Decimals )
			Assert.Equal( "123,46", Str(f,-1) )

			u := d
			f := u
			Assert.Equal( 2, f:Decimals )
			Assert.Equal( "123,46", Str(f,-1) )

			SetDecimal(4)
			f := d
			Assert.Equal( 4, f:Decimals )
			Assert.Equal( "123,4560", Str(f,-1) )

			u := d
			f := u
			Assert.Equal( 4, f:Decimals )
			Assert.Equal( "123,4560", Str(f,-1) )


			SetDecimal(1)
			LOCAL y AS Currency
			y := 123.456
			Assert.Equal( "123,4560", Str(y,-1) )
			f := y
			Assert.Equal( "123,4560", Str(f,-1) )
			u := y
			Assert.Equal( "123,4560", Str(u,-1) )
			u := y
			f := u
			Assert.Equal( "123,4560", Str(f,-1) )

			SetDecimalSep(deci)
			SetThousandSep(thou)
			SetDecimal(decimal)

			// picture is invalid so result random, but make sure no exception is thrown
			LOCAL dVal := 1 AS Decimal
			LOCAL yVal := 1 AS Currency
			LOCAL uVal AS USUAL
			? Transform(dVal, "@R{9999}")
			? Transform(yVal, "@R{9999}")
			uVal := dVal
			? Transform(uVal, "@R{9999}")
			uVal := yVal
			? Transform(uVal, "@R{9999}")
		RETURN
		[Fact, Trait("Category", "Psz")];
        METHOD PSZConversions() AS VOID
            LOCAL p AS PSZ
            LOCAL s AS STRING
            LOCAL u AS USUAL
            p := String2Psz("Test")
            s := p
            Assert.Equal(s, "Test")
            p := NULL_PSZ
            s := p
            Assert.Equal(s, "")

            s := NULL_PSZ
            Assert.Equal(s, "")

            u := NULL_PSZ
            s := u
            Assert.Equal(s, "")


	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
