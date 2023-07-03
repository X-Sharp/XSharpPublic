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

// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.RT.Tests

	CLASS StringTests

		[Fact, Trait("Category", "String")];
		METHOD DescendTests() AS VOID
            LOCAL cString AS STRING
            LOCAL cDesc   AS STRING
            cString := "the quick brown fox jumps over the lazy dog 1234567890"
            cDesc   := Descend(cString)
            Assert.Equal(cString:Length, cDesc:Length)
            FOR VAR i := 1 TO SLen(cString)
                Assert.Equal(Asc(SubStr(cDesc,i,1)), 256 - Asc(SubStr(cString,i,1)))
            NEXT
            cString := "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG !@#$%^&*()"
            cDesc   := Descend(cString)
            Assert.Equal(cString:Length, cDesc:Length)
            FOR VAR i := 1 TO SLen(cString)
                Assert.Equal(Asc(SubStr(cDesc,i,1)), 256 - Asc(SubStr(cString,i,1)))
            NEXT
            cString := ""
            FOR VAR i := 128u TO 255u
                cString += Chr(i)
             NEXT
             cDesc   := Descend(cString)
             Assert.Equal(cString:Length, cDesc:Length)
             FOR VAR i := 1 TO SLen(cString)
                 Assert.Equal(Asc(SubStr(cDesc,i,1)), 256 - Asc(SubStr(cString,i,1)))
             NEXT
            RETURN

		[Fact, Trait("Category", "String")];
		METHOD MoreStringTests() AS VOID
			Assert.Equal("          ", Transform("", "@R XXXXXXXXXX")        )
			Assert.Equal("Italia    ", Transform("Italia", "@R XXXXXXXXXX")  )
			Assert.Equal("ITALIA    ", Transform("Italia", "@! XXXXXXXXXX")  )
			Assert.Equal("Italia    ", Transform("Italia", "XXXXXXXXXX")     )
			Assert.Equal("1234512345", Transform("1234512345", "XXXXXXXXXX") )
			Assert.Equal("1234"      , Transform("1234", "XX99") )
			Assert.Equal("ITALIA"    , Transform("Italia", "@!") )

			LOCAL c AS STRING
			c := "Not nil!"
			Assert.False(c == NIL)
			Assert.True(c != NIL)
			LOCAL u AS STRING
			u := "Not nil!"
			Assert.False(c == NIL)
			Assert.True(c != NIL)
		RETURN

		[Fact, Trait("Category", "StringEq")];
		METHOD StringEquality() AS VOID
			LOCAL c1,c2 AS STRING
			LOCAL u1,u2 AS USUAL
			LOCAL exact AS LOGIC
			u1 := c1 := "TESTLONG"
			u2 := c2 := "TEST"
			exact := SetExact( FALSE )

			Assert.Equal(TRUE, c1 = c2)
			Assert.Equal(TRUE, c1 != c2)

			Assert.Equal(TRUE, u1 = u2)
			Assert.Equal(FALSE, u1 != u2)

			SetExact( exact )
		RETURN

		[Fact, Trait("Category", "Str")];
		METHOD StrTests() AS VOID
			LOCAL deci,thou,digit,decimal,fixed_,digitfixed AS USUAL
			deci := SetDecimalSep(Asc("."))
			thou := SetThousandSep(Asc(","))
			digit := SetDigit(6)
			decimal := SetDecimal(3)
			fixed_ := SetFixed(FALSE)
			digitfixed := SetDigitFixed(FALSE)

			Assert.Equal( "     1.2"			, Str(1.2) )
			Assert.Equal( "    12.3"			, Str(12.3) )
			Assert.Equal( "    12.34"			, Str(12.34) )
			Assert.Equal( "   123.456"			, Str(123.456) )
			Assert.Equal( "  1234.567"			, Str(1234.567) )
			Assert.Equal( "  1234.5678"			, Str(1234.5678) )
			Assert.Equal( "  1234.5679012345"	, Str(1234.5679012345) )
			Assert.Equal( "123456.78901234567"	, Str(123456.78901234567) )
			Assert.Equal( "999999" , Str(999999) )
			Assert.Equal( "******" , Str(1000000) )


			Assert.Equal( "0.0" , Transform(0 , "9.9") )
			Assert.Equal( "1.0" , Transform(1 , "9.9") )
			Assert.Equal( "*.*" , Transform(-1 , "9.9") )
			SetDecimalSep(Asc(","))
			SetThousandSep(Asc("."))
			Assert.Equal( " 0,00" , Transform(0 , "99.99") )
			Assert.Equal( " 1,00" , Transform(1 , "99.99") )
			Assert.Equal( "-1,00" , Transform(-1 , "99.99") )

			Assert.Equal( "  0,00" , Transform(0 , "999.99") )
			Assert.Equal( "  0,0" , Transform(0 , "999.9") )
			Assert.Equal( " 0,0" , Transform(0 , "99.9") )

			Assert.Equal( "  123" , Transform(1234567890 , "@s5 999999999999") )
			Assert.Equal( "12345" , Transform("1234567890" , "@S5") )
			Assert.Equal( "1234567890" , Transform("1234567890" , "@S15") )
			Assert.Equal( "1234567890" , Transform("1234567890" , "@S") )
			Assert.Equal( "1234567890" , Transform("1234567890" , "@S0") )
			Assert.Equal( "01/01" , Transform(ConDate(2000,1,1) , "@S5") )


			SetFixed(TRUE)
			SetDigit(5)
			Assert.Equal("  123" , Str(123))
			Assert.Equal("123" , Str(123 , -1))
			Assert.Equal("123" , Str(123 , 3))
			Assert.Equal("-123" , Str(-123 , 4))
			Assert.Equal("-123" , Str(-123 , -1))
			Assert.Equal(" -123" , Str(-123))
			Assert.Equal(" 123" , Str(123 , 4))
			Assert.Equal("**" , Str(123 , 2))
			SetDigit(10)
			Assert.Equal(" 123456789" , Str(123456789))
			Assert.Equal("**********" , Str(12345678901))

			SetFixed(FALSE)
			SetDigit(5)
			Assert.Equal("  123" , Str(123))
			Assert.Equal("123" , Str(123 , -1))
			Assert.Equal("123" , Str(123 , 3))
			Assert.Equal("-123" , Str(-123 , 4))
			Assert.Equal("-123" , Str(-123 , -1))
			Assert.Equal(" -123" , Str(-123))
			Assert.Equal(" 123" , Str(123 , 4))
			Assert.Equal("**" , Str(123 , 2))
			SetDigit(10)
			Assert.Equal(" 123456789" , Str(123456789))
			Assert.Equal("**********" , Str(12345678901))

			SetDecimal(2)
			SetDecimalSep(Asc(","))
			SetThousandSep(Asc("."))
			Assert.Equal( "**",    Str(1.23 , 2 , 1) )
			Assert.Equal( "1,2",   Str(1.2 , 3) )
			Assert.Equal( "123",   Str(123.456 , 3) )

			Assert.Equal( "123",   Str(123.4567 , 3 , 1) )
			Assert.Equal( "123,",  Str(123.4567 , 4 , 2) )
			Assert.Equal( "123,4", Str(123.4567 , 5 , 3) )
			Assert.Equal( "****" , Str(123.4567 , 4 , 3) )
			Assert.Equal( "*****", Str(123.4567 , 5 , 4) )

			Assert.Equal( "***",   Str(-123.4567 , 3 , 1) )
			Assert.Equal( "-123",  Str(-123.4567 , 4 , 2) )
			Assert.Equal( "-123,", Str(-123.4567 , 5 , 3) )
			Assert.Equal( "****" , Str(-123.4567 , 4 , 3) )
			Assert.Equal( "*****", Str(-123.4567 , 5 , 4) )

			Assert.Equal( "0,0",   Str(0.0 , -1) )
			Assert.Equal( "0,1",   Str(0.1 , -1) )
			Assert.Equal( "-0,1",  Str(-0.1 , -1) )
			Assert.Equal( "0,000",   Str(0.0 , -1 , 3) )
			Assert.Equal( "0,100",   Str(0.1 , -1 , 3) )
			Assert.Equal( "-0,100",  Str(-0.1 , -1 , 3) )

			Assert.Equal( "  123"	, Str(FloatFormat(123 , 5, 0)) )
			Assert.Equal( "   123"	, Str(FloatFormat(123 , 6, 0)) )
			Assert.Equal( " 123,5"	, Str(FloatFormat(123.456 , 6, 1) ) )
			Assert.Equal( "123,46"	, Str(FloatFormat(123.456 , 6, 2) ) )
			Assert.Equal( " 123,46"	, Str(FloatFormat(123.456 , 7, 2) ) )


			SetDecimalSep(deci)
			SetThousandSep(thou)
			SetDigit(digit)
			SetDecimal(decimal)
			SetFixed(fixed_)
			SetDigitFixed(digitfixed)
		RETURN

		[Fact, Trait("Category", "Str")];
		METHOD Str_With_SetFixed_TRUE() AS VOID

		   SetDecimalSep(Asc("."))
		   SetThousandSep(Asc(","))
		   SetDecimal(3)
		   SetDigit(12)

		   SetFixed(TRUE)
		   SetDigitFixed(TRUE)

		   Assert.Equal( Str(12.34)            		, "          12.340")
		   Assert.Equal( Str1(12.34)          		, "          12.340")
		   Assert.Equal( Str(123.456)         		, "         123.456")
		   Assert.Equal( Str1(123.456)       		, "         123.456")
		   Assert.Equal( Str(1230.4567)         	, "        1230.457")
		   Assert.Equal( Str1(1230.4567)        	, "        1230.457")
		   Assert.Equal( Str(100000001234.4567)		, "100000001234.457")
		   Assert.Equal( Str1(100000001234.4567)	, "100000001234.457")
		   Assert.Equal( Str(1000000001234.4567)	, "****************")
		   Assert.True ( Str1(1000000001234.4567):StartsWith("********"))
		   Assert.Equal( Str(12.3)              	, "          12.300")
		   Assert.Equal( Str1(12.3)            		, "          12.300")
		   Assert.Equal( Str(12.34567)        	 	, "          12.346")
		   Assert.Equal( Str1(12.34567)        		, "          12.346")
		   Assert.Equal( Str(12.34 , 10, 2)    		, "     12.34")
		   Assert.Equal( Str(12.34 , 5, 2)     		, "12.34")
		   Assert.Equal( Str(12.34 , 4, 2)     		, "12.3")
		   Assert.Equal( Str(12.34 , 4, 1)     		, "12.3")
		   Assert.Equal( Str(12.34 , 2, 1)     		, "**")
		   Assert.Equal( Str3(12.34 , 10, 2)   		, "     12.34")
		   Assert.Equal( Str3(12.34 , 5, 2)    		, "12.34")
		   Assert.Equal( Str3(12.34 , 4, 2)     	, "12.3")
		   Assert.Equal( Str3(12.34 , 4, 1)     	, "12.3")
		   Assert.Equal( Str3(12.34 , 2, 1)    		, "**")
		   Assert.Equal( Str(-12.34)            	, "         -12.340")
		   Assert.Equal( Str1(-12.34)           	, "         -12.340")
		   Assert.Equal( Str(-12.3456)         		, "         -12.346")
		   Assert.Equal( Str1(-12.3456)         	, "         -12.346")
		   Assert.Equal( Str(-12.34 , 10, 2)    	, "    -12.34")
		   Assert.Equal( Str(-12.34 , 5, 2)     	, "-12.3")	 //VO wrong: -2.34
		   Assert.Equal( Str(-12.34 , 4, 2)     	, "-12.")    // VO wrong: -12.
		   Assert.Equal( Str(-12.34 , 4, 1)     	, "-12.")    //VO wrong: -2.3
	       Assert.Equal( Str(-12.34 , 4, 0)     	, " -12")    //VO wrong: -2.3
		   Assert.Equal( Str(-12.34 , 2, 1)     	, "**")


		   SetDigit(8)
		   SetDecimal(2)
		   SetDecimalSep(Asc(","))
		   SetThousandSep(Asc("."))

		   Assert.Equal( Str(123456789.1)	, "***********")
		   Assert.True(Str1(123456789.1):StartsWith("*****"))
		   Assert.Equal( Str(12345678.1)	, "12345678,10")
		   Assert.Equal(Str1(12345678.1)	, "12345678,10")
		   Assert.Equal( Str(1234567.1)		, " 1234567,10")
		   Assert.Equal(Str1(1234567.1)		, " 1234567,10")
		   Assert.Equal( Str(123456.1)		, "  123456,10")
		   Assert.Equal(Str1(123456.1)		, "  123456,10")

		RETURN

		[Fact, Trait("Category", "Str")];
		METHOD Str_With_SetFixed_FALSE() AS VOID

		   SetDecimalSep(Asc("."))
		   SetThousandSep(Asc(","))
		   SetDecimal(3)
		   SetDigit(12)

		   SetFixed(FALSE)
		   SetDigitFixed(FALSE)

		   Assert.Equal( Str(12.34)            		, "          12.34")
		   Assert.Equal( Str1(12.34)          		, "          12.34")
		   Assert.Equal( Str(123.456)         		, "         123.456")
		   Assert.Equal( Str1(123.456)				, "         123.456")
		   Assert.Equal( Str(1230.4567)         	, "        1230.4567")
		   Assert.Equal( Str1(1230.4567)        	, "        1230.4567")
		   Assert.Equal( Str(100000001234.4567)		, "100000001234.4567")
		   Assert.Equal( Str1(100000001234.4567)	, "100000001234.4567")
		   Assert.Equal( Str(1000000001234.4567)	, "*****************")
//		   Assert.Equal( Str1(1000000001234.4567)	, "*****************")
		   Assert.True(  Str1(1000000001234.4567):StartsWith("*")) // it's really impossible to replicate completely VO's insane behavior
		   Assert.Equal( Str(12.3)              	, "          12.3")
		   Assert.Equal( Str1(12.3)            		, "          12.3")
		   Assert.Equal( Str(12.34567)         		, "          12.34567")
		   Assert.Equal( Str1(12.34567)        		, "          12.34567")
		   Assert.Equal( Str(12.34 , 10, 2)    		, "     12.34")
		   Assert.Equal( Str(12.34 , 5, 2)     		, "12.34")
		   Assert.Equal( Str(12.34 , 4, 2)     		, "12.3") // VO returns 12.3
		   Assert.Equal( Str(12.34 , 4, 1)     		, "12.3")
		   Assert.Equal( Str(12.34 , 2, 1)     		, "**")
		   Assert.Equal( Str3(12.34 , 10, 2)   		, "     12.34")
		   Assert.Equal( Str3(12.34 , 5, 2)    		, "12.34")
		   Assert.Equal( Str3(12.34 , 4, 2)     	, "12.3")   // VO 12.3
		   Assert.Equal( Str3(12.34 , 4, 1)     	, "12.3")
		   Assert.Equal( Str3(12.34 , 2, 1)    		, "**")
		   Assert.Equal( Str(-12.34)            	, "         -12.34")
		   Assert.Equal( Str1(-12.34)           	, "         -12.34")
		   Assert.Equal( Str(-12.3456)         		, "         -12.3456")
		   Assert.Equal( Str1(-12.3456)         	, "         -12.3456")
		   Assert.Equal( Str(-12.34 , 10, 2)    	, "    -12.34")
		   Assert.Equal( Str(-12.34 , 5, 2)     	, "-12.3")	 //VO: -2.34
		   Assert.Equal( Str(-12.34 , 4, 2)     	, "-12.")    // VO
		   Assert.Equal( Str(-12.34 , 4, 1)     	, "-12.")    //VO: -2.3
		   Assert.Equal( Str(-12.34 , 2, 1)     	, "**")

		   SetDigit(8)
		   SetDecimal(2)

		   Assert.True( Str(123456789.1):StartsWith("*"))
		   Assert.True(Str1(123456789.1):StartsWith("*"))

		   Assert.Equal( "12.34" , Str(12.34 , -1))
		   Assert.Equal( "12.3400" , Str(12.34 , -1 , 4))
		   Assert.Equal( "1.1" , Str(1.1 , -1 , 1))

            [Fact, Trait("Category", "Str")];
		    METHOD Pad_Test() AS VOID
                LOCAL u AS USUAL
                Assert.Equal(Space(10), PadR(u,10))
                Assert.Equal(Space(10), Pad(u,10))
                Assert.Equal(Space(10), PadC(u,10))
                Assert.Equal(Space(10), PadL(u,10))
                u := "a"
                Assert.Equal("a         ", PadR(u,10))
                Assert.Equal("a         ", Pad(u,10))
                Assert.Equal("    a     ", PadC(u,10))
                Assert.Equal("----a-----", PadC(u,10,"-"))
                Assert.Equal("         a", PadL(u,10))
                u := 1
                Assert.Equal("1         ", PadR(u,10))
                Assert.Equal("1         ", Pad(u,10))
                Assert.Equal("    1     ", PadC(u,10))
                Assert.Equal("====1=====", PadC(u,10,"="))
                Assert.Equal("         1", PadL(u,10))
                u := 1.23
                SetDecimalSep(Asc("."))
                Assert.Equal("1.23      ", PadR(u,10))
                Assert.Equal("1.23      ", Pad(u,10))
                Assert.Equal("   1.23   ", PadC(u,10))
                Assert.Equal("...1.23...", PadC(u,10,"."))
                Assert.Equal("      1.23", PadL(u,10))


#pragma options ("vo13" , on)
            [Fact, Trait("Category", "Str")];
		    METHOD StringComparison_WINDOWS_notEXACT() AS VOID
				LOCAL uAB,uA AS USUAL
				LOCAL sAB,sA AS USUAL
				LOCAL c,e,vo13 AS USUAL
				vo13 := XSharp.RuntimeState.CompilerOptionVO13
				XSharp.RuntimeState.CompilerOptionVO13 := TRUE
				c := SetCollation(#WINDOWS)
				e := SetExact(FALSE)

				uAB := "AB"
				uA := "A"
				sAB := uAB
				sA := uA

				Assert.True ( "AB" = "A"  )
				Assert.False( "A" = "AB"  )
				Assert.True ( uAB = uA    )
				Assert.False( uA = uAB    )
				Assert.True ( sAB = sA    )
				Assert.False( sA = sAB    )

				Assert.True ( "AB" != "A" ) // Moronic VO behavior, different behavior with literals than with USUALs/STRINGs
				Assert.True ( "A" != "AB" )
				Assert.False( uAB != uA   )
				Assert.True ( uA != uAB   )
				Assert.False( sAB != sA   )
				Assert.True ( sA != sAB   )

				Assert.True ( "AB" >= "A" )
				Assert.True ( "AB" <= "A" )
				Assert.True ( uAB >= uA   )
				Assert.True ( uAB <= uA   )
				Assert.True ( sAB >= sA   )
				Assert.True ( sAB <= sA   )

				Assert.False( "AB" > "A"  )
				Assert.False( "AB" < "A"  )
				Assert.False( uAB > uA    )
				Assert.False( uAB < uA    )
				Assert.False( sAB > sA    )
				Assert.False( sAB < sA    )

				SetCollation(c)
				SetExact(e)

				XSharp.RuntimeState.CompilerOptionVO13 := vo13

            [Fact, Trait("Category", "Str")];
		    METHOD StringComparison_CLIPPER_notEXACT() AS VOID
				LOCAL uAB,uA AS USUAL
				LOCAL sAB,sA AS USUAL
				LOCAL c,e,vo13 AS USUAL
				vo13 := XSharp.RuntimeState.CompilerOptionVO13
				XSharp.RuntimeState.CompilerOptionVO13 := TRUE
				c := SetCollation(#CLIPPER)
				e := SetExact(FALSE)

				uAB := "AB"
				uA := "A"
				sAB := uAB
				sA := uA

				Assert.True ( "AB" = "A"  )
				Assert.False( "A" = "AB"  )
				Assert.True ( uAB = uA    )
				Assert.False( uA = uAB    )
				Assert.True ( sAB = sA    )
				Assert.False( sA = sAB    )

				Assert.True ( "AB" != "A" )
				Assert.True ( "A" != "AB" )
				Assert.False( uAB != uA   )
				Assert.True ( uA != uAB   )
				Assert.False( sAB != sA   )
				Assert.True ( sA != sAB   )

				Assert.True ( "AB" >= "A" )
				Assert.True ( "AB" <= "A" )
				Assert.True ( uAB >= uA   )
				Assert.True ( uAB <= uA   )
				Assert.True ( sAB >= sA   )
				Assert.True ( sAB <= sA   )

				Assert.False( "AB" > "A"  )
				Assert.False( "AB" < "A"  )
				Assert.False( uAB > uA    )
				Assert.False( uAB < uA    )
				Assert.False( sAB > sA    )
				Assert.False( sAB < sA    )

				SetCollation(c)
				SetExact(e)

				XSharp.RuntimeState.CompilerOptionVO13 := vo13

            [Fact, Trait("Category", "Str")];
		    METHOD StringComparison_WINDOWS_EXACT() AS VOID
				LOCAL uAB,uA AS USUAL
				LOCAL sAB,sA AS USUAL
				LOCAL c,e,vo13 AS USUAL
				vo13 := XSharp.RuntimeState.CompilerOptionVO13
				XSharp.RuntimeState.CompilerOptionVO13 := TRUE
				c := SetCollation(#WINDOWS)
				e := SetExact(TRUE)

				uAB := "AB"
				uA := "A"
				sAB := uAB
				sA := uA

				Assert.False( "AB" = "A"  )
				Assert.False( "A" = "AB"  )
				Assert.False( uAB = uA    )
				Assert.False( uA = uAB    )
				Assert.False( sAB = sA    )
				Assert.False( sA = sAB    )

				Assert.True ( "AB" != "A" )
				Assert.True ( "A" != "AB" )
				Assert.True ( uAB != uA   )
				Assert.True ( uA != uAB   )
				Assert.True ( sAB != sA   )
				Assert.True ( sA != sAB   )

				Assert.True ( "AB" >= "A" )
				Assert.False( "AB" <= "A" )
				Assert.True ( uAB >= uA   )
				Assert.False( uAB <= uA   )
				Assert.True ( sAB >= sA   )
				Assert.False( sAB <= sA   )

				Assert.True ( "AB" > "A"  )
				Assert.False( "AB" < "A"  )
				Assert.True ( uAB > uA    )
				Assert.False( uAB < uA    )
				Assert.True ( sAB > sA    )
				Assert.False( sAB < sA    )

				SetCollation(c)
				SetExact(e)

				XSharp.RuntimeState.CompilerOptionVO13 := vo13

            [Fact, Trait("Category", "Str")];
		    METHOD StringComparison_CLIPPER_EXACT() AS VOID
				LOCAL uAB,uA AS USUAL
				LOCAL sAB,sA AS USUAL
				LOCAL c,e,vo13 AS USUAL
				vo13 := XSharp.RuntimeState.CompilerOptionVO13
				XSharp.RuntimeState.CompilerOptionVO13 := TRUE
				c := SetCollation(#CLIPPER)
				e := SetExact(TRUE)

				uAB := "AB"
				uA := "A"
				sAB := uAB
				sA := uA

				Assert.False( "AB" = "A"  )
				Assert.False( "A" = "AB"  )
				Assert.False( uAB = uA    )
				Assert.False( uA = uAB    )
				Assert.False( sAB = sA    )
				Assert.False( sA = sAB    )

				Assert.True ( "AB" != "A" )
				Assert.True ( "A" != "AB" )
				Assert.True ( uAB != uA   )
				Assert.True ( uA != uAB   )
				Assert.True ( sAB != sA   )
				Assert.True ( sA != sAB   )

				Assert.True ( "AB" >= "A" )
				Assert.False( "AB" <= "A" )
				Assert.True ( uAB >= uA   )
				Assert.False( uAB <= uA   )
				Assert.True ( sAB >= sA   )
				Assert.False( sAB <= sA   )

				Assert.True ( "AB" > "A"  )
				Assert.False( "AB" < "A"  )
				Assert.True ( uAB > uA    )
				Assert.False( uAB < uA    )
				Assert.True ( sAB > sA    )
				Assert.False( sAB < sA    )

				SetCollation(c)
				SetExact(e)

				XSharp.RuntimeState.CompilerOptionVO13 := vo13


#pragma options ("vo13" , default)

  [Fact, Trait("Category", "Between")];
	METHOD BetweenTest() AS VOID
        LOCAL s1, s2, s3 AS STRING
        s1 := "BBB"
        s2 := "AAA"
        s3 := "CCC"
        SetExact(TRUE)
        RuntimeState.CompilerOptionVO13 := TRUE
        Assert.True(Between(s1, s2, s3))
        Assert.True(Between(s2, s2, s3))
        Assert.True(Between(s3, s2, s3))
        RuntimeState.CompilerOptionVO13 := FALSE
        Assert.True(Between(s1, s2, s3))
        Assert.True(Between(s2, s2, s3))
        Assert.True(Between(s3, s2, s3))
        RuntimeState.CompilerOptionVO13 := TRUE
        Assert.False(Between(s2, s1, s3))
        RuntimeState.CompilerOptionVO13 := FALSE
        Assert.False(Between(s2, s1, s3))

        SetExact(FALSE)
        // AAA = A
        S1 := "AAA"
        s2 := "A"
        RuntimeState.CompilerOptionVO13 := TRUE
        Assert.True(Between(s1, s2, s3))
        RuntimeState.CompilerOptionVO13 := FALSE
        Assert.True(Between(s1, s2, s3))

	END CLASS

END NAMESPACE
