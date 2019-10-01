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
BEGIN NAMESPACE XSharp.VO.Tests

	CLASS StringTests
	
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
			LOCAL exact,thou,digit,decimal,fixed_,digitfixed AS USUAL
			exact := SetDecimalSep(Asc("."))
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
			Assert.Equal( "123456.78901234568"	, Str(123456.78901234567) )
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
			
	
			SetDecimalSep(exact)
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
                Assert.Equal("         a", PadL(u,10))
                u := 1
                Assert.Equal("1         ", PadR(u,10))
                Assert.Equal("1         ", Pad(u,10))
                Assert.Equal("    1     ", PadC(u,10))
                Assert.Equal("         1", PadL(u,10))
                u := 1.23
                SetDecimalSep(Asc("."))
                Assert.Equal("1.23      ", PadR(u,10))
                Assert.Equal("1.23      ", Pad(u,10))
                Assert.Equal("   1.23   ", PadC(u,10))
                Assert.Equal("      1.23", PadL(u,10))

	END CLASS

END NAMESPACE
