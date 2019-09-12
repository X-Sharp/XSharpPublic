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


BEGIN NAMESPACE XSharp.VO.Tests

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

		[Fact, Trait("Category", "Conversion")];
		METHOD StrTest() AS VOID 
			LOCAL c AS STRING
            LOCAL f AS FLOAT
			SetDecimalSep(46)
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

		[Fact, Trait("Category", "Val")];
		METHOD ValTest() AS VOID
			LOCAL u AS USUAL
            SetDecimalSep('.')
            SetThousandSep(',') 
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
			Assert.Equal("123.456", AsString(Val("  123,456"  )))
			Assert.Equal("-123.4567", AsString(Val(" - 123.4567")))

			Assert.Equal( "  12345678901234567000", Str(Val("12345678901234567890"),22,-1) )
			Assert.Equal( " -12345678901234567000", Str(Val("-12345678901234567890"),22,-1) )

			Assert.Equal("255", AsString(Val("0xFF")))
			Assert.Equal("-255", AsString(Val("-0xFF")))
			Assert.Equal("-255", AsString(Val(" - 0xFF")))
			Assert.Equal("65534", AsString(Val("0xFFFE")))
			Assert.Equal("2147483648", AsString(Val(" 0X80000000")))

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

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
