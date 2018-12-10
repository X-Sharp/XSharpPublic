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
			assert.Equal("  -0.000000000000005", c )  

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
