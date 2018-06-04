//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using System.Globalization


BEGIN NAMESPACE XSharp.VO.Tests

	CLASS VoConversionTests

		[Fact, Trait("Category", "Conversion")];
		method AsStringTest() as void 
			local u as usual
			u := "123"
			Assert.Equal("123", AsString(u))
			u := #A123
			Assert.Equal("A123", AsString(u))

			Assert.Equal("1", AsString(1))
			var c1 := GetRTFullPath()
			Assert.Equal(true, c1:ToLower():IndexOf(".vo.dll") > 0)

			var n1 := GetThreadCount()
			Assert.Equal(true, n1 > 1)

		[Fact, Trait("Category", "Conversion")];
		method StrTest() as void 
			local c as String
			SetDecimalSep(46)
			c := Str3(12.3456,5,2)
			Assert.Equal("12.35", c)	// ROunded up
			c := Str3(12.3411,5,2)
			Assert.Equal("12.34", c)	// ROunded down
			c := Str3(FloatFormat(12.3456,10,4),10,5)
			Assert.Equal("  12.34560", c)	// ROunded down
			c := StrZero(12.3456,10,2)
			Assert.Equal("0000012.35", c)	// ROunded up
			c := STR3(2.49999,4,2)
			assert.Equal("2.50", c )  
			c := STR3(2.50012,4,2)
			assert.Equal("2.50", c )  

		[Fact, Trait("Category", "Val")];
		METHOD ValTest() AS VOID
			LOCAL u AS USUAL
			u := Val("1.234")
			Assert.Equal(1.234, (REAL8)(FLOAT) u)
			SetDecimalSep(44) // ,
			u := Val("1,234")
			Assert.Equal(1.234, (Real8)(Float) u)
			u := Val("1.23E2")
			Assert.Equal(123, (REAL8)(FLOAT) u)
			u := Val("1.2345E2")
			Assert.Equal(123.45, (Real8)(Float) u)


	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
