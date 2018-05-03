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

	CLASS ConversionTests

		[Fact, Trait("Category", "AsString")];
		method AsStringTest() as void 
			local u as usual
			u := "123"
			Assert.Equal("123", AsString(u))
			u := #A123
			Assert.Equal("A123", AsString(u))

			var c1 := GetRTFullPath()
			Assert.Equal(true, c1:ToLower():IndexOf(".vo.dll") > 0)

			var n1 := GetThreadCount()
			Assert.Equal(true, n1 > 1)

		[Fact, Trait("Category", "AsString")];
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


	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
