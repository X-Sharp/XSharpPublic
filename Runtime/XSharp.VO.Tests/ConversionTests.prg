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

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
