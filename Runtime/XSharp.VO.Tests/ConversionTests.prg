USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using System.Globalization


BEGIN NAMESPACE XSharp.VO.Tests

	CLASS ConversionTests

		[Fact, Trait("Category", "Date")];
		method AsStringTest() as void 
			local u as usual
			u := "123"
			Assert.Equal("123", AsString(u))
			u := #A123
			Assert.Equal("A123", AsString(u))



	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
