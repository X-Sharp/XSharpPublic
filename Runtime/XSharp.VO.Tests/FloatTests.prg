USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit



BEGIN NAMESPACE XSharp.VO.Tests

	class FloatTests
		[Fact, Trait("Category", "Numeric")];
		METHOD CONTIMETest() as void
			Assert.Equal("13:34:54",CONTIME((dword)13,(dword)34,(dword)54))
		RETURN

		[Fact, Trait("Category", "Numeric")];
		METHOD FracTest() as void
			Assert.Equal((Float)120,Fact((dword)5))
		RETURN

	END CLASS
end namespace
