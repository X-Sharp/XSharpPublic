USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit


BEGIN NAMESPACE XSharp.Core.Tests

	CLASS NumericTests

		[Fact, Trait("Category", "Numeric")];
		METHOD CHRTest() as void
			Assert.Equal(" ",CHR((DWORD)32))
		RETURN


		[Fact, Trait("Category", "Numeric")];
		METHOD DW2BINTest() as void
			Assert.Equal("    ",DW2Bin((dword) 32*256*256*256+32*256*256+32*256+32))
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests 