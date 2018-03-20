USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit



BEGIN NAMESPACE XSharp.Core.Tests

	CLASS ByteTests

		[Trait("Category", "Byte")];
		[Fact];
		METHOD SwapByteTest() as void 
			Assert.Equal((word)86,SwapByte((byte)101))
		RETURN
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests