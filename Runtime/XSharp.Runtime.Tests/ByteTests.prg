USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using XSharp.Runtime


BEGIN NAMESPACE XSharp.Runtime.Tests

	CLASS ByteTests

		[Fact];
		METHOD SwapByteTest() as void
			Assert.Equal((word)86,SwapByte((byte)101))
		RETURN
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests