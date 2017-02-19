USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using Microsoft.VisualStudio.TestTools.UnitTesting
using static Microsoft.VisualStudio.TestTools.UnitTesting.Assert
using XSharp.Runtime


BEGIN NAMESPACE XSharp.Runtime.Tests

	[TestClass];
	CLASS RuntimeByteTests

		[TestMethod];
		METHOD SwapByteTest() as void
			AreEqual((word)86,SwapByte((byte)101))
		RETURN
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests