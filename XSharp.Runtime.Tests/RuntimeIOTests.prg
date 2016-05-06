USING System
USING System.Collections.Generic
USING System.Text


USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using Microsoft.VisualStudio.TestTools.UnitTesting
using static Microsoft.VisualStudio.TestTools.UnitTesting.Assert
using XSharp.Runtime
using Vulcan

BEGIN NAMESPACE XSharp.Runtime.Tests

	[TestClass];
	CLASS RuntimeIOTests

		[TestMethod];
		METHOD FileTest() as void
			AreEqual(true,File("c:\windows\system32\shell32.dll"))
			AreEqual(false,File(null))
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests