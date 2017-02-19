USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using Microsoft.VisualStudio.TestTools.UnitTesting
using static Microsoft.VisualStudio.TestTools.UnitTesting.Assert
using XSharp.Runtime


BEGIN NAMESPACE XSharp.Runtime.Tests

	[TestClass];
	CLASS RuntimeUsualTests

		[TestMethod];
		METHOD DateTimeTest() as void

			local now as __VODate
			local u   as __Usual
		    now := (__VODate)System.DateTime.Now 
			u := __Usual{now}
			var s := u:ToString()
			AreEqual(now:ToString(),u:ToString())
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests