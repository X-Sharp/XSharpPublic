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
	CLASS RuntimeDateTests

		[TestMethod];
		METHOD DToCTest() as void
			var u := ctod("03/13/2016")
			AreEqual("01.01.2016" ,ctod("01/01/2016").ToString())
			AreEqual("13.02.2016" ,ctod("13/02/2016").ToString())
			AreEqual("01.01.0001" ,ctod("03/13/2016").ToString())	
		RETURN
		[TestMethod];
		METHOD ElapTimeTest() as void
			AreEqual("11:23:34",elaptime("12:00:00","23:23:34"))
			AreEqual("-11:23:34",elaptime("23:23:34","12:00:00"))	
			AreEqual("",elaptime("29:23:34","12:00:00"))
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests