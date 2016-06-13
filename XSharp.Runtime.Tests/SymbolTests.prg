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
	CLASS SymbolTests

		[TestMethod];
		METHOD CreateSymbolTest() as void
			var sym := #TestSymbol
			AreEqual("TestSymbol",sym:ToString())
		RETURN
		[TestMethod];
		METHOD CompareSymbolTest() as void
			var sym1 := #TestSymbol
			var sym2 := #TestSymbol
			AreEqual(true,sym1==sym2)
			AreEqual(true,sym1=="TestSymbol")
			AreEqual(false,sym1==#TestSymbol1)
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests