USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using XSharp.Runtime

BEGIN NAMESPACE XSharp.Runtime.Tests

	CLASS SymbolTests

		[Fact];
		METHOD CreateSymbolTest() as void
			VAR sym := __Symbol{"TestSymbol",true}
			Assert.Equal("TESTSYMBOL",sym:ToString())
		RETURN
		[Fact];
		METHOD CompareSymbolTest() as void
			var sym1 := #TestSymbol
			var sym2 := #TestSymbol
			Assert.Equal(true,sym1==sym2)
			Assert.Equal(true,sym1=="TESTSYMBOL")
			Assert.Equal(false,sym1==#TestSymbol1)
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests