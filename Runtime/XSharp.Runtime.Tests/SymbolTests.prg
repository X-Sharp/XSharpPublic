using System
using System.Collections.Generic
using System.Linq
using System.Text
using XUnit
using XSharp.Runtime

BEGIN NAMESPACE XSharp.Runtime.Tests

	CLASS SymbolTests
	
		[Fact, Trait("Category", "Symbol")];
		METHOD CreateSymbolTest() as void
			VAR sym := __Symbol{"TestSymbol",true}
			Assert.Equal("TESTSYMBOL",sym:ToString())
		RETURN
		[Fact, Trait("Category", "Symbol")];
		METHOD CompareSymbolTest() as void
			var sym1 := __Symbol{"TestSymbol"}
			var sym2 := __Symbol{"TestSymbol"}
			Assert.Equal(true,sym1==sym2)
			Assert.Equal(true,sym1=="TESTSYMBOL")
			Assert.Equal(false,sym1==#TestSymbol1)
		RETURN

		[Fact, Trait("Category", "Symbol")];
		METHOD GreaterSymbolTest() as void
			var sym1 := __Symbol{"TestSymbol1"}
			var sym2 := __Symbol{"TestSymbol2"}
			Assert.Equal(true,sym1<=sym2)
			Assert.Equal(true,sym1<sym2)
			Assert.Equal(false,sym1 > sym2)
			Assert.Equal(false,sym1 >= sym2)
			Assert.Equal(true,sym2 > sym1)
			Assert.Equal(true,sym2 >= sym1)
			Assert.Equal(false,sym2 < sym1)
		RETURN

		[Fact, Trait("Category", "Symbol")];
		METHOD ImplicitConverter() as void
			local s as string
			local sym as __Symbol
			sym := __Symbol{"test"}
			s := sym
			Assert.Equal(s, sym:ToString())
			sym := s
			Assert.Equal(s, sym:ToString())

		[Fact, Trait("Category", "Symbol")];
		METHOD ExplicitConverter() as void
			local d as DWORD
			local sym1 as __Symbol
			local sym2 as __Symbol
			sym1 := __Symbol{"test"}
			d:= (DWORD) sym1
			sym2 := (__Symbol) d
			Assert.Equal(sym1, sym2)
			sym2 := (__Symbol) 0x42U
			Assert.NotEqual(sym1, sym2)
			

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests