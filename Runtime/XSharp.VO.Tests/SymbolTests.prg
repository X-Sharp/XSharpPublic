using System
using System.Collections.Generic
using System.Linq
using System.Text
using XUnit

BEGIN NAMESPACE XSharp.VO.Tests

	CLASS SymbolTests
	
		[Fact, Trait("Category", "Symbol")];
		METHOD CreateSymbolTest() as void
			VAR sym := #TestSymbol
			Assert.Equal("TESTSYMBOL",sym:ToString())
		RETURN
		[Fact, Trait("Category", "Symbol")];
		METHOD CompareSymbolTest() as void
			var sym1 := #TestSymbol
			var sym2 := #TestSymbol
			Assert.Equal(true,sym1==sym2)
			Assert.Equal(true,sym1=="TESTSYMBOL")
			Assert.Equal(false,sym1==#TestSymbol1)
		RETURN

		[Fact, Trait("Category", "Symbol")];
		METHOD GreaterSymbolTest() as void
			var sym1 := #TestSymbol1
			var sym2 := #TestSymbol2
			Assert.Equal(true,sym1<=sym2)
			Assert.Equal(true,sym1<sym2)
			Assert.Equal(false,sym1 > sym2)
			Assert.Equal(false,sym1 >= sym2)
			Assert.Equal(true,sym2 > sym1)
			Assert.Equal(true,sym2 >= sym1)
			Assert.Equal(false,sym2 < sym1)
			sym2 := #testSymbol
			Assert.Equal(false,sym1=sym2)
			SetExact(false)
			// with setequal FALSE then #TestSymbol1 == #testSymbol
			Assert.Equal(false,sym1<sym2)
			Assert.Equal(true,sym1<=sym2)
		RETURN

		[Fact, Trait("Category", "Symbol")];
		METHOD ImplicitConverter() as void
			local s as string
			local sym as Symbol
			sym := #Test
			s := sym
			Assert.Equal(s, sym:ToString())
			sym := s
			Assert.Equal(s, sym:ToString())

		[Fact, Trait("Category", "Symbol")];
		METHOD ExplicitConverter() as void
			local d as DWORD
			local sym1 as Symbol
			local sym2 as Symbol
			sym1 := #test
			d:= (DWORD) sym1
			sym2 := (Symbol) d
			Assert.Equal(sym1, sym2)
			sym2 := (Symbol) 0x42U
			Assert.NotEqual(sym1, sym2)
			

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests