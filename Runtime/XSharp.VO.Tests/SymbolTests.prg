//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
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
			VAR sym2 := #TestSymbol2
			Assert.Equal(#Windows,SetCollation())
			SetCollation(#Ordinal)
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
			
		[Fact, Trait("Category", "Symbol")];
		METHOD AtomTester() as void
			local sym1 as symbol
			local sym2 as symbol
			local dwStart as dword
			dwStart := MaxAtom()
			sym1 := SysAddAtom("Robert")
			sym2 := String2Symbol("Robert")
			Assert.Equal(MaxAtom(),dwstart+2)		// there are symbols defined in the global symbol table
			Assert.NotEqual(sym1, sym2)
			Assert.NotEqual(sym1:ToString(), sym2:ToString())
			Assert.Equal(sym1:ToString():ToUpper(), sym2:ToString())
			sym2 := SysFindAtom("Robert")
			Assert.Equal(sym1, sym2)
			sym2 := SysFindAtom("RobertIsNotThere")		// Find should not have added the new symbol
			Assert.Equal(MaxAtom(),dwstart+2)
			Assert.NotEqual(sym1, sym2)
			sym1 := ConCatAtom(#one, #two)
			Assert.Equal(sym1, #onetwo)
			sym1 := ConCatAtom3(#one, #two,#three)
			Assert.Equal(sym1, #onetwothree)
			sym1 := ConCatAtom5(#one, #two,#three,#four,#five)
			Assert.Equal(sym1, #onetwothreefourfive)
			Assert.Equal(MaxAtom(),dwstart+2)			// the literals do not create a new atom


	END CLASS
END NAMESPACE // XSharp.Runtime.Tests