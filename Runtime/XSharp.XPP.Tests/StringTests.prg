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
begin namespace XSharp.XPP.Tests

	class StringTests

		[Fact, Trait("Category", "String")];
		method Var2CharTests as void
            Assert.Equal(DToS(Today()), Var2Char(ToDay()))
            Assert.Equal("{1, 2}", Var2Char({1,2}))
            Assert.Equal("123.456", Var2Char(123.456))
            Assert.Equal(".T.", Var2Char(true))
            Assert.Equal(".F.", Var2Char(false))
            Assert.Equal("{|x| x + 1}", Var2Char({|x| x + 1}))
            Assert.Equal("NIL", Var2Char(nil))
            Assert.Equal("Hello World", Var2Char("Hello World"))
            Assert.Equal("StringTests", Var2Char(StringTests{}))
		return

	end class

end namespace
