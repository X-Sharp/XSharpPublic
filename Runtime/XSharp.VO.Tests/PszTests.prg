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

begin namespace XSharp.VO.Tests
	
	class pszTests
		[Fact, Trait("Category", "Psz")];
		method CreatePszTest() as void
			local p as psz
			p := psz{"Robert was here"}
			Assert.Equal("Robert was here", Psz2String(p))
			Assert.Equal((int) PszLen(p), 15)

	end class
end namespace

