//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit

BEGIN NAMESPACE XSharp.VO.Tests
	
	CLASS pszTests
		[Fact, Trait("Category", "Psz")];
		METHOD CreatePszTest() AS VOID
			LOCAL p AS PSZ
			p := String2Psz("Robert was here")
			Assert.Equal("Robert was here", Psz2String(p))
			Assert.Equal((INT) PszLen(p), 15)

	END CLASS
END NAMESPACE

