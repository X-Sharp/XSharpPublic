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

	CLASS FloatTests
		[Fact, Trait("Category", "Numeric")];
		METHOD CONTIMETest() AS VOID
			Assert.Equal("13:34:54",CONTIME((DWORD)13,(DWORD)34,(DWORD)54))
		RETURN

		[Fact, Trait("Category", "Numeric")];
		METHOD FracTest() AS VOID
			Assert.Equal((FLOAT)120,Fact((DWORD)5))
		RETURN

	END CLASS
END NAMESPACE
