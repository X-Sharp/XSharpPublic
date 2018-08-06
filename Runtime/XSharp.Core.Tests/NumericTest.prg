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


BEGIN NAMESPACE XSharp.Core.Tests

	CLASS NumericTests

		[Fact, Trait("Category", "Numeric")];
		METHOD CHRTest() AS VOID
			Assert.Equal(" ",CHR((DWORD)32))
		RETURN


		[Fact, Trait("Category", "Numeric")];
		METHOD DW2BINTest() AS VOID
			Assert.Equal("    ",DW2Bin((DWORD) 32*256*256*256+32*256*256+32*256+32))
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests 