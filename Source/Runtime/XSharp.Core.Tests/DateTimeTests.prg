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

	CLASS DateTimeTests

		[Trait("Category", "DateTime")];
		[Fact];
		METHOD Conversions() AS VOID 
			Assert.Equal(DateTime.MinValue, SToDt(" "))
			Assert.Equal(DateTime.MinValue, SToDt("424242"))
			Assert.Equal(DateTime{2021,1,1}, SToDt("20210101"))
		    Assert.Equal(DtToS(	DateTime{2021,1,1}), "20210101")
		    Assert.Equal(DtToS(	DateTime.MinValue), "        ")
            Assert.Equal(CToDtAnsi(	"2021.10.21") , DateTime{2021,10,21} )
            SetEpoch(1930)
            Assert.Equal(CToDt("21.10.21","YY.MM.DD"), DateTime{2021,10,21} )
            Assert.Equal(	DateTime{2021,1,1}, DateTime(2021,1,1))
            Assert.Equal(	DateTime{2021,1,2}, DateTime(2021,1,2,0))
            Assert.Equal(	DateTime{2021,1,3}, DateTime(2021,1,3,0,0))
            Assert.Equal(	DateTime{2021,1,4}, DateTime(2021,1,4,0,0,0))
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
