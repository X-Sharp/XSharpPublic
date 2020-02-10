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


// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS NumericTests
	
		[Fact, Trait("Category", "Numeric")];
		METHOD ConversionTests() AS VOID
            LOCAL c1 AS CURRENCY
            LOCAL c2 AS CURRENCY
            LOCAL f1 AS FLOAT
            LOCAL f2 AS FLOAT
            c1 := $1.2345
            c2 := NToM(1.2345444)
            Assert.True( c1  == c2 )
            f1 := 1.2345
            f2 := MToN($1.2345)
            Assert.True( f1  == f2 )
            Assert.Equal(1, Sign(1.2345))
            Assert.Equal(-1, Sign(-1.2345))
            Assert.Equal(0, Sign(0.0))
	END CLASS

END NAMESPACE
