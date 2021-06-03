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


BEGIN NAMESPACE XSharp.RT.Tests

    CLASS CurrencyTests
 		[Trait("Category", "Currency")];
		[Fact];
		METHOD SimpleCurrencyTests() AS VOID
            LOCAL c1 as Currency
            LOCAL c2 as Currency
            LOCAL c3 as Currency
            c1 := $1.23
            c2 := $4.56
            c3 := c1 + c2
            Assert.Equal(c3, $5.79)
            c1 := -c1
            Assert.Equal(c1, -$1.23)

            VAR i := Integer(c1)
            Assert.Equal(-1, (INT) i)

            i := Integer(c2)
            Assert.Equal(4, (INT) i)


END CLASS
END NAMESPACE
