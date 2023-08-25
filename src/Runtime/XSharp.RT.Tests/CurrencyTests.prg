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

            local i64 as int64
            local u as usual
            i64 := 1
            u := i64
            c1 := $41
            Assert.Equal($42, c1 + u)
            Assert.Equal($40, c1 - u)

[Fact, Trait("Category", "Numeric")];
        METHOD Collection_Tests() AS VOID

			local aValuesC := List<CURRENCY>{} as List<CURRENCY>
            aValuesC:Add($1.0)
            aValuesC:Add($2.0)
            aValuesC:Add($3.0)
            aValuesC:Add($4.0)
            var sumC := aValuesC:Sum()
            var minC := aValuesC:Min()
            var maxC := aValuesC:Max()
            Assert.True (sumC == $10.0)
            Assert.True (minC == $1.0)
            Assert.True (maxC == $4.0)

END CLASS
END NAMESPACE
