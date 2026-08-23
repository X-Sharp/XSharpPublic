//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using XUnit

begin namespace XSharp.VFP.Tests
    class OOPTests
        static constructor
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        end constructor

        [Fact, Trait("Category", "OOP")];
        method NumericPropertyIsUsableInArithmetic as void
            var oCircle := Circle{}
            oCircle:Ratio := 5
            Assert.Equal(31.4, (real8)oCircle:Perimeter(), 2)
        end method

        [Fact, Trait("Category", "OOP")];
        method NumericPropertyRoundTripsThroughLateBoundAccess as void
            var oCircle := Circle{}
            oCircle:Ratio := 5
            Assert.Equal(10, (int)(oCircle:Ratio * 2))
        end method
    end class

    define class Circle as Custom
        ratio = 0

        procedure Perimeter()
            return 2 * 3.14 * this.ratio
        endproc
    enddefine
end namespace
