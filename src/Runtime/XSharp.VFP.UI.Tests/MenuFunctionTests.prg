//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING XUnit

BEGIN NAMESPACE XSharp.VFP.UI.Tests
    CLASS MenuFunctionTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        // BAR(), PAD(), POPUP() and MENU() read the last selection from MenuState.
        // With no menu activated they must return the empty value, not NIL.

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD BarReturnsZeroWhenNothingWasSelected AS VOID
            Assert.Equal(0, (INT) BAR())
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD PadReturnsEmptyWhenNothingWasSelected AS VOID
            Assert.Equal("", PAD())
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD PopupReturnsEmptyWhenNothingWasSelected AS VOID
            Assert.Equal("", POPUP())
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD MenuReturnsEmptyWhenNothingWasSelected AS VOID
            Assert.Equal("", MENU())
        END METHOD

        // The four lookup functions take a name. An unknown name is not an error,
        // they return the empty value.

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD MrkBarReturnsFalseForAnUnknownPopup AS VOID
            Assert.False(MRKBAR("nosuchpopup", 1))
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD MrkPadReturnsFalseForAnUnknownMenu AS VOID
            Assert.False(MRKPAD("nosuchmenu", "nosuchpad"))
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD PrmBarReturnsEmptyForAnUnknownPopup AS VOID
            Assert.Equal("", PRMBAR("nosuchpopup", 1))
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD PrmPadReturnsEmptyForAnUnknownMenu AS VOID
            Assert.Equal("", PRMPAD("nosuchmenu", "nosuchpad"))
        END METHOD

    END CLASS
END NAMESPACE
