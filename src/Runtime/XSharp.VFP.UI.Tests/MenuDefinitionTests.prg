//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// The menu functions against menus built with the DEFINE commands.
// Measured on VFP 9 SP2: PRMBAR() and MRKBAR() take the bar number given in
// DEFINE BAR, not its position, a separator has an empty prompt, the prompt
// comes back without the \< hotkey marker, and menu, pad and popup names
// are not case sensitive.

#include "VFPMenuSupport.xh"

USING System
USING XUnit
USING XSharp.VFP.UI

BEGIN NAMESPACE XSharp.VFP.UI.Tests
    CLASS MenuDefinitionTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        // what the ON SELECTION handler saw, read back by the selection test
        STATIC PROPERTY Seen AS STRING AUTO := ""

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD PrmBarTakesTheBarNumberNotThePosition AS VOID
            DEFINE POPUP mdt_pop1
            DEFINE BAR 1 OF mdt_pop1 PROMPT "\<Open"
            DEFINE BAR 2 OF mdt_pop1 PROMPT "\-"
            DEFINE BAR 5 OF mdt_pop1 PROMPT "Exit"
            TRY
                Assert.Equal("Open", PRMBAR("mdt_pop1", 1))
                Assert.Equal("", PRMBAR("mdt_pop1", 2))
                Assert.Equal("Exit", PRMBAR("mdt_pop1", 5))
                Assert.Equal("", PRMBAR("mdt_pop1", 3))
            FINALLY
                RELEASE POPUPS mdt_pop1
            END TRY
            Assert.Null(Popup.Find("mdt_pop1"))
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD PopupNamesAreNotCaseSensitive AS VOID
            DEFINE POPUP mdt_pop2
            DEFINE BAR 1 OF mdt_pop2 PROMPT "Open"
            TRY
                Assert.Equal("Open", PRMBAR("MDT_POP2", 1))
            FINALLY
                RELEASE POPUPS mdt_pop2
            END TRY
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD MrkBarFollowsTheCheckedStateByBarNumber AS VOID
            DEFINE POPUP mdt_pop3
            DEFINE BAR 1 OF mdt_pop3 PROMPT "Open"
            DEFINE BAR 2 OF mdt_pop3 PROMPT "\-"
            DEFINE BAR 3 OF mdt_pop3 PROMPT "Exit"
            TRY
                // bar 3 is the second Bar object, the separator is not one
                Popup.Find("mdt_pop3"):Bars[2]:Checked := TRUE
                Assert.True(MRKBAR("mdt_pop3", 3))
                Assert.False(MRKBAR("mdt_pop3", 1))
                Assert.False(MRKBAR("mdt_pop3", 2))
            FINALLY
                RELEASE POPUPS mdt_pop3
            END TRY
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD PrmPadReturnsTheCaptionWithoutTheHotkeyMarker AS VOID
            DEFINE MENU mdt_m1 BAR
            DEFINE PAD mdt_p1 OF mdt_m1 PROMPT "\<File"
            DEFINE PAD mdt_p2 OF mdt_m1 PROMPT "Edit"
            TRY
                Assert.Equal("File", PRMPAD("mdt_m1", "mdt_p1"))
                Assert.Equal("Edit", PRMPAD("MDT_M1", "MDT_P2"))
                Assert.Equal("", PRMPAD("mdt_m1", "nosuchpad"))
            FINALLY
                RELEASE MENUS mdt_m1
            END TRY
            Assert.Null(Menu.Find("mdt_m1"))
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD MrkPadFollowsTheCheckedState AS VOID
            DEFINE MENU mdt_m2 BAR
            DEFINE PAD mdt_p1 OF mdt_m2 PROMPT "File"
            DEFINE PAD mdt_p2 OF mdt_m2 PROMPT "Edit"
            TRY
                Menu.Find("mdt_m2"):Pads[2]:Checked := TRUE
                Assert.True(MRKPAD("mdt_m2", "mdt_p2"))
                Assert.False(MRKPAD("mdt_m2", "mdt_p1"))
            FINALLY
                RELEASE MENUS mdt_m2
            END TRY
        END METHOD

        // CNTBAR() is the highest bar number, not how many bars were defined
        [Fact, Trait("Category", "MenuFunctions")];
        METHOD CntBarReturnsTheHighestBarNumber AS VOID
            DEFINE POPUP mdt_pop5
            DEFINE BAR 1 OF mdt_pop5 PROMPT "\<Open"
            DEFINE BAR 2 OF mdt_pop5 PROMPT "\-"
            DEFINE BAR 5 OF mdt_pop5 PROMPT "Exit"
            DEFINE POPUP mdt_pop6
            DEFINE BAR 10 OF mdt_pop6 PROMPT "Ten"
            DEFINE BAR 20 OF mdt_pop6 PROMPT "Twenty"
            TRY
                Assert.Equal(5, (INT) CNTBAR("mdt_pop5"))
                Assert.Equal(5, (INT) CNTBAR("MDT_POP5"))
                Assert.Equal(20, (INT) CNTBAR("mdt_pop6"))
            FINALLY
                RELEASE POPUPS mdt_pop5, mdt_pop6
            END TRY
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD CntBarCountsASeparatorAtTheEnd AS VOID
            DEFINE POPUP mdt_pop7
            DEFINE BAR 1 OF mdt_pop7 PROMPT "Open"
            DEFINE BAR 2 OF mdt_pop7 PROMPT "Save"
            DEFINE BAR 3 OF mdt_pop7 PROMPT "\-"
            TRY
                Assert.Equal(3, (INT) CNTBAR("mdt_pop7"))
            FINALLY
                RELEASE POPUPS mdt_pop7
            END TRY
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD CntBarIsZeroForAnEmptyOrUnknownPopup AS VOID
            DEFINE POPUP mdt_pop8
            TRY
                Assert.Equal(0, (INT) CNTBAR("mdt_pop8"))
                Assert.Equal(0, (INT) CNTBAR("nosuchpopup"))
            FINALLY
                RELEASE POPUPS mdt_pop8
            END TRY
        END METHOD

        [Fact, Trait("Category", "MenuFunctions")];
        METHOD CntPadReturnsTheNumberOfPads AS VOID
            DEFINE MENU mdt_m4 BAR
            TRY
                Assert.Equal(0, (INT) CNTPAD("mdt_m4"))
                DEFINE PAD mdt_p1 OF mdt_m4 PROMPT "\<File"
                DEFINE PAD mdt_p2 OF mdt_m4 PROMPT "Edit"
                Assert.Equal(2, (INT) CNTPAD("mdt_m4"))
                Assert.Equal(2, (INT) CNTPAD("MDT_M4"))
                Assert.Equal(0, (INT) CNTPAD("nosuchmenu"))
            FINALLY
                RELEASE MENUS mdt_m4
            END TRY
        END METHOD

        // BAR(), POPUP(), PAD() and MENU() describe the last selection, inside the
        // handler and after it. BAR() is the bar number from DEFINE BAR.
        [Fact, Trait("Category", "MenuFunctions")];
        METHOD SelectingABarFillsBarPopupPadAndMenu AS VOID
            DEFINE MENU mdt_m3 BAR
            DEFINE PAD mdt_p1 OF mdt_m3 PROMPT "File"
            DEFINE POPUP mdt_pop4
            DEFINE BAR 1 OF mdt_pop4 PROMPT "Open"
            DEFINE BAR 2 OF mdt_pop4 PROMPT "\-"
            DEFINE BAR 3 OF mdt_pop4 PROMPT "Exit"
            ON PAD mdt_p1 OF mdt_m3 ACTIVATE POPUP mdt_pop4
            ON SELECTION BAR 3 OF mdt_pop4 MenuDefinitionTests.Seen := TRANSFORM(BAR()) + "/" + POPUP() + "/" + PAD() + "/" + MENU()
            TRY
                MenuDefinitionTests.Seen := ""
                Popup.Find("mdt_pop4"):Bars[2]:PerformClick()
                Assert.Equal("3/mdt_pop4/mdt_p1/mdt_m3", MenuDefinitionTests.Seen)
                Assert.Equal(3, (INT) BAR())
                Assert.Equal("mdt_pop4", POPUP())
                Assert.Equal("mdt_p1", PAD())
                Assert.Equal("mdt_m3", MENU())
            FINALLY
                RELEASE POPUPS mdt_pop4
                RELEASE MENUS mdt_m3
            END TRY
        END METHOD

    END CLASS
END NAMESPACE
