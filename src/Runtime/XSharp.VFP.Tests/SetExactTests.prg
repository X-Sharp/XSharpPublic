//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    // Measured on VFP 9 SP2: with SET EXACT ON the shorter string is padded with blanks,
    // so trailing blanks do not count for = and <>, but == still compares them (issue 2157).
    // One side of every comparison is a variable: two literals are folded by the compiler.
    CLASS SetExactTests
        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        [Fact, Trait("Category", "String")];
        METHOD ExactOnIgnoresTrailingBlanksOnBothSides() AS VOID
            LOCAL cL3, cEmpty AS STRING
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Exact
            cL3 := "L3 "
            cEmpty := ""
            TRY
                RuntimeState.Exact := TRUE
                Assert.True(cL3 = "L3")
                Assert.True("L3" = cL3)
                Assert.True(cEmpty = "   ")
                Assert.True("   " = cEmpty)
                // <> and # on typed strings do not go through the runtime, see the usual test
            FINALLY
                RuntimeState.Exact := lOld
            END TRY
        END METHOD

        [Fact, Trait("Category", "String")];
        METHOD ExactOnStillComparesTheRest() AS VOID
            LOCAL cL3, cAbc AS STRING
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Exact
            cL3 := "L3"
            cAbc := "abc"
            TRY
                RuntimeState.Exact := TRUE
                Assert.False(cAbc = "ab")
                Assert.False(cL3 = "L3 x")
                Assert.False(cL3 = " L3")
                // only blanks are padding, a Chr(0) or a tab is a character
                Assert.False(cL3 = "L3" + Chr(0))
                Assert.False(cL3 = "L3" + Chr(9))
            FINALLY
                RuntimeState.Exact := lOld
            END TRY
        END METHOD

        [Fact, Trait("Category", "String")];
        METHOD ExactOnDoesNotChangeTheExactOperator() AS VOID
            LOCAL cL3 AS STRING
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Exact
            cL3 := "L3  "
            TRY
                RuntimeState.Exact := TRUE
                Assert.False(cL3 == "L3")
                Assert.True(cL3 == "L3  ")
            FINALLY
                RuntimeState.Exact := lOld
            END TRY
        END METHOD

        // the usual path goes through the same comparison
        [Fact, Trait("Category", "String")];
        METHOD ExactOnWorksWithUsuals() AS VOID
            LOCAL uLeft, uRight AS USUAL
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Exact
            uLeft := "L3 "
            uRight := "L3"
            TRY
                RuntimeState.Exact := TRUE
                Assert.True(uLeft = uRight)
                Assert.True(uRight = uLeft)
                Assert.False(uLeft <> uRight)
            FINALLY
                RuntimeState.Exact := lOld
            END TRY
        END METHOD

        [Fact, Trait("Category", "String")];
        METHOD ExactOffIsUnchanged() AS VOID
            LOCAL cL3, cAbc AS STRING
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Exact
            cL3 := "L3 "
            cAbc := "abc"
            TRY
                RuntimeState.Exact := FALSE
                Assert.True(cL3 = "L3")
                Assert.False("L3" = cL3)
                Assert.True(cAbc = "ab")
            FINALLY
                RuntimeState.Exact := lOld
            END TRY
        END METHOD

    END CLASS

END NAMESPACE
