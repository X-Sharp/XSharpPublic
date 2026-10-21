//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests
    CLASS WaitCommandTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        [Fact, Trait("Category", "Wait")];
        METHOD WaitWindowAcceptsTheMessageAfterTheWindowKeyword AS VOID
            WAIT WINDOW "x" NOWAIT
            WAIT WINDOW "x" AT 10, 20 NOWAIT
            WAIT WINDOW "x" NOWAIT NOCLEAR
        END METHOD

        [Fact, Trait("Category", "Wait")];
        METHOD WaitWindowWithMessageStoresTheResult AS VOID
            LOCAL cKey AS STRING
            cKey := "unset"
            WAIT WINDOW "x" TO cKey NOWAIT
            Assert.Equal("", cKey)
        END METHOD

        [Fact, Trait("Category", "Wait")];
        METHOD WaitWindowAcceptsAtBeforeTo AS VOID
            LOCAL cKey AS STRING
            cKey := "unset"
            WAIT WINDOW "x" AT 10, 20 TO cKey NOWAIT
            Assert.Equal("", cKey)
        END METHOD

        [Fact, Trait("Category", "Wait")];
        METHOD WaitWindowAcceptsToBeforeAt AS VOID
            LOCAL cKey AS STRING
            cKey := "unset"
            WAIT WINDOW "x" TO cKey AT 10, 20 NOWAIT
            Assert.Equal("", cKey)
        END METHOD

        [Fact, Trait("Category", "Wait")];
        METHOD WaitWindowWithoutMessageStoresTheResult AS VOID
            LOCAL cKey AS STRING
            cKey := "unset"
            WAIT WINDOW TO cKey NOWAIT
            Assert.Equal("", cKey)
        END METHOD

        [Fact, Trait("Category", "Wait")];
        METHOD WaitStillAcceptsTheMessageBeforeTheWindowKeyword AS VOID
            LOCAL cKey AS STRING
            cKey := "unset"
            WAIT "x" WINDOW NOWAIT
            WAIT "x" TO cKey WINDOW AT 3, 3 NOWAIT
            Assert.Equal("", cKey)
        END METHOD

        [Fact, Trait("Category", "Wait")];
        METHOD WaitWithoutMessageStillWorks AS VOID
            WAIT WINDOW NOWAIT
            WAIT WINDOW AT 5, 5 NOWAIT
            WAIT CLEAR
        END METHOD

    END CLASS
END NAMESPACE
