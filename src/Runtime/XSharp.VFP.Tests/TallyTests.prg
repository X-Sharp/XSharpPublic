//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests
    CLASS TallyTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        PRIVATE METHOD CreateTestCursor() AS VOID
            CREATE CURSOR curtally (f I)
            INSERT INTO curtally VALUES (1)
            INSERT INTO curtally VALUES (2)
            INSERT INTO curtally VALUES (3)
            INSERT INTO curtally VALUES (4)
            INSERT INTO curtally VALUES (5)
            GO TOP
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD CountWithoutToClauseSetsTally AS VOID
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                COUNT
                Assert.Equal(5, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD CountWithForSetsTally AS VOID
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                COUNT FOR curtally->f > 2
                Assert.Equal(3, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD CountToVariableSetsBothTheVariableAndTally AS VOID
            LOCAL n AS USUAL
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                COUNT TO n
                Assert.Equal(5, (INT) n)
                Assert.Equal(5, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD CountAcceptsToBeforeFor AS VOID
            LOCAL n AS USUAL
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                COUNT TO n FOR curtally->f > 3
                Assert.Equal(2, (INT) n)
                Assert.Equal(2, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD CountAcceptsToAfterFor AS VOID
            LOCAL n AS USUAL
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                COUNT FOR curtally->f > 3 TO n
                Assert.Equal(2, (INT) n)
                Assert.Equal(2, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD CountAcceptsScopeBeforeFor AS VOID
            LOCAL n AS USUAL
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                COUNT ALL FOR curtally->f > 2 TO n NOOPTIMIZE
                Assert.Equal(3, (INT) n)
                Assert.Equal(3, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD CountNextLimitsTheScope AS VOID
            LOCAL n AS USUAL
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                COUNT NEXT 3 TO n
                Assert.Equal(3, (INT) n)
                Assert.Equal(3, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD NavigationDoesNotChangeTally AS VOID
            TRY
                SELF:CreateTestCursor()
                COUNT
                _TALLY := 99
                GO TOP
                Assert.Equal(99, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

    END CLASS
END NAMESPACE
