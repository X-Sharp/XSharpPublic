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
            RegisterFoxMemVarSupport()
        END CONSTRUCTOR

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

        [Fact, Trait("Category", "Tally")];
        METHOD ReplaceOnCurrentRecordSetsTallyToOne AS VOID
            TRY
                SELF:CreateTestCursor()
                GO TOP
                _TALLY := -1
                REPLACE f WITH 99
                Assert.Equal(1, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD ReplaceMultipleFieldsOnCurrentRecordSetsTallyToOne AS VOID
            TRY
                SELF:CreateTwoFieldCursor()
                GO TOP
                _TALLY := -1
                REPLACE f WITH 99, c WITH "zz"
                Assert.Equal(1, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD ReplaceAllSetsTally AS VOID
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                REPLACE ALL f WITH 99
                Assert.Equal(5, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD ReplaceForSetsTally AS VOID
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                REPLACE ALL f WITH 99 FOR curtally->f > 3
                Assert.Equal(2, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD ReplaceWithoutMatchesResetsTallyToZero AS VOID
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                REPLACE ALL f WITH 99 FOR curtally->f > 99
                Assert.Equal(0, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD ReplaceNextSetsTally AS VOID
            TRY
                SELF:CreateTestCursor()
                GO TOP
                _TALLY := -1
                REPLACE f WITH 99 NEXT 3
                Assert.Equal(3, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD ReplaceNextBeyondEofCountsOnlyTheRemainingRecords AS VOID
            TRY
                SELF:CreateTestCursor()
                GO 3
                _TALLY := -1
                REPLACE f WITH 99 NEXT 10
                Assert.Equal(3, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD ReplaceRestSetsTally AS VOID
            TRY
                SELF:CreateTestCursor()
                GO 4
                _TALLY := -1
                REPLACE f WITH 99 REST
                Assert.Equal(2, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD ReplaceInAliasSetsTallyToOne AS VOID
            TRY
                SELF:CreateTestCursor()
                SELF:CreateOtherCursor()
                _TALLY := -1
                REPLACE f WITH 99 IN curtally
                Assert.Equal(1, (INT) _TALLY)
            FINALLY
                USE IN curother
                USE IN curtally
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD DeleteOnCurrentRecordSetsTallyToOne AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                GO TOP
                _TALLY := -1
                DELETE
                Assert.Equal(1, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD DeleteOnAnAlreadyDeletedRecordSetsTallyToZero AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                GO TOP
                DELETE
                GO TOP
                _TALLY := -1
                DELETE
                Assert.Equal(0, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD DeleteAllSetsTally AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                _TALLY := -1
                DELETE ALL
                Assert.Equal(5, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD DeleteAllCountsAffectedRecordsNotVisitedOnes AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                GO 5
                DELETE
                GO TOP
                _TALLY := -1
                DELETE ALL
                // one record was already deleted: VFP reports 4, not 5
                Assert.Equal(4, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD DeleteForSetsTally AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                _TALLY := -1
                DELETE ALL FOR curtally->f > 3
                Assert.Equal(2, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD DeleteNextSetsTally AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                GO TOP
                _TALLY := -1
                DELETE NEXT 2
                Assert.Equal(2, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD RecallOnADeletedRecordSetsTallyToOne AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                GO 2
                DELETE
                GO 2
                _TALLY := -1
                RECALL
                Assert.Equal(1, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD RecallOnARecordThatIsNotDeletedSetsTallyToZero AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                GO 2
                _TALLY := -1
                RECALL
                Assert.Equal(0, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD RecallAllCountsOnlyTheDeletedRecords AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                GO 2
                DELETE
                _TALLY := -1
                RECALL ALL
                // 5 records visited, only 1 was deleted
                Assert.Equal(1, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD RecallAllWithNothingDeletedSetsTallyToZero AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                _TALLY := -1
                RECALL ALL
                Assert.Equal(0, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD RecallAllIgnoresSetDeletedOn AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                DELETE ALL FOR curtally->f > 3
                SetDeleted(TRUE)
                _TALLY := -1
                RECALL ALL
                // regression: RECALL has to see the deleted records even with
                // SET DELETED ON (Peter Stephan, RECALL + SET DELETED ON)
                Assert.Equal(2, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD RecallForSetsTally AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                DELETE ALL FOR curtally->f > 3
                _TALLY := -1
                RECALL ALL FOR curtally->f > 3
                Assert.Equal(2, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD RecallInAliasResolvesTheAlias AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                GO 2
                DELETE
                GO 2
                SELF:CreateOtherCursor()
                _TALLY := -1
                // regression: the rule emitted (a)-> and did not even compile
                RECALL IN curtally
                Assert.Equal(1, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE IN curother
                USE IN curtally
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD ContinueInAliasResolvesTheAlias AS VOID
            TRY
                SELF:CreateTestCursor()
                LOCATE FOR curtally->f > 3
                SELF:CreateOtherCursor()
                // regression: same (a)-> bug as RECALL IN
                CONTINUE IN curtally
                Assert.Equal(5, (INT) curtally->f)
            FINALLY
                USE IN curother
                USE IN curtally
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD SumSetsTally AS VOID
            LOCAL n AS USUAL
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                SUM curtally->f TO n
                Assert.Equal(5, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD SumWithForSetsTally AS VOID
            LOCAL n AS USUAL
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                SUM curtally->f TO n FOR curtally->f > 3
                Assert.Equal(2, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD AverageSetsTally AS VOID
            LOCAL n AS USUAL
            TRY
                SELF:CreateTestCursor()
                _TALLY := -1
                AVERAGE curtally->f TO n
                Assert.Equal(5, (INT) _TALLY)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD AverageComputesTheMean AS VOID
            LOCAL n AS USUAL
            TRY
                SELF:CreateTestCursor()
                AVERAGE curtally->f TO n
                // regression: the rule in dbcmd.xh never matched, so AVERAGE
                // did not even translate
                Assert.Equal(3, (INT) n)
            FINALLY
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD DeleteSqlSetsTally AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                _TALLY := -1
                DELETE FROM curtally WHERE curtally->f > 3
                Assert.Equal(2, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        [Fact, Trait("Category", "Tally")];
        METHOD DeleteSqlCountsMatchedRowsNotAffectedOnes AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SetDeleted(FALSE)
                SELF:CreateTestCursor()
                GO 5
                DELETE
                GO TOP
                _TALLY := -1
                DELETE FROM curtally WHERE curtally->f > 0
                // DELETE - SQL counts the rows matching the WHERE clause, so
                // the already deleted record still counts: 5, where the xBase
                // DELETE ALL FOR over the same records reports 4
                Assert.Equal(5, (INT) _TALLY)
            FINALLY
                SetDeleted(lOld)
                USE
            END TRY
        END METHOD

        PRIVATE METHOD CreateTestCursor() AS VOID
            CREATE CURSOR curtally (f I)
            INSERT INTO curtally VALUES (1)
            INSERT INTO curtally VALUES (2)
            INSERT INTO curtally VALUES (3)
            INSERT INTO curtally VALUES (4)
            INSERT INTO curtally VALUES (5)
            GO TOP
        END METHOD

        PRIVATE METHOD CreateTwoFieldCursor() AS VOID
            CREATE CURSOR curtally (f I, c C(10))
            INSERT INTO curtally VALUES(1, "a")
            INSERT INTO curtally VALUES(2, "b")
            INSERT INTO curtally VALUES(3, "c")
            INSERT INTO curtally VALUES(4, "d")
            INSERT INTO curtally VALUES(5, "e")
            GO TOP
        END METHOD

        PRIVATE METHOD CreateOtherCursor() AS VOID
            SELECT 0
            CREATE CURSOR curother (g I)
            INSERT INTO curother VALUES(1)
            GO TOP
        END METHOD

    END CLASS
END NAMESPACE
