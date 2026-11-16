//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// DELETE and RECALL with an IN clause and the other clauses in an order that
// is not the one of the rules in FoxProCmd.xh: scope before FOR, WHILE before
// FOR, NOOPTIMIZE after IN. They did not compile. The expected values were
// measured on VFP 9 SP2 with five records, the record pointer of the table on
// record 2 and another work area selected.

USING System
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests
    CLASS ScopeInAliasTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            RegisterFoxMemVarSupport()
        END CONSTRUCTOR

        [Fact, Trait("Category", "Scope")];
        METHOD DeleteAllForInAlias AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SELF:Setup(FALSE)
                _TALLY := -1
                DELETE ALL FOR curscope->x > 1 IN curscope
                Assert.Equal(4, (INT) _TALLY)
                Assert.Equal("CUROTHER", Upper(Alias()))
                Assert.Equal(".DDDD", SELF:Flags())
            FINALLY
                SELF:Cleanup(lOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "Scope")];
        METHOD DeleteNextForInAlias AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SELF:Setup(FALSE)
                _TALLY := -1
                DELETE NEXT 2 FOR curscope->x > 2 IN curscope
                Assert.Equal(1, (INT) _TALLY)
                Assert.Equal("..D..", SELF:Flags())
            FINALLY
                SELF:Cleanup(lOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "Scope")];
        METHOD DeleteRestForInAlias AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SELF:Setup(FALSE)
                _TALLY := -1
                DELETE REST FOR curscope->x > 3 IN curscope
                Assert.Equal(2, (INT) _TALLY)
                Assert.Equal("...DD", SELF:Flags())
            FINALLY
                SELF:Cleanup(lOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "Scope")];
        METHOD DeleteRestWhileInAlias AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SELF:Setup(FALSE)
                _TALLY := -1
                DELETE REST WHILE curscope->x < 4 IN curscope
                Assert.Equal(2, (INT) _TALLY)
                Assert.Equal(".DD..", SELF:Flags())
            FINALLY
                SELF:Cleanup(lOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "Scope")];
        METHOD DeleteWhileBeforeForInAlias AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SELF:Setup(FALSE)
                _TALLY := -1
                DELETE WHILE curscope->x < 4 FOR curscope->x > 2 IN curscope
                Assert.Equal(1, (INT) _TALLY)
                Assert.Equal("..D..", SELF:Flags())
            FINALLY
                SELF:Cleanup(lOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "Scope")];
        METHOD DeleteAllForInAliasFromAVariable AS VOID
            LOCAL lOld AS LOGIC
            LOCAL cAlias := "curscope" AS STRING
            TRY
                lOld := SELF:Setup(FALSE)
                _TALLY := -1
                DELETE ALL FOR curscope->x > 1 IN (cAlias)
                Assert.Equal(4, (INT) _TALLY)
                Assert.Equal(".DDDD", SELF:Flags())
            FINALLY
                SELF:Cleanup(lOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "Scope")];
        METHOD DeleteAllForInAliasNoOptimize AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SELF:Setup(FALSE)
                _TALLY := -1
                DELETE ALL FOR curscope->x > 1 IN curscope NOOPTIMIZE
                Assert.Equal(4, (INT) _TALLY)
                Assert.Equal(".DDDD", SELF:Flags())
            FINALLY
                SELF:Cleanup(lOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "Scope")];
        METHOD RecallAllForInAlias AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SELF:Setup(TRUE)
                _TALLY := -1
                RECALL ALL FOR curscope->x > 1 IN curscope
                Assert.Equal(4, (INT) _TALLY)
                Assert.Equal("CUROTHER", Upper(Alias()))
                Assert.Equal("D....", SELF:Flags())
            FINALLY
                SELF:Cleanup(lOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "Scope")];
        METHOD RecallNextForInAlias AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SELF:Setup(TRUE)
                _TALLY := -1
                RECALL NEXT 2 FOR curscope->x > 2 IN curscope
                Assert.Equal(1, (INT) _TALLY)
                Assert.Equal("DD.DD", SELF:Flags())
            FINALLY
                SELF:Cleanup(lOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "Scope")];
        METHOD RecallRestWhileInAlias AS VOID
            LOCAL lOld AS LOGIC
            TRY
                lOld := SELF:Setup(TRUE)
                _TALLY := -1
                RECALL REST WHILE curscope->x < 4 IN curscope
                Assert.Equal(2, (INT) _TALLY)
                Assert.Equal("D..DD", SELF:Flags())
            FINALLY
                SELF:Cleanup(lOld)
            END TRY
        END METHOD

        // five records in curscope with the pointer on record 2, and curother selected
        PRIVATE METHOD Setup(lAllDeleted AS LOGIC) AS LOGIC
            LOCAL lOld AS LOGIC
            lOld := SetDeleted(FALSE)
            CREATE CURSOR curscope (x I)
            INSERT INTO curscope VALUES (1)
            INSERT INTO curscope VALUES (2)
            INSERT INTO curscope VALUES (3)
            INSERT INTO curscope VALUES (4)
            INSERT INTO curscope VALUES (5)
            IF lAllDeleted
                DELETE ALL
            ENDIF
            GO 2
            SELECT 0
            CREATE CURSOR curother (g I)
            INSERT INTO curother VALUES (1)
            GO TOP
            RETURN lOld
        END METHOD

        PRIVATE METHOD Cleanup(lOld AS LOGIC) AS VOID
            SetDeleted(lOld)
            USE IN curother
            USE IN curscope
        END METHOD

        // one character per record of curscope: D deleted, . not deleted
        PRIVATE METHOD Flags() AS STRING
            LOCAL cResult := "" AS STRING
            SELECT curscope
            GO TOP
            DO WHILE ! Eof()
                cResult += IIF(Deleted(), "D", ".")
                SKIP
            ENDDO
            SELECT curother
            RETURN cResult
        END METHOD

    END CLASS
END NAMESPACE
