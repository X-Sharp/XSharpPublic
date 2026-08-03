//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS InClauseAliasTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        // Two cursors, so the IN clause has to act on the one that is NOT selected
        PRIVATE METHOD CreateCursors() AS VOID
            CREATE CURSOR aliasone (id I)
            INSERT INTO aliasone VALUES (1)
            INSERT INTO aliasone VALUES (2)
            INSERT INTO aliasone VALUES (3)
            GO TOP
            CREATE CURSOR aliastwo (id I)
            INSERT INTO aliastwo VALUES (9)
            GO TOP
        END METHOD

        [Fact, Trait("Category", "InClause")];
        METHOD SkipInAcceptsAnUnquotedAlias AS VOID
            SELF:CreateCursors()
            SELECT aliastwo
            SKIP 1 IN aliasone
            SELECT aliasone
            Assert.Equal((DWORD) 2, RECNO())
        END METHOD

        [Fact, Trait("Category", "InClause")];
        METHOD SkipWithoutCountInAcceptsAnUnquotedAlias AS VOID
            SELF:CreateCursors()
            SELECT aliastwo
            SKIP IN aliasone
            SELECT aliasone
            Assert.Equal((DWORD) 2, RECNO())
        END METHOD

        [Fact, Trait("Category", "InClause")];
        METHOD GotoBottomInAcceptsAnUnquotedAlias AS VOID
            SELF:CreateCursors()
            SELECT aliastwo
            GOTO BOTTOM IN aliasone
            SELECT aliasone
            Assert.Equal((DWORD) 3, RECNO())
        END METHOD

        [Fact, Trait("Category", "InClause")];
        METHOD GotoTopInAcceptsAnUnquotedAlias AS VOID
            SELF:CreateCursors()
            SELECT aliasone
            GO BOTTOM
            SELECT aliastwo
            GOTO TOP IN aliasone
            SELECT aliasone
            Assert.Equal((DWORD) 1, RECNO())
        END METHOD

        [Fact, Trait("Category", "InClause")];
        METHOD GotoRecordInAcceptsAnUnquotedAlias AS VOID
            SELF:CreateCursors()
            SELECT aliastwo
            GOTO 3 IN aliasone
            SELECT aliasone
            Assert.Equal((DWORD) 3, RECNO())
        END METHOD

        [Fact, Trait("Category", "InClause")];
        METHOD QuotedAliasKeepsWorking AS VOID
            SELF:CreateCursors()
            SELECT aliastwo
            SKIP 1 IN "aliasone"
            SELECT aliasone
            Assert.Equal((DWORD) 2, RECNO())
        END METHOD

        // The IN clause must not change the selected work area
        [Fact, Trait("Category", "InClause")];
        METHOD InClauseDoesNotChangeTheSelectedArea AS VOID
            SELF:CreateCursors()
            SELECT aliastwo
            VAR nBefore := SELECT()
            SKIP 1 IN aliasone
            Assert.Equal(nBefore, SELECT())
        END METHOD

    END CLASS

END NAMESPACE
