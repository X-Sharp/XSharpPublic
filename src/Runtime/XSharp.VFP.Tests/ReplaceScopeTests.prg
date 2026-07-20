//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Text
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS ReplaceScopeTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        PRIVATE METHOD CreateTestCursor() AS VOID
            LOCAL i AS LONG
            CREATE CURSOR curRepl (id N(4), grupo C(1), nombre C(20), notas M)
            FOR i := 1 TO 10
                INSERT INTO curRepl VALUES (i, IIF(i % 2 == 0, "P", "I"), "ORIG", "memo")
            NEXT
            GO TOP
        END METHOD

        PRIVATE METHOD CountName(cValue AS STRING) AS LONG
            FIELD nombre
            VAR nCount := 0
            VAR nOldRec := RECNO()
            COUNT FOR ALLTRIM(nombre) == cValue TO nCount
            IF nOldRec <= RECCOUNT()
                GO nOldRec
            ENDIF
            RETURN nCount
        END METHOD

        PRIVATE METHOD CountMemo(cValue AS STRING) AS LONG
            FIELD notas
            LOCAL nCount := 0 AS LONG
            VAR nOldRec := RECNO()
            COUNT FOR ALLTRIM(notas) == cValue TO nCount
            IF nOldRec <= RECCOUNT()
                GO nOldRec
            ENDIF
            RETURN nCount
        END METHOD

        // --- REPLACE without scope: current record only (VFP default) ---

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceNoScopeUpdatesOnlyCurrentRecord AS VOID
            FIELD nombre
            SELF:CreateTestCursor()
            GO 3
            REPLACE nombre WITH "X1"
            Assert.Equal(1, SELF:CountName("X1"))
            Assert.Equal((DWORD)3, RECNO())
            Assert.Equal("X1", ALLTRIM(nombre))
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceNoScopeMultiFieldUpdatesAllFields AS VOID
            FIELD nombre, grupo
            SELF:CreateTestCursor()
            GO 5
            REPLACE nombre WITH "X2", grupo WITH "Z"
            Assert.Equal(1, SELF:CountName("X2"))
            GO 5
            Assert.Equal("Z", grupo)
            GO 6
            Assert.Equal("ORIG", ALLTRIM(nombre))
        END METHOD

        // --- Explicit scopes ---

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceAllUpdatesAllRecords AS VOID
            SELF:CreateTestCursor()
            GO 3
            REPLACE nombre WITH "X3" ALL
            Assert.Equal(10, SELF:CountName("X3"))
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceAllPrefixUpdatesAllRecords AS VOID
            SELF:CreateTestCursor()
            REPLACE ALL nombre WITH "X4"
            Assert.Equal(10, SELF:CountName("X4"))
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceAllMultiFieldUpdatesAllFields AS VOID
            // regression: the second field was silently dropped by the UDC
            FIELD grupo
            SELF:CreateTestCursor()
            LOCAL nCount := 0 AS LONG
            REPLACE ALL nombre WITH "X5", grupo WITH "W"
            Assert.Equal(10, SELF:CountName("X5"))
            COUNT FOR grupo == "W" TO nCount
            Assert.Equal(10, nCount)
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceForUpdatesMatchingRecords AS VOID
            FIELD grupo
            SELF:CreateTestCursor()
            REPLACE nombre WITH "X6" FOR grupo == "P"
            Assert.Equal(5, SELF:CountName("X6"))
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceNextUpdatesScopedRecords AS VOID
            FIELD nombre
            SELF:CreateTestCursor()
            GO 4
            REPLACE nombre WITH "X7" NEXT 3
            Assert.Equal(3, SELF:CountName("X7"))
            GO 4
            Assert.Equal("X7", ALLTRIM(nombre))
            GO 7
            Assert.Equal("ORIG", ALLTRIM(nombre))
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceRestUpdatesFromCurrentRecord AS VOID
            SELF:CreateTestCursor()
            GO 8
            REPLACE nombre WITH "X8" REST
            Assert.Equal(3, SELF:CountName("X8"))
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceRecordUpdatesSpecificRecord AS VOID
            FIELD nombre
            SELF:CreateTestCursor()
            GO 1
            REPLACE nombre WITH "X9" RECORD 7
            Assert.Equal(1, SELF:CountName("X9"))
            GO 7
            Assert.Equal("X9", ALLTRIM(nombre))
        END METHOD

        // --- ADDITIVE ---

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceAdditiveAppendsToMemoCurrentRecordOnly AS VOID
            FIELD notas
            SELF:CreateTestCursor()
            GO 2
            REPLACE notas WITH "+X" ADDITIVE
            Assert.Equal("memo+X", ALLTRIM(notas))
            Assert.Equal(1, SELF:CountMemo("memo+X"))
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceAdditiveOnCharFieldIsIgnored AS VOID
            // per VFP spec ADDITIVE only applies to memo fields
            FIELD nombre
            SELF:CreateTestCursor()
            GO 2
            REPLACE nombre WITH "X10" ADDITIVE
            Assert.Equal("X10", ALLTRIM(nombre))
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceAdditiveWithScopeAppendsToMatchingRecords AS VOID
            FIELD grupo
            SELF:CreateTestCursor()
            REPLACE notas WITH "+A" ADDITIVE FOR grupo == "P"
            Assert.Equal(5, SELF:CountMemo("memo+A"))
        END METHOD

        // --- IN workarea ---

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceInUpdatesOnlyCurrentRecordOfAlias AS VOID
            FIELD nombre
            SELF:CreateTestCursor()
            GO 3
            CREATE CURSOR curCtx (dummy C(1))
            INSERT INTO curCtx VALUES ("x")
            REPLACE nombre WITH "X11" IN curRepl
            Assert.Equal("CURCTX", ALIAS())
            SELECT curRepl
            Assert.Equal((DWORD)3, RECNO())
            Assert.Equal(1, SELF:CountName("X11"))
            Assert.Equal("X11", ALLTRIM(nombre))
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceInMultiFieldUpdatesAllFields AS VOID
            FIELD grupo
            SELF:CreateTestCursor()
            GO 4
            CREATE CURSOR curCtx (dummy C(1))
            INSERT INTO curCtx VALUES ("x")
            REPLACE nombre WITH "X12", grupo WITH "Z" IN curRepl
            SELECT curRepl
            Assert.Equal(1, SELF:CountName("X12"))
            GO 4
            Assert.Equal("Z", grupo)
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceAdditiveInAppendsToMemoOfAlias AS VOID
            FIELD notas
            SELF:CreateTestCursor()
            GO 3
            CREATE CURSOR curCtx (dummy C(1))
            INSERT INTO curCtx VALUES ("x")
            REPLACE notas WITH "+Y" ADDITIVE IN curRepl
            SELECT curRepl
            Assert.Equal("memo+Y", ALLTRIM(notas))
            Assert.Equal(1, SELF:CountMemo("memo+Y"))
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceAllInUpdatesAllRecordsOfAlias AS VOID
            SELF:CreateTestCursor()
            CREATE CURSOR curCtx (dummy C(1))
            INSERT INTO curCtx VALUES ("x")
            REPLACE nombre WITH "X13" ALL IN curRepl
            SELECT curRepl
            Assert.Equal(10, SELF:CountName("X13"))
        END METHOD

        [Fact, Trait("Category", "Replace")];
        METHOD ReplaceForInUpdatesMatchingRecordsOfAlias AS VOID
            FIELD grupo
            SELF:CreateTestCursor()
            CREATE CURSOR curCtx (dummy C(1))
            INSERT INTO curCtx VALUES ("x")
            REPLACE nombre WITH "X14" FOR grupo == "P" IN curRepl
            SELECT curRepl
            Assert.Equal(5, SELF:CountName("X14"))
        END METHOD

    END CLASS

END NAMESPACE
