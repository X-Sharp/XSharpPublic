//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.IO
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS UdcCommandFixTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        PRIVATE METHOD TempFile() AS STRING
            RETURN Path.Combine(Path.GetTempPath(), "CopyMemo_" + Guid.NewGuid():ToString("N") + ".txt")
        END METHOD

        PRIVATE METHOD CreateMemoCursor() AS VOID
            CREATE CURSOR curmemo (m1 M)
            APPEND BLANK
            REPLACE m1 WITH "memo content"
        END METHOD

        // --- SET SECONDS (#2026) ---

        [Fact, Trait("Category", "SetCommands")];
        METHOD SetSecondsOnOffUpdatesTheSetting AS VOID
            VAR lOld := SetSeconds()
            TRY
                SET SECONDS ON
                Assert.True(SetSeconds())
                SET SECONDS OFF
                Assert.False(SetSeconds())
            FINALLY
                SetSeconds(lOld)
            END TRY
        END METHOD

        // --- COPY MEMO (#2022) ---

        [Fact, Trait("Category", "CopyMemo")];
        METHOD CopyMemoWorksWithoutAdditive AS VOID
            SELF:CreateMemoCursor()
            VAR cFile := SELF:TempFile()
            COPY MEMO m1 TO (cFile)
            Assert.Equal("memo content", File.ReadAllText(cFile))
            File.Delete(cFile)
        END METHOD

        [Fact, Trait("Category", "CopyMemo")];
        METHOD CopyMemoAppendsWithAdditive AS VOID
            SELF:CreateMemoCursor()
            VAR cFile := SELF:TempFile()
            COPY MEMO m1 TO (cFile)
            COPY MEMO m1 TO (cFile) ADDITIVE
            Assert.Equal("memo contentmemo content", File.ReadAllText(cFile))
            File.Delete(cFile)
        END METHOD

        [Fact, Trait("Category", "CopyMemo")];
        METHOD CopyMemoAcceptsTheAsCodePageClause AS VOID
            SELF:CreateMemoCursor()
            VAR cFile := SELF:TempFile()
            COPY MEMO m1 TO (cFile) AS 1252
            Assert.True(File.Exists(cFile))
            File.Delete(cFile)
            VAR cFile2 := SELF:TempFile()
            COPY MEMO m1 TO (cFile2) ADDITIVE AS 1252
            Assert.True(File.Exists(cFile2))
            File.Delete(cFile2)
        END METHOD

    END CLASS

END NAMESPACE
