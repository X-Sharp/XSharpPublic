//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.IO
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests
    CLASS ATagInfoTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        // Every test gets its own folder with a fresh TagTest table
        PRIVATE cOldDir   AS STRING
        PRIVATE cTempPath AS STRING

        PRIVATE METHOD Setup() AS VOID
            cOldDir := Directory.GetCurrentDirectory()
            cTempPath := Directory.CreateDirectory(Path.Combine(Path.GetTempPath(), ;
                "ATagInfoTest_" + Guid.NewGuid():ToString("N"))):FullName
            SET DEFAULT TO (cTempPath)
            CREATE TABLE TagTest (Id INT, Name C(10))
            INSERT INTO TagTest VALUES (1, "Alpha")
            INSERT INTO TagTest VALUES (2, "Beta")
            INSERT INTO TagTest VALUES (3, "Gamma")
        END METHOD

        PRIVATE METHOD Teardown() AS VOID
            XSharp.CoreDb.CloseAll()
            SET DEFAULT TO (cOldDir)
            Directory.SetCurrentDirectory(cOldDir)
            TRY ; Directory.Delete(cTempPath, TRUE) ; CATCH ; NOP; END TRY
        END METHOD

        [Fact, Trait("Category", "ATagInfo")];
        METHOD RegularTagFillsTheSixColumns AS VOID
            LOCAL ARRAY aTags[1]
            TRY
                SELF:Setup()
                INDEX ON Id TAG IdIdx
                Assert.Equal(1, ATagInfo(aTags))
                Assert.Equal(1, (INT) ALen(aTags, 1))
                Assert.Equal(6, (INT) ALen(aTags, 2))
                Assert.Equal("IDIDX",     (STRING) aTags[1, 1])
                Assert.Equal("REGULAR",   (STRING) aTags[1, 2])
                Assert.Equal("ID",        (STRING) aTags[1, 3])
                Assert.Equal("",          (STRING) aTags[1, 4])
                Assert.Equal("ASCENDING", (STRING) aTags[1, 5])
                Assert.Equal("MACHINE",   (STRING) aTags[1, 6])
            FINALLY
                SELF:Teardown()
            END TRY
        END METHOD

        [Fact, Trait("Category", "ATagInfo")];
        METHOD UniqueTagIsReportedAsUnique AS VOID
            LOCAL ARRAY aTags[1]
            TRY
                SELF:Setup()
                INDEX ON Name TAG NmIdx UNIQUE
                Assert.Equal(1, ATagInfo(aTags))
                Assert.Equal("UNIQUE", (STRING) aTags[1, 2])
            FINALLY
                SELF:Teardown()
            END TRY
        END METHOD

        [Fact, Trait("Category", "ATagInfo")];
        METHOD DescendingTagIsReportedInColumnFive AS VOID
            LOCAL ARRAY aTags[1]
            TRY
                SELF:Setup()
                INDEX ON Id TAG IdIdx DESCENDING
                Assert.Equal(1, ATagInfo(aTags))
                Assert.Equal("DESCENDING", (STRING) aTags[1, 5])
            FINALLY
                SELF:Teardown()
            END TRY
        END METHOD

        [Fact, Trait("Category", "ATagInfo")];
        METHOD ForClauseIsReportedInColumnFour AS VOID
            LOCAL ARRAY aTags[1]
            TRY
                SELF:Setup()
                INDEX ON Name TAG NmIdx FOR Id>1
                Assert.Equal(1, ATagInfo(aTags))
                // VFP returns the filter as stored; only the spacing may differ
                Assert.Equal("ID>1", Upper(StrTran((STRING) aTags[1, 4], " ", "")))
            FINALLY
                SELF:Teardown()
            END TRY
        END METHOD

        [Fact, Trait("Category", "ATagInfo")];
        METHOD CollateSequenceFallsBackToMachineForIndexesCreatedByXSharp AS VOID
            LOCAL ARRAY aTags[1]
            TRY
                SELF:Setup()
                // The RDD never writes the collation into the tag header, so
                // COLLATE is silently ignored and every index X# creates is a
                // MACHINE one. Column 6 does report the stored sequence when
                // the index was built by VFP itself.
                INDEX ON Name TAG NmIdx COLLATE GENERAL
                Assert.Equal(1, ATagInfo(aTags))
                Assert.Equal("MACHINE", (STRING) aTags[1, 6])
            FINALLY
                SELF:Teardown()
            END TRY
        END METHOD

        [Fact, Trait("Category", "ATagInfo")];
        METHOD TagsComeInCreationOrder AS VOID
            LOCAL ARRAY aTags[1]
            TRY
                SELF:Setup()
                INDEX ON Id TAG IdIdx
                INDEX ON Name TAG NmIdx
                INDEX ON Name + Str(Id) TAG BothIdx
                Assert.Equal(3, ATagInfo(aTags))
                Assert.Equal(3, (INT) ALen(aTags, 1))
                Assert.Equal("IDIDX",   (STRING) aTags[1, 1])
                Assert.Equal("NMIDX",   (STRING) aTags[2, 1])
                Assert.Equal("BOTHIDX", (STRING) aTags[3, 1])
            FINALLY
                SELF:Teardown()
            END TRY
        END METHOD

        [Fact, Trait("Category", "ATagInfo")];
        METHOD CdxNameFiltersByFileWithOrWithoutExtension AS VOID
            LOCAL ARRAY aTags[1]
            TRY
                SELF:Setup()
                INDEX ON Id TAG IdIdx
                INDEX ON Name TAG NmIdx
                Assert.Equal(2, ATagInfo(aTags, ""))
                Assert.Equal(2, ATagInfo(aTags, "TagTest"))
                Assert.Equal(2, ATagInfo(aTags, "tagtest.cdx"))
            FINALLY
                SELF:Teardown()
            END TRY
        END METHOD

        [Fact, Trait("Category", "ATagInfo")];
        METHOD UnknownCdxNameReturnsZeroAndLeavesTheArrayAlone AS VOID
            LOCAL ARRAY aTags[2, 2]
            TRY
                SELF:Setup()
                INDEX ON Id TAG IdIdx
                aTags[1, 1] := "keep"
                Assert.Equal(0, ATagInfo(aTags, "nope.cdx"))
                Assert.Equal(2, (INT) ALen(aTags, 1))
                Assert.Equal(2, (INT) ALen(aTags, 2))
                Assert.Equal("keep", (STRING) aTags[1, 1])
            FINALLY
                SELF:Teardown()
            END TRY
        END METHOD

        [Fact, Trait("Category", "ATagInfo")];
        METHOD TableWithoutIndexesReturnsZeroAndLeavesTheArrayAlone AS VOID
            LOCAL ARRAY aTags[2, 2]
            TRY
                SELF:Setup()
                aTags[1, 1] := "keep"
                Assert.Equal(0, ATagInfo(aTags))
                Assert.Equal(2, (INT) ALen(aTags, 1))
                Assert.Equal("keep", (STRING) aTags[1, 1])
            FINALLY
                SELF:Teardown()
            END TRY
        END METHOD

        [Fact, Trait("Category", "ATagInfo")];
        METHOD WorksFromAnotherAreaByAliasAndByNumber AS VOID
            LOCAL ARRAY aTags[1]
            TRY
                SELF:Setup()
                INDEX ON Id TAG IdIdx
                VAR nArea := Select()
                SELECT 0
                CREATE CURSOR Other (x INT)
                Assert.Equal(1, ATagInfo(aTags, "", "TagTest"))
                Assert.Equal("IDIDX", (STRING) aTags[1, 1])
                Assert.Equal(1, ATagInfo(aTags, "", nArea))
            FINALLY
                SELF:Teardown()
            END TRY
        END METHOD

        [Fact, Trait("Category", "ATagInfo")];
        METHOD EmptyOrUnknownAreaReturnsZero AS VOID
            LOCAL ARRAY aTags[1]
            TRY
                SELF:Setup()
                SELECT 0
                Assert.Equal(0, ATagInfo(aTags))
                Assert.Equal(0, ATagInfo(aTags, "", "NoSuchAlias"))
            FINALLY
                SELF:Teardown()
            END TRY
        END METHOD

    END CLASS
END NAMESPACE
