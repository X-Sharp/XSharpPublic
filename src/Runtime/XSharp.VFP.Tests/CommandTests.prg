//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS CommandTests
        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        [Fact];
        METHOD TestDirFullSuite() AS VOID
            LOCAL cOldDir AS STRING
            LOCAL cTempPath AS STRING
            LOCAL cOutFile AS STRING
            LOCAL cContent AS STRING
            cOldDir := Directory.GetCurrentDirectory()

            local oDir as DirectoryInfo
            oDir := Directory.CreateDirectory(Path.Combine(Path.GetTempPath(), "DirTest_" + Guid.NewGuid():ToString("N")))
            cTempPath := oDir:FullName

            Assert.True(Directory.Exists(cTempPath))

            TRY
                SET DEFAULT TO (cTempPath)

                CREATE TABLE NormalTest (Id INT, Name CHAR(20))
                INSERT INTO NormalTest VALUES (1, "John")
                INSERT INTO NormalTest VALUES (2, "Peter")

                CREATE TABLE this_is_a_table_with_a_long_name (Id INT)

                File.WriteAllText(Path.Combine(cTempPath, "Readme.txt"), "Test content")
                File.WriteAllText(Path.Combine(cTempPath, "NoExtensionFile"), "No extension")

                XSharp.CoreDb.CloseAll()

                cOutFile := Path.GetTempFileName()
                LOCAL lcFileVar := cOutFile AS STRING

                DIR *.dbf TO FILE (lcFileVar)
                cContent := File.ReadAllText(cOutFile)
                Assert.True(cContent:Contains("NORMALTEST.DBF"))
                Assert.True(cContent:Contains("2"))
                Assert.True(cContent:Contains("Database Table/DBF files"))

                DIR *. TO FILE (lcFileVar)
                cContent := File.ReadAllText(cOutFile)
                Assert.True(cContent:Contains("NOEXTENSIONFILE"))

                DIR LIKE *.* TO FILE (lcFileVar)
                cContent := File.ReadAllText(cOutFile)
                Assert.True(cContent:Contains("README.TXT"))
                Assert.True(cContent:Contains("files found."))

                DIR TO FILE (lcFileVar)
                cContent := File.ReadAllText(cOutFile)
                Assert.True(cContent:Contains("NORMALTEST.DBF"))

                LOCAL lcSubFolder := "SubTest" AS STRING
                Directory.CreateDirectory(Path.Combine(cTempPath, lcSubFolder))

                DIR (lcSubFolder + "\*.dbf") TO FILE (lcFileVar)
                cContent := File.ReadAllText(cOutFile)
                Assert.True(cContent:Contains("No matching files found."))

            FINALLY
                XSharp.CoreDb.CloseAll()
                SET DEFAULT TO (cOldDir)
                Directory.SetCurrentDirectory(cOldDir)

                IF File.Exists(cOutFile)
                    File.Delete(cOutFile)
                ENDIF

                IF Directory.Exists(cTempPath)
                    Directory.Delete(cTempPath, TRUE)
                ENDIF
            END TRY
        RETURN

        [Fact];
        METHOD TestCreateTableWithPath() AS VOID
            VAR cOldDir := Directory.GetCurrentDirectory()
            VAR cTempPath := Path.Combine(Path.GetTempPath(), "CreateTableTest_" + Guid.NewGuid():ToString("N"))
            VAR cSubDir := Path.Combine(cTempPath, "SubDir")
            System.IO.Directory.CreateDirectory(cSubDir)

            TRY
                SET DEFAULT TO (cTempPath)
                VAR cFile1 := Path.Combine(cSubDir, "TestTable1.dbf")
                CREATE TABLE (cFile1) (Id INT, Name C(20))
                Assert.True(File.Exists(cFile1))
                Assert.True(Used("TestTable1"))

                VAR cFile2 := Path.Combine(cSubDir, "TestTable2.dbf")
                CREATE TABLE (cFile2) (Id INT, Value c(10))
                Assert.True(File.Exists(cFile2))
                Assert.True(Used("TestTable2"))
                XSharp.CoreDb.CloseAll()
            FINALLY
                System.IO.Directory.SetCurrentDirectory(cOldDir)
                TRY
                    System.IO.Directory.Delete(cTempPath, TRUE)
                CATCH
                END TRY
            END TRY
        END METHOD

        [Fact];
        METHOD TestIsExclusiveAndIsReadOnly() AS VOID
            VAR cOldDir := System.IO.Directory.GetCurrentDirectory()
            VAR oDir := System.IO.Directory.CreateDirectory(Path.Combine(Path.GetTempPath(), ;
                "IsExclusiveTest_" + Guid.NewGuid():ToString("N")))
            VAR cTempPath := oDir:FullName
            VAR cFile := Path.Combine(cTempPath, "TestExcl.dbf")

            TRY
                SET DEFAULT TO (cTempPath)
                CREATE TABLE TestExcl (Id INT)
                XSharp.CoreDb.CloseAll()

                // Shared -> ISEXCLUSIVE = FALSE
                DbUseArea(TRUE, "DBFVFP", cFile, "TestExcl", TRUE, FALSE)
                Assert.False(IsExclusive())
                Assert.False(IsExclusive("TestExcl"))
                Assert.False(IsExclusive(Select()))
                XSharp.CoreDb.CloseAll()

                // Exclusive -> ISEXCLUSIVE = TRUE
                DbUseArea(TRUE, "DBFVFP", cFile, "TestExcl", FALSE, FALSE)
                Assert.True(IsExclusive())
                Assert.True(IsExclusive("TestExcl"))
                Assert.True(IsExclusive(Select()))
                XSharp.CoreDb.CloseAll()

                // ReadOnly (NOUPDATE) -> ISREADONLY = TRUE
                DbUseArea(TRUE, "DBFVFP", cFile, "TestExcl", FALSE, TRUE)
                Assert.True(IsReadOnly())
                Assert.True(IsReadOnly("TestExcl"))
                Assert.True(IsReadOnly(Select()))
                XSharp.CoreDb.CloseAll()

                // Normal -> ISREADONLY = FALSE
                DbUseArea(TRUE, "DBFVFP", cFile, "TestExcl", FALSE, FALSE)
                Assert.False(IsReadOnly())
                Assert.False(IsReadOnly("TestExcl"))
                Assert.False(IsReadOnly(Select()))
                XSharp.CoreDb.CloseAll()
            FINALLY
                XSharp.CoreDb.CloseAll()
                SET DEFAULT TO (cOldDir)
                System.IO.Directory.SetCurrentDirectory(cOldDir)
                TRY
                    System.IO.Directory.Delete(cTempPath, TRUE)
                CATCH
                END TRY
            END TRY
        END METHOD

        [Fact];
        METHOD TestIndexSeek() AS VOID
            VAR cOldDir := System.IO.Directory.GetCurrentDirectory()
            VAR oDir := System.IO.Directory.CreateDirectory(Path.Combine(Path.GetTempPath(), ;
                "IndexSeekTest_" + Guid.NewGuid():ToString("N")))
            VAR cTempPath := oDir:FullName
            TRY
                SET DEFAULT TO (cTempPath)
                CREATE TABLE SeekTest (Id INT, Name C(10))
                INSERT INTO SeekTest VALUES(1, "Alpha")
                INSERT INTO SeekTest VALUES(2, "Beta")
                INSERT INTO SeekTest VALUES(3, "Gamma")
                INDEX ON Name TAG NameIdx
                // Found -> pointer must not move.
                GO TOP
                VAR nRec := RecNo()
                Assert.True(IndexSeek("Beta"))
                Assert.Equal((INT)nRec, (INT) RecNo())

                // Not found -> pointer must not move
                nRec := RecNo()
                Assert.False(IndexSeek("Zeta"))
                Assert.Equal((INT)nRec, (INT) RecNo())

                // Found -> pointer must move
                GO TOP
                Assert.True(IndexSeek("Beta", .T.))
                Assert.Equal("Beta", ALLTRIM(SeekTest.Name))

                // Not found with lMovePointer = .T.
                Assert.False(IndexSeek("Zeta", .T.))

                // With alias string -> pointer must not move
                GO TOP
                nRec := RecNo()
                Assert.True(IndexSeek("Gamma", .F., "SeekTest"))
                Assert.Equal((INT)nRec, (INT) RecNo())

                // With workarea number -> pointer must not move
                GO TOP
                nRec := RecNo()
                Assert.True(IndexSeek("Alpha", .F., Select()))
                Assert.Equal((INT)nRec, (INT) RecNo())

                // Non-existent area -> must return false
                Assert.False(IndexSeek("Alpha", .F., "NonExistent"))
            FINALLY
                XSharp.CoreDb.CloseAll()
                SET DEFAULT TO (cOldDir)
                System.IO.Directory.SetCurrentDirectory(cOldDir)
                TRY ; System.IO.Directory.Delete(cTempPath, TRUE) ; CATCH ; END TRY
            END TRY
        END METHOD

        [Fact];
        METHOD TestGetFldStateAndSetFldState() AS VOID
            VAR cOldDir := System.IO.Directory.GetCurrentDirectory()
            VAR oDir := System.IO.Directory.CreateDirectory(Path.Combine(Path.GetTempPath(), ;
                "FldStateTest_" + Guid.NewGuid():ToString("N")))
            VAR cTempPath := oDir:FullName

            TRY
                SET DEFAULT TO (cTempPath)

                // 3-field table  (field count differs from other tests -> fresh state array)
                CREATE TABLE FldTest (Id INT, Name C(10), Active L)
                INSERT INTO FldTest VALUES (1, "Alice", .T.)
                GO TOP

                // Default state: 1 for everything (no buffering active yet)
                Assert.Equal(1, (INT) GetFldState(0))           // deletion flag
                Assert.Equal(1, (INT) GetFldState(1))           // field 1 by number
                Assert.Equal(1, (INT) GetFldState("NAME"))      // field by name
                Assert.Equal("1111", (STRING) GetFldState(-1))  // all: deletion + 3 fields

                // SETFLDSTATE roundtrip: mark field 1 as modified (2)
                Assert.True(SetFldState(1, 2))
                Assert.Equal(2, (INT) GetFldState(1))
                Assert.Equal("1211", (STRING) GetFldState(-1))

                // SETFLDSTATE by name
                Assert.True(SetFldState("ACTIVE", 2))
                Assert.Equal(2, (INT) GetFldState("ACTIVE"))
                Assert.Equal(2, (INT) GetFldState(3))           // same field by number

                // Deletion field (0)
                Assert.True(SetFldState(0, 2))
                Assert.Equal(2, (INT) GetFldState(0))
                Assert.Equal("2212", (STRING) GetFldState(-1))  // deletion=2, Id=2, Name=1, Active=2

                // Reset a field back to 1
                Assert.True(SetFldState(1, 1))
                Assert.Equal(1, (INT) GetFldState(1))

                // Alias and workarea number overloads
                Assert.True(SetFldState(2, 2))
                Assert.Equal(2, (INT) GetFldState(2, "FldTest"))
                Assert.Equal(2, (INT) GetFldState(2, Select()))

                // Invalid state value -> FALSE
                Assert.False(SetFldState(1, 5))
                Assert.False(SetFldState(1, 0))

                // EOF -> .NULL.  (verify as non-NIL, non-numeric — exact NULL type per runtime)
                GO BOTTOM
                SKIP
                Assert.True(Eof())
                VAR vNull := GetFldState(1)
                Assert.True((OBJECT) vNull IS System.DBNull)

                // Non-existent alias -> NIL
                Assert.True(GetFldState(1, "NoSuchAlias") == NIL)

            FINALLY
                XSharp.CoreDb.CloseAll()
                SET DEFAULT TO (cOldDir)
                System.IO.Directory.SetCurrentDirectory(cOldDir)
                TRY ; System.IO.Directory.Delete(cTempPath, TRUE) ; CATCH ; END TRY
            END TRY
        END METHOD

        [Fact];
        METHOD TestCursorSetPropAndCursorGetProp() AS VOID
            VAR cOldDir := System.IO.Directory.GetCurrentDirectory()
            VAR oDir := System.IO.Directory.CreateDirectory(Path.Combine(Path.GetTempPath(), ;
                "CursorPropTest_" + Guid.NewGuid():ToString("N")))
            VAR cTempPath := oDir:FullName

            TRY
                SET DEFAULT TO (cTempPath)
                CREATE TABLE TmpX (Id INT, Name C(20), Active L)
                INSERT INTO TmpX VALUES (1, "Alice", .T.)
                GO TOP

                // Default buffering is 1 (off)
                Assert.Equal(1, (INT) CursorGetProp("Buffering"))
                Assert.Equal(3, (INT) CursorGetProp("SourceType"))

                // Set buffering to optimistic table (5)
                Assert.True(CursorSetProp("Buffering", 5))
                Assert.Equal(5, (INT) CursorGetProp("Buffering"))

                // Set buffering to pessimistic row (2) -- by alias
                Assert.True(CursorSetProp("Buffering", 2, "TmpX"))
                Assert.Equal(2, (INT) CursorGetProp("Buffering"))

                // Set buffering -- by workarea number
                Assert.True(CursorSetProp("Buffering", 3, Select()))
                Assert.Equal(3, (INT) CursorGetProp("Buffering"))

                // Invalid buffering value -> FALSE
                Assert.False(CursorSetProp("Buffering", 0))
                Assert.False(CursorSetProp("Buffering", 6))
                Assert.False(CursorSetProp("Buffering", "abc"))

                // Invalid alias -> FALSE for SET, NIL for GET
                Assert.False(CursorSetProp("Buffering", 2, "NoSuchAlias"))
                Assert.False(CursorGetProp("Buffering", "NoSuchAlias"))

                // Read-only properties
                VAR cSrc := CursorGetProp("SourceName")
                Assert.True(IsString(cSrc))
                Assert.True(((STRING) cSrc):Contains(".DBF"))
                Assert.Equal("", (STRING) CursorGetProp("Database"))
                Assert.Equal("", (STRING) CursorGetProp("SQL"))
                Assert.Equal(0, (INT) CursorGetProp("ConnectHandle"))

                // Cargo cleared on close
                Assert.True(CursorSetProp("Buffering", 5))
                XSharp.CoreDb.CloseAll()
                DbUseArea(TRUE, "DBFVFP", Path.Combine(cTempPath, "TmpX.dbf"), "TmpX", FALSE, FALSE)
                Assert.Equal(1, (INT) CursorGetProp("Buffering"))

            FINALLY
                XSharp.CoreDb.CloseAll()
                SET DEFAULT TO (cOldDir)
                System.IO.Directory.SetCurrentDirectory(cOldDir)
                TRY ; System.IO.Directory.Delete(cTempPath, TRUE) ; CATCH ; END TRY
            END TRY
        END METHOD

        [Fact];
        METHOD LabelCommandAbsorbed() AS VOID
            // LABEL FORM should compile and run without error (silent no-op)
            LABEL FORM "nonexistent.lbx"
            LABEL FORM "nonexistent.lbx" FOR .T.
            LABEL FORM "nonexistent.lbx" TO FILE "output.txt"
            LABEL FORM "nonexistent.lbx" TO PRINTER
            LABEL FORM "nonexistent.lbx" PREVIEW NOWAIT
            // If we get here without exception, the command is properly absorbed
            Assert.True(.T.)
        END METHOD
    END CLASS

END NAMESPACE
