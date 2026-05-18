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
    END CLASS

END NAMESPACE
