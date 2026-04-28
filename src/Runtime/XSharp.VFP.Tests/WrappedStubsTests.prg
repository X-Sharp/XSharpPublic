//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit

using System.Diagnostics

BEGIN NAMESPACE XSharp.VFP.Tests
    CLASS WrappedStubsTests
        [Fact, Trait("Category", "RuntimeCore")];
        METHOD DiskSpaceTest() AS VOID
            LOCAL nFreeSpace AS REAL8
            LOCAL nTotalSpace AS REAL8

            // 2 = Default FreeSpace
            nFreeSpace := DiskSpace()
            Assert.True(nFreeSpace > 0, "Free space should be greater than 0")

            // 1 = Total size
            nTotalSpace := DiskSpace("", 1)
            Assert.True(nTotalSpace > 0, "Total space should be greater than 0")

            // nTotalSpace should be greater or equal than nFreeSpace
            Assert.True(nTotalSpace >= nFreeSpace, "Total space must be >= Free space")
        END METHOD

        #pragma options ("undeclared", on)
        [Fact, Trait("Category", "RuntimeCore")];
        METHOD ADirTest() AS VOID
            LOCAL nFiles AS INT
            LOCAL ARRAY aFiles(1)

            nFiles := (INT)ADir(aFiles, "*.*")

            IF nFiles > 0
                Assert.Equal(5, (INT)aFiles:Columns)
                Assert.Equal(nFiles, (INT)aFiles:Rows)

                LOCAL cFileName AS STRING
                cFileName := (STRING)aFiles[1, 1]
                Assert.Equal(cFileName:ToUpper(), cFileName)
            ENDIF

            LOCAL ARRAY aFilesOriginal(1)
            ADir(aFilesOriginal, "*.*", "", 1)
            IF nFiles > 0
                Assert.NotNull(aFilesOriginal[1,1])
            ENDIF

            // Test undeclared array against a controlled directory to avoid
            // environment-dependent failures when the current directory is empty.
            LOCAL cTempDir AS STRING
            LOCAL cTempFile AS STRING
            LOCAL cSearchPattern AS STRING

            cTempDir := System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.Guid.NewGuid():ToString())
            cTempFile := System.IO.Path.Combine(cTempDir, "adirtest.tmp")
            cSearchPattern := System.IO.Path.Combine(cTempDir, "*.*")

            TRY
                System.IO.Directory.CreateDirectory(cTempDir)
                System.IO.File.WriteAllText(cTempFile, "x")

                nFiles := (INT)ADir(aNonExistentArray, cSearchPattern)
                Assert.True(nFiles > 0)
                Assert.Equal(5, (INT)aNonExistentArray:Columns)
                Assert.Equal(nFiles, (INT)aNonExistentArray:Rows)
            FINALLY
                IF System.IO.File.Exists(cTempFile)
                    System.IO.File.Delete(cTempFile)
                ENDIF
                IF System.IO.Directory.Exists(cTempDir)
                    System.IO.Directory.Delete(cTempDir)
                ENDIF
            END TRY
        END METHOD
        #pragma options("undeclared", default)

        #pragma options ("undeclared", on)
        [Fact];
        METHOD ACopyTest() AS VOID
            DIMENSION aTest[3]
            aTest[1] := "Apple"
            aTest[2] := "Banana"
            aTest[3] := "Pineapple"

            VAR nElements := (INT)ACopy(aTest, aCopied)

            Assert.True(nElements > 0)
            Assert.Equal(3, (INT)ALEN(aCopied))
        END METHOD
        #pragma options ("undeclared", default)

        [Fact, Trait("Category", "RuntimeCore")];
        METHOD VersionTest() AS VOID
            LOCAL uRet AS USUAL

            // (0) default
            uRet := Version()
            Assert.True(IsString(uRet))
            Assert.True(((STRING)uRet):Contains("XSharp"))

            // VERSION(2) -> Numeric
            uRet := Version(2)
            Assert.True(IsNumeric(uRet))
            Assert.Equal(2, (INT)uRet)

            // VERSION(3) -> "00"
            uRet := Version(3)
            Assert.True(IsString(uRet))
            Assert.Equal("00", (STRING)uRet)

            // VERSION(5) -> n * 100
            uRet := Version(5)
            Assert.True(IsNumeric(uRet))
            Assert.True((INT)uRet >= 200) // X# 2.24 -> 200

        END METHOD
    END CLASS
END NAMESPACE // XSharp.Runtime.VFP.Tests
