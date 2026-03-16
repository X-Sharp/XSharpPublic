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
        END METHOD

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
