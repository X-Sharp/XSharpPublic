//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.IO
USING System.Text
USING XUnit

using System.Diagnostics

// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS MiscTests
		STATIC CONSTRUCTOR
        XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro

		[Fact, Trait("Category", "String")];
		METHOD Various() AS VOID
            Assert.Equal(TRUE, IsMouse())
            Assert.Equal(TRUE, IsColor())


		[Fact, Trait("Category", "String")];
		METHOD ProgramTests() AS VOID
            Assert.Equal((int) Program(-1), StackTrace{ FALSE }:FrameCount)
            Assert.Equal(Program(), "MISCTESTS:PROGRAMTESTS")
            Assert.Equal(Program(0), "MISCTESTS:PROGRAMTESTS")
            Assert.Equal(Program(1), "MISCTESTS:PROGRAMTESTS")


        [Fact, Trait("Category", "String")];
        METHOD IsNullTests() AS VOID
            // Basic tests
            Assert.True(IsNull(DBNull.Value))
            Assert.False(IsNull(0))
            Assert.False(IsNull(""))
            Assert.False(IsNull(FALSE))
        END METHOD

        [Fact, Trait("Category", "String")];
        METHOD IsBlankTests() AS VOID
            // STRINGS
            Assert.True(IsBlank(""))
            Assert.True(IsBlank("   "))
            Assert.False(IsBlank("abc"))

            // // NUMBERS
            Assert.False(IsBlank(0))
            Assert.False(IsBlank(9.99))

            // DATES
            VAR dEmpty := (DATE) 0
            VAR dToday := Today()
            Assert.True(IsBlank(dEmpty))
            Assert.False(IsBlank(dToday))

            // LOGICAL
            Assert.False(IsBlank(TRUE))
            Assert.False(IsBlank(FALSE))

            // NULL
            Assert.False(IsBlank(DBNull.Value))
        END METHOD

        [Fact, Trait("Category", "File")];
        METHOD FCreateAndFOpenTest() AS VOID
            LOCAL cFile AS STRING
            LOCAL nHandle AS INT64
            LOCAL nWritten AS INT

            cFile := Path.Combine(Path.GetTempPath(), "vfp_test_" + Guid.NewGuid():ToString("N") + ".txt")

            && 1. Test FCREATE()
            nHandle := FCreate(cFile, 0)
            LOCAL nBadHandle AS INT64

            cFile := Path.Combine(Path.GetTempPath(), "vfp_test_" + Guid.NewGuid():ToString("N") + ".txt")
            nHandle := 0

            TRY

                && 1. Test FCREATE()
                nHandle := FCreate(cFile, 0)
                Assert.True(nHandle > 0, "FCreate should return a valid handle")

                nWritten := FPuts(nHandle, "XSharp VFP File Test")
                Assert.True(nWritten > 0, "Should have written data in the file")

                IF nHandle > 0
                    FClose(nHandle)
                    nHandle := 0
                ENDIF

                && 2. Test FOPEN()
                nHandle := FOpen(cFile, 0)
                Assert.True(nHandle > 0, "FOpen should open existing file")

                IF nHandle > 0
                    FClose(nHandle)
                    nHandle := 0
                ENDIF

                && 3. Test expected failure (non existent file)
                nBadHandle := FOpen("non_existent_file.txt", 0)
                Assert.Equal(-1L, nBadHandle)

            FINALLY
                IF nHandle > 0
                    FClose(nHandle)
                ENDIF

                IF FILE(cFile)
                    File.Delete(cFile)
                ENDIF
            END TRY
        END METHOD

        [Fact, Trait("Category", "File")];
        METHOD FOpenSetPathTest() AS VOID
            LOCAL cSubDir AS STRING
            LOCAL cPathFile AS STRING
            LOCAL nHandle AS INT64
            LOCAL cOldPath AS STRING

            cSubDir := Path.Combine(Path.GetTempPath(), "VfpSetPathTest_" + Guid.NewGuid():ToString("N"))
            Directory.CreateDirectory(cSubDir)

            cPathFile := Path.Combine(cSubDir, "hidden_data.txt")
            File.WriteAllText(cPathFile, "Data")

            cOldPath := Set(Set.Path)

            TRY
                Set(Set.Path, cSubDir)

                nHandle := FOpen("hidden_data.txt", 0)
                Assert.True(nHandle > 0, "FOpen should find the file based on SET PATH")

                IF nHandle > 0
                    FClose(nHandle)
                ENDIF
            FINALLY
                Set(Set.Path, cOldPath)

                IF FILE(cPathFile)
                    File.Delete(cPathFile)
                ENDIF
                IF Directory.Exists(cSubDir)
                    Directory.Delete(cSubDir)
                ENDIF
            END TRY
        END METHOD

	END CLASS

END NAMESPACE
