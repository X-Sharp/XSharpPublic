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

        [Fact, Trait("Category", "DBF")];
		METHOD TestDBFFunction() AS VOID
            VAR cDbf := DBF()
            Assert.Equal("", cDbf)
        END METHOD

        [Fact, Trait("Category", "OS")];
        METHOD TestOSFunction() AS VOID
            VAR cOS := OS(1)
            Assert.True(cOS:Length > 0)
            Assert.Equal("1", OS(11))
        END METHOD

        [Fact, Trait("Category", "DBF")];
        METHOD TestFSizeFunction() AS VOID
            CREATE CURSOR TestCursor (CUSTOMER C(15))
            VAR nSize := FSize("CUSTOMER", "TestCursor")
            Assert.Equal(15, nSize)
        END METHOD

        [Fact, Trait("Category", "DBF")];
        METHOD TestAFieldsFunction() AS VOID
            CREATE CURSOR TestCursor (ID I, NAME C(25))

            DIMENSION laFields[1]
            AFields(laFields)

            // ID
            Assert.True(laFields[1, 1] == "ID")
            Assert.True(laFields[1, 2] == "I")

            // NAME
            Assert.True(laFields[2, 1] == "NAME")
            Assert.True(laFields[2, 2] == "C")
            Assert.True(laFields[2, 3] == 25)



        END METHOD

        [Fact, Trait("Category", "UIAndWindows")];
        METHOD TestMessageBoxBell() AS VOID

            VAR cWaveSet := SET("BELL", 1)
            Assert.Equal("", cWaveSet)

            SET BELL ON
            VAR lBellState := SET("BELL")
            Assert.True(lBellState)
        END METHOD

        #pragma options ("undeclared", on)
        [Fact, Trait("Category", "General")];
        METHOD TestVarTypeMissingVariable() AS VOID
            LOCAL cResult AS STRING

            cResult := __VfpVarType({ || SomeNonExistentVariable123 })
            Assert.Equal("U", cResult)

            cResult := __VfpVarType({ || SomeNonExistentVariable123 }, .T.)
            Assert.Equal("U", cResult)
        END METHOD
        #pragma options ("undeclared", default)

        [Fact];
        METHOD TestSetDefaultTO() AS VOID
            VAR cOldDir := Environment.CurrentDirectory

            SET DEFAULT TO "C"
            Assert.True(SET("DEFAULT"):StartsWith("C:\", StringComparison.OrdinalIgnoreCase))

            SET DEFAULT TO "C:"
            Assert.True(SET("DEFAULT"):StartsWith("C:\", StringComparison.OrdinalIgnoreCase))

            VAR cNewDir := Path.GetTempPath()
            SET DEFAULT TO (cNewDir)
            Assert.Equal(cNewDir:TrimEnd(c'\\'), SET("DEFAULT"):TrimEnd(c'\\'))

            SET DEFAULT TO ".."
            Assert.True(SET("DEFAULT") != cNewDir:TrimEnd(c'\\'))

            SET DEFAULT TO (cOldDir)

        END METHOD

        [Fact];
        METHOD VfpSetDefaultReninemenTest() AS VOID
            VAR cOldDir := GetDefault()

            RuntimeState.Dialect := XSharpDialect.FoxPro

            SetDefault("c:\")
            SetDefault("C:\")
            Assert.Equal("C:\", GetDefault())

            VAR drive := XSharp.RT.Functions.Set("DEFAULT", NIL)
            Assert.Equal("C:", (STRING)drive)

            VAR dir := XSharp.RT.Functions.Set("DIRECTORY", NIL)
            Assert.Equal("C:\", (STRING)dir)

            SetDefault(cOldDir)
        END METHOD

        [Fact];
        METHOD ReleaseAllLikeTest() AS VOID
	        MemVarPut("cVar1", "Test 1")
	        MemVarPut("cVar2", "Test 2")
	        MemVarPut("nVar3", 3)

	        _MRelease("""c*""", TRUE)

	        Assert.Equal("U", Type("cVar1"))
	        Assert.Equal("U", Type("cVar2"))
	        Assert.Equal("N", Type("nVar3")) // This one remains alive

	        MemVarPut("cVar4", 4)
	        MemVarPut("cVar5", 5)

	        _MRelease("""cV*""", TRUE)

	        Assert.Equal("U", Type("cVar4"))
	        Assert.Equal("U", Type("cVar5"))
	        Assert.Equal("N", Type("nVar3")) // Still alive

	        _MClear()
        END METHOD

    	[Fact, Trait("Category", "Database")];
        METHOD SetMemoWidthTests() AS VOID
            VAR nOld := (INT)Set(Set.MemoWidth)

            Set(Set.MemoWidth, 100)
            Assert.Equal(100, (INT)Set(Set.MemoWidth))

            Set(Set.MemoWidth, 50)
            Assert.Equal(50, (INT)Set(Set.MemoWidth))

            Set(Set.MemoWidth, nOld)
        END METHOD
	END CLASS

END NAMESPACE
