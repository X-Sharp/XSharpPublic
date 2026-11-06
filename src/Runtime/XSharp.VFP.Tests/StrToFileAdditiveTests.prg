//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.IO
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS StrToFileAdditiveTests
        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        STATIC PRIVATE METHOD NewTempFile() AS STRING
            RETURN System.IO.Path.Combine(System.IO.Path.GetTempPath(), "VfpStrToFileAdd_" + Guid.NewGuid():ToString("N") + ".txt")
        END METHOD

        // STRTOFILE(cText, cFile, .T.) threw a NullReferenceException (issue 2157)
        [Fact, Trait("Category", "File")];
        METHOD StrToFileAdditiveAppendsToAnExistingFile() AS VOID
            LOCAL cFile AS STRING
            cFile := NewTempFile()
            TRY
                Assert.Equal(3, (INT) StrToFile("abc", cFile))
                Assert.Equal(3, (INT) StrToFile("def", cFile, .T.))
                Assert.Equal("abcdef", FileToStr(cFile))
            FINALLY
                System.IO.File.Delete(cFile)
            END TRY
        END METHOD

        // the numeric flag 1 is the same as .T.
        [Fact, Trait("Category", "File")];
        METHOD StrToFileAdditiveWithTheNumericFlag() AS VOID
            LOCAL cFile AS STRING
            cFile := NewTempFile()
            TRY
                StrToFile("abc", cFile)
                Assert.Equal(3, (INT) StrToFile("def", cFile, 1))
                Assert.Equal("abcdef", FileToStr(cFile))
            FINALLY
                System.IO.File.Delete(cFile)
            END TRY
        END METHOD

        // VFP creates the file when it does not exist yet
        [Fact, Trait("Category", "File")];
        METHOD StrToFileAdditiveCreatesAMissingFile() AS VOID
            LOCAL cFile AS STRING
            cFile := NewTempFile()
            TRY
                Assert.Equal(3, (INT) StrToFile("abc", cFile, .T.))
                Assert.Equal("abc", FileToStr(cFile))
            FINALLY
                System.IO.File.Delete(cFile)
            END TRY
        END METHOD

        // appending an empty string returns 0 and leaves the file alone
        [Fact, Trait("Category", "File")];
        METHOD StrToFileAdditiveWithAnEmptyString() AS VOID
            LOCAL cFile AS STRING
            cFile := NewTempFile()
            TRY
                StrToFile("abc", cFile)
                Assert.Equal(0, (INT) StrToFile("", cFile, .T.))
                Assert.Equal("abc", FileToStr(cFile))
            FINALLY
                System.IO.File.Delete(cFile)
            END TRY
        END METHOD

        // a binary is appended as bytes too
        [Fact, Trait("Category", "File")];
        METHOD StrToFileAdditiveAppendsABinary() AS VOID
            LOCAL cFile AS STRING
            LOCAL uBin AS USUAL
            cFile := NewTempFile()
            uBin := (BINARY) <BYTE>{0x64, 0x65, 0x66}
            TRY
                StrToFile("abc", cFile)
                Assert.Equal(3, (INT) StrToFile(uBin, cFile, .T.))
                Assert.Equal("abcdef", FileToStr(cFile))
            FINALLY
                System.IO.File.Delete(cFile)
            END TRY
        END METHOD

    END CLASS

END NAMESPACE
