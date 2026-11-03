//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.IO
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS StrToFileBinaryTests
        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        STATIC PRIVATE METHOD NewTempFile() AS STRING
            RETURN System.IO.Path.Combine(System.IO.Path.GetTempPath(), "VfpStrToFileTest_" + Guid.NewGuid():ToString("N") + ".bin")
        END METHOD

        STATIC PRIVATE METHOD Hex(aBytes AS BYTE[]) AS STRING
            LOCAL oBuilder AS System.Text.StringBuilder
            oBuilder := System.Text.StringBuilder{}
            FOREACH VAR b IN aBytes
                oBuilder:Append(b:ToString("X2"))
            NEXT
            RETURN oBuilder:ToString()
        END METHOD

        // STRCONV(,9) returns a binary in X#. STRTOFILE has to write those bytes,
        // not the 0h... text, which is the usual way to produce a UTF-8 file
        [Fact, Trait("Category", "File")];
        METHOD StrToFileWritesTheBytesOfABinary() AS VOID
            LOCAL cFile AS STRING
            LOCAL uConv AS USUAL
            LOCAL nWritten AS INT
            cFile := NewTempFile()
            uConv := StrConv("<x>abc</x>", 9)
            TRY
                nWritten := StrToFile(uConv, cFile)
                Assert.Equal("3C783E6162633C2F783E", Hex(System.IO.File.ReadAllBytes(cFile)))
                Assert.Equal(10, nWritten)
            FINALLY
                System.IO.File.Delete(cFile)
            END TRY
        END METHOD

        // bytes that are not readable text must survive untouched
        [Fact, Trait("Category", "File")];
        METHOD StrToFileWritesBytesThatAreNotText() AS VOID
            LOCAL cFile AS STRING
            LOCAL uBin AS USUAL
            cFile := NewTempFile()
            uBin := (BINARY) <BYTE>{0x00, 0xFF, 0x10}
            TRY
                Assert.Equal(3, (INT) StrToFile(uBin, cFile))
                Assert.Equal("00FF10", Hex(System.IO.File.ReadAllBytes(cFile)))
            FINALLY
                System.IO.File.Delete(cFile)
            END TRY
        END METHOD

        [Fact, Trait("Category", "File")];
        METHOD StrToFileAppendsABinary() AS VOID
            LOCAL cFile AS STRING
            cFile := NewTempFile()
            TRY
                StrToFile((BINARY) <BYTE>{0x41, 0x42}, cFile)
                // VFP returns what it appended, not the size of the file
                Assert.Equal(1, (INT) StrToFile((BINARY) <BYTE>{0x43}, cFile, TRUE))
                Assert.Equal("414243", Hex(System.IO.File.ReadAllBytes(cFile)))
            FINALLY
                System.IO.File.Delete(cFile)
            END TRY
        END METHOD

        [Fact, Trait("Category", "File")];
        METHOD StrToFileWritesTheUtf8BomBeforeABinary() AS VOID
            LOCAL cFile AS STRING
            cFile := NewTempFile()
            TRY
                // the BOM counts towards the number of bytes written
                Assert.Equal(4, (INT) StrToFile((BINARY) <BYTE>{0x41}, cFile, 4))
                Assert.Equal("EFBBBF41", Hex(System.IO.File.ReadAllBytes(cFile)))
            FINALLY
                System.IO.File.Delete(cFile)
            END TRY
        END METHOD

        // a string still goes through the path it always used
        [Fact, Trait("Category", "File")];
        METHOD StrToFileStillWritesAString() AS VOID
            LOCAL cFile AS STRING
            cFile := NewTempFile()
            TRY
                Assert.Equal(3, (INT) StrToFile("abc", cFile))
                Assert.Equal("abc", FileToStr(cFile))
            FINALLY
                System.IO.File.Delete(cFile)
            END TRY
        END METHOD

        // the same call the user reported, a USUAL holding a binary
        [Fact, Trait("Category", "File")];
        METHOD StrToFileDoesNotWriteTheHexRepresentation() AS VOID
            LOCAL cFile AS STRING
            LOCAL uConv AS USUAL
            cFile := NewTempFile()
            uConv := StrConv("abc", 9)
            TRY
                StrToFile(uConv, cFile)
                Assert.False(FileToStr(cFile):StartsWith("0h"))
            FINALLY
                System.IO.File.Delete(cFile)
            END TRY
        END METHOD

    END CLASS
END NAMESPACE
