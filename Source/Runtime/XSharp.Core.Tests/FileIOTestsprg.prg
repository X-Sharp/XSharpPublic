//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Text
USING XUnit


BEGIN NAMESPACE XSharp.Core.Tests

    CLASS FileIOTests

        [Fact, Trait("Category", "File IO")];
        METHOD FileTest() AS VOID
            LOCAL hFile AS IntPtr
            LOCAL sText AS STRING
            LOCAL sToWrite AS STRING
            LOCAL sFile AS STRING
            sFile	 := TempFile("txt")
            sToWrite := "This is a line of text"
            hFile := FCreate2(sFile,FC_NORMAL)
            assert.NotEqual(hFile, F_ERROR)
            FWriteLine(hFile, sToWrite)
            FWriteLine(hFile, sToWrite)
            FWriteLine(hFile, sToWrite)
            FRewind(hFile)
            sText := FReadLine2(hFile, SLen(sToWrite)+20)
            Assert.Equal(sText , sToWrite)
            FChSize(hFile, SLen(sToWrite)+2)
            Assert.Equal(TRUE, FCommit(hFile))
            Assert.Equal(TRUE, FFlush(hFile))
            Assert.Equal(TRUE, FEof(hFile))
            FSeek3(hFile, 0, FS_SET)
            Assert.Equal(FALSE, FEof(hFile))
            FClose(hFile)
            Assert.Equal(FALSE, FCommit(hFile))
            Assert.Equal(FALSE, FFlush(hFile))
            Assert.Equal(TRUE, FEof(hFile))
            sText := System.IO.File.ReadAllText(sFile)
            Assert.Equal(sText , sToWrite +e"\r\n")
            hFile := FCreate2(sFile,FC_NORMAL)
            FWriteLine(hFile, sToWrite,10)
            Assert.Equal((INT) FSeek3(hFile, 0, FS_SET) , 0)
            Assert.Equal((INT) FSeek3(hFile, 1, FS_RELATIVE) , 1)
            Assert.Equal((INT) FSeek3(hFile, 0, FS_END) , (INT) FTell(hFile))


            FClose(hFile)
            sText := System.IO.File.ReadAllText(sFile)
            Assert.Equal(sText , Left(sToWrite,10) +e"\r\n")
            hFile := FCreate2(sFile,FC_NORMAL)
            FWriteLine3(hFile, sToWrite,10)
            FClose(hFile)
            sText := System.IO.File.ReadAllText(sFile)
            Assert.Equal(sText , Left(sToWrite,10) +e"\r\n")
            hFile := FCreate2(sFile,FC_NORMAL)
            FWriteLine3(hFile, sToWrite,10)
            FRewind(hFile)
            FWriteLine3(hFile, sToWrite,10)
            FRewind(hFile)
            sText := FReadLine2(hFile, 9)
            Assert.Equal(sText , Left(sToWrite,9) )
            FClose(hFile)
            sText := System.IO.File.ReadAllText(sFile)
            Assert.Equal(sText , Left(sToWrite,10) +e"\r\n")


            hFile := FCreate2(sFile,FC_NORMAL)
            Assert.Equal(3U, FWrite(hFile, "abc"))		// overload with 2 args
            Assert.Equal(3U, FWrite(hFile, "def",3U))	// overload with 3 args, DWORD
            Assert.Equal(2U, FWrite(hFile, "ghi",2))	// overload with 3 args,INT,  shorter length than source
            Assert.Equal(8U, FTell(hFile))
            FClose(hFile)
            sText := System.IO.File.ReadAllText(sFile)
            Assert.Equal(sText , "abcdefgh")
            FErase(sFile)

        RETURN
        [Fact, Trait("Category", "File IO")];
        METHOD FileTest2() AS VOID
            Assert.Equal(FALSE, File("D:\t?est\FileDoesnotExist.txt"))
            Assert.Equal(87, (INT) FError())   // Illegal characters in path
            Assert.Equal(FALSE, FErase("D:\FileThatDoesNotExist"))
            Assert.Equal(2, (INT) FError())   // Ferase should set this to FileNotFound
            Assert.Equal(FALSE, FRename("D:\FileThatDoesNotExist","D:\AnotherFileName.txt"))
            Assert.Equal(2, (INT) FError())   // FRename should set this to FileNotFound
        [Fact, Trait("Category", "File IO")];
        METHOD FReadEoFTest() AS VOID
            LOCAL hFile AS PTR
            LOCAL nRead AS DWORD
            LOCAL cBuffer := "" AS STRING
            LOCAL cFileName AS STRING
            LOCAL cString AS STRING
            LOCAL nSize := 0 AS INT

            cFileName := TempFile("txt")
            hFile := FCreate(cFileName)

            FOR LOCAL n := 70 AS INT UPTO 200 // try also from n := 1

                FOR LOCAL m := 1 UPTO n
                    cString := Replicate("A" , 200)
                    // We cannot use String2Psz() here because this code is compiled against Core
                    // And String2Psz() requires XSharp.RT
//					FWrite3(hFile, String2Psz(cString), SLen(cString))
                    FWrite(hFile, cString, SLen(cString)) // workaround
                    nSize += 200
                NEXT

                FSeek3(hFile, 0, FS_SET)

                LOCAL nCount := 0 AS INT
                LOCAL nExpectedCount := nSize / 65530 + 1 AS INT
                DO WHILE .not. FEof(hFile)
                    nCount ++
                    nRead := FRead(hFile,REF cBuffer,65530)
                    IF nCount == nExpectedCount
                        Assert.Equal(nRead,  (DWORD) (nSize % 65530))
                    ELSE
                        Assert.Equal(nRead, (DWORD) 65530)
                    END IF
//					? nRead, cBuffer:Length, FEof(hFile)
                    IF nCount > nExpectedCount
                        EXIT
                    END IF
                END DO
                Assert.True(FEof(hFile))
                Assert.Equal(nExpectedCount, nCount)

            NEXT
            FClose(hFile)
            FErase(cFileName)
        RETURN
    END CLASS
END NAMESPACE // XSharp.Runtime.Tests
