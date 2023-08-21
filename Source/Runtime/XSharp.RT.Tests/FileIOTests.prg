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


// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.RT.Tests

	CLASS FileIOTests

 		[Trait("Category", "File")];
		[Fact];
		METHOD FileTest() AS VOID
			LOCAL hFile AS PTR
			LOCAL cLine AS USUAL
			LOCAL cFile AS STRING
			LOCAL cText	AS STRING
			cFile := TempFile("txt")
			hFile := FCreate(cFile)
			cLine := "line1"
			FWriteLine(hFile, cLine)
			cLine := "line2"
			FWriteLine(hFile, cLine)
			FClose(hFile)
			cText := MemoRead(cFile)
			Assert.Equal(e"line1\r\nline2\r\n", cText)
            VAR aFiles := Directory(cFile)
            Assert.Equal(1, (INT) Alen(aFiles))
            aFiles := Directory(System.Environment.CurrentDirectory+"\*.txt")
            Assert.True( Alen(aFiles) >= 1)
            // test readonly
            hFile := FOpen(cFile, FO_READ )
            Assert.True(hFile != F_ERROR)
            Assert.Equal(TRUE, FClose(hFile))


			FErase(cFile)
            aFiles := Directory(cFile)
            Assert.Equal(0, (INT) Alen(aFiles))
            aFiles := Directory("C:\XSharp\*.*","D")
		RETURN

      [Fact, Trait("Category", "File")];
		METHOD File_Tests() AS VOID
			LOCAL hFile AS PTR
			LOCAL cFileName AS STRING
			LOCAL nResult AS DWORD
			LOCAL cBuffer := "" AS STRING

			cFileName := TempFile("txt")
			hFile := FCreate(cFileName , FC_NORMAL)
			Assert.True(FEof(hFile))
			FWrite(hFile , "ABCDE" , 5)
			Assert.True(FEof(hFile))

			FSeek3(hFile, 1, FS_SET)
			Assert.False(FEof(hFile))
			nResult := FRead(hFile , REF cBuffer , 4)
			Assert.Equal(4U , nResult)
			Assert.Equal("BCDE" , cBuffer)
			Assert.True(FEof(hFile))

			FSeek3(hFile, 1, FS_SET)
			Assert.False(FEof(hFile))
			nResult := FRead(hFile , REF cBuffer , 3)
			Assert.Equal(3U , nResult)
			Assert.Equal("BCD" , cBuffer)
			Assert.False(FEof(hFile))

			FSeek3(hFile, 2, FS_SET)
			Assert.False(FEof(hFile))
			nResult := FRead(hFile , REF cBuffer , 100)
			Assert.Equal(3U , nResult)
			Assert.Equal(3 , cBuffer:Length)
			Assert.Equal("CDE" , cBuffer)
			Assert.True(FEof(hFile))

			Assert.True( FClose(hFile) )



			hFile := FOpen(cFileName , FC_NORMAL)

			nResult := FRead(hFile , REF cBuffer , 3)
			Assert.Equal(3U , nResult)
			Assert.Equal("ABC" , cBuffer)
			Assert.False(FEof(hFile))

			nResult := FRead(hFile , REF cBuffer , 3)
			Assert.Equal(2U , nResult)
			Assert.Equal("DE" , cBuffer)
			Assert.True(FEof(hFile))

			FSeek3(hFile, 1, FS_SET)
			Assert.False(FEof(hFile))
			nResult := FRead(hFile , REF cBuffer , 4)
			Assert.Equal(4U , nResult)
			Assert.Equal("BCDE" , cBuffer)
			Assert.True(FEof(hFile))

			FSeek3(hFile, 1, FS_SET)
			Assert.False(FEof(hFile))
			nResult := FRead(hFile , REF cBuffer , 3)
			Assert.Equal(3U , nResult)
			Assert.Equal("BCD" , cBuffer)
			Assert.False(FEof(hFile))

			FSeek3(hFile, 2, FS_SET)
			Assert.False(FEof(hFile))
			nResult := FRead(hFile , REF cBuffer , 100)
			Assert.Equal(3U , nResult)
			Assert.Equal(3 , cBuffer:Length)
			Assert.Equal("CDE" , cBuffer)
			Assert.True(FEof(hFile))

			Assert.True( FClose(hFile) )


			hFile := FOpen(cFileName , FO_EXCLUSIVE)

			nResult := FRead(hFile , REF cBuffer , 3)
			Assert.Equal(3U , nResult)
			Assert.Equal("ABC" , cBuffer)
			Assert.False(FEof(hFile))

			nResult := FRead(hFile , REF cBuffer , 3)
			Assert.Equal(2U , nResult)
			Assert.Equal("DE" , cBuffer)
			Assert.True(FEof(hFile))

			FSeek3(hFile, 1, FS_SET)
			Assert.False(FEof(hFile))
			nResult := FRead(hFile , REF cBuffer , 4)
			Assert.Equal(4U , nResult)
			Assert.Equal("BCDE" , cBuffer)
			Assert.True(FEof(hFile))

			FSeek3(hFile, 1, FS_SET)
			Assert.False(FEof(hFile))
			nResult := FRead(hFile , REF cBuffer , 3)
			Assert.Equal(3U , nResult)
			Assert.Equal("BCD" , cBuffer)
			Assert.False(FEof(hFile))

			FSeek3(hFile, 2, FS_SET)
			Assert.False(FEof(hFile))
			nResult := FRead(hFile , REF cBuffer , 100)
			Assert.Equal(3U , nResult)
			Assert.Equal(3 , cBuffer:Length)
			Assert.Equal("CDE" , cBuffer)
			Assert.True(FEof(hFile))

			Assert.True( FClose(hFile) )


	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
