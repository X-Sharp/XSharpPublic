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
			sFile	 := "FileIo.txt"
			sToWrite := "This is a line of text"
			hFile := FCreate2(sFile,FC_NORMAL)
			assert.NotEqual(hFile:ToInt32(), F_ERROR)
			FWriteLine(hFile, sToWrite)
			FWriteLine(hFile, sToWrite)
			FWriteLine(hFile, sToWrite)
			frewind(hFile)
			sText := FreadLine2(hFile, Slen(sToWrite)+20)
			Assert.Equal(sText , sToWrite) 
			FChSize(hFile, Slen(sToWrite)+2)
			Assert.Equal(true, fCommit(hFile))
			Assert.Equal(true, FFlush(hFile))
			Assert.Equal(true, fEof(hFile))
			FSeek3(hFile, 0, FS_SET)
			Assert.Equal(false, fEof(hFile))
			FClose(hFile)
			Assert.Equal(false, fCommit(hFile))
			Assert.Equal(false, FFlush(hFile))
			Assert.Equal(true, fEof(hFile))
			sText := System.IO.File.ReadAllText(sFile)
			Assert.Equal(sText , sToWrite +e"\r\n")
			hFile := FCreate2(sFile,FC_NORMAL)
			FWriteLine(hFile, sToWrite,10)


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
			frewind(hFile)
			FWriteLine3(hFile, sToWrite,10)
			frewind(hFile)
			sText := FreadLine2(hFile, 9)
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
			
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests