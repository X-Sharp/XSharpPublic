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

	CLASS FileSearchTests 

		[Fact, Trait("Category", "File Search")]; 
		METHOD FileTest() AS VOID
			LOCAL lOk AS LOGIC
			LOCAL cName AS STRING
			//LOCAL dDate AS DateTime
			LOCAL nAttrib, nSize AS DWORD
			LOCAL cTime AS STRING
			ASsert.Equal(1L, (INT) FFCount("XSharp.Core.dll", 0))
			lOk := FFirst("XSharp.Core.dll", 0)
			Assert.Equal(lOk, TRUE)
			cName := FName()
			//dDate := FDate()
			nAttrib := FAttrib()
			nSize := FSize()
			cTime := FTime()
			Assert.Equal(cName:ToLower(), "xsharp.core.dll")
			//Assert.Equal(dDate:@@Date, DateTime.Now:@@Date)
			Assert.Equal(TRUE, nAttrib != 0)
			Assert.Equal(TRUE, nSize > 0)
			Assert.Equal(FALSE, String.IsNullOrEmpty(cTime))
			lOk := FNext()
			Assert.Equal(lOk, FALSE)
			cName := FName()
			Assert.Equal(TRUE, String.IsNullOrEmpty(cName))
			ASsert.Equal(TRUE, FFCount("XSharp.*.dll", 0) > 3)

		RETURN
		[Fact, Trait("Category", "File Search")]; 
		METHOD File2Test() AS VOID
			LOCAL cName AS STRING
			LOCAL cName2 AS STRING
			FErase("FOO.TXT")
			Assert.Equal(FALSE, File("FOO.TXT"))
			FClose(Fcreate("FOO.TXT"))
			Assert.Equal(TRUE, File("FOO.TXT"))
			cName := FPathName()
			Assert.Equal(FALSE, String.IsNullOrEmpty(cName))
			Assert.Equal(TRUE, File("FO?.TXT"))
			cName2 := FPathName()
			Assert.Equal(cName, cName2)
			Assert.Equal(TRUE, File("XCOPY.EXE"))	// uses GetEnv("PATH")
			cName := FPathName()
			Assert.Equal(TRUE, File("XCOP*.EXE"))	// uses GetEnv("PATH")
			cName2 := FPathName()
			Assert.Equal(cName:ToLower(), cName2:ToLower())

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
