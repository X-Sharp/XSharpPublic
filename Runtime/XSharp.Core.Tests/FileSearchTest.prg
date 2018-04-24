//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Text
using XUnit


BEGIN NAMESPACE XSharp.Core.Tests

	CLASS FileSearchTests 

		[Fact, Trait("Category", "File Search")]; 
		method FileTest() as void
			local lOk as logic
			local cName as string
			local dDate as DateTime
			local nAttrib, nSize as dword
			local cTime as string
			ASsert.Equal(1L, (int) FFCount("XSharp.Core.dll", 0))
			lOk := FFirst("XSharp.Core.dll", 0)
			Assert.Equal(lOk, TRUE)
			cName := FName()
			dDate := FDate()
			nAttrib := FAttrib()
			nSize := FSize()
			cTime := FTime()
			Assert.Equal(cName:ToLower(), "xsharp.core.dll")
			Assert.Equal(dDate:@@Date, DateTime.Now:@@Date)
			Assert.Equal(true, nAttrib != 0)
			Assert.Equal(true, nSize > 0)
			Assert.Equal(false, String.IsNullOrEmpty(cTime))
			lOk := FNext()
			Assert.Equal(lOk, FALSE)
			cName := FName()
			Assert.Equal(true, String.IsNullOrEmpty(cName))
			ASsert.Equal(TRUE, FFCount("XSharp.*.dll", 0) > 3)


			 
						
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests