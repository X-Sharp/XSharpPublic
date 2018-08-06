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


BEGIN NAMESPACE XSharp.Core.Tests

	PUBLIC CLASS MiscTests

		//[Fact];
		//METHOD AsSymbolTest() as void
			//local u as usual 
			//u := "Testsymbol"
			//Assert.Equal(true,AsSymbol(u)==#Testsymbol)
		//RETURN
		//[Fact];
		//METHOD BetweenTest() as void
			//local u1 as usual 
			//local u2 as usual 
			//local u3 as usual 
			//u1 := 3
			//u2 := 1
			//u3 := 8
			//Assert.Equal(true,Between(u1,u2,u3))
		//RETURN
		//[Fact];
		//METHOD CheckInstanceOfTest() as void
			//local u as usual
			//u := 6 
			//Assert.Equal(true,CheckInstanceOf(u,#__Usual))
		//RETURN
		//[Fact];
		//METHOD ClassCountTest() as void
			//Assert.Equal(true,ClassCount()>0)
		//RETURN
		//[Fact];
		//METHOD ClassListTest() as void
		    //local classes as array
			//classes := ClassList()
			//Assert.Equal(true,classes:Count>0)
		//RETURN
		//[Fact];
		//METHOD ClassNameTest() as void
		    //local fi := System.IO.DriveInfo{"C"} as System.IO.DriveInfo
			//Assert.Equal("DriveInfo",ClassName(fi))
		//RETURN
		[Fact, Trait("Category", "Misc")]; 
		METHOD CurDriveTest() AS VOID
			Assert.Equal("C",CurDrive():ToUpper())
		[Fact, Trait("Category", "Misc")]; 
		METHOD GetFAttrTest() AS VOID
			Assert.Equal((DWORD) FA_VOLUME, GetFAttr("V"))
			Assert.Equal((DWORD) FA_COMPRESSED, GetFAttr("C"))
			Assert.Equal((DWORD) FC_ARCHIVED, GetFAttr("A"))
			Assert.Equal((DWORD) FC_HIDDEN, GetFAttr("H"))
			Assert.Equal((DWORD) FC_SYSTEM, GetFAttr("S"))
			Assert.Equal((DWORD) FC_READONLY, GetFAttr("R"))
			Assert.Equal((DWORD) FC_HIDDEN|FC_SYSTEM, GetFAttr("HS"))
			Assert.Equal((DWORD) FC_HIDDEN|FC_SYSTEM, GetFAttr(FC_HIDDEN+FC_SYSTEM))
			Assert.Equal((DWORD) FA_VOLUME, String2FAttr("V"))
			Assert.Equal((DWORD) FA_COMPRESSED, String2FAttr("C"))
			Assert.Equal((DWORD) FC_ARCHIVED, String2FAttr("A"))
			Assert.Equal((DWORD) FC_HIDDEN, String2FAttr("H"))
			Assert.Equal((DWORD) FC_SYSTEM, String2FAttr("S"))
			Assert.Equal((DWORD) FC_READONLY, String2FAttr("R"))
			Assert.Equal((DWORD) FC_HIDDEN|FC_SYSTEM, String2FAttr("HS"))

		RETURN
		[Fact, Trait("Category", "Misc")]; 
		METHOD GetFMaskTest() AS VOID
			Assert.Equal("C:*.*", GetFMask("C:"))
			Assert.Equal("C:\*.*", GetFMask("C:\"))
			Assert.Equal("C:\*.DBF", GetFMask("C:\*.DBF"))
			Assert.Equal("C:\*.", GetFMask("C:\*."))
			Assert.Equal("*.*", GetFMask(NULL))

		RETURN
		//[Fact];
		//METHOD DiskFreeTest() as void
			//Assert.Equal(true,DiskFree()>0)
		//RETURN
		//[Fact];
		//METHOD DiskSpaceTest() as void
			//Assert.Equal(true,DiskSpace()>0)
		//RETURN
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests