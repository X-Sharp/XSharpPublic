USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit


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
		METHOD CurDriveTest() as void
			Assert.Equal("C",CurDrive():ToUpper())
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