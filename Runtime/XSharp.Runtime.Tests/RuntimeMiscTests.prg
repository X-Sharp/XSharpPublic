USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using Microsoft.VisualStudio.TestTools.UnitTesting
using static Microsoft.VisualStudio.TestTools.UnitTesting.Assert
using XSharp.Runtime
using Vulcan

BEGIN NAMESPACE XSharp.Runtime.Tests

	[TestClass];
	CLASS MiscTests

		//[TestMethod];
		//METHOD AsSymbolTest() as void
			//local u as usual 
			//u := "Testsymbol"
			//AreEqual(true,AsSymbol(u)==#Testsymbol)
		//RETURN
		//[TestMethod];
		//METHOD BetweenTest() as void
			//local u1 as usual 
			//local u2 as usual 
			//local u3 as usual 
			//u1 := 3
			//u2 := 1
			//u3 := 8
			//AreEqual(true,Between(u1,u2,u3))
		//RETURN
		//[TestMethod];
		//METHOD CheckInstanceOfTest() as void
			//local u as usual
			//u := 6 
			//AreEqual(true,CheckInstanceOf(u,#__Usual))
		//RETURN
		//[TestMethod];
		//METHOD ClassCountTest() as void
			//AreEqual(true,ClassCount()>0)
		//RETURN
		//[TestMethod];
		//METHOD ClassListTest() as void
		    //local classes as array
			//classes := ClassList()
			//AreEqual(true,classes:Count>0)
		//RETURN
		//[TestMethod];
		//METHOD ClassNameTest() as void
		    //local fi := System.IO.DriveInfo{"C"} as System.IO.DriveInfo
			//AreEqual("DriveInfo",ClassName(fi))
		//RETURN
		[TestMethod];
		METHOD CurDriveTest() as void
			AreEqual("C",CurDrive())
		RETURN
		//[TestMethod];
		//METHOD DiskFreeTest() as void
			//AreEqual(true,DiskFree()>0)
		//RETURN
		//[TestMethod];
		//METHOD DiskSpaceTest() as void
			//AreEqual(true,DiskSpace()>0)
		//RETURN
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests