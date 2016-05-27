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
	CLASS RuntimeNumericTests

		[TestMethod];
		METHOD CHRTest() as void
			AreEqual(" ",CHR((DWORD)32))
		RETURN

		[TestMethod];
		METHOD CONTIMETest() as void
			AreEqual("13:34:54",CONTIME((dword)13,(dword)34,(dword)54))
		RETURN

		[TestMethod];
		METHOD FracTest() as void
			AreEqual((float)120,Fact((dword)5))
		RETURN

		[TestMethod];
		METHOD DW2BINTest() as void
			AreEqual("    ",DW2Bin((dword) 32*256*256*256+32*256*256+32*256+32))
		RETURN

		[TestMethod];
		METHOD NToCDoWTest() as void
			AreEqual("Freitag",NToCDoW(DOW(CTOD("27/05/2016"))))
		RETURN

		[TestMethod];
		METHOD NToCMonthTest() as void
			AreEqual("Juni",NToCMonth((dword)6))
		RETURN		 
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests