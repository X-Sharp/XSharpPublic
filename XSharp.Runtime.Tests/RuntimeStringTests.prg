USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using Microsoft.VisualStudio.TestTools.UnitTesting
using static Microsoft.VisualStudio.TestTools.UnitTesting.Assert
using XSharp.Runtime

BEGIN NAMESPACE XSharp.Runtime.Tests

	[TestClass];
	CLASS RuntimeStringTests

	[TestMethod];
	METHOD LeftTest() as void

		local s := "Hello World" as string
		local unassigned		 as string		                  
   			                  
		AreEqual("",left(s,0))
		AreEqual("H",left(s,1))
		AreEqual("Hello World",left(s,99))

		AreEqual(null,left(unassigned,0))
		AreEqual(null,left(unassigned,1))
		AreEqual(null,left(unassigned,99))
	         	
	RETURN

	[TestMethod];
	METHOD AdjustFNameTest() as void
	    AreEqual("    xyz   ddss.dbf",AdjustFName("    xyz   ddss    .dbf"))
	    AreEqual(null,AdjustFName(null))
		AreEqual(" abc ced",AdjustFName(" abc ced   "))
	RETURN

	[TestMethod];
	METHOD AllTrimTest() as void

		local s := e" Hello World\t\r\n" as string

		AreEqual("Hello World",AllTrim(s))
		AreEqual(null,AllTrim(null))

		s:= "Hello World    "
		AreEqual("Hello World",AllTrim(s))
	         	
	RETURN

	[TestMethod];
	METHOD AmPmTest() as void
		var time := "16:55:23"
		AreEqual("04:55:23",AmPm(time))
		time := "26:55:23"
		AreEqual(null,AmPm(time))
		AreEqual(null,AmPm(null))
	RETURN
	
	[TestMethod];
	METHOD AscTest() as void
		local value := " 123" as string
		AreEqual((dword)32,Asc(value))
		AreEqual((dword)32,Asc(" "))
		AreEqual((dword)0,Asc(null))
	RETURN

	[TestMethod];
	METHOD AtTest() as void
		var time := "16:55:23"
		AreEqual((dword)4,At("55",time))
		AreEqual((dword)0,At("55",null))
		AreEqual((dword)0,At(null,time))
	RETURN

	[TestMethod];
	METHOD AtCTest() as void
        AreEqual((dword)7,AtC("World","Hello World"))
	    AreEqual((dword)7,AtC("world","Hello World"))
        AreEqual((dword)0,At("world","Hello World"))
	RETURN

	[TestMethod];
	METHOD At3Test() as void
		var time := "16:25:23"
		AreEqual((dword)7,At3("2",time,5))
		AreEqual((dword)0,At3("2",time,12))

	RETURN

	[TestMethod];
	METHOD CharEvenTest() as void
		AreEqual("1234",CharEven("A1B2C3D4"))
		AreEqual("1234",CharEven("A1B2C3D4E"))
		AreEqual(null,CharEven(null))
	RETURN

	[TestMethod];
	METHOD CharOddTest() as void
		AreEqual("ABCD",CharOdd("A1B2C3D4"))
		AreEqual("ABCDE",CharOdd("A1B2C3D4E"))
		AreEqual(null,CharOdd(null))
	RETURN

	[TestMethod];
	METHOD CharPosTest() as void
		AreEqual("o",CharPos("Hello World",5))
		AreEqual(String.Empty,CharPos("A1B2C3D4E",33))
	RETURN

	END CLASS

END NAMESPACE
