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

	[TestMethod];
	METHOD InStrTest() as void
		AreEqual(true,Instr("o W","Hello World"))
		AreEqual(false,Instr("o w","Hello World"))
		AreEqual(false,Instr(null,null))
		AreEqual(false,Instr("w",null))
		AreEqual(false,Instr(null,"w"))
	RETURN

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
	METHOD LowerTest() as void
		AreEqual("hello world",Lower("Hello World"))
		AreEqual(null,Lower(null))
	RETURN

	[TestMethod];
	METHOD LTrimTest() as void
		AreEqual("Hello World",LTrim("    Hello World"))
		AreEqual(null,Lower(null))
	RETURN

	[TestMethod];
	METHOD OccursTest() as void
		AreEqual((dword)2,Occurs("or","the world according to me"))
		AreEqual((dword)0,Occurs(null,null))
		AreEqual((dword)0,Occurs("x",null))
		AreEqual((dword)0,Occurs(null,"xx"))
	RETURN

	[TestMethod];
	METHOD Occurs3Test() as void
		AreEqual((dword)1,Occurs3("or","the world according to me",7))
	RETURN

	[TestMethod];
	METHOD ProperTest() as void
		AreEqual("Hello World",Proper("hello world"))
		AreEqual(null,Proper(null))
	RETURN

	[TestMethod];
	METHOD RAtTest() as void
		AreEqual((dword)14,RAt("or","the world according to me"))
		AreEqual((dword)0,Rat(null,null))
		AreEqual((dword)0,Rat("or",null))
		AreEqual((dword)0,Rat(null,"the world"))
	RETURN

	[TestMethod];
	METHOD RAt3Test() as void
		AreEqual((dword)14,RAt3("or","the world according to me",9))
	RETURN

	[TestMethod];
	METHOD ReplicateTest() as void
		AreEqual("dudadudaduda",Replicate("duda",3))
	RETURN

	[TestMethod];
	METHOD RTrimTest() as void
		AreEqual("    Hello World",RTrim("    Hello World     "))
		AreEqual(null,RTrim(null))
	RETURN

	[TestMethod];
	METHOD RightTest() as void
		AreEqual("World",Right("Hello World",5))
		AreEqual(null,Right(null,0))
	RETURN

	[TestMethod];
	METHOD SCloneTest() as void
		local s:="Hello World" as string
		AreEqual(s,SClone(s))
	RETURN

	[TestMethod];
	METHOD SLenTest() as void
		local s:="Hello World" as string
		var l := SLen(s)
		AreEqual((dword)11,SLen(s))
	RETURN

	[TestMethod];
	[ExpectedException(typeof(InvalidOperationException))];
	METHOD SLenExceptionTest() as void
		local s as string
		s:=null_string
		AreEqual((dword)0,SLen(s))
	RETURN

	[TestMethod];
	METHOD StuffTest() as void
		local s:="Hello World" as string
		AreEqual("Hello Kiel",Stuff(s,7,5,"Kiel"))
		AreEqual("Kiel",Stuff(null,7,5,"Kiel"))
		AreEqual("Hello ",Stuff(s,7,5,""))
		AreEqual("Hello WorldKiel",Stuff(s,12,5,"Kiel"))
	RETURN

	[TestMethod];
	METHOD SubStr2Test() as void
		local s:="Hello World" as string
		AreEqual("World",SubStr2(s,7))
		AreEqual("",SubStr2(s,20))
		AreEqual(null,SubStr2(null,5))
	RETURN
	[TestMethod];
	METHOD SubStr3Test() as void
		local s:="Hello World" as string
		AreEqual("World",SubStr3(s,7,5))
		AreEqual("",SubStr3(s,20,5))
		AreEqual(null,SubStr3(null,5,2))
	RETURN
	[TestMethod];
	METHOD TrimTest() as void
		local s:="Hello World   " as string
		AreEqual("Hello World",Trim(s))
		AreEqual(null,Trim(null))
	RETURN
	[TestMethod];
	METHOD UpperTest() as void
		local s:="Hello World" as string
		AreEqual("HELLO WORLD",Upper(s))
		AreEqual(null,Upper(null))
	RETURN
	END CLASS

END NAMESPACE
