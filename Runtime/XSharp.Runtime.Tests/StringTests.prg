USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using XSharp.Runtime

BEGIN NAMESPACE XSharp.Runtime.Tests

	CLASS StringTests

	[Fact, Trait("Category", "String")];
	METHOD AdjustFNameTest() as void
	    Assert.Equal("    xyz   ddss.dbf",AdjustFName("    xyz   ddss    .dbf"))
	    Assert.Equal(null,AdjustFName(null))
		Assert.Equal(" abc ced",AdjustFName(" abc ced   "))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD AllTrimTest() as void

		local s := e" Hello World\t\r\n" as string

		Assert.Equal("Hello World",AllTrim(s))
		Assert.Equal(null,AllTrim(null))

		s:= "Hello World    "
		Assert.Equal("Hello World",AllTrim(s))
	         	
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD AmPmTest() as void
		var time := "16:55:23"
		Assert.Equal("04:55:23",AmPm(time))
		time := "26:55:23"
		Assert.Equal(null,AmPm(time))
		Assert.Equal(null,AmPm(null))
	RETURN
	
	[Fact, Trait("Category", "String")];
	METHOD AscTest() as void
		local value := " 123" as string
		Assert.Equal((dword)32,Asc(value))
		Assert.Equal((dword)32,Asc(" "))
		Assert.Equal((dword)0,Asc(null))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD AtTest() as void
		var time := "16:55:23"
		Assert.Equal((dword)4,At("55",time))
		Assert.Equal((dword)0,At("55",null))
		Assert.Equal((dword)0,At(null,time))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD AtCTest() as void
        Assert.Equal((dword)7,AtC("World","Hello World"))
	    Assert.Equal((dword)7,AtC("world","Hello World"))
        Assert.Equal((dword)0,At("world","Hello World"))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD At3Test() as void
		var time := "16:25:23"
		Assert.Equal((dword)7,At3("2",time,5))
		Assert.Equal((dword)0,At3("2",time,12))

	RETURN

	[Fact, Trait("Category", "String")];
	METHOD CharEvenTest() as void
		Assert.Equal("1234",CharEven("A1B2C3D4"))
		Assert.Equal("1234",CharEven("A1B2C3D4E"))
		Assert.Equal(null,CharEven(null))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD CharOddTest() as void
		Assert.Equal("ABCD",CharOdd("A1B2C3D4"))
		Assert.Equal("ABCDE",CharOdd("A1B2C3D4E"))
		Assert.Equal(null,CharOdd(null))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD CharPosTest() as void
		Assert.Equal("o",CharPos("Hello World",5))
		Assert.Equal(String.Empty,CharPos("A1B2C3D4E",33))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD InStrTest() as void
		Assert.Equal(true,Instr("o W","Hello World"))
		Assert.Equal(false,Instr("o w","Hello World"))
		Assert.Equal(false,Instr(null,null))
		Assert.Equal(false,Instr("w",null))
		Assert.Equal(false,Instr(null,"w"))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD LeftTest() as void

		local s := "Hello World" as string
		local unassigned		 as string		                  
   			                  
		Assert.Equal("",left(s,0))
		Assert.Equal("H",left(s,1))
		Assert.Equal("Hello World",left(s,99))

		Assert.Equal(null,left(unassigned,0))
		Assert.Equal(null,left(unassigned,1))
		Assert.Equal(null,left(unassigned,99))
	         	
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD LowerTest() as void
		Assert.Equal("hello world",Lower("Hello World"))
		Assert.Equal(null,Lower(null))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD LTrimTest() as void
		Assert.Equal("Hello World",LTrim("    Hello World"))
		Assert.Equal(null,Lower(null))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD OccursTest() as void
		Assert.Equal((dword)2,Occurs("or","the world according to me"))
		Assert.Equal((dword)0,Occurs(null,null))
		Assert.Equal((dword)0,Occurs("x",null))
		Assert.Equal((dword)0,Occurs(null,"xx"))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD Occurs3Test() as void
		Assert.Equal((dword)1,Occurs3("or","the world according to me",7))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD ProperTest() as void
		Assert.Equal("Hello World",Proper("hello world"))
		Assert.Equal(null,Proper(null))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD RAtTest() as void
		Assert.Equal((dword)14,RAt("or","the world according to me"))
		Assert.Equal((dword)0,Rat(null,null))
		Assert.Equal((dword)0,Rat("or",null))
		Assert.Equal((dword)0,Rat(null,"the world"))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD RAt3Test() as void
		Assert.Equal((dword)14,RAt3("or","the world according to me",9))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD ReplicateTest() as void
		Assert.Equal("dudadudaduda",Replicate("duda",3))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD RTrimTest() as void
		Assert.Equal("    Hello World",RTrim("    Hello World     "))
		Assert.Equal(null,RTrim(null))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD RightTest() as void
		Assert.Equal("World",Right("Hello World",5))
		Assert.Equal(null,Right(null,0))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD SCloneTest() as void
		local s:="Hello World" as string
		Assert.Equal(s,SClone(s))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD SLenTest() as void
		local s:="Hello World" as string
		var l := SLen(s)
		Assert.Equal((dword)11,SLen(s))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD SLenExceptionTest() as void
		local s as string
		s:=string.Empty
		//XUnit.Assert.Throws<InvalidOperationException>( () => SLen(s))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD StuffTest() as void
		local s:="Hello World" as string
		Assert.Equal("Hello Kiel",Stuff(s,7,5,"Kiel"))
		Assert.Equal("Kiel",Stuff(null,7,5,"Kiel"))
		Assert.Equal("Hello ",Stuff(s,7,5,""))
		Assert.Equal("Hello WorldKiel",Stuff(s,12,5,"Kiel"))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD SubStr2Test() as void
		local s:="Hello World" as string
		Assert.Equal("World",SubStr2(s,7))
		Assert.Equal("",SubStr2(s,20))
		Assert.Equal(null,SubStr2(null,5))
	RETURN
	[Fact, Trait("Category", "String")];
	METHOD SubStr3Test() as void
		local s:="Hello World" as string
		Assert.Equal("World",SubStr3(s,7,5))
		Assert.Equal("",SubStr3(s,20,5))
		Assert.Equal(null,SubStr3(null,5,2))
	RETURN
	[Fact, Trait("Category", "String")];
	METHOD TrimTest() as void
		local s:="Hello World   " as string
		Assert.Equal("Hello World",Trim(s))
		Assert.Equal(null,Trim(null))
	RETURN
	[Fact, Trait("Category", "String")];
	METHOD UpperTest() as void
		local s:="Hello World" as string
		Assert.Equal("HELLO WORLD",Upper(s))
		Assert.Equal(null,Upper(null))
	RETURN
	END CLASS

END NAMESPACE
