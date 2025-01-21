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
	CLASS StringTests
	[Fact, Trait("Category", "String")];
	METHOD AdjustFNameTest() AS VOID
		Assert.Equal("    xyz   ddss.dbf",AdjustFName("    xyz   ddss    .dbf"))
		Assert.Equal(NULL,AdjustFName(NULL))
		Assert.Equal(" abc ced",AdjustFName(" abc ced   "))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD AllTrimTest() AS VOID

		LOCAL s := e" Hello World  " AS STRING

		Assert.Equal("Hello World",AllTrim(s))
		Assert.Equal(NULL,AllTrim(NULL))

		s:= "Hello World    "
		Assert.Equal("Hello World",AllTrim(s))

	RETURN

	[Fact, Trait("Category", "String")];
	METHOD AscWTest() AS VOID
		LOCAL VALUE := " 123" AS STRING
		Assert.Equal((DWORD)32,AscW(VALUE))
		Assert.Equal((DWORD)955,AscW(e"\x03bb")) // Lower Lambda
		Assert.Equal((DWORD)936,AscW(e"\x03a8")) // Psi
		Assert.Equal((DWORD)32,AscW(" "))
		Assert.Equal((DWORD)512,AscW(((CHAR) 512):ToString()))
		Assert.Equal((DWORD)0,AscW(NULL))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD AscTest() AS VOID
		LOCAL VALUE := " 123" AS STRING
		Assert.Equal((DWORD)32,Asc(VALUE))
		Assert.Equal((DWORD)32,Asc(" "))
		LOCAL nOld AS LONG
		nOld := RuntimeState.WinCodePage
		RuntimeState.WinCodePage := 1253 // Greek
		Assert.Equal((DWORD)235,Asc(e"\x03bb")) // Lower Lambda
		RuntimeState.WinCodePage  := 1252 // Western Europea
		Assert.Equal((DWORD)63,Asc(e"\x03bb"))	// ? because not defined for the codepage
		RuntimeState.WinCodePage  := nOld
		Assert.Equal((DWORD)0,Asc(NULL))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD AtTest() AS VOID
		VAR time := "16:55:23"
		Assert.Equal((DWORD)4,At("55",time))
		Assert.Equal((DWORD)0,At("55",NULL))
		Assert.Equal((DWORD)0,At(NULL,time))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD At2Test() AS VOID
		VAR time := "16:55:23"
		Assert.Equal((DWORD)4,At2("55",time))
		Assert.Equal((DWORD)0,At2("55",NULL))
		Assert.Equal((DWORD)0,At2(NULL,time))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD AtCTest() AS VOID
		Assert.Equal((DWORD)7,AtC("World","Hello World"))
		Assert.Equal((DWORD)7,AtC("world","Hello World"))
		Assert.Equal((DWORD)0,At("world","Hello World"))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD AtC2Test() AS VOID
		Assert.Equal((DWORD)7,AtC2("World","Hello World"))
		Assert.Equal((DWORD)7,AtC2("world","Hello World"))
		Assert.Equal((DWORD)0,At2("world","Hello World"))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD At3Test() AS VOID
		VAR time := "16:25:23"
		Assert.Equal((DWORD)7,At3("2",time,5))
		Assert.Equal((DWORD)0,At3("2",time,12))

	RETURN


	[Fact, Trait("Category", "String")];
	METHOD CharEvenTest() AS VOID
		Assert.Equal("1234",CharEven("A1B2C3D4"))
		Assert.Equal("1234",CharEven("A1B2C3D4E"))
		Assert.Equal(NULL,CharEven(NULL))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD CharOddTest() AS VOID
		Assert.Equal("ABCD",CharOdd("A1B2C3D4"))
		Assert.Equal("ABCDE",CharOdd("A1B2C3D4E"))
		Assert.Equal(NULL,CharOdd(NULL))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD CharPosTest() AS VOID
		Assert.Equal("o",CharPos("Hello World",5))
		Assert.Equal(String.Empty,CharPos("A1B2C3D4E",33))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD HardCRTest() AS VOID
		LOCAL s AS STRING
		s := "aaa" + ((CHAR) 141):ToString() +Chr(10)+ "bbb"
		Assert.Equal(1U, MLCount1(s))
		Assert.Equal(2U, MLCount1(HardCR(s)))


	[Fact, Trait("Category", "String")];
	METHOD InStrTest() AS VOID
		Assert.Equal(TRUE,Instr("o W","Hello World"))
		Assert.Equal(FALSE,Instr("o w","Hello World"))
		Assert.Equal(FALSE,Instr(NULL,NULL))
		Assert.Equal(FALSE,Instr("w",NULL))
		Assert.Equal(FALSE,Instr(NULL,"w"))
	RETURN


	[Fact, Trait("Category", "String")];
	METHOD StringIsTest() AS VOID
	//

	Assert.Equal(TRUE,IsAlpha("A"))
	Assert.Equal(TRUE,IsAlNum("A"))
	Assert.Equal(FALSE,IsAlpha("9"))
	Assert.Equal(TRUE,IsDigit("9"))
	Assert.Equal(TRUE,IsXDigit("9"))
	Assert.Equal(TRUE,IsBDigit("0"))
	Assert.Equal(TRUE,IsBDigit("1"))
	Assert.Equal(FALSE,IsBDigit("2"))
	Assert.Equal(FALSE,IsUpper("2"))
	Assert.Equal(FALSE,IsUpper("a"))
	Assert.Equal(TRUE,IsUpper("A"))
	Assert.Equal(FALSE,IsLower("2"))
	Assert.Equal(FALSE,IsLower("A"))
	Assert.Equal(TRUE,IsLower("a"))
	Assert.Equal(TRUE,IsSpace(" "))
	Assert.Equal(TRUE,IsSpace(e"\t"))
	Assert.Equal(TRUE,IsSpace(e"\r"))
	Assert.Equal(TRUE,IsSpace(e"\v"))
	Assert.Equal(TRUE,IsSpace(e"\n"))
	Assert.Equal(FALSE,IsSpace("A"))
	Assert.Equal(FALSE,IsSpace(""))
	Assert.Equal(FALSE,IsSpace(NULL))

#pragma warnings(165, off)
	[Fact, Trait("Category", "String")];
	METHOD LeftTest() AS VOID

		LOCAL s := "Hello World" AS STRING
		LOCAL unassigned		 AS STRING

		Assert.Equal("",Left(s,0))
		Assert.Equal("H",Left(s,1))
		Assert.Equal("Hello World",Left(s,99))

		Assert.Equal(NULL,Left(unassigned,0))
		Assert.Equal(NULL,Left(unassigned,1))
		Assert.Equal(NULL,Left(unassigned,99))

	RETURN
#pragma warnings(165, default)
	[Fact, Trait("Category", "String")];
	METHOD LowerTest() AS VOID
		Assert.Equal("hello world",Lower("Hello World"))
		Assert.Equal(NULL,Lower(NULL))
		VAR s := "Hello World"
		LowerA(REF s)
		Assert.Equal("hello world",s)
	RETURN


	[Fact, Trait("Category", "String")];
	METHOD LenTest() AS VOID
		Assert.Equal(0U, SLen(""))
		Assert.Equal(0U, SLen(NULL))
		Assert.Equal(1U, SLen("x"))
		Assert.Equal(5U, SLen("12345"))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD LTrimTest() AS VOID
		Assert.Equal("Hello World",LTrim("    Hello World"))
		Assert.Equal(NULL,Lower(NULL))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD OccursTest() AS VOID
		Assert.Equal((DWORD)2,Occurs("or","the world according to me"))
		Assert.Equal((DWORD)0,Occurs(NULL,NULL))
		Assert.Equal((DWORD)0,Occurs("x",NULL))
		Assert.Equal((DWORD)0,Occurs(NULL,"xx"))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD Occurs3Test() AS VOID
		Assert.Equal((DWORD)1,Occurs3("or","the world according to me",7))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD ProperTest() AS VOID
		Assert.Equal("Hello World",Proper("hello world"))
		Assert.Equal(NULL,Proper(NULL))
		Assert.Equal(Proper("1st characters are in uppercase"), "1st Characters Are In Uppercase")

	RETURN

	[Fact, Trait("Category", "String")];
	METHOD RAtTest() AS VOID
		Assert.Equal((DWORD)14,RAt("or","the world according to me"))
		Assert.Equal((DWORD)0,RAt(NULL,NULL))
		Assert.Equal((DWORD)0,RAt("or",NULL))
		Assert.Equal((DWORD)0,RAt(NULL,"the world"))
	RETURN


	[Fact, Trait("Category", "String")];
	METHOD RAtLineTest() AS VOID
	  LOCAL cSource AS STRING

	  cSource := e"This is the first line\r\nThis is the second line\r\nThis is the third line"

	  Assert.Equal(3U, RAtLine("line", cSource) )
	  Assert.Equal(2U, RAtLine("second line", cSource) )

	  Assert.Equal(3U, RATLine2("line", cSource) )
	  Assert.Equal(2U, RATLine2("second line", cSource) )
	  RETURN

	[Fact, Trait("Category", "String")];
	METHOD RAt2Test() AS VOID
		Assert.Equal((DWORD)14,RAt2("or","the world according to me"))
		Assert.Equal((DWORD)0,RAt2(NULL,NULL))
		Assert.Equal((DWORD)0,RAt2("or",NULL))
		Assert.Equal((DWORD)0,RAt2(NULL,"the world"))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD RAt3Test() AS VOID
		Assert.Equal((DWORD)14,RAt3("or","the world according to me",9))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD ReplicateTest() AS VOID
		Assert.Equal("dudadudaduda",Replicate("duda",3))
		Assert.Equal("dudadudaduda",Repl("duda",3))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD RTrimTest() AS VOID
		Assert.Equal("    Hello World",RTrim("    Hello World     "))
		Assert.Equal(NULL,RTrim(NULL))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD RightTest() AS VOID
		Assert.Equal("World",Right("Hello World",5))
		Assert.Equal(NULL,Right(NULL,0))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD SCloneTest() AS VOID
		LOCAL s:="Hello World" AS STRING
		Assert.Equal(s,SClone(s))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD SLenTest() AS VOID
		LOCAL s:="Hello World" AS STRING
		VAR l := SLen(s)
		Assert.Equal((DWORD)11,l)
	RETURN

    /*
	[Fact, Trait("Category", "String")];
	METHOD SLenExceptionTest() AS VOID
		LOCAL s AS STRING
		s:=null
		XUnit.Assert.Throws<InvalidOperationException>( {  => SLen(s)} )
	RETURN
    */
	[Fact, Trait("Category", "String")];
	METHOD StuffTest() AS VOID
		LOCAL s:="Hello World" AS STRING
		Assert.Equal("Hello Kiel",Stuff(s,7,5,"Kiel"))
		Assert.Equal("Kiel",Stuff(NULL,7,5,"Kiel"))
		Assert.Equal("Hello ",Stuff(s,7,5,""))
		Assert.Equal("Hello WorldKiel",Stuff(s,12,5,"Kiel"))
	RETURN

	[Fact, Trait("Category", "String")];
	METHOD SubStr2Test() AS VOID
		LOCAL s:="Hello World" AS STRING
		Assert.Equal("World",SubStr2(s,7))
		Assert.Equal("",SubStr2(s,20))
		Assert.Equal(NULL,SubStr2(NULL,5))
	RETURN
	[Fact, Trait("Category", "String")];
	METHOD SubStr3Test() AS VOID
		LOCAL s:="Hello World" AS STRING
		Assert.Equal("World",SubStr3(s,7,5))
		Assert.Equal("",SubStr3(s,20,5))
		Assert.Equal(NULL,SubStr3(NULL,5,2))
	RETURN
	[Fact, Trait("Category", "String")];
	METHOD TrimTest() AS VOID
		LOCAL s:="Hello World   " AS STRING
		Assert.Equal("Hello World",Trim(s))
		Assert.Equal(NULL,Trim(NULL))
	RETURN
	[Fact, Trait("Category", "String")];
	METHOD UpperTest() AS VOID
		LOCAL s:="Hello World" AS STRING
		Assert.Equal("HELLO WORLD",Upper(s))
		Assert.Equal(NULL,Upper(NULL))
		UpperA(REF s)
		Assert.Equal("HELLO WORLD",s)
	RETURN

   [Fact, Trait("Category", "String")];
   METHOD SoundExTest() AS VOID
	  Assert.Equal("F631", SoundEx("Fred Bloggs")  )
	  Assert.Equal("S530", SoundEx("Smith")  )
	  Assert.Equal("S530", SoundEx("Smythe") )

	  RETURN

   [Fact, Trait("Category", "String")];
   METHOD CharMixTest() AS VOID
	  Assert.Equal("1234567890", CharMix("13579","24680"))
	  Assert.Equal("", CharMix("","24680"))
	  Assert.Equal("12", CharMix("1","24680"))
	  Assert.Equal("", CharMix("13579",""))
	  Assert.Equal("1232527292", CharMix("13579","2"))

	  RETURN
   [Fact, Trait("Category", "Memo")];
   METHOD AtLineTest() AS VOID
	  LOCAL test AS STRING
	  test := e"first line\r\nsecond line\r\nthird line"
	  Assert.Equal(1U, ATLine("first", test))
	  Assert.Equal(2U, ATLine("second", test))
	  Assert.Equal(3U, ATLine("third", test))
	  Assert.Equal(0U, ATLine("fourth", test))
	  Assert.Equal(1U, ATCLine("First", test))
	  Assert.Equal(2U, ATCLine("Second", test))
	  Assert.Equal(3U, ATCLine("Third", test))
	  Assert.Equal(0U, ATCLine("Fourth", test))

	  RETURN

   [Fact, Trait("Category", "Memo")];
   METHOD MemLinesTest() AS VOID
	  LOCAL test AS STRING
	  test := e"first line\r\nsecond line\r\nthird line"
	  Assert.Equal(3U, MemLines(test))
	  Assert.Equal(3U, MLCount1(test))
	  Assert.Equal(0U, MemLines(""))
	  Assert.Equal(1U, MemLines(" "))
	  Assert.Equal(0U, MLCount1(""))
	  Assert.Equal(1U, MLCount1(" "))
	  Assert.Equal(0U, MemLines(NULL))
	  Assert.Equal(0U, MLCount1(NULL))
	  RETURN
   [Fact, Trait("Category", "String")];
   METHOD LikeTest() AS VOID
        LOCAL search AS STRING
        var dia := RuntimeState.Dialect
        RuntimeState.Dialect := XSharpDialect.Core
	  search := "file*.txt"
	  Assert.Equal(true, _Like(search,"file.txt"))
	  Assert.Equal(true, _Like(search,"file1.txt"))
	  Assert.Equal(true, _Like(search,"file2a.txt"))
	  Assert.Equal(true, _Like(search,"file*.txt"))
	  Assert.Equal(false, _Like(search,"gile.txt")) // first char different
	  Assert.Equal(false, _Like(search,"File.txt")) // first char case different
	  Assert.Equal(true, Like(search,"File.txt"))
	  Assert.Equal(true, Like(search,"File1.txt"))
	  Assert.Equal(true, Like(search,"File2a.txt"))
	  Assert.Equal(true, Like(search,"File*.txt"))
	  Assert.Equal(false, Like(search,"gile.txt"))
	  search := "file?.txt"
	  Assert.Equal(false, _Like(search,"file.txt"))  // too short
	  Assert.Equal(true, _Like(search,"file1.txt"))
	  Assert.Equal(false, _Like(search,"file2a.txt")) // too long
        Assert.Equal(true, _Like(search,"file*.txt"))
        RuntimeState.Dialect := dia
	  RETURN


   [Fact, Trait("Category", "Memo")];
	METHOD MemoReadWriteTest() AS VOID
		LOCAL sToWrite := "test" AS STRING
        LOCAL cFileName :=TempFile("txt") AS STRING
		MemoWrit(cFileName, sToWrite)
		VAR sText := MemoRead(cFileName)
		Assert.Equal(sToWrite, sText)
        FErase(cFileName)


   [Fact, Trait("Category", "Conversion")];
	METHOD Ansi2OemTest() AS VOID
        XSharp.RuntimeState.DosCodePage := 437
        XSharp.RuntimeState.WinCodePage := 1252
        LOCAL cSource AS STRING
        LOCAL cTarget AS STRING
        cSource := "����������"
        cTarget := e"\u017d\u0045\u0049\u2122\u0161\u201e\u2030\u2039\u201d\u0081"
        Assert.Equal(cTarget, Ansi2Oem(cSource))
        cSource := "�EI�������"     // there are no E and I with umlaut in codepage 437
        Assert.Equal(cSource, Oem2Ansi(cTarget))
        Assert.Equal(cSource, Oem2Ansi(Ansi2Oem(cSource)))


END CLASS
END NAMESPACE
