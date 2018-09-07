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
USING System.Globalization


BEGIN NAMESPACE XSharp.VO.Tests

	CLASS TransFormTests

		[Fact, Trait("Category", "TransForm")];
		METHOD TransformLogicalTest() AS VOID 
			// with @R other chars are appended, but only a single YN or TF flag is written. Other positions become a space
			Assert.Equal("Y", Transform(TRUE, "Y"))
			Assert.Equal("N", Transform(FALSE, "Y"))
			Assert.Equal("T", Transform(TRUE, "L"))
			Assert.Equal("F", Transform(FALSE, "L"))
			Assert.Equal("T x", Transform(TRUE, "@R L x"))
			Assert.Equal("F x", Transform(FALSE, "@R L x"))
			Assert.Equal("Y x", Transform(TRUE, "@R YYx"))
			Assert.Equal("N x", Transform(FALSE, "@R YYx"))
			SetNatDll("French.DLL")
			Assert.Equal("O", Transform(TRUE, "Y"))
			Assert.Equal("N", Transform(FALSE, "Y"))
			Assert.Equal("T", Transform(TRUE, "L"))     // you would expect V but VO returns T
			Assert.Equal("F", Transform(FALSE, "L"))
			Assert.Equal("T x", Transform(TRUE, "@R L x"))  // you would expect V but VO returns T x
			Assert.Equal("F x", Transform(FALSE, "@R L x"))
			Assert.Equal("O x", Transform(TRUE, "@R YYx"))
			Assert.Equal("N x", Transform(FALSE, "@R YYx"))

		[Fact, Trait("Category", "TransForm")];
		METHOD TransformDateTest() AS VOID 
			// with @R other chars are appended, but only a single YN or TF flag is written. Other positions become a space
            SetCentury(TRUE)
			Assert.Equal("10/12/2010", Transform(2010.12.10,"@E"))
			Assert.Equal("20/11/2010", Transform(2010.11.20,"@E"))
			SetDateFormat("dd-mm-yyyy")
			Assert.Equal("20-11-2010", Transform(2010.11.20,"@D"))
			SetDateFormat("dd-mm-yy")
			Assert.Equal("20-11-10", Transform(2010.11.20,"@D"))

		[Fact, Trait("Category", "TransForm")];
		METHOD TransformStringTest() AS VOID 
			Assert.Equal("ABC", Transform("abc", "@!"))
			Assert.Equal("AbC", Transform("abc", "!a!"))
			Assert.Equal("1234 AB", Transform("1234ab", "@R! 9999 AA"))
			Assert.Equal("1234 AB", Transform("1234ab", "@R 9999 !!"))

		[Fact, Trait("Category", "TransForm")];
		METHOD TransformLongTest() AS VOID 
			SetDecimalSep('.')
			SetThousandSep(',')
			Assert.Equal("  1", Transform(1, "999"))
			Assert.Equal("123", Transform(123, "###"))
			Assert.Equal("**", Transform(123, "##"))			
			Assert.Equal("**", Transform(123, "99"))			
			Assert.Equal("(1)", Transform(-1, "@( 99"))
			Assert.Equal("(-1)", Transform(-1, "@( 999"))
			Assert.Equal("( -1)", Transform(-1, "@( 9999"))
			Assert.Equal("(-1)", Transform(-1, "@) 9999"))
			Assert.Equal(" (-1)", Transform(-1, "@) 99999"))
			Assert.Equal("   ", Transform(0, "@Z 999"))
			Assert.Equal(" -1 DB", Transform(-1, "@X 999"))
			Assert.Equal(" -1 DB", Transform(-1, "@XC 999"))
			Assert.Equal("  1 CR", Transform(1, "@XC 999"))
			Assert.Equal("  1 CR", Transform(1, "@C 999"))
			Assert.Equal("  0", Transform(0, "@C 999"))
			Assert.Equal("1,234", Transform(1234, "9,999"))
			SetDecimalSep(',')
			SetThousandSep('.')
			Assert.Equal("1.234", Transform(1234, "9,999"))


		[Fact, Trait("Category", "TransForm")];
		METHOD TransformFLoatTest() AS VOID 
			SetDecimalSep('.')
			SetThousandSep(',')
			Assert.Equal("1.23", Transform(1.23, "9.99"))
			Assert.Equal("1,23", Transform(1.23, "@E 9.99"))
			Assert.Equal("*.**", Transform(12.34, "9.99"))
        [Fact, Trait("Category", "TransForm")];
        METHOD TransformNumericTest AS VOID
			SetDecimalSep(',')
			SetThousandSep('.')
            Assert.Equal("      2,45", Transform (    2.45 , "@R 999,999.99" ) )
            Assert.Equal("     -2,45", Transform (   -2.45 , "@R 999,999.99" ) )
            Assert.Equal("    124,00", Transform (     124 , "@R 999,999.99" )  )  
            Assert.Equal("   -124,00", Transform (    -124 , "@R 999,999.99" )  ) 

        [Fact, Trait("Category", "Unformat")];
        METHOD UnformatCTest AS VOID
            Assert.Equal("8161XV", Unformat("8161 XV", "@R! 9999 AA","C"))
            Assert.Equal("8161 XV", Unformat("8161 XV", "@! 9999 AA","C"))

       [Fact, Trait("Category", "Unformat")];
        METHOD UnformatNTest AS VOID
			SetDecimalSep(',')
			SetThousandSep('.')
            Assert.Equal( 1.234d, (REAL8)(FLOAT) Unformat("1,234", "9.999","N"))
            Assert.Equal( 1234567.89d, (REAL8)(FLOAT) Unformat("1.234.567,89", "9,999,999.99","N"))


// some new failing tests:
	[Fact, Trait("Category", "TransForm")];
	METHOD IntLiteralTest() AS VOID
		LOCAL dec AS DWORD
		LOCAL thou AS DWORD

		dec := SetDecimalSep(Asc(","))
		thou := SetThousandSep(Asc("."))
		Assert.Equal(" 12.345,00" , Transform(12345.0, "999,999.99"))
		// this fails:
		Assert.Equal(" 12.345,00" , Transform(12345, "999,999.99"))
        
		SetDecimalSep(Asc("."))
		SetThousandSep(Asc(","))
		Assert.Equal(" 12,345.00" , Transform(12345.0, "999,999.99"))
		Assert.Equal(" 12,345.00" , Transform(12345, "999,999.99"))

		SetDecimalSep(dec)
		SetThousandSep(thou)
	RETURN



// Tests that need to be moved in separate files in VO.Tests:
	[Fact, Trait("Category", "StringEq")];
	METHOD StringEquality() AS VOID
		LOCAL c1,c2 AS STRING
		LOCAL u1,u2 AS USUAL
		LOCAL exact AS LOGIC
		u1 := c1 := "TESTLONG"
		u2 := c2 := "TEST"
		exact := SetExact( FALSE )
		
		Assert.Equal(TRUE, c1 = c2)
		Assert.Equal(TRUE, c1 != c2)

		Assert.Equal(TRUE, u1 = u2)
		Assert.Equal(FALSE, u1 != u2)

		SetExact( exact )	
	RETURN

	[Fact, Trait("Category", "Str")];
	METHOD StrTests() AS VOID
		LOCAL exact,thou,digit,decimal,fixed_,digitfixed AS USUAL
		exact := SetDecimalSep(Asc("."))
		thou := SetThousandSep(Asc(","))
		digit := SetDigit(6)
		decimal := SetDecimal(3)
		fixed_ := SetFixed(FALSE)
		digitfixed := SetDigitFixed(FALSE)

		Assert.Equal( "     1.2"			, Str(1.2) )
		Assert.Equal( "    12.3"			, Str(12.3) )
		Assert.Equal( "    12.34"			, Str(12.34) )
		Assert.Equal( "   123.456"			, Str(123.456) )
		Assert.Equal( "  1234.567"			, Str(1234.567) )
		Assert.Equal( "  1234.5678"			, Str(1234.5678) )
		Assert.Equal( "  1234.5679012345"	, Str(1234.5679012345) )
		Assert.Equal( "123456.78901234568"	, Str(123456.78901234567) )
		Assert.Equal( "999999" , Str(999999) )
		Assert.Equal( "******" , Str(1000000) )

		SetDecimalSep(exact)
		SetThousandSep(thou)
		SetDigit(digit)
		SetDecimal(decimal)
		SetFixed(fixed_)
		SetDigitFixed(digitfixed)
	RETURN

	[Fact, Trait("Category", "Str")];
	METHOD Str_With_SetFixed_TRUE() AS VOID

	   SetDecimalSep(Asc(".")) 
	   SetThousandSep(Asc(",")) 
	   SetDecimal(3)
	   SetDigit(12)

	   SetFixed(TRUE)
	   SetDigitFixed(TRUE)
   
	   Assert.Equal( Str(12.34)            		, "          12.340")
	   Assert.Equal( Str1(12.34)          		, "          12.340")
	   Assert.Equal( Str(123.456)         		, "         123.456")
	   Assert.Equal( Str1(123.456)       		, "         123.456")
	   Assert.Equal( Str(1230.4567)         	, "        1230.457")
	   Assert.Equal( Str1(1230.4567)        	, "        1230.457")
	   Assert.Equal( Str(100000001234.4567)		, "100000001234.457")
	   Assert.Equal( Str1(100000001234.4567)	, "100000001234.457")
	   Assert.Equal( Str(1000000001234.4567)	, "****************")
	   Assert.Equal( Str1(1000000001234.4567)	, "****************")
	   Assert.Equal( Str(12.3)              	, "          12.300")
	   Assert.Equal( Str1(12.3)            		, "          12.300")
	   Assert.Equal( Str(12.34567)        	 	, "          12.346")
	   Assert.Equal( Str1(12.34567)        		, "          12.346")
	   Assert.Equal( Str(12.34 , 10, 2)    		, "     12.34")
	   Assert.Equal( Str(12.34 , 5, 2)     		, "12.34")
	   Assert.Equal( Str(12.34 , 4, 2)     		, "****") // VO wrong: 12.3
	   Assert.Equal( Str(12.34 , 4, 1)     		, "12.3")
	   Assert.Equal( Str(12.34 , 2, 1)     		, "**")
	   Assert.Equal( Str3(12.34 , 10, 2)   		, "     12.34")
	   Assert.Equal( Str3(12.34 , 5, 2)    		, "12.34")
	   Assert.Equal( Str3(12.34 , 4, 2)     	, "****") // VO wrong: 12.3
	   Assert.Equal( Str3(12.34 , 4, 1)     	, "12.3")
	   Assert.Equal( Str3(12.34 , 2, 1)    		, "**")
	   Assert.Equal( Str(-12.34)            	, "         -12.340")
	   Assert.Equal( Str1(-12.34)           	, "         -12.340")
	   Assert.Equal( Str(-12.3456)         		, "         -12.346")
	   Assert.Equal( Str1(-12.3456)         	, "         -12.346")
	   Assert.Equal( Str(-12.34 , 10, 2)    	, "    -12.34")
	   Assert.Equal( Str(-12.34 , 5, 2)     	, "*****")	 //VO wrong: -2.34
	   Assert.Equal( Str(-12.34 , 4, 2)     	, "****")    // VO wrong: -12.
	   Assert.Equal( Str(-12.34 , 4, 1)     	, "****")    //VO wrong: -2.3
       Assert.Equal( Str(-12.34 , 4, 0)     	, " -12")    //VO wrong: -2.3
	   Assert.Equal( Str(-12.34 , 2, 1)     	, "**")
	RETURN

	[Fact, Trait("Category", "Str")];
	METHOD Str_With_SetFixed_FALSE() AS VOID

	   SetDecimalSep(Asc(".")) 
	   SetThousandSep(Asc(",")) 
	   SetDecimal(3)
	   SetDigit(12)

	   SetFixed(FALSE)
	   SetDigitFixed(FALSE) 
   
	   Assert.Equal( Str(12.34)            		, "          12.34")
	   Assert.Equal( Str1(12.34)          		, "          12.34")
	   Assert.Equal( Str(123.456)         		, "         123.456")
	   Assert.Equal( Str1(123.456)				, "         123.456")
	   Assert.Equal( Str(1230.4567)         	, "        1230.4567")
	   Assert.Equal( Str1(1230.4567)        	, "        1230.4567")
	   Assert.Equal( Str(100000001234.4567)		, "100000001234.4567")
	   Assert.Equal( Str1(100000001234.4567)	, "100000001234.4567")
	   Assert.Equal( Str(1000000001234.4567)	, "*****************")
	   Assert.Equal( Str1(1000000001234.4567)	, "*****************")
	   Assert.Equal( Str(12.3)              	, "          12.3")
	   Assert.Equal( Str1(12.3)            		, "          12.3")
	   Assert.Equal( Str(12.34567)         		, "          12.34567")
	   Assert.Equal( Str1(12.34567)        		, "          12.34567")
	   Assert.Equal( Str(12.34 , 10, 2)    		, "     12.34")
	   Assert.Equal( Str(12.34 , 5, 2)     		, "12.34")
	   Assert.Equal( Str(12.34 , 4, 2)     		, "****") // VO returns 12.3
	   Assert.Equal( Str(12.34 , 4, 1)     		, "12.3")
	   Assert.Equal( Str(12.34 , 2, 1)     		, "**")
	   Assert.Equal( Str3(12.34 , 10, 2)   		, "     12.34")
	   Assert.Equal( Str3(12.34 , 5, 2)    		, "12.34")
	   Assert.Equal( Str3(12.34 , 4, 2)     	, "****")   // VO 12.3
	   Assert.Equal( Str3(12.34 , 4, 1)     	, "12.3")
	   Assert.Equal( Str3(12.34 , 2, 1)    		, "**")
	   Assert.Equal( Str(-12.34)            	, "         -12.34")
	   Assert.Equal( Str1(-12.34)           	, "         -12.34")
	   Assert.Equal( Str(-12.3456)         		, "         -12.3456")
	   Assert.Equal( Str1(-12.3456)         	, "         -12.3456")
	   Assert.Equal( Str(-12.34 , 10, 2)    	, "    -12.34")
	   Assert.Equal( Str(-12.34 , 5, 2)     	, "*****")	 //VO: -2.34
	   Assert.Equal( Str(-12.34 , 4, 2)     	, "****")    // VO 
	   Assert.Equal( Str(-12.34 , 4, 1)     	, "****")    //VO: -2.3
	   Assert.Equal( Str(-12.34 , 2, 1)     	, "**")


	[Fact, Trait("Category", "Val")];
	METHOD ValTests() AS VOID
		SetDecimalSep(',')
		SetThousandSep('.')
		Assert.Equal(123, (INT) Val("123") )
		Assert.Equal(123.456, (FLOAT) Val("123,456") )

		Assert.Equal(0, (INT) Val("") )

		Assert.Equal(0, (INT) Val("abc") )
		Assert.Equal(123, (INT) Val("123abc") )
		Assert.Equal(123, (INT) Val("123abc456") )
		Assert.Equal(123, (INT) Val("123abc456,789") )
		Assert.Equal(0, (INT) Val("abc123456,789") )
		Assert.Equal(255, (INT) Val("0xFF") )
		Assert.Equal(0xFFFF, (INT) Val("0xFFFF") )
		Assert.Equal(4294967295, (INT64) Val("0xFFFFFFFF") )
		Assert.Equal(11, (INT) Val("11L11") )
        
		Assert.Equal(1.0001, (FLOAT) Val("1,000.1") )
		Assert.Equal(1.0011, (FLOAT) Val("1,001.1") )
		SetDecimalSep('.')
		SetThousandSep(',')
		Assert.Equal(1000.1, (FLOAT) Val("1,000.1") )
		Assert.Equal(1001.1, (FLOAT) Val("1,001.1") )
	RETURN

	[Fact, Trait("Category", "SplitPath")];
	METHOD SplitPathTests() AS VOID
		LOCAL cDrive := "",cDir := "",cFile := "",cExt := "" AS STRING
		SplitPath("C:\folder\file.ext" ,  REF cDrive , REF cDir , REF cFile , REF cExt)
		
		Assert.Equal("C:" , cDrive)
		Assert.Equal("\folder\" , cDir)

		Assert.Equal("file" , cFile)
		Assert.Equal(".ext" , cExt)
	RETURN

	[Fact, Trait("Category", "SplitPathVO")];
	METHOD SplitPathVO() AS VOID
		LOCAL cPath ,cDrive ,cDir ,cFile ,cExt AS STRING
		LOCAL pszDrive, pszDir AS PSZ
		LOCAL pszFile, pszExt AS PSZ
	
		cPath := "C:\testfolder\testfile.prg"
	
		pszDrive := MemAlloc(2+1)
		pszDir   := MemAlloc(255+1)
		pszFile  := MemAlloc(255+1)
		pszExt   := MemAlloc(7+1) 
	
		SplitPath( String2Psz(cPath), pszDrive, pszDir, pszFile, pszExt)
	
		cDrive := Psz2String(pszDrive)
		cDir   := Psz2String(pszDir)
		cFile  := Psz2String(pszFile)
		cExt   := Psz2String(pszExt)
	
		MemFree(pszDrive)
		MemFree(pszDir)
		MemFree(pszFile)
		MemFree(pszExt)
	
		Assert.Equal("\testfolder\" , cDir)
		Assert.Equal("\testfolder\" , cDir)
		Assert.Equal("testfile" , cFile)
		Assert.Equal(".prg" , cExt)
	RETURN

	[Fact, Trait("Category", "Directory")];
	METHOD DirectoryTests() AS VOID
		Assert.Equal(0u , ALen(Directory("K:\drive does not exist")))
		Assert.Equal(0u , ALen(Directory("K:\drive does not exist","V")))
		Assert.Equal(0u , ALen(Directory("C:\invalid:path")))
		Assert.Equal(1u , ALen(Directory("C:\invalid:path","V")))
		Assert.Equal(0u , ALen(Directory("C:invalid:path")))
		Assert.Equal(1u , ALen(Directory("C:invalid:path","V")))
		Assert.Equal(0u , ALen(Directory("N:")))
		Assert.Equal(0u , ALen(Directory("N:","V")))

	[Fact, Trait("Category", "Empty")];
	METHOD EmptyFuncTests() AS VOID
		Assert.True(Empty(0))
		Assert.True(Empty(FALSE))
		Assert.True(Empty(""))
		Assert.True(Empty(Chr(10) + Chr(9) + Chr(13)))
		Assert.True(Empty(NULL_SYMBOL))
		Assert.True(Empty(0000.00.00))
		Assert.True(Empty({}))

		Assert.True(Empty(NULL_PTR))
		LOCAL p AS PTR
		p := NULL_PTR
		Assert.True(Empty(p))

		Assert.True(Empty(NULL_PSZ))
		LOCAL z AS PSZ
		z := NULL_PSZ
		Assert.True(Empty(z))
		
		

		Assert.False(Empty(1))
		Assert.False(Empty(TRUE))
		Assert.False(Empty("a"))
		Assert.False(Empty(Chr(1)))
		Assert.False(Empty(#abc))
		Assert.False(Empty(2000.01.01))
		Assert.False(Empty({{}}))
		
		p := @p
		Assert.False(Empty(p))
		z := String2Psz("test")
		Assert.False(Empty(z))

	[Fact, Trait("Category", "IsClassOf")];
	METHOD IsClassOf_Tests() AS VOID
		Assert.True(IsClassOf(#TestClassChild, #TestClassParent))
		Assert.False(IsClassOf(#TestClassParent, #TestClassChild))
		Assert.True(IsClassOf(#TestClassChild, #TestClassChild))
		Assert.True(IsClassOf(#TestClassParent, #TestClassParent))
		Assert.False(IsClassOf(#None, #None))
		Assert.False(IsClassOf(#None, #TestClassChild))
		Assert.False(IsClassOf(#TestClassChild, #None))

	[Fact, Trait("Category", "IsInstanceOf")];
	METHOD IsInstanceOf_Tests() AS VOID
		Assert.True(IsInstanceOf(123 , "System.Int32"))
		Assert.True(IsInstanceOf(TRUE , "System.Boolean"))
		Assert.False(IsInstanceOf(123 , "Nothing"))

	[Fact, Trait("Category", "AClone")];
	METHOD AClone_Tests() AS VOID
		Assert.True(ALen(AClone({1}))==1)
		Assert.True(ALen(AClone({1,2}))==2)
		Assert.True(  AClone({1,2})[2] ==2  )
		Assert.True(  AClone({1,{2,3},4})[2,2] == 3  )
		Assert.True(  AClone({1,{},4})[3] == 4  )

		Assert.True(AClone(NULL_ARRAY)==NULL_ARRAY)
		LOCAL aNull := NULL_ARRAY AS ARRAY
		Assert.True(AClone(aNull)==NULL_ARRAY)
		Assert.True(ALen(AClone({}))==0)
		Assert.True(ALen(AClone({{}}))==1)


	[Fact, Trait("Category", "ACloneShallow")];
	METHOD ACloneShallow_Tests() AS VOID
		Assert.True(ALen(ACloneShallow({1}))==1)
		Assert.True(ALen(ACloneShallow({1,2}))==2)
		Assert.True(  ACloneShallow({1,2})[2] ==2  )
		Assert.True(  ACloneShallow({1,{2,3},4})[2,2] == 3  )
		Assert.True(  ACloneShallow({1,{},4})[3] == 4  )
		
		LOCAL aSub AS ARRAY
		aSub := {1,2}
		Assert.True(  ACloneShallow({1,aSub,3})[2] == aSub  )

		Assert.True(ACloneShallow(NULL_ARRAY)==NULL_ARRAY)
		LOCAL aNull AS ARRAY
		aNull := NULL_ARRAY
		Assert.True(ACloneShallow(aNull)==NULL_ARRAY)
		Assert.True(ALen(ACloneShallow({}))==0)
		Assert.True(ALen(ACloneShallow({{}}))==1)

	[Fact, Trait("Category", "AClone")];
	METHOD ACloneWith_NULL_ARRAY_as_Elements() AS VOID
		Assert.True(  ALen(  AClone({NULL_ARRAY,NULL_ARRAY}) ) == 2  )
		Assert.True(  AClone({NULL_ARRAY})[1] == NULL_ARRAY  )

		Assert.True(  ALen(  ACloneShallow({NULL_ARRAY,NULL_ARRAY}) ) == 2  )
		Assert.True(  ACloneShallow({NULL_ARRAY})[1] == NULL_ARRAY  )

	END CLASS
END NAMESPACE

CLASS TestClassParent
END CLASS
CLASS TestClassChild INHERIT TestClassParent
END CLASS
