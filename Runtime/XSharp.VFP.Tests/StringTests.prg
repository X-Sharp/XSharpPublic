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
#pragma warnings (219, disable) // unused variables

// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS StringTests
	
		[Fact, Trait("Category", "String")];
		METHOD JustFunctionTests() AS VOID
            Assert.Equal("C:", JustDrive("C:\Folder\test.txt") )
            Assert.Equal("txt", JustExt("C:\Folder\test.txt") )
            Assert.Equal("C:\Folder", JustPath("C:\Folder\test.txt") )
            Assert.Equal("test.txt", JustFName("C:\Folder\test.txt") )
            Assert.Equal("test", JustStem("C:\Folder\test.txt") )
            Assert.Equal("C:\",Addbs("C:"))
            Assert.Equal("C:\test\",Addbs("C:\test"))

		[Fact, Trait("Category", "String")];
        METHOD RatFunctionTests() AS VOID


        Assert.Equal( RAt( NULL , NULL ,1) , 0U )
        Assert.Equal( RAt( NULL , "eeee" ,1) , 0U)
        Assert.Equal( RAt( "" , NULL ,1) , 0U )
        Assert.Equal( RAt( "" , "eeee" ,1) , 0U )
        Assert.Equal( RAt( "ee" , "" , 1) , 0U )
        Assert.Equal( RAt( "" , "" , )   , 0U )
        Assert.Equal( RAt( "gg" , "ggeegggege" , 2) , 5U )
        Assert.Equal( RAt( "gg" , "ggeegggege" , 3) , 1U)
        Assert.Equal( RAt( "ag" , "ageegggege" , 6) , 0U)
        Assert.Equal( RAt( "g" , "eegggege" , 2) , 5U)
        Assert.Equal( RAt( "g" , "eegggege" , 3 ) , 4U)
        Assert.Equal( RAt( "gg" , "eegggege" , 2 ) , 3U)
        Assert.Equal( RAt( "gg" , "eegggege" , 1 ) , 4U)
        Assert.Equal( RAt( "gg" , "eegggege" ,1) , 4U)
        Assert.Equal( RAt( "eeee" , "eeee",1 ) , 1U)
        Assert.Equal( RAt( "ee" , "eeee" , 1) , 3U) 
        Assert.Equal( RAt( "ee" , "eeee" , 2) , 2U)  
        Assert.Equal( RAt( "ee" , "eeee" , 3) , 1U)
        Assert.Equal( RAt( "ee" , "eeee" , 4) , 0U)
        Assert.Equal( RAt( "ee" , "eeee",1) , 3U)
        Assert.Equal( RAt( "ee" , "eeaeeaa" ,1) , 4U) 
        Assert.Equal( RAt( "ee" , "eeaeeaa" , 2 ) , 1U)
        Assert.Equal( RAt( "e" , "eeee" , 1) , 4U)
        Assert.Equal( RAt( "e" , "eeee" , 2) , 3U)
        Assert.Equal( RAt( "e" , "eeee" , 3) , 2U)
        Assert.Equal( RAt( "e" , "eeee" , 4) , 1U)
        Assert.Equal( RAt( "e" , "eeee" ,1) , 4U)
        Assert.Equal( RAt( "jkh" , "jkhjkhejkaaajkh" , 2 ) , 4U)
        Assert.Equal( RAt( "jkh" , "jkhjkhejkaaajkh" ,1)	, 13U)
        Assert.Equal( RAt("bb", "abbcabbcbba" , 1  ) , 9U)
        Assert.Equal( RAt("bb", "abbcabbcbba" , 2  ) , 6U)
        Assert.Equal( RAt("bb", "abbcabbcbba" , 3  ) , 2U)
        Assert.Equal( RAt("bb", "abbcabbcbba" , 4  ) , 0U)
        Assert.Equal( RAt("bb", "abbcabbcbba" ,1) , 9U )
        Assert.Equal( RAt("e", "abbcabbcbba"  ,1)  , 0U)  
        Assert.Equal( RAt("b", "abbcabbcbba" , 5 ) , 3U) 
        Assert.Equal( RAt("a","abracadabra",1)  ,  11U)
        Assert.Equal( RAt("a","abracadabra",3)  ,  6U)
        Assert.Equal( RAt("a","abracadabra",43)  ,  0U)
        Assert.Equal( RAt("a","abracadabra",1)  ,  11U)
        Assert.Equal( RAt("a" , "AaaA", 2  ) , 2U)
        Assert.Equal( RAt("hj" , "eeagfeehjkhjkhje" ,3 )  , 8U)
        Assert.Equal( RAt("e" , "eeagfeehjkhjkhje" , 3 ) , 6U)
        Assert.Equal( RAt("ee" , "eeagfeehjkhjkhje" , 2 ) , 1U)



		[Fact, Trait("Category", "String")];
        METHOD ChrTranFunctionTests() AS VOID
        VAR cAllowedCharacters := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        VAR cExpressionSearched := "123!EB5466677eggtz123%&Y" 

        Assert.Equal(ChrTran( cExpressionSearched, ChrTran(cExpressionSearched, cAllowedCharacters, ""), "") , "EBeggtzY")

        Assert.Equal(ChrTran("ABCDEFGHABCDEFGH", "AE", "ae") , "aBCDeFGHaBCDeFGH" )
        Assert.Equal(ChrTran("ABCDEFGHIJK","DEF","") , "ABCGHIJK")
        Assert.Equal(ChrTran("ABCabc2468", "1234567890", "" ) , "ABCabc" )
        Assert.Equal(ChrTran("ABCDEF", "ACE", "XYZ")  , "XBYDZF"  )
        Assert.Equal(ChrTran("ABCDEF", "", "XYZ") , "ABCDEF")
        Assert.Equal(ChrTran("ABCDEF", "ACE", "") , "BDF")
        Assert.Equal(ChrTran("ABCD", "ABC", "YZ")  , "YZD")
        Assert.Equal(ChrTran("ABCDEF", "ACE", "XYZQRST") ,  "XBYDZF" )
        Assert.Equal(ChrTran("", "ABC", "YZ") , "" )
        Assert.Equal(ChrTran("ABCDEF", "", "ACE")  , "ABCDEF")
        Assert.Equal(ChrTran("ABCDEF", "", "")  , "ABCDEF" )
        Assert.Equal(ChrTran("abcDeF","ADF" , "xYZ1TZ" ) , "abcYeZ")
        Assert.Equal(ChrTran("","" , "" ) , "" )
        Assert.Equal(ChrTran(NULL,NULL , "" ) , "")

        VAR x := Replicate ( "A", 200000 ) 
        VAR y := Replicate ( "I", 200000 )
 
        VAR g := ChrTran( x ,"A" , "IE" ) 

		[Fact, Trait("Category", "String")];
		METHOD AtFuctionTests() AS VOID
            Assert.Equal(9u , At("b", "abucabucbba", 3 )  )
            Assert.Equal(6u , At("bu", "abucabucbba", 2 ) )
            Assert.Equal(2u , At("bu", "abucabucbba", 1 ) )
            Assert.Equal(0u , At("bu", "abucabucbba", 12 ))
            
            Assert.Equal(9u , AtC("B", "abucabucbba", 3 )  )
            Assert.Equal(6u , AtC("Bu", "abucabucbba", 2 ) )
            Assert.Equal(2u , AtC("Bu", "abucabucbba", 1 ) )
            Assert.Equal(0u , AtC("Bu", "abucabucbba", 12 ))
            
            Assert.Equal(0u , At("B", "abucabucbba", 2 ) )
            Assert.Equal(0u , At("Bu", "abucabucbba", 2 ) )
            Assert.Equal(0u , At("Bu", "abucabucbba", 1 ) )
            Assert.Equal(0u , At("Bu", "abucabucbba", 12 ))

            Assert.Equal(0u, At( NULL , NULL ) )
            Assert.Equal(0u, At( NULL , "eeee" ) )
            Assert.Equal(0u, At( "" , NULL ) )
            Assert.Equal(0u, At( "" , "eeee" ) )
            Assert.Equal(0u, At( "ee" , "" , ) )
            Assert.Equal(0u, At( "" , "" , )   )
            Assert.Equal(6u, At( "gg" , "ggeegggege" , 3) )
            Assert.Equal(1u, At( "gg" , "ggeegggege" , 1) )
            Assert.Equal(0u, At( "ag" , "ageegggege" , 6) )
            Assert.Equal(4u, At( "g" , "eegggege" , 2) )
            Assert.Equal(7u, At( "g" , "eegggege" , 4 ) )
            Assert.Equal(9u, At( "gg" , "eegggegegg" , 3 ) )
            Assert.Equal(3u, At( "gg" , "eegggege" , 1 ) )
            Assert.Equal(3u, At( "gg" , "eegggege" ) )
            Assert.Equal(1u, At( "eeee" , "eeee" ) )
            Assert.Equal(1u, At( "ee" , "eeee" , 1) )
            Assert.Equal(2u, At( "ee" , "eeee" , 2) )
            Assert.Equal(3u, At( "ee" , "eeee" , 3) )
            Assert.Equal(0u, At( "ee" , "eeee" , 4) )
            Assert.Equal(1u, At( "ee" , "eeee" ) )
            Assert.Equal(1u, At( "ee" , "eeaeeaa" ) )
            Assert.Equal(4u, At( "ee" , "eeaeeaa" , 2 ) )
            Assert.Equal(1u, At( "e" , "eeee" , 1) )
            Assert.Equal(2u, At( "e" , "eeee" , 2) )
            Assert.Equal(3u, At( "e" , "eeee" , 3) )
            Assert.Equal(4u, At( "e" , "eeee" , 4) )
            Assert.Equal(1u, At( "e" , "eeee" ) )
            Assert.Equal(1u, At( "jkh" , "jkhjkhejkaaajkh" ) )
            Assert.Equal(4u, At( "jkh" , "jkhjkhejkaaajkh" , 2 ) )
            Assert.Equal(13u, At( "jkh" , "jkhjkhejkaaajkh" , 3 ) )
            Assert.Equal(2u, At("bb", "abbcabbcbba" , 1  ) )
            Assert.Equal(6u, At("bb", "abbcabbcbba" , 2  ) )
            Assert.Equal(9u, At("bb", "abbcabbcbba" , 3  ) )
            Assert.Equal(0u, At("bb", "abbcabbcbba" , 4  ) )
            Assert.Equal(0u, At("e", "abbcabbcbba"  )  )
            Assert.Equal(9u, At("b", "abbcabbcbba" , 5 ) )
            Assert.Equal(0u, At("a","abracadabra",43)  )
            Assert.Equal(4u, At("a","abracadabra",2)  )
            Assert.Equal(3u, At("a" , "AaaA", 2  ) )
            Assert.Equal(2u, At("a" , "AaaA" ) )
            Assert.Equal(14u, At("hj" , "eeagfeehjkhjkhje" ,3 )  )
            Assert.Equal(16u, At("e" , "eeagfeehjkhjkhje" , 5 ) )
            Assert.Equal(8u, At("ee" , "eeeeagfeehjkhjkhje" , 4 ) )
            
            Assert.Equal(1u, AtC ("a" , "AaaA" ) )
            Assert.Equal(2u, AtC ("aa" , "AaaA" , 2 ) )
            Assert.Equal(2u, AtC ("aA" , "AaaA", 2 ) )
            Assert.Equal(3u, AtC ("aA" , "AaaA", 3 ) )
            Assert.Equal(0u, AtC ("aA" , "AaaA", 12 ) )
            Assert.Equal(0u, AtC ("aA" , "AaaA", 4 ) )

		[Fact, Trait("Category", "String")];
		METHOD TrimTests() AS VOID

            Assert.Equal("",		AllTrim(" "))
            Assert.Equal("",		AllTrim(e" \n ", 0, " ", ChrW(9), ChrW(10), ChrW(13)))
            Assert.Equal("Test",	AllTrim("Test "))
            Assert.Equal("Test",	AllTrim(e"Test \n ", 0, " ", ChrW(9), ChrW(10), ChrW(13)))
            Assert.Equal("TeTeTest",AllTrim("TeTeTest", 0, "te"))
            Assert.Equal("st",		AllTrim("TeTeTest", 1, "te"))
            Assert.Equal("TeTeTes",	AllTrim("TeTeTest", 0, "t", "e"))
            Assert.Equal("s",		AllTrim("TeTeTest", 1, "t", "e"))
            Assert.Equal("abc",		AllTrim("***abc*", 0, "*"))
            Assert.Equal("abc*",	LTrim("***abc*", 0, "*"))
            Assert.Equal("***abc",	RTrim("***abc*", 0, "*"))
            Assert.Equal("cdefgh",	AllTrim("abcdefghab", 1, "AB", "B"))
            Assert.Equal("cdefgha",	AllTrim("abcdefghab", 1, "B", "AB"))
            Assert.Equal("ef",		AllTrim("abABabCdef", 1, "ab", "CD"))

	END CLASS

END NAMESPACE
