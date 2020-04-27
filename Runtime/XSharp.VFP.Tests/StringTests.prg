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


        Assert.Equal( RAt( NULL , NULL ,1) , 0 )
        Assert.Equal( RAt( NULL , "eeee" ,1) , 0)
        Assert.Equal( RAt( "" , NULL ,1) , 0 )
        Assert.Equal( RAt( "" , "eeee" ,1) , 0 )
        Assert.Equal( RAt( "ee" , "" , 1) , 0 )
        Assert.Equal( RAt( "" , "" , )   , 0 )
        Assert.Equal( RAt( "gg" , "ggeegggege" , 2) , 5 )
        Assert.Equal( RAt( "gg" , "ggeegggege" , 3) , 1)
        Assert.Equal( RAt( "ag" , "ageegggege" , 6) , 0)
        Assert.Equal( RAt( "g" , "eegggege" , 2) , 5)
        Assert.Equal( RAt( "g" , "eegggege" , 3 ) , 4)
        Assert.Equal( RAt( "gg" , "eegggege" , 2 ) , 3)
        Assert.Equal( RAt( "gg" , "eegggege" , 1 ) , 4)
        Assert.Equal( RAt( "gg" , "eegggege" ,1) , 4)
        Assert.Equal( RAt( "eeee" , "eeee",1 ) , 1)
        Assert.Equal( RAt( "ee" , "eeee" , 1) , 3) 
        Assert.Equal( RAt( "ee" , "eeee" , 2) , 2)  
        Assert.Equal( RAt( "ee" , "eeee" , 3) , 1)
        Assert.Equal( RAt( "ee" , "eeee" , 4) , 0)
        Assert.Equal( RAt( "ee" , "eeee",1) , 3)
        Assert.Equal( RAt( "ee" , "eeaeeaa" ,1) , 4) 
        Assert.Equal( RAt( "ee" , "eeaeeaa" , 2 ) , 1)
        Assert.Equal( RAt( "e" , "eeee" , 1) , 4)
        Assert.Equal( RAt( "e" , "eeee" , 2) , 3)
        Assert.Equal( RAt( "e" , "eeee" , 3) , 2)
        Assert.Equal( RAt( "e" , "eeee" , 4) , 1)
        Assert.Equal( RAt( "e" , "eeee" ,1) , 4)
        Assert.Equal( RAt( "jkh" , "jkhjkhejkaaajkh" , 2 ) , 4)
        Assert.Equal( RAt( "jkh" , "jkhjkhejkaaajkh" ,1)	, 13)
        Assert.Equal( RAt("bb", "abbcabbcbba" , 1  ) , 9)
        Assert.Equal( RAt("bb", "abbcabbcbba" , 2  ) , 6)
        Assert.Equal( RAt("bb", "abbcabbcbba" , 3  ) , 2)
        Assert.Equal( RAt("bb", "abbcabbcbba" , 4  ) , 0)
        Assert.Equal( RAt("bb", "abbcabbcbba" ,1) , 9 )
        Assert.Equal( RAt("e", "abbcabbcbba"  ,1)  , 0)  
        Assert.Equal( RAt("b", "abbcabbcbba" , 5 ) , 3) 
        Assert.Equal( RAt("a","abracadabra",1)  ,  11)
        Assert.Equal( RAt("a","abracadabra",3)  ,  6)
        Assert.Equal( RAt("a","abracadabra",43)  ,  0)
        Assert.Equal( RAt("a","abracadabra",1)  ,  11)
        Assert.Equal( RAt("a" , "AaaA", 2  ) , 2)
        Assert.Equal( RAt("hj" , "eeagfeehjkhjkhje" ,3 )  , 8)
        Assert.Equal( RAt("e" , "eeagfeehjkhjkhje" , 3 ) , 6)
        Assert.Equal( RAt("ee" , "eeagfeehjkhjkhje" , 2 ) , 1)



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
		METHOD At_Tests() AS VOID
            Assert.Equal(9 , At("b", "abucabucbba", 3 )  )
            Assert.Equal(6 , At("bu", "abucabucbba", 2 ) )
            Assert.Equal(2 , At("bu", "abucabucbba", 1 ) )
            Assert.Equal(0 , At("bu", "abucabucbba", 12 ))
            
            Assert.Equal(9 , AtC("B", "abucabucbba", 3 )  )
            Assert.Equal(6 , AtC("Bu", "abucabucbba", 2 ) )
            Assert.Equal(2 , AtC("Bu", "abucabucbba", 1 ) )
            Assert.Equal(0 , AtC("Bu", "abucabucbba", 12 ))
            
            Assert.Equal(0 , At("B", "abucabucbba", 2 ) )
            Assert.Equal(0 , At("Bu", "abucabucbba", 2 ) )
            Assert.Equal(0 , At("Bu", "abucabucbba", 1 ) )
            Assert.Equal(0 , At("Bu", "abucabucbba", 12 ))

	END CLASS

END NAMESPACE
