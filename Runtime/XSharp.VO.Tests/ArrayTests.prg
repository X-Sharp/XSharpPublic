//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit


// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VO.Tests

	CLASS RuntimeArrayTests
	 
 		[Trait("Category", "Array")];
		[Fact]; 
		METHOD ArrayCreateTest() as void
			local testArray := ArrayNew(2,3) as Array 
			Assert.NotEqual(null,testArray)
			Assert.Equal((dword)2,testArray:Length) 
			Assert.Equal((dword)3,((Array)testArray[1]):Length)
		RETURN
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayFillTest() as void
			local testArray := Array{3} as Array
			testArray[1] := 1
			testArray[2] := 2
			testArray[3] := 3
		return
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArraySwapTest() as void
			local testArray := {1,2,3} as Array
			local newValue := 4 as int
			local oldValue := ArraySwap(testArray, 2,newValue) as Usual
			Assert.Equal( 2 , (int) oldValue)
			Assert.Equal( 4 , (int) testArray[2])
		return
		
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayDeleteTest() as void
			local testArray := {1,2,3} as Array
			Adel(testArray, 1)
			Assert.Equal( (dword)3 , testArray:Length)
			Assert.Equal( NIL ,  testArray[3])
		return
		
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArraySizeTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := Array{values} as Array
			testArray:ReSize(4)
			Assert.Equal( NIL ,  testArray[4])
			testArray:ReSize(2)
			Assert.Equal((dword)2,testArray:Length)
		return

 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayTailTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := Array{values} as Array
			Assert.Equal( 3 , (int) testArray:Tail())
		return

 		[Trait("Category", "Array")];
		[Fact];
		METHOD AFillTest() as void
			local testArray := {}  as array
			Asize(testArray, 10)
			afill(testArray, "",1,9)
			Assert.Equal( "" , testArray[1])
			Assert.Equal( "" , testArray[9])
			Assert.Equal( NIL , testArray[10])
		return

		
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayDimTest() as void
			local subArray := {5,4,1,2,3,4,5,6} as Array
			local mainArray := Array{} as Array
			AAdd(mainArray,1)
			AAdd(mainArray,"2")
			AAdd(mainArray,true)
			AAdd(mainArray,"test")
			AAdd(mainArray,subArray)
			var u := mainArray[5][2] 
			Assert.Equal( 4, (int) u)
			mainArray[5][2] := "anothertest"
			u := mainArray[5][2]
			Assert.Equal( "anothertest", (string) u)
		return
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests