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
			local testArray := __Array:ArrayCreate(2,3) as __Array
			Assert.NotEqual(null,testArray)
			Assert.Equal((dword)2,testArray:Length)
			Assert.Equal((dword)3,((__Array)testArray[1]):Length)
		RETURN
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayFillTest() as void
			local testArray := __Array{3} as __Array
			testArray[1] := 1
			testArray[2] := 2
			testArray[3] := 3
		return
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArraySwapTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			local newValue := 4 as int
			local oldValue := testArray:Swap(2,newValue) as __Usual
			Assert.Equal( 2 , (int) oldValue)
			Assert.Equal( 4 , (int) testArray[2])
		return
		
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayDeleteTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			testArray:Delete(1)
			Assert.Equal( (dword)3 , testArray:Length)
			Assert.Equal( __Usual._NIL ,  testArray[3])
		return
		
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArraySizeTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			testArray:ReSize(4)
			Assert.Equal( __Usual._NIL ,  testArray[4])
			testArray:ReSize(2)
			Assert.Equal((dword)2,testArray:Length)
		return

 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayTailTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			Assert.Equal( 3 , (int) testArray:Tail())
		return
		
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayDimTest() as void
			local values := <object>{5,4} as object[]
			local subArray := __Array{values} as __Array
			local mainArray := __Array{} as __Array
			mainArray:Add(1)
			mainArray:Add("2")
			mainArray:Add(true)
			mainArray:Add("test")
			mainArray:Add(__Usual{subArray})
			var u := mainArray:__GetElement(4,1)
			Assert.Equal( 4, (int) u)
			mainArray:__SetElement ("anothertest", 4,1)
			u := mainArray:__GetElement(4,1)
			Assert.Equal( "anothertest", (string) u)
		return
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests