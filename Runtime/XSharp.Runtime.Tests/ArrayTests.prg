USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using XSharp.Runtime

// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.Runtime.Tests

	CLASS RuntimeArrayTests

		[Fact];
		METHOD ArrayCreateTest() as void
			local testArray := __Array:ArrayCreate(2,3) as __Array
			Assert.NotEqual(null,testArray)
			Assert.Equal((dword)2,testArray:Length)
			Assert.Equal((dword)3,((__Array)testArray[1]):Length)
		RETURN
		[Fact];
		METHOD ArrayFillTest() as void
			local testArray := __Array{3} as __Array
			testArray[1] := 1
			testArray[2] := 2
			testArray[3] := 3
		return
		[Fact];
		METHOD ArraySwapTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			local newValue := 4 as int
			local oldValue := testArray:Swap(2,newValue) as __Usual
			Assert.Equal( 2 , (int) oldValue)
			Assert.Equal( 4 , (int) testArray[2])
		return
		
		[Fact];
		METHOD ArrayDeleteTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			testArray:Delete(1)
			Assert.Equal( (dword)3 , testArray:Length)
			Assert.Equal( __Usual._NIL ,  testArray[3])
		return
		
		[Fact];
		METHOD ArraySizeTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			testArray:Size(4)
			Assert.Equal( __Usual._NIL ,  testArray[4])
			testArray:Size(2)
			Assert.Equal((dword)2,testArray:Length)
		return

		[Fact];
		METHOD ArrayTailTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			Assert.Equal( 3 , (int) testArray:Tail())
		return
		
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
			var u := mainArray:__GetElement(5,2)
			Assert.Equal( 4, (int) u)
		return
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests