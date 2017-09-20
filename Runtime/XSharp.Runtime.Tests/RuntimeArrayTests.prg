USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using XSharp.Runtime

/*
// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.Runtime.Tests

	[TestClass];
	CLASS RuntimeArrayTests

		[TestMethod];
		METHOD ArrayCreateTest() as void
			local testArray := __Array:ArrayCreate(2,3) as __Array
			AreNotEqual(null,testArray)
			AreEqual((dword)2,testArray:Length)
			AreEqual((dword)3,((__Array)testArray[1]):Length)
		RETURN
		[TestMethod];
		METHOD ArrayFillTest() as void
			local testArray := __Array{3} as __Array
			testArray[1] := 1
			testArray[2] := 2
			testArray[3] := 3
			__Array.ArrayFill(testArray,4)
			AreEqual(4,(int)testArray[1])
			AreEqual(4,(int)testArray[2])
			AreEqual(4,(int)testArray[3])

            __Array.ArrayFill(testArray, 5,1)
            AreEqual(5, (int)testArray[2])
            AreEqual(5, (int)testArray[3])

            __Array.ArrayFill(testArray, 9, 1 , 1)
            AreEqual(4, (int)testArray[1])
            AreEqual(9, (int)testArray[2])
            AreEqual(5, (int)testArray[3])
		return
		[TestMethod];
		METHOD ArraySwapTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			local newValue := 4 as int
			local oldValue := testArray:Swap(2,newValue) as __Usual
			AreEqual( 2 , (int) oldValue)
			AreEqual( 4 , (int) testArray[2])
		return
		[TestMethod];
		METHOD ArrayDeleteTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			__Array.ArrayDelete(testArray,1)
			AreEqual( (dword)3 , testArray:Length)
			AreEqual( __Usual.___Usual._NIL ,  testArray[3])
		return
		[TestMethod];
		METHOD ArraySizeTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			testArray:Size(4)
			AreEqual( __Usual.___Usual._NIL ,  testArray[4])
			testArray:Size(2)
			AreEqual((dword)2,testArray:Length)
		return
		[TestMethod];
		METHOD ArrayTailTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := __Array{values} as __Array
			AreEqual( 3 , (int) testArray:Tail())
		return
		[TestMethod];
		METHOD ArrayDimTest() as void
			local values := <object>{5,4} as object[]
			local subArray := __Array{values} as __Array
			local mainArray := __Array{} as __Array
			mainArray:Add(1)
			mainArray:Add("2")
			mainArray:Add(true)
			mainArray:Add("test")
			mainArray:Add(__Usual{subArray})
			var u := mainArray:GetElement(5,2)
			AreEqual( 4, (int) u)
		return
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
*/