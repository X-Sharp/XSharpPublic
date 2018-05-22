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

	CLASS ArrayTests
	 
 		[Trait("Category", "Array")];
		[Fact]; 
		METHOD ArrayCreateTest() as void
			local testArray := ArrayNew(2,3) as Array 
			Assert.NotEqual(null,testArray)
			Assert.Equal((dword)2, Alen(testArray)) 
			Assert.Equal((dword)3,(Alen(testArray[1])))
		RETURN
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayFillTest() as void
			local testArray := Array{3} as Array
			Afill(testArray, 42)
			Assert.Equal( 42 , (int) testArray[1])
			Assert.Equal( 42 , (INT) testArray[2])
			Assert.Equal( 42 , (int) testArray[3])
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
			Assert.Equal( (dword)3 , Alen(testArray))
			Assert.Equal( NIL ,  testArray[3])

		return
		
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArraySizeTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := Array{values} as Array
			ASize(testArray, 4)
			Assert.Equal( NIL ,  testArray[4])
			ASize(testArray, 2)
			Assert.Equal((dword)2,Alen(testArray))
		return

 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayTailTest() as void
			local values := <object>{1,2,3} as object[]
			local testArray := Array{values} as Array
			Assert.Equal( 3 , (int) ATail(testArray))
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
		RETURN

 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArraySortTest() as void
			LOCAL a := {1,3,2,5,4,6} AS array
			ASort(a)
			Assert.Equal( 1, (INT) a[1])
			Assert.Equal( 2, (int) a[2])
			Assert.Equal( 3, (INT) a[3])
			Assert.Equal( 4, (int) a[4])
			Assert.Equal( 5, (int) a[5])
			Assert.Equal( 6, (INT) a[6])
			ASort(a, 1, alen(a), {|x,y| x > y })
			Assert.Equal( 6,(INT) a[1])
			Assert.Equal( 5,(int) a[2])
			Assert.Equal( 4,(INT) a[3])
			Assert.Equal( 3, (int) a[4])
			Assert.Equal( 2, (int) a[5])
			Assert.Equal( 1, (INT) a[6])
			a := {"Fred", "Kate", "ALVIN", "friend"}

			ASort(a,,, {|x, y| Upper(x) <= Upper(y)})        // {ALVIN, FRED, FRIEND, KATE}
			Assert.Equal( "ALVIN",(string) a[1])
			Assert.Equal( "Fred",(string) a[2])
			Assert.Equal( "friend",(string) a[3])
			Assert.Equal( "Kate" ,(string) a[4])

		[Trait("Category", "Array")];
		[Fact];
		METHOD AscanTest() as void
			LOCAL a := {1,3,2,5,4,6} AS array
			Assert.Equal( 1, (INT) Ascan(a, 1))
			Assert.Equal( 3, (int) Ascan(a, 2))
			Assert.Equal( 2, (INT) Ascan(a, 3))
			Assert.Equal( 5, (int) Ascan(a, 4))
			Assert.Equal( 4, (int) Ascan(a, 5))
			Assert.Equal( 6, (int) Ascan(a, 6))
			ASort(a)
			Assert.Equal( 1, (INT) Ascan(a, 1))
			Assert.Equal( 2, (int) Ascan(a, 2))
			Assert.Equal( 3, (INT) Ascan(a, 3))
			Assert.Equal( 4, (int) Ascan(a, 4))
			Assert.Equal( 5, (int) Ascan(a, 5))
			Assert.Equal( 6, (int) Ascan(a, 6))
			a := {"Fred", "Kate", "ALVIN", "friend"}
			Assert.Equal( 3, (int)  Ascan(a, "ALVIN"))
			Assert.Equal( 1, (int) Ascan(a, "Fred"))
			Assert.Equal( 4, (int) Ascan(a, "friend"))
			Assert.Equal( 2, (int) Ascan(a, "Kate"))

			ASort(a,,, {|x, y| Upper(x) <= Upper(y)})        // {ALVIN, FRED, FRIEND, KATE}
			Assert.Equal( 1, (int) Ascan(a, "ALVIN"))
			Assert.Equal( 2, (int) Ascan(a, "Fred"))
			Assert.Equal( 3, (int) Ascan(a, {|e| e == "friend"}))
			Assert.Equal( 4, (int) Ascan(a, {|e| e == "Kate" }))
			Assert.Equal( 2, (INT) AscanBin(a, "Fred"))
			SetExact(FALSE)
			Assert.Equal( 2, (INT) AscanBin(a, "Fre"))
			Assert.Equal( 0, (INT) AscanBinExact(a, "Fre"))
		[Trait("Category", "Array")];
		[Fact];
		METHOD AevalTest() AS VOID
			LOCAL aValues := {1,2,3} AS ARRAY
			LOCAL nCounter  AS LONG
			nCounter := 0
			Aeval(aValues, {|x| nCounter++})
			Assert.Equal( 3, nCounter)
			nCounter := 0
			Aeval(aValues, {|x| nCounter++}, 2)
			Assert.Equal( 2, nCounter)

			nCounter := 0
			Aeval(aValues, {|x| nCounter++}, 2,1)
			Assert.Equal( 1, nCounter)
			nCounter := 0
			// AevalA assigns the return value to the array
			AevalA(aValues, {|x| nCounter++, x*2})
			Assert.Equal( 3, nCounter)
			Assert.Equal( 2, (INT) aValues[1])
			Assert.Equal( 4, (int) aValues[2])
			Assert.Equal( 6, (int) aValues[3])
			// AevalOld passes an extra parameter
			nCounter := 0
			AevalOld(aValues, {|x, n| nCounter+=n})
			Assert.Equal( 6, nCounter)
		[Trait("Category", "Array")];
		[Fact];
		METHOD ACopyTest() AS VOID
			LOCAL aValues := {1,2,3} AS ARRAY
			LOCAL aDest   := ArrayNew(3) AS ARRAY
			ACopy(aValues, aDest)
			Assert.Equal( 1, (INT) aDest[1])
			Assert.Equal( 2, (int) aDest[2])
			Assert.Equal( 3, (INT) aDest[3])
			aDest   := ArrayNew(2) 
			ACopy(aValues, aDest)
			Assert.Equal( 1, (INT) aDest[1])
			Assert.Equal( 2, (int) aDest[2])



	END CLASS
END NAMESPACE // XSharp.Runtime.Tests