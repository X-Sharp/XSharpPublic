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
BEGIN NAMESPACE XSharp.VO.Tests

	CLASS ArrayTests
	 
 		[Trait("Category", "Array")];
		[Fact]; 
		METHOD ArrayCreateTest() AS VOID
			LOCAL testArray := ArrayNew(2,3) AS ARRAY 
			Assert.NotEqual(NULL,testArray)
			Assert.Equal((DWORD)2, ALen(testArray)) 
			Assert.Equal((DWORD)3,(ALen(testArray[1])))
		RETURN
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayFillTest() AS VOID
			LOCAL testArray := ARRAY{3} AS ARRAY
			AFill(testArray, 42)
			Assert.Equal( 42 , (INT) testArray[1])
			Assert.Equal( 42 , (INT) testArray[2])
			Assert.Equal( 42 , (INT) testArray[3])
		RETURN
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArraySwapTest() AS VOID
			LOCAL testArray := {1,2,3} AS ARRAY
			LOCAL newValue := 4 AS INT
			LOCAL oldValue := ArraySwap(testArray, 2,newValue) AS USUAL
			Assert.Equal( 2 , (INT) oldValue)
			Assert.Equal( 4 , (INT) testArray[2])
		RETURN
		
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayDeleteTest() AS VOID
			LOCAL testArray := {1,2,3} AS ARRAY
			ADel(testArray, 1)
			Assert.Equal( (DWORD)3 , ALen(testArray))
			Assert.Equal( NIL ,  testArray[3])

		RETURN
		
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArraySizeTest() AS VOID
			LOCAL values := <OBJECT>{1,2,3} AS OBJECT[]
			LOCAL testArray := ARRAY{values} AS ARRAY
			ASize(testArray, 4)
			Assert.Equal( NIL ,  testArray[4])
			ASize(testArray, 2)
			Assert.Equal((DWORD)2,ALen(testArray))
		RETURN

 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayTailTest() AS VOID
			LOCAL values := <OBJECT>{1,2,3} AS OBJECT[]
			LOCAL testArray := ARRAY{values} AS ARRAY
			Assert.Equal( 3 , (INT) ATail(testArray))
		RETURN

 		[Trait("Category", "Array")];
		[Fact];
		METHOD AFillTest() AS VOID
			LOCAL testArray := {}  AS ARRAY
			ASize(testArray, 10)
			AFill(testArray, "",1,9)
			Assert.Equal( "" , testArray[1])
			Assert.Equal( "" , testArray[9])
			Assert.Equal( NIL , testArray[10])
		RETURN

		
 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArrayDimTest() AS VOID
			LOCAL subArray := {5,4,1,2,3,4,5,6} AS ARRAY
			LOCAL mainArray := ARRAY{} AS ARRAY
			AAdd(mainArray,1)
			AAdd(mainArray,"2")
			AAdd(mainArray,TRUE)
			AAdd(mainArray,"test")
			AAdd(mainArray,subArray)
			VAR u := mainArray[5][2] 
			Assert.Equal( 4, (INT) u)
			mainArray[5][2] := "anothertest"
			u := mainArray[5][2]
			Assert.Equal( "anothertest", (STRING) u)
		RETURN

 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArraySortTest() AS VOID
			LOCAL a := {1,3,2,5,4,6} AS ARRAY
			ASort(a)
			Assert.Equal( 1, (INT) a[1])
			Assert.Equal( 2, (INT) a[2])
			Assert.Equal( 3, (INT) a[3])
			Assert.Equal( 4, (INT) a[4])
			Assert.Equal( 5, (INT) a[5])
			Assert.Equal( 6, (INT) a[6])
			ASort(a, 1, ALen(a), {|x,y| x > y })
			Assert.Equal( 6,(INT) a[1])
			Assert.Equal( 5,(INT) a[2])
			Assert.Equal( 4,(INT) a[3])
			Assert.Equal( 3, (INT) a[4])
			Assert.Equal( 2, (INT) a[5])
			Assert.Equal( 1, (INT) a[6])
			a := {"Fred", "Kate", "ALVIN", "friend"}

			ASort(a,,, {|x, y| Upper(x) <= Upper(y)})        // {ALVIN, FRED, FRIEND, KATE}
			Assert.Equal( "ALVIN",(STRING) a[1])
			Assert.Equal( "Fred",(STRING) a[2])
			Assert.Equal( "friend",(STRING) a[3])
			Assert.Equal( "Kate" ,(STRING) a[4])

			a := {8,2,1,4,3,0}
			ASort(a , 2 , 4)
			Assert.Equal( 8, (INT) a[1])
			Assert.Equal( 1, (INT) a[2])
			Assert.Equal( 2, (INT) a[3])
			Assert.Equal( 3, (INT) a[4])
			Assert.Equal( 4, (INT) a[5])
			Assert.Equal( 0, (INT) a[6])
			? a[1],a[2],a[3],a[4],a[5]
			
			a := {}
			ASort(a)
			ASort(a , 0)
			ASort(a , 10)
			ASort(a , 10 , 10)

			a := {8,2,1,4,3,0}
			ASort(a , 10)
			Assert.Equal( 8, (INT) a[1])
			Assert.Equal( 0, (INT) a[6])
			ASort(a , 10 , 10)
			Assert.Equal( 8, (INT) a[1])
			Assert.Equal( 0, (INT) a[6])

			ASort(a , 0)
			Assert.Equal( 0, (INT) a[1])
			Assert.Equal( 8, (INT) a[6])

			a := {8,2,1,4,3,0}
			ASort(a , 0 , 100)
			Assert.Equal( 0, (INT) a[1])
			Assert.Equal( 8, (INT) a[6])


		[Trait("Category", "Array")];
		[Fact];
		METHOD AscanTest() AS VOID
			LOCAL a := {1,3,2,5,4,6} AS ARRAY
			Assert.Equal( 1, (INT) AScan(a, 1))
			Assert.Equal( 3, (INT) AScan(a, 2))
			Assert.Equal( 2, (INT) AScan(a, 3))
			Assert.Equal( 5, (INT) AScan(a, 4))
			Assert.Equal( 4, (INT) AScan(a, 5))
			Assert.Equal( 6, (INT) AScan(a, 6))
			ASort(a)
			Assert.Equal( 1, (INT) AScan(a, 1))
			Assert.Equal( 2, (INT) AScan(a, 2))
			Assert.Equal( 3, (INT) AScan(a, 3))
			Assert.Equal( 4, (INT) AScan(a, 4))
			Assert.Equal( 5, (INT) AScan(a, 5))
			Assert.Equal( 6, (INT) AScan(a, 6))
			a := {"Fred", "Kate", "ALVIN", "friend"}
			Assert.Equal( 3, (INT)  AScan(a, "ALVIN"))
			Assert.Equal( 1, (INT) AScan(a, "Fred"))
			Assert.Equal( 4, (INT) AScan(a, "friend"))
			Assert.Equal( 2, (INT) AScan(a, "Kate"))

			ASort(a,,, {|x, y| Upper(x) <= Upper(y)})        // {ALVIN, FRED, FRIEND, KATE}
			Assert.Equal( 1, (INT) AScan(a, "ALVIN"))
			Assert.Equal( 2, (INT) AScan(a, "Fred"))
			Assert.Equal( 3, (INT) AScan(a, {|e| e == "friend"}))
			Assert.Equal( 4, (INT) AScan(a, {|e| e == "Kate" }))
			Assert.Equal( 2, (INT) AScanBin(a, "Fred"))
			SetExact(FALSE)
			Assert.Equal( 2, (INT) AScanBin(a, "Fre"))
			Assert.Equal( 0, (INT) AScanBinExact(a, "Fre"))
		[Trait("Category", "Array")];
		[Fact];
		METHOD AevalTest() AS VOID
			LOCAL aValues := {1,2,3} AS ARRAY
			LOCAL nCounter  AS LONG
			nCounter := 0
			AEval(aValues, {|x| nCounter++})
			Assert.Equal( 3, nCounter)
			nCounter := 0
			AEval(aValues, {|x| nCounter++}, 2)
			Assert.Equal( 2, nCounter)

			nCounter := 0
			AEval(aValues, {|x| nCounter++}, 2,1)
			Assert.Equal( 1, nCounter)
			nCounter := 0
			// AevalA assigns the return value to the array
			AEvalA(aValues, {|x| nCounter++, x*2})
			Assert.Equal( 3, nCounter)
			Assert.Equal( 2, (INT) aValues[1])
			Assert.Equal( 4, (INT) aValues[2])
			Assert.Equal( 6, (INT) aValues[3])
			// AevalOld passes an extra parameter
			nCounter := 0
			AEvalOld(aValues, {|x, n| nCounter+=n})
			Assert.Equal( 6, nCounter)
		[Trait("Category", "Array")];
		[Fact];
		METHOD ACopyTest() AS VOID
			LOCAL aValues := {1,2,3} AS ARRAY
			LOCAL aDest   := ArrayNew(3) AS ARRAY
			ACopy(aValues, aDest)
			Assert.Equal( 1, (INT) aDest[1])
			Assert.Equal( 2, (INT) aDest[2])
			Assert.Equal( 3, (INT) aDest[3])
			aDest   := ArrayNew(2) 
			ACopy(aValues, aDest)
			Assert.Equal( 1, (INT) aDest[1])
			Assert.Equal( 2, (INT) aDest[2])


		[Trait("Category", "Array")];
		[Fact];
		METHOD AInsDelTests() AS VOID
			LOCAL arr := {1,2,3} AS ARRAY

			AIns(arr , 2)
			Assert.True( ALen(arr) == 3)
			Assert.True( arr[1] == 1)
			Assert.True( arr[2] == NIL)
			Assert.True( arr[3] == 2)
			
			ADel(arr , 2)
			Assert.True( ALen(arr) == 3)
			Assert.True( arr[1] == 1)
			Assert.True( arr[2] == 2)
			Assert.True( arr[3] == NIL)

		[Trait("Category", "Array")];
		[Fact];
		METHOD AFillTests() AS VOID
			LOCAL arr := {1,2,3,4} AS ARRAY
			
			//#warning AFill(arr , val , , count) is not supported due to AFill() being strongly typed
			AFill(arr , "a" , , 3)
			AFill(arr , "a" , 1 , 3)
			Assert.True( arr[1] == "a")
			Assert.True( arr[2] == "a")
			Assert.True( arr[3] == "a")
			Assert.True( arr[4] == 4)

			AFill(arr)
			Assert.True( arr[1] == NIL)
			Assert.True( arr[2] == NIL)
			Assert.True( arr[3] == NIL)
			Assert.True( arr[4] == NIL)
			
			AFill(arr , #abc , 3 , 10)
			Assert.True( arr[1] == NIL)
			Assert.True( arr[2] == NIL)
			Assert.True( arr[3] == #abc)
			Assert.True( arr[4] == #abc)
			
//			AFill(arr , 0 , , 1)
			AFill(arr , 0 , 1 , 1)
			Assert.True( arr[1] == 0)
			Assert.True( arr[2] == NIL)
			Assert.True( arr[3] == #abc)
			Assert.True( arr[4] == #abc)

 		[Trait("Category", "Array")];
		[Fact];
		METHOD ArraySignedUnsignedTest() AS VOID
			LOCAL a AS ARRAY
			a := ArrayNew(10L)
			a := ArrayNew(10U)
			LOCAL n1 AS INT
			LOCAL n2 AS DWORD
			FOR n1 := 1 TO 10
				a[n1] := n1
			NEXT
			FOR n2 := 1 TO 10
				a[n2] := n2
			NEXT
			Assert.Equal(1, (INT) a[1L])
			Assert.Equal(2, (INT) a[2U])

		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
