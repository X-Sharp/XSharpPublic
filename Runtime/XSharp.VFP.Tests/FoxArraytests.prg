USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit
BEGIN NAMESPACE XSharp.VFP.Tests

#pragma warnings(1998, off)

	CLASS FoxArrayTests
        PRIVATE STATIC gate := Object{} AS OBJECT
        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro

        [Fact, Trait("Category", "FoxArray")];
		ASYNC METHOD SimpleArrayTests() AS VOID
            LOCAL ARRAY a(1)
            Dimension a(10)
            // You cannot assign an array to an element of a FOX array
            AWAIT Assert.ThrowsAsync<Error>( { => a[1] := {1,2,3} })

            Assert.True( IsNil(a[1]),__LINE__:ToString())
            Assert.True( IsNil(a[2]),__LINE__:ToString())
            FillArray(a)
            Assert.True( a[1] == 1 ,__LINE__:ToString())
            Assert.True( a[2] == 2,__LINE__:ToString())
            a:Redim(5,2)
            Assert.True( a[1,1] == 1 ,__LINE__:ToString())
            Assert.True( a[1,2] == 2,__LINE__:ToString())
            Assert.True( a[2,1] == 3,__LINE__:ToString())
            Assert.True( a[5,2] == 10,__LINE__:ToString())
            // Alen with nArrayAttribute
            Assert.Equal(5U,  ALen(a,1) )
            Assert.Equal(2U,  ALen(a,2) )
            Assert.Equal(10U,  ALen(a) )
            // AElement = returns element number
            Assert.True( AElement(a, 1,1) == 1,__LINE__:ToString())
            Assert.True( AElement(a, 1,2) == 2,__LINE__:ToString())
            Assert.True( AElement(a, 2,1) == 3,__LINE__:ToString())
            Assert.True( AElement(a, 2,2) == 4,__LINE__:ToString())
            Assert.True( AElement(a, 3,1) == 5,__LINE__:ToString())
            Assert.True( AElement(a, 3,2) == 6,__LINE__:ToString())
            Assert.True( AElement(a, 4,1) == 7,__LINE__:ToString())
            Assert.True( AElement(a, 4,2) == 8,__LINE__:ToString())
            Assert.True( AElement(a, 5,1) == 9,__LINE__:ToString())
            Assert.True( AElement(a, 5,2) == 10,__LINE__:ToString())
            // Now delete column 1
            FillArray(a)
            Assert.Equal(1U,  ADel(a, 1, 2) )
            Assert.True( a[ 1,1] == 2)
            Assert.True( a[ 1,2] == NIL)
            Assert.True( a[ 2,1] == 4)
            Assert.True( a[ 2,2] == NIL)
            Assert.True( a[ 3,1] == 6)
            Assert.True( a[ 3,2] == NIL)
            Assert.True( a[ 4,1] == 8)
            Assert.True( a[ 4,2] == NIL)
            Assert.True( a[ 5,1] == 10)
            Assert.True( a[ 5,2] == NIL)
            // Now insert column 1
            FillArray(a)
            Assert.Equal(1U,  AIns(a, 1, 2) )
            Assert.True( a[ 1,1] == NIL)
            Assert.True( a[ 1,2] == 1)
            Assert.True( a[ 2,1] == NIL)
            Assert.True( a[ 2,2] == 3)
            Assert.True( a[ 3,1] == NIL)
            Assert.True( a[ 3,2] == 5)
            Assert.True( a[ 4,1] == NIL)
            Assert.True( a[ 4,2] == 7)
            Assert.True( a[ 5,1] == NIL)
            Assert.True( a[ 5,2] == 9)
            FillArray(a)
            Assert.Equal(1U, AIns(a, 2, 2) )
            Assert.True( a[ 1,1] == 1)
            Assert.True( a[ 1,2] == NIL)
            Assert.True( a[ 2,1] == 3)
            Assert.True( a[ 2,2] == NIL)
            Assert.True( a[ 3,1] == 5)
            Assert.True( a[ 3,2] == NIL)
            Assert.True( a[ 4,1] == 7)
            Assert.True( a[ 4,2] == NIL)
            Assert.True( a[ 5,1] == 9)
            Assert.True( a[ 5,2] == NIL)
            FillArray(a)
            // delete row 1
            Assert.Equal(1U, ADel(a, 1, 1) )
            Assert.True( a[ 1,1] == 3)
            Assert.True( a[ 1,2] == 4)
            Assert.True( a[ 2,1] == 5)
            Assert.True( a[ 2,2] == 6)
            Assert.True( a[ 3,1] == 7)
            Assert.True( a[ 3,2] == 8)
            Assert.True( a[ 4,1] == 9)
            Assert.True( a[ 4,2] == 10)
            Assert.True( a[ 5,1] == NIL)
            Assert.True( a[ 5,2] == NIL)

            FillArray(a)
            // delete row 5
            Assert.Equal(1U, ADel(a, 5, 1) )
            Assert.True( a[ 1,1] == 1)
            Assert.True( a[ 1,2] == 2)
            Assert.True( a[ 2,1] == 3)
            Assert.True( a[ 2,2] == 4)
            Assert.True( a[ 3,1] == 5)
            Assert.True( a[ 3,2] == 6)
            Assert.True( a[ 4,1] == 7)
            Assert.True( a[ 4,2] == 8)
            Assert.True( a[ 5,1] == NIL)
            Assert.True( a[ 5,2] == NIL)

            FillArray(a)
            // delete row 5
            Assert.Equal(1U, AIns(a, 1, 1))
            Assert.True( a[ 1,1] == NIL)
            Assert.True( a[ 1,2] == NIL)
            Assert.True( a[ 2,1] == 1)
            Assert.True( a[ 2,2] == 2)
            Assert.True( a[ 3,1] == 3)
            Assert.True( a[ 3,2] == 4)
            Assert.True( a[ 4,1] == 5)
            Assert.True( a[ 4,2] == 6)
            Assert.True( a[ 5,1] == 7)
            Assert.True( a[ 5,2] == 8)


            FillArray(a)
            // delete row 5
            Assert.Equal(1U, AIns(a, 5, 1))
            Assert.True( a[ 1,1] == 1)
            Assert.True( a[ 1,2] == 2)
            Assert.True( a[ 2,1] == 3)
            Assert.True( a[ 2,2] == 4)
            Assert.True( a[ 3,1] == 5)
            Assert.True( a[ 3,2] == 6)
            Assert.True( a[ 4,1] == 7)
            Assert.True( a[ 4,2] == 8)
            Assert.True( a[ 5,1] == NIL)
            Assert.True( a[ 5,2] == NIL)


       // [Fact, Trait("Category", "FoxArray")];
//		METHOD UsualArrayTests() AS VOID
//            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
//            LOCAL ARRAY a(1)
//            DIMENSION a(10)
//            FillArray(a)
//            LOCAL u := a AS USUAL
//            Assert.True(IsArray(u))
//            Assert.True(u[1] == 1)
//            Assert.True(u[10] == 10)
//            DIMENSION u(5,2)
//            Assert.True(u[1,1] == 1)
//            Assert.True(u[1,2] == 2)
//            Assert.True(u[5,2] == 10)
//            //Assert.True(u[6,2] == 10)

        STATIC METHOD FillArray(a AS __FoxArray) AS VOID
            FOR VAR i := 1 TO ALen(a)
                a[i] := i
            NEXT
        [Fact, Trait("Category", "FoxArray")];
        METHOD AinsTests() AS VOID
            LOCAL ARRAY aOneDim ( 4 )

            aOneDim [1] := 1
            aOneDim [2] := 2
            aOneDim [3] := 3
            aOneDim [4] := 4

            ?

            AIns ( aOneDim , 1 )
            ShowArray ( aOneDim )
            ?

            AIns ( aOneDim , 1 , 1 )
            ShowArray ( aOneDim )
            ?

            AIns ( aOneDim , 1 , 0 )
            ShowArray ( aOneDim )
            ?

            AIns ( aOneDim , 1 )
            ShowArray ( aOneDim )
            ?
            FOR VAR i := 1 TO ALen ( aOneDim )

	            Assert.True(IsNil ( aOneDim[i] ),AsString(aOneDim[i]))
	            Assert.True(IsLogic ( aOneDim[i] ),AsString(aOneDim[i]))
            NEXT

            ? "-------- two-dim array ----"
            ?

            DIMENSION aTwoDim ( 2, 4  )
            FOR VAR i := 1 TO ALen (aTwoDim)
               aTwoDim[i] := i
            ENDFOR
            ShowArray ( aTwoDim )
            ?


            // ----  There are 3 options to get this array content

            /*

            a[1] [1,1] = 1 (N)
            a[2] [1,2] = 2 (N)
            a[3] [1,3] = 3 (N)
            a[4] [1,4] = 4 (N)
            a[5] [2,1] = .F. (Nil)
            a[6] [2,2] = .F. (Nil)
            a[7] [2,3] = .F. (Nil)
            a[8] [2,4] = .F. (Nil)

            */

            AIns ( aTwoDim , 2 , 1  )
            ShowArray ( aTwoDim )
            ?

            AIns ( aTwoDim , 2 , 0  )
            ShowArray ( aTwoDim )
            ?

            AIns ( aTwoDim , 2 )
            ShowArray ( aTwoDim )

            // ? AIns ( aTwoDim , 0 , 2 ) //  Exception: 'nElementNumber' number is out of range
            // ? AIns ( aTwoDim , 0  ) //  Exception: 'nElementNumber' number is out of range
            // ? AIns ( aTwoDim , 3  ) //  Exception: 'nElementNumber' number is out of range
            // ? AIns ( aTwoDim , 2 , 4 ) // Exception 'nInsertType' number is out of range

            // ------------------------

            FOR VAR i := 1 TO ALen (aTwoDim)
               aTwoDim[i] := i
            ENDFOR

            AIns ( aTwoDim , 1 , 2  )

            Assert.True(IsNil(aTwoDim [1,1]))
            Assert.True(IsNil(aTwoDim [2,1]))
            Assert.True(IsLogic(aTwoDim [1,1]))
            Assert.True(IsLogic(aTwoDim [2,1]))
            /*
            a[1] [1,1] = .F. (NIL)
            a[2] [1,2] = 1 (N)
            a[3] [1,3] = 2 (N)
            a[4] [1,4] = 3 (N)
            a[5] [2,1] = .F. (NIL)
            a[6] [2,2] = 5 (N)
            a[7] [2,3] = 6 (N)
            a[8] [2,4] = 7 (N)
            */
            ShowArray ( aTwoDim )
            ?
            AIns ( aTwoDim , 2 , 2  )
            Assert.True(IsNil(aTwoDim [1,2]),__LINE__:ToString())
            Assert.True(IsNil(aTwoDim [2,2]),__LINE__:ToString())
            Assert.True(IsLogic(aTwoDim [1,2]),__LINE__:ToString())
            Assert.True(IsLogic(aTwoDim [2,2]),__LINE__:ToString())

        [Fact, Trait("Category", "FoxArray")];
        METHOD ASubScriptTests() AS VOID
            LOCAL x, y AS DWORD
            LOCAL ARRAY aTest(1)
            DIMENSION aTest(4)

             aTest[1] :=  'E'
             aTest[2] :=  'F'
             aTest[3] :=  'G'
             aTest[4] :=  'H'
            ShowArray(aTest)

            ? aTest [ ASubscript ( aTest , 4 , 1) ] // "H"
            ? aTest [ ASubscript ( aTest , 2 , 1) ] // "F"

            Assert.True( "H" == (STRING) aTest [ASubscript ( aTest , 4 , 1)])
            Assert.True( "F" == (STRING) aTest [ASubscript ( aTest , 2 , 1)])
            // ? aTest [ ASubscript ( aTest , 4 ) ] // Fox and X# compile error - third param is missing

            // ? ASubscript ( aTest , 8 , 1) // throws the exception 'nElementNumber' number is out of range
            // ? ASubscript ( aTest , 4 , 2) // throws the exception "a one-dimensional array has no columns"

            DIMENSION aTest(2,3)

            /*

            now the content OF the ARRAY looks like

            aTest[1] [1,1] = E (C)
            aTest[2] [1,2] = F (C)
            aTest[3] [1,3] = G (C)
            aTest[4] [2,1] = H (C)
            aTest[5] [2,2] = .F. (NIL)
            aTest[6] [2,3] = .F. (NIL)

            */


            x := ASubscript (aTest , 4 , 1 ) // Element 4 , 1 = get the corresponding row subscript
            y := ASubscript (aTest , 4 , 2 ) // Element 4 , 2 = get the corresponding col subscript
            ? "aTest ["+x:Tostring()+"," + y:Tostring()+ "] must show 'H':" , aTest [x,y]

            Assert.True( "H" == (STRING) aTest [x,y])


            x := ASubscript (aTest , 3 , 1 ) // Element 3 , 1 = get the corresponding row subscript
            y := ASubscript (aTest , 3 , 2 ) // Element 3 , 2 = get the corresponding col subscript
            ? "aTest ["+x:Tostring()+"," + y:Tostring()+ "] must show 'G':" , aTest [x,y]
            Assert.True( "G" == (STRING) aTest [x,y])

            x := ASubscript (aTest , 6 , 1 ) // Element 6 , 1 = get the corresponding row subscript
            y := ASubscript (aTest , 6 , 2 ) // Element 6 , 2 = get the corresponding col subscript
            ? "aTest ["+x:Tostring()+"," + y:Tostring()+ "] must show '.F.':" , aTest [x,y]
            Assert.True( .F. == (LOGIC) aTest [x,y])

        [Fact, Trait("Category", "FoxArray")];
		ASYNC METHOD ALenTests() AS VOID
            LOCAL ARRAY arr (1)
			DIMENSION arr(1,1)
            Assert.Equal(1U, ALen(arr,0))
            Assert.Equal(1U, ALen(arr,1))
            Assert.Equal(1U, ALen(arr,2))

			DIMENSION arr(3,1)
            Assert.Equal(3U, ALen(arr,0))
            Assert.Equal(3U, ALen(arr,1))
            Assert.Equal(1U, ALen(arr,2))

			DIMENSION arr(1,3)
            Assert.Equal(3U, ALen(arr,0))
            Assert.Equal(1U, ALen(arr,1))
            Assert.Equal(3U, ALen(arr,2))
            Assert.Throws<ArgumentOutOfRangeException>( { => ALen(arr,3) == 3 })

            DIMENSION arr(4,5)
            Assert.Equal(20U, ALen(arr,0))
            Assert.Equal(4U,  ALen(arr,1))
            Assert.Equal(5U,  ALen(arr,2))

            DIMENSION arr(100)
            Assert.Equal(100U, ALen(arr,0))
            Assert.Equal(100U, ALen(arr,1))
            Assert.Equal(0U,   ALen(arr,2))
    END CLASS
END NAMESPACE
