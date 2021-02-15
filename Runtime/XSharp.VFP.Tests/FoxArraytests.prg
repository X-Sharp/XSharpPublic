USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS FoxArrayTests
        [Fact, Trait("Category", "FoxArray")];
		ASYNC METHOD SimpleArrayTests() AS VOID
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            VAR a := FoxArrayCreate(10)
            // You cannot assign an array to an element of a FOX array
            AWAIT Assert.ThrowsAsync<Error>( { => a[1] := {1,2,3} })
            
            Assert.True( IsNil(a[1]) )
            Assert.True( IsNil(a[2]))
            FillArray(a)
            Assert.True( a[1] == 1 )
            Assert.True( a[2] == 2)
            a:Redim(5,2)
            Assert.True( a[1,1] == 1 )
            Assert.True( a[1,2] == 2)
            Assert.True( a[2,1] == 3)
            Assert.True( a[5,2] == 10)
            // Alen with nArrayAttribute
            Assert.True( ALen(a,1) == 5)
            Assert.True( ALen(a,2) == 2)
            Assert.True( ALen(a) == 10)
            // AElement = like index operator
            Assert.True( AElement(a, 1,1) == 1)
            Assert.True( AElement(a, 1,2) == 2)
            Assert.True( AElement(a, 2,1) == 3)
            Assert.True( AElement(a, 2,2) == 4)
            Assert.True( AElement(a, 3,1) == 5)
            Assert.True( AElement(a, 3,2) == 6)
            Assert.True( AElement(a, 4,1) == 7)
            Assert.True( AElement(a, 4,2) == 8)
            Assert.True( AElement(a, 5,1) == 9)
            Assert.True( AElement(a, 5,2) == 10)
            // Now delete column 1
            FillArray(a)
            ADel(a, 1, 2)
            Assert.True( AElement(a, 1,1) == 2)
            Assert.True( AElement(a, 1,2) == NIL)
            Assert.True( AElement(a, 2,1) == 4)
            Assert.True( AElement(a, 2,2) == NIL)
            Assert.True( AElement(a, 3,1) == 6)
            Assert.True( AElement(a, 3,2) == NIL)
            Assert.True( AElement(a, 4,1) == 8)
            Assert.True( AElement(a, 4,2) == NIL)
            Assert.True( AElement(a, 5,1) == 10)
            Assert.True( AElement(a, 5,2) == NIL)
            // Now insert column 1
            FillArray(a)
            AIns(a, 1, 2)
            Assert.True( AElement(a, 1,1) == NIL)
            Assert.True( AElement(a, 1,2) == 1)
            Assert.True( AElement(a, 2,1) == NIL)
            Assert.True( AElement(a, 2,2) == 3)
            Assert.True( AElement(a, 3,1) == NIL)
            Assert.True( AElement(a, 3,2) == 5)
            Assert.True( AElement(a, 4,1) == NIL)
            Assert.True( AElement(a, 4,2) == 7)
            Assert.True( AElement(a, 5,1) == NIL)
            Assert.True( AElement(a, 5,2) == 9)
            FillArray(a)
            AIns(a, 2, 2)
            Assert.True( AElement(a, 1,1) == 1)
            Assert.True( AElement(a, 1,2) == NIL)
            Assert.True( AElement(a, 2,1) == 3)
            Assert.True( AElement(a, 2,2) == NIL)
            Assert.True( AElement(a, 3,1) == 5)
            Assert.True( AElement(a, 3,2) == NIL)
            Assert.True( AElement(a, 4,1) == 7)
            Assert.True( AElement(a, 4,2) == NIL)
            Assert.True( AElement(a, 5,1) == 9)
            Assert.True( AElement(a, 5,2) == NIL)
            FillArray(a)
            // delete row 1
            ADel(a, 1, 1)
            Assert.True( AElement(a, 1,1) == 3)
            Assert.True( AElement(a, 1,2) == 4)
            Assert.True( AElement(a, 2,1) == 5)
            Assert.True( AElement(a, 2,2) == 6)
            Assert.True( AElement(a, 3,1) == 7)
            Assert.True( AElement(a, 3,2) == 8)
            Assert.True( AElement(a, 4,1) == 9)
            Assert.True( AElement(a, 4,2) == 10)
            Assert.True( AElement(a, 5,1) == NIL)
            Assert.True( AElement(a, 5,2) == NIL)

            FillArray(a)
            // delete row 5
            ADel(a, 5, 1)
            Assert.True( AElement(a, 1,1) == 1)
            Assert.True( AElement(a, 1,2) == 2)
            Assert.True( AElement(a, 2,1) == 3)
            Assert.True( AElement(a, 2,2) == 4)
            Assert.True( AElement(a, 3,1) == 5)
            Assert.True( AElement(a, 3,2) == 6)
            Assert.True( AElement(a, 4,1) == 7)
            Assert.True( AElement(a, 4,2) == 8)
            Assert.True( AElement(a, 5,1) == NIL)
            Assert.True( AElement(a, 5,2) == NIL)

            FillArray(a)
            // delete row 5
            AIns(a, 1, 1)
            Assert.True( AElement(a, 1,1) == NIL)
            Assert.True( AElement(a, 1,2) == NIL)
            Assert.True( AElement(a, 2,1) == 1)
            Assert.True( AElement(a, 2,2) == 2)
            Assert.True( AElement(a, 3,1) == 3)
            Assert.True( AElement(a, 3,2) == 4)
            Assert.True( AElement(a, 4,1) == 5)
            Assert.True( AElement(a, 4,2) == 6)
            Assert.True( AElement(a, 5,1) == 7)
            Assert.True( AElement(a, 5,2) == 8)


            FillArray(a)
            // delete row 5
            AIns(a, 5, 1)
            Assert.True( AElement(a, 1,1) == 1)
            Assert.True( AElement(a, 1,2) == 2)
            Assert.True( AElement(a, 2,1) == 3)
            Assert.True( AElement(a, 2,2) == 4)
            Assert.True( AElement(a, 3,1) == 5)
            Assert.True( AElement(a, 3,2) == 6)
            Assert.True( AElement(a, 4,1) == 7)
            Assert.True( AElement(a, 4,2) == 8)
            Assert.True( AElement(a, 5,1) == NIL)
            Assert.True( AElement(a, 5,2) == NIL)


      [Fact, Trait("Category", "FoxArray")];
		METHOD UsualArrayTests() AS VOID
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            VAR a := FoxArrayCreate(10)
            FillArray(a)
            LOCAL u := a as USUAL
            Assert.True(IsArray(u))
            Assert.True(u[1] == 1)
            Assert.True(u[10] == 10)
            u := __FoxRedim(a,5,2)
            Assert.True(u[1,1] == 1)
            Assert.True(u[1,2] == 2)
            Assert.True(u[5,2] == 10)
            Assert.True(u[6,2] == 10)
 
        STATIC METHOD FillArray(a as __FoxArray) as void
            FOR VAR i := 1 to ALen(a)
                a[i] := i
            NEXT
    END CLASS
END NAMESPACE
