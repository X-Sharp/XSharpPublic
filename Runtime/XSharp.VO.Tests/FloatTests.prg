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



BEGIN NAMESPACE XSharp.VO.Tests

	CLASS FloatTests
		[Fact, Trait("Category", "Numeric")];
		METHOD CONTIMETest() AS VOID
			Assert.Equal("13:34:54",ConTime((DWORD)13,(DWORD)34,(DWORD)54))
		RETURN

		[Fact, Trait("Category", "Numeric")];
		METHOD FracTest() AS VOID
			Assert.Equal((FLOAT)120,Fact((DWORD)5))
		RETURN

        [Fact, Trait("Category", "Numeric")];
        METHOD FloatConvertFromObject AS VOID
            LOCAL o AS OBJECT
            LOCAL f AS FLOAT
            o := 1.234d
            f := Object2Float(o)
            Assert.Equal ( (REAL8) 1.234, (REAL8) f)
            o := 1.234m
            f := Object2Float(o)
            Assert.Equal ( (REAL8) 1.234, (REAL8) f)
           o := 1234U
            f := Object2Float(o)
            Assert.Equal ( (REAL8) 1234, (REAL8) f)
            o := 1234L
            f := Object2Float(o)
            Assert.Equal ( (REAL8) 1234, (REAL8) f)
             
        [Fact, Trait("Category", "Numeric")];
        METHOD FloatComparisons AS VOID
			LOCAL f1,f2 AS FLOAT
			LOCAL fDelta AS FLOAT
			fDelta := SetFloatDelta()

			SetFloatDelta(0.0000000001)
			
			f1 := 0.00001
			f2 := 0.00001
			Assert.False( f1 < f2  )
			Assert.True( f1 <= f2 )
			Assert.False( f1 > f2  )
			Assert.True( f1 >= f2 )
			Assert.True( f1 == f2 )

			f1 := 0.00001
			f2 := 0.00002
			Assert.True( f1 < f2  )
			Assert.True( f1 <= f2 )
			Assert.False( f1 > f2  )
			Assert.False( f1 >= f2 )
			Assert.False( f1 == f2 )

			f1 := 0.00002
			f2 := 0.00001
			Assert.False( f1 < f2  )
			Assert.False( f1 <= f2 )
			Assert.True( f1 > f2  )
			Assert.True( f1 >= f2 )
			Assert.False( f1 == f2 )

			f1 := -0.00001
			f2 := -0.00002
			Assert.False( f1 < f2  )
			Assert.False( f1 <= f2 )
			Assert.True( f1 > f2  )
			Assert.True( f1 >= f2 )
			Assert.False( f1 == f2 )

			SetFloatDelta(0.01)

			f1 := 0.00001
			f2 := 0.00001
			Assert.False( f1 < f2  )
			Assert.True( f1 <= f2 )
			Assert.False( f1 > f2  )
			Assert.True( f1 >= f2 )
			Assert.True( f1 == f2 )

			f1 := 0.00001
			f2 := 0.00002
			Assert.False( f1 < f2  )
			Assert.True( f1 <= f2 )
			Assert.False( f1 > f2  )
			Assert.True( f1 >= f2 )
			Assert.True( f1 == f2 )

			f1 := 0.00002
			f2 := 0.00001
			Assert.False( f1 < f2  )
			Assert.True( f1 <= f2 )
			Assert.False( f1 > f2  )
			Assert.True( f1 >= f2 )
			Assert.True( f1 == f2 )

			f1 := -0.00001
			f2 := -0.00002
			Assert.False( f1 < f2  )
			Assert.True( f1 <= f2 )
			Assert.False( f1 > f2  )
			Assert.True( f1 >= f2 )
			Assert.True( f1 == f2 )

			SetFloatDelta(fDelta)
    END CLASS
            
END NAMESPACE
