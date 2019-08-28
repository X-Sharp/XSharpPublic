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

		[Fact, Trait("Category", "Numeric")];
        METHOD Round_Tests() AS VOID
			LOCAL fDelta AS FLOAT
			fDelta := SetFloatDelta()
			SetFloatDelta(0.0000000001)
			
			Assert.True( Round( 65.305, 2 ) == 65.31 )
			Assert.True( Round( 65.315, 2 ) == 65.32 )
			Assert.True( Round( 65.325, 2 ) == 65.33 )
			Assert.True( Round( 65.335, 2 ) == 65.34 )
			Assert.True( Round( 65.345, 2 ) == 65.35 )
			Assert.True( Round( 65.355, 2 ) == 65.36 )
			Assert.True( Round( 65.365, 2 ) == 65.37 )
			Assert.True( Round( 65.375, 2 ) == 65.38 )
			Assert.True( Round( 65.385, 2 ) == 65.39 )
			Assert.True( Round( 65.395, 2 ) == 65.40 )
			Assert.True( Round( 65.475, 2 ) == 65.48 )
			Assert.True( Round( 77.445, 2 ) == 77.45 )
			
			Assert.True( Round( -65.305, 2 ) == -65.31 )
			Assert.True( Round( -65.315, 2 ) == -65.32 )
			Assert.True( Round( -65.325, 2 ) == -65.33 )
			Assert.True( Round( -65.335, 2 ) == -65.34 )
			Assert.True( Round( -65.345, 2 ) == -65.35 )
			Assert.True( Round( -65.355, 2 ) == -65.36 )
			Assert.True( Round( -65.365, 2 ) == -65.37 )
			Assert.True( Round( -65.375, 2 ) == -65.38 )
			Assert.True( Round( -65.385, 2 ) == -65.39 )
			Assert.True( Round( -65.395, 2 ) == -65.40 )
			Assert.True( Round( -65.475, 2 ) == -65.48 )
			Assert.True( Round( -77.445, 2 ) == -77.45 )
			
			Assert.True( Round( 65.0305, 3 ) == 65.031 )
			Assert.True( Round( 65.0315, 3 ) == 65.032 )
			Assert.True( Round( 65.0325, 3 ) == 65.033 )
			Assert.True( Round( 65.0335, 3 ) == 65.034 )
			Assert.True( Round( 65.0345, 3 ) == 65.035 )
			Assert.True( Round( 65.0355, 3 ) == 65.036 )
			Assert.True( Round( 65.0365, 3 ) == 65.037 )
			Assert.True( Round( 65.0375, 3 ) == 65.038 )
			Assert.True( Round( 65.0385, 3 ) == 65.039 )
			Assert.True( Round( 65.0395, 3 ) == 65.040 )
			Assert.True( Round( 65.0475, 3 ) == 65.048 )
			Assert.True( Round( 77.0445, 3 ) == 77.045 )
			
			SetFloatDelta(fDelta)
		RETURN
		
    END CLASS
            
END NAMESPACE
