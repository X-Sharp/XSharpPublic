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

#pragma warnings(165, off)  // unassigned variables
#pragma warnings(219, off)  // assigned but not used

BEGIN NAMESPACE XSharp.RT.Tests

    CLASS MathTests

        [Fact, Trait("Category", "Math")];
        METHOD AbsIntTest() AS VOID
            Assert.Equal(5,  (INT) Abs(-5))
            Assert.Equal(0,  (INT) Abs(0))
            Assert.Equal(5,  (INT) Abs(5))
            Assert.Equal(100,(INT) Abs(-100))
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD AbsInt64Test() AS VOID
            LOCAL n64 AS INT64
            n64 := -1000000000000L
            Assert.Equal(1000000000000L, (INT64) Abs(n64))
            n64 := 1000000000000L
            Assert.Equal(1000000000000L, (INT64) Abs(n64))
            n64 := 0L
            Assert.Equal(0L, (INT64) Abs(n64))
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD AbsFloatTest() AS VOID
            LOCAL fDelta AS REAL8
            fDelta := SetFloatDelta()
            SetFloatDelta(0.0000001)
            Assert.True( Abs(-3.14) == 3.14 )
            Assert.True( Abs(3.14)  == 3.14 )
            Assert.True( Abs(0.0)   == 0.0  )
            Assert.True( Abs(-0.001) == 0.001 )
            SetFloatDelta(fDelta)
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD PITest() AS VOID
            Assert.Equal(Math.PI, (REAL8) PI())
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD AngleConversionDToRTest() AS VOID
            // DToR: degrees to radians
            Assert.True( Math.Abs(DToR(0.0)   - 0.0)          < 0.0000001 )
            Assert.True( Math.Abs(DToR(90.0)  - Math.PI/2.0)  < 0.0000001 )
            Assert.True( Math.Abs(DToR(180.0) - Math.PI)       < 0.0000001 )
            Assert.True( Math.Abs(DToR(360.0) - Math.PI*2.0)   < 0.0000001 )
            Assert.True( Math.Abs(DToR(-90.0) - -Math.PI/2.0) < 0.0000001 )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD AngleConversionRToDTest() AS VOID
            // RToD: radians to degrees
            Assert.True( Math.Abs(RToD(0.0)        - 0.0)   < 0.0000001 )
            Assert.True( Math.Abs(RToD(Math.PI/2)  - 90.0)  < 0.0000001 )
            Assert.True( Math.Abs(RToD(Math.PI)    - 180.0) < 0.0000001 )
            Assert.True( Math.Abs(RToD(Math.PI*2)  - 360.0) < 0.0000001 )
            Assert.True( Math.Abs(RToD(-Math.PI/2) - -90.0) < 0.0000001 )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD AngleRoundTripTest() AS VOID
            // Converting to radians and back should give the original value
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs(RToD(DToR(0.0))   - 0.0)   < epsilon )
            Assert.True( Math.Abs(RToD(DToR(45.0))  - 45.0)  < epsilon )
            Assert.True( Math.Abs(RToD(DToR(90.0))  - 90.0)  < epsilon )
            Assert.True( Math.Abs(RToD(DToR(180.0)) - 180.0) < epsilon )
            Assert.True( Math.Abs(RToD(DToR(-45.0)) - -45.0) < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD SinTest() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) Sin(0.0)      - 0.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Sin(PI()/2)   - 1.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Sin(PI())      - 0.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Sin(3*PI()/2) - -1.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Sin(-PI()/2)  - -1.0) < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD CosTest() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) Cos(0.0)     - 1.0)  < epsilon )
            Assert.True( Math.Abs((REAL8) Cos(PI()/2)  - 0.0)  < epsilon )
            Assert.True( Math.Abs((REAL8) Cos(PI())    - -1.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Cos(-PI()/2) - 0.0)  < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD TanTest() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) Tan(0.0)    - 0.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Tan(PI()/4) - 1.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Tan(-PI()/4) - -1.0) < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD ATanTest() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) ATan(0.0)  - 0.0)       < epsilon )
            Assert.True( Math.Abs((REAL8) ATan(1.0)  - PI()/4)    < epsilon )
            Assert.True( Math.Abs((REAL8) ATan(-1.0) - -PI()/4)   < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD ASinTest() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) ASin(0.0)  - 0.0)      < epsilon )
            Assert.True( Math.Abs((REAL8) ASin(1.0)  - PI()/2)   < epsilon )
            Assert.True( Math.Abs((REAL8) ASin(-1.0) - -PI()/2)  < epsilon )
            Assert.True( Math.Abs((REAL8) ASin(0.5)  - PI()/6)   < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD ACosTest() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) ACos(1.0)  - 0.0)      < epsilon )
            Assert.True( Math.Abs((REAL8) ACos(0.0)  - PI()/2)   < epsilon )
            Assert.True( Math.Abs((REAL8) ACos(-1.0) - PI())     < epsilon )
            Assert.True( Math.Abs((REAL8) ACos(0.5)  - PI()/3)   < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD ATan2Test() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) ATan2(0.0, 1.0)  - 0.0)       < epsilon )
            Assert.True( Math.Abs((REAL8) ATan2(1.0, 1.0)  - PI()/4)    < epsilon )
            Assert.True( Math.Abs((REAL8) ATan2(1.0, 0.0)  - PI()/2)    < epsilon )
            Assert.True( Math.Abs((REAL8) ATan2(-1.0, 1.0) - -PI()/4)   < epsilon )
            Assert.True( Math.Abs((REAL8) ATan2(0.0, -1.0) - PI())      < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD ExpTest() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) Exp(0.0) - 1.0)     < epsilon )
            Assert.True( Math.Abs((REAL8) Exp(1.0) - Math.E)  < epsilon )
            Assert.True( Math.Abs((REAL8) Exp(-1.0) - 1.0/Math.E) < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD LogTest() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) Log(1.0)    - 0.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Log(Math.E) - 1.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Log(Math.E * Math.E) - 2.0) < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD Log10Test() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) Log10(1.0)   - 0.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Log10(10.0)  - 1.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Log10(100.0) - 2.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Log10(0.1)   - -1.0) < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD PowTest() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) Pow(2.0, 3.0)  - 8.0)  < epsilon )
            Assert.True( Math.Abs((REAL8) Pow(5.0, 0.0)  - 1.0)  < epsilon )
            Assert.True( Math.Abs((REAL8) Pow(2.0, -1.0) - 0.5)  < epsilon )
            Assert.True( Math.Abs((REAL8) Pow(10.0, 2.0) - 100.0) < epsilon )
            Assert.True( Math.Abs((REAL8) Pow(1.0, 100.0) - 1.0) < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD SQrtTest() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) SQrt(0.0)  - 0.0) < epsilon )
            Assert.True( Math.Abs((REAL8) SQrt(1.0)  - 1.0) < epsilon )
            Assert.True( Math.Abs((REAL8) SQrt(4.0)  - 2.0) < epsilon )
            Assert.True( Math.Abs((REAL8) SQrt(9.0)  - 3.0) < epsilon )
            Assert.True( Math.Abs((REAL8) SQrt(2.0)  - Math.Sqrt(2.0)) < epsilon )
            Assert.True( Math.Abs((REAL8) SQrt(100.0) - 10.0) < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD CeilIntegerTest() AS VOID
            // Ceiling of an integer is unchanged
            Assert.Equal(5,  (INT) Ceil(5))
            Assert.Equal(-5, (INT) Ceil(-5))
            Assert.Equal(0,  (INT) Ceil(0))
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD CeilFloatTest() AS VOID
            Assert.Equal(3,  (INT) Ceil(2.1))
            Assert.Equal(3,  (INT) Ceil(2.9))
            Assert.Equal(3,  (INT) Ceil(3.0))
            Assert.Equal(-2, (INT) Ceil(-2.1))
            Assert.Equal(-2, (INT) Ceil(-2.9))
            Assert.Equal(0,  (INT) Ceil(0.0))
            Assert.Equal(1,  (INT) Ceil(0.0001))
            Assert.Equal(0,  (INT) Ceil(-0.9999))
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD FloorIntegerTest() AS VOID
            // Floor of an integer is unchanged
            Assert.Equal(5,  (INT) Floor(5))
            Assert.Equal(-5, (INT) Floor(-5))
            Assert.Equal(0,  (INT) Floor(0))
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD FloorFloatTest() AS VOID
            Assert.Equal(2,  (INT) Floor(2.1))
            Assert.Equal(2,  (INT) Floor(2.9))
            Assert.Equal(2,  (INT) Floor(2.0))
            Assert.Equal(-3, (INT) Floor(-2.1))
            Assert.Equal(-3, (INT) Floor(-2.9))
            Assert.Equal(0,  (INT) Floor(0.0))
            Assert.Equal(0,  (INT) Floor(0.9999))
            Assert.Equal(-1, (INT) Floor(-0.0001))
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD ModIntTest() AS VOID
            Assert.Equal(1, (INT) Mod(10, 3))
            Assert.Equal(0, (INT) Mod(9, 3))
            Assert.Equal(2, (INT) Mod(2, 5))
            Assert.Equal(0, (INT) Mod(0, 5))
            Assert.Equal(-1, (INT) Mod(-10, 3))
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD ModInt64Test() AS VOID
            LOCAL n64 AS INT64
            n64 := 1000000000000L
            Assert.Equal(1L, (INT64) Mod(n64 + 1L, n64))
            Assert.Equal(0L, (INT64) Mod(n64, n64))
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD ModFloatTest() AS VOID
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            Assert.True( Math.Abs((REAL8) Mod(10.5, 3.0) - 1.5)  < epsilon )
            Assert.True( Math.Abs((REAL8) Mod(7.0, 2.5)  - 2.0)  < epsilon )
            Assert.True( Math.Abs((REAL8) Mod(5.0, 5.0)  - 0.0)  < epsilon )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD Float2LongTest() AS VOID
            Assert.Equal(3,    Float2Long(3.7))
            Assert.Equal(-3,   Float2Long(-3.7))
            Assert.Equal(0,    Float2Long(0.0))
            Assert.Equal(100,  Float2Long(100.0))
            Assert.Equal(-100, Float2Long(-100.4))
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD RandRangeTest() AS VOID
            LOCAL f AS FLOAT
            // Rand() should return a value in [0, 1)
            f := Rand()
            Assert.True( (REAL8) f >= 0.0 )
            Assert.True( (REAL8) f < 1.0 )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD RandSeededTest() AS VOID
            LOCAL f1 AS FLOAT
            LOCAL f2 AS FLOAT
            // Two calls with the same seed should return the same value
            f1 := Rand(42)
            f2 := Rand(42)
            Assert.Equal( (REAL8) f1, (REAL8) f2 )
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD SinCosIdentityTest() AS VOID
            // sin²(x) + cos²(x) = 1 for all x
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            LOCAL x AS REAL8
            FOREACH x IN <REAL8>{ 0.0, 0.5, 1.0, 1.5, 2.0, PI()/3, PI()/4 }
                LOCAL sinVal AS REAL8
                LOCAL cosVal AS REAL8
                sinVal := (REAL8) Sin(x)
                cosVal := (REAL8) Cos(x)
                Assert.True( Math.Abs(sinVal*sinVal + cosVal*cosVal - 1.0) < epsilon )
            NEXT
        RETURN

        [Fact, Trait("Category", "Math")];
        METHOD ExpLogInverseTest() AS VOID
            // log(exp(x)) should equal x
            LOCAL epsilon AS REAL8
            epsilon := 0.0000001
            LOCAL x AS REAL8
            FOREACH x IN <REAL8>{ 0.0, 0.5, 1.0, 2.0, 5.0 }
                Assert.True( Math.Abs((REAL8) Log(Exp(x)) - x) < epsilon )
            NEXT
        RETURN

    END CLASS

END NAMESPACE
