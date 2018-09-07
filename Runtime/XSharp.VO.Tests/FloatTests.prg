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
			Assert.Equal("13:34:54",CONTIME((DWORD)13,(DWORD)34,(DWORD)54))
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
             

    END CLASS
            
END NAMESPACE
