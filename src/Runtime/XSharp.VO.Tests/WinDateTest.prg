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


// WinBool test
BEGIN NAMESPACE XSharp.VO.Tests

	CLASS WinDateTests
	 
		[Fact, Trait("Category", "Misc")]; 
		METHOD ConversionTests AS VOID
			LOCAL wd AS __WinDate
			LOCAL d AS DATE
            LOCAL u AS USUAL
			d := ToDay()
			wd := d
    		Assert.Equal(d == wd, TRUE)
            Assert.Equal(d != wd, FALSE)
			d := wd
    		Assert.Equal(d == wd, TRUE)
            Assert.Equal(d != wd, FALSE)
            Assert.Equal(d:ToString(), wd:ToString())
            u := wd
            Assert.Equal( d == u, TRUE)
            d := NULL_DATE
            wd := d
            Assert.Equal(wd:JulianValue, 0)
            d := SToD("00010101")
            wd := d
            Assert.Equal(wd:JulianValue, 2451911)
            d := SToD("19010101")
            wd := d
            Assert.Equal(wd:JulianValue, 2415386)
            
			
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
