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
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS DateTests
	
		[Fact, Trait("Category", "Date and Time")];
		METHOD ConversionTests() AS VOID
            LOCAL d AS DATE
            LOCAL dt AS DateTime
            dt := DateTime{2020,1,1,11,12,13}

            Assert.Equal(11, Hour(dt))
            Assert.Equal(12, Minute(dt))
            Assert.Equal(13, Sec(dt))
            d := ConDate(2020,1,1)
            Assert.Equal(d, TTod(dt))
            dt := DateTime{2020,1,1}
            Assert.True( DToT(d)== dt)
            
            


	END CLASS

END NAMESPACE
