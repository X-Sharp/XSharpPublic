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
	    STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro

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


		[Fact, Trait("Category", "Date and Time")];
            METHOD QuarterTests() AS VOID
            Assert.Equal( Quarter ( ConDate ( 2020, 1 , 1  ) ) , 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 2 , 1  ) ) , 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 3 , 1  ) ) , 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 4 , 1  ) ) , 2 )
            Assert.Equal( Quarter ( ConDate ( 2020, 5 , 1  ) ) , 2 )
            Assert.Equal( Quarter ( ConDate ( 2020, 6 , 1  ) ) , 2 )
            Assert.Equal( Quarter ( ConDate ( 2020, 7 , 1  ) ) , 3 )
            Assert.Equal( Quarter ( ConDate ( 2020, 8 , 1  ) ) , 3 )
            Assert.Equal( Quarter ( ConDate ( 2020, 9 , 1  ) ) , 3 )
            Assert.Equal( Quarter ( ConDate ( 2020, 10 , 1 ) ) , 4 )
            Assert.Equal( Quarter ( ConDate ( 2020, 11 , 1 ) ) , 4 )
            Assert.Equal( Quarter ( ConDate ( 2020, 12 , 1 ) ) , 4 )

            Assert.Equal( Quarter ( ConDate ( 2020, 1 , 1  ) , 1 ), 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 2 , 1  ) , 1 ), 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 3 , 1  ) , 1 ), 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 4 , 1  ) , 1) ,2 )
            Assert.Equal( Quarter ( ConDate ( 2020, 5 , 1  ) , 1) ,2  )
            Assert.Equal( Quarter ( ConDate ( 2020, 6 , 1  ) , 1) ,2  )
            Assert.Equal( Quarter ( ConDate ( 2020, 7 , 1  ) , 1) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2020, 8 , 1  ) , 1) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2020, 9 , 1  ) , 1) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2020, 10 , 1  ) , 1), 4 )
            Assert.Equal( Quarter ( ConDate ( 2020, 11 , 1  ) , 1), 4 )
            Assert.Equal( Quarter ( ConDate ( 2020, 12 , 1  ) , 1), 4 )

            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 1  ) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 2  ) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 3  ) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 4  ) ,2  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 5  ) ,2 )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 6  ) ,2  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 7  ) ,1  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 8  ) ,1  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 9  ) ,1  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 10  ), 4 )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 11  ), 4 )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 12  ), 4 )

            Assert.Equal( Quarter ( ConDate ( 2018, 1, 14    ), 4) ,4   )
            Assert.Equal( Quarter ( ConDate ( 2018, 3, 14    ), 4) ,4   )
            Assert.Equal( Quarter ( ConDate ( 2018, 4, 14    ), 4) ,1   )
            Assert.Equal( Quarter ( ConDate ( 2018, 6, 14    ), 4) ,1   )
            Assert.Equal( Quarter ( ConDate ( 2018, 7, 14    ), 4) ,2   )
            Assert.Equal( Quarter ( ConDate ( 2018, 9, 14    ), 4) ,2   )
            Assert.Equal( Quarter ( ConDate ( 2018, 10, 14    ), 4), 3  )
            Assert.Equal( Quarter ( ConDate ( 2018, 12, 14    ), 4), 3  )

            Assert.Equal( Quarter ( {^2000-9-23}, 7 ) , 1)
            Assert.Equal( Quarter ( NULL_DATE ) , 0)
            Assert.Equal( Quarter ( DateTime{} ) , 0  )
            Assert.Equal( Quarter ( 2020.04.20 ) , 2         )

		[Fact, Trait("Category", "Date and Time")];
            METHOD GoMonthTests() AS VOID

            Assert.Equal(GoMonth ( NULL_DATE , -12 ) , NULL_DATE)
            Assert.Equal(GoMonth ( DateTime{} , -12 ) , NULL_DATE)
            Assert.Equal(GoMonth ( Today() , 0 ) , Today()  )
            Assert.Equal(GoMonth ( ConDate ( 9999, 12 , 31 )  , 1 ) , NULL_DATE) // this causes a suppressed exception !
            Assert.Equal(GoMonth ( ConDate ( 1753, 1 , 1 )  , 0 ) , ConDate ( 1753, 1 , 1 ))
            Assert.Equal(GoMonth ( ConDate ( 1753, 1 , 1 )  , -1 ) , NULL_DATE) // VFP year limit is 1753
            Assert.Equal(GoMonth({^1998-12-31}, 2) , ConDate ( 1999 , 2 , 28 ) )
            Assert.Equal(GoMonth({^1998-12-31}, -2) , ConDate ( 1998 , 10 , 31  ) )

		[Fact, Trait("Category", "Date and Time")];
            METHOD WeekTests() AS VOID
                Assert.Equal(Week(1998.02.16)  , 8)
                Assert.Equal(Week(1998.02.16,1,1)  , 8)
                Assert.Equal(Week(2010.01.03,2,4)  , 1)
                Assert.Equal(Week(2013.01.05,1,1)  , 1)    // Sunday, 1st week has jan 1
                Assert.Equal(Week(2013.01.05,2,1)  , 1)    // Sunday, 1st week has 4 days
                Assert.Equal(Week(2013.01.05,3,1)  , 53)    // Sunday, 1st week has 7 days





END CLASS

END NAMESPACE
