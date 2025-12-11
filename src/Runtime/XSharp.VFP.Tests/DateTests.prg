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

	// ---------------------------------------------------------------- //
	/// TToC() - Basic Tests
	// ---------------------------------------------------------------- //
	[Fact, Trait("Category", "Date and Time")];
	method TToCTests as void
        local dt as DateTime
        SetHours(12) // set 12 hours format for time tests
	    dt := DateTime{2025, 8, 13, 14, 30, 45}

	    // 0 - numeric format (same as param 1)
	    Assert.Equal("20250813143045", TToC(dt, 0))

	    // 1 - numeric format
	    Assert.Equal("20250813143045", TToC(dt, 1))

	    // 2 - just time
	    Assert.Equal("02:30:45 PM", TToC(dt, 2))

	    // 3 - ISO format
	    Assert.Equal("2025-08-13T14:30:45", TToC(dt, 3))

	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCDefaultFormatTests as void
	  local dt as DateTime
	  dt := DateTime{2025, 8, 13, 14, 30, 45}

	  // No params - should use the VFP default (depends on SET CENTURY, etc.)
	  local result := TToC(dt) as string
	  Assert.True(result != null)
	  Assert.True(result:Length > 0)

	  // with SET CENTURY ON debería incluir siglo completo
	  local oldCentury := SetCentury(true) as logic
	  local resultWithCentury := TToC(dt) as string
	  SetCentury(oldCentury)

	  Assert.True(resultWithCentury:Contains("2025")) // must contain the whole year
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCHoursFormatTests as void
	  local dt as DateTime
	  dt := DateTime{2025, 8, 13, 14, 30, 45} // 2:30 PM

	  // save current configuration
	  local oldHours := SetHours() as long
	  local oldSeconds := SetSeconds() as logic

	  try
	      // 12 hours format with seconds
	      SetHours(12)
	      SetSeconds(true)
	      Assert.Equal("02:30:45 PM", TToC(dt, 2))

	      // 12 hours format no seconds
	      SetSeconds(false)
	      Assert.Equal("02:30 PM", TToC(dt, 2))

	      // 24 hours format with seconds
	      SetHours(24)
	      SetSeconds(true)
	      Assert.Equal("14:30:45", TToC(dt, 2))

	      // 24 hours format no seconds
	      SetSeconds(false)
	      Assert.Equal("14:30", TToC(dt, 2))

	  finally
	      // restore configuration
	      SetHours(oldHours)
	      SetSeconds(oldSeconds)
	  end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCEdgeCasesTests as void
	  // midnight
      SetHours(12)
	  local midnight := DateTime{2025, 8, 13, 0, 0, 0} as DateTime
	  Assert.Equal("12:00:00 AM", TToC(midnight, 2))

	  // noon
	  local noon := DateTime{2025, 8, 13, 12, 0, 0} as DateTime
	  Assert.Equal("12:00:00 PM", TToC(noon, 2))

	  // 1 AM
	  local oneAM := DateTime{2025, 8, 13, 1, 0, 0} as DateTime
	  Assert.Equal("01:00:00 AM", TToC(oneAM, 2))

	  // 1 PM
	  local onePM := DateTime{2025, 8, 13, 13, 0, 0} as DateTime
	  Assert.Equal("01:00:00 PM", TToC(onePM, 2))

	  // 11 PM
	  local elevenPM := DateTime{2025, 8, 13, 23, 59, 59} as DateTime
	  Assert.Equal("11:59:59 PM", TToC(elevenPM, 2))
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCNumericFormatTests as void
	  local dt as DateTime
	  dt := DateTime{2025, 8, 13, 14, 30, 45}

	  // numeric format
	  Assert.Equal("20250813143045", TToC(dt, 1))

	  // no secs
	  local oldSeconds := SetSeconds(false) as logic
	  try
	      // numeric format should include seconds no matter SET SECONDS command
	      Assert.Equal("20250813143045", TToC(dt, 1))
	  finally
	      SetSeconds(oldSeconds)
	  end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCISOFormatTests as void
	  local dt as DateTime
	  dt := DateTime{2025, 8, 13, 14, 30, 45}

	  // standard ISO format
	  Assert.Equal("2025-08-13T14:30:45", TToC(dt, 3))

	  // check is not affected by locals configurations
	  local oldHours := SetHours(12) as long
	  local oldSeconds := SetSeconds(false) as logic
	  try
	      // ISO should keep 24 hours and always include seconds
	      Assert.Equal("2025-08-13T14:30:45", TToC(dt, 3))
	  finally
	      SetHours(oldHours)
	      SetSeconds(oldSeconds)
	  end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCInvalidParameterTests as void
	  local dt as DateTime
	  dt := DateTime{2025, 8, 13, 14, 30, 45}

	  // invalid parameters - should use default format or throw exception
	  try
	      local result := TToC(dt, 99) as string
	      // if no exception then should return something valid
	      Assert.True(result != null)
	      Assert.True(result:Length > 0)
	  catch e as Exception
	      // if thorws exception is ok too
	      Assert.True(e != null)
	  end try

	  // negavite parameter
	  try
	      local result := TToC(dt, -1) as string
	      Assert.True(result != null)
	      Assert.True(result:Length > 0)
	  catch e as Exception
	      Assert.True(e != null)
	  end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCDateOnlyTests as void
	  // try DATE no time
	  local d := DateTime{2025, 8, 13} as DateTime
      SetHours(12)
	  // numeric format with date and no time
	  local numericResult := TToC(d, 1) as string
	  Assert.True(numericResult:StartsWith("20250813"))

	  // time format with date and no time
	  local timeResult := TToC(d, 2) as string
	  Assert.True(timeResult:Contains("12:00:00 AM")) // Debería ser medianoche

	  // ISO format with date and no time
	  local isoResult := TToC(d, 3) as string
	  Assert.True(isoResult:StartsWith("2025-08-13T00:00:00"))
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCConfigurationPersistenceTests as void
	  // check settings persists
	  local originalHours := SetHours() as long
	  local originalSeconds := SetSeconds() as logic

	  // change settings
	  SetHours(12)
	  SetSeconds(true)

	  // check that the changes are kept
	  Assert.Equal(12, SetHours())
	  Assert.Equal(true, SetSeconds())

	  // use TToC and check it uses the new setting
	  local dt := DateTime{2025, 8, 13, 14, 30, 45} as DateTime
	  local result := TToC(dt, 2) as string
	  Assert.True(result:Contains("PM"))
	  Assert.True(result:Contains(":45")) // Debe incluir segundos

	  // restore original setting
	  SetHours(originalHours)
	  SetSeconds(originalSeconds)

	  // check original settings are restored
	  Assert.Equal(originalHours, SetHours())
	  Assert.Equal(originalSeconds, SetSeconds())
	end method

	// ---------------------------------------------------------------- //
	/// CToT() - Basic Tests
	// ---------------------------------------------------------------- //
	[Fact, Trait("Category", "Date and Time")];
	method CToTBasicTests as void
	    local result as DateTime

	    // completed numeric format
	    result := CToT("20250813143045")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    // numeric format no time
	    result := CToT("20250813")
	    Assert.Equal(DateTime{2025, 8, 13, 0, 0, 0}, result)

	    // ISO format
	    result := CToT("2025-08-13T14:30:45")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    // ISO format no seconds
	    result := CToT("2025-08-13T14:30")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 0}, result)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTDateFormatTests as void
	    local result as DateTime
	    local oldDateFormat := SetDateFormat("MM/DD/YYYY") as string
	    local oldCentury := SetCentury() as logic

	    try
	        // AMERICAN format (MM/dd/yyyy)
	        SetDateFormat("MM/DD/YYYY")
	        SetCentury(true)

	        result := CToT("08/13/2025")
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	        result := CToT("08/13/2025 14:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	        // BRITISH format (dd/MM/yyyy)
	        SetDateFormat("DD/MM/YYYY")

	        result := CToT("13/08/2025")
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	        result := CToT("13/08/2025 14:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

//	        ANSI format (yyyy.MM.dd)
	        SetDateFormat("YYYY.MM.DD")

	        result := CToT("2025.08.13")
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	        result := CToT("2025.08.13 14:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    finally
	        SetDateFormat(oldDateFormat)
	        SetCentury(oldCentury)
	    end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTTimeFormatTests as void
	    local result as DateTime
	    local oldHours := SetHours() as long
	    local oldSeconds := SetSeconds() as logic

	    try
	        // 12 hours format with AM/PM
	        SetHours(12)
	        SetSeconds(true)

	        result := CToT("08/13/2025 02:30:45 PM")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	        result := CToT("08/13/2025 02:30:45 AM")
	        Assert.Equal(DateTime{2025, 8, 13, 2, 30, 45}, result)

	        // 24 hours format
	        SetHours(24)

	        result := CToT("08/13/2025 14:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	        result := CToT("08/13/2025 02:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 2, 30, 45}, result)

	        // no seconds
	        SetSeconds(false)

	        result := CToT("08/13/2025 14:30")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 0}, result)

	    finally
	        SetHours(oldHours)
	        SetSeconds(oldSeconds)
	    end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTCenturyTests as void
	    local result as DateTime
	    local oldCentury := SetCentury() as logic
	    local oldDateFormat := SetDateFormat("MM/DD/YYYY") as string

	    try
	    	// AMERICAN
	        SetDateFormat("MM/DD/YYYY")

	        // full century
	        SetCentury(true)
	        result := CToT("08/13/2025")
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	        result := CToT("08/13/25")  // Debería interpretar como 2025
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	        // no century
	        SetCentury(false)
	        result := CToT("08/13/25")
	        Assert.Equal(DateTime{2025, 8, 13}, result)  // Asumiendo ventana de siglo apropiada

	        result := CToT("08/13/2025")  // Debería funcionar también con 4 dígitos
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	    finally
	        SetCentury(oldCentury)
	        SetDateFormat(oldDateFormat)
	    end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTEdgeCasesTests as void
	    local result as DateTime

	    // midnight
	    result := CToT("08/13/2025 12:00:00 AM")
	    Assert.Equal(DateTime{2025, 8, 13, 0, 0, 0}, result)

	    // noon
	    result := CToT("08/13/2025 12:00:00 PM")
	    Assert.Equal(DateTime{2025, 8, 13, 12, 0, 0}, result)

	    // 1 AM
	    result := CToT("08/13/2025 01:00:00 AM")
	    Assert.Equal(DateTime{2025, 8, 13, 1, 0, 0}, result)

	    // 11:59:59 PM
	    result := CToT("08/13/2025 11:59:59 PM")
	    Assert.Equal(DateTime{2025, 8, 13, 23, 59, 59}, result)

	    // time only (it should use minimal date or current)
	    result := CToT("14:30:45")
	    Assert.Equal(14, result:Hour)
	    Assert.Equal(30, result:Minute)
	    Assert.Equal(45, result:Second)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTInvalidInputTests as void
	    local result as DateTime

	    // empty string
	    result := CToT("")
	    Assert.Equal(DateTime.MinValue, result)

	    // numm string
	    result := CToT(null)
	    Assert.Equal(DateTime.MinValue, result)

	    // only spaces
	    result := CToT("   ")
	    Assert.Equal(DateTime.MinValue, result)

	    // invalid format
	    result := CToT("INVALID DATE")
	    Assert.Equal(DateTime.MinValue, result)

	    // nonexistent date
	    result := CToT("02/30/2025")
	    Assert.Equal(DateTime.MinValue, result)

	    // invalid hour
	    result := CToT("08/13/2025 25:00:00")
	    Assert.Equal(DateTime.MinValue, result)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTFlexibleFormatsTests as void
	    local result as DateTime

	    // different date separator
	    result := CToT("2025-08-13")
	    Assert.Equal(DateTime{2025, 8, 13}, result)

	    result := CToT("2025/08/13")
	    Assert.Equal(DateTime{2025, 8, 13}, result)

	    result := CToT("2025.08.13")
	    Assert.Equal(DateTime{2025, 8, 13}, result)

	    // no starting zeros
	    result := CToT("8/13/2025")
	    Assert.Equal(DateTime{2025, 8, 13}, result)

	    result := CToT("8/3/2025")
	    Assert.Equal(DateTime{2025, 8, 3}, result)

	    // different hours format
	    result := CToT("08/13/2025 2:30 PM")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 0}, result)

	    result := CToT("08/13/2025 14:30")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 0}, result)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTSpecialVFPFormatsTests as void
	    local result as DateTime

	    // compact numeric format no separator
	    result := CToT("20250813")
	    Assert.Equal(DateTime{2025, 8, 13}, result)

	    result := CToT("20250813143045")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    result := CToT("202508131430")  // no seconds
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 0}, result)

	    // ISO format with T (ISO-like)
	    result := CToT("20250813T143045")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTRoundTripTests as void
	    // check CToT can parse what TToC generates
	    local originalDt := DateTime{2025, 8, 13, 14, 30, 45} as DateTime

	    // numeric format
	    local numericStr := TToC(originalDt, 1) as string
	    local parsedDt := CToT(numericStr) as DateTime
	    Assert.Equal(originalDt, parsedDt)

	    // ISO format
	    local isoStr := TToC(originalDt, 3) as string
	    parsedDt := CToT(isoStr)
	    Assert.Equal(originalDt, parsedDt)

	    // default format
	    local defaultStr := TToC(originalDt) as string
	    parsedDt := CToT(defaultStr)
	    Assert.Equal(originalDt:Date, parsedDt:Date)  // Al menos la fecha debe coincidir
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTWhitespaceHandlingTests as void
	    local result as DateTime

	    // starting and ending spaces
	    result := CToT("  20250813143045  ")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    // spaces between date and time
	    result := CToT("08/13/2025   14:30:45")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    // tab and others whitespaces
	    result := CToT(e"\t08/13/2025\t14:30:45\t")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTConfigurationIndependenceTests as void
	    // check certain format works no matter settings
	    local oldDateFormat := SetDateFormat("MM/DD/YYYY") as string
	    local oldCentury := SetCentury() as logic
	    local oldHours := SetHours() as long

	    try
	        // change all settings
	        SetDateFormat("DD/MM/YYYY")
	        SetCentury(false)
	        SetHours(12)

	        // numeric format should work no matter what
	        local result := CToT("20250813143045") as DateTime
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	        // ISO format must work no matter what
	        result := CToT("2025-08-13T14:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    finally
	        SetDateFormat(oldDateFormat)
	        SetCentury(oldCentury)
	        SetHours(oldHours)
	    end try
	end method

end class

end namespace
