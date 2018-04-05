USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit

BEGIN NAMESPACE XSharp.Core.Tests
	class TimeTests

	[Fact, Trait("Category", "Time")];
	METHOD AmPmTest() as void
		var time := "16:55:23"
		SetAmPm(true)
		SetAMExt(" AM")
		SetPMExt(" PM")
		Assert.Equal("04:55:23 PM",AmPm(time))
		time := "26:55:23"
		Assert.Equal("",AmPm(time))
		Assert.Equal("",AmPm(null))
	RETURN

		[Fact, Trait("Category", "Time")];
		METHOD ElapTimeTest() as void
			Assert.Equal("11:23:34",elaptime("12:00:00","23:23:34"))
			Assert.Equal("12:36:26",elaptime("23:23:34","12:00:00"))	
			Assert.Equal("06:36:26",elaptime("29:23:34","12:00:00"))
		RETURN

		[Fact, Trait("Category", "Time")];
		METHOD SecondsTest() as void
			local r81, r82 as real8
			r81 := Seconds()
			System.Threading.Thread.Sleep(1000)
			r82 := Seconds()
			Assert.True(r82 >= r81 + 1.0)
			r81 := Secs("12:00:00")
			r82 := Secs("12:00:01")
			Assert.True(r82 == r81 + 1.0)

		[Fact, Trait("Category", "Time")];
		METHOD ConTimeTest() as void
			local cTime as string
			cTime := ConTime(12,0,0)
			Assert.Equal("12:00:00", cTime)
			cTime := ConTime(24,0,0)
			Assert.Equal("", cTime)
			cTime := ConTime(23,60,0)
			Assert.Equal("", cTime)
			cTime := ConTime(23,59,60)
			Assert.Equal("", cTime)

		[Fact, Trait("Category", "Time")];
		METHOD TimeTest() as void
			local cTime1 as string
			local cTime2 as string
			SetAmPm(FALSE)
			cTime1 := Time()
			cTime2 := Time24()
			Assert.Equal(cTime1, cTIme2)
			SetAmPm(true)
			SetAmExt("")
			SetPmExt("")
			cTime1 := Time()
			// can't control the time of the day the test is run
			if cTime2 > "12:00:00"
				Assert.NotEqual(cTime1, cTIme2)
			else
				Assert.Equal(cTime1, cTIme2)
			endif

		RETURN

	end CLASS
end NAMESPACE