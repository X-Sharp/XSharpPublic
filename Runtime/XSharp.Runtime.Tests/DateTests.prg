USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using XSharp.Runtime


BEGIN NAMESPACE XSharp.Runtime.Tests

	CLASS DateTests

		[Fact, Trait("Category", "Date")];
		METHOD CTODTest() as void
			var u := ctod("03/13/2016")
			Assert.Equal(__VODate{2016,01,01} ,ctod("01/01/2016"))
			Assert.Equal(__VODate{2016,02,13} ,ctod("13/02/2016"))
			Assert.Equal(__VODate{0001,01,01} ,ctod("03/13/2016"))	
		RETURN

 		
		[Fact, Trait("Category", "Date")];
		METHOD ElapTimeTest() as void
			Assert.Equal("11:23:34",elaptime("12:00:00","23:23:34"))
			Assert.Equal("-11:23:34",elaptime("23:23:34","12:00:00"))	
			Assert.Equal("",elaptime("29:23:34","12:00:00"))
		RETURN

		[Fact, Trait("Category", "Date")];
		METHOD STODTest() as void
			Assert.Equal(__VODate{2016,05,06},STOD("20160506"))
		RETURN

		[Fact, Trait("Category", "Date")];
		METHOD CDOWTest() as void
		//	Assert.Equal("Dienstag",CDOW(Condate(2016,5,24)))
		RETURN

		[Fact, Trait("Category", "Date")];
		METHOD CMonthTest() as void
			//Assert.Equal("Mai",CMonth(Condate(2016,5,24)))
		RETURN

		[Fact, Trait("Category", "Date")];
		METHOD DayTest() as void
			Assert.Equal((dword)24,Day(Condate(2016,5,24)))
		RETURN

		[Fact, Trait("Category", "Date")];
		METHOD DOWTest() as void
			Assert.Equal((dword)2,DOW(Condate(2016,5,24)))
		RETURN

		[Fact, Trait("Category", "Date")];
		METHOD DTOCTest() as void
			//Assert.Equal("24.05.2016",DTOC(CTOD("24/05/2016")))
		RETURN

		[Fact, Trait("Category", "Date")];
		METHOD DTOSTest() as void
			Assert.Equal("20160524",DTOS(Condate(2016,5,24)))
		RETURN

		[Fact, Trait("Category", "Date")];
		METHOD MonthTest() as void
			Assert.Equal((dword)5,MONTH(Condate(2016,5,24)))
		RETURN

		[Fact, Trait("Category", "Date")];
		METHOD YearTest() as void
			Assert.Equal((dword)2016,YEAR(Condate(2016,5,24)))
		RETURN

		[Fact, Trait("Category", "Date")];
		METHOD CastTest() as void
			LOCAL i as LONG
			LOCAL dw as DWORD
			VAR d := __VoDate{2017,1,1}
			i := (LONG) d
			dw := (DWORD) d
			Assert.Equal(d, (__VoDate) i)
			Assert.Equal(d, (__VoDate) dw)

		RETURN
		[Fact, Trait("Category", "Date")];
		METHOD CompareTest() as VOID
		VAR d1 := __VoDate{2017,09,25}
		VAR d2 := __VoDate{2017,09,26}	// different day
		Assert.True(d1 < d2)
		Assert.True(d1 <= d2)
		Assert.True(d1+1 == d2)
		Assert.False(d1 > d2)
		Assert.False(d1 >= d2)

		d2 := __VoDate{2017,10,25}	// different month
		Assert.True(d1 < d2)
		Assert.True(d1 <= d2)
		Assert.False(d1 > d2)
		Assert.False(d1 >= d2)

		d2 := __VoDate{2018,09,24}	// different year
		Assert.True(d1 < d2)
		Assert.True(d1 <= d2)
		Assert.False(d1 > d2)
		Assert.False(d1 >= d2)

		Assert.True(d2 > d1)
		Assert.True(d2 >= d1)
		Assert.False(d2 < d1)
		Assert.False(d2 <= d1)
		
		Var d3 := d1
		Assert.False(d3 > d1)
		Assert.True(d3 >= d1)
		Assert.False(d3 < d1)
		Assert.True(d3 <= d1)


	END CLASS
END NAMESPACE // XSharp.Runtime.Tests