USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using XSharp.Runtime


BEGIN NAMESPACE XSharp.Runtime.Tests

	CLASS DateTests

		[Fact];
		METHOD CTODTest() as void
			var u := ctod("03/13/2016")
			Assert.Equal(__VODate{2016,01,01} ,ctod("01/01/2016"))
			Assert.Equal(__VODate{2016,02,13} ,ctod("13/02/2016"))
			Assert.Equal(__VODate{0001,01,01} ,ctod("03/13/2016"))	
		RETURN

		[Fact];
		METHOD ElapTimeTest() as void
			Assert.Equal("11:23:34",elaptime("12:00:00","23:23:34"))
			Assert.Equal("-11:23:34",elaptime("23:23:34","12:00:00"))	
			Assert.Equal("",elaptime("29:23:34","12:00:00"))
		RETURN

		[Fact];
		METHOD STODTest() as void
			Assert.Equal(__VODate{2016,05,06},STOD("20160506"))
		RETURN

		[Fact];
		METHOD CDOWTest() as void
		//	Assert.Equal("Dienstag",CDOW(CTOD("24/05/2016")))
		RETURN

		[Fact];
		METHOD CMonthTest() as void
			//Assert.Equal("Mai",CMonth(CTOD("24/05/2016")))
		RETURN

		[Fact];
		METHOD DayTest() as void
			Assert.Equal((dword)24,Day(CTOD("24/05/2016")))
		RETURN

		[Fact];
		METHOD DOWTest() as void
			Assert.Equal((dword)2,DOW(CTOD("24/05/2016")))
		RETURN

		[Fact];
		METHOD DTOCTest() as void
			//Assert.Equal("24.05.2016",DTOC(CTOD("24/05/2016")))
		RETURN

		[Fact];
		METHOD DTOSTest() as void
			Assert.Equal("20160524",DTOS(CTOD("24/05/2016")))
		RETURN

		[Fact];
		METHOD MonthTest() as void
			Assert.Equal((dword)5,MONTH(CTOD("24/05/2016")))
		RETURN

		[Fact];
		METHOD YearTest() as void
			Assert.Equal((dword)2016,YEAR(CTOD("24/05/2016")))
		RETURN
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests