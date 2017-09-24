USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using XSharp.Runtime


BEGIN NAMESPACE XSharp.Runtime.Tests

	CLASS UsualTests

		[Fact];
		METHOD DateTimeTest() as void
			local now as __VODate
			local u   as __Usual
		    now := __VoDate{System.DateTime.Now}
			u := __Usual{now}
			var s := u:ToString()
			Assert.Equal(now:ToString(),u:ToString())
		RETURN

		[Fact];
		METHOD UsualLongTests() AS VOID
			LOCAL u AS __Usual
			LOCAL l as LONG
			u := __Usual{1}
			Assert.Equal(UsualType(u), (DWORD) LONG)
			l := u
			Assert.Equal(l,1)
			Assert.Equal(l,  (LONG) u)
			u := __Usual{UInt32.MaxValue}
			Assert.Throws(typeof(Error), { => l := (LONG) u})	// Overflow Error
			u := __Usual{"a text"}
			Assert.Throws(typeof(Error), { => l := (LONG) u})	// Conversion Error
			
			RETURN
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests