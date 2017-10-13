USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using XSharp.Runtime

// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.Runtime.Tests

	CLASS WinBoolTests
	 
		[Fact]; 
		METHOD ConversionTests as VOID
			local wb as __WinBool
			local l as logic
			l := true
			wb := l
			l := false
			l := wb
			Assert.Equal(l, true)
			l := !wb
			Assert.Equal(l, false)
			Assert.Equal((logic) (wb .and. wb), true)
			Assert.Equal((logic) (wb .and. !wb), false)
			Assert.Equal((logic) (wb .or. wb), true)
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests