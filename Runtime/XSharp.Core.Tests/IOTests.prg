USING System
USING System.Collections.Generic
USING System.Text
using XUnit


BEGIN NAMESPACE XSharp.Core.Tests

	CLASS IOTests

		[Fact, Trait("Category", "Misc")]; 
		METHOD FileTest() as void
			Assert.Equal(true,File("c:\windows\system32\shell32.dll"))
			Assert.Equal(false,File(null))
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests