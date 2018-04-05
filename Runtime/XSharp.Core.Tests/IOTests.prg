USING System
USING System.Collections.Generic
USING System.Text
using XUnit


BEGIN NAMESPACE XSharp.Core.Tests

	CLASS IOTests

		[Fact, Trait("Category", "Misc")]; 
		method FileTest() as void
			Assert.Equal(true,File("c:\windows\system32\shell32.dll"))
			Assert.Equal(false,File(null))

			Assert.Equal("XSHARP\DEVRT\BINARIES\DEBUG", CurDir():ToUpper())
		RETURN

	END CLASS
end namespace // XSharp.Runtime.Tests

