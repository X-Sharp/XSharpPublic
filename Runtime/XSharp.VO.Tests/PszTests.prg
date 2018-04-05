using System
using System.Collections.Generic
using System.Linq
using System.Text
using XUnit

begin namespace XSharp.VO.Tests
	
	class pszTests
		[Fact, Trait("Category", "Psz")];
		method CreatePszTest() as void
			local p as psz
			p := psz{"Robert was here"}
			Assert.Equal("Robert was here", Psz2String(p))
			Assert.Equal((int) PszLen(p), 15)

	end class
end namespace

