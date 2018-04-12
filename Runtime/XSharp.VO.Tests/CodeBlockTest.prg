USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit


// WinBool test
BEGIN NAMESPACE XSharp.VO.Tests

	CLASS CodeBlockTests
	 
		[Fact, Trait("Category", "CodeBlocks")];
		method ConversionTests as void
			local cb as CodeBlock
			cb := {|a,b| a + b }
			Assert.Equal(3, (int) eval(cb, 1,2)) 
			Assert.Equal(2, (int) CParamCount(cb))
			local o as object
			o := CreateInstance(#TestMe)
			cb := {|o|Send(o,"CallMe",1,2)}
			Assert.Equal(1, (int) CParamCount(cb))
			Assert.Equal(42, (int) eval(cb,o)) 
		RETURN

	END CLASS
end namespace // XSharp.Runtime.Tests



class testMe
	method CallMe (i1,i2) as int
		return 42
end class