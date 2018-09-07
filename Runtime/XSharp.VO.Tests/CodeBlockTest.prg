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


// WinBool test
BEGIN NAMESPACE XSharp.VO.Tests

	CLASS CodeBlockTests
	 
		[Fact, Trait("Category", "CodeBlocks")];
		METHOD ConversionTests AS VOID
			LOCAL cb AS CODEBLOCK
			cb := {|a,b| a + b }
			Assert.Equal(3, (INT) eval(cb, 1,2)) 
			Assert.Equal(2, (INT) CParamCount(cb))
			LOCAL o AS OBJECT
			o := CreateInstance(#TestMe)
			cb := {|o|Send(o,"CallMe",1,2)}
			Assert.Equal(1, (INT) CParamCount(cb))
			Assert.Equal(42, (INT) eval(cb,o)) 
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests



CLASS testMe
	METHOD CallMe (i1,i2) AS INT
		RETURN 42
END CLASS