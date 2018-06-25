//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit


// WinBool test
BEGIN NAMESPACE XSharp.VO.Tests

	CLASS ErrorTests
	 
		[Fact, Trait("Category", "Error")]; 
		method Tests as void
			local u as usual
			begin sequence
				Assert.Equal (true, canBreak())
				if canBreak()
					break 10
				endif
			recover using u
				Assert.Equal(10, (int) u)
			end sequence
			Assert.Equal (false, canBreak())
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests