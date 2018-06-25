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

	CLASS WinBoolTests
	 
		[Fact, Trait("Category", "Misc")]; 
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