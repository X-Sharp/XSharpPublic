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

	CLASS WinBoolTests
	 
		[Fact, Trait("Category", "Misc")]; 
		METHOD ConversionTests AS VOID
			LOCAL wb AS __WinBool
			LOCAL l AS LOGIC
			l := true
			wb := l
			l := false
			l := wb
			Assert.Equal(l, true)
			l := !wb
			Assert.Equal(l, false)
			Assert.Equal((LOGIC) (wb .and. wb), true)
			Assert.Equal((LOGIC) (wb .and. !wb), false)
			Assert.Equal((LOGIC) (wb .or. wb), true)
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests