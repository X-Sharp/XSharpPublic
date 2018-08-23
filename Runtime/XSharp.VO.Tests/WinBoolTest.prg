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
			l := FALSE
			wb := l
			l := TRUE
            Assert.Equal(l, TRUE)
			l := wb
			Assert.Equal(l, FALSE)
			l := !wb
			Assert.Equal(l, TRUE)
            wb := ! wb
			Assert.Equal((LOGIC) (wb .AND. wb), TRUE)
			Assert.Equal((LOGIC) (wb .AND. !wb), FALSE)
			Assert.Equal((LOGIC) (wb .OR. wb), TRUE)
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
