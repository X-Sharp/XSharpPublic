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

	CLASS ErrorTests
	 
		[Fact, Trait("Category", "Error")]; 
		METHOD Tests AS VOID
			LOCAL u AS USUAL
			BEGIN SEQUENCE
				Assert.Equal (TRUE, canBreak())
				IF canBreak()
					BREAK 10
				ENDIF
			RECOVER USING u
				Assert.Equal(10, (INT) u)
			END SEQUENCE
			Assert.Equal (FALSE, canBreak())
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests