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


// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS MiscTests
	
		[Fact, Trait("Category", "String")];
		METHOD Various() AS VOID
            Assert.Equal(TRUE, IsMouse())
            Assert.Equal(TRUE, IsColor())

	END CLASS

END NAMESPACE
