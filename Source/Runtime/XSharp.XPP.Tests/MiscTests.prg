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

BEGIN NAMESPACE XSharp.XPP.Tests

	CLASS MiscTests

		[Fact, Trait("Category", "Misc")];
		METHOD PlaceholderXPPtests() AS VOID
			Assert.Equal("axxxe" , PosRepl("abcde" , "xxx" , 2) )
		RETURN

	END CLASS

END NAMESPACE
