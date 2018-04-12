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



BEGIN NAMESPACE XSharp.Core.Tests

	CLASS OSTests

		[Trait("Category", "OS")];
		[Fact];
		method TestOs as void
			local c1, c2 as string
			c1 := OS()
			c2 := OS(true)
			Assert.NotEqual(c1, c2)

		[Trait("Category", "OS")];
		[Fact];
		method TestEnv as void
			local c1, c2 as string
			c1 := GetEnv("Robert")
			Assert.Equal(true, SetEnv("Robert", "IsSet"))
			c2 := GetEnv("Robert")
			Assert.NotEqual(c1, c2)


	END CLASS
END NAMESPACE // XSharp.Runtime.Tests