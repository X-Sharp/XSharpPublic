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

define SUBKEY := "XSharpCoreTest"
define TESTINT := "TESTINT"
define TESTSTRING := "TESTSTRING"

BEGIN NAMESPACE XSharp.Core.Tests

	CLASS RegistryTests

		[Trait("Category", "Registry")];
		[Fact];
		METHOD RegistryTest() as void 
			local nResult as dword
			local cResult as string
			local lOk as logic
			lOk := SetRtRegInt(SUBKEY, TESTINT, 42)
			Assert.Equal(TRUE, lOk)
			lOk := DeleteRTRegKey(SUBKEY)
			Assert.Equal(TRUE, lOk)
			lOk := SetRtRegInt(SUBKEY, TESTINT, 42)
			Assert.Equal(TRUE, lOk)
			lOk := SetRtRegString(SUBKEY, TESTSTRING, "424242")
			Assert.Equal(TRUE, lOk)
			nResult := QueryRtRegInt(SUBKEY, TESTINT)
			Assert.Equal(42, (long) nResult)
			cResult := QueryRtRegString(SUBKEY, TESTSTRING)
			Assert.Equal("424242", cResult)
			nResult := QueryRtRegInt(SUBKEY, TESTSTRING)
			Assert.Equal(424242, (long) nResult)

		END CLASS
END NAMESPACE // XSharp.Runtime.Tests