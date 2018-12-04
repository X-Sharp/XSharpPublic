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

DEFINE SUBKEY := "XSharpCoreTest"
DEFINE TESTINT := "TESTINT"
DEFINE TESTSTRING := "TESTSTRING"

BEGIN NAMESPACE XSharp.Core.Tests

	CLASS RegistryTests

		[Trait("Category", "Registry")];
		[Fact];
		METHOD RegistryTest() AS VOID 
			LOCAL nResult AS DWORD
			LOCAL cResult AS STRING
			LOCAL lOk AS LOGIC
			lOk := SetRtRegInt(SUBKEY, TESTINT, 42)
			Assert.Equal(TRUE, lOk)
			lOk := DeleteRTRegKey(SUBKEY)
			Assert.Equal(TRUE, lOk)
			lOk := SetRtRegInt(SUBKEY, TESTINT, 42)
			Assert.Equal(TRUE, lOk)
			lOk := SetRtRegString(SUBKEY, TESTSTRING, "424242")
			Assert.Equal(TRUE, lOk)
			nResult := QueryRtRegInt(SUBKEY, TESTINT)
			Assert.Equal(42, (LONG) nResult)
			cResult := QueryRtRegString(SUBKEY, TESTSTRING)
			Assert.Equal("424242", cResult)
			nResult := QueryRtRegInt(SUBKEY, TESTSTRING)
			Assert.Equal(424242, (LONG) nResult)

		END CLASS
END NAMESPACE // XSharp.Runtime.Tests