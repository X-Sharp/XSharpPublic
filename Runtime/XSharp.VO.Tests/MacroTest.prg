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

	CLASS MacroTests
	 
		[Fact, Trait("Category", "Macro")]; 
		method Tests as void
			LOCAL cMacro AS STRING
			LOCAL bMacro AS USUAL
			local uValue as USUAL
			cMacro := "1+1"
			uValue := &(cMacro)
			Assert.Equal (2, (int) uValue)
			cMacro := "{||1+2}"
			bMacro := MCompile(cMacro)
			uValue := Eval(bMacro)
			Assert.Equal (3, (int) uValue)
			cMacro := "{||SQrt(25)}"
			bMacro := MCompile(cMacro)
			uValue := Eval(bMacro)
			Assert.Equal (5, (int) uValue)
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests