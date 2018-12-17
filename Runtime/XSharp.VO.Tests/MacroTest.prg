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

	CLASS MacroTests
	 
		[Fact, Trait("Category", "Macro")]; 
		METHOD Tests AS VOID
			LOCAL cMacro AS STRING
			LOCAL bMacro AS USUAL
			LOCAL uValue AS USUAL
			cMacro := "1+1"
			uValue := &(cMacro)
			Assert.Equal (2, (INT) uValue)
			cMacro := "{||1+2}"
			bMacro := MCompile(cMacro)
			uValue := Eval(bMacro)
			Assert.Equal (3, (INT) uValue)
			cMacro := "{||SQrt(25)}"
			bMacro := MCompile(cMacro)
			uValue := Eval(bMacro)
			Assert.Equal (5, (INT) uValue)
			cMacro := e"{|aaa,bbb| \"aaa\" + \"bbb\" }"
			bMacro := MCompile(cMacro)
			uValue := Eval(bMacro,20,22)
			Assert.Equal ("aaabbb", (STRING) uValue)
			cMacro := "{|aaa,bbb| aaa + bbb }"
			bMacro := MCompile(cMacro)
			uValue := Eval(bMacro,20,22)
			Assert.Equal (42, (INT) uValue)

            cMacro := "{| a, b, c |  'a' $ 'a|b|c|d' }"
            //cMacro := "{| a,b,c| c == a + b}"
            bMacro := MCompile(cMacro,TRUE)
			uValue := Eval(bMacro,1,2,3)
            Assert.Equal (TRUE, (LOGIC) uValue)
			cMacro := "{|aaa , bbb| aaa + bbb }"
			bMacro := MCompile(cMacro)
			uValue := Eval(bMacro,20,22)
			Assert.Equal (42, (INT) uValue)
		    cMacro := "{|aaa , bbb| aaa + bbb + ccc }" // not existing field
			bMacro := MCompile(cMacro)
	        Assert.ThrowsAny<Exception>( { => Eval(bMacro,20,22) })

            RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
