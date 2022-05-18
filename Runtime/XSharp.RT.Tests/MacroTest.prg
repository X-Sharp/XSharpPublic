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



BEGIN NAMESPACE XSharp.RT.Tests

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

            cMacro := "{||NIL}"
            bMacro := MCompile(cMacro)
            uValue := Eval(bMacro)
            Assert.Equal(NIL, uValue)
            Assert.Equal(0U, UsualType(uValue))    // returned 6 but should be 0
            Assert.Equal("NIL", AsString(uValue))    // returned "{(0x0000)0x00000000} CLASS"  but should be "NIL"

            RETURN

		[Fact, Trait("Category", "Macro")];
		METHOD TestsWhitespace AS VOID
			LOCAL cMacro AS STRING
			LOCAL bMacro AS USUAL
			LOCAL uValue AS USUAL
            cMacro := ".t..and..f."
            bMacro := MCompile(cMacro)
            uValue := Eval(bMacro)
            Assert.Equal(FALSE, (LOGIC) uValue)
            cMacro := ".t..and..t."
            bMacro := MCompile(cMacro)
            uValue := Eval(bMacro)
            Assert.Equal(TRUE, (LOGIC) uValue)
            cMacro := ".t..or..f."
            bMacro := MCompile(cMacro)
            uValue := Eval(bMacro)
            Assert.Equal(TRUE, (LOGIC) uValue)
            cMacro := ".n..or..y."
            bMacro := MCompile(cMacro)
            uValue := Eval(bMacro)
            Assert.Equal(TRUE, (LOGIC) uValue)
            cMacro := ".n..and..y."
            bMacro := MCompile(cMacro)
            uValue := Eval(bMacro)
            Assert.Equal(FALSE, (LOGIC) uValue)

            cMacro := "{||.n..and..y.}"
            bMacro := MCompile(cMacro)
            uValue := Eval(bMacro)
            Assert.Equal(FALSE, (LOGIC) uValue)

		[Fact, Trait("Category", "Macro")];
		METHOD TestsWithConversions AS VOID
            Assert.Equal(2, (INT) Eval(MCompile("1 + INT( 1)")))
            Assert.Equal(2, (INT) Eval(MCompile("1 + INT(+1)")))
            Assert.Equal(0, (INT) Eval(MCompile("1 - INT(+1)")))
            Assert.Equal(0, (INT) Eval(MCompile("1 + INT(-1)")))
            Assert.Equal(2, (INT) Eval(MCompile("1 - INT(-1)")))
            Assert.Equal(-2, (INT) Eval(MCompile("-1 + INT(-1)")))
            Assert.Equal(0, (INT) Eval(MCompile("-1 - INT(-1)")))
            Assert.Equal(2, (INT) Eval(MCompile("1 + (INT) 1")))
            Assert.Equal(2, (INT) Eval(MCompile("1 + (USUAL) 1")))
            Assert.Equal(2, (INT) Eval(MCompile("1 + USUAL(+1)")))
            Assert.Equal(0, (INT) Eval(MCompile("1 - USUAL(+1)")))
            Assert.Equal(0, (INT) Eval(MCompile("-1 - USUAL(-1)")))
            Assert.Equal(0, (INT) Eval(MCompile("1 + USUAL(-1)")))

            Assert.Equal(2, (INT) Eval( MCompile( "1 + USUAL(-1) + USUAL(+2)") ) )
            Assert.Equal(2, (INT) Eval( MCompile( "1 + INT(-1) + INT(+2)") ) )
            Assert.Equal(4, (INT) Eval( MCompile( "1 + (USUAL)1 + (USUAL)2") ) )

            Assert.Equal(2, (INT) Eval(MCompile("1 + (INT)+1")))
            Assert.Equal(0, (INT) Eval(MCompile("1 + (INT)-1")))
            Assert.Equal(2, (INT) Eval(MCompile("1 + (USUAL)+1")))
            Assert.Equal(0, (INT) Eval(MCompile("1 + (USUAL)-1")))
            Assert.Equal(2, (INT) Eval(MCompile("1 - (USUAL)-1")))
            Assert.Equal(2, (INT) Eval( MCompile( "1 + (USUAL)-1 + (USUAL)+2") ) )
            Assert.Equal(2, (INT) Eval( MCompile( "-1 - (USUAL)-1 - (USUAL)-2") ) )



//		[Fact, Trait("Category", "Macro")];
//		METHOD FunctionOverloadTest AS VOID
//			LOCAL cMacro AS STRING
//			LOCAL bMacro AS USUAL
//		    cMacro := "Left('abcdefg',2)" // should call my replacement function
//			bMacro := MCompile(cMacro)
//
//            Assert.Equal("MyLeft", eval(bMacro))
//
//            RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests

//FUNCTION Left(cValue AS STRING, nLen AS DWORD) AS STRING
//    RETURN "MyLeft"
