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
BEGIN NAMESPACE XSharp.VO.Tests

	CLASS FileIOTests
	 
 		[Trait("Category", "File")];
		[Fact]; 
		METHOD FileTest() AS VOID
			LOCAL hFile AS PTR
			LOCAL cLine AS USUAL
			LOCAL cFile AS STRING
			LOCAL cText	AS STRING
			cFile := "test.txt"
			hFile := FCreate(cFile)
			cLine := "line1"
			FWriteLine(hFile, cLine)
			cLine := "line2"
			FWriteLine(hFile, cLine)
			FClose(hFile)
			cText := MemoRead(cFile)
			Assert.Equal(e"line1\r\nline2\r\n", cText)
			FErase(cFile)
		RETURN

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
