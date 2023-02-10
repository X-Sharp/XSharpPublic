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

	CLASS OemTests

 		[Trait("Category", "Oem")];
		[Fact];
		METHOD ToAnsi() AS VOID
            LOCAL c1, c2 AS STRING
            // Note that this is codepage dependent. 
            RuntimeState.WinCodePage := 1252
            RuntimeState.DosCodePage := 850
			c1 := "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
			c2 := Oem2Ansi(c1)
			// should be the same, no special characters
			Assert.Equal(c1, c2)
			c1 := "the quick brown fox jumps over the lazy dog"
			c2 := Oem2Ansi(c1)
			// should be the same, no special characters
			Assert.Equal(c1, c2)
			c1 := "ÇüéâäàåçêëèïîìÄÅÉæ"
			c2 := Ansi2Oem(c1)
			Assert.NotEqual(c1, c2)
			c2 := Oem2Ansi(c2)
			Assert.Equal(c1, c2)

		RETURN
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests

