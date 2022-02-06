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

using System.Diagnostics

// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS MiscTests
		STATIC CONSTRUCTOR
        XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro

		[Fact, Trait("Category", "String")];
		METHOD Various() AS VOID
            Assert.Equal(TRUE, IsMouse())
            Assert.Equal(TRUE, IsColor())


		[Fact, Trait("Category", "String")];
		METHOD ProgramTests() AS VOID
            Assert.Equal((int) Program(-1), StackTrace{ FALSE }:FrameCount)
            Assert.Equal(Program(), "MISCTESTS:PROGRAMTESTS")
            Assert.Equal(Program(0), "MISCTESTS:PROGRAMTESTS")
            Assert.Equal(Program(1), "MISCTESTS:PROGRAMTESTS")



	END CLASS

END NAMESPACE
