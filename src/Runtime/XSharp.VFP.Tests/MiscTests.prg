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


        [Fact, Trait("Category", "String")];
        METHOD IsNullTests() AS VOID
            // Basic tests
            Assert.True(IsNull(DBNull.Value))
            Assert.False(IsNull(0))
            Assert.False(IsNull(""))
            Assert.False(IsNull(FALSE))
        END METHOD

        [Fact, Trait("Category", "String")];
        METHOD IsBlankTests() AS VOID
            // STRINGS
            Assert.True(IsBlank(""))
            Assert.True(IsBlank("   "))
            Assert.False(IsBlank("abc"))

            // // NUMBERS
            Assert.False(IsBlank(0))
            Assert.False(IsBlank(9.99))

            // DATES
            VAR dEmpty := (DATE) 0
            VAR dToday := Today()
            Assert.True(IsBlank(dEmpty))
            Assert.False(IsBlank(dToday))

            // LOGICAL
            Assert.False(IsBlank(TRUE))
            Assert.False(IsBlank(FALSE))

            // NULL
            Assert.False(IsBlank(DBNull.Value))
        END METHOD

	END CLASS

END NAMESPACE
