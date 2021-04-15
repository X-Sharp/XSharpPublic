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
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS OtherTests

		[Fact, Trait("Category", "Other")];
		METHOD IOTests() AS VOID
            // In the VO Dialect this is allowed with a non existing path
            XSharp.RuntimeState.Dialect := XSharpDialect.VO
            var cOld := SetDefault()
            var cNew := "C:\NonExistingFolder"
            Assert.Equal(cOld, SetDefault(cNew))
            Assert.Equal(cNew, SetDefault())

            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            Assert.Equal(cNew, SetDefault(cOld))
            // a not existing path should throw an error in the FoxPro dialect
            Assert.Throws<XSharp.Error>( { =>  SetDefault(cNew)})

		[Fact, Trait("Category", "Other")];
        METHOD KeyboardTests() AS VOID
            NumLock(TRUE)
            Assert.True(NumLock())
            NumLock(FALSE)
            Assert.False(NumLock())


            CapsLock(TRUE)
            Assert.True(CapsLock())
            CapsLock(FALSE)
            Assert.False(CapsLock())


            InsMode(TRUE)
            Assert.True(InsMode())
            InsMode(FALSE)
            Assert.False(InsMode())



	END CLASS

END NAMESPACE
