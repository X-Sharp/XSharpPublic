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
#pragma options("fox2", off)

// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS EmptyTests
	    STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro


        [Fact, Trait("Category", "Custom Class")];
		method CustomClasstests() as void
            local o as Custom
            o := Custom{}
            o:Width := 42
            o:Height := 24
            o:Top := 1
            o:Left := 2
            Assert.True(o:Width == 42)
            Assert.True(o:Height == 24)
            Assert.True(o:Top == 1)
            Assert.True(o:Left == 2)
            Assert.True(o:Class == "CUSTOM")
            o:AddProperty("Foo",42)
            Assert.True(o:Foo == 42)
            o := Custom{}
            o:AddProperty("test",123)


        [Fact, Trait("Category", "Empty Class")];
		method EmptyClasstests() as void
            local o as object
            o := XSharp.VFP.Empty{}
            Assert.Throws( typeof(XSharp.VFP.PropertyNotFoundException), { => o:NonExistingProperty := 123 })
            try
                o:NonExistingProperty := 123
            catch e as exception
                ? e
            end try
            AddProperty(o, "SomeProperty", 42)
            Assert.True( (Int) o:SomeProperty  == 42)
            AddProperty(o, "BestLanguage","X#")
            Assert.True( (string) o:BestLanguage  == "X#")
            o:BestLanguage := true
            Assert.True(  o:BestLanguage  == true)
            RemoveProperty(o, "BestLanguage")
            RemoveProperty(o, "BestLanguage") // FoxPro does not throw an exception
            Assert.Throws( typeof(XSharp.VFP.PropertyNotFoundException), { => o:BestLanguage  := false})

        [Fact, Trait("Category", "Empty Function")];
        method EmptyFunctiontests() as void
            local u AS USUAL
            u := .NULL.
            Assert.False(Empty(u))
            u := DBNull.Value
            Assert.False(Empty(u))
    END CLASS
END NAMESPACE
