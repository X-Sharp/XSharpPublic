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

	CLASS EmptyTests
        [Fact, Trait("Category", "Empty Class")];
		METHOD EmptyClasstests() AS VOID
            local o as Object
            o := XSharp.VFP.Empty{}
            Assert.Throws( typeof(XSharp.Error), { => o:NonExistingProperty := 123 })
            try
                o:NonExistingProperty := 123 
            catch e as exception
                ? e
            end try
            AddProperty(o, "SomeProperty", 42)
            Assert.True( (Int) o:SomeProperty  == 42)
            AddProperty(o, "BestLanguage","X#")
            Assert.True( (string) o:BestLanguage  == "X#")
            RemoveProperty(o, "BestLanguage")
            Assert.Throws( typeof(XSharp.Error), { => o:BestLanguage  := FALSE})
    END CLASS
END NAMESPACE
