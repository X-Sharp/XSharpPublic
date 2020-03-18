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

	CLASS StringTests
	
		[Fact, Trait("Category", "String")];
		METHOD JustFunctionTests() AS VOID
            Assert.Equal("C:", JustDrive("C:\Folder\test.txt") )
            Assert.Equal("txt", JustExt("C:\Folder\test.txt") )
            Assert.Equal("C:\Folder", JustPath("C:\Folder\test.txt") )
            Assert.Equal("test.txt", JustFName("C:\Folder\test.txt") )
            Assert.Equal("test", JustStem("C:\Folder\test.txt") )
            Assert.Equal("C:\",Addbs("C:"))
            Assert.Equal("C:\test\",Addbs("C:\test"))

	END CLASS

END NAMESPACE
