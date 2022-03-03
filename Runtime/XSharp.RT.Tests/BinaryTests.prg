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

    CLASS BinaryTests

 		[Trait("Category", "Binary")];
		[Fact]; 
		METHOD SimpleBinaryTests() AS VOID
            LOCAL b1 as Binary
            LOCAL b2 as Binary
            LOCAL b3 as Binary
            LOCAL b as Byte[]
  
            b1 := 0h123456
            b2 := 0habcdef
            b3 := b1 + b2
            Assert.True( b3 == 0h123456abcdef)
            b := b3
            assert.True(b:Length == 6)
            Assert.True(b[1] == 0x12)
            Assert.True(b[2] == 0x34)
            Assert.True(b[3] == 0x56)
            Assert.True(b[4] == 0xab)
            Assert.True(b[5] == 0xcd)
            Assert.True(b[6] == 0xef)

  

 		[Trait("Category", "Binary")];
		[Fact]; 
		METHOD BinaryStringTests AS VOID
            LOCAL b1 as Binary
            LOCAL b2 as Binary

            // Convert to string
            b1 := "Hello"
            Assert.True(b1 == "Hello")

            // Add Binary to string
            b2 := 0h20
            b1 := b2 + "test"
            Assert.True (b1 == " test")


 		[Trait("Category", "Binary")];
		[Fact]; 
		METHOD BinaryComparisontests as VOID
            LOCAL b1 as Binary
            LOCAL b2 as Binary
            // Convert to string
            b1 := 0h10
            b2 := 0h20
            Assert.True(b1 < b2)
            Assert.True(b1 <= b2)
            Assert.False(b1 == b2)
            Assert.False(b1 >= b2)
            Assert.False(b1 > b2)

            b1 := " "   // Should be the same as 0h20
            Assert.False(b1 < b2)
            Assert.True(b1 <= b2)
            Assert.True(b1 == b2)
            Assert.True(b1 >= b2)
            Assert.False(b1 > b2)



END CLASS
END NAMESPACE
