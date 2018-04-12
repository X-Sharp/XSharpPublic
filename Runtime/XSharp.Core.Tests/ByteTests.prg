//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit



BEGIN NAMESPACE XSharp.Core.Tests

	CLASS ByteTests

		[Trait("Category", "Swap")];
		[Fact];
		METHOD SwapByteTest() as void 
			Assert.Equal((word)86,SwapByte((byte)101))
			Assert.Equal((word)0x12,SwapByte(0x21))
		return
		[Trait("Category", "Swap")];
		[Fact];
		METHOD SwapwordTest() as void 
			Assert.Equal((word)0x1234,SwapWord(0x3412))
		return
		[Trait("Category", "Swap")];
		[Fact];
		METHOD SwapLongTest() as void 
			Assert.Equal(0x12345678L,Swaplong(0x56781234))
		RETURN
		[Trait("Category", "Swap")];
		[Fact];
		METHOD SwapDwordTest() as void 
			Assert.Equal( 0x12345678U,Swapdword(0x56781234))
		RETURN
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests