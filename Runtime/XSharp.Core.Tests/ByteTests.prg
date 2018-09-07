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



BEGIN NAMESPACE XSharp.Core.Tests

	CLASS ByteTests

		[Trait("Category", "Swap")];
		[Fact];
		METHOD SwapByteTest() AS VOID 
			Assert.Equal((WORD)86,SwapByte((BYTE)101))
			Assert.Equal((WORD)0x12,SwapByte(0x21))
		RETURN
		[Trait("Category", "Swap")];
		[Fact];
		METHOD SwapwordTest() AS VOID 
			Assert.Equal((WORD)0x1234,SwapWord(0x3412))
		RETURN
		[Trait("Category", "Swap")];
		[Fact];
		METHOD SwapLongTest() AS VOID 
			Assert.Equal(0x12345678L,Swaplong(0x56781234))
		RETURN
		[Trait("Category", "Swap")];
		[Fact];
		METHOD SwapDwordTest() AS VOID 
			Assert.Equal( 0x12345678U,Swapdword(0x56781234))
		RETURN
	END CLASS
END NAMESPACE // XSharp.Runtime.Tests