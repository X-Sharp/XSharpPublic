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

	CLASS NumericTests

		[Fact, Trait("Category", "Numeric")];
		METHOD CHRTest() AS VOID
			Assert.Equal(" ",Chr((DWORD)32))
		RETURN


		[Fact, Trait("Category", "Numeric")];
		METHOD DW2BINTest() AS VOID
			Assert.Equal("    ",DW2Bin((DWORD) 32*256*256*256+32*256*256+32*256+32))
		RETURN

		[Fact, Trait("Category", "Numeric")];
		METHOD MakeLongTests() AS VOID
			Assert.Equal(65537,MakeLong(1, 1))
			Assert.Equal(    1,MakeLong(1, 0))
			Assert.Equal(65536,MakeLong(0, 1))
			Assert.Equal(    0,MakeLong(0, 0))

            Assert.Equal(2^24 + 257    , MakeLong(257, 256))
            Assert.Equal(-1            , MakeLong(65535, 65535))
			Assert.Equal(Int32.MaxValue, MakeLong(65535, 32767))
		RETURN

		[Fact, Trait("Category", "Numeric")];
		METHOD MakeDWordTests() AS VOID
			Assert.Equal(65537,MakeDWord(1, 1))
			Assert.Equal(    1,MakeDWord(1, 0))
			Assert.Equal(65536,MakeDWord(0, 1))
			Assert.Equal(    0,MakeDWord(0, 0))

            Assert.Equal(2^24 + 257     , MakeDWord(257, 256))
            Assert.Equal(UInt32.MaxValue, MakeDWord(65535, 65535))
			Assert.Equal(Int32.MaxValue , MakeDWord(65535, 32767))
		RETURN

		[Fact, Trait("Category", "Numeric")];
		METHOD MakeWordTests() AS VOID
			Assert.Equal(257,MakeWord(1, 1))
			Assert.Equal(  1,MakeWord(1, 0))
			Assert.Equal(256,MakeWord(0, 1))
			Assert.Equal(  0,MakeWord(0, 0))
			Assert.Equal(65535,MakeWord(255, 255))
		RETURN

		[Fact, Trait("Category", "Numeric")];
		METHOD RGBTests() AS VOID
			Assert.Equal(65793,RGB(1,1,1))
			Assert.Equal(65792,RGB(0,1,1))
			Assert.Equal(65537,RGB(1,0,1))
			Assert.Equal(  257,RGB(1,1,0))
			Assert.Equal(    0,RGB(0,0,0))

			Assert.Equal(2^24-1 , RGB(255,255,255))
		RETURN

    END CLASS
END NAMESPACE // XSharp.Runtime.Tests 
