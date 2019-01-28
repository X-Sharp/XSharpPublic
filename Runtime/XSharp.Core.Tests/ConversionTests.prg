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

	CLASS CoreConversionTests

		[Trait("Category", "Conversion")];
		[Fact];
		METHOD ShortTest() AS VOID 
			LOCAL si AS SHORT
			si := 0x1234
			Assert.Equal(si, Bin2I(I2Bin(si)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD WordTest() AS VOID 
			LOCAL w AS WORD
			LOCAL si AS SHORT
			si := 0x1234			
			w := 0x1234
			Assert.Equal(w, Bin2W(W2Bin(w)))
			Assert.Equal(I2Bin(si), W2Bin(w))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD LongTest() AS VOID 
			LOCAL l AS LONG
			l := 0x12345678
			Assert.Equal(l, Bin2L(L2Bin(l)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD DWordTest() AS VOID 
			LOCAL dw AS DWORD
			LOCAL l AS LONG
			dw := 0x12345678
			l := 0x12345678
			Assert.Equal(DW2Bin(dw), L2Bin(l))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD LogicTest() AS VOID 
			LOCAL l AS LOGIC
			l := TRUE
			Assert.Equal(l, Bin2Logic(Logic2Bin(l)))
			l := FALSE
			Assert.Equal(l, Bin2Logic(Logic2Bin(l)))
			Assert.NotEqual(!l, Bin2Logic(Logic2Bin(l)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD Real4Test() AS VOID 
			LOCAL r4 AS REAL4
			r4 := 123.456
			Assert.Equal(r4, Bin2Real4(Real42Bin(r4)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD Real8Test() AS VOID 
			LOCAL r8 AS REAL8
			r8 := 123.456
			Assert.Equal(r8, Bin2Real8(Real82Bin(r8)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD PtrTest() AS VOID 
			LOCAL p AS IntPtr
			p := (IntPtr) 0x12345
			Assert.Equal(p, Bin2Ptr(Ptr2Bin(p)))

		RETURN	

		[Trait("Category", "Conversion")];
		[Fact];
		METHOD HiLoByteTest() AS VOID 
			LOCAL w AS WORD
			w := 0x1234
			Assert.Equal( 0x12, HiByte(w))
			Assert.Equal( 0x34, LoByte(w))

		RETURN	
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD HiLoWordTest() AS VOID 
			LOCAL w AS DWORD
			w := 0x12345678
			Assert.Equal( 0x1234, HiWord(w))
			Assert.Equal( 0x5678, LoWord(w))

		RETURN	

		[Trait("Category", "Conversion")];
		[Fact];
		METHOD LTocTest() AS VOID 
			Assert.Equal( "T", LToC(TRUE))
			Assert.Equal( "F", LToC(FALSE))
			Assert.Equal( TRUE, CTOL("T"))
			Assert.Equal( TRUE, CTOL("t"))
			Assert.Equal( TRUE, CTOL("Y"))
			Assert.Equal( TRUE, CTOL("y"))
			Assert.Equal( FALSE, CTOL("F"))
			Assert.Equal( FALSE, CTOL("f"))
			Assert.Equal( FALSE, CTOL("N"))
			Assert.Equal( FALSE, CTOL("n"))

		[Trait("Category", "Conversion")];
		[Fact];
		METHOD C2HexTest() AS VOID 
			Assert.Equal("40", C2Hex("@"))
			Assert.Equal("404040", C2Hex("@@@"))
			Assert.Equal("@", Hex2C("40"))
			Assert.Equal("@AB", Hex2C("404142"))
			Assert.Equal("@A", Hex2C("40414"))	// incomplete last character


		END CLASS
END NAMESPACE // XSharp.Runtime.Tests
