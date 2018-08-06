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
			Assert.Equal(si, bin2i(i2bin(si)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD WordTest() AS VOID 
			LOCAL w AS WORD
			LOCAL si AS SHORT
			si := 0x1234			
			w := 0x1234
			Assert.Equal(w, bin2w(w2bin(w)))
			Assert.Equal(i2bin(si), w2bin(w))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD LongTest() AS VOID 
			LOCAL l AS LONG
			l := 0x12345678
			Assert.Equal(l, bin2l(l2bin(l)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD DWordTest() AS VOID 
			LOCAL dw AS DWORD
			LOCAL l AS LONG
			dw := 0x12345678
			l := 0x12345678
			Assert.Equal(dw2bin(dw), l2bin(l))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD LogicTest() AS VOID 
			LOCAL l AS LOGIC
			l := true
			Assert.Equal(l, bin2logic(Logic2Bin(l)))
			l := false
			Assert.Equal(l, bin2logic(Logic2Bin(l)))
			Assert.NotEqual(!l, bin2logic(Logic2Bin(l)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD Real4Test() AS VOID 
			LOCAL r4 AS REAL4
			r4 := 123.456
			Assert.Equal(r4, bin2Real4(Real42Bin(r4)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD Real8Test() AS VOID 
			LOCAL r8 AS REAL4
			r8 := 123.456
			Assert.Equal(r8, bin2Real8(Real82Bin(r8)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD PtrTest() AS VOID 
			LOCAL p AS IntPtr
			p := (IntPtr) 0x12345
			Assert.Equal(p, bin2Ptr(Ptr2Bin(p)))

		RETURN	

		[Trait("Category", "Conversion")];
		[Fact];
		METHOD HiLoByteTest() AS VOID 
			LOCAL w AS WORD
			w := 0x1234
			Assert.Equal( 0x12, hibyte(w))
			Assert.Equal( 0x34, lobyte(w))

		RETURN	
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD HiLoWordTest() AS VOID 
			LOCAL w AS DWORD
			w := 0x12345678
			Assert.Equal( 0x1234, hiword(w))
			Assert.Equal( 0x5678, loword(w))

		RETURN	

		[Trait("Category", "Conversion")];
		[Fact];
		METHOD LTocTest() AS VOID 
			Assert.Equal( "T", LTOC(TRUE))
			Assert.Equal( "F", LTOC(FALSE))
			Assert.Equal( TRUE, CTOL("T"))
			Assert.Equal( TRUE, CTOL("t"))
			Assert.Equal( TRUE, CTOL("Y"))
			Assert.Equal( true, CTOL("y"))
			Assert.Equal( false, CTOL("F"))
			Assert.Equal( false, CTOL("f"))
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