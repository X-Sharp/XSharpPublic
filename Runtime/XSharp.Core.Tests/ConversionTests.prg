USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit



BEGIN NAMESPACE XSharp.Core.Tests

	CLASS ConversionTests

		[Trait("Category", "Conversion")];
		[Fact];
		METHOD ShortTest() as void 
			local si as short
			si := 0x1234
			Assert.Equal(si, bin2i(i2bin(si)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD WordTest() as void 
			local w as word
			local si as short
			si := 0x1234			
			w := 0x1234
			Assert.Equal(w, bin2w(w2bin(w)))
			Assert.Equal(i2bin(si), w2bin(w))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD LongTest() as void 
			local l as long
			l := 0x12345678
			Assert.Equal(l, bin2l(l2bin(l)))
		return
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD DWordTest() as void 
			local dw as dword
			local l as long
			dw := 0x12345678
			l := 0x12345678
			Assert.Equal(dw2bin(dw), l2bin(l))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD LogicTest() as void 
			local l as logic
			l := true
			Assert.Equal(l, bin2logic(Logic2Bin(l)))
			l := false
			Assert.Equal(l, bin2logic(Logic2Bin(l)))
			Assert.NotEqual(!l, bin2logic(Logic2Bin(l)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD Real4Test() as void 
			local r4 as Real4
			r4 := 123.456
			Assert.Equal(r4, bin2Real4(Real42Bin(r4)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD Real8Test() as void 
			local r8 as Real4
			r8 := 123.456
			Assert.Equal(r8, bin2Real8(Real82Bin(r8)))
		RETURN
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD PtrTest() as void 
			local p as IntPtr
			p := (IntPtr) 0x12345
			Assert.Equal(p, bin2Ptr(Ptr2Bin(p)))

		return	

		[Trait("Category", "Conversion")];
		[Fact];
		METHOD HiLoByteTest() as void 
			local w as word
			w := 0x1234
			Assert.Equal( 0x12, hibyte(w))
			Assert.Equal( 0x34, lobyte(w))

		return	
		[Trait("Category", "Conversion")];
		[Fact];
		METHOD HiLoWordTest() as void 
			local w as dword
			w := 0x12345678
			Assert.Equal( 0x1234, hiword(w))
			Assert.Equal( 0x5678, loword(w))

		return	

		[Trait("Category", "Conversion")];
		[Fact];
		METHOD LTocTest() as void 
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
		METHOD C2HexTest() as void 
			Assert.Equal("40", C2Hex("@"))
			Assert.Equal("404040", C2Hex("@@@"))
			Assert.Equal("@", Hex2C("40"))
			Assert.Equal("@AB", Hex2C("404142"))
			Assert.Equal("@A", Hex2C("40414"))	// incomplete last character


		END CLASS
END NAMESPACE // XSharp.Runtime.Tests