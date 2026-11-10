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
USING System.Globalization



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
			Assert.Equal( "T", LTOC(TRUE))
			Assert.Equal( "F", LTOC(FALSE))
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
			Assert.Equal("JJJJ", Hex2C("4a4A4a4A"))
			Assert.Equal(":", Hex2C("3a"))


		END CLASS

	// The tests in this collection replace ConversionHelpers.usCulture, so they must not run
	// in parallel with any other test that calls _Val()
	[CollectionDefinition("UsCulture", DisableParallelization := TRUE)];
	CLASS UsCultureCollection
	END CLASS

	[Collection("UsCulture")];
	CLASS CoreUsCultureTests

		[Trait("Category", "Conversion")];
		[Fact];
		METHOD UsCultureIgnoresUserOverridesTest() AS VOID
			VAR oCulture := XSharp.ConversionHelpers.usCulture
			Assert.Equal("en-US", oCulture:Name)
			Assert.False(oCulture:UseUserOverride)
			Assert.Equal(".", oCulture:NumberFormat:NumberDecimalSeparator)
			Assert.Equal(",", oCulture:NumberFormat:NumberGroupSeparator)

		[Trait("Category", "Conversion")];
		[Fact];
		METHOD ValWithUserOverriddenSeparatorsTest() AS VOID
			// Simulates Windows format "English (United States)" with decimal symbol "," and digit grouping "."
			VAR oSaved   := XSharp.ConversionHelpers.usCulture
			VAR oSwapped := (CultureInfo) CultureInfo{"en-US", FALSE}:Clone()
			oSwapped:NumberFormat:NumberDecimalSeparator := ","
			oSwapped:NumberFormat:NumberGroupSeparator   := "."
			VAR nOldDec  := RuntimeState.DecimalSep
			RuntimeState.DecimalSep := 46
			TRY
				TRY
					XSharp.ConversionHelpers.usCulture := oSwapped
					// "." is now parsed as a group separator, which is the bug this culture setup reproduces
					Assert.Equal(99.0, (REAL8) _Val("0.99"))
				FINALLY
					XSharp.ConversionHelpers.usCulture := oSaved
				END TRY
				Assert.Equal(0.99, (REAL8) _Val("0.99"))
			FINALLY
				RuntimeState.DecimalSep := nOldDec
			END TRY

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
