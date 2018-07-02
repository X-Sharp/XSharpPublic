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



BEGIN NAMESPACE XSharp.VO.Tests

	CLASS UsualTests

		[Fact, Trait("Category", "Usual")];
		METHOD UsualDateTimeTest() AS VOID
			LOCAL now AS DateTime
			LOCAL u   AS USUAL
		    now := System.DateTime.Now
			u := now
			// cannot call u:ToString directly because of late binding in other tests
			VAR s := u:ToString()
			Assert.Equal(now:ToString(),u:ToString())
			LOCAL check AS DateTime
			check := u
			Assert.Equal(now, check)
		RETURN

		[Fact, Trait("Category", "Usual")];
		METHOD UsualDecimalTests() AS VOID
			LOCAL u AS USUAL
			LOCAL l AS Decimal
			l := 1
			u := l
			Assert.Equal(UsualType(u), (DWORD) 27)
			l := u
			Assert.Equal(l,1)
			Assert.Equal(l,  (Decimal) u)
			u += 1
			Assert.Equal(2,  (Decimal) u)
			u++
			Assert.Equal(3,  (Decimal) u)
			u += 1U
			Assert.Equal(4,  (Decimal) u)
			// Note: adding a float to a usual results in a usual of type FLOAT
			// Therefore we must add the m suffix here
			u += 1.0m
			Assert.Equal(5, (Decimal) u)
			u := Decimal.MaxValue
			RETURN


		[Fact, Trait("Category", "Usual")];
		METHOD UsualInt64Tests() AS VOID
			LOCAL u AS USUAL
			LOCAL l AS INT64
			u :=  1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			l := u
			Assert.Equal(l,1)
			Assert.Equal(l,  (INT64) u)
			u := UInt64.MaxValue
			Assert.Throws(typeof(Error), { => l := (INT64) u})	// Overflow Error
			u := "a text"
			Assert.Throws(typeof(Error), { => l := (INT64) u})	// Conversion Error
			RETURN

		[Fact, Trait("Category", "Usual")];
		METHOD UsualLongTests() AS VOID
			LOCAL u AS USUAL
			LOCAL l AS LONG
			u := 1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			l := u
			Assert.Equal(l,1)
			Assert.Equal(l,  (LONG) u)
			u := UInt32.MaxValue
			Assert.Throws(typeof(Error), { => l := (LONG) u})	// Overflow Error
			u := "a text"
			Assert.Throws(typeof(Error), { => l := (LONG) u})	// Conversion Error
			
			RETURN

		[Fact, Trait("Category", "Usual")];
		METHOD UsualShortTests() AS VOID
			LOCAL u AS USUAL
			LOCAL l AS LONG
			u := (USUAL) (SHORT)1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			l := u
			Assert.Equal(l,1)
			Assert.Equal(l,  (SHORT) u)
			u := UInt16.MaxValue
			Assert.Throws(typeof(Error), { => l := (SHORT) u})	// Overflow Error
			u := "a text"
			Assert.Throws(typeof(Error), { => l := (SHORT) u})	// Conversion Error
			
			RETURN

		[Fact, Trait("Category", "Usual")];
		METHOD UsualSByteTests() AS VOID
			LOCAL u AS USUAL
			LOCAL l AS SByte
			u := (SByte)1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			l := u
			Assert.Equal(l,1)
			Assert.Equal(l,  (SByte) u)
			u := Byte.MaxValue
			Assert.Throws(typeof(Error), { => l := (SByte) u})	// Overflow Error
			u := "a text"
			Assert.Throws(typeof(Error), { => l := (SByte) u})	// Conversion Error
			RETURN

		[Fact, Trait("Category", "Usual")];
		METHOD UsualUInt64Tests() AS VOID
			LOCAL u AS USUAL
			LOCAL d AS UInt64
			u := (Uint64) 1
			Assert.Equal(UsualType(u), (DWORD) INT64)
			d := u
			Assert.Equal(d,1U)
			Assert.Equal(d,  (UInt64) u)
			u := -1
			Assert.Throws(typeof(Error), { => d := (UInt64) u})	// Overflow Error
			u := "a text"
			Assert.Throws(typeof(Error), { => d := (UInt64) u})	// Conversion Error
			RETURN

		[Fact, Trait("Category", "Usual")];
		METHOD UsualDwordTests() AS VOID
			LOCAL u AS USUAL
			LOCAL d AS DWORD
			u := (DWORD) 1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			d := u
			Assert.Equal(d,1U)
			Assert.Equal(d,  (DWORD) u)
			u := -1
			Assert.Throws(typeof(Error), { => d := (DWORD) u})	// Overflow Error
			u := "a text"
			Assert.Throws(typeof(Error), { => d := (DWORD) u})	// Conversion Error
			RETURN

		[Fact, Trait("Category", "Usual")];
		METHOD UsualwordTests() AS VOID
			LOCAL u AS USUAL
			LOCAL w AS WORD
			u :=  (WORD) 1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			w := u
			Assert.Equal(w,1)
			Assert.Equal(w,  (WORD) u)
			u := -1
			Assert.Throws(typeof(Error), { => w := (WORD) u})	// Overflow Error
			u := "a text"
			Assert.Throws(typeof(Error), { => w := (WORD) u})	// Conversion Error
			RETURN

		[Fact, Trait("Category", "Usual")];
		METHOD UsualByteTests() AS VOID
			LOCAL u AS USUAL
			LOCAL b AS BYTE
			b := 1
			u :=  b
			Assert.Equal(UsualType(u), (DWORD) LONG)
			b := u
			Assert.Equal(b,1U)
			Assert.Equal(b,  (BYTE) u)
			u :=  -1
			Assert.Throws(typeof(Error), { => b := (BYTE) u})	// Overflow Error
			u := "a text"
			Assert.Throws(typeof(Error), { => b := (BYTE) u})	// Conversion Error
			RETURN

		[Fact, Trait("Category", "Usual")];
		METHOD UsualAddOperatorTests() AS VOID
			LOCAL u AS USUAL
			u :=  1
			u += 1
			Assert.Equal((INT) u, 2)
			u += (INT64) 1
			Assert.Equal((INT) u, 3)
			u += 1.0
			Assert.Equal((INT) u, 4)
			u++
			Assert.Equal((INT) u, 5)
			Assert.Throws(typeof(Error), { => u += "a"})	

			u := (INT64) 1
			u += (INT) 1
			Assert.Equal((INT64) u, 2)
			u += (INT64) 1
			Assert.Equal((INT64) u, 3)
			u += 1.0
			Assert.Equal((INT64) u, 4)
			u++
			Assert.Equal((INT64) u, 5)
			Assert.Throws(typeof(Error), { => u += "a"})	

			u :=   1.0
			u += 1
			Assert.Equal((FLOAT) u, 2.0)
			u += (INT64) 1
			Assert.Equal((FLOAT) u, 3.0)
			u += 1.0
			Assert.Equal((FLOAT) u, 4.0)
			u ++
			Assert.Equal((FLOAT) u, 5.0)
			Assert.Throws(typeof(Error), { => u += "a"})	

			u := "abc"
			u += "def"
			Assert.Equal((STRING) u, "abcdef")
			Assert.Throws(typeof(Error), { => u += 1})	

			VAR d := (DATE) datetime.Now
			u := d
			u += 1
			d += 1
			Assert.Equal((DATE) u,  d)

		[Fact, Trait("Category", "Usual")];
		METHOD UsualSubOperatorTests() AS VOID
			LOCAL u AS USUAL
			u := 100
			u -= 1
			Assert.Equal((INT) u, 99)
			u -= (INT64) 1
			Assert.Equal((INT) u, 98)
			u -= 1.0
			Assert.Equal((INT) u, 97)
			u--
			Assert.Equal((INT) u, 96)
			Assert.Throws(typeof(Error), { => u -= "a"})	

			u := (INT64) 1000
			u -= (INT) 1
			Assert.Equal((INT64) u, 999)
			u -= (INT64) 1
			Assert.Equal((INT64) u, 998)
			u -= 1.0
			Assert.Equal((INT64) u, 997)
			u--
			Assert.Equal((INT64) u, 996)
			Assert.Throws(typeof(Error), { => u -= "a"})	

			u := 111.1
			u -= 1
			Assert.Equal((FLOAT) u, 110.1)
			u -= (INT64) 1
			Assert.Equal((FLOAT) u, 109.1)
			u -= 1.0
			Assert.Equal((FLOAT) u, 108.1)
			u--
			Assert.Equal((FLOAT) u, 107.1)
			Assert.Throws(typeof(Error), { => u -= "a"})	

			u := "abc"
			u -= "def"
			Assert.Equal((STRING) u, "abcdef")
			Assert.Throws(typeof(Error), { => u -= 1})	

		[Fact, Trait("Category", "Usual string comparisons")];
		METHOD UsualStringComparisons() AS VOID
			LOCAL lExact := SetExact() AS LOGIC
			LOCAL u AS USUAL
			
			FOR LOCAL n := 1 AS INT UPTO 2
				SetExact(n == 1)
				u := "ABC"
				// do not chnage to Assert.Equal(), we need to test the operators
				Assert.False(u = "123456")
				Assert.False(u == "123456")
				Assert.True(u != "123456")
				Assert.True(u <> "123456")

				u := "123456"
				Assert.False(u = "ABC")
				Assert.False(u == "ABC")
				Assert.True(u != "ABC")
				Assert.True(u <> "ABC")
			NEXT
			
			SetExact(lExact)

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests
