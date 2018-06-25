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



BEGIN NAMESPACE XSharp.VO.Tests

	CLASS UsualTests

		[Fact, Trait("Category", "Usual")];
		METHOD UsualDateTimeTest() as void
			local now as DateTime
			local u   as Usual
		    now := System.DateTime.Now
			u := now
			// cannot call u:ToString directly because of late binding in other tests
			var s := u:ToString()
			Assert.Equal(now:ToString(),u:ToString())
			local check as DateTime
			check := u
			Assert.Equal(now, check)
		RETURN

		[Fact, Trait("Category", "Usual")];
		METHOD UsualDecimalTests() AS VOID
			LOCAL u AS Usual
			LOCAL l as Decimal
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
			LOCAL u AS Usual
			LOCAL l as Int64
			u :=  1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			l := u
			Assert.Equal(l,1)
			Assert.Equal(l,  (Int64) u)
			u := UInt64.MaxValue
			Assert.Throws(typeof(Error), { => l := (Int64) u})	// Overflow Error
			u := "a text"
			Assert.Throws(typeof(Error), { => l := (Int64) u})	// Conversion Error
			RETURN

		[Fact, Trait("Category", "Usual")];
		METHOD UsualLongTests() AS VOID
			LOCAL u AS Usual
			LOCAL l as LONG
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
			LOCAL u AS Usual
			LOCAL l as LONG
			u := (Usual) (Short)1
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
			LOCAL u AS Usual
			LOCAL l as SByte
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
			LOCAL u AS Usual
			LOCAL d as UInt64
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
			LOCAL u AS Usual
			LOCAL d as DWORD
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
			LOCAL u AS Usual
			LOCAL w as WORD
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
			LOCAL u AS Usual
			LOCAL b as Byte
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
			LOCAL u AS Usual
			u :=  1
			u += 1
			Assert.Equal((Int) u, 2)
			u += (int64) 1
			Assert.Equal((Int) u, 3)
			u += 1.0
			Assert.Equal((Int) u, 4)
			u++
			Assert.Equal((Int) u, 5)
			Assert.Throws(typeof(Error), { => u += "a"})	

			u := (INT64) 1
			u += (int) 1
			Assert.Equal((Int64) u, 2)
			u += (int64) 1
			Assert.Equal((Int64) u, 3)
			u += 1.0
			Assert.Equal((Int64) u, 4)
			u++
			Assert.Equal((Int64) u, 5)
			Assert.Throws(typeof(Error), { => u += "a"})	

			u :=   1.0
			u += 1
			Assert.Equal((Float) u, 2.0)
			u += (int64) 1
			Assert.Equal((Float) u, 3.0)
			u += 1.0
			Assert.Equal((Float) u, 4.0)
			u ++
			Assert.Equal((Float) u, 5.0)
			Assert.Throws(typeof(Error), { => u += "a"})	

			u := "abc"
			u += "def"
			Assert.Equal((string) u, "abcdef")
			Assert.Throws(typeof(Error), { => u += 1})	

			var d := (Date) datetime.Now
			u := d
			u += 1
			d += 1
			Assert.Equal((Date) u,  d)

		[Fact, Trait("Category", "Usual")];
		METHOD UsualSubOperatorTests() AS VOID
			LOCAL u AS Usual
			u := 100
			u -= 1
			Assert.Equal((Int) u, 99)
			u -= (int64) 1
			Assert.Equal((Int) u, 98)
			u -= 1.0
			Assert.Equal((Int) u, 97)
			u--
			Assert.Equal((Int) u, 96)
			Assert.Throws(typeof(Error), { => u -= "a"})	

			u := (INT64) 1000
			u -= (int) 1
			Assert.Equal((Int64) u, 999)
			u -= (int64) 1
			Assert.Equal((Int64) u, 998)
			u -= 1.0
			Assert.Equal((Int64) u, 997)
			u--
			Assert.Equal((Int64) u, 996)
			Assert.Throws(typeof(Error), { => u -= "a"})	

			u := 111.1
			u -= 1
			Assert.Equal((Float) u, 110.1)
			u -= (int64) 1
			Assert.Equal((Float) u, 109.1)
			u -= 1.0
			Assert.Equal((Float) u, 108.1)
			u--
			Assert.Equal((Float) u, 107.1)
			Assert.Throws(typeof(Error), { => u -= "a"})	

			u := "abc"
			u -= "def"
			Assert.Equal((string) u, "abcdef")
			Assert.Throws(typeof(Error), { => u -= 1})	

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests