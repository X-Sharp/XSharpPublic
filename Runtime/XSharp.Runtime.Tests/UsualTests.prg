USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit
using XSharp.Runtime


BEGIN NAMESPACE XSharp.Runtime.Tests

	CLASS UsualTests

		[Fact];
		METHOD UsualDateTimeTest() as void
			local now as DateTime
			local u   as __Usual
		    now := System.DateTime.Now
			u := (__Usual) now
			var s := u:ToString()
			Assert.Equal(now:ToString(),u:ToString())
			local check as DateTime
			check := u
			Assert.Equal(now, check)
		RETURN

		[Fact];
		METHOD UsualDecimalTests() AS VOID
			LOCAL u AS __Usual
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
			u += 1.0
			Assert.Equal(5,  (Decimal) u)
			u := __Usual{Decimal.MaxValue}
			RETURN


		[Fact];
		METHOD UsualInt64Tests() AS VOID
			LOCAL u AS __Usual
			LOCAL l as Int64
			u := (__Usual) 1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			l := u
			Assert.Equal(l,1)
			Assert.Equal(l,  (Int64) u)
			u := __Usual{UInt64.MaxValue}
			Assert.Throws(typeof(Error), { => l := (Int64) u})	// Overflow Error
			u := __Usual{"a text"}
			Assert.Throws(typeof(Error), { => l := (Int64) u})	// Conversion Error
			RETURN

		[Fact];
		METHOD UsualLongTests() AS VOID
			LOCAL u AS __Usual
			LOCAL l as LONG
			u := (__Usual) 1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			l := u
			Assert.Equal(l,1)
			Assert.Equal(l,  (LONG) u)
			u := __Usual{UInt32.MaxValue}
			Assert.Throws(typeof(Error), { => l := (LONG) u})	// Overflow Error
			u := __Usual{"a text"}
			Assert.Throws(typeof(Error), { => l := (LONG) u})	// Conversion Error
			
			RETURN

		[Fact];
		METHOD UsualShortTests() AS VOID
			LOCAL u AS __Usual
			LOCAL l as LONG
			u := (__Usual) (Short)1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			l := u
			Assert.Equal(l,1)
			Assert.Equal(l,  (SHORT) u)
			u := __Usual{UInt16.MaxValue}
			Assert.Throws(typeof(Error), { => l := (SHORT) u})	// Overflow Error
			u := __Usual{"a text"}
			Assert.Throws(typeof(Error), { => l := (SHORT) u})	// Conversion Error
			
			RETURN

		[Fact];
		METHOD UsualSByteTests() AS VOID
			LOCAL u AS __Usual
			LOCAL l as SByte
			u := (__Usual) (SByte)1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			l := u
			Assert.Equal(l,1)
			Assert.Equal(l,  (SByte) u)
			u := __Usual{Byte.MaxValue}
			Assert.Throws(typeof(Error), { => l := (SByte) u})	// Overflow Error
			u := __Usual{"a text"}
			Assert.Throws(typeof(Error), { => l := (SByte) u})	// Conversion Error
			RETURN

		[Fact];
		METHOD UsualUInt64Tests() AS VOID
			LOCAL u AS __Usual
			LOCAL d as UInt64
			u := (__Usual) (Uint64) 1
			Assert.Equal(UsualType(u), (DWORD) INT64)
			d := u
			Assert.Equal(d,1U)
			Assert.Equal(d,  (UInt64) u)
			u := (__Usual) -1
			Assert.Throws(typeof(Error), { => d := (UInt64) u})	// Overflow Error
			u := __Usual{"a text"}
			Assert.Throws(typeof(Error), { => d := (UInt64) u})	// Conversion Error
			RETURN

		[Fact];
		METHOD UsualDwordTests() AS VOID
			LOCAL u AS __Usual
			LOCAL d as DWORD
			u := (__Usual) (DWORD) 1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			d := u
			Assert.Equal(d,1U)
			Assert.Equal(d,  (DWORD) u)
			u := (__Usual) -1
			Assert.Throws(typeof(Error), { => d := (DWORD) u})	// Overflow Error
			u := __Usual{"a text"}
			Assert.Throws(typeof(Error), { => d := (DWORD) u})	// Conversion Error
			RETURN

		[Fact];
		METHOD UsualwordTests() AS VOID
			LOCAL u AS __Usual
			LOCAL w as WORD
			u := (__Usual) (WORD) 1
			Assert.Equal(UsualType(u), (DWORD) LONG)
			w := u
			Assert.Equal(w,1)
			Assert.Equal(w,  (WORD) u)
			u := (__Usual) -1
			Assert.Throws(typeof(Error), { => w := (WORD) u})	// Overflow Error
			u := __Usual{"a text"}
			Assert.Throws(typeof(Error), { => w := (WORD) u})	// Conversion Error
			RETURN

		[Fact];
		METHOD UsualByteTests() AS VOID
			LOCAL u AS __Usual
			LOCAL b as Byte
			b := 1
			u := (__Usual) b
			Assert.Equal(UsualType(u), (DWORD) LONG)
			b := u
			Assert.Equal(b,1U)
			Assert.Equal(b,  (BYTE) u)
			u := (__Usual) -1
			Assert.Throws(typeof(Error), { => b := (BYTE) u})	// Overflow Error
			u := __Usual{"a text"}
			Assert.Throws(typeof(Error), { => b := (BYTE) u})	// Conversion Error
			RETURN

		[Fact];
		METHOD UsualAddOperatorTests() AS VOID
			LOCAL u AS __Usual
			u := (__Usual) 1
			u += 1
			Assert.Equal((Int) u, 2)
			u += (int64) 1
			Assert.Equal((Int) u, 3)
			u += 1.0
			Assert.Equal((Int) u, 4)
			u++
			Assert.Equal((Int) u, 5)
			Assert.Throws(typeof(Error), { => u += "a"})	

			u := __Usual{(INT64) 1}
			u += (int) 1
			Assert.Equal((Int64) u, 2)
			u += (int64) 1
			Assert.Equal((Int64) u, 3)
			u += 1.0
			Assert.Equal((Int64) u, 4)
			u++
			Assert.Equal((Int64) u, 5)
			Assert.Throws(typeof(Error), { => u += "a"})	

			u := (__Usual)  1.0
			u += 1
			Assert.Equal((Real8) u, 2.0)
			u += (int64) 1
			Assert.Equal((Real8) u, 3.0)
			u += 1.0
			Assert.Equal((Real8) u, 4.0)
			u ++
			Assert.Equal((Real8) u, 5.0)
			Assert.Throws(typeof(Error), { => u += "a"})	

			u := (__Usual)  "abc"
			u += "def"
			Assert.Equal((string) u, "abcdef")
			Assert.Throws(typeof(Error), { => u += 1})	

			var d := (__VoDate) datetime.Now
			u := d
			u += 1
			d += 1
			Assert.Equal((__VoDate) u, d)
			Assert.Equal(u, (__Usual) d)

		[Fact];
		METHOD UsualSubOperatorTests() AS VOID
			LOCAL u AS __Usual
			u := (__Usual) 100
			u -= 1
			Assert.Equal((Int) u, 99)
			u -= (int64) 1
			Assert.Equal((Int) u, 98)
			u -= 1.0
			Assert.Equal((Int) u, 97)
			u--
			Assert.Equal((Int) u, 96)
			Assert.Throws(typeof(Error), { => u -= "a"})	

			u := __Usual{(INT64) 1000}
			u -= (int) 1
			Assert.Equal((Int64) u, 999)
			u -= (int64) 1
			Assert.Equal((Int64) u, 998)
			u -= 1.0
			Assert.Equal((Int64) u, 997)
			u--
			Assert.Equal((Int64) u, 996)
			Assert.Throws(typeof(Error), { => u -= "a"})	

			u := (__Usual)  111.1
			u -= 1
			Assert.Equal((Real8) u, 110.1)
			u -= (int64) 1
			Assert.Equal((Real8) u, 109.1)
			u -= 1.0
			Assert.Equal((Real8) u, 108.1)
			u--
			Assert.Equal((Real8) u, 107.1)
			Assert.Throws(typeof(Error), { => u -= "a"})	

			u := (__Usual)  "abc"
			u -= "def"
			Assert.Equal((string) u, "abcdef")
			Assert.Throws(typeof(Error), { => u -= 1})	

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests