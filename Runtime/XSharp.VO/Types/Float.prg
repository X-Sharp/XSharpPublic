//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Runtime.InteropServices

begin namespace XSharp
	// use explicit layout so we can compact the size into 12 bytes
	// Type is Immutable, so no settable properties
	[StructLayout(LayoutKind.Explicit, Pack := 4)];
	public structure __VOFloat implements IFLoat, ;
		IConvertible,; 
		IFormattable, ;
		IComparable<__VoFloat>, ;
		IComparable
		[FieldOffset(0)]  private initonly _value as real8
		[FieldOffset(8)]  private initonly _length as shortint
		[FieldOffset(10)] private initonly _decimals as shortint
		
		#region constructors
			constructor (r8 as real8)
				self:_value    := r8
				self:_length   := 0
				self:_decimals := 0
			
			constructor (r8 as real8, decimals as int)
				self:_value    := r8
				self:_length   := 0
				self:_decimals := (shortint) decimals 
			
			constructor (r8 as real8, decimals as dword)
				self:_value    := r8
				self:_length   := 0
				self:_decimals := (shortint) decimals 
			
			constructor (r8 as real8, length as dword, decimals as dword)
				self:_value    := r8
				self:_length   := (shortint) length
				self:_decimals := (shortint) decimals 
			
			constructor (r8 as real8, length as int, decimals as int )
				self:_value    := r8
				self:_length   := (shortint) length
				self:_decimals := (shortint) decimals
			
			constructor (value as IFloat)
				self:_value		:= value:value
				self:_length	:= (short) value:Digits
				self:_decimals	:= (short) value:Decimals
		#endregion
		#region Properties
			property Value    as real8	get _value			
			property Digits   as int	get _length		
			property Decimals as int	get _decimals	
		#endregion
		
		#region Equality Operators
			override method Equals(rhs as object  ) as logic
				local result as logic
				if rhs != null .and. rhs is Float
					result := self:Equals( (Float) rhs)
				else
					result := false
				endif
				return result

			method equals(rhs as float ) as logic
				local delta as real8
				local diff  as real8
				local equal as logic
				delta := RuntimeState.FloatDelta
				diff := _value - rhs:_value
				if delta == 0.0
					equal :=  diff == 0.0
				elseif diff < 0.0
					equal :=  Math.Abs(diff) < delta
				else
					equal := diff < delta
				endif
				return equal

			virtual method GetHashCode() as int
				return self:_value:GetHashCode()
			
			method GetTypeCode() as TypeCode
				return TypeCode.Double
			
			
			operator ==(lhs as Float, rhs as Float) as logic
				return lhs:Equals(rhs)
			
			operator !=(lhs as Float, rhs as Float) as logic
				return ! lhs:Equals(rhs)
		#endregion
		
		#region Comparison Operators
			operator >(lhs as Float, rhs as Float) as logic
				local delta as real8
				local diff  as real8
				delta := RuntimeState.FloatDelta
				diff := lhs:_value - rhs:_value
				if (delta == 0.0)
					return diff > 0.0
				endif
				return diff > delta
				
			
			operator <(lhs as Float, rhs as Float) as logic
				local delta as real8
				local diff  as real8
				delta := RuntimeState.FloatDelta
				diff := lhs:_value - rhs:_value
				if (delta == 0.0)
					return diff < 0.0
				endif
				return diff < delta
			
			operator >=(lhs as float, rhs as float) as logic
				// call other operator methods for simplicity
				// we may want to optimize this later
				return lhs > rhs .or. lhs == rhs

			operator <=(lhs as Float, rhs as Float) as logic
				// call other operator methods for simplicity
				// we may want to optimize this later
				return lhs < rhs .or. lhs == rhs

		#endregion
		
		#region Implicit Converters
			static operator implicit(b as byte) as Float
				return Float{b, 0}
			
			static operator implicit(sb as SByte) as Float
				return Float{sb, 0}
			
			static operator implicit(si as short) as Float
				return Float{si, 0}
			
			static operator implicit(w as word) as Float
				return Float{w, 0}
			
			static operator implicit(i as int) as Float
				return Float{i, 0}
			
			static operator implicit(dw as dword) as Float
				return Float{dw, 0}
			
			static operator implicit(i64 as int64) as Float
				return Float{i64, 0}
			
			static operator implicit(ui64 as uint64) as Float
				return Float{ui64, 0}
			
			static operator implicit(r4 as real4) as Float
				return Float{r4, RuntimeState.Decimals}
			
			static operator implicit(r8 as real8) as Float
				return Float{r8, RuntimeState.Decimals}
			
			
			static operator implicit(value as System.Decimal) as Float
				return Float{ (real8) value, RuntimeState.Decimals}
			
			static operator implicit(fl  as Float) as real8
				return fl:_value
			
			static operator implicit(fl  as Float) as real4
				return (real4) fl:_value
			
			static operator implicit(fl  as Float) as System.Decimal
				return (System.Decimal) fl:_value			
		#endregion
		#region Explicit Converters
			static operator explicit(fl  as Float) as byte
				if RuntimeState.CompilerOptionVO11
					return Convert.ToByte(fl:_value)
				endif
				return (byte) fl:_value
			static operator explicit(fl as Float) as SByte
				return (SByte) fl:_value
			static operator explicit(fl  as Float) as short
				if RuntimeState.CompilerOptionVO11
					return Convert.ToInt16(fl:_value)
				endif
				return (short) fl:_value
			static operator explicit(fl  as Float) as word
				return (word) fl:_value
			static operator explicit(fl  as Float) as long
				if RuntimeState.CompilerOptionVO11
					return Convert.ToInt32(fl:_value)
				endif
				return (long) fl:_value
			static operator explicit(fl  as Float) as dword
				return (dword) fl:_value
			static operator explicit(fl  as Float) as int64
				if RuntimeState.CompilerOptionVO11
					return Convert.ToInt64(fl:_value)
				endif
				return (int64) fl:_value
			static operator explicit(fl  as Float) as uint64
				return (uint64) fl:_value
			
		#endregion
		
		#region Numeric Operators
			operator +(fl  as Float) as Float
				return fl
			
			operator -(fl  as Float) as Float
				return Float{- fl:_value, fl:Digits, fl:Decimals}
			
			operator+(lhs as Float, rhs as Float) as Float
				return lhs:Add(rhs)
			
			operator+(lhs as Float, rhs as usual) as Float
				return lhs:Add(rhs)
			
			operator+(lhs as usual, rhs as Float) as Float
				return rhs:Add(lhs)
			
			operator-(lhs as Float, rhs as Float) as Float
				return lhs:Subtract(rhs)
			
			operator-(lhs as Float, rhs as usual) as Float
				return lhs:Subtract(rhs)
			
			operator-(lhs as usual, rhs as Float) as Float
				// set decimals for LHS to 0, so max decmals is decimals right
				return Float{lhs, 0}:Subtract(rhs)		
			
			operator*(lhs as Float, rhs as Float) as Float
				return Float{ lhs:_value * rhs:_value, lhs:Decimals + rhs:Decimals}
			
			operator/(lhs as Float, rhs as Float) as Float
				return Float{ lhs:_value / rhs:_value, RuntimeState.Decimals}
			
			operator%(lhs as Float, rhs as Float) as Float
				return Float{ lhs:_value % rhs:_value, RuntimeState.Decimals}
			
		#endregion
		#region Unary Operators
			operator ++ (fl  as Float) as Float
				return Float{fl:_value+1, fl:Digits, fl:Decimals}
			
			operator -- (fl  as Float) as Float
				return Float{fl:_value-1, fl:Digits, fl:Decimals}
		#endregion
		
		#region Explicit casts. Used inside Transform
	   METHOD CastToInt() AS INT
		  RETURN (INT)(SELF:_value)

	   METHOD CastToInt64() AS INT64
		  RETURN (INT64)(SELF:_value)

		#endregion
		#region Add and Subtract
			method Add(rhs as Float) as Float
				return Float{ self:_value + rhs:_value, math.Max(self:_decimals, rhs:_decimals)}
			
			method Add(rhs as usual) as Float
				local result as Float
				if rhs:IsFloat
					result := self:Add ( (Float) rhs)
				elseif rhs:IsDecimal
					result := self:Add ( (System.Decimal) rhs)
				elseif  rhs:IsLong
					result := Float{ self:_value + (long) rhs, self:Digits, self:Decimals}
				else
					throw Error.ArgumentError(__ENTITY__,nameof(rhs), "Argument is not numeric")
				endif
				return result
			
			
			method Subtract(rhs as Float) as Float
				return Float{ self:_value - rhs:_value, math.Max(self:_decimals, rhs:_decimals)}
			
			method Subtract(rhs as usual) as Float
				local result as Float
				if rhs:IsFloat
					result := self:Subtract( (Float) rhs)
				elseif rhs:IsDecimal
					result := self:Subtract( (System.Decimal) rhs)
				elseif  rhs:IsLong
					result := Float{ self:_value - (long) rhs, self:Digits, self:Decimals}			
				else
					throw Error.ArgumentError(__ENTITY__,nameof(rhs), "Argument is not numeric")
				endif
				return result
			
			
		#endregion
		
		#region IConvertable
			public method ToBoolean(provider as System.IFormatProvider) as logic
				throw NotImplementedException{}
			
			public method ToByte(provider as System.IFormatProvider) as byte
				return ((IConvertible) _value):ToByte(provider)
			
			public method ToChar(provider as System.IFormatProvider) as char
				return ((IConvertible) _value):ToChar(provider)
			
			public method ToDateTime(provider as System.IFormatProvider) as System.DateTime
				return ((IConvertible) _value):ToDateTime(provider)
			
			public method ToDecimal(provider as System.IFormatProvider) as Decimal
				return ((IConvertible) _value):ToDecimal(provider)
			
			public method ToDouble(provider as System.IFormatProvider) as real8
				return ((IConvertible) _value):ToDouble(provider)
			
			public method ToInt16(provider as System.IFormatProvider) as short
				return ((IConvertible) _value):ToInt16(provider)
			
			public method ToInt32(provider as System.IFormatProvider) as long
				return ((IConvertible) _value):ToInt32(provider)
			
			public method ToInt64(provider as System.IFormatProvider) as int64
				return ((IConvertible) _value):ToInt64(provider)
			
			public method ToSByte(provider as System.IFormatProvider) as SByte
				return ((IConvertible) _value):ToSByte(provider)
			
			public method ToSingle(provider as System.IFormatProvider) as real4
				return ((IConvertible) _value):ToSingle(provider)
			
			public method ToType(conversionType as System.Type, provider as System.IFormatProvider) as object
				return ((IConvertible) _value):ToType(conversionType, provider)
			
			public method ToUInt16(provider as System.IFormatProvider) as word
				return ((IConvertible) _value):ToUInt16(provider)
			
			public method ToUInt32(provider as System.IFormatProvider) as dword
				return ((IConvertible) _value):ToUInt32(provider)
			
			public method ToUInt64(provider as System.IFormatProvider) as uint64
				return ((IConvertible) _value):ToUInt64(provider)
			
			public method ToString(provider as System.IFormatProvider) as string
				return ((IConvertible) _value):ToString(provider)
		#endregion
		#region IFormattable
			public method ToString() as string
				return Str1(self)
			
			public method ToString(sFormat as STRING) as string
				return _value:ToString(sFormat)

			public method ToString(format as string, provider as System.IFormatProvider) as string
				return ((IFormattable) _value):ToString(format, provider)
		#endregion
		#region IComparable
			public method CompareTo(rhs as Float) as int
				return _Value:CompareTo( rhs:_Value)
			
			public method CompareTo(rhs as object) as int
				return self:CompareTo( (Float) rhs)
		#endregion
	end structure
	
end namespace