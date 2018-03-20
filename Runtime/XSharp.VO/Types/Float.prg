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
	[StructLayout(LayoutKind.Explicit)];
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
			virtual method Equals(rhs as object  ) as logic
				local result as logic
				if rhs != null .and. rhs is __VoFloat
					result := self:Equals( (__VoFLoat) rhs)
				else
					result := false
				endif
				return result
			
			virtual method GetHashCode() as int
				return self:_value:GetHashCode()
			
			method GetTypeCode() as TypeCode
				return TypeCode.Double
			
			method Equals( rhs as __VOFLoat) as logic
				//Todo Use SetFloatDelta
				return self:_value:Equals(rhs:_value)
			
			operator ==(lhs as __VoFLoat, rhs as __VoFLoat) as logic
				return lhs:EQUALS(rhs)
			
			operator !=(lhs as __VoFLoat, rhs as __VoFLoat) as logic
				return ! lhs:Equals(rhs)
		#endregion
		
		#region Comparison Operators
			operator >(lhs as __VoFLoat, rhs as __VoFLoat) as logic
				//Todo Use SetFloatDelta
				return lhs:_Value > rhs:_Value
			
			operator <(lhs as __VoFLoat, rhs as __VoFLoat) as logic
				//Todo Use SetFloatDelta
				return lhs:_Value < rhs:_Value
			
			operator >=(lhs as __VoFLoat, rhs as __VoFLoat) as logic
				//Todo Use SetFloatDelta
				return lhs:_Value >= rhs:_Value
			
			operator <=(lhs as __VoFLoat, rhs as __VoFLoat) as logic
				//Todo Use SetFloatDelta
				return lhs:_Value <= rhs:_Value
		#endregion
		
		#region Implicit Converters
			static operator implicit(value as byte) as __VoFLoat
				return __VoFloat{value, 0}
			
			static operator implicit(value as SByte) as __VoFLoat
				return __VoFloat{value, 0}
			
			static operator implicit(value as short) as __VoFLoat
				return __VoFloat{value, 0}
			
			static operator implicit(value as word) as __VoFLoat
				return __VoFloat{value, 0}
			
			static operator implicit(value as int) as __VoFLoat
				return __VoFloat{value, 0}
			
			static operator implicit(value as dword) as __VoFLoat
				return __VoFloat{value, 0}
			
			static operator implicit(value as int64) as __VoFLoat
				return __VoFloat{value, 0}
			
			static operator implicit(value as uint64) as __VoFLoat
				return __VoFloat{value, 0}
			
			static operator implicit(value as real4) as __VoFLoat
				return __VoFloat{value, RuntimeState.Decimals}
			
			static operator implicit(value as real8) as __VoFLoat
				return __VoFloat{value, RuntimeState.Decimals}
			
			
			static operator implicit(value as System.Decimal) as __VoFLoat
				return __VoFloat{ (real8) value, RuntimeState.Decimals}
			
			static operator implicit(value as __VoFLoat) as real8
				return value:_value
			
			static operator implicit(value as __VoFLoat) as real4
				return (real4) value:_value
			
			static operator implicit(value as __VoFLoat) as System.Decimal
				return (System.Decimal) value:_value			
		#endregion
		#region Explicit Converters
			static operator explicit(value as __VoFloat) as byte
				// todo use CompilerOptionVO11 
				return (byte) value:_value
			static operator explicit(value as __VoFloat) as SByte
				return (SByte) value:_value
			static operator explicit(value as __VoFloat) as short
				// todo use CompilerOptionVO11 
				return (short) value:_value
			static operator explicit(value as __VoFloat) as word
				return (word) value:_value
			static operator explicit(value as __VoFloat) as long
				// todo use CompilerOptionVO11 
				return (long) value:_value
			static operator explicit(value as __VoFloat) as dword
				return (dword) value:_value
			static operator explicit(value as __VoFloat) as int64
				// todo use CompilerOptionVO11 
				return (int64) value:_value
			static operator explicit(value as __VoFloat) as uint64
				return (uint64) value:_value
			
		#endregion
		
		#region Numeric Operators
			operator +(value as __VoFLoat) as __VoFLoat
				return value
			
			operator -(value as __VoFLoat) as __VoFLoat
				return __VoFLoat{- value:_value, value:Digits, value:Decimals}
			
			operator+(lhs as __VoFloat, rhs as __VoFLoat) as __VoFLoat
				return lhs:Add(rhs)
			
			operator+(lhs as __VoFloat, rhs as usual) as __VoFLoat
				return lhs:Add(rhs)
			
			operator+(lhs as usual, rhs as __VoFloat) as __VoFLoat
				return rhs:Add(lhs)
			
			operator-(lhs as __VoFloat, rhs as __VoFLoat) as __VoFLoat
				return lhs:Subtract(rhs)
			
			
			operator-(lhs as __VoFloat, rhs as usual) as __VoFLoat
				return lhs:Subtract(rhs)
			
			operator-(lhs as usual, rhs as __VoFloat) as __VoFLoat
				// set decimals for LHS to 0, so max decmals is decimals right
				return __VoFLoat{lhs, 0}:Subtract(rhs)		
			
			operator*(lhs as __VoFloat, rhs as __VoFLoat) as __VoFLoat
				return __VoFloat{ lhs:_value * rhs:_value, lhs:Decimals + rhs:Decimals}
			
			operator/(lhs as __VoFloat, rhs as __VoFLoat) as __VoFLoat
				return __VoFloat{ lhs:_value / rhs:_value, RuntimeState.Decimals}
			
			operator%(lhs as __VoFloat, rhs as __VoFLoat) as __VoFLoat
				return __VoFloat{ lhs:_value % rhs:_value, RuntimeState.Decimals}
			
			operator ++ (value as __VoFLoat) as __VoFLoat
				return __VoFLoat{value:_value+1, value:Digits, value:Decimals}
			
			operator -- (value as __VoFLoat) as __VoFLoat
				return __VoFLoat{value:_value-1, value:Digits, value:Decimals}
		#endregion
		
		#region Add and Subtract
			method Add(rhs as __VoFloat) as __VoFloat
				return __VoFloat{ self:_value + rhs:_value, math.Max(self:_decimals, rhs:_decimals)}
			
			method Add(rhs as usual) as __VoFloat
				local result as __VoFLoat
				if rhs:UsualType == __UsualType.Float
					result := self:Add ( (__VoFloat) rhs)
				elseif  rhs:UsualType == __UsualType.Long
					result := __VoFLoat{ self:_value + (long) rhs, self:Digits, self:Decimals}
				else
					throw Error.ArgumentError(rhs, "Argument is not numeric")
				endif
				return result
			
			
			method Subtract(rhs as __VoFloat) as __VoFloat
				return __VoFloat{ self:_value - rhs:_value, math.Max(self:_decimals, rhs:_decimals)}
			
			method Subtract(rhs as usual) as __VoFloat
				local result as __VoFLoat
				if rhs:UsualType == __UsualType.Float
					result := self:Subtract( (__VoFloat) rhs)
				elseif  rhs:UsualType == __UsualType.Long
					result := __VoFLoat{ self:_value - (long) rhs, self:Digits, self:Decimals}			
				else
					throw Error.ArgumentError(rhs, "Argument is not numeric")
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
			public method ToString(format as string, provider as System.IFormatProvider) as string
				return ((IFormattable) _value):ToString(format, provider)
		#endregion
		#region IComparable
			public method CompareTo(rhs as __VoFLoat) as int
				return _Value:CompareTo( rhs:_Value)
			
			public method CompareTo(rhs as object) as int
				return self:CompareTo( (__VoFLoat) rhs)
		#endregion
	end structure
	
end namespace