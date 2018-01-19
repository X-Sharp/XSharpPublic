//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Runtime.InteropServices
using XSharp.Runtime
begin namespace XSharp
	// use explicit layout so we can compact the size into 12 bytes
	// Type is Immutable, so no settable properties
	[StructLayout(LayoutKind.Explicit)];
	PUBLIC STRUCTURE __VOFloat IMPLEMENTS IFLoat, ;
		IConvertible,; 
		IFormattable, ;
		IComparable<__VoFloat>, ;
		IComparable
		[FieldOffset(0)]  PRIVATE INITONLY _value as Real8
		[FieldOffset(8)]  PRIVATE INITONLY _length as ShortInt
		[FieldOffset(10)] PRIVATE INITONLY _decimals as ShortInt

#region constructors
		CONSTRUCTOR (r8 as Real8)
			self:_value    := r8
			self:_length   := 0
			self:_decimals := 0

		CONSTRUCTOR (r8 as Real8, decimals as int)
			self:_value    := r8
			self:_length   := 0
			self:_decimals := (shortint) decimals 

		CONSTRUCTOR (r8 as Real8, decimals as DWORD)
			self:_value    := r8
			self:_length   := 0
			self:_decimals := (shortint) decimals 

		CONSTRUCTOR (r8 as Real8, length as int, decimals as int )
			self:_value    := r8
			self:_decimals := (shortint) decimals
			SELF:_length   := (SHORTINT) length

		CONSTRUCTOR (value AS IFloat)
			SELF:_value		:= value:value
			SELF:_length	:= (SHORT) value:Digits
			self:_decimals	:= (SHORT) value:Decimals
#endregion
#region Properties
		PROPERTY Value    AS REAL8	GET _value		
		PROPERTY Digits   AS INT	GET _length		
		PROPERTY Decimals AS INT	GET _decimals	
#endregion

#region Equality Operators
		VIRTUAL METHOD Equals(rhs as OBJECT  ) AS LOGIC
			local result as LOGIC
			IF rhs != null .and. rhs IS __VoFloat
				result := Equals( (__VoFLoat) rhs)
			ELSE
				result := FALSE
			ENDIF
			RETURN result

		VIRTUAL METHOD GetHashCode() as INT
			RETURN SELF:_value:GetHashCode()

		METHOD GetTypeCode() as TypeCode
			RETURN SELF:_value:GetTypeCode()

		METHOD Equals( rhs as __VOFLoat) AS LOGIC
			//Todo Use SetFloatDelta
			RETURN SELF:_value:Equals(rhs:_value)

		OPERATOR ==(lhs AS __VoFLoat, rhs AS __VoFLoat) AS LOGIC
			RETURN lhs:EQUALS(rhs)

		OPERATOR !=(lhs AS __VoFLoat, rhs AS __VoFLoat) AS LOGIC
			return ! lhs:Equals(rhs)
#endregion

#region Comparison Operators
		OPERATOR >(lhs AS __VoFLoat, rhs AS __VoFLoat) AS LOGIC
			//Todo Use SetFloatDelta
			RETURN lhs._Value > rhs._Value

		OPERATOR <(lhs AS __VoFLoat, rhs AS __VoFLoat) AS LOGIC
			//Todo Use SetFloatDelta
			RETURN lhs._Value < rhs._Value

		OPERATOR >=(lhs AS __VoFLoat, rhs AS __VoFLoat) AS LOGIC
			//Todo Use SetFloatDelta
			RETURN lhs._Value >= rhs._Value

		OPERATOR <=(lhs AS __VoFLoat, rhs AS __VoFLoat) AS LOGIC
			//Todo Use SetFloatDelta
			RETURN lhs._Value <= rhs._Value
#endregion

#region Implicit Converters
	static operator implicit(value as Byte) as __VoFLoat
		return __VoFloat{value, 0}

	static operator implicit(value as SByte) as __VoFLoat
		return __VoFloat{value, 0}

	static operator implicit(value as Short) as __VoFLoat
		return __VoFloat{value, 0}

	static operator implicit(value as Word) as __VoFLoat
		return __VoFloat{value, 0}

	static operator implicit(value as Int) as __VoFLoat
		RETURN __VoFloat{value, 0}

	static operator implicit(value as DWORD) as __VoFLoat
		RETURN __VoFloat{value, 0}

	static operator implicit(value as Int64) as __VoFLoat
		return __VoFloat{value, 0}

	static operator implicit(value as UInt64) as __VoFLoat
		return __VoFloat{value, 0}

	static operator implicit(value as Real4) as __VoFLoat
		return __VoFloat{value, Runtime.State.Decimals}

	static operator implicit(value as Real8) as __VoFLoat
		return __VoFloat{value, Runtime.State.Decimals}

	static operator implicit(value as __VoFLoat) as REAL8
		return value._value

	static operator implicit(value as __VoFLoat) as REAL4
		return (Real4) value._value
		
#endregion
#region Explicit Converters
	static operator explicit(value as __VoFloat) as Byte
		return (Byte) value:_value
	static operator explicit(value as __VoFloat) as SByte
		return (SByte) value:_value
	static operator explicit(value as __VoFloat) as Short
		return (Short) value:_value
	static operator explicit(value as __VoFloat) as Word
		return (Word) value:_value
	static operator explicit(value as __VoFloat) as LONG
		return (Long) value:_value
	static operator explicit(value as __VoFloat) as DWORD
		return (DWORD) value:_value
	static operator explicit(value as __VoFloat) as INT64
		return (INT64) value:_value
	static operator explicit(value as __VoFloat) as UINT64
		return (UINT64) value:_value
	static operator explicit(value as __VoFloat) as Decimal
		return (Decimal) value:_value

#endregion

#region Numeric Operators
		OPERATOR +(value AS __VoFLoat) AS __VoFLoat
			RETURN value
		
		OPERATOR -(value AS __VoFLoat) AS __VoFLoat
			return __VoFLoat{- value:_value, value:Digits, value:Decimals}
		
		OPERATOR+(lhs AS __VoFloat, rhs AS __VoFLoat) AS __VoFLoat
			RETURN lhs:Add(rhs)
		
		OPERATOR+(lhs AS __VoFloat, rhs AS __Usual) AS __VoFLoat
			RETURN lhs:Add(rhs)

		OPERATOR+(lhs AS __Usual, rhs AS __VoFloat) AS __VoFLoat
			RETURN rhs:Add(lhs)

		OPERATOR-(lhs AS __VoFloat, rhs AS __VoFLoat) AS __VoFLoat
			RETURN lhs:Subtract(rhs)


		OPERATOR-(lhs AS __VoFloat, rhs AS __Usual) AS __VoFLoat
			RETURN lhs:Subtract(rhs)

		OPERATOR-(lhs AS __Usual, rhs AS __VoFloat) AS __VoFLoat
			// set decimals for LHS to 0, so max decmals is decimals right
			RETURN __VoFLoat{lhs, 0}:Subtract(rhs)		

		OPERATOR*(lhs AS __VoFloat, rhs AS __VoFLoat) AS __VoFLoat
			RETURN __VoFloat{ lhs:_value * rhs:_value, lhs:Decimals + rhs:Decimals}

		OPERATOR/(lhs AS __VoFloat, rhs AS __VoFLoat) AS __VoFLoat
			RETURN __VoFloat{ lhs:_value / rhs:_value, Runtime.State.Decimals}

		OPERATOR%(lhs AS __VoFloat, rhs AS __VoFLoat) AS __VoFLoat
			RETURN __VoFloat{ lhs:_value % rhs:_value, Runtime.State.Decimals}

		OPERATOR ++ (value AS __VoFLoat) AS __VoFLoat
			return __VoFLoat{value:_value+1, value:Digits, value:Decimals}

		OPERATOR -- (value AS __VoFLoat) AS __VoFLoat
			return __VoFLoat{value:_value-1, value:Digits, value:Decimals}
#endregion

#region Add and Subtract
		METHOD Add(rhs AS __VoFloat) AS __VoFloat
			RETURN __VoFloat{ SELF:_value + rhs:_value, math.Max(SELF:_decimals, rhs:_decimals)}

		METHOD Add(rhs AS __Usual) AS __VoFloat
			LOCAL result AS __VoFLoat
			IF rhs:UsualType == __UsualType.Float
				result := Self:Add ( (__VoFloat) rhs)
			ELSEIF  rhs:UsualType == __UsualType.Long
				result := __VoFLoat{ SELF:_value + (Long) rhs, self:Digits, self:Decimals}
			ELSE
				throw Error.ArgumentError(rhs, "Argument is not numeric")
			ENDIF
			RETURN result


		METHOD Subtract(rhs AS __VoFloat) AS __VoFloat
			RETURN __VoFloat{ SELF:_value - rhs:_value, math.Max(SELF:_decimals, rhs:_decimals)}

		METHOD Subtract(rhs AS __Usual) AS __VoFloat
			LOCAL result AS __VoFLoat
			IF rhs:UsualType == __UsualType.Float
				result := Self:Subtract( (__VoFloat) rhs)
			ELSEIF  rhs:UsualType == __UsualType.Long
				result := __VoFLoat{ SELF:_value - (LONG) rhs, SELF:Digits, SELF:Decimals}			
			ELSE
				throw Error.ArgumentError(rhs, "Argument is not numeric")
			ENDIF
			RETURN result


#endregion

#region IConvertable
		public method ToBoolean(provider as System.IFormatProvider) as Logic
			THROW NotImplementedException{}

		public method ToByte(provider as System.IFormatProvider) as Byte
			return ((IConvertible) _value):ToByte(provider)

		public method ToChar(provider as System.IFormatProvider) as Char
			return ((IConvertible) _value):ToChar(provider)

		public method ToDateTime(provider as System.IFormatProvider) as System.DateTime
			return ((IConvertible) _value):ToDateTime(provider)

		public method ToDecimal(provider as System.IFormatProvider) as Decimal
			return ((IConvertible) _value):ToDecimal(provider)

		public method ToDouble(provider as System.IFormatProvider) as real8
			return ((IConvertible) _value):ToDouble(provider)

		public method ToInt16(provider as System.IFormatProvider) as Short
			return ((IConvertible) _value):ToInt16(provider)

		public method ToInt32(provider as System.IFormatProvider) as Long
			return ((IConvertible) _value):ToInt32(provider)

		public method ToInt64(provider as System.IFormatProvider) as Int64
			return ((IConvertible) _value):ToInt64(provider)

		public method ToSByte(provider as System.IFormatProvider) as SByte
			return ((IConvertible) _value):ToSByte(provider)

		public method ToSingle(provider as System.IFormatProvider) as real4
			RETURN ((IConvertible) _value):ToSingle(provider)

		public method ToType(conversionType as System.Type, provider as System.IFormatProvider) as Object
			return ((IConvertible) _value):ToType(conversionType, provider)

		public method ToUInt16(provider as System.IFormatProvider) as Word
			RETURN ((IConvertible) _value):ToUInt16(provider)

		public method ToUInt32(provider as System.IFormatProvider) as DWord
			RETURN ((IConvertible) _value):ToUInt32(provider)

		public method ToUInt64(provider as System.IFormatProvider) as UInt64
			RETURN ((IConvertible) _value):ToUInt64(provider)

		public method ToString(provider as System.IFormatProvider) as string
			RETURN ((IConvertible) _value):ToString(provider)
#endregion
#region IFormattable
		public method ToString(format as string, provider as System.IFormatProvider) as string
			RETURN ((IFormattable) _value):ToString(format, provider)
#endregion
#region IComparable
		PUBLIC METHOD CompareTo(rhs AS __VoFLoat) as INT
			return _Value:CompareTo( rhs:_Value)

		PUBLIC METHOD CompareTo(rhs AS OBJECT) as INT
			return self:CompareTo( (__VoFLoat) rhs)
#endregion
	END STRUCTURE

end namespace