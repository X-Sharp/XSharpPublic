//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using  System
using System.Runtime.InteropServices
using XSharp
begin namespace XSharp	
	structure __VODate implements System.IComparable, System.IFormattable, System.IConvertible, IDate
		#region static fields
		static private _dateFormat as string
		static export _NULL_DATE as __VODate
		#endregion
		#region fields
		private _value as System.DateTime
		#endregion
		#region constrcutors
		static  constructor()
			__VODate._dateFormat := "MM/DD/YY"
			_NULL_DATE := __VODate{}
		return       

		constructor(d as System.DateTime)
			_value := d:@@Date
		return

		constructor(d as iDate)
			_value := d:Value
		return

		constructor(ticks as Int64)
			try
				_value := System.DateTime{ticks}
			catch ex as Exception
				_value := System.DateTime.MinValue
			end try
		return

		constructor(strDate as string)
			throw System.NotImplementedException{"Constructor __VODate(string __VODate) is not implemented yet."}

		constructor(year as Int, month as Int, day as Int)
			if (year > 0x7fffffff)
				throw System.ArgumentOutOfRangeException{"year"}
			endif
			if (month > 0x7fffffff)
				throw System.ArgumentOutOfRangeException{"month"}
			endif
			if (day > 0x7fffffff)
				throw System.ArgumentOutOfRangeException{"day"}
			endif
			try
				_value := System.DateTime{year , month , day }
			catch ex as Exception
				_value := System.DateTime.MinValue
			end try
		return

		constructor(year as DWord, month as DWord, day as DWord)
			if (year > 0x7fffffff)
				throw System.ArgumentOutOfRangeException{"year"}
			endif
			if (month > 0x7fffffff)
				throw System.ArgumentOutOfRangeException{"month"}
			endif
			if (day > 0x7fffffff)
				throw System.ArgumentOutOfRangeException{"day"}
			endif
			try
				_value := System.DateTime{(Long)year , (Long)month , (Long)day }
			catch ex as Exception
				_value := System.DateTime.MinValue
			end try
		return
		#endregion
		#region methods
		method Add(days as __Usual) as __VODate
			THROW NotImplementedException{}

		method Add(days as real8) as __VODate
			return __VODate{(_value:AddDays(days))}

		method Add(days as Long) as __VODate
			return __VODate{(_value:AddDays(days))}

		method Add(days as Int64) as __VODate
			return __VODate{(_value:AddDays(days))}

		method Add(span as System.TimeSpan) as __VODate
			return __VODate{(_value:Add(span))}

		method Add(days as DWord) as __VODate
			return __VODate{(_value:AddDays(days))}

		method Add(days as UInt64) as __VODate
			return __VODate{(_value:AddDays(days))}

		method CompareTo(o as Object) as Long
			local @@__VODate as __VODate
			@@__VODate := (__VODate)o 
		return _value:CompareTo(@@__VODate:value)


		method Equals(o as __VODate) as Logic
		return (o:value == _value)

		method Equals(o as Object) as Logic
			if o:getType() == typeof(__VODate)
				return Equals( (__VoDate) o)
			elseif o:getType() == typeof(System.DateTime)
				return Equals( __VoDate{ (System.DateTime) o})
			endif
			return false

		method FromDateTime(v as System.DateTime) as __VODate
		return __VODate{v}

		method GetHashCode() as Long
		return _value:GetHashCode()

		method GetTypeCode() as System.TypeCode
		return _value:GetTypeCode()

		static operator +(d as __VODate, days as __Usual) as __VODate
		return d:Add(days)

		static operator +(d as __VODate, days as real8) as __VODate
		return d:Add(days)

		static operator +(d as __VODate, days as Long) as __VODate
		return d:Add(days)

		static operator +(d as __VODate, days as Int64) as __VODate
		return d:Add(days)

		static operator +(d as __VODate, ts as System.TimeSpan) as __VODate
		return d:Add(ts)

		static operator +(d as __VODate, days as DWord) as __VODate
		return d:Add(days)

		static operator +(d as __VODate, days as UInt64) as __VODate
		return d:Add(days)

		static operator --(d as __VODate) as __VODate
		return d:Subtract(1)

		static operator ==(d as __VODate, d2 as __VODate) as Logic
		return (d:value == d2:value)

		static operator explicit(v as __VODate) as DWord
			THROW NotImplementedException{}

		static operator explicit(v as System.DateTime) as __VODate
			return __VODate{v}

		static operator explicit(v as DWord) as __VODate
			THROW NotImplementedException{}

		static operator >(d as __VODate, d2 as __VODate) as Logic
		return (d:value > d2:value)

		static operator >=(d as __VODate, d2 as __VODate) as Logic
		return (d:value >= d2:value)

		static operator implicit(v as __VODate) as System.DateTime
		return v:value

		static operator ++(d as __VODate) as __VODate
		return d:Add(1)

		static operator !=(d as __VODate, d2 as __VODate) as Logic
		return (d:value != d2:value)

		static operator <(d as __VODate, d2 as __VODate) as Logic
		return (d:value < d2:value)

		static operator <=(d as __VODate, d2 as __VODate) as Logic
		return (d:value <= d2:value)

		static operator -(d as __VODate, d2 as __VODate) as Long
		return d:Subtract(d2)

		static operator -(d as __VODate, days as __Usual) as __VODate
		return d:Subtract(days)

		static operator -(d as __VODate, days as real8) as __VODate
		return d:Subtract(days)

		static operator -(d as __VODate, days as Long) as __VODate
		return d:Subtract(days)

		static operator -(d as __VODate, days as Int64) as __VODate
		return d:Subtract(days)

		static operator -(d as __VODate, ts as System.TimeSpan) as __VODate
		return d:Subtract(ts)

		static operator -(d as __VODate, days as DWord) as __VODate
		return d:Subtract(days)

		static operator -(d as __VODate, days as UInt64) as __VODate
		return d:Subtract(days)

		method Subtract(d as __VODate) as Long
			local span as System.TimeSpan
			span := (System.TimeSpan)(_value - d:value) 
		return span:Days

		method Subtract(days as __Usual) as __VODate
		return SELF:Add(-days)		

		method Subtract(days as real8) as __VODate
		return SELF:Add(-days)

		method Subtract(days as Long) as __VODate
		return SELF:Add(-days)

		method Subtract(days as Int64) as __VODate
		return SELF:Add(-days)

		method Subtract(ts as System.TimeSpan) as __VODate
		return SELF:Add(-ts)

		method Subtract(days as DWord) as __VODate
		return SELF:Add(-days)

		method Subtract(days as UInt64) as __VODate
		return SELF:Add(-(int64)days)

		method ToBoolean(provider as System.IFormatProvider) as Logic
			THROW NotImplementedException{}

		method ToByte(provider as System.IFormatProvider) as Byte
			THROW NotImplementedException{}

		method ToChar(provider as System.IFormatProvider) as Char
			THROW NotImplementedException{}

		method ToDateTime() as System.DateTime
		return _value

		method ToDateTime(provider as System.IFormatProvider) as System.DateTime
			THROW NotImplementedException{}

		method ToDecimal(provider as System.IFormatProvider) as Decimal
			THROW NotImplementedException{}

		method ToDouble(provider as System.IFormatProvider) as real8
			THROW NotImplementedException{}

		method ToInt16(provider as System.IFormatProvider) as Short
			THROW NotImplementedException{}

		method ToInt32(provider as System.IFormatProvider) as Long
			THROW NotImplementedException{}

		method ToInt64(provider as System.IFormatProvider) as Int64
			THROW NotImplementedException{}

		method ToSByte(provider as System.IFormatProvider) as SByte
			THROW NotImplementedException{}

		method ToSingle(provider as System.IFormatProvider) as real4
			THROW NotImplementedException{}

		method ToString() as string
		return _value:ToString("d")

		method ToString(provider as System.IFormatProvider) as string
		return _value:ToString(provider)

		method ToString(s as string) as string
		return _value:ToString(s)

		method ToString(s as string, fp as System.IFormatProvider) as string
		return _value:ToString(s, fp)

		method ToType(conversionType as System.Type, provider as System.IFormatProvider) as Object
			THROW NotImplementedException{}

		method ToUInt16(provider as System.IFormatProvider) as Word
			THROW NotImplementedException{}

		method ToUInt32(provider as System.IFormatProvider) as DWord
			THROW NotImplementedException{}

		method ToUInt64(provider as System.IFormatProvider) as UInt64
			THROW NotImplementedException{}
		#endregion
		#region properties

		property Day as int
			Get
				return IIF(! (_value == System.DateTime.MinValue),_value:Day ,0)
			End Get
			Set
				_value := DateTime{_value:Year, _value:Month, value}
			End Set
		end property

		property DayOfWeek as Long
			Get
				return (IIF(! (_value == System.DateTime.MinValue),(Long)_value:DayOfWeek ,0) + 1)
			End Get
		end property


		property IsEmpty as Logic
			Get
				return (_value == System.DateTime.MinValue)
			End Get
		end property

		property Month as int
			Get
				return IIF(! (_value == System.DateTime.MinValue),_value:Month,0)
			End Get
			Set
				_value := DateTime{_value:Year, value, _value:Day}
			End Set
		end property

		property Value as System.DateTime
			Get
				return self:_value
			End Get
			Set
				self:_value := value:@@Date
			End Set
		end property

		property Year as Long 
			Get
				return IIF(! (_value == System.DateTime.MinValue),_value:Year,0)
			End Get
			Set
				self:_value := DateTime{value, _value:Month, _value:Day}
			end Set
		end property
	#region Static Properties
		static property DateFormat as string
			Get
				return __VODate._dateFormat
			End Get
			Set
				__VODate._dateFormat := value
			End Set
		end property

		static property Epoch as Long
			Get
				return System.Threading.Thread.CurrentThread:CurrentCulture:Calendar:TwoDigitYearMax - 100
			End Get
			Set
				System.Threading.Thread.CurrentThread:CurrentCulture:Calendar:TwoDigitYearMax := (Long)value  + 100
			End Set
		end property

		static property NullDate as __VODate
			Get
				return (__VODate)System.DateTime.MinValue 
			End Get
		end property

		static method ElapTime(cStartTime as string, cEndTime as string) as string
			return System.DateTime.ParseExact(cEndTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture):Subtract(System.DateTime.ParseExact(cStartTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture)):ToString()

	#endregion

	end structure
end namespace
