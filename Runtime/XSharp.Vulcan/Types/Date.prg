//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Runtime.Serialization
using System.Globalization
using System.Runtime.InteropServices
using XSharp
using System.Diagnostics
begin namespace XSharp	
    [DebuggerDisplay("{ToString(),nq}", Type := "DATE" )];
    [StructLayout(LayoutKind.Explicit)];
	STRUCTURE __VODate IMPLEMENTS System.IComparable, ;
		System.IFormattable, ;
		System.IConvertible, ;
		IDate, ;
		IComparable<__VoDate>, ;
		IEquatable<__VoDate>
        // This structure uses an explicit layout to map
        // year, month and date into 32 bits
        // the _value field is for convenience, so we can do a single numeric comparison
        // to determine equality or relative comparisons in stead of doing 3 comparisons
        // for date calculation we use the Value property which returns a System.DateTime type
		// Note that the Vulcan type uses a datetime which takes 8 bytes. We only use 4 bytes
		#region fields
        [FieldOffSet(00)] private initonly _value as System.Int32
        [FieldOffSet(00)] private initonly _year  as System.UInt16
        [FieldOffSet(02)] private initonly _month as System.Byte
        [FieldOffSet(03)] private initonly _day   as System.Byte
		#endregion

		#region static fields
		// TODO move to settings object later
		static private _dateFormat as string
		static initonly public  _NULL_DATE as __VODate

		#endregion

        #region datetime conversions
        Property Value as System.DateTime 
        get
            if (_value == 0)
                return System.DateTime.MinValue
            endif
            return System.DateTime{_year, _month, _day}
        end get
        end Property
        #endregion

		#region constructors
		static constructor()
			_dateFormat := System.Globalization.DateTimeFormatInfo.CurrentInfo:ShortDatePattern
			_NULL_DATE := __VODate{}
		return       

		constructor(d as System.DateTime)
			_year  := (Word) d:Year
            _month := (Byte) d:Month
            _day   := (Byte) d:Day
		    return

		constructor(d as iDate)
			_year  := (Word) d:Year
            _month := (Byte) d:Month
            _day   := (Byte) d:Day
		return

		constructor(ticks as Int64)
			SELF(System.DateTime{ticks})
		return

		constructor(strDate as string)
			throw System.NotImplementedException{"Constructor __VODate(string __VODate) is not implemented yet."}

		constructor(year as Int, month as Int, day as Int)
			_value := 0
			_year  := 0
			_month := 0
			_day   := 0
			if year > 9999
				throw System.ArgumentOutOfRangeException{nameof(year)}
			endif
			if month > 12
				throw System.ArgumentOutOfRangeException{nameof(month)}
			endif
			if day > 31
				throw System.ArgumentOutOfRangeException{nameof(day)}
			endif
			try
                // this may throw an exception when the combination is not valid
                VAR d := System.DateTime{year, month, day}
				_year  := (Word) d:Year
				_month := (Byte) d:Month
				_day   := (Byte) d:Day
			catch 
				_value := 0 // null_date
			end try
		return

		constructor(year as DWord, month as DWord, day as DWord)
            self( (Int) year, (int) month, (int) day)
		return

		#endregion
		
		#region methods

		method CompareTo(o as Object) as Long
			var rhs := (__VODate)o 
		    return _value:CompareTo(rhs:_value)

		method CompareTo(rhs as __VODate) as Long
		    return _value:CompareTo(rhs:_value)


		override method GetHashCode() as Long
		    return _value:GetHashCode()

		method GetTypeCode() as System.TypeCode
		    return Value:GetTypeCode()
		#endregion
		#region Operators
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

		#endregion
		#region Equality Operators
		method Equals(o as __VODate) as Logic
			local rhs as __VODate
			rhs := (__VODate)o 
		    return _value == rhs:_value

		override method Equals(o as Object) as Logic
			if o:getType() == typeof(__VODate)
				return Equals( (__VoDate) o)
			elseif o:getType() == typeof(System.DateTime)
				return Equals( __VoDate{ (System.DateTime) o})
			endif
			return false

		static operator ==(d as __VODate, d2 as __VODate) as Logic
		    RETURN (d:Value == d2:Value)

		static operator !=(d as __VODate, d2 as __VODate) as Logic
		    return (d:_value != d2:_value)

		#endregion
		#region Implicit and Explicit converters

		static operator explicit(value as __VODate) as DWord
			return (DWORD) value:_value

		static operator explicit(v as __VODate) as INT
			return v:_value

		static operator explicit(v as DWORD ) as __VoDate
			var result := __VoDate{v}
            return result

		static operator explicit(v as INT) as __VoDate
			var result := __VoDate{v}
            return result

		static operator explicit(v as System.DateTime) as __VODate
			RETURN __VODate{v}

		method ToDateTime() as System.DateTime
		    return SELF:Value

		method FromDateTime(value as System.DateTime) as __VODate
		    return __VODate{value}

		#endregion
		#region Comparison Operators
		static operator >(d as __VODate, d2 as __VODate) as Logic
		    return (d:_value > d2:_value)

		static operator >=(d as __VODate, d2 as __VODate) as Logic
		    return (d:_value >= d2:_value)

		static operator implicit(v as __VODate) as System.DateTime
		    return v:Value

		static operator ++(d as __VODate) as __VODate
		    return d:Add(1)


		static operator <(d as __VODate, d2 as __VODate) as Logic
		    return (d:_value < d2:_value)

		static operator <=(d as __VODate, d2 as __VODate) as Logic
		    return (d:_value <= d2:_value)
		#endregion

		#region Add and Subtract Methods
		method Add(days as __Usual) as __VODate
			THROW NotImplementedException{}

		method Add(days as real8) as __VODate
            var res := self:Value:AddDays(days)
            return __VoDate{res}

		method Add(days as Long) as __VODate
            var res := self:Value:AddDays(days)
            return __VoDate{res}

		method Add(days as Int64) as __VODate
            var res := self:Value:AddDays(days)
            return __VoDate{res}

		method Add(span as System.TimeSpan) as __VODate
            var res := self:Value:Add(span)
            return __VoDate{res}

		method Add(days as DWord) as __VODate
            var res := self:Value:AddDays(days)
            return __VoDate{res}

		method Add(days as UInt64) as __VODate
            var res := self:Value:AddDays(days)
            return __VoDate{res}

		method Subtract(d as __VODate) as Long
			local span as System.TimeSpan
			span := (System.TimeSpan)(self:Value - d:Value) 
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
		#endregion

    #region IConvertable 
        // forward most methods to the DateTime class so there will
        // be a proper (localized) error message
		method IConvertible.ToBoolean(provider as System.IFormatProvider) as Logic
			return ((IConvertible)Value):ToBoolean(provider)

		method IConvertible.ToByte(provider as System.IFormatProvider) as Byte
			return ((IConvertible)Value):ToByte(provider)

		method IConvertible.ToChar(provider as System.IFormatProvider) as Char
			return ((IConvertible)Value):ToChar(provider)

		method IConvertible.ToDateTime(provider as System.IFormatProvider) as System.DateTime
			return Value

		method IConvertible.ToDecimal(provider as System.IFormatProvider) as Decimal
			return ((IConvertible)Value):ToDecimal(provider)

		method IConvertible.ToDouble(provider as System.IFormatProvider) as real8
			return ((IConvertible)Value):ToDouble(provider)

		method IConvertible.ToInt16(provider as System.IFormatProvider) as Short
			return ((IConvertible)Value):ToInt16(provider)

		method IConvertible.ToInt32(provider as System.IFormatProvider) as Long
			return ((IConvertible)Value):ToInt32(provider)

		method IConvertible.ToInt64(provider as System.IFormatProvider) as Int64
			return ((IConvertible)Value):ToInt64(provider)

		method IConvertible.ToSByte(provider as System.IFormatProvider) as SByte
			return ((IConvertible)Value):ToSByte(provider)

		method IConvertible.ToSingle(provider as System.IFormatProvider) as real4
			return ((IConvertible)Value):ToSingle(provider)

		method IConvertible.ToType(conversionType as System.Type, provider as System.IFormatProvider) as Object
			if conversionType == typeof(__VoDate)
                return SELF
            elseif conversionType == typeof(System.DateTime)
                return self:Value
            endif
            return ((IConvertible)Value):ToType(conversionType, provider)

		method IConvertible.ToUInt16(provider as System.IFormatProvider) as Word
			return ((IConvertible)Value):ToUInt16(provider)

		method IConvertible.ToUInt32(provider as System.IFormatProvider) as DWord
			return ((IConvertible)Value):ToUInt32(provider)

		method IConvertible.ToUInt64(provider as System.IFormatProvider) as UInt64
			return ((IConvertible)Value):ToUInt64(provider)
		#endregion

        #region ToString()
        // Use DateTime ToString) methods as helpers
		override method ToString() as string
            if (_value == 0)
                return "NULL_DATE"
            endif
		    return Value:ToString(_dateformat)

		method ToString(provider as System.IFormatProvider) as string
            if (_value == 0)
                return "NULL_DATE"
            endif
		    return Value:ToString(provider)

		method ToString(s as string) as string
            if (_value == 0)
                return "NULL_DATE"
            endif
		    return Value:ToString(s)

		method ToString(s as string, fp as System.IFormatProvider) as string
            if (_value == 0)
                return "NULL_DATE"
            endif
            if (s == null)
              s := _dateformat
            endif
		    return Value:ToString(s, fp)
        #endregion
		#region properties

		property Day as int Get _day 

		property IsEmpty as Logic
			Get
				return _value == 0
			End Get
		end property

		property Month as int GET _month 

		property Year as int GET _year 
	#endregion

	#region Static Properties
		static property DateFormat as string GET _dateFormat SET _dateFormat := value

		static property Epoch as Long
			Get
				return System.Threading.Thread.CurrentThread:CurrentCulture:Calendar:TwoDigitYearMax - 100
			End Get
			Set
				System.Threading.Thread.CurrentThread:CurrentCulture:Calendar:TwoDigitYearMax := (Long)value  + 100
			End Set
		end property

		static property NullDate as __VODate GET _NULL_DATE

		static method ElapTime(cStartTime as string, cEndTime as string) as string
			return System.DateTime.ParseExact(cEndTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture):Subtract(System.DateTime.ParseExact(cStartTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture)):ToString()

	#endregion
	end structure
end namespace
