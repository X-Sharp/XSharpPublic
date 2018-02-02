//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Runtime.Serialization
USING System.Globalization
USING System.Runtime.InteropServices
USING XSharp
USING System.Diagnostics
BEGIN NAMESPACE XSharp	
    [DebuggerDisplay("{ToString(),nq}", Type := "DATE" )];
	[DebuggerTypeProxy(typeof(DateDebugView))];
    [StructLayout(LayoutKind.Explicit)];
	PUBLIC STRUCTURE __VODate IMPLEMENTS System.IComparable, ;
		System.IFormattable, ;
		System.IConvertible, ;
		IDate, ;
		IComparable<__VoDate>, ;
		IEquatable<__VoDate>
        // This structure uses an explicit layout to map
        // year, month and date into 32 bits
        // the _value field is for convenience, so we can do a single numeric comparison
        // to determine equality or relative comparisons in stead of doing 3 comparisons
        // for date calculation we use the Value PROPERTY which returns a System.DateTime type
		// Note that the Vulcan type uses a datetime which takes 8 bytes. We only use 4 bytes
		#region fields
        [FieldOffSet(00)] private initonly _value as System.Int32
        [FieldOffSet(00)] private initonly _year  as System.UInt16
        [FieldOffSet(02)] private initonly _month as System.Byte
        [FieldOffSet(03)] private initonly _day   as System.Byte
		#endregion

		#region static fields
		// TODO move to settings object later
		static private _dateFormat AS STRING
		static initonly public  _NULL_DATE as __VODate

		#endregion

        #region datetime conversions
        PROPERTY Value as System.DateTime 
        GET
            IF (_value == 0)
                RETURN System.DateTime.MinValue
            ENDIF
            RETURN System.DateTime{_year, _month, _day}
        END GET
        END PROPERTY
        #endregion

		#region constructors
		static constructor()
			_dateFormat := System.Globalization.DateTimeFormatInfo.CurrentInfo:ShortDatePattern
			_NULL_DATE  := __VODate{}
		RETURN       

		constructor(lhs as System.DateTime)
			_year  := (Word) lhs:Year
            _month := (Byte) lhs:Month
            _day   := (Byte) lhs:Day
		    return

		constructor(lhs as iDate)
			_year  := (Word) lhs:Year
            _month := (Byte) lhs:Month
            _day   := (Byte) lhs:Day
		return

		constructor(ticks AS INT64)
			SELF(System.DateTime{ticks})
		return

		private constructor(v as INT, lValueAssign as LOGIC)
			_value := v
		return


		constructor(strDate AS STRING)
			throw System.NotImplementedException{"Constructor __VODate(string __VODate) is not implemented yet."}

		constructor(year AS INT, month AS INT, day AS INT)
			try
                // this may throw an exception when the combination is not valid
                VAR lhs := System.DateTime{year, month, day}
				_year  := (Word) lhs:Year
				_month := (Byte) lhs:Month
				_day   := (Byte) lhs:Day
			catch e as Exception
				_value := 0 // null_date
				throw e
			end try
		return

		CONSTRUCTOR(year AS DWORD, month AS DWORD, day AS DWORD)
			// Chain to Int constructor
            self( (Int) year, (int) month, (int) day)
		return

		#endregion
		
		#region methods

		METHOD CompareTo(o as Object) as Long
			var rhs := (__VODate)o 
		    RETURN _value:CompareTo(rhs:_value)

		METHOD CompareTo(rhs as __VODate) as Long
		    RETURN _value:CompareTo(rhs:_value)

		OVERRIDE METHOD GetHashCode() as Long
		    RETURN _value:GetHashCode()

		METHOD GetTypeCode() as System.TypeCode
		    RETURN Value:GetTypeCode()
		#endregion
		#region Operators
		static operator +(lhs as __VODate, days as __Usual) as __VODate
		    RETURN lhs:Add(days)

		static operator +(lhs as __VODate, days as real8) as __VODate
		    RETURN lhs:Add(days)

		static operator +(lhs as __VODate, days as Long) as __VODate
		    RETURN lhs:Add(days)

		static operator +(lhs as __VODate, days AS INT64) as __VODate
		    RETURN lhs:Add(days)

		static operator +(lhs as __VODate, ts as System.TimeSpan) as __VODate
		    RETURN lhs:Add(ts)

		static operator +(lhs as __VODate, days as DWord) as __VODate
		    RETURN lhs:Add(days)

		static operator +(lhs as __VODate, days as UInt64) as __VODate
		    RETURN lhs:Add(days)

		static operator --(lhs as __VODate) as __VODate
		    RETURN lhs:Subtract(1)

		static operator -(lhs as __VODate, rhs as __VODate) as Long
		    RETURN lhs:Subtract(rhs)

		static operator -(lhs as __VODate, days as __Usual) as __VODate
		    RETURN lhs:Subtract(days)

		static operator -(lhs as __VODate, days as real8) as __VODate
		    RETURN lhs:Subtract(days)

		static operator -(lhs as __VODate, days as Long) as __VODate
		    RETURN lhs:Subtract(days)

		static operator -(lhs as __VODate, days AS INT64) as __VODate
		    RETURN lhs:Subtract(days)

		static operator -(lhs as __VODate, ts as System.TimeSpan) as __VODate
		    RETURN lhs:Subtract(ts)

		static operator -(lhs as __VODate, days as DWord) as __VODate
		    RETURN lhs:Subtract(days)

		static operator -(lhs as __VODate, days as UInt64) as __VODate
		    RETURN lhs:Subtract(days)

		#endregion
		#region Equality Operators
		METHOD Equals(lhs as __VODate) AS LOGIC
		    RETURN _value == lhs:_value

		override METHOD Equals(o as Object) AS LOGIC
			if o:getType() == typeof(__VODate)
				RETURN Equals( (__VoDate) o)
			elseif o:getType() == typeof(System.DateTime)
				RETURN Equals( __VoDate{ (System.DateTime) o})
			endif
			RETURN false

		static operator ==(lhs as __VODate, rhs as __VODate) AS LOGIC
		    RETURN (lhs:Value == rhs:Value)

		static operator !=(lhs as __VODate, rhs as __VODate) AS LOGIC
		    RETURN (lhs:_value != rhs:_value)

		#endregion
		#region Implicit and Explicit converters

		static operator explicit(value as __VODate) as DWord
			RETURN (DWORD) value:_value

		static operator explicit(v as __VODate) AS INT
			RETURN v:_value

		static operator explicit(v as DWORD ) as __VoDate
			var result := __VoDate{(INT) v, true}
            RETURN result

		static operator explicit(v AS INT) as __VoDate
			var result := __VoDate{v, true}

            RETURN result

		static operator explicit(v as System.DateTime) as __VODate
			RETURN __VODate{v}

		METHOD ToDateTime() as System.DateTime
		    RETURN SELF:Value

		METHOD FromDateTime(value as System.DateTime) as __VODate
		    RETURN __VODate{value}

		#endregion
		#region Comparison Operators
		static operator >(lhs as __VODate, rhs as __VODate) AS LOGIC
			// note: cannot use _value for > comparison because that would depend
			//       on the intel little endian layout 
			if lhs:_value == rhs:_value
				return false
			elseif lhs:_year > rhs:_year
				return true
			ELSEIF lhs:_year == rhs:_year 
				if lhs:_month > rhs:_month
					return true
				elseif lhs:_month == rhs:_month .and. lhs:_day > rhs:_day
					RETURN TRUE
				endif
			endif
		    RETURN false

		static operator >=(lhs as __VODate, rhs as __VODate) AS LOGIC
			// note: cannot use _value for > comparison because that would depend
			//       on the intel little endian layout 
			if lhs:_value == rhs:_value
				return true
			elseif lhs:_year > rhs:_year
				return true
			ELSEIF lhs:_year == rhs:_year 
				IF lhs:_month > rhs:_month
					return true
				ELSEIF lhs:_month == rhs:_month .and. lhs:_day >= rhs:_day
					RETURN TRUE
				endif
			endif
		    RETURN false

		static operator implicit(v as __VODate) as System.DateTime
		    RETURN v:Value

		static operator ++(lhs as __VODate) as __VODate
		    RETURN lhs:Add(1)

		static operator <(lhs as __VODate, rhs as __VODate) AS LOGIC
			// note: cannot use _value for < comparison because that would depend
			//       on the intel little endian layout 
			if lhs:_value == rhs:_value
				return false
			elseif lhs:_year < rhs:_year
				return true
			ELSEIF lhs:_year == rhs:_year 
				IF lhs:_month < rhs:_month
					return true
				elseif lhs:_month == rhs:_month .and. lhs:_day < rhs:_day
					RETURN TRUE
				endif
			endif
		    RETURN false

		static operator <=(lhs as __VODate, rhs as __VODate) AS LOGIC
			// note: cannot use _value for < comparison because that would depend
			//       on the intel little endian layout 
			if lhs:_value == rhs:_value
				return true
			elseif lhs:_year < rhs:_year
				return true
			ELSEIF lhs:_year == rhs:_year 
				IF lhs:_month < rhs:_month
					return true 
				elseif lhs:_month == rhs:_month .and. lhs:_day <= rhs:_day
					RETURN TRUE
				endif
			endif
		    RETURN false 
		#endregion

		#region Add and Subtract Methods
		METHOD Add(days as __Usual) as __VODate
			RETURN SELF:Add((LONG) days)

		METHOD Add(days as real8) as __VODate
            var res := self:Value:AddDays(days)
            RETURN __VoDate{res}

		METHOD Add(days as Long) as __VODate
            var res := self:Value:AddDays(days)
            RETURN __VoDate{res}

		METHOD Add(days AS INT64) as __VODate
            var res := self:Value:AddDays(days)
            RETURN __VoDate{res}

		METHOD Add(span as System.TimeSpan) as __VODate
            var res := self:Value:Add(span)
            RETURN __VoDate{res}

		METHOD Add(days as DWord) as __VODate
            var res := self:Value:AddDays(days)
            RETURN __VoDate{res}

		METHOD Add(days as UInt64) as __VODate
            var res := self:Value:AddDays(days)
            RETURN __VoDate{res}

		METHOD Subtract(lhs as __VODate) as Long
			local span as System.TimeSpan
			span := (System.TimeSpan)(self:Value - lhs:Value) 
		    RETURN span:Days

		METHOD Subtract(days as __Usual) as __VODate
    		RETURN SELF:Add(- (LONG) days)		

		METHOD Subtract(days as real8) as __VODate
		    RETURN SELF:Add(-days)

		METHOD Subtract(days as Long) as __VODate
		    RETURN SELF:Add(-days)

		METHOD Subtract(days AS INT64) as __VODate
		    RETURN SELF:Add(-days)

		METHOD Subtract(ts as System.TimeSpan) as __VODate
		    RETURN SELF:Add(-ts)

		METHOD Subtract(days as DWord) as __VODate
		    RETURN SELF:Add(-days)

		METHOD Subtract(days as UInt64) as __VODate
		    RETURN SELF:Add(-(int64)days)
		#endregion

    #region IConvertable 
        // forward most methods to the DateTime class so there will
        // be a proper (localized) error message
		METHOD IConvertible.ToBoolean(provider as System.IFormatProvider) AS LOGIC
			RETURN ((IConvertible)Value):ToBoolean(provider)

		METHOD IConvertible.ToByte(provider as System.IFormatProvider) as Byte
			RETURN ((IConvertible)Value):ToByte(provider)

		METHOD IConvertible.ToChar(provider as System.IFormatProvider) as Char
			RETURN ((IConvertible)Value):ToChar(provider)

		METHOD IConvertible.ToDateTime(provider as System.IFormatProvider) as System.DateTime
			RETURN Value

		METHOD IConvertible.ToDecimal(provider as System.IFormatProvider) as Decimal
			RETURN ((IConvertible)Value):ToDecimal(provider)

		METHOD IConvertible.ToDouble(provider as System.IFormatProvider) as real8
			RETURN ((IConvertible)Value):ToDouble(provider)

		METHOD IConvertible.ToInt16(provider as System.IFormatProvider) as Short
			RETURN ((IConvertible)Value):ToInt16(provider)

		METHOD IConvertible.ToInt32(provider as System.IFormatProvider) as Long
			RETURN ((IConvertible)Value):ToInt32(provider)

		METHOD IConvertible.ToInt64(provider as System.IFormatProvider) AS INT64
			RETURN ((IConvertible)Value):ToInt64(provider)

		METHOD IConvertible.ToSByte(provider as System.IFormatProvider) as SByte
			RETURN ((IConvertible)Value):ToSByte(provider)

		METHOD IConvertible.ToSingle(provider as System.IFormatProvider) as real4
			RETURN ((IConvertible)Value):ToSingle(provider)

		METHOD IConvertible.ToType(conversionType as System.Type, provider as System.IFormatProvider) as Object
			if conversionType == typeof(__VoDate)
                RETURN SELF
            elseif conversionType == typeof(System.DateTime)
                RETURN self:Value
            endif
            RETURN ((IConvertible)Value):ToType(conversionType, provider)

		METHOD IConvertible.ToUInt16(provider as System.IFormatProvider) as Word
			RETURN ((IConvertible)Value):ToUInt16(provider)

		METHOD IConvertible.ToUInt32(provider as System.IFormatProvider) AS DWORD
			RETURN ((IConvertible)Value):ToUInt32(provider)

		METHOD IConvertible.ToUInt64(provider as System.IFormatProvider) AS UINT64
			RETURN ((IConvertible)Value):ToUInt64(provider)
		#endregion

        #region ToString()
        // Use DateTime ToString) methods as helpers
		override METHOD ToString() AS STRING
            if (_value == 0)
                RETURN "NULL_DATE"
            endif
		    RETURN Value:ToString(_dateformat)

		METHOD ToString(provider as System.IFormatProvider) AS STRING
            if (_value == 0)
                RETURN "NULL_DATE"
            endif
		    RETURN Value:ToString(provider)

		METHOD ToString(s AS STRING) AS STRING
            if (_value == 0)
                RETURN "NULL_DATE"
            endif
		    RETURN Value:ToString(s)

		METHOD ToString(s AS STRING, fp as System.IFormatProvider) AS STRING
            if (_value == 0)
                RETURN "NULL_DATE"
            endif
            if (s == null)
              s := _dateformat
            endif
		    RETURN Value:ToString(s, fp)
        #endregion
		#region properties

		PROPERTY IsEmpty AS LOGIC
			Get
				RETURN _value == 0
			End Get
		end property

		PROPERTY Month	AS INT GET _month 
		PROPERTY Year	AS INT GET _year 
		PROPERTY Day	AS INT GET _day 
		// Next properties for easy access in right type
		INTERNAL PROPERTY DYear		as DWORD Get _year
		INTERNAL PROPERTY DMonth	as DWORD Get _month
		INTERNAL PROPERTY DDay		as DWORD Get _day

	internal class DateDebugView
		private _value as __VoDate
		public constructor (d as __VoDate)
			_value := d
		
		PUBLIC PROPERTY Year	as INT Get _value:Year
		PUBLIC PROPERTY Month	as INT Get _value:Month
		PUBLIC PROPERTY Day		as INT Get _value:Day

	end class

	#endregion

	#region Static Properties
		static PROPERTY DateFormat AS STRING GET _dateFormat SET _dateFormat := value

		static PROPERTY Epoch as Long
			Get
				RETURN System.Threading.Thread.CurrentThread:CurrentCulture:Calendar:TwoDigitYearMax - 100
			End Get
			Set
				System.Threading.Thread.CurrentThread:CurrentCulture:Calendar:TwoDigitYearMax := (Long)value  + 100
			End Set
		end property

		static PROPERTY NullDate as __VODate GET _NULL_DATE

		static METHOD ElapTime(cStartTime AS STRING, cEndTime AS STRING) AS STRING
			RETURN System.DateTime.ParseExact(cEndTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture):Subtract(System.DateTime.ParseExact(cStartTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture)):ToString()

	#endregion
	end structure
end namespace
