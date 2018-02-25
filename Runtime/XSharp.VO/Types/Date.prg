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
		[DebuggerTypeProxy(typeof(DateDebugView))];
		[StructLayout(LayoutKind.Explicit)];
		public structure __VODate implements System.IComparable, ;
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
			static private _dateFormat as string
			static initonly public  _NULL_DATE as __VODate
			
		#endregion
		
		#region datetime conversions
			property Value as System.DateTime 
				get
					if (_value == 0)
						return System.DateTime.MinValue
					endif
					return System.DateTime{_year, _month, _day}
				end get
			end property
		#endregion
		
		#region constructors
			static constructor()
				_dateFormat := System.Globalization.DateTimeFormatInfo.CurrentInfo:ShortDatePattern
				_NULL_DATE  := __VODate{}
				return       
			
			constructor(lhs as System.DateTime)
				_year  := (word) lhs:Year
				_month := (byte) lhs:Month
				_day   := (byte) lhs:Day
				return
			
			constructor(lhs as iDate)
				_year  := (word) lhs:Year
				_month := (byte) lhs:Month
				_day   := (byte) lhs:Day
				return
			
			constructor(ticks as int64)
				self(System.DateTime{ticks})
				return
			
			private constructor(v as int, lValueAssign as logic)
				_value := v
				return
			
			
			constructor(strDate as string)
				throw System.NotImplementedException{"Constructor __VODate(string __VODate) is not implemented yet."}
			
			constructor(year as int, month as int, day as int)
				try
					// this may throw an exception when the combination is not valid
					var lhs := System.DateTime{year, month, day}
					_year  := (word) lhs:Year
					_month := (byte) lhs:Month
					_day   := (byte) lhs:Day
				catch e as Exception
					_value := 0 // null_date
					throw e
				end try
				return
			
			constructor(year as dword, month as dword, day as dword)
				// Chain to Int constructor
				self( (int) year, (int) month, (int) day)
				return
			
		#endregion
		
		#region methods
			
			method CompareTo(o as object) as long
				var rhs := (__VODate)o 
				return _value:CompareTo(rhs:_value)
			
			method CompareTo(rhs as __VODate) as long
				return _value:CompareTo(rhs:_value)
			
			override method GetHashCode() as long
			return _value:GetHashCode()
			
			method GetTypeCode() as System.TypeCode
				return TypeCode.DateTime
		#endregion
		#region Operators
			static operator ++(lhs as __VODate) as __VODate
				return lhs:Add(1)
			static operator +(lhs as __VODate, days as __Usual) as __VODate
				return lhs:Add(days)
			
			static operator +(lhs as __VODate, days as real8) as __VODate
				return lhs:Add(days)
			
			static operator +(lhs as __VODate, days as long) as __VODate
				return lhs:Add(days)
			
			static operator +(lhs as __VODate, days as int64) as __VODate
				return lhs:Add(days)
			
			static operator +(lhs as __VODate, ts as System.TimeSpan) as __VODate
				return lhs:Add(ts)
			
			static operator +(lhs as __VODate, days as dword) as __VODate
				return lhs:Add(days)
			
			static operator +(lhs as __VODate, days as uint64) as __VODate
				return lhs:Add(days)
			
			static operator --(lhs as __VODate) as __VODate
				return lhs:Subtract(1)
			
			static operator -(lhs as __VODate, rhs as __VODate) as long
				return lhs:Subtract(rhs)
			
			static operator -(lhs as __VODate, days as __Usual) as __VODate
				return lhs:Subtract(days)
			
			static operator -(lhs as __VODate, days as real8) as __VODate
				return lhs:Subtract(days)
			
			static operator -(lhs as __VODate, days as long) as __VODate
				return lhs:Subtract(days)
			
			static operator -(lhs as __VODate, days as int64) as __VODate
				return lhs:Subtract(days)
			
			static operator -(lhs as __VODate, ts as System.TimeSpan) as __VODate
				return lhs:Subtract(ts)
			
			static operator -(lhs as __VODate, days as dword) as __VODate
				return lhs:Subtract(days)
			
			static operator -(lhs as __VODate, days as uint64) as __VODate
				return lhs:Subtract(days)
			
		#endregion
		#region Equality Operators
			method Equals(lhs as __VODate) as logic
				return _value == lhs:_value
			
			override method Equals(o as object) as logic
				if o:getType() == typeof(__VODate)
					return Equals( (__VoDate) o)
				elseif o:getType() == typeof(System.DateTime)
					return Equals( __VoDate{ (System.DateTime) o})
				endif
			return false
			
			static operator ==(lhs as __VODate, rhs as __VODate) as logic
				return (lhs:Value == rhs:Value)
			
			static operator !=(lhs as __VODate, rhs as __VODate) as logic
				return (lhs:_value != rhs:_value)
			
		#endregion
		#region Implicit and Explicit converters
			
			static operator explicit(value as __VODate) as dword
				return (dword) value:_value
			
			static operator explicit(v as __VODate) as int
				return v:_value
			
			static operator explicit(v as dword ) as __VoDate
				var result := __VoDate{(int) v, true}
				return result
			
			static operator explicit(v as int) as __VoDate
				var result := __VoDate{v, true}
				
				return result
			
			static operator explicit(v as System.DateTime) as __VODate
				return __VODate{v}
			
			method ToDateTime() as System.DateTime
				return self:Value
			
			method FromDateTime(value as System.DateTime) as __VODate
				return __VODate{value}
			
		#endregion
		#region Comparison Operators
			static operator >(lhs as __VODate, rhs as __VODate) as logic
				// note: cannot use _value for > comparison because that would depend
				//       on the intel little endian layout 
				if lhs:_value == rhs:_value
					return false
				elseif lhs:_year > rhs:_year
					return true
				elseif lhs:_year == rhs:_year 
					if lhs:_month > rhs:_month
						return true
					elseif lhs:_month == rhs:_month .and. lhs:_day > rhs:_day
						return true
					endif
				endif
				return false
			
			static operator >=(lhs as __VODate, rhs as __VODate) as logic
				// note: cannot use _value for > comparison because that would depend
				//       on the intel little endian layout 
				if lhs:_value == rhs:_value
					return true
				elseif lhs:_year > rhs:_year
					return true
				elseif lhs:_year == rhs:_year 
					if lhs:_month > rhs:_month
						return true
					elseif lhs:_month == rhs:_month .and. lhs:_day >= rhs:_day
						return true
					endif
				endif
				return false
			
			static operator implicit(v as __VODate) as System.DateTime
				return v:Value
			
			static operator <(lhs as __VODate, rhs as __VODate) as logic
				// note: cannot use _value for < comparison because that would depend
				//       on the intel little endian layout 
				if lhs:_value == rhs:_value
					return false
				elseif lhs:_year < rhs:_year
					return true
				elseif lhs:_year == rhs:_year 
					if lhs:_month < rhs:_month
						return true
					elseif lhs:_month == rhs:_month .and. lhs:_day < rhs:_day
						return true
					endif
				endif
				return false
			
			static operator <=(lhs as __VODate, rhs as __VODate) as logic
				// note: cannot use _value for < comparison because that would depend
				//       on the intel little endian layout 
				if lhs:_value == rhs:_value
					return true
				elseif lhs:_year < rhs:_year
					return true
				elseif lhs:_year == rhs:_year 
					if lhs:_month < rhs:_month
						return true 
					elseif lhs:_month == rhs:_month .and. lhs:_day <= rhs:_day
						return true
					endif
				endif
				return false 
		#endregion
		
		#region Add and Subtract Methods
			method Add(days as __Usual) as __VODate
				return self:Add((long) days)
			
			method Add(days as real8) as __VODate
				var res := self:Value:AddDays(days)
				return __VoDate{res}
			
			method Add(days as long) as __VODate
				var res := self:Value:AddDays(days)
				return __VoDate{res}
			
			method Add(days as int64) as __VODate
				var res := self:Value:AddDays(days)
				return __VoDate{res}
			
			method Add(span as System.TimeSpan) as __VODate
				var res := self:Value:Add(span)
				return __VoDate{res}
			
			method Add(days as dword) as __VODate
				var res := self:Value:AddDays(days)
				return __VoDate{res}
			
			method Add(days as uint64) as __VODate
				var res := self:Value:AddDays(days)
				return __VoDate{res}
			
			method Subtract(lhs as __VODate) as long
				local span as System.TimeSpan
				span := (System.TimeSpan)(self:Value - lhs:Value) 
				return span:Days
			
			method Subtract(days as __Usual) as __VODate
				return self:Add(- (long) days)		
			
			method Subtract(days as real8) as __VODate
				return self:Add(-days)
			
			method Subtract(days as long) as __VODate
				return self:Add(-days)
			
			method Subtract(days as int64) as __VODate
				return self:Add(-days)
			
			method Subtract(ts as System.TimeSpan) as __VODate
				return self:Add(-ts)
			
			method Subtract(days as dword) as __VODate
				return self:Add(-days)
			
			method Subtract(days as uint64) as __VODate
				return self:Add(-(int64)days)
		#endregion
		
		#region IConvertable 
			// forward most methods to the DateTime class so there will
			// be a proper (localized) error message
			method IConvertible.ToBoolean(provider as System.IFormatProvider) as logic
				return ((IConvertible)Value):ToBoolean(provider)
			
			method IConvertible.ToByte(provider as System.IFormatProvider) as byte
				return ((IConvertible)Value):ToByte(provider)
			
			method IConvertible.ToChar(provider as System.IFormatProvider) as char
				return ((IConvertible)Value):ToChar(provider)
			
			method IConvertible.ToDateTime(provider as System.IFormatProvider) as System.DateTime
				return Value
			
			method IConvertible.ToDecimal(provider as System.IFormatProvider) as Decimal
				return ((IConvertible)Value):ToDecimal(provider)
			
			method IConvertible.ToDouble(provider as System.IFormatProvider) as real8
				return ((IConvertible)Value):ToDouble(provider)
			
			method IConvertible.ToInt16(provider as System.IFormatProvider) as short
				return ((IConvertible)Value):ToInt16(provider)
			
			method IConvertible.ToInt32(provider as System.IFormatProvider) as long
				return ((IConvertible)Value):ToInt32(provider)
			
			method IConvertible.ToInt64(provider as System.IFormatProvider) as int64
				return ((IConvertible)Value):ToInt64(provider)
			
			method IConvertible.ToSByte(provider as System.IFormatProvider) as SByte
				return ((IConvertible)Value):ToSByte(provider)
			
			method IConvertible.ToSingle(provider as System.IFormatProvider) as real4
				return ((IConvertible)Value):ToSingle(provider)
			
			method IConvertible.ToType(conversionType as System.Type, provider as System.IFormatProvider) as object
				if conversionType == typeof(__VoDate)
					return self
				elseif conversionType == typeof(System.DateTime)
					return self:Value
				endif
				return ((IConvertible)Value):ToType(conversionType, provider)
			
			method IConvertible.ToUInt16(provider as System.IFormatProvider) as word
				return ((IConvertible)Value):ToUInt16(provider)
			
			method IConvertible.ToUInt32(provider as System.IFormatProvider) as dword
				return ((IConvertible)Value):ToUInt32(provider)
			
			method IConvertible.ToUInt64(provider as System.IFormatProvider) as uint64
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
			
			property IsEmpty as logic
				get
					return _value == 0
				end get
			end property
			
			property Month	as int get _month 
			property Year	as int get _year 
			property Day	as int get _day 
			// Next properties for easy access in right type
			internal property DYear		as dword get _year
			internal property DMonth	as dword get _month
			internal property DDay		as dword get _day
			
			internal class DateDebugView
				private _value as __VoDate
				public constructor (d as __VoDate)
					_value := d
				
				public property Year	as int get _value:Year
				public property Month	as int get _value:Month
				public property Day		as int get _value:Day
				
			end class
			
		#endregion
		
		#region Static Properties
			static property DateFormat as string get _dateFormat set _dateFormat := value
			
			static property Epoch as long
				get
					return System.Threading.Thread.CurrentThread:CurrentCulture:Calendar:TwoDigitYearMax - 100
				end get
				set
					System.Threading.Thread.CurrentThread:CurrentCulture:Calendar:TwoDigitYearMax := (long)value  + 100
				end set
			end property
			
			static property NullDate as __VODate get _NULL_DATE
			
			static method ElapTime(cStartTime as string, cEndTime as string) as string
				return System.DateTime.ParseExact(cEndTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture):Subtract(System.DateTime.ParseExact(cStartTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture)):ToString()
			
		#endregion
	end structure
end namespace
