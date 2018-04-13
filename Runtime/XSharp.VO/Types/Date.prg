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
	[StructLayout(LayoutKind.Explicit,Pack := 1)];
	public structure __VODate implements System.IComparable, ;
		System.IFormattable, ;
		System.IConvertible, ;
		IDate, ;
		IComparable<__VODate>, ;
		IEquatable<__VODate>
		// This structure uses an explicit layout to map
		// year, month and date into 32 bits
		// the _value field is for convenience, so we can do a single numeric comparison
		// to determine equality or relative comparisons in stead of doing 3 comparisons
		// for date calculation we use the Value PROPERTY which returns a System.DateTime type
		// Note that the Vulcan type uses a datetime which takes 8 bytes. We only use 4 bytes
		#region fields
			[FieldOffSet(00)] private _value as System.Int32
			[FieldOffSet(00)] private _year  as System.UInt16
			[FieldOffSet(02)] private _month as System.Byte
			[FieldOffSet(03)] private _day   as System.Byte
		#endregion
		
		#region static fields
			// TODO move to settings object later
			static private _dateFormat as string
			static initonly public  _NULL_DATE as date
			const  VO_MIN_DATE := 2415386U as Dword	// 1901-01-01
			const  VO_MAX_DATE := 4606840U as Dword	// 7900-12-31
			static initonly _dtCalc as Datetime
			
		#endregion
		
		#region datetime conversions
			property Value as System.DateTime  get self:_dtValue set _dtValue := value

			private property _dtValue as System.DateTime 
				get
					if (_value == 0)
						return System.DateTime.MinValue
					endif
					return System.DateTime{_year, _month, _day}
				end get
				set 
				if value != DateTime.MinValue
					_year  := (word) value:Year
					_month := (byte) value:Month
					_day   := (byte) value:Day
				else
					_value := 0
				endif					
				end set
			end property
		#endregion
		
		#region constructors
			static constructor()
				_dateFormat := System.Globalization.DateTimeFormatInfo.CurrentInfo:ShortDatePattern
				_NULL_DATE  := date{}
				_dtCalc     := DateTime{1901,1,1}
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
				throw System.NotImplementedException{"Constructor Date(string Date) is not implemented yet."}
			
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
				var rhs := (Date)o 
				return _value:CompareTo(rhs:_value)
			
			method CompareTo(rhs as Date) as long
				return _value:CompareTo(rhs:_value)
			
			override method GetHashCode() as long
			return _value:GetHashCode()
			
			method GetTypeCode() as System.TypeCode
				return TypeCode.DateTime
		#endregion
		#region Operators
			static operator ++(lhs as Date) as Date
				return lhs:Add(1)

			static operator +(lhs as Date, days as usual) as Date
				return lhs:Add(days)
			
			static operator +(lhs as Date, days as real8) as Date
				return lhs:Add(days)
			
			static operator +(lhs as Date, days as long) as Date
				return lhs:Add(days)
			
			static operator +(lhs as Date, days as int64) as Date
				return lhs:Add(days)
			
			static operator +(lhs as Date, ts as System.TimeSpan) as Date
				return lhs:Add(ts)
			
			static operator +(lhs as Date, days as dword) as Date
				return lhs:Add(days)
			
			static operator +(lhs as Date, days as uint64) as Date
				return lhs:Add(days)
			
			static operator --(lhs as Date) as Date
				return lhs:Subtract(1)
			
			static operator -(lhs as Date, rhs as Date) as long
				return lhs:Subtract(rhs)
			
			static operator -(lhs as Date, days as usual) as Date
				return lhs:Subtract(days)
			
			static operator -(lhs as Date, days as real8) as Date
				return lhs:Subtract(days)
			
			static operator -(lhs as Date, days as long) as Date
				return lhs:Subtract(days)
			
			static operator -(lhs as Date, days as int64) as Date
				return lhs:Subtract(days)
			
			static operator -(lhs as Date, ts as System.TimeSpan) as Date
				return lhs:Subtract(ts)
			
			static operator -(lhs as Date, days as dword) as Date
				return lhs:Subtract(days)
			
			static operator -(lhs as Date, days as uint64) as Date
				return lhs:Subtract(days)
			
		#endregion
		#region Equality Operators
			method Equals(lhs as Date) as logic
				return _value == lhs:_value
			
			override method equals(o as object) as logic
				if o != null
					if o:getType() == typeof(Date)
						return self:Equals(  (Date) o)
					elseif o:getType() == typeof(System.DateTime)
						return self:Equals( Date{ (System.DateTime) o})
					endif
				endif
			return false
			
			static operator ==(lhs as Date, rhs as Date) as logic
				return (lhs:_value == rhs:_value)
			
			static operator !=(lhs as Date, rhs as Date) as logic
				return (lhs:_value != rhs:_value)
			
		#endregion
		#region Implicit and Explicit converters
			
			static operator explicit(dvalue as Date) as dword
				// convert to julian date like vo
				// # of days since 1-1-1901 + 2415386 
				local nDays as dword
				nDays := (DWORD) (dValue:_dtValue - _dtCalc):Days + VO_MIN_DATE
				return nDays

			
			static operator explicit(dValue as Date) as int
				local nDays as long
				nDays := (dValue:_dtValue - _dtCalc):Days + VO_MIN_DATE
				return nDays
			
			static operator explicit(dw as dword ) as Date
				// convert julian date to our date
				local result := _NULL_DATE as Date
				if dw >= VO_MIN_DATE .and. dw <= VO_MAX_DATE
					local dt as DateTime
					dt := _dtCalc:AddDays ( (int) dw - VO_MIN_DATE)
					result := Date{dt}
				endif
				return result
			
			static operator explicit(i as int) as Date
				local result := _NULL_DATE as Date
				if i >= VO_MIN_DATE .and. i <= VO_MAX_DATE
					local dt as DateTime
					dt := _dtCalc:AddDays ( i - VO_MIN_DATE)
					result := Date{dt}
				endif
				return result

			static operator implicit(v as System.DateTime) as Date
				return Date{v}
			
			method ToDateTime() as System.DateTime
				return self:_dtValue
			
			method FromDateTime(dtvalue as System.DateTime) as Date
				return Date{dtvalue}
			
		#endregion
		#region Comparison Operators
			static operator >(lhs as Date, rhs as Date) as logic
				// note: cannot use _value for > comparison because that would depend
				//       on the processor layout 
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
			
			static operator >=(lhs as Date, rhs as Date) as logic
				// note: cannot use _value for > comparison because that would depend
				//       on the processor layout 
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
			
			static operator implicit(v as Date) as System.DateTime
				return v:_dtValue
			
			static operator <(lhs as Date, rhs as Date) as logic
				// note: cannot use _value for < comparison because that would depend
				//       on the processor layout 
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
			
			static operator <=(lhs as Date, rhs as Date) as logic
				// note: cannot use _value for < comparison because that would depend
				//       on the processor layout 
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
			method Add(days as usual) as Date
				if days:IsLong
					return self:Add( (long) days)
				elseif days:IsFloat
					 return self:Add( (real8) days)
				else
					throw Error.ArgumentError(__ENTITY__,nameof(days), "Incompatible argument for Date:Add()")
				endif

			method Add(days as real8) as Date
				var res := self:_dtValue:AddDays(days)
				return Date{res}
			
			method Add(days as long) as Date
				var res := self:_dtValue:AddDays(days)
				return Date{res}
			
			method Add(days as int64) as Date
				var res := self:_dtValue:AddDays(days)
				return Date{res}
			
			method Add(span as System.TimeSpan) as Date
				var res := self:_dtValue:Add(span)
				return Date{res}
			
			method Add(days as dword) as Date
				var res := self:_dtValue:AddDays(days)
				return Date{res}
			
			method Add(days as uint64) as Date
				var res := self:_dtValue:AddDays(days)
				return Date{res}
			
			method Subtract(lhs as Date) as long
				local span as System.TimeSpan
				span := (System.TimeSpan)(self:_dtValue - lhs:_dtValue) 
				return span:Days
			
			method Subtract(days as usual) as Date
				if days:IsLong
					return self:Add( -(long) days)
				elseif days:IsFloat
					 return self:Add( -(real8) days)
				else
					throw Error.ArgumentError(__ENTITY__,nameof(days), "Incompatible argument for Date:Subtract()")
				endif
			
			method Subtract(days as real8) as Date
				return self:Add(-days)
			
			method Subtract(days as long) as Date
				return self:Add(-days)
			
			method Subtract(days as int64) as Date
				return self:Add(-days)
			
			method Subtract(ts as System.TimeSpan) as Date
				return self:Add(-ts)
			
			method Subtract(days as dword) as Date
				return self:Add(-days)
			
			method Subtract(days as uint64) as Date
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
				if conversionType == typeof(Date)
					return self
				elseif conversionType == typeof(System.DateTime)
					return self:_dtValue
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
			return DTOC(SELF)
			
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
				private _value as Date
				public constructor (d as Date)
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
			
			static method NullDate as date 
				return  _NULL_DATE
			
			static method ElapTime(cStartTime as string, cEndTime as string) as string
				return System.DateTime.ParseExact(cEndTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture):Subtract(System.DateTime.ParseExact(cStartTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture)):ToString()
			
		#endregion
	end structure
end namespace
