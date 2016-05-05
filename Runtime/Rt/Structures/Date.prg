//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using  System
using System.Runtime.InteropServices
using Vulcan
BEGIN NAMESPACE Vulcan
	//STRUCTURE __VODate
		//PRIVATE Value as INT
		//CONSTRUCTOR(i as INT)
			//Value := i
		//OPERATOR IMPLICIT(i as INT) AS ____VODate
			//RETURN __VODate{}
		//PROPERTY __Value as Int GET Value
	//END STRUCTURE

	
    public interface IDate
        property IsEmpty as Logic get
        property Value as System.DateTime get set
    end interface


    [StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, CharSet:=System.Runtime.InteropServices.CharSet.Auto)];
    structure __VODate implements System.IComparable, System.IFormattable, System.IConvertible, IDate
        #region static fields
        static private _dateFormat as string
        static private _timeSeparator as DWord
        static export _NULL_DATE as __VODate
		#endregion
		#region fields
        private _value as System.DateTime
		#endregion
		#region constrcutors
        static  constructor()
            __VODate._dateFormat := "MM/DD/YY"
            __VODate._timeSeparator := 0x3a
			_NULL_DATE := __VODate{}
		return       

        constructor(d as System.DateTime)
            _value := d:@@Date
		return

        constructor(ticks as Int64)
            try
                _value := System.DateTime{ticks}
            catch ex as Exception
                _value := System.DateTime.MinValue
            end try
		return

        constructor(@@date as string)
            throw System.NotImplementedException{"Constructor __VODate(string date) is not implemented yet."}

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
            if (days:IsLong)
                return self:Add((Long)days )
            endif
            if (! days:IsFloat)
                throw System.InvalidOperationException{"Only numeric value can be added to a date."}
            endif
        return self:Add((real8)days )

        method Add(days as real8) as __VODate
        return __VODate{(self:value + System.TimeSpan.FromDays(days))}

        method Add(days as Long) as __VODate
        return __VODate{(self:value + System.TimeSpan.FromDays((real8)days ))}

        method Add(days as Int64) as __VODate
        return __VODate{(self:value + System.TimeSpan.FromDays((real8)days ))}

        method Add(span as System.TimeSpan) as __VODate
        return __VODate{(self:value + span)}

        method Add(days as DWord) as __VODate
        return __VODate{(self:value + System.TimeSpan.FromDays((real8)days ))}

        method Add(days as UInt64) as __VODate
        return __VODate{(self:value + System.TimeSpan.FromDays((real8)days ))}

        method CompareTo(o as Object) as Long
            local @@date as __VODate
            @@date := (__VODate)o 
        return self:value:CompareTo(@@date:value)

        static method ConTime(dwHour as DWord, dwMinute as DWord, dwSeconds as DWord) as string
            local parts as Object[]
            parts := <Object>{dwHour, ":", dwMinute, ":", dwSeconds}
        return String.Concat(parts)

        static method ElapTime(cStartTime as string, cEndTime as string) as string
        return System.DateTime.ParseExact(cEndTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture):Subtract(System.DateTime.ParseExact(cStartTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture)):ToString()

        method Equals(o as __VODate) as Logic
        return (o:value == self:value)

        method Equals(o as Object) as Logic
            local @@date as __VODate
            if (o == null)
                return false
            endif
            if (o:GetType() == typeof(System.DateTime))
                return (self:value == (System.DateTime)o )
            endif
            if (! (o:GetType() == typeof(__VODate)))
                return false
            endif
            @@date := (__VODate)o 
        return (self:value == @@date:value)

        method FromDateTime(v as System.DateTime) as __VODate
        return __VODate{v}

        method GetHashCode() as Long
        return self:value:GetHashCode()

        method GetTypeCode() as System.TypeCode
        return self:value:GetTypeCode()

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
            local span as System.TimeSpan
            span := (System.TimeSpan)(v:value - System.DateTime{0x76d, 1, 1}) 
        return (DWord)(span:Days + 0x24db1a) 

        static operator explicit(v as System.DateTime) as __VODate
        return __VODate{v}

        static operator explicit(v as DWord) as __VODate
            local @@date as __VODate
            @@date := __VODate{System.DateTime.MinValue}
            if ((v >= 0x24db1a) .and. (v <= 0x464b78))
                v := v - 0x24db1a
                @@date := __VODate{(System.DateTime{0x76d, 1, 1} + System.TimeSpan{(Long)v , 0, 0, 0})}
            endif
        return @@date

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

        static method Secs(cTime as string) as DWord
            throw System.NotImplementedException{"__VODate.Secs is not implemented yet."}

        method Subtract(d as __VODate) as Long
            local span as System.TimeSpan
            span := (System.TimeSpan)(self:value - d:value) 
        return span:Days

        method Subtract(days as __Usual) as __VODate
            if (days:IsLong)
                return self:Subtract((Long)days )
            endif
            if (days:IsFloat)
                return self:Subtract((real8)days )
            endif
            if (! days:IsDate)
                throw System.InvalidOperationException{}
            endif
        return self:Subtract(days)

        method Subtract(days as real8) as __VODate
        return __VODate{(self:value - System.TimeSpan.FromDays(days))}

        method Subtract(days as Long) as __VODate
        return __VODate{(self:value - System.TimeSpan.FromDays((real8)days ))}

        method Subtract(days as Int64) as __VODate
        return __VODate{(self:value - System.TimeSpan.FromDays((real8)days ))}

        method Subtract(ts as System.TimeSpan) as __VODate
        return __VODate{(self:value - ts)}

        method Subtract(days as DWord) as __VODate
        return __VODate{(self:value - System.TimeSpan.FromDays((real8)days ))}

        method Subtract(days as UInt64) as __VODate
        return __VODate{(self:value - System.TimeSpan.FromDays((real8)days ))}

        method ToBoolean(provider as System.IFormatProvider) as Logic
        return ((System.IConvertible)self:value):ToBoolean(provider)

        method ToByte(provider as System.IFormatProvider) as Byte
        return ((System.IConvertible)self:value):ToByte(provider)

        method ToChar(provider as System.IFormatProvider) as Char
        return ((System.IConvertible)self:value):ToChar(provider)

        method ToDateTime() as System.DateTime
        return self:value

        method ToDateTime(provider as System.IFormatProvider) as System.DateTime
        return ((System.IConvertible)self:value):ToDateTime(provider)

        method ToDecimal(provider as System.IFormatProvider) as Decimal
        return ((System.IConvertible)self:value):ToDecimal(provider)

        method ToDouble(provider as System.IFormatProvider) as real8
        return ((System.IConvertible)self:value):ToDouble(provider)

        method ToInt16(provider as System.IFormatProvider) as Short
        return ((System.IConvertible)self:value):ToInt16(provider)

        method ToInt32(provider as System.IFormatProvider) as Long
        return ((System.IConvertible)self:value):ToInt32(provider)

        method ToInt64(provider as System.IFormatProvider) as Int64
        return ((System.IConvertible)self:value):ToInt64(provider)

        method ToSByte(provider as System.IFormatProvider) as SByte
        return ((System.IConvertible)self:value):ToSByte(provider)

        method ToSingle(provider as System.IFormatProvider) as real4
        return ((System.IConvertible)self:value):ToSingle(provider)

        method ToString() as string
        return self:value:ToString("d")

        method ToString(provider as System.IFormatProvider) as string
        return self:value:ToString(provider)

        method ToString(s as string) as string
        return self:value:ToString(s)

        method ToString(s as string, fp as System.IFormatProvider) as string
        return self:value:ToString(s, fp)

        method ToType(conversionType as System.Type, provider as System.IFormatProvider) as Object
        return ((System.IConvertible)self:value):ToType(conversionType, provider)

        method ToUInt16(provider as System.IFormatProvider) as Word
        return ((System.IConvertible)self:value):ToUInt16(provider)

        method ToUInt32(provider as System.IFormatProvider) as DWord
        return ((System.IConvertible)self:value):ToUInt32(provider)

        method ToUInt64(provider as System.IFormatProvider) as UInt64
        return ((System.IConvertible)self:value):ToUInt64(provider)
		#endregion
		#region properties
        static property DateFormat as string
            Get
                return __VODate._dateFormat
            End Get
            Set
                __VODate._dateFormat := value
            End Set
        end property

        property Day as DWord
            Get
                return IIF(! (self:value == System.DateTime.MinValue),(DWord)self:value:Day ,0)
            End Get
        end property

        property DayOfWeek as Long
            Get
                return (IIF(! (self:value == System.DateTime.MinValue),(Long)self:value:DayOfWeek ,0) + 1)
            End Get
        end property

        static property Epoch as DWord
            Get
                return (DWord)(System.Threading.Thread.CurrentThread:CurrentCulture:Calendar:TwoDigitYearMax - 100) 
            End Get
            Set
                System.Threading.Thread.CurrentThread:CurrentCulture:Calendar:TwoDigitYearMax := ((Long)value  + 100)
            End Set
        end property

        property IsEmpty as Logic
            Get
                return (self:value == System.DateTime.MinValue)
            End Get
        end property

        property Month as Long
            Get
                return IIF(! (self:value == System.DateTime.MinValue),self:value:Month,0)
            End Get
        end property

        static property NullDate as __VODate
            Get
                return (__VODate)System.DateTime.MinValue 
            End Get
        end property

        static property TimeSeparator as DWord
            Get
                return __VODate._timeSeparator
            End Get
            Set
                __VODate._timeSeparator := value
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
                return IIF(! (self:value == System.DateTime.MinValue),self:value:Year,0)
            End Get
        end property


    end structure


END NAMESPACE