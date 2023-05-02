//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Runtime.Serialization
USING System.Globalization
USING System.Runtime.InteropServices
USING System.Runtime.Serialization
USING System.Runtime.CompilerServices
USING XSharp
USING System.Diagnostics

#include "attributes.xh"
#pragma warnings(171, off)   // field must be fully assigned before control is returned to the caller

BEGIN NAMESPACE XSharp
/// <summary>Internal type that implements the VO Compatible DATE type<br/>
/// This type has many operators and implicit converters that normally are never directly called from user code.
/// It holds year, month, day in 32 bits. For date calculations it uses the System.DateTime calculation logic.
/// There are implicit converters between Date and DateTime.
/// </summary>
/// <seealso cref="IDate"/>
/// <seealso cref="T:XSharp.RDD.DbDate"/>
//[DebuggerTypeProxy(TYPEOF(DateDebugView))];
[DebuggerDisplay("{ToDebugString(),nq}", Type := "DATE" )];
[StructLayout(LayoutKind.Explicit, Size := 4)];
[Serializable];
PUBLIC STRUCTURE __Date IMPLEMENTS System.IComparable, ;
        System.IFormattable, ;
        System.IConvertible, ;
        IDate, ;
        IComparable<__Date>, ;
        IEquatable<__Date>,;
        ISerializable
    // This structure uses an explicit layout to map
    // year, month and date into 32 bits
    // the _ymd field is for convenience, so we can do a single numeric comparison
    // to determine equality or relative comparisons in stead of doing 3 comparisons
    // for date calculation we use the Value PROPERTY which returns a System.DateTime type
    // Note that the Vulcan type uses a datetime which takes 8 bytes. We only use 4 bytes
#region fields
    [NOSHOW] [FieldOffset(00)] PRIVATE _ymd   AS System.Int32
    [NOSHOW] [FieldOffset(00)] PRIVATE _year  AS System.UInt16
    [NOSHOW] [FieldOffset(02)] PRIVATE _month AS System.Byte
    [NOSHOW] [FieldOffset(03)] PRIVATE _day   AS System.Byte
#endregion

#region STATIC fields
    /// <exclude />
    [NOSHOW] STATIC INITONLY PUBLIC  _NULL_DATE AS DATE
    /// <exclude />
    [NOSHOW] CONST  CLIPPER_MIN_DATE := 2415386U AS DWORD	// 1901-01-01
    /// <exclude />
    [NOSHOW] CONST  CLIPPER_MAX_DATE := 4606840U AS DWORD	// 7900-12-31
    /// <exclude />
    [NOSHOW] STATIC INITONLY _dtCalc AS DateTime

#endregion

#region datetime conversions
    /// <summary>Return DATE value as DateTime.</summary>
    [NOSHOW] PUBLIC PROPERTY Value AS System.DateTime
    GET
        IF (_ymd == 0)
            RETURN System.DateTime.MinValue
        ENDIF
        RETURN System.DateTime{_year, _month, _day}
    END GET
    SET
        IF value != DateTime.MinValue
            _year  := (WORD) value:Year
            _month := (BYTE) value:Month
            _day   := (BYTE) value:Day
        ELSE
            _ymd := 0
        ENDIF
    END SET
    END PROPERTY
#endregion

#region constructors
    STATIC CONSTRUCTOR()
        _NULL_DATE  := DATE{}
        _dtCalc     := DateTime{1901,1,1}
        RETURN
    /// <summary>Construct a date from a DateTime value.</summary>
    [NODEBUG] [INLINE];
    CONSTRUCTOR(dt AS System.DateTime)
        IF dt != DateTime.MinValue
            _year  := (WORD) dt:Year
            _month := (BYTE) dt:Month
            _day   := (BYTE) dt:Day
        ELSE
            _ymd := 0
        ENDIF
        RETURN

    /// <summary>Construct a date from another IDate type.</summary>
    [NODEBUG] [INLINE];
    CONSTRUCTOR(d AS IDate)
        _year  := (WORD) d:Year
        _month := (BYTE) d:Month
        _day   := (BYTE) d:Day
        RETURN

    /// <summary>Construct a date from a number of Ticks.</summary>
    [NODEBUG] [INLINE];
    CONSTRUCTOR(ticks AS INT64)
        SELF(System.DateTime{ticks})
        RETURN

    /// <summary>Construct a date from a string. This assumes the string is in current Date format</summary>
    [NODEBUG] [INLINE];
    CONSTRUCTOR(strDate AS STRING)
        LOCAL dValue := CToD(strDate) AS DATE
        _ymd := dValue:_ymd

    /// <summary>Construct a date from a string using the specified Date Format.</summary>
    [NODEBUG] [INLINE];
    CONSTRUCTOR(strDate AS STRING, strFormat AS STRING)
        LOCAL dValue := CToD(strDate,strFormat) AS DATE
        _ymd := dValue:_ymd

    /// <summary>Construct a date from year, month, day </summary>
    [NODEBUG] [INLINE];
    CONSTRUCTOR(year AS INT, month AS INT, day AS INT)
        _ymd := 0
        IF year != 0 .AND. month != 0 .AND. day != 0
            TRY
                // this may throw an exception when the combination is not valid
                IF (year == 1 .AND. month == 1 .AND. day == 1)
                    // this is DateTime.Mindate, but a valid date in VO
                    _year  := 1
                    _month := 1
                    _day   := 1
                ELSE
                    VAR dt := System.DateTime{year, month, day}
                    _year  := (WORD) dt:Year
                    _month := (BYTE) dt:Month
                    _day   := (BYTE) dt:Day
                ENDIF
            CATCH
                _ymd := 0 // null_date
                NOP
            END TRY
        ELSE
            _ymd := 0 // null_date
        ENDIF
        RETURN

    /// <summary>Construct a date from year, month, day </summary>
    [NODEBUG] [INLINE];
    CONSTRUCTOR(year AS DWORD, month AS DWORD, day AS DWORD)
        // Chain to Int constructor
        SELF( (INT) year, (INT) month, (INT) day)
        RETURN

#endregion

#region methods
    /// <inheritdoc />
    METHOD CompareTo(o AS OBJECT) AS LONG
        VAR rhs := (DATE)o
        RETURN SELF:YMD:CompareTo(rhs:YMD)

    /// <inheritdoc />
    METHOD CompareTo(rhs AS DATE) AS LONG
        RETURN SELF:YMD:CompareTo(rhs:YMD)

    /// <inheritdoc />
    OVERRIDE METHOD GetHashCode() AS LONG
        RETURN _ymd:GetHashCode()

    /// <inheritdoc />
    METHOD GetTypeCode() AS System.TypeCode
        RETURN TypeCode.DateTime
#endregion
#region Operators
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR ++(lhs AS DATE) AS DATE
        RETURN lhs:Add(1)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR +(lhs AS DATE, days AS USUAL) AS DATE
        RETURN lhs:Add(days)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR +(lhs AS DATE, days AS REAL8) AS DATE
        RETURN lhs:Add(days)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR +(lhs AS DATE, days AS LONG) AS DATE
        RETURN lhs:Add(days)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    STATIC OPERATOR +(lhs AS DATE, days AS INT64) AS DATE
        RETURN lhs:Add(days)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    STATIC OPERATOR +(lhs AS DATE, ts AS System.TimeSpan) AS DATE
        RETURN lhs:Add(ts)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR +(lhs AS DATE, days AS DWORD) AS DATE
        RETURN lhs:Add(days)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR +(lhs AS DATE, days AS UINT64) AS DATE
        RETURN lhs:Add(days)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR --(lhs AS DATE) AS DATE
        RETURN lhs:Subtract(1)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR -(lhs AS DATE, rhs AS DATE) AS LONG
        RETURN lhs:Subtract(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR -(lhs AS DATE, days AS USUAL) AS USUAL
        RETURN lhs:Subtract(days)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR -(lhs AS DATE, days AS REAL8) AS DATE
        RETURN lhs:Subtract(days)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR -(lhs AS DATE, days AS LONG) AS DATE
        RETURN lhs:Subtract(days)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR -(lhs AS DATE, days AS INT64) AS DATE
        RETURN lhs:Subtract(days)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR -(lhs AS DATE, ts AS System.TimeSpan) AS DATE
        RETURN lhs:Subtract(ts)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR -(lhs AS DATE, days AS DWORD) AS DATE
        RETURN lhs:Subtract(days)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR -(lhs AS DATE, days AS UINT64) AS DATE
        RETURN lhs:Subtract(days)

#endregion
#region Equality Operators

    /// <inheritdoc />
    METHOD Equals(lhs AS DATE) AS LOGIC
        RETURN _ymd == lhs:_ymd

    /// <inheritdoc />
    OVERRIDE METHOD Equals(o AS OBJECT) AS LOGIC
        IF o != NULL
            IF o:GetType() == TYPEOF(DATE)
                RETURN SELF:Equals(  (DATE) o)
            ELSEIF o:GetType() == TYPEOF(System.DateTime)
                RETURN SELF:Equals( DATE{ (System.DateTime) o})
            ENDIF
        ENDIF
        RETURN FALSE

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    STATIC OPERATOR ==(lhs AS DATE, rhs AS DATE) AS LOGIC
        RETURN (lhs:_ymd == rhs:_ymd)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    STATIC OPERATOR !=(lhs AS DATE, rhs AS DATE) AS LOGIC
        RETURN (lhs:_ymd != rhs:_ymd)

#endregion
#region Implicit and Explicit converters

    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    STATIC OPERATOR EXPLICIT(dValue AS DATE) AS DWORD
        // convert to julian date like vo
        // # of days since 1-1-1901 + 2415386
        IF dValue:IsEmpty
            RETURN 0
        ENDIF
        LOCAL nDays AS DWORD
        nDays := (DWORD) (dValue:Value - _dtCalc):Days + CLIPPER_MIN_DATE
        RETURN nDays


    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    STATIC OPERATOR EXPLICIT(dValue AS DATE) AS INT
        IF dValue:IsEmpty
            RETURN 0
        ENDIF
        LOCAL nDays AS LONG
        nDays := (INT) ((dValue:Value - _dtCalc):Days + CLIPPER_MIN_DATE)
        RETURN nDays

    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    STATIC OPERATOR EXPLICIT(dw AS DWORD ) AS DATE
        // convert julian date to our date
        LOCAL result := _NULL_DATE AS DATE
        IF dw >= CLIPPER_MIN_DATE .AND. dw <= CLIPPER_MAX_DATE
            LOCAL dt AS DateTime
            dt := _dtCalc:AddDays ( (INT) dw -CLIPPER_MIN_DATE)
            result := DATE{dt}
        ENDIF
        RETURN result

    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    STATIC OPERATOR EXPLICIT(i AS INT) AS DATE
        LOCAL result := _NULL_DATE AS DATE
        IF i >= CLIPPER_MIN_DATE .AND. i <= CLIPPER_MAX_DATE
            LOCAL dt AS DateTime
            dt := _dtCalc:AddDays ( i - CLIPPER_MIN_DATE)
            result := DATE{dt}
        ENDIF
        RETURN result

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    STATIC OPERATOR IMPLICIT(v AS System.DateTime) AS DATE
        RETURN DATE{v}

    /// <exclude />
    METHOD ToDateTime() AS System.DateTime
        RETURN SELF:Value

    /// <exclude />
    METHOD FromDateTime(dtvalue AS System.DateTime) AS DATE
        RETURN DATE{dtvalue}

#endregion
#region Comparison Operators
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    STATIC OPERATOR >(lhs AS DATE, rhs AS DATE) AS LOGIC
        // note: cannot use _ymd for > comparison because that would depend
        //       on the processor layout
        IF lhs:_ymd == rhs:_ymd
            RETURN FALSE
        ELSEIF lhs:_year > rhs:_year
            RETURN TRUE
        ELSEIF lhs:_year == rhs:_year
            IF lhs:_month > rhs:_month
                RETURN TRUE
            ELSEIF lhs:_month == rhs:_month .AND. lhs:_day > rhs:_day
                RETURN TRUE
            ENDIF
        ENDIF
        RETURN FALSE

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    STATIC OPERATOR >=(lhs AS DATE, rhs AS DATE) AS LOGIC
        // note: cannot use _ymd for > comparison because that would depend
        //       on the processor layout
        IF lhs:_ymd == rhs:_ymd
            RETURN TRUE
        ELSEIF lhs:_year > rhs:_year
            RETURN TRUE
        ELSEIF lhs:_year == rhs:_year
            IF lhs:_month > rhs:_month
                RETURN TRUE
            ELSEIF lhs:_month == rhs:_month .AND. lhs:_day >= rhs:_day
                RETURN TRUE
            ENDIF
        ENDIF
        RETURN FALSE

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    STATIC OPERATOR IMPLICIT(v AS DATE) AS System.DateTime
        RETURN v:Value

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    STATIC OPERATOR <(lhs AS DATE, rhs AS DATE) AS LOGIC
        // note: cannot use _ymd for < comparison because that would depend
        //       on the processor layout
        IF lhs:_ymd == rhs:_ymd
            RETURN FALSE
        ELSEIF lhs:_year < rhs:_year
            RETURN TRUE
        ELSEIF lhs:_year == rhs:_year
            IF lhs:_month < rhs:_month
                RETURN TRUE
            ELSEIF lhs:_month == rhs:_month .AND. lhs:_day < rhs:_day
                RETURN TRUE
            ENDIF
        ENDIF
        RETURN FALSE

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    STATIC OPERATOR <=(lhs AS DATE, rhs AS DATE) AS LOGIC
        // note: cannot use _ymd for < comparison because that would depend
        //       on the processor layout
        IF lhs:_ymd == rhs:_ymd
            RETURN TRUE
        ELSEIF lhs:_year < rhs:_year
            RETURN TRUE
        ELSEIF lhs:_year == rhs:_year
            IF lhs:_month < rhs:_month
                RETURN TRUE
            ELSEIF lhs:_month == rhs:_month .AND. lhs:_day <= rhs:_day
                RETURN TRUE
            ENDIF
        ENDIF
        RETURN FALSE
#endregion

#region ADD and Subtract Methods
    /// <exclude />
    METHOD Add(days AS USUAL) AS DATE
        IF days:IsInteger
            RETURN SELF:Add( (INT64) days)
        ELSEIF days:IsNumeric
            RETURN SELF:Add( (REAL8) days)
        ELSE
            THROW Error.ArgumentError(__FUNCTION__,NAMEOF(days),1, "Incompatible argument for Date:Add()",{days})
        ENDIF
    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Add(days AS REAL8) AS DATE
        VAR res := SELF:Value:AddDays(days)
        RETURN DATE{res}
    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Add(days AS LONG) AS DATE
        VAR res := SELF:Value:AddDays(days)
        RETURN DATE{res}
    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Add(days AS INT64) AS DATE
        VAR res := SELF:Value:AddDays(days)
        RETURN DATE{res}

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Add(span AS System.TimeSpan) AS DATE
        VAR res := SELF:Value:Add(span)
        RETURN DATE{res}

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Add(days AS DWORD) AS DATE
        VAR res := SELF:Value:AddDays(days)
        RETURN DATE{res}

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Add(days AS UINT64) AS DATE
        VAR res := SELF:Value:AddDays(days)
        RETURN DATE{res}

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Subtract(lhs AS DATE) AS LONG
        LOCAL span AS System.TimeSpan
        span := (System.TimeSpan)(SELF:Value - lhs:Value)
        RETURN span:Days

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Subtract(days AS USUAL) AS USUAL
        IF days:IsInteger
            RETURN SELF:Subtract( (INT64) days)
        ELSEIF days:IsNumeric
            RETURN SELF:Subtract( (REAL8) days)
        ELSEIF days:IsDate
            RETURN SELF:Subtract( days:_dateValue)
        ELSE
            THROW Error.ArgumentError(__FUNCTION__,NAMEOF(days), 1, "Incompatible argument for Date:Subtract()", {days})
        ENDIF

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Subtract(days AS REAL8) AS DATE
        RETURN SELF:Add(-days)


    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Subtract(days AS LONG) AS DATE
        RETURN SELF:Add(-days)

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Subtract(days AS INT64) AS DATE
        RETURN SELF:Add(-days)

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Subtract(ts AS System.TimeSpan) AS DATE
        RETURN SELF:Add(-ts)

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Subtract(days AS DWORD) AS DATE
        RETURN SELF:Add(-days)

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Subtract(days AS UINT64) AS DATE
        RETURN SELF:Add(-(INT64)days)
#endregion

#region IConvertable
        // forward most methods to the DateTime class so there will
        // be a proper (localized) error message
    /// <inheritdoc />
    METHOD IConvertible.ToBoolean(provider AS System.IFormatProvider) AS LOGIC
        RETURN ((IConvertible)SELF:Value):ToBoolean(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToByte(provider AS System.IFormatProvider) AS BYTE
        RETURN ((IConvertible)SELF:Value):ToByte(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToChar(provider AS System.IFormatProvider) AS CHAR
        RETURN ((IConvertible)SELF:Value):ToChar(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToDateTime(provider AS System.IFormatProvider) AS System.DateTime
        RETURN SELF:Value

    /// <inheritdoc />
    METHOD IConvertible.ToDecimal(provider AS System.IFormatProvider) AS Decimal
        RETURN ((IConvertible)SELF:Value):ToDecimal(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToDouble(provider AS System.IFormatProvider) AS REAL8
        RETURN ((IConvertible)SELF:Value):ToDouble(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToInt16(provider AS System.IFormatProvider) AS SHORT
        RETURN ((IConvertible)SELF:Value):ToInt16(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToInt32(provider AS System.IFormatProvider) AS LONG
        RETURN ((IConvertible)SELF:Value):ToInt32(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToInt64(provider AS System.IFormatProvider) AS INT64
        RETURN ((IConvertible)SELF:Value):ToInt64(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToSByte(provider AS System.IFormatProvider) AS SByte
        RETURN ((IConvertible)SELF:Value):ToSByte(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToSingle(provider AS System.IFormatProvider) AS REAL4
        RETURN ((IConvertible)SELF:Value):ToSingle(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToType(conversionType AS System.Type, provider AS System.IFormatProvider) AS OBJECT
        IF conversionType == TYPEOF(DATE)
            RETURN SELF
        ELSEIF conversionType == TYPEOF(STRING)
            RETURN SELF:ToString()
        ELSEIF conversionType == TYPEOF(System.DateTime)
            RETURN SELF:Value
        ENDIF
        RETURN ((IConvertible)SELF:Value):ToType(conversionType, provider)

    /// <inheritdoc />
    METHOD IConvertible.ToUInt16(provider AS System.IFormatProvider) AS WORD
        RETURN ((IConvertible)SELF:Value):ToUInt16(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToUInt32(provider AS System.IFormatProvider) AS DWORD
        RETURN ((IConvertible)SELF:Value):ToUInt32(provider)

    /// <inheritdoc />
    METHOD IConvertible.ToUInt64(provider AS System.IFormatProvider) AS UINT64
        RETURN ((IConvertible)SELF:Value):ToUInt64(provider)
#endregion

#region ToString()
        // Use DateTime ToString() methods as helpers
    /// <inheritdoc />
    OVERRIDE METHOD ToString() AS STRING
        IF (_ymd == 0)
            RETURN RuntimeState.NullDateString
        ENDIF
        RETURN DToC(SELF)
    /// <inheritdoc />
    METHOD ToString(provider AS System.IFormatProvider) AS STRING
        IF (_ymd == 0)
            RETURN RuntimeState.NullDateString
        ENDIF
        RETURN SELF:Value:ToString(provider)
    /// <inheritdoc cref="M:System.DateTime.ToString(System.String)"/>
    METHOD ToString(s AS STRING) AS STRING
        IF (_ymd == 0)
            RETURN RuntimeState.NullDateString
        ENDIF
        RETURN SELF:Value:ToString(s)
    /// <inheritdoc />
    METHOD ToString(s AS STRING, fp AS System.IFormatProvider) AS STRING
        IF (_ymd == 0)
            RETURN RuntimeState.NullDateString
        ENDIF
        IF (s == NULL)
            s := XSharp.RuntimeState.GetValue<STRING>(Set.DateFormatNet)
        ENDIF
        RETURN SELF:Value:ToString(s, fp)
#endregion

#region ISerializable
    /// <inheritdoc/>
    PUBLIC METHOD GetObjectData(info AS SerializationInfo, context AS StreamingContext) AS VOID
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        info:AddValue("Value", DToS(SELF))
        RETURN
    /// <include file="RTComments.xml" path="Comments/SerializeConstructor/*" />
    CONSTRUCTOR (info AS SerializationInfo, context AS StreamingContext)
        _ymd := _year := _month := _day := 0
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        SELF := SToD(info:GetString("Value"))
#endregion

#region properties

    /// <inheritdoc />
    [NOSHOW] PROPERTY IsEmpty AS LOGIC
    GET
        RETURN _ymd == 0
    END GET
    END PROPERTY
    /// <inheritdoc />
    PROPERTY Month	AS INT GET _month
    /// <inheritdoc />
    PROPERTY Year	AS INT GET _year
    /// <inheritdoc />
    PROPERTY Day	AS INT GET _day
        // Next properties for easy access in right type
    [NOSHOW] INTERNAL PROPERTY DYear	AS DWORD GET _year
    [NOSHOW] INTERNAL PROPERTY DMonth	AS DWORD GET _month
    [NOSHOW] INTERNAL PROPERTY DDay	    AS DWORD GET _day
    [NOSHOW] INTERNAL PROPERTY YMD      AS DWORD GET (SELF:DYear * 0xFFFF)  + (SELF:DMonth *0xFF) + _day

    INTERNAL METHOD ToDebugString() AS STRING
        IF (_ymd == 0)
            RETURN "NULL_DATE"
        ENDIF
        RETURN SELF:Value:ToShortDateString()


#endregion

#region STATIC Properties

    /// <exclude />
    STATIC METHOD NullDate AS DATE
        RETURN  _NULL_DATE


#endregion

END STRUCTURE
END NAMESPACE
