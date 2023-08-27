//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Runtime.InteropServices
USING System.Runtime.CompilerServices
USING System.Runtime.Serialization
USING System.Diagnostics
#include "attributes.xh"


BEGIN NAMESPACE XSharp
    // use explicit layout so we can compact the size into 12 bytes
    // Type is Immutable, so no settable properties
/// <summary>Internal type that implements the XBase Compatible FLOAT type.
/// This type has many operators and implicit converters that normally are never directly called from user code.
/// </summary>
/// <seealso cref="IFloat"/>
/// <seealso cref="RDD.DbFloat"/>
[DebuggerDisplay("{ToDebugString(),nq}", Type := "FLOAT" )];
[StructLayout(LayoutKind.Explicit, Pack := 4)];
[Serializable];
PUBLIC STRUCTURE __Float IMPLEMENTS IFloat, ;
        IConvertible,;
        IFormattable, ;
        IComparable<__Float>, ;
        IEquatable<__Float>, ;
        IComparable,            ;
        ISerializable

    [NOSHOW] [FieldOffset(0)]  PRIVATE INITONLY _value AS REAL8
    [NOSHOW] [FieldOffset(8)]  PRIVATE INITONLY _length AS SHORTINT
    [NOSHOW] [FieldOffset(10)] PRIVATE INITONLY _decimals AS SHORTINT

#region constructors
    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    /// <param name="r8">Real8 value to convert to a FLOAT</param>
    [NODEBUG]  [INLINE];
    CONSTRUCTOR (r8 AS REAL8)
        SELF:_value    := r8
        SELF:_length   := 0
        SELF:_decimals := -1

    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    [NODEBUG]  [INLINE];
    CONSTRUCTOR (r8 AS REAL8, decimals AS INT)
        SELF:_value    := r8
        SELF:_length   := 0
        SELF:_decimals := (SHORTINT) decimals

    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    [NODEBUG]  [INLINE];
    CONSTRUCTOR (r8 AS REAL8, decimals AS DWORD)
        SELF:_value    := r8
        SELF:_length   := 0
        SELF:_decimals := (SHORTINT) decimals

    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    [NODEBUG]  [INLINE];
    CONSTRUCTOR (r8 AS REAL8, length AS DWORD, decimals AS DWORD)
        SELF:_value    := r8
        SELF:_length   := (SHORTINT) length
        SELF:_decimals := (SHORTINT) decimals

    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    [NODEBUG]  [INLINE];
    CONSTRUCTOR (r8 AS REAL8, length AS INT, decimals AS INT )
        SELF:_value    := r8
        SELF:_length   := (SHORTINT) length
        SELF:_decimals := (SHORTINT) decimals

    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    [NODEBUG]  [INLINE];
    CONSTRUCTOR (@@Value AS IFloat)
        SELF:_value		:= @@Value:Value
        SELF:_length	:= (SHORT)  @@Value:Digits
        SELF:_decimals	:= (SHORT)  @@Value:Decimals
#endregion
#region Properties
    /// <summary>REAL8 (System.Double) value</summary>
    PROPERTY @@Value    AS REAL8	GET _value
    /// <summary>Width </summary>
    PROPERTY Digits   AS INT	GET _length
    /// <summary>Number of decimals</summary>
    PROPERTY Decimals AS INT	GET _decimals
#endregion

#region Equality Operators
    /// <inheritdoc />
    OVERRIDE METHOD Equals(rhs AS OBJECT  ) AS LOGIC
        LOCAL result AS LOGIC
        IF rhs != NULL .AND. rhs IS FLOAT
            result := SELF:Equals( (FLOAT) rhs)
        ELSE
            result := FALSE
        ENDIF
        RETURN result

    /// <inheritdoc />
    METHOD Equals(rhs AS FLOAT ) AS LOGIC
        LOCAL delta AS REAL8
        LOCAL diff  AS REAL8
        LOCAL equal AS LOGIC

        IF Double.IsNaN(rhs:_value) .or. Double.IsNaN(SELF:_value)
            RETURN Double.IsNaN(rhs:_value) .and. Double.IsNaN(SELF:_value)
        ELSEIF rhs:_value == Double.PositiveInfinity .or. SELF:_value == Double.PositiveInfinity
            RETURN rhs:_value == SELF:_value
        ELSEIF rhs:_value == Double.NegativeInfinity .or. SELF:_value == Double.NegativeInfinity
            RETURN rhs:_value == SELF:_value
        END IF

        delta := RuntimeState.FloatDelta
        diff := _value - rhs:_value
        IF delta == 0.0
            equal :=  diff == 0.0
        ELSEIF diff < 0.0
            equal :=  Math.Abs(diff) < delta
        ELSE
            equal := diff < delta
        ENDIF
        RETURN equal

    /// <inheritdoc />
    OVERRIDE METHOD GetHashCode() AS INT
        RETURN SELF:_value:GetHashCode()

    /// <exclude />
    METHOD GetTypeCode() AS TypeCode
        RETURN TypeCode.Double


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR ==(lhs AS FLOAT, rhs AS FLOAT) AS LOGIC
        RETURN lhs:Equals(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR !=(lhs AS FLOAT, rhs AS FLOAT) AS LOGIC
        RETURN ! lhs:Equals(rhs)
#endregion

#region Comparison Operators
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR >(lhs AS FLOAT, rhs AS FLOAT) AS LOGIC
        LOCAL delta AS REAL8
        LOCAL diff  AS REAL8
        delta := RuntimeState.FloatDelta
        diff := lhs:_value - rhs:_value
        IF (delta == 0.0)
            RETURN diff > 0.0
        ENDIF
        RETURN diff > delta


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR <(lhs AS FLOAT, rhs AS FLOAT) AS LOGIC
        LOCAL delta AS REAL8
        LOCAL diff  AS REAL8
        delta := RuntimeState.FloatDelta
        diff := lhs:_value - rhs:_value
        IF (delta == 0.0)
            RETURN diff < 0.0
        ENDIF
        RETURN diff < -delta

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR >=(lhs AS FLOAT, rhs AS FLOAT) AS LOGIC
        // call other operator methods for simplicity
        // we may want to optimize this later
        RETURN lhs > rhs .OR. lhs == rhs

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR <=(lhs AS FLOAT, rhs AS FLOAT) AS LOGIC
        // call other operator methods for simplicity
        // we may want to optimize this later
        RETURN lhs < rhs .OR. lhs == rhs

#endregion

#region Implicit Converters
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(b AS BYTE) AS FLOAT
        RETURN FLOAT{b, 0}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(sb AS SByte) AS FLOAT
        RETURN FLOAT{sb, 0}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(si AS SHORT) AS FLOAT
        RETURN FLOAT{si, 0}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(w AS WORD) AS FLOAT
        RETURN FLOAT{w, 0}
    /// <include file="RTComments.xml" path="Comments/Converter/*" />

    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(i AS INT) AS FLOAT
        RETURN FLOAT{i, 0}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(dw AS DWORD) AS FLOAT
        RETURN FLOAT{dw, 0}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(i64 AS INT64) AS FLOAT
        RETURN FLOAT{i64, 0}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(ui64 AS UINT64) AS FLOAT
        RETURN FLOAT{ui64, 0}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(r4 AS REAL4) AS FLOAT
        RETURN FLOAT{r4, RuntimeState.Decimals}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(r8 AS REAL8) AS FLOAT
        RETURN FLOAT{r8, RuntimeState.Decimals}


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(val AS System.Decimal) AS FLOAT
        RETURN FLOAT{ (REAL8) val, RuntimeState.Decimals}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(fl  AS FLOAT) AS REAL8
        RETURN fl:_value

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(fl  AS FLOAT) AS REAL4
        RETURN (REAL4) fl:_value

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(fl  AS FLOAT) AS System.Decimal
        RETURN (System.Decimal) fl:_value

#endregion
#region Explicit Converters
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(fl  AS FLOAT) AS BYTE
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToByte(fl:_value)
        ENDIF
        RETURN CHECKED((BYTE) fl:_value)
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(fl AS FLOAT) AS SByte
        RETURN CHECKED((SByte) fl:_value)
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(fl  AS FLOAT) AS SHORT
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToInt16(fl:_value)
        ENDIF
        RETURN CHECKED((SHORT) fl:_value)
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(fl  AS FLOAT) AS WORD
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToUInt16(fl:_value)
        ENDIF
        RETURN CHECKED((WORD) fl:_value)
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(fl  AS FLOAT) AS LONG
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToInt32(fl:_value)
        ENDIF
        RETURN CHECKED((LONG) fl:_value)
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(fl  AS FLOAT) AS DWORD
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToUInt32(fl:_value)
        ENDIF
        RETURN CHECKED((DWORD) fl:_value)
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(fl  AS FLOAT) AS INT64
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToInt64(fl:_value)
        ENDIF
        RETURN CHECKED((INT64) fl:_value)
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(fl  AS FLOAT) AS UINT64
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToUInt64(fl:_value)
        ENDIF
        RETURN CHECKED((UINT64) fl:_value)

#endregion

#region Numeric Operators
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR +(fl  AS FLOAT) AS FLOAT
        RETURN fl

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR -(fl  AS FLOAT) AS FLOAT
        RETURN FLOAT{- fl:_value, fl:Digits, fl:Decimals}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR+(lhs AS FLOAT, rhs AS FLOAT) AS FLOAT
        RETURN lhs:Add(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR+(lhs AS FLOAT, rhs AS USUAL) AS FLOAT
        RETURN lhs:Add(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR+(lhs AS USUAL, rhs AS FLOAT) AS FLOAT
        RETURN rhs:Add(lhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR-(lhs AS FLOAT, rhs AS FLOAT) AS FLOAT
        RETURN lhs:Subtract(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR-(lhs AS FLOAT, rhs AS USUAL) AS FLOAT
        RETURN lhs:Subtract(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR-(lhs AS USUAL, rhs AS FLOAT) AS FLOAT
        // set decimals for LHS to 0, so max decmals is decimals right
        RETURN FLOAT{lhs, 0}:Subtract(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR*(lhs AS FLOAT, rhs AS FLOAT) AS FLOAT
        RETURN FLOAT{ lhs:_value * rhs:_value, lhs:Decimals + rhs:Decimals}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    //[NODEBUG] [INLINE];
    OPERATOR/(lhs AS FLOAT, rhs AS FLOAT) AS FLOAT
        VAR tmp := lhs:_value / rhs:_value
        IF System.Double.IsNaN(tmp) .or. System.Double.IsInfinity(tmp)
            THROW DivideByZeroException{}
        ENDIF
        RETURN FLOAT{ tmp, RuntimeState.Decimals}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR%(lhs AS FLOAT, rhs AS FLOAT) AS FLOAT
        RETURN FLOAT{ lhs:_value % rhs:_value, RuntimeState.Decimals}

#endregion
#region Unary Operators
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR ++ (fl  AS FLOAT) AS FLOAT
        RETURN FLOAT{fl:_value+1, fl:Digits, fl:Decimals}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR -- (fl  AS FLOAT) AS FLOAT
        RETURN FLOAT{fl:_value-1, fl:Digits, fl:Decimals}
#endregion

#region Explicit casts. Used inside Transform
    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD CastToInt() AS INT
        RETURN CHECKED((INT)(SELF:_value))

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD CastToInt64() AS INT64
        RETURN CHECKED((INT64)(SELF:_value))

#endregion
#region Add and Subtract
    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Add(rhs AS FLOAT) AS FLOAT
        RETURN FLOAT{ SELF:_value + rhs:_value, Math.Max(SELF:_decimals, rhs:_decimals)}

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Add(rhs AS USUAL) AS FLOAT
        LOCAL result AS FLOAT
        IF rhs:IsFloat
            result := SELF:Add (  rhs:_floatValue)
        ELSEIF rhs:IsDecimal .OR. rhs:IsCurrency
            result := SELF:Add ( (System.Decimal) rhs)
        ELSEIF  rhs:IsLong
            result := FLOAT{ SELF:_value + (LONG) rhs, SELF:Digits, SELF:Decimals}
        ELSEIF rhs:IsInt64
            result := FLOAT{ SELF:_value + (INT64) rhs, SELF:Digits, SELF:Decimals}
       ELSE
            THROW Error.ArgumentError(__FUNCTION__,Nameof(rhs), "Argument is not numeric")
        ENDIF
        RETURN result


    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Subtract(rhs AS FLOAT) AS FLOAT
        RETURN FLOAT{ SELF:_value - rhs:_value, Math.Max(SELF:_decimals, rhs:_decimals)}

    /// <exclude />
    [NODEBUG] [INLINE];
    METHOD Subtract(rhs AS USUAL) AS FLOAT
        LOCAL result AS FLOAT
        IF rhs:IsFloat
            result := SELF:Subtract(  rhs:_floatValue)
        ELSEIF rhs:IsDecimal .OR. rhs:IsCurrency
            result := SELF:Subtract( (System.Decimal) rhs)
        ELSEIF rhs:IsLong
            result := FLOAT{ SELF:_value - (LONG) rhs, SELF:Digits, SELF:Decimals}
        ELSEIF rhs:IsInt64
            result := FLOAT{ SELF:_value - (INT64) rhs, SELF:Digits, SELF:Decimals}
        ELSE
            THROW Error.ArgumentError(__FUNCTION__,Nameof(rhs), "Argument is not numeric")
        ENDIF
        RETURN result


#endregion

#region IConvertable
    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToBoolean(provider AS System.IFormatProvider) AS LOGIC
        RETURN _value ==  0.0

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToByte(provider AS System.IFormatProvider) AS BYTE
        RETURN ((IConvertible) _value):ToByte(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToChar(provider AS System.IFormatProvider) AS CHAR
        RETURN ((IConvertible) _value):ToChar(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToDateTime(provider AS System.IFormatProvider) AS System.DateTime
        RETURN ((IConvertible) _value):ToDateTime(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToDecimal(provider AS System.IFormatProvider) AS Decimal
        RETURN ((IConvertible) _value):ToDecimal(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToDouble(provider AS System.IFormatProvider) AS REAL8
        RETURN ((IConvertible) _value):ToDouble(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToInt16(provider AS System.IFormatProvider) AS SHORT
        RETURN ((IConvertible) _value):ToInt16(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToInt32(provider AS System.IFormatProvider) AS LONG
        RETURN ((IConvertible) _value):ToInt32(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToInt64(provider AS System.IFormatProvider) AS INT64
        RETURN ((IConvertible) _value):ToInt64(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToSByte(provider AS System.IFormatProvider) AS SByte
        RETURN ((IConvertible) _value):ToSByte(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToSingle(provider AS System.IFormatProvider) AS REAL4
        RETURN ((IConvertible) _value):ToSingle(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToType(conversionType AS System.Type, provider AS System.IFormatProvider) AS OBJECT
        RETURN ((IConvertible) _value):ToType(conversionType, provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToUInt16(provider AS System.IFormatProvider) AS WORD
        RETURN ((IConvertible) _value):ToUInt16(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToUInt32(provider AS System.IFormatProvider) AS DWORD
        RETURN ((IConvertible) _value):ToUInt32(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToUInt64(provider AS System.IFormatProvider) AS UINT64
        RETURN ((IConvertible) _value):ToUInt64(provider)

    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToString(provider AS System.IFormatProvider) AS STRING
        RETURN ((IConvertible) _value):ToString(provider)
#endregion
#region IFormattable
    /// <inheritdoc />
    PUBLIC OVERRIDE METHOD ToString() AS STRING
        RETURN Str1(SELF)

    /// <inheritdoc cref="System.Double.ToString(System.String)"/>
    PUBLIC METHOD ToString(sFormat AS STRING) AS STRING
        RETURN _value:ToString(sFormat)

    /// <inheritdoc />
    PUBLIC METHOD ToString(format AS STRING, provider AS System.IFormatProvider) AS STRING
        RETURN ((IFormattable) _value):ToString(format, provider)
#endregion
#region IComparable
    /// <inheritdoc />
    PUBLIC METHOD CompareTo(rhs AS FLOAT) AS INT
        RETURN _value:CompareTo( rhs:_value)

    /// <inheritdoc />
    PUBLIC METHOD CompareTo(rhs AS OBJECT) AS INT
        RETURN SELF:CompareTo( (FLOAT) rhs)
#endregion

#region ISerializable
    /// <inheritdoc/>
    PUBLIC METHOD GetObjectData(info AS SerializationInfo, context AS StreamingContext) AS VOID
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        info:AddValue("Value", SELF:_value)
        info:AddValue("Length", SELF:_length)
        info:AddValue("Decimals", SELF:_decimals)
        RETURN

    /// <include file="RTComments.xml" path="Comments/SerializeConstructor/*" />
    CONSTRUCTOR (info AS SerializationInfo, context AS StreamingContext)
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        SELF:_value     := info:GetDouble("Value")
        SELF:_length    := info:GetInt16("Length")
        SELF:_decimals  := info:GetInt16("Decimals")
#endregion


    PUBLIC METHOD ToDebugString() AS STRING
        RETURN SELF:Value:ToString()


END STRUCTURE

END NAMESPACE
