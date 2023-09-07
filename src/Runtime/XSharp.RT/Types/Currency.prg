//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Runtime.InteropServices
USING System.Runtime.CompilerServices
USING System.Diagnostics
USING System.Runtime.Serialization


#include "attributes.xh"

BEGIN NAMESPACE XSharp
// Type is Immutable, so has no settable properties
/// <summary>Internal type that implements the FoxPro Compatible CURRENCY type.
/// This type has many operators and implicit converters that normally are never directly called from user code.
/// The data in this type is stored as a System.Decimal with 4 decimal places
/// </summary>
/// <seealso cref="__Float"/>
/// <seealso cref="System.Decimal"/>
[DebuggerDisplay("{ToDebugString(),nq}")];
[Serializable];
PUBLIC STRUCTURE __Currency IMPLEMENTS IConvertible,;
        IFormattable, ;
        IComparable<__Currency>, ;
        IEquatable<__Currency>, ;
        IComparable,            ;
        ISerializable,          ;
        ICurrency

    [NOSHOW] PRIVATE INITONLY _value AS System.Decimal

#region constructors
    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    /// <param name="r8">Real8 value to convert to a FLOAT</param>
    [NODEBUG] [INLINE];
    CONSTRUCTOR (r8 AS REAL8)
        SELF:_value    := Math.Round((Decimal)r8,4)

    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    [NODEBUG] [INLINE];
    CONSTRUCTOR (d AS System.Decimal)
        SELF:_value    := Math.Round(d,4)
    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    [NODEBUG] [INLINE];
    CONSTRUCTOR (f AS IFloat)
        SELF:_value		:= Math.Round((Decimal)f:Value,4)

    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    [NODEBUG] [INLINE];
    CONSTRUCTOR (c AS ICurrency)
        SELF:_value		:= c:Value

#endregion
#region Properties
    /// <summary>Decimal (System.Decimal) value</summary>
    [NOSHOW] PROPERTY @@Value    AS System.Decimal	GET _value
#endregion

#region Equality Operators
    /// <inheritdoc />
    OVERRIDE METHOD Equals(rhs AS OBJECT  ) AS LOGIC
        LOCAL result AS LOGIC
        IF rhs != NULL .AND. rhs IS CURRENCY
            result := SELF:Equals( (CURRENCY) rhs)
        ELSE
            result := FALSE
        ENDIF
        RETURN result

    /// <inheritdoc />
    METHOD Equals(rhs AS CURRENCY ) AS LOGIC
        RETURN SELF:Value == rhs:Value

    /// <inheritdoc />
    OVERRIDE METHOD GetHashCode() AS INT
        RETURN SELF:_value:GetHashCode()

    /// <exclude />
    METHOD GetTypeCode() AS TypeCode
        RETURN TypeCode.Decimal


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR ==(lhs AS CURRENCY, rhs AS CURRENCY) AS LOGIC
        RETURN lhs:Equals(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR !=(lhs AS CURRENCY, rhs AS CURRENCY) AS LOGIC
        RETURN ! lhs:Equals(rhs)
#endregion

#region Comparison Operators
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR >(lhs AS CURRENCY, rhs AS CURRENCY) AS LOGIC
        RETURN lhs:_value > rhs:_value


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR <(lhs AS CURRENCY, rhs AS CURRENCY) AS LOGIC
        RETURN lhs:_value < rhs:_value

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR >=(lhs AS CURRENCY, rhs AS CURRENCY) AS LOGIC
        RETURN lhs:_value >= rhs:_value

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR <=(lhs AS CURRENCY, rhs AS CURRENCY) AS LOGIC
        RETURN lhs:_value <= rhs:_value

#endregion

#region Implicit Converters
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(b AS BYTE) AS CURRENCY
        RETURN CURRENCY{(Decimal) b}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(sb AS SByte) AS CURRENCY
        RETURN CURRENCY{(Decimal) sb}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(si AS SHORT) AS CURRENCY
        RETURN CURRENCY{(Decimal)si}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(w AS WORD) AS CURRENCY
        RETURN CURRENCY{(Decimal)w}
    /// <include file="RTComments.xml" path="Comments/Converter/*" />

    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(i AS INT) AS CURRENCY
        RETURN CURRENCY{(Decimal)i}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(dw AS DWORD) AS CURRENCY
        RETURN CURRENCY{(Decimal)dw}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(i64 AS INT64) AS CURRENCY
        RETURN CURRENCY{(Decimal)i64}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(ui64 AS UINT64) AS CURRENCY
        RETURN CURRENCY{(Decimal)ui64}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(r4 AS REAL4) AS CURRENCY
        RETURN CURRENCY{(REAL8)r4}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(r8 AS REAL8) AS CURRENCY
        RETURN CURRENCY{r8}

    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(fl AS FLOAT) AS CURRENCY
        RETURN CURRENCY{fl:Value}


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(val AS System.Decimal) AS CURRENCY
        RETURN CURRENCY{val}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(c  AS CURRENCY) AS REAL8
        RETURN CHECKED((REAL8) c:_value)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(c  AS CURRENCY) AS REAL4
        RETURN CHECKED((REAL4) c:_value)


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(c  AS CURRENCY) AS FLOAT
        RETURN CHECKED(FLOAT{ (REAL8) c:_value, -1, 4})

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(c  AS CURRENCY) AS System.Decimal
        RETURN c:_value

#endregion
#region Explicit Converters
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(c  AS CURRENCY) AS BYTE
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToByte(c:_value)
        ENDIF
        RETURN CHECKED((BYTE) c:_value)
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(c AS CURRENCY) AS SByte
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToSByte(c:_value)
        ENDIF
        RETURN CHECKED((SByte) c:_value)
    /// <include file="RTComments.xml" path="Comments/Converter/*" />

    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(c  AS CURRENCY) AS SHORT
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToInt16(c:_value)
        ENDIF
        RETURN CHECKED((SHORT)c:_value)

    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(c  AS CURRENCY) AS WORD
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToUInt16(c:_value)
        ENDIF
        RETURN CHECKED((WORD) c:_value)
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(c  AS CURRENCY) AS LONG
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToInt32(c:_value)
        ENDIF
        RETURN CHECKED((LONG) c:_value)
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(c  AS CURRENCY) AS DWORD
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToUInt32(c:_value)
        ENDIF
        RETURN (DWORD) c:_value
    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(c  AS CURRENCY) AS INT64
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToInt64(c:_value)
        ENDIF
        RETURN CHECKED((INT64) c:_value)

    /// <include file="RTComments.xml" path="Comments/Converter/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR EXPLICIT(c  AS CURRENCY) AS UINT64
        IF RuntimeState.CompilerOptionVO11
            RETURN Convert.ToUInt64(c:_value)
        ENDIF
        RETURN CHECKED((UINT64) c:_value)

#endregion

#region Numeric Operators
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR +(c  AS CURRENCY) AS CURRENCY
        RETURN c

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR -(c  AS CURRENCY) AS CURRENCY
        RETURN CURRENCY{ -c:_value}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR+(lhs AS CURRENCY, rhs AS CURRENCY) AS CURRENCY
        RETURN lhs:Add(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR+(lhs AS CURRENCY, rhs AS USUAL) AS CURRENCY
        RETURN lhs:Add(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR+(lhs AS USUAL, rhs AS CURRENCY) AS CURRENCY
        RETURN rhs:Add(lhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR-(lhs AS CURRENCY, rhs AS CURRENCY) AS CURRENCY
        RETURN lhs:Subtract(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR-(lhs AS CURRENCY, rhs AS USUAL) AS CURRENCY
        RETURN lhs:Subtract(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR-(lhs AS USUAL, rhs AS CURRENCY) AS CURRENCY
        // set decimals for LHS to 0, so max decmals is decimals right
        local lCurr AS Currency
        IF lhs:IsCurrency
            lCurr := lhs:_currencyValue
        ELSE
            lCurr := lhs
        ENDIF
        RETURN lCurr:Subtract(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR*(lhs AS CURRENCY, rhs AS CURRENCY) AS CURRENCY
        RETURN CURRENCY{ lhs:_value * rhs:_value}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR/(lhs AS CURRENCY, rhs AS CURRENCY) AS CURRENCY
        RETURN CURRENCY{ lhs:_value / rhs:_value}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR%(lhs AS CURRENCY, rhs AS CURRENCY) AS CURRENCY
        RETURN CURRENCY{ lhs:_value % rhs:_value}

#endregion
#region Unary Operators
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR ++ (c  AS CURRENCY) AS CURRENCY
        RETURN CURRENCY{c:_value+1}


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR -- (c  AS CURRENCY) AS CURRENCY
        RETURN CURRENCY{c:_value-1}
#endregion

#region Explicit casts. Used inside Transform
    /// <exclude />
    METHOD CastToInt() AS INT
        RETURN CHECKED((INT)(SELF:_value))

    /// <exclude />
    METHOD CastToInt64() AS INT64
        RETURN CHECKED((INT64)(SELF:_value))

#endregion
#region Add and Subtract
    /// <exclude />
    METHOD Add(rhs AS CURRENCY) AS CURRENCY
        RETURN CURRENCY{ SELF:_value + rhs:_value}

    /// <exclude />
    METHOD Add(rhs AS USUAL) AS CURRENCY
        LOCAL result AS CURRENCY
        IF rhs:IsFloat
            result := SELF:Add ( (CURRENCY) rhs)
        ELSEIF rhs:IsDecimal
            result := SELF:Add ( (System.Decimal) rhs)
        ELSEIF rhs:IsCurrency
            result := SELF:Add ( rhs:_currencyValue)
        ELSEIF  rhs:IsLong
            result := CURRENCY{ SELF:_value + (LONG) rhs}
        ELSEIF  rhs:IsInt64
            result := CURRENCY{ SELF:_value + (INT64) rhs}
        ELSE
            THROW Error.ArgumentError(__FUNCTION__,Nameof(rhs), "Argument is not numeric")
        ENDIF
        RETURN result


    /// <exclude />
    METHOD Subtract(rhs AS CURRENCY) AS CURRENCY
        RETURN CURRENCY{ SELF:_value - rhs:_value}

    /// <exclude />
    METHOD Subtract(rhs AS USUAL) AS CURRENCY
        LOCAL result AS CURRENCY
        IF rhs:IsFloat
            result := SELF:Subtract( (CURRENCY) rhs)
        ELSEIF rhs:IsDecimal
            result := SELF:Subtract( (System.Decimal) rhs)
        ELSEIF rhs:IsCurrency
            result := SELF:Subtract( rhs:_currencyValue)
        ELSEIF  rhs:IsLong
            result := CURRENCY{ SELF:_value - (LONG) rhs}
        ELSEIF  rhs:IsInt64
            result := CURRENCY{ SELF:_value - (INT64) rhs}
        ELSE
            THROW Error.ArgumentError(__FUNCTION__,Nameof(rhs), "Argument is not numeric")
        ENDIF
        RETURN result


#endregion

#region IConvertable
    /// <inheritdoc />
    PUBLIC METHOD IConvertible.ToBoolean(provider AS System.IFormatProvider) AS LOGIC
        RETURN _value ==  0.0m

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
    /// <inheritdoc cref="System.Double.ToString"/>
    PUBLIC OVERRIDE METHOD ToString() AS STRING
        RETURN SELF:ToString("0.0000")

    /// <inheritdoc cref="System.Double.ToString"/>
    PUBLIC METHOD ToString(sFormat AS STRING) AS STRING
        RETURN _value:ToString(sFormat)

    /// <inheritdoc />
    PUBLIC METHOD ToString(format AS STRING, provider AS System.IFormatProvider) AS STRING
        RETURN ((IFormattable) _value):ToString(format, provider)


    INTERNAL METHOD ToDebugString() AS STRING
        RETURN SELF:ToString("0.0000")

#endregion

#region IComparable
    /// <inheritdoc />
    PUBLIC METHOD CompareTo(rhs AS CURRENCY) AS INT
        RETURN _value:CompareTo( rhs:_value)

    /// <inheritdoc />
    PUBLIC METHOD CompareTo(rhs AS OBJECT) AS INT
        RETURN SELF:CompareTo( (CURRENCY) rhs)
#endregion

#region ISerializable
    /// <inheritdoc/>
    PUBLIC METHOD GetObjectData(info AS SerializationInfo, context AS StreamingContext) AS VOID
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        info:AddValue("Value", SELF:_value)
        RETURN
    /// <include file="RTComments.xml" path="Comments/SerializeConstructor/*" />
    CONSTRUCTOR (info AS SerializationInfo, context AS StreamingContext)
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        SELF:_value   := info:GetDecimal("Value")
#endregion



END STRUCTURE

END NAMESPACE
