//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Text
USING System.Runtime.InteropServices
USING System.Runtime.CompilerServices
USING System.Diagnostics
USING System.Runtime.Serialization
#pragma options ("az", ON)

#include "attributes.xh"

BEGIN NAMESPACE XSharp
/// <summary>Internal type that implements the FoxPro Compatible BINARY type.<br/>
/// This type has many operators and implicit converters that normally are never directly called from user code.
/// The data in this type is stored as an array of Bytes<br/>
/// Conversions from and to String are supported and they use the current active windows codepage.
/// </summary>
[DebuggerDisplay("{ToDebugString(),nq}")];
[Serializable];
PUBLIC STRUCTURE __Binary IMPLEMENTS IFormattable, ;
        IComparable<__Binary>, ;
        IEquatable<__Binary>, ;
        IComparable,           ;
        ISerializable

    [NOSHOW] PRIVATE INITONLY _value AS BYTE[]

#region constructors
    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    /// <param name="b">Byte[] value that has the bytes that define the binary</param>
    [NODEBUG] [INLINE];
    CONSTRUCTOR (b AS BYTE[])
        IF b == NULL
            THROW NullError()
        ENDIF
        SELF:_value    := b

    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    /// <param name="s">STRING that will be converted to bytes using the current windows codepage.</param>
    /// <remarks>Accented characters and characters outside the current windows codepage may be replaced with a question mark.
    /// and when the current codepage is a so called multi byte codepage than one character may be mapped to more than one byte in the result</remarks>
    [NODEBUG] [INLINE];
    CONSTRUCTOR (s AS STRING)
        IF s == NULL
            THROW NullError()
        ENDIF
        SELF:_value    := RuntimeState.WinEncoding:GetBytes(s)

    PRIVATE STATIC METHOD NullError() AS Error
        VAR err			 := Error{ArgumentException{}}
        err:Gencode		 := Gencode.EG_ARG
        err:ArgNum		 := 1
        err:FuncSym		 := "Binary.ctor"
        err:Description  := "Argument cannot be null"
        err:Args         := <OBJECT> {NULL}
        RETURN err

    PRIVATE CONSTRUCTOR( lhs AS BYTE[], rhs AS BYTE[])
        VAR len := lhs:Length + rhs:Length
        VAR result := BYTE[]{len}
        System.Array.Copy(lhs, result, lhs:Length)
        System.Array.Copy(rhs, 0, result,lhs:Length, rhs:Length)
        _value := result


#endregion
#region Properties
    /// <summary>Binary value as array of Bytes</summary>
    PROPERTY @@Value    AS BYTE[]	GET  IIF (_value == NULL, NULL, (BYTE[]) _value:Clone())
    PROPERTY Length     AS LONG GET iif(_value == NULL, 0, _value:Length)
#endregion

#region Equality Operators
    /// <inheritdoc />
    OVERRIDE METHOD Equals(rhs AS OBJECT  ) AS LOGIC
        LOCAL result AS LOGIC
        IF rhs != NULL .AND. rhs IS __Binary
            result := SELF:Equals( (__Binary) rhs)
        ELSE
            result := FALSE
        ENDIF
        RETURN result

    /// <inheritdoc />
    METHOD Equals(rhs AS __Binary ) AS LOGIC
        IF SELF:Length != rhs:Length
            RETURN FALSE
        ENDIF
        FOR VAR i := 0 TO SELF:Length-1
            IF SELF:_value[i] != rhs:_value[i]
                RETURN FALSE
            ENDIF
        NEXT
        RETURN TRUE

    /// <inheritdoc />
    OVERRIDE METHOD GetHashCode() AS INT
        RETURN SELF:_value:GetHashCode()

    /// <exclude />
    METHOD GetTypeCode() AS TypeCode
        RETURN TypeCode.Object


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR ==(lhs AS __Binary, rhs AS __Binary) AS LOGIC
        RETURN lhs:Equals(rhs)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    OPERATOR !=(lhs AS __Binary, rhs AS __Binary) AS LOGIC
        RETURN ! lhs:Equals(rhs)
#endregion

#region Comparison Operators
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR >(lhs AS __Binary, rhs AS __Binary) AS LOGIC
        VAR len := Math.Min(lhs:Length,rhs:Length)
        VAR res := RuntimeState.StringCompare(lhs:_value, rhs:_value, len)
        IF res > 0
            RETURN TRUE
        ELSEIF res < 0
            RETURN FALSE
        ENDIF
        RETURN lhs:Length > rhs:Length


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR <(lhs AS __Binary, rhs AS __Binary) AS LOGIC
        VAR len := Math.Min(lhs:Length,rhs:Length)
        VAR res := RuntimeState.StringCompare(lhs:_value, rhs:_value, len)
        IF res < 0
            RETURN TRUE
        ELSEIF res > 0
            RETURN FALSE
        ENDIF
        RETURN lhs:Length < rhs:Length

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR >=(lhs AS __Binary, rhs AS __Binary) AS LOGIC
        VAR len := Math.Min(lhs:Length,rhs:Length)
        VAR res := RuntimeState.StringCompare(lhs:_value, rhs:_value, len)
        IF res > 0
            RETURN TRUE
        ELSEIF res < 0
            RETURN FALSE
        ENDIF
        RETURN lhs:Length >= rhs:Length

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR <=(lhs AS __Binary, rhs AS __Binary) AS LOGIC
        VAR len := Math.Min(lhs:Length,rhs:Length)
        VAR res := RuntimeState.StringCompare(lhs:_value, rhs:_value, len)
        IF res < 0
            RETURN TRUE
        ELSEIF res > 0
            RETURN FALSE
        ENDIF
        RETURN lhs:Length <= rhs:Length

#endregion

#region Implicit Converters
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(b AS BYTE[]) AS __Binary
        RETURN __Binary{(BYTE[])b:Clone()}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(b AS __Binary) AS BYTE[]
        RETURN b:Value


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(bytes AS __Binary) AS STRING
        RETURN RuntimeState.WinEncoding:GetString(bytes:_value)

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG] [INLINE];
    STATIC OPERATOR IMPLICIT(s AS STRING) AS __Binary
        RETURN __Binary{ s }

#endregion

#region Numeric Operators


    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR+(lhs AS __Binary, rhs AS __Binary) AS __Binary
        RETURN __Binary{lhs:_value, rhs:_value}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR+(lhs AS __Binary, rhs AS STRING) AS __Binary
        RETURN __Binary{lhs:_value, RuntimeState.WinEncoding:GetBytes(rhs)}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    OPERATOR+(lhs AS STRING, rhs AS __Binary) AS STRING
        VAR sb := StringBuilder{}
        sb:Append(lhs)
        sb:Append(RuntimeState.WinEncoding:GetString(rhs))
        RETURN sb:ToString()

#endregion
#region Unary Operators

#endregion
#region Add and Subtract
    /// <exclude />
    METHOD Add(rhs AS __Binary) AS __Binary
        RETURN SELF + rhs

    /// <inheritdoc />
    PUBLIC METHOD CompareTo(rhs AS __Binary) AS INT
        VAR len := Math.Min(SELF:Length,rhs:Length)
        FOR VAR i := 0 TO len-1
            IF SELF:_value[i] > rhs:_value[i]
                RETURN 1
            ELSEIF SELF:_value[i] < rhs:_value[i]
                RETURN -1
            ENDIF
        NEXT
        IF SELF:Length > rhs:Length
            RETURN 1
        ELSEIF SELF:Length < rhs:Length
            RETURN -1
        ENDIF
        RETURN 0
    /// <inheritdoc />
    PUBLIC METHOD CompareTo(rhs AS OBJECT) AS INT
        RETURN SELF:CompareTo( (__Binary) rhs)
#endregion

#region IFormattable
    /// <inheritdoc />
    PUBLIC OVERRIDE METHOD ToString() AS STRING
        RETURN SELF:ToString("")

    /// <inheritdoc cref="System.Double.ToString"/>
    PUBLIC METHOD ToString(sFormat AS STRING) AS STRING
        IF sFormat == "G"
            RETURN RuntimeState.WinEncoding:GetString(SELF:_value)
        ENDIF
        VAR sb := StringBuilder{}
        sb:Append("0h")
        FOREACH VAR b IN SELF:_value
            sb:Append(b:ToString("X2"))
        NEXT
        RETURN sb:ToString()
    /// <inheritdoc />
    PUBLIC METHOD ToString(format AS STRING, provider AS System.IFormatProvider) AS STRING
        RETURN SELF:ToString(format)
#endregion

    INTERNAL METHOD ToDebugString() AS STRING
        IF _value == NULL
            RETURN "<uninitialized>"
        ENDIF
        RETURN SELF:ToString("")

#region ISerializable
    /// <inheritdoc/>

    PUBLIC METHOD GetObjectData(info AS SerializationInfo, context AS StreamingContext) AS VOID
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        info:AddValue("Value", SELF:ToString(""))
        RETURN
    /// <include file="RTComments.xml" path="Comments/SerializeConstructor/*" />
    CONSTRUCTOR (info AS SerializationInfo, context AS StreamingContext)
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        _value := NULL
        VAR s := info:GetString("Value")
        IF s:ToLower():StartsWith("0h")
            _value := BYTE[]{(s:Length -2)/2} // subtract 2 for 0h
            LOCAL current := 0 AS INT
            FOR VAR i := 2 TO s:Length-1 STEP 2
                VAR nibble := s:Substring(i,2)
                _value[current] := Val("0x"+nibble)
                current += 1
            NEXT
        ENDIF
#endregion
        /*
        #region IConvertible
        // forward most methods to the DateTime class so there will
        // be a proper (localized) error message
        /// <inheritdoc />
        METHOD IConvertible.ToBoolean(provider AS System.IFormatProvider) AS LOGIC
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToByte(provider AS System.IFormatProvider) AS BYTE
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToChar(provider AS System.IFormatProvider) AS CHAR
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToDateTime(provider AS System.IFormatProvider) AS System.DateTime
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToDecimal(provider AS System.IFormatProvider) AS DECIMAL
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToDouble(provider AS System.IFormatProvider) AS REAL8
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToInt16(provider AS System.IFormatProvider) AS SHORT
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToInt32(provider AS System.IFormatProvider) AS LONG
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToInt64(provider AS System.IFormatProvider) AS INT64
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToSByte(provider AS System.IFormatProvider) AS SByte
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToSingle(provider AS System.IFormatProvider) AS REAL4
        THROW NotImplementedException{}


        /// <inheritdoc />
        METHOD IConvertible.ToUInt16(provider AS System.IFormatProvider) AS WORD
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToUInt32(provider AS System.IFormatProvider) AS DWORD
        THROW NotImplementedException{}

        /// <inheritdoc />
        METHOD IConvertible.ToUInt64(provider AS System.IFormatProvider) AS UINT64
        THROW NotImplementedException{}

        METHOD IConvertible.ToType(conversionType AS System.Type, provider AS System.IFormatProvider) AS OBJECT
        IF conversionType == TYPEOF(STRING)
        RETURN SELF:ToString("")
        ENDIF
        THROW NotImplementedException{}

        METHOD IConvertible.ToString(provider AS System.IFormatProvider) AS STRING
        RETURN SELF:ToString("")

        #endregion
        */
END STRUCTURE

END NAMESPACE
