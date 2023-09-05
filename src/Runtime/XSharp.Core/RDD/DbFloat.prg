//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <summary>Implementation of the IFloat interface that can be used by the RDD system. </summary>
/// <seealso cref="T:XSharp.IFloat"/>
/// <seealso cref="T:XSharp.__Float"/>
STRUCTURE XSharp.RDD.DbFloat IMPLEMENTS IFloat, IConvertible
    /// <inheritdoc />
    PROPERTY @@Value	AS REAL8 AUTO GET PRIVATE SET
    /// <inheritdoc />
    PROPERTY Digits		AS INT AUTO GET PRIVATE SET
    /// <inheritdoc />
    PROPERTY Decimals	AS INT AUTO GET PRIVATE SET
    /// <summary></summary>
    CONSTRUCTOR(val AS REAL8, len AS INT, dec AS INT)
        @@Value := val
        Digits  := len
        Decimals := dec
    /// <inheritdoc />
    OVERRIDE METHOD ToString() AS STRING
        RETURN SELF:Value:ToString()

#region IConvertible INTERFACE
    /// <inheritdoc/>
    METHOD GetTypeCode() AS TypeCode
        RETURN TypeCode.Object

    /// <inheritdoc/>
    METHOD IConvertible.ToBoolean(provider AS IFormatProvider ) AS LOGIC
        RETURN @@Value != 0.0

    /// <inheritdoc/>
    METHOD IConvertible.ToByte(provider AS IFormatProvider ) AS BYTE
        RETURN Convert.ToByte(@@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToChar(provider AS IFormatProvider ) AS CHAR
        RETURN Convert.ToChar(@@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToDateTime(provider AS IFormatProvider ) AS System.DateTime
        RETURN Convert.ToDateTime(@@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToDecimal(provider AS IFormatProvider ) AS DECIMAL
        RETURN Convert.ToDecimal(@@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToDouble(provider AS IFormatProvider ) AS Double
        RETURN @@Value

    /// <inheritdoc/>
    METHOD IConvertible.ToInt16(provider AS IFormatProvider ) AS SHORT
        RETURN Convert.ToInt16(@@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToInt32(provider AS IFormatProvider ) AS INT
        RETURN Convert.ToInt32(@@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToInt64(provider AS IFormatProvider ) AS INT64
        RETURN Convert.ToInt64(@@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToSByte(provider AS IFormatProvider ) AS SByte
        RETURN Convert.ToSByte(@@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToSingle(provider AS IFormatProvider ) AS REAL4
        RETURN Convert.ToSingle(@@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToString(provider AS IFormatProvider ) AS STRING
        RETURN String.Format("{0}", @@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToType( conversionType AS Type, provider AS IFormatProvider ) AS OBJECT
        RETURN Convert.ChangeType(@@Value,conversionType)

    /// <inheritdoc/>
    METHOD IConvertible.ToUInt16(provider AS IFormatProvider ) AS WORD
        RETURN Convert.ToUInt16(@@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToUInt32(provider AS IFormatProvider ) AS DWORD
        RETURN Convert.ToUInt32(@@Value)

    /// <inheritdoc/>
    METHOD IConvertible.ToUInt64(provider AS IFormatProvider ) AS UINT64
        RETURN Convert.ToUInt64(@@Value)

#endregion
END	STRUCTURE

