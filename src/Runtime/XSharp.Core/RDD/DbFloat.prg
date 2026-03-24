//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat/*" />
STRUCTURE XSharp.RDD.DbFloat IMPLEMENTS IFloat, IConvertible
    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.Value/*" />
    PROPERTY @@Value	AS REAL8 AUTO GET PRIVATE SET
    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.Digits/*" />
    PROPERTY Digits		AS INT AUTO GET PRIVATE SET
    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.Decimals/*" />
    PROPERTY Decimals	AS INT AUTO GET PRIVATE SET
    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.ctor/*" />
    CONSTRUCTOR(val AS REAL8, len AS INT, dec AS INT)
        @@Value := val
        Digits  := len
        Decimals := dec
    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.ToString/*" />
    OVERRIDE METHOD ToString() AS STRING
        RETURN SELF:Value:ToString()

#region IConvertible INTERFACE
    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.GetTypeCode/*" />
    METHOD GetTypeCode() AS TypeCode
        RETURN TypeCode.Object

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToBoolean/*" />
    METHOD IConvertible.ToBoolean(provider AS IFormatProvider ) AS LOGIC
        RETURN @@Value != 0.0

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToByte/*" />
    METHOD IConvertible.ToByte(provider AS IFormatProvider ) AS BYTE
        RETURN Convert.ToByte(@@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToChar/*" />
    METHOD IConvertible.ToChar(provider AS IFormatProvider ) AS CHAR
        RETURN Convert.ToChar(@@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToDateTime/*" />
    METHOD IConvertible.ToDateTime(provider AS IFormatProvider ) AS System.DateTime
        RETURN Convert.ToDateTime(@@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToDecimal/*" />
    METHOD IConvertible.ToDecimal(provider AS IFormatProvider ) AS DECIMAL
        RETURN Convert.ToDecimal(@@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToDouble/*" />
    METHOD IConvertible.ToDouble(provider AS IFormatProvider ) AS Double
        RETURN @@Value

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToInt16/*" />
    METHOD IConvertible.ToInt16(provider AS IFormatProvider ) AS SHORT
        RETURN Convert.ToInt16(@@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToInt32/*" />
    METHOD IConvertible.ToInt32(provider AS IFormatProvider ) AS INT
        RETURN Convert.ToInt32(@@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToInt64/*" />
    METHOD IConvertible.ToInt64(provider AS IFormatProvider ) AS INT64
        RETURN Convert.ToInt64(@@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToSByte/*" />
    METHOD IConvertible.ToSByte(provider AS IFormatProvider ) AS SByte
        RETURN Convert.ToSByte(@@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToSingle/*" />
    METHOD IConvertible.ToSingle(provider AS IFormatProvider ) AS REAL4
        RETURN Convert.ToSingle(@@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToString/*" />
    METHOD IConvertible.ToString(provider AS IFormatProvider ) AS STRING
        RETURN String.Format("{0}", @@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToType/*" />
    METHOD IConvertible.ToType( conversionType AS Type, provider AS IFormatProvider ) AS OBJECT
        RETURN Convert.ChangeType(@@Value,conversionType)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToUInt16/*" />
    METHOD IConvertible.ToUInt16(provider AS IFormatProvider ) AS WORD
        RETURN Convert.ToUInt16(@@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToUInt32/*" />
    METHOD IConvertible.ToUInt32(provider AS IFormatProvider ) AS DWORD
        RETURN Convert.ToUInt32(@@Value)

    /// <include file="XSharp.CoreDocs.xml" path="doc/DbFloat.IConvertible.ToUInt64/*" />
    METHOD IConvertible.ToUInt64(provider AS IFormatProvider ) AS UINT64
        RETURN Convert.ToUInt64(@@Value)

#endregion
END	STRUCTURE

