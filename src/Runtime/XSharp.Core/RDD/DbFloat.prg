//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>Implementation of the IFloat interface that can be used by the RDD system. </summary> 
/// <seealso cref="T:XSharp.IFloat"/>
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
	METHOD GetTypeCode() AS TypeCode
		RETURN TypeCode.Object

	METHOD IConvertible.ToBoolean(provider AS IFormatProvider ) AS LOGIC
		IF(	@@Value != 0.0) 
			RETURN TRUE
		ENDIF
		RETURN FALSE

		METHOD IConvertible.ToByte(provider AS IFormatProvider ) AS BYTE
			RETURN Convert.ToByte(@@Value)

		METHOD IConvertible.ToChar(provider AS IFormatProvider ) AS CHAR
			RETURN Convert.ToChar(@@Value)

		METHOD IConvertible.ToDateTime(provider AS IFormatProvider ) AS DateTime
			RETURN Convert.ToDateTime(@@Value)

		METHOD IConvertible.ToDecimal(provider AS IFormatProvider ) AS Decimal
			RETURN Convert.ToDecimal(@@Value)

		METHOD IConvertible.ToDouble(provider AS IFormatProvider ) AS Double
			RETURN @@Value

		METHOD IConvertible.ToInt16(provider AS IFormatProvider ) AS SHORT
			RETURN Convert.ToInt16(@@Value)

		METHOD IConvertible.ToInt32(provider AS IFormatProvider ) AS INT
			RETURN Convert.ToInt32(@@Value)

		METHOD IConvertible.ToInt64(provider AS IFormatProvider ) AS INT64
			RETURN Convert.ToInt64(@@Value)

		METHOD IConvertible.ToSByte(provider AS IFormatProvider ) AS SByte
			RETURN Convert.ToSByte(@@Value)

		METHOD IConvertible.ToSingle(provider AS IFormatProvider ) AS REAL4
			RETURN Convert.ToSingle(@@Value)

		METHOD IConvertible.ToString(provider AS IFormatProvider ) AS STRING
			RETURN String.Format("{0}", @@Value)

		METHOD IConvertible.ToType( conversionType AS Type, provider AS IFormatProvider ) AS OBJECT
			RETURN Convert.ChangeType(@@Value,conversionType)

		METHOD IConvertible.ToUInt16(provider AS IFormatProvider ) AS WORD
			RETURN Convert.ToUInt16(@@Value)

		METHOD IConvertible.ToUInt32(provider AS IFormatProvider ) AS DWORD
			RETURN Convert.ToUInt32(@@Value)

		METHOD IConvertible.ToUInt64(provider AS IFormatProvider ) AS UINT64
			RETURN Convert.ToUInt64(@@Value)

	#endregion
END	STRUCTURE
		
/// <summary>Implementation of the IDate interface that can be used by the RDD system. </summary> 
/// <seealso cref="T:XSharp.IDate"/>
STRUCTURE XSharp.RDD.DbDate IMPLEMENTS IDate
	/// <inheritdoc />
	PROPERTY Year		AS INT AUTO GET PRIVATE SET
	/// <inheritdoc />
	PROPERTY Month		AS INT AUTO GET PRIVATE SET
	/// <inheritdoc />
	PROPERTY Day		AS INT AUTO GET PRIVATE SET
	/// <inheritdoc />
	PROPERTY @@Value		AS DateTime GET DateTime{Year, Month, Day}
	/// <inheritdoc />
	PROPERTY IsEmpty	AS LOGIC GET Month == 0
    /// <summary></summary>
	CONSTRUCTOR(nYear AS INT, nMonth AS INT, nDay AS INT)
		Year	:= nYear
		Month   := nMonth
		Day     := nDay
		RETURN
	/// <inheritdoc />
	OVERRIDE METHOD ToString() AS STRING
        IF IsEmpty
            RETURN "    -  -  "
        ENDIF
        RETURN SELF:Value:ToString("yyyy-MM-dd")
END STRUCTURE
