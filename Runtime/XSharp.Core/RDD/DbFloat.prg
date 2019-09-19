//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>Implementation of the IFloat interface that can be used by the RDD system. </summary> 
/// <seealso cref="T:XSharp.IFloat"/>
STRUCTURE XSharp.RDD.DbFloat IMPLEMENTS IFLoat, IConvertible
	/// <inheritdoc />
	PROPERTY @@Value	AS REAL8 AUTO GET PRIVATE SET
	/// <inheritdoc />
	PROPERTY Digits		AS INT AUTO GET PRIVATE SET
	/// <inheritdoc />
	PROPERTY Decimals	AS INT AUTO GET PRIVATE SET
    /// <summary></summary>
    CONSTRUCTOR(val AS REAL8, len AS INT, dec AS INT)
		VALUE := val
		Digits := len
		Decimals := dec
	/// <inheritdoc />
	OVERRIDE METHOD ToString() AS STRING
        RETURN SELF:Value:ToString()

	#region IConvertible INTERFACE
	METHOD GetTypeCode() AS TypeCode
		RETURN TypeCode.Object

	METHOD IConvertible.ToBoolean(provider AS IFormatProvider ) AS LOGIC
		IF(	VALUE != 0.0) 
			RETURN TRUE
		ENDIF
		RETURN FALSE

		METHOD IConvertible.ToByte(provider AS IFormatProvider ) AS BYTE
			RETURN Convert.ToByte(VALUE)

		METHOD IConvertible.ToChar(provider AS IFormatProvider ) AS CHAR
			RETURN Convert.ToChar(VALUE)

		METHOD IConvertible.ToDateTime(provider AS IFormatProvider ) AS DateTime
			RETURN Convert.ToDateTime(VALUE)

		METHOD IConvertible.ToDecimal(provider AS IFormatProvider ) AS Decimal
			RETURN Convert.ToDecimal(VALUE)

		METHOD IConvertible.ToDouble(provider AS IFormatProvider ) AS Double
			RETURN VALUE

		METHOD IConvertible.ToInt16(provider AS IFormatProvider ) AS SHORT
			RETURN Convert.ToInt16(VALUE)

		METHOD IConvertible.ToInt32(provider AS IFormatProvider ) AS INT32
			RETURN Convert.ToInt32(VALUE)

		METHOD IConvertible.ToInt64(provider AS IFormatProvider ) AS INT64
			RETURN Convert.ToInt64(VALUE)

		METHOD IConvertible.ToSByte(provider AS IFormatProvider ) AS SByte
			RETURN Convert.ToSByte(VALUE)

		METHOD IConvertible.ToSingle(provider AS IFormatProvider ) AS SINGLE
			RETURN Convert.ToSingle(VALUE)

		METHOD IConvertible.ToString(provider AS IFormatProvider ) AS STRING
			RETURN String.Format("{0}", VALUE)

		METHOD IConvertible.ToType( conversionType AS Type, provider AS IFormatProvider ) AS OBJECT
			RETURN Convert.ChangeType(VALUE,conversionType)

		METHOD IConvertible.ToUInt16(provider AS IFormatProvider ) AS WORD
			RETURN Convert.ToUInt16(VALUE)

		METHOD IConvertible.ToUInt32(provider AS IFormatProvider ) AS DWORD
			RETURN Convert.ToUInt32(VALUE)

		METHOD IConvertible.ToUInt64(provider AS IFormatProvider ) AS UINT64
			RETURN Convert.ToUInt64(VALUE)

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
