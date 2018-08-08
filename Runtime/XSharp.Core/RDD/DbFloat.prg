//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>Implementation of the IFloat interface that can be used by the RDD system. </summary> 
/// <seealso cref="T:XSharp.IFloat"/>
STRUCTURE XSharp.RDD.DbFloat IMPLEMENTS IFLoat
	/// <inheritdoc />
	PROPERTY Value	AS REAL8 AUTO GET PRIVATE SET
	/// <inheritdoc />
	PROPERTY Digits		AS INT AUTO GET PRIVATE SET
	/// <inheritdoc />
	PROPERTY Decimals	AS INT AUTO GET PRIVATE SET
	CONSTRUCTOR(val AS REAL8, len AS INT, dec AS INT)
		VALUE := val
		Digits := len
		Decimals := dec
		
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
	PROPERTY Value		AS DateTime GET DateTime{Year, Month, Day}
	/// <inheritdoc />
	PROPERTY IsEmpty	AS LOGIC GET Month != 0
	CONSTRUCTOR(nYear AS INT, nMonth AS INT, nDay AS INT)
		Year	:= nYear
		Month   := nMonth
		Day     := nDay
		RETURN
		
END STRUCTURE
