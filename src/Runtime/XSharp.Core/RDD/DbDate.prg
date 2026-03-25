//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="XSharp.Core.Docs.xml" path="doc/DbDate/*" />
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
 /// <include file="XSharp.Core.Docs.xml" path="doc/DbDate.ctor/*" />
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

