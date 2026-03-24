//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="XSharp.CoreDocs.xml" path="doc/DbDate/*" />
STRUCTURE XSharp.RDD.DbDate IMPLEMENTS IDate
	/// <include file="XSharp.CoreDocs.xml" path="doc/DbDate.Year/*" />
	PROPERTY Year		AS INT AUTO GET PRIVATE SET
	/// <include file="XSharp.CoreDocs.xml" path="doc/DbDate.Month/*" />
	PROPERTY Month		AS INT AUTO GET PRIVATE SET
	/// <include file="XSharp.CoreDocs.xml" path="doc/DbDate.Day/*" />
	PROPERTY Day		AS INT AUTO GET PRIVATE SET
	/// <include file="XSharp.CoreDocs.xml" path="doc/DbDate.Value/*" />
	PROPERTY @@Value		AS DateTime GET DateTime{Year, Month, Day}
	/// <include file="XSharp.CoreDocs.xml" path="doc/DbDate.IsEmpty/*" />
	PROPERTY IsEmpty	AS LOGIC GET Month == 0
    /// <include file="XSharp.CoreDocs.xml" path="doc/DbDate.ctor/*" />
	CONSTRUCTOR(nYear AS INT, nMonth AS INT, nDay AS INT)
		Year	:= nYear
		Month   := nMonth
		Day     := nDay
		RETURN
	/// <include file="XSharp.CoreDocs.xml" path="doc/DbDate.ToString/*" />
	OVERRIDE METHOD ToString() AS STRING
        IF IsEmpty
            RETURN "    -  -  "
        ENDIF
        RETURN SELF:Value:ToString("yyyy-MM-dd")
END STRUCTURE

