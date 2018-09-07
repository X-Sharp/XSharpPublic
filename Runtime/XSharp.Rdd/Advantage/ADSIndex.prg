//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp
USING XSharp.RDD

/// <summary>Advantage Index support.</summary>
CLASS XSharp.ADS.ADSIndex INHERIT BaseIndex
    PRIVATE oRDD AS ADSRDD
	/// <inheritdoc />
	CONSTRUCTOR(oArea AS WorkArea)
		SUPER(oArea)
        oRdd := oArea ASTYPE ADSRDD
	/// <inheritdoc />
    VIRTUAL METHOD OrderCondition(info AS XSharp.RDD.DbOrderCondInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
    VIRTUAL METHOD OrderCreate(info AS XSharp.RDD.DbOrderCreateInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
    VIRTUAL METHOD OrderDestroy(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
    VIRTUAL METHOD OrderInfo(nOrdinal AS DWORD) AS OBJECT
		THROW NotImplementedException{}

	/// <inheritdoc />
    VIRTUAL METHOD OrderListAdd(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
    VIRTUAL METHOD OrderListDelete(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
    VIRTUAL METHOD OrderListFocus(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
    VIRTUAL METHOD OrderListRebuild( ) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
    VIRTUAL METHOD Seek(info AS XSharp.RDD.DbSeekInfo) AS LOGIC
		THROW NotImplementedException{}

END CLASS
