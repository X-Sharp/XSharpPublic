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


CLASS XSharp.Ads.ADSIndex INHERIT BaseIndex
    PRIVATE oRDD as ADSRDD

	CONSTRUCTOR(oArea as WorkArea)
		SUPER(oArea)
        oRdd := oArea ASTYPE ADSRDD

    VIRTUAL METHOD OrderCondition(info AS XSharp.DbOrderCondInfo) AS LOGIC
		THROW NotImplementedException{}

    VIRTUAL METHOD OrderCreate(info AS XSharp.DbOrderCreateInfo) AS LOGIC
		THROW NotImplementedException{}

    VIRTUAL METHOD OrderDestroy(info AS XSharp.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

    VIRTUAL METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
		THROW NotImplementedException{}

    VIRTUAL METHOD OrderListAdd(info AS XSharp.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

    VIRTUAL METHOD OrderListDelete(info AS XSharp.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

    VIRTUAL METHOD OrderListFocus(info AS XSharp.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

    VIRTUAL METHOD OrderListRebuild( ) AS LOGIC
		THROW NotImplementedException{}

    VIRTUAL METHOD Seek(info AS XSharp.DbSeekInfo) AS LOGIC
		THROW NotImplementedException{}

END CLASS