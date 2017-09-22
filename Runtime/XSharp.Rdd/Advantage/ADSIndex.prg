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
using AdvantageClientEngine

CLASS XSharp.RDD.ADSIndex INHERIT BaseIndex
    PRIVATE oRDD as ADSRDD

	CONSTRUCTOR(oArea as WorkArea)
		SUPER(oArea)
        oRdd := oArea ASTYPE ADSRDD

    VIRTUAL METHOD OrderCondition(info AS XSharp.RDD.DbOrderCondInfo) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

    VIRTUAL METHOD OrderCreate(info AS XSharp.RDD.DbOrderCreateInfo) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

    VIRTUAL METHOD OrderDestroy(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

    VIRTUAL METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
		THROW NotImplementedException{__ENTITY__}

    VIRTUAL METHOD OrderListAdd(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

    VIRTUAL METHOD OrderListDelete(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

    VIRTUAL METHOD OrderListFocus(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

    VIRTUAL METHOD OrderListRebuild( ) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

    VIRTUAL METHOD Seek(info AS XSharp.RDD.DbSeekInfo) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

END CLASS