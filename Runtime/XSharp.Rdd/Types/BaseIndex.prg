//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


CLASS XSharp.RDD.BaseIndex IMPLEMENTS IOrder
	PRIVATE _oArea AS WorkArea

	CONSTRUCTOR(oArea as WorkArea)
		_oArea := oArea

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

VIRTUAL PROPERTY Found as LOGIC	
GET
	THROW NotImplementedException{__ENTITY__}
END GET
END PROPERTY
END CLASS
