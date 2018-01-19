//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


CLASS XSharp.RDD.BaseIndex IMPLEMENTS IOrder
	PRIVATE _oArea AS WorkArea

	CONSTRUCTOR(oArea AS WorkArea)
		_oArea := oArea

VIRTUAL METHOD OrderCondition(info AS XSharp.RDD.DbOrderCondInfo) AS LOGIC
		THROW NotImplementedException{}
VIRTUAL METHOD OrderCreate(info AS XSharp.RDD.DbOrderCreateInfo) AS LOGIC
		THROW NotImplementedException{}

VIRTUAL METHOD OrderDestroy(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

VIRTUAL METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
		THROW NotImplementedException{}

VIRTUAL METHOD OrderListAdd(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

VIRTUAL METHOD OrderListDelete(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

VIRTUAL METHOD OrderListFocus(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

VIRTUAL METHOD OrderListRebuild( ) AS LOGIC
		THROW NotImplementedException{}

VIRTUAL METHOD Seek(info AS XSharp.RDD.DbSeekInfo) AS LOGIC
		THROW NotImplementedException{}

VIRTUAL PROPERTY Found AS LOGIC	GET _oArea:_Found SET _oArea:_Found := VALUE
END CLASS
