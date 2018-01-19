//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

INTERFACE XSharp.RDD.IOrder

	// Indexes
    METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
    METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC	
	METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC    	
	METHOD OrderInfo(nOrdinal AS LONG) AS OBJECT
	METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
	METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
	METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
	METHOD OrderListRebuild() AS LOGIC 
	METHOD Seek(info AS DbSeekInfo) AS LOGIC
	PROPERTY Found AS LOGIC GET SET
END INTERFACE	





