//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Support
/// <include file="XSharp.Core.Docs.xml" path="doc/IOrder/*" />
INTERFACE XSharp.RDD.IOrder
 /// <include file="XSharp.Core.Docs.xml" path="doc/IOrder.OrderCondition/*" />
	METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
	
    /// <include file="XSharp.Core.Docs.xml" path="doc/IOrder.OrderCreate/*" />
    METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC	
	
 /// <include file="XSharp.Core.Docs.xml" path="doc/IOrder.OrderDestroy/*" />
	METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC    	

 /// <include file="XSharp.Core.Docs.xml" path="doc/IOrder.OrderInfo/*" />
	METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT

 /// <include file="XSharp.Core.Docs.xml" path="doc/IOrder.OrderListAdd/*" />
	METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IOrder.OrderListDelete/*" />
	METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IOrder.OrderListFocus/*" />
	METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IOrder.OrderListRebuild/*" />
	METHOD OrderListRebuild() AS LOGIC 

 /// <include file="XSharp.Core.Docs.xml" path="doc/IOrder.Seek/*" />
	METHOD Seek(info AS DbSeekInfo) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IOrder.Found/*" />
	PROPERTY Found	AS LOGIC GET SET

 /// <include file="XSharp.Core.Docs.xml" path="doc/IOrder.Flush/*" />
	METHOD Flush() 	AS LOGIC

END INTERFACE	





