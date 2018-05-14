//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>This interface defines the mimimum methods that a RDD that supports indexes should implement.</summary>	
INTERFACE XSharp.RDD.IOrder
	/// <summary>Set the condition for the next Index Creation</summary>
    METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
	/// <summary>Create a new index or tag.</summary>
    METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC	
	/// <summary>Delete an index or tag.</summary>
	METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC    	
	/// <summary>Retrieve information about the current index.</summary>
	METHOD OrderInfo(nOrdinal AS LONG) AS OBJECT
	/// <summary>Open an index file and add to the list of open indexes for the current workarea.</summary>
	METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
	/// <summary>Close an index file and remove it from the list of open indexes for the current workarea.</summary>
	METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
	/// <summary>Set focus to another index in the list open indexes for the current workarea.</summary>
	METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
	/// <summary>Rebuild all indexes for the current workarea.</summary>
	METHOD OrderListRebuild() AS LOGIC 
	/// <summary>Perform a seek operation on the current selected index for the current workarea.</summary>
	METHOD Seek(info AS DbSeekInfo) AS LOGIC
	/// <summary>Result of the last seek operation for the current workarea.</summary>
	PROPERTY Found	AS LOGIC GET SET
	/// <summary>Flush the changes to the index.</summary>	
	METHOD Flush() 	AS LOGIC

END INTERFACE	





