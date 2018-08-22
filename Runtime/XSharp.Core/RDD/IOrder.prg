//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>This interface defines the mimimum methods that a RDD that supports indexes should implement.</summary>	
INTERFACE XSharp.RDD.IOrder
	/// <summary>Set the condition for the next Index Creation</summary>
    /// <param name="info">An object containing information for the order condition.</param>
	METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
	
	/// <summary>Create a new index or tag.</summary>
	/// <param name="info">An object containing information for order creation.</param>
    METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC	
	
	/// <summary>Delete an index or tag.</summary>
	/// <param name="info">An object containing information about the order to remove.</param>
	METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC    	

	/// <summary>Retrieve information about the current index.</summary>
	/// <param name="nOrdinal"></param>
	METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT

	/// <summary>Open an index file and add to the list of open indexes for the current workarea.</summary>
	/// <param name="info">An object containing information about the orderlist (file)  to add.</param>
	METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC

	/// <summary>Close an index file and remove it from the list of open indexes for the current workarea.</summary>
	/// <param name="info"></param>
	/// <param name="info">An object containing information about the orderlist (file) to delete.</param>
	METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC

	/// <summary>Set focus to another index in the list open indexes for the current workarea.</summary>
	/// <param name="info">An object containing information about the order to select.</param>
	METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC

	/// <summary>Rebuild all indexes for the current workarea.</summary>
	METHOD OrderListRebuild() AS LOGIC 

	/// <summary>Perform a seek operation on the current selected index for the current workarea.</summary>
	/// <param name="info">An object containing containing the necessary seek information.</param>
	METHOD Seek(info AS DbSeekInfo) AS LOGIC

	/// <summary>Result of the last seek operation for the current workarea.</summary>
	PROPERTY Found	AS LOGIC GET SET

	/// <summary>Flush the changes to the index.</summary>	
	METHOD Flush() 	AS LOGIC

END INTERFACE	





