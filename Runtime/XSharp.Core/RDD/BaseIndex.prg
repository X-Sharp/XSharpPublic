//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>Base Index class. Does not implement anything. </summary>
/// <seealso cref="T:XSharp.RDD.IOrder"/>
CLASS XSharp.RDD.BaseIndex IMPLEMENTS IOrder
	PRIVATE _oArea AS WorkArea
	
	/// <summary>Create the BaseIndex object</summary>
	/// <param name="oArea">Workarea object that 'owns' this index object </param>
	
	CONSTRUCTOR(oArea AS WorkArea)
		_oArea := oArea

	/// <inheritdoc />
	VIRTUAL METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
		THROW NotImplementedException{}
		
	/// <inheritdoc />
	VIRTUAL METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC
		THROW NotImplementedException{}
		
	/// <inheritdoc />
	VIRTUAL METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}
		
	/// <inheritdoc />
	VIRTUAL METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT
		THROW NotImplementedException{}
		
	/// <inheritdoc />
	VIRTUAL METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}
		
	/// <inheritdoc />
	VIRTUAL METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}
		
	/// <inheritdoc />
	VIRTUAL METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}
		
	/// <inheritdoc />
	VIRTUAL METHOD OrderListRebuild( ) AS LOGIC
		THROW NotImplementedException{}
		
	/// <inheritdoc />
	VIRTUAL METHOD Seek(info AS DbSeekInfo) AS LOGIC
		THROW NotImplementedException{}
		
	/// <inheritdoc />
	VIRTUAL PROPERTY Found AS LOGIC	GET _oArea:_Found SET _oArea:_Found := VALUE

	/// <inheritdoc />
	VIRTUAL METHOD Flush() 							AS LOGIC
		THROW NotImplementedException{}
	
END CLASS
