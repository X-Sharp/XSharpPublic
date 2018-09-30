//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#ifdef COMPLETED
USING XSharp.RDD.Support
BEGIN NAMESPACE XSharp.RDD
// Inherits all standard DBF and Memo behavior
// Only adds Order Handling
/// <summary>DBFCDX RDD. For DBF/FPT/CDX.</summary>
CLASS DBFCDX INHERIT DBFFPT
	CONSTRUCTOR()
		SUPER()
		SELF:_oIndex := CdxIndex{SELF}
		RETURN
		
	PROPERTY SysName AS STRING GET typeof(DBFCDX):ToString()

/// <summary>DBFCDX worker class that implements the CDX support.</summary>
CLASS CdxIndex INHERIT BaseIndex    
	PROTECT _oRDD AS DBF
/// <inheritdoc />
CONSTRUCTOR(oRDD AS DBF)
	SUPER(oRDD)   
	_oRDD := oRDD
/// <inheritdoc />		
METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC
    THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderInfo(nOrdinal AS DWORD) AS OBJECT
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderListRebuild( ) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD Seek(info AS DbSeekInfo) AS LOGIC		
    THROW NotImplementedException{}
/// <inheritdoc />
VIRTUAL PROPERTY Found AS LOGIC	
GET
	THROW NotImplementedException{}
END GET
END PROPERTY

END CLASS

END CLASS    

END NAMESPACE
#endif
