//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

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
METHOD OrderCondition(info AS XSharp.RDD.DbOrderCondInfo) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderCreate(info AS XSharp.RDD.DbOrderCreateInfo) AS LOGIC
    THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderDestroy(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderListAdd(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderListDelete(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderListFocus(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD OrderListRebuild( ) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD Seek(info AS XSharp.RDD.DbSeekInfo) AS LOGIC		
    THROW NotImplementedException{}
/// <inheritdoc />
VIRTUAL PROPERTY Found as LOGIC	
GET
	THROW NotImplementedException{}
END GET
END PROPERTY

END CLASS

END CLASS    

END NAMESPACE
