//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD
// Inherits all standard DBF and Memo behavior
// Only adds Order Handling
CLASS DBFCDX INHERIT DBFFPT
	CONSTRUCTOR()
		SUPER()
		SELF:_oIndex := CdxIndex{SELF}
		RETURN
		
	PROPERTY SysName AS STRING GET typeof(DBFCDX):ToString()

CLASS CdxIndex INHERIT BaseIndex    
	PROTECT _oRDD AS DBF
CONSTRUCTOR(oRDD AS DBF)
	SUPER(oRDD)   
	_oRDD := oRDD
		
METHOD OrderCondition(info AS XSharp.RDD.DbOrderCondInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderCreate(info AS XSharp.RDD.DbOrderCreateInfo) AS LOGIC
    THROW NotImplementedException{}
METHOD OrderDestroy(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
	THROW NotImplementedException{}
METHOD OrderListAdd(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderListDelete(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderListFocus(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderListRebuild( ) AS LOGIC
	THROW NotImplementedException{}
METHOD Seek(info AS XSharp.RDD.DbSeekInfo) AS LOGIC		
    THROW NotImplementedException{}
VIRTUAL PROPERTY Found as LOGIC	
GET
	THROW NotImplementedException{}
END GET
END PROPERTY

END CLASS

END CLASS    

END NAMESPACE
