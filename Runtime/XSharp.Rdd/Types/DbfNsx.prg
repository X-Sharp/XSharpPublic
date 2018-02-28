//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD
CLASS DBFNSX INHERIT DBFSMT
	CONSTRUCTOR()
		SUPER()
		SELF:_oIndex := NsxIndex{SELF}
		RETURN
		
	PROPERTY SysName AS STRING GET typeof(DbfNsx):ToString()

CLASS NsxIndex INHERIT BaseIndex    
	PROTECT _oRDD AS DBF
	CONSTRUCTOR(oRDD AS DBF)
		SUPER(oRDD)   
		_oRDD := oRDD
		
METHOD OrderCondition(info AS XSharp.DbOrderCondInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderCreate(info AS XSharp.DbOrderCreateInfo) AS LOGIC
    THROW NotImplementedException{}
METHOD OrderDestroy(info AS XSharp.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
	THROW NotImplementedException{}
METHOD OrderListAdd(info AS XSharp.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderListDelete(info AS XSharp.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderListFocus(info AS XSharp.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderListRebuild( ) AS LOGIC
	THROW NotImplementedException{}
METHOD Seek(info AS XSharp.DbSeekInfo) AS LOGIC		
    THROW NotImplementedException{}
VIRTUAL PROPERTY Found AS LOGIC	
GET
	THROW NotImplementedException{}
END GET
END PROPERTY

END CLASS    
END CLASS    

END NAMESPACE
