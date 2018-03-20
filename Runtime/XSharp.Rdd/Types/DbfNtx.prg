//
// Copyright (c) B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD
CLASS DBFNTX INHERIT DBFDBT
	CONSTRUCTOR()
		SUPER()
		SELF:_oIndex := NtxIndex{SELF}
		RETURN

	PROPERTY SysName AS STRING GET typeof(DbfNtx):ToString()	

CLASS NtxIndex INHERIT BaseIndex    
	PROTECT _oRDD AS DBF
	CONSTRUCTOR(oRDD AS DBF)
		SUPER(oRDD)   
		_oRDD := oRDD
		
METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC
    THROW NotImplementedException{}
METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
	THROW NotImplementedException{}
METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
	THROW NotImplementedException{}
METHOD OrderListRebuild( ) AS LOGIC
	THROW NotImplementedException{}
METHOD Seek(info AS DbSeekInfo) AS LOGIC		
    THROW NotImplementedException{}
VIRTUAL PROPERTY Found as LOGIC	
GET
	THROW NotImplementedException{}
END GET
END PROPERTY

END CLASS
END CLASS

END NAMESPACE
