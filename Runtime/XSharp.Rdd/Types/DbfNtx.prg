//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
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
		
METHOD OrderCondition(info AS XSharp.RDD.DbOrderCondInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}
METHOD OrderCreate(info AS XSharp.RDD.DbOrderCreateInfo) AS LOGIC
    THROW NotImplementedException{__ENTITY__}
METHOD OrderDestroy(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}
METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
	THROW NotImplementedException{__ENTITY__}
METHOD OrderListAdd(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}
METHOD OrderListDelete(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}
METHOD OrderListFocus(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}
METHOD OrderListRebuild( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}
METHOD Seek(info AS XSharp.RDD.DbSeekInfo) AS LOGIC		
    THROW NotImplementedException{__ENTITY__}
VIRTUAL PROPERTY Found as LOGIC	
GET
	THROW NotImplementedException{__ENTITY__}
END GET
END PROPERTY

END CLASS
END CLASS

END NAMESPACE
