//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD
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
	RETURN FALSE
METHOD OrderCreate(info AS XSharp.RDD.DbOrderCreateInfo) AS LOGIC
    RETURN FALSE
METHOD OrderDestroy(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	RETURN FALSE
METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
	RETURN FALSE
METHOD OrderListAdd(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	RETURN FALSE
METHOD OrderListDelete(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	RETURN FALSE
METHOD OrderListFocus(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	RETURN FALSE
METHOD OrderListRebuild( ) AS LOGIC
	RETURN FALSE
METHOD Seek(info AS XSharp.RDD.DbSeekInfo) AS LOGIC		
RETURN FALSE
END CLASS
END CLASS    

END NAMESPACE
