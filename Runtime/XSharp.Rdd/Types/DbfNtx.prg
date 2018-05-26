//
// Copyright (c) B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD
/// <summary>DBFNTX RDD. For DBF/DBT/NTX.</summary>
CLASS DBFNTX INHERIT DBFDBT
	CONSTRUCTOR()
		SUPER()
		SELF:_oIndex := NtxIndex{SELF}
		RETURN

	PROPERTY SysName AS STRING GET typeof(DbfNtx):ToString()	

/// <summary>DBFNTX worker class that implements the NTX support.</summary>
CLASS NtxIndex INHERIT BaseIndex    
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
METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
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
VIRTUAL PROPERTY Found as LOGIC	
GET
	THROW NotImplementedException{}
END GET
END PROPERTY

END CLASS
END CLASS

END NAMESPACE
