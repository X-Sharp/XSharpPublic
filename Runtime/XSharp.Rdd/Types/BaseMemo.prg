//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

CLASS XSharp.RDD.BaseMemo Implements IMemo
	PRIVATE _oArea AS WorkArea

	CONSTRUCTOR(oArea as WorkArea)
		_oArea := oArea

	// Read & Write		
	VIRTUAL METHOD GetValue(nFldPos AS INT) AS OBJECT
		THROW NotImplementedException{__ENTITY__}

	VIRTUAL METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

	VIRTUAL METHOD GetValueLength(nFldPos AS INT) AS INT
		THROW NotImplementedException{__ENTITY__}

	VIRTUAL METHOD Flush() 			AS LOGIC
		THROW NotImplementedException{__ENTITY__}

	VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

	VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

	// Memo File Access 
	VIRTUAL METHOD CloseMemFile( ) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

	VIRTUAL METHOD CreateMemFile(info AS XSharp.RDD.DbOpenInfo) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

	VIRTUAL METHOD OpenMemFile( ) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

END CLASS
