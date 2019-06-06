//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#ifdef COMPLETED
USING XSharp.RDD.Support
BEGIN NAMESPACE XSharp.RDD

/// <summary>DBFSMT RDD. For DBF/SMT. No index support at this level</summary>
CLASS DBFSMT INHERIT DBF
	CONSTRUCTOR
	SUPER()
	SELF:_oMemo := SMTMemo{SELF}

VIRTUAL PROPERTY Driver AS STRING GET "DBFSMT"


/// <summary>SMT Memo class. Implements the SMT support.</summary>
CLASS SMTMemo INHERIT BaseMemo  IMPLEMENTS IMemo
	PROTECT _oRDD AS DBF
/// <inheritdoc />
	CONSTRUCTOR (oRDD AS DBF)
		SUPER(oRDD)
		SELF:_oRdd := oRDD

/// <inheritdoc />
METHOD Flush() 			AS LOGIC		
	THROW NotImplementedException{}

/// <inheritdoc />
METHOD GetValue(nFldPos AS INT) AS OBJECT
	THROW NotImplementedException{}

/// <inheritdoc />
METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	THROW NotImplementedException{}

/// <inheritdoc />
METHOD GetValueLength(nFldPos AS INT) AS INT
	THROW NotImplementedException{}

/// <inheritdoc />
VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
	THROW NotImplementedException{}

/// <inheritdoc />
VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
VIRTUAL METHOD CloseMemFile( ) AS LOGIC
	THROW NotImplementedException{}

/// <inheritdoc />
VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
	THROW NotImplementedException{}

/// <inheritdoc />
VIRTUAL METHOD OpenMemFile( ) AS LOGIC
	THROW NotImplementedException{}
END CLASS 

END CLASS
END NAMESPACE
#endif
