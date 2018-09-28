//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#ifdef COMPLETED
USING XSharp.RDD.Support
BEGIN NAMESPACE XSharp.RDD

/// <summary>DBFFPT RDD. For DBF/NTX/DBV.</summary>
CLASS DBFMEMO INHERIT DbfNtx

/// <inheritdoc />
VIRTUAL METHOD GetValue(nFldPos AS INT) AS OBJECT
	THROW NotImplementedException{}

/// <inheritdoc />
VIRTUAL METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	THROW NotImplementedException{}

/// <inheritdoc />
VIRTUAL METHOD GetValueLength(nFldPos AS INT) AS INT
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
VIRTUAL METHOD OpenMemFile( info AS DbOpenInfo) AS LOGIC
	THROW NotImplementedException{}

/// <inheritdoc />
VIRTUAL PROPERTY SysName AS STRING GET typeof(DbfMemo):ToString()

END CLASS
END NAMESPACE
#endif
