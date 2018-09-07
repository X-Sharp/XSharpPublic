//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// BLOB.PRG	Weakly typed BLOB functions
#ifdef COMPILEIT	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBDirectExport(nPointer, cTargetFile, nMode) AS LOGIC
	LOCAL       lRet AS LOGIC
	lRet := DbInfo( BLOB_DIRECT_EXPORT, <OBJECT>{ nPointer, cTargetFile, nMode } )
	IF !lRet
		DoError(#BLOBDirectExport)
	ENDIF
	RETURN lRet
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBDirectGet(nPointer, nStart, nCount) AS USUAL
	RETURN DbInfo( BLOB_DIRECT_GET, <OBJECT>{nPointer, nStart, nCount} )
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBDirectImport(nOldPointer, cSourceFile) AS USUAL
	RETURN DbInfo( BLOB_DIRECT_IMPORT, <OBJECT>{nOldPointer, cSourceFile} )

		
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBDirectPut(nOldPointer, uBLOB) AS USUAL
	RETURN DbInfo( BLOB_DIRECT_PUT, <OBJECT>{nOldPointer, uBlob} )
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBExport (nFieldPos, cFileName, nMode)  AS LOGIC
	LOCAL lRet          AS LOGIC
	DbInfo( BLOB_NMODE, nMode )
	lRet := VODBFileGet( nfieldPos, cFileName )
	IF !lRet
		DoError(#BLOBExport)
	ENDIF
	RETURN lRet


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBGet(nFieldNo, nStart, nLen)  AS USUAL
	RETURN DbInfo( BLOB_GET, <OBJECT>{nFieldNo, nStart, nLen} )
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBImport (nFieldPos, cFileName)  AS LOGIC
	LOCAL lRet      AS LOGIC
	lRet := VODBFilePut( nFieldPos, cFileName )
	IF !lRet
		DoError(#BLOBImport)
	ENDIF
	RETURN lRet
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBRootGet() AS USUAL
	RETURN DbInfo( BLOB_ROOT_GET )
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBRootLock() AS USUAL
	RETURN DbInfo( BLOB_ROOT_LOCK )

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBRootPut(xblob) AS USUAL
	RETURN DbInfo( BLOB_ROOT_PUT, xBlob )
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBRootUnlock()  AS USUAL
	
	RETURN DbInfo( BLOB_ROOT_UNLOCK )
	

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBBLOBINFO(nOrdinal, nPos, xNewVal)
	IF !VODBBlobInfo(nOrdinal, nPos, @xNewVal)
		DoError(#DBBLOBINFO)
	ENDIF
	RETURN xNewVal
#endif