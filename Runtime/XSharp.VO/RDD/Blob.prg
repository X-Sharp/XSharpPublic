//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// BLOB.PRG	Weakly typed BLOB functions

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBDirectExport(nPointer, cTargetFile, nMode) AS LOGIC CLIPPER
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Info( BLOB_DIRECT_EXPORT, <OBJECT>{ nPointer, cTargetFile, nMode } ))
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBDirectGet(nPointer, nStart, nCount) AS USUAL CLIPPER
	RETURN VoDb.Info( BLOB_DIRECT_GET, <OBJECT>{nPointer, nStart, nCount} )
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBDirectImport(nOldPointer, cSourceFile) AS USUAL CLIPPER
	RETURN VoDb.Info( BLOB_DIRECT_IMPORT, <OBJECT>{nOldPointer, cSourceFile} )

		
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBDirectPut(nOldPointer, uBLOB) AS USUAL CLIPPER
	RETURN VoDb.Info( BLOB_DIRECT_PUT, <OBJECT>{nOldPointer, uBlob} )
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBExport (nFieldPos, cFileName, nMode)  AS LOGIC CLIPPER
	DbInfo( BLOB_NMODE, nMode )
  	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FileGet( nfieldPos, cFileName ))

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBGet(nFieldNo, nStart, nLen)  AS USUAL CLIPPER
	RETURN VoDb.Info( BLOB_GET, <OBJECT>{nFieldNo, nStart, nLen} )
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBImport (nFieldPos, cFileName)  AS LOGIC CLIPPER
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FilePut( nFieldPos, cFileName ) )
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBRootGet() AS USUAL STRICT
	RETURN VoDb.Info( BLOB_ROOT_GET , NIL)
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBRootLock() AS USUAL STRICT
	RETURN VoDb.Info( BLOB_ROOT_LOCK, NIL )

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBRootPut(xblob) AS USUAL CLIPPER
	RETURN VoDb.Info( BLOB_ROOT_PUT, xBlob )
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BLOBRootUnlock()  AS USUAL STRICT
	RETURN VoDb.Info( BLOB_ROOT_UNLOCK, NIL )
	

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbBlobInfo(nOrdinal, nPos, xNewVal) AS USUAL CLIPPER
	_DbThrowErrorOnFailure(__FUNCTION__, VoDb.BlobInfo(nOrdinal, nPos, REF xNewVal))
	RETURN xNewVal
