//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// BLOB.PRG	Weakly typed BLOB functions

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobdirectexport/*" />
FUNCTION BLOBDirectExport(nPointer, cTargetFile, kMode) AS LOGIC CLIPPER
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Info( BLOB_DIRECT_EXPORT, <OBJECT>{ nPointer, cTargetFile, kMode } ))
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobdirectget/*" />
FUNCTION BLOBDirectGet(nPointer, nStart, nCount) AS USUAL CLIPPER
	RETURN VoDb.Info( BLOB_DIRECT_GET, <OBJECT>{nPointer, nStart, nCount} )
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobdirectimport/*" />
FUNCTION BLOBDirectImport(nOldPointer, cSourceFile) AS USUAL CLIPPER
	RETURN VoDb.Info( BLOB_DIRECT_IMPORT, <OBJECT>{nOldPointer, cSourceFile} )

		
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobdirectput/*" />
FUNCTION BLOBDirectPut(nOldPointer, uBLOB) AS USUAL CLIPPER
	RETURN VoDb.Info( BLOB_DIRECT_PUT, <OBJECT>{nOldPointer, uBlob} )
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobexport/*" />
FUNCTION BLOBExport (nFieldPos, cTargetFile, kMode)  AS LOGIC CLIPPER
	DbInfo( BLOB_NMODE, kMode )
  	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FileGet( nfieldPos, cTargetFile ))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobget/*" />
FUNCTION BLOBGet(nFieldPos, nStart, nCount)  AS USUAL CLIPPER
	RETURN VoDb.Info( BLOB_GET, <OBJECT>{nFieldPos, nStart, nCount} )
	
	

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobimport/*" />
FUNCTION BLOBImport (nFieldPos, cSourceFile)  AS LOGIC CLIPPER
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FilePut( nFieldPos, cSourceFile ) )
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobrootget/*" />
FUNCTION BLOBRootGet() AS USUAL STRICT
	RETURN VoDb.Info( BLOB_ROOT_GET , NIL)
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobrootlock/*" />
FUNCTION BLOBRootLock() AS USUAL STRICT
	RETURN VoDb.Info( BLOB_ROOT_LOCK, NIL )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobrootput/*" />
FUNCTION BLOBRootPut(uBLOB) AS USUAL CLIPPER
	RETURN VoDb.Info( BLOB_ROOT_PUT, uBLOB )
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobrootunlock/*" />
FUNCTION BLOBRootUnlock()  AS USUAL STRICT
	RETURN VoDb.Info( BLOB_ROOT_UNLOCK, NIL )
	

FUNCTION DbBlobInfo(nOrdinal, nPos, xNewVal) AS USUAL CLIPPER
	_DbThrowErrorOnFailure(__FUNCTION__, VoDb.BlobInfo(nOrdinal, nPos, REF xNewVal))
	RETURN xNewVal
