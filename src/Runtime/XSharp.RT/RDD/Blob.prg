//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// BLOB.PRG	Weakly typed BLOB functions

/// <summary>
/// This class is used to write BLOB data to a FPT file.
/// </summary>
CLASS XSharp.BlobData IMPLEMENTS XSharp.RDD.IBlobData
    /// <inheritdoc />
    PROPERTY Pointer   as INT AUTO
    /// <inheritdoc />
    PROPERTY Start     AS INT AUTO
    /// <inheritdoc />
    PROPERTY Length    AS INT AUTO
    /// <inheritdoc />
    PROPERTY Data      AS OBJECT AUTO
END CLASS


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobdirectexport/*" />
FUNCTION BLOBDirectExport(nPointer, cTargetFile, kMode) AS LOGIC CLIPPER
    VoDb.Info( BLOB_NMODE, kMode )
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Info( BLOB_DIRECT_EXPORT, BlobData{}{ Pointer := nPointer, Data := cTargetFile } ))



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobdirectget/*" />
FUNCTION BLOBDirectGet(nPointer, nStart, nCount) AS USUAL CLIPPER
    @@Default(REF nStart, 1)
    @@Default(REF nCount, Int32.MaxValue)
    LOCAL uValue := BlobData{}{Pointer := nPointer, Start := nStart, Length := nCount} AS USUAL
    IF VoDb.Info( BLOB_DIRECT_GET,  ref uValue)
        RETURN uValue
    ENDIF
    RETURN NIL



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobdirectimport/*" />
FUNCTION BLOBDirectImport(nOldPointer, cSourceFile) AS USUAL CLIPPER
    LOCAL uValue := BlobData{}{Pointer := nOldPointer, Data := cSourceFile}  AS USUAL
    IF VoDb.Info( BLOB_DIRECT_IMPORT, REF uValue)
        RETURN uValue
    ENDIF
    RETURN 0


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobdirectput/*" />
FUNCTION BLOBDirectPut(nOldPointer, uBlob) AS USUAL CLIPPER
    LOCAL uValue := BlobData{}{Pointer :=nOldPointer, Data := uBlob} AS USUAL
    IF VoDb.Info( BLOB_DIRECT_PUT,  REF uValue)
        RETURN uValue
    ENDIF
    RETURN NIL



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobexport/*" />
FUNCTION BLOBExport (nFieldPos, cTargetFile, kMode)  AS LOGIC CLIPPER
    DbInfo( BLOB_NMODE, kMode )
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FileGet( nFieldPos, cTargetFile ))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobget/*" />
FUNCTION BLOBGet(nFieldPos, nStart, nCount)  AS USUAL CLIPPER
    LOCAL info as OBJECT
    @@Default( REF nStart, 1)
    @@Default( REF nCount, Int32.MaxValue)
    info := BlobData{}{Data := nFieldPos, Start := nStart, Length := nCount}
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Info( BLOB_GET,  REF info))
    RETURN info



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobimport/*" />
FUNCTION BLOBImport (nFieldPos, cSourceFile)  AS LOGIC CLIPPER
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FilePut( nFieldPos, cSourceFile ) )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobrootget/*" />
FUNCTION BLOBRootGet() AS USUAL STRICT
    LOCAL uValue := BlobData{} AS USUAL
    IF VoDb.Info( BLOB_ROOT_GET,  REF uValue)
        RETURN uValue
    ENDIF
    RETURN NIL


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobrootlock/*" />
FUNCTION BLOBRootLock() AS USUAL STRICT
    RETURN VoDb.Info( BLOB_ROOT_LOCK, BlobData{} )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobrootput/*" />
FUNCTION BLOBRootPut(uBLOB) AS USUAL CLIPPER
    RETURN VoDb.Info( BLOB_ROOT_PUT, BlobData{} {Data := uBLOB })


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/blobrootunlock/*" />
FUNCTION BLOBRootUnlock()  AS USUAL STRICT
    RETURN VoDb.Info( BLOB_ROOT_UNLOCK, BlobData{} )


FUNCTION DbBlobInfo(nOrdinal, nPos, xNewVal) AS USUAL CLIPPER
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.BlobInfo(nOrdinal, nPos, REF xNewVal))
    RETURN xNewVal

