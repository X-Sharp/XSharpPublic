//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING XSharp.RDD.CDX
USING System.Diagnostics
USING System.IO
USING STATIC XSharp.Conversions
using XSharp.RDD.FlexFile

BEGIN NAMESPACE XSharp.RDD
/// <summary>DBFFPT RDD. For DBF/FPT. No index support at this level</summary>

[DebuggerDisplay("{Driver,q} ({Alias,nq})")];
CLASS DBFBLOB INHERIT Workarea
    INTERNAL _area              as FlexArea
    INTERNAL _OpenInfo		    AS DbOpenInfo // current dbOpenInfo structure in OPEN/CREATE method
    INTERNAL _oStream           AS FileStream

    STATIC PROPERTY DefExt AS STRING AUTO
    STATIC CONSTRUCTOR
        DefExt := DBV_MEMOEXT

    CONSTRUCTOR
        SUPER()
        SELF:_area := FlexArea{SELF}
        SELF:_area:ExportMode := BLOB_EXPORT_APPEND

    INTERNAL METHOD Error(ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING) AS VOID
        SELF:_dbfError(ex, iSubCode, iGenCode,strFunction)

    /// <inheritdoc />
    OVERRIDE PROPERTY Driver AS STRING GET nameof(DBFBLOB)
    PROTECT PROPERTY IsOpen AS LOGIC GET SELF:_hFile != F_ERROR
    OVERRIDE METHOD Open(info AS DbOpenInfo ) AS LOGIC
        LOCAL isOk AS LOGIC
        SELF:_OpenInfo := info
        // Should we set to .DBF per default ?
        IF String.IsNullOrEmpty(SELF:_OpenInfo:Extension)
            SELF:_OpenInfo:Extension := DefExt
        ENDIF
        SELF:_FileName := SELF:_OpenInfo:FullName
        IF File(SELF:_FileName)
            SELF:_FileName := FPathName()
            SELF:_OpenInfo:FullName := SELF:_FileName
        ENDIF
        SELF:_Alias := SELF:_OpenInfo:Alias
        SELF:_Shared := SELF:_OpenInfo:Shared
        SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
        SELF:_hFile    := FOpen(SELF:_FileName, SELF:_OpenInfo:FileMode)
        IF SELF:IsOpen
            SELF:_oStream    := (FileStream) FGetStream(SELF:_hFile)
            isOk := _area:Open(_oStream, _hFile)
        ELSE
            SELF:Error( FException(),ERDD.OPEN_FILE, XSharp.Gencode.EG_OPEN ,"DBFBLOB.Open")
            isOk := FALSE
        ENDIF
        //
        RETURN isOk

    OVERRIDE METHOD Close() AS LOGIC
        local isOk := FALSE as LOGIC
        IF SELF:IsOpen
            isOk := SUPER:Close() 
            FClose( SELF:_hFile )
            SELF:_hFile := F_ERROR
            SELF:_oStream := NULL
        ENDIF
        RETURN isOk

    OVERRIDE METHOD Create(info AS DbOpenInfo) AS LOGIC
        LOCAL isOk := FALSE AS LOGIC
        SELF:_OpenInfo := info
        // Should we set to .DBF per default ?
        IF String.IsNullOrEmpty(SELF:_OpenInfo:Extension)
            SELF:_OpenInfo:Extension := DefExt
            //
        ENDIF
        SELF:_FileName := SELF:_OpenInfo:FullName
        SELF:_Alias := SELF:_OpenInfo:Alias
        SELF:_Shared := SELF:_OpenInfo:Shared
        SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
        SELF:_hFile    := FCreate2( SELF:_FileName, FO_EXCLUSIVE | FO_UNBUFFERED)
        IF SELF:IsOpen
            SELF:_oStream    := (FileStream) FGetStream(SELF:_hFile)
            SELF:_FileName   := _oStream:Name
            isOk := _area:Create(_oStream,_hFile, 1)
        ELSE
            SELF:Error( FException(), ERDD.CREATE_FILE, XSharp.Gencode.EG_CREATE, "DBFBLOB.Create")
        ENDIF

        RETURN isOk

    OVERRIDE METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC
        SELF:SetFieldExtent(1)
        local oField as RddFieldInfo
        oField := RddFieldInfo{"NOFIELDS","L",1,0}
        SELF:AddField(oField)
        RETURN TRUE

    OVERRIDE METHOD Info(nOrdinal as int, oNewValue as object) AS OBJECT
        LOCAL oResult := NULL AS OBJECT
        SWITCH nOrdinal
        CASE DbInfo.BLOB_DIRECT_PUT
            IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                oResult := _area:BlobInfo(nOrdinal, oBlob)
            ENDIF
        CASE DbInfo.BLOB_DIRECT_GET
            IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                oResult := _area:BlobInfo(nOrdinal, oBlob)
            ENDIF
        CASE DbInfo.BLOB_ROOT_GET
            IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                oResult := _area:BlobInfo(nOrdinal, oBlob)
            ENDIF
        CASE DbInfo.BLOB_ROOT_PUT
            IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                oResult := _area:BlobInfo(nOrdinal, oBlob)
            ENDIF
        CASE DbInfo.BLOB_ROOT_LOCK
            IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                oResult := _area:BlobInfo(nOrdinal, oBlob)
            ENDIF
        CASE DbInfo.BLOB_ROOT_UNLOCK
            IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                oResult := _area:BlobInfo(nOrdinal, oBlob)
            ENDIF
        CASE DbInfo.BLOB_DIRECT_IMPORT
            IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                oResult := _area:BlobInfo(nOrdinal, oBlob)
            ENDIF
        CASE DbInfo.BLOB_DIRECT_EXPORT
            IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                oResult := _area:BlobInfo(nOrdinal, oBlob)
            ENDIF
        CASE DbFieldInfo.DBS_BLOB_DIRECT_TYPE
            IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                oResult := _area:BlobInfo(nOrdinal, oBlob)
            ENDIF
        CASE DbFieldInfo.DBS_BLOB_DIRECT_LEN
            IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                oResult := _area:BlobInfo(nOrdinal, oBlob)
            ENDIF
        OTHERWISE
            oResult := SUPER:Info(nOrdinal, oNewValue)
        END SWITCH
        RETURN oResult

END CLASS

END NAMESPACE
