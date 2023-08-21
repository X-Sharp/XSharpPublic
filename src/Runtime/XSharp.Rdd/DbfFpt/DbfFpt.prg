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
BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFFPT RDD. For DBF/FPT. No index support at this level</summary>
    CLASS DBFFPT INHERIT DBF IMPLEMENTS IRawData
        PRIVATE _oFptMemo AS FPTMemo
        PROPERTY ReturnRawData as LOGIC AUTO
        CONSTRUCTOR
            SUPER()
            SELF:ReturnRawData := FALSE
            SELF:_Memo := _oFptMemo := FPTMemo{SELF}
            SELF:_oFptMemo:ExportMode := BLOB_EXPORT_APPEND

            /// <inheritdoc />
        OVERRIDE PROPERTY Driver AS STRING GET nameof(DBFFPT)

        OVERRIDE METHOD GetValue(nFldPos AS LONG) AS OBJECT
            IF SELF:_isMemoField( nFldPos )
                // At this level, the return value is the raw Data, in BYTE[]
                VAR rawData := _oFptMemo:GetRawValueWithHeader(nFldPos)
                if rawData != NULL
                    IF SELF:ReturnRawData
                        RETURN rawData
                    ENDIF
                    RETURN SELF:_oFptMemo:DecodeValue(rawData)
                else
                    IF SELF:ReturnRawData
                        RETURN <BYTE>{}
                    ENDIF
                    var column  := SELF:_GetColumn(nFldPos)
                    return column:BlankValue()
                ENDIF
            ENDIF
            RETURN SUPER:GetValue(nFldPos)

        OVERRIDE METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
            IF SELF:_ReadOnly
                SELF:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY )
            ENDIF
            SELF:ForceRel()
            IF SELF:_readRecord()
                // GoHot() must be called first because this saves the current index values
                IF ! SELF:_Hot
                    SELF:GoHot()
                ENDIF
                VAR oColumn := SELF:_GetColumn(nFldPos) ASTYPE DbfColumn
                IF oColumn != NULL
                    IF oColumn:IsMemo
                        IF SELF:HasMemo
                            IF oValue == NULL .OR. oValue IS STRING VAR cValue .AND. cValue:Length == 0
                                VAR oldValue := oColumn:GetValue(SELF:_RecordBuffer)
                                IF oldValue != NULL .and. oldValue != DBNull.Value
                                    SELF:_oFptMemo:DeleteBlock((INT) oldValue)
                                ENDIF
                                RETURN oColumn:PutValue(NULL, SELF:_RecordBuffer)
                            ENDIF
                            LOCAL bData AS BYTE[]
                            if oColumn:IsBinary
                                bData := (Byte[]) oValue
                            ELSE
                                bData := SELF:_oFptMemo:EncodeValue(oValue)
                            ENDIF
                            IF SELF:_oFptMemo:PutValue(nFldPos, bData)
                                // Update the Field Info with the new MemoBlock Position
                                RETURN oColumn:PutValue(SELF:_oFptMemo:LastWrittenBlockNumber, SELF:_RecordBuffer)
                            ENDIF
                        ELSE
                            RETURN SUPER:PutValue(nFldPos, oValue)
                        ENDIF
                    ELSE
                        RETURN oColumn:PutValue(oValue, SELF:_RecordBuffer)
                    ENDIF
                 ENDIF
            ENDIF
            RETURN FALSE



            /// <inheritdoc />
        OVERRIDE METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
            LOCAL oResult := NULL AS OBJECT
            SWITCH nOrdinal
            CASE DbInfo.DBI_MEMOHANDLE
                IF ( SELF:_oFptMemo != NULL .AND. SELF:_oFptMemo:IsOpen)
                    oResult := SELF:_oFptMemo:_hFile
                ELSE
                    oResult := IntPtr.Zero
                ENDIF
            CASE DbInfo.DBI_MEMOSTREAM
                IF ( SELF:_oFptMemo != NULL .AND. SELF:_oFptMemo:IsOpen)
                    oResult := SELF:_oFptMemo:_oStream
                ELSE
                    oResult := NULL
                ENDIF
            CASE DbInfo.DBI_MEMOEXT
                IF ( SELF:_oFptMemo != NULL .AND. SELF:_oFptMemo:IsOpen)
                    oResult := System.IO.Path.GetExtension(SELF:_oFptMemo:FileName)
                ELSE
                    oResult := FPTMemo.DefExt
                ENDIF
                IF oNewValue IS STRING VAR strExt
                    FPTMemo.DefExt := strExt
                ENDIF
            CASE DbInfo.DBI_MEMOBLOCKSIZE
                oResult := SELF:_oFptMemo:BlockSize
                IF oNewValue != NULL
                    TRY
                        LOCAL size AS LONG
                        size:= Convert.ToInt32(oNewValue)
                        SELF:_oFptMemo:BlockSize := (WORD) size
                    CATCH ex AS Exception
                        oResult := ""
                        SELF:_dbfError(ex, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)
                    END TRY
                ENDIF

            CASE DbInfo.DBI_MEMOFIELD
                oResult := ""
                IF oNewValue != NULL
                    TRY
                        LOCAL fldPos AS LONG
                        fldPos := Convert.ToInt32(oNewValue)
                        oResult := SELF:GetValue(fldPos)
                    CATCH ex AS Exception
                        oResult := ""
                        SELF:_dbfError(ex, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)
                    END TRY
                ENDIF
            CASE DbInfo.DBI_MEMOTYPE
                oResult := DB_MEMO_FPT
            CASE DbInfo.DBI_MEMOVERSION
                oResult := DB_MEMOVER_STD
            CASE DbInfo.BLOB_GET
                IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                    oResult := _oFptMemo:BlobInfo(nOrdinal, oBlob)
                ENDIF

            CASE DbInfo.BLOB_NMODE
                IF oNewValue IS LONG VAR iExportMode
                    SELF:_oFptMemo:ExportMode := iExportMode
                ENDIF
            CASE DbFieldInfo.DBS_BLOB_DIRECT_TYPE
                IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                    oResult := _oFptMemo:BlobInfo(nOrdinal, oBlob)
                ENDIF
            CASE DbFieldInfo.DBS_BLOB_DIRECT_LEN
                IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                    oResult := _oFptMemo:BlobInfo(nOrdinal, oBlob)
                ENDIF
            CASE DbInfo.BLOB_DIRECT_IMPORT
                IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                    oResult := _oFptMemo:BlobInfo(nOrdinal, oBlob)
                ENDIF
            CASE DbInfo.BLOB_DIRECT_EXPORT
                IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                    oResult := _oFptMemo:BlobInfo(nOrdinal, oBlob)
                ENDIF
            CASE DbInfo.BLOB_DIRECT_GET
                IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                    oResult := _oFptMemo:BlobInfo(nOrdinal, oBlob)
                ENDIF
            CASE DbInfo.BLOB_DIRECT_PUT
                IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                    oResult := _oFptMemo:BlobInfo(nOrdinal, oBlob)
                ENDIF
            CASE DbInfo.BLOB_ROOT_GET
                IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                    oResult := _oFptMemo:BlobInfo(nOrdinal, oBlob)
                ENDIF
            CASE DbInfo.BLOB_ROOT_PUT
                IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                    oResult := _oFptMemo:BlobInfo(nOrdinal, oBlob)
                ENDIF
            CASE DbInfo.BLOB_ROOT_LOCK
                IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                    oResult := _oFptMemo:BlobInfo(nOrdinal, oBlob)
                ENDIF
            CASE DbInfo.BLOB_ROOT_UNLOCK
                IF oNewValue IS XSharp.RDD.IBlobData VAR oBlob
                    oResult := _oFptMemo:BlobInfo(nOrdinal, oBlob)
                ENDIF
            OTHERWISE
                oResult := SUPER:Info(nOrdinal, oNewValue)
            END SWITCH
            RETURN oResult

        OVERRIDE METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
           IF SELF:_ReadOnly
                SELF:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY )
            ENDIF
            SELF:ForceRel()
            IF SELF:_readRecord()
                // GoHot() must be called first because this saves the current index values
                IF ! SELF:IsHot
                    SELF:GoHot()
                ENDIF
                RETURN SELF:_oFptMemo:PutValueFile(nFldPos, fileName)
            ENDIF
            RETURN FALSE
    END CLASS


 END NAMESPACE
