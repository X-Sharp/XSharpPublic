//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFDBT RDD. For DBF/DBT. No index support at this level</summary>
    CLASS DBFDBT INHERIT DBF IMPLEMENTS IRawData
        PRIVATE _oDbtMemo AS DBTMemo
        PROPERTY Encoding AS Encoding GET SUPER:_Encoding
        PROPERTY ReturnRawData as LOGIC AUTO

        CONSTRUCTOR
            SUPER()
            SELF:_Memo := _oDbtMemo := DBTMemo{SELF}

        OVERRIDE PROPERTY Driver AS STRING GET nameof(DBFDBT)
        // Return the memo content as STRING
        OVERRIDE METHOD GetValue(nFldPos AS LONG) AS OBJECT
            LOCAL buffer AS BYTE[]
            // not a memo ?
            IF SELF:_isMemoField( nFldPos )
                // At this level, the return value is the raw Data, in BYTE[]
                buffer := (BYTE[])SUPER:GetValue(nFldPos)
                IF ( buffer != NULL )
                    IF ReturnRawData
                        RETURN buffer
                    ENDIF
                    LOCAL str AS STRING
                    str :=  SELF:Encoding:GetString(buffer)
                    // Convert to String and return
                    RETURN str
                ELSE
                    // No Memo ?!, Empty String
                    IF ReturnRawData
                        RETURN <BYTE>{}
                    ENDIF
                    RETURN String.Empty
                ENDIF
            ENDIF
            RETURN SUPER:GetValue(nFldPos)

        /// <inheritdoc />
        OVERRIDE METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
            LOCAL oResult AS OBJECT
            SWITCH nOrdinal
            CASE DbInfo.DBI_MEMOHANDLE
                IF ( SELF:_oDbtMemo != NULL .AND. SELF:_oDbtMemo:IsOpen)
                    oResult := SELF:_oDbtMemo:_hFile
                ELSE
                    oResult := IntPtr.Zero
                ENDIF

            CASE DbInfo.DBI_MEMOSTREAM
                IF ( SELF:_oDbtMemo != NULL .AND. SELF:_oDbtMemo:IsOpen)
                    oResult := SELF:_oDbtMemo:_oStream
                ELSE
                    oResult := IntPtr.Zero
                ENDIF
            CASE DbInfo.DBI_MEMOEXT
                IF ( SELF:_oDbtMemo != NULL .AND. SELF:_oDbtMemo:IsOpen)
                    oResult := System.IO.Path.GetExtension(SELF:_oDbtMemo:FileName)
                ELSE
                    oResult := DBT_MEMOEXT
                ENDIF
                IF oNewValue IS STRING VAR strExt
                    DBTMemo.DefExt := strExt
                ENDIF
            CASE DbInfo.DBI_MEMOBLOCKSIZE
                oResult := SELF:_oDbtMemo:BlockSize
            CASE DbInfo.DBI_MEMOFIELD
                SELF:ForceRel()
                oResult := ""
                IF oNewValue != NULL
                    TRY
                       LOCAL fldPos AS LONG
                       fldPos := Convert.ToInt32(oNewValue)
                       oResult := SELF:GetValue(fldPos)
                    CATCH ex AS Exception
                        oResult := ""
                        SELF:_dbfError(ex, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, "DBFDBT.Info")
                    END TRY
                ENDIF
            CASE DbInfo.DBI_MEMOTYPE
                oResult := DB_MEMO_DBT
            CASE DbInfo.DBI_MEMOVERSION
                oResult := DB_MEMOVER_STD
            OTHERWISE
                oResult := SUPER:Info(nOrdinal, oNewValue)
            END SWITCH
            RETURN oResult


     END CLASS



END NAMESPACE
