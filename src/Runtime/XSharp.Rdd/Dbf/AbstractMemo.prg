//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.IO

BEGIN NAMESPACE XSharp.RDD
    INTERNAL ABSTRACT CLASS AbstractMemo INHERIT BaseMemo IMPLEMENTS IMemo
        INTERNAL _hFile	    AS IntPtr
        INTERNAL _oStream   AS FileStream
        INTERNAL FileName  AS STRING
        PROTECT _Shared     AS LOGIC
        PROTECT _ReadOnly   AS LOGIC
        PROTECT _lockScheme AS DbfLocking
        PROTECT _blockSize  AS WORD
        PROTECT _oRdd       AS DBF

        PROPERTY IsOpen     AS LOGIC GET SELF:_hFile != F_ERROR  .AND. SELF:_hFile != IntPtr.Zero
        PROPERTY Shared     AS LOGIC GET _Shared
        PROPERTY Encoding   AS Encoding GET SELF:_oRdd:_Encoding
        PROPERTY Extension  AS STRING AUTO GET SET
        OVERRIDE PROPERTY FullPath   AS STRING GET FileName
        VIRTUAL PROPERTY BlockSize 	AS WORD GET SELF:_blockSize SET SELF:_blockSize := value

        /// <inheritdoc />
       CONSTRUCTOR (oRDD AS DBF)
            SUPER(oRDD)
            SELF:_oRdd  := oRDD
            SELF:_hFile := F_ERROR
            SELF:_oStream   := NULL
            SELF:_Shared := oRDD:Shared
            SELF:_ReadOnly := oRDD:ReadOnly

            /// <inheritdoc />
        OVERRIDE METHOD Flush() AS LOGIC
            LOCAL isOk := FALSE AS LOGIC
            IF SELF:IsOpen
                SELF:_oStream:Flush()
                isOk := TRUE
            ENDIF
            RETURN isOk

        /// <inheritdoc />
        OVERRIDE METHOD CloseMemFile( ) AS LOGIC
            LOCAL isOk := FALSE AS LOGIC
            IF SELF:IsOpen
                //
                TRY
                    isOk := FClose( SELF:_hFile )

                CATCH ex AS Exception
                    isOk := FALSE
                    SELF:Error(ex, Subcodes.ERDD_CLOSE_MEMO, Gencode.EG_CLOSE, "AbstractMemo.CloseMemFile")

                END TRY
                SELF:_hFile := F_ERROR
                SELF:_oStream := NULL
            ENDIF
            RETURN isOk

        OVERRIDE METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
            SELF:FileName  := System.IO.Path.ChangeExtension( info:FullName, SELF:Extension )
            SELF:_Shared    := info:Shared
            SELF:_ReadOnly  := info:ReadOnly
            SELF:_hFile     := FCreate( SELF:FileName)
            SELF:_oStream   := FGetStream(_hFile)
            if _oStream != NULL_OBJECT
                SELF:FileName   := _oStream:Name
            endif
            RETURN SELF:IsOpen

       OVERRIDE METHOD OpenMemFile(info AS DbOpenInfo ) AS LOGIC
            SELF:FileName  := System.IO.Path.ChangeExtension( info:FullName, SELF:Extension )
            SELF:_Shared    := info:Shared
            SELF:_ReadOnly  := info:ReadOnly
            SELF:_hFile     := FOpen(SELF:FileName, info:FileMode)
            SELF:_oStream   := FGetStream(_hFile)
            if _oStream != NULL_OBJECT
                SELF:FileName   := _oStream:Name
            endif
            RETURN SELF:IsOpen

     METHOD Error(ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING) AS VOID
        SELF:_oRdd:_dbfError(ex, iSubCode, iGenCode,strFunction)

    END CLASS
END NAMESPACE

