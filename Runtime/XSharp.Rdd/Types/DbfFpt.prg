//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING XSharp.RDD.CDX


INTERNAL FUNCTION ShortToBuff(siValue AS SHORT, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:shortValue := siValue
    buffer[nOffSet+0]   := nValue:b1
    buffer[nOffSet+1]   := nValue:b2
    
INTERNAL FUNCTION BuffToShort(buffer AS BYTE[], nOffSet AS LONG) AS SHORT
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:b1 := buffer[nOffSet+0]   
    nValue:b2 := buffer[nOffSet+1]
    RETURN nValue:shortValue
    
INTERNAL FUNCTION ShortToFox(siValue AS SHORT, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:shortValue := siValue
    buffer[nOffSet+1]   := nValue:b1
    buffer[nOffSet+0]   := nValue:b2
    
INTERNAL FUNCTION FoxToShort(buffer AS BYTE[], nOffSet AS LONG) AS SHORT
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:b1 := buffer[nOffSet+1]   
    nValue:b2 := buffer[nOffSet+0]
    RETURN nValue:shortValue
    
    
INTERNAL FUNCTION BuffToLong(buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:b1 := buffer[nOffSet+0]
    nValue:b2 := buffer[nOffSet+1]
    nValue:b3 := buffer[nOffSet+2]
    nValue:b4 := buffer[nOffSet+3]
    RETURN nValue:longValue
    
INTERNAL FUNCTION LongToBuff(liValue AS LONG, buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:LongValue := liValue
    buffer[nOffSet+0] := nValue:b1
    buffer[nOffSet+1] := nValue:b2
    buffer[nOffSet+2] := nValue:b3
    buffer[nOffSet+3] := nValue:b4
    RETURN liValue
    
    
INTERNAL FUNCTION FoxToLong(buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:b4 := buffer[nOffSet+0]
    nValue:b3 := buffer[nOffSet+1]
    nValue:b2 := buffer[nOffSet+2]
    nValue:b1 := buffer[nOffSet+3]
    RETURN nValue:longValue
    
INTERNAL FUNCTION LongToFox(liValue AS LONG, buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:LongValue := liValue
    buffer[nOffSet+0] := nValue:b4
    buffer[nOffSet+1] := nValue:b3
    buffer[nOffSet+2] := nValue:b2
    buffer[nOffSet+3] := nValue:b1
    RETURN liValue
    
    
    BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFFPT RDD. For DBF/FPT. No index support at this level</summary>
    CLASS DBFFPT INHERIT DBF 
        PRIVATE _oFptMemo AS FptMemo
        CONSTRUCTOR   
            SUPER()
            SELF:_oMemo := _oFptMemo := FptMemo{SELF}
            /// <inheritdoc />	
        PROPERTY Driver AS STRING GET "DBFFPT"
        
        
        
        METHOD GetValue(nFldPos AS LONG) AS OBJECT
            LOCAL rawData AS BYTE[]
            LOCAL buffer AS BYTE[]
            LOCAL nType AS LONG
            IF SELF:_isMemoField( nFldPos )
                // At this level, the return value is the raw Data, in BYTE[]
                rawData := (BYTE[])SUPER:GetValue(nFldPos)
                IF rawData != NULL
                    // So, extract the "real" Data
                    buffer := BYTE[]{ rawData:Length - 8}
                    Array.Copy(rawData,8, buffer, 0, buffer:Length)
                    // Get the Underlying type from the MemoBlock
                    // 0 : Picture (on MacOS); 1: Memo; 2 : Object
                    nType := FoxToLong( rawData, 0)
                    IF SELF:_isMemoField( nFldPos ) .AND. ( nType == 1 )
                        LOCAL encoding AS Encoding //ASCIIEncoding
                        LOCAL str AS STRING
                        encoding := SELF:_Encoding //ASCIIEncoding{}
                        str :=  encoding:GetString(buffer)
                        // Convert to String and return
                        RETURN str
                    ENDIF
                    RETURN buffer
                ELSE
                    RETURN String.Empty
                ENDIF
            ENDIF
            RETURN SUPER:GetValue(nFldPos)
            
            // Indicate if a Field is a Memo; Called by GetValue() in Parent Class
            // At DbfFpt Level, TRUE for DbFieldType.Memo, DbFieldType.Picture, DbFieldType.Object
        INTERNAL VIRTUAL METHOD _isMemoFieldType( fieldType AS DbFieldType ) AS LOGIC
            RETURN ( ( fieldType == DbFieldType.Memo ) .OR. ( fieldType == DbFieldType.Picture ) .OR. ( fieldType == DbFieldType.Ole ) )
            /// <inheritdoc />
        VIRTUAL METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
            LOCAL oResult AS OBJECT
            SWITCH nOrdinal
            CASE DbInfo.DBI_MEMOHANDLE
                    IF ( SELF:_oFptMemo != NULL .AND. SELF:_oFptMemo:_Open)
                        oResult := SELF:_oFptMemo:_hFile
                    ELSE
                        oResult := IntPtr.Zero
                    ENDIF
                    
            CASE DbInfo.DBI_MEMOEXT
                    IF ( SELF:_oFptMemo != NULL .AND. SELF:_oFptMemo:_Open)
                        oResult := System.IO.Path.GetExtension(SELF:_oFptMemo:_FileName)
                    ELSE
                        oResult := FptMemo.DefExt
                    ENDIF
                    IF oNewValue IS STRING
                        FptMemo.DefExt := (STRING) oNewValue
                ENDIF
            CASE DbInfo.DBI_MEMOBLOCKSIZE
                oResult := SELF:_oFptMemo:BlockSize
            CASE DBInfo.DBI_MEMOFIELD
                    oResult := ""
                    IF oNewValue != NULL
                        TRY
                            LOCAL fldPos AS LONG
                            fldPos := Convert.ToInt32(oNewValue)
                            oResult := SELF:GetValue(fldPos)
                        CATCH ex AS exception
                            oResult := ""   
                            SELF:_dbfError(ex, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                        END TRY
                ENDIF
            CASE DbInfo.DBI_MEMOTYPE
                oResult := DB_MEMO_FPT
            CASE DbInfo.DBI_MEMOVERSION
                oResult := DB_MEMOVER_STD
            OTHERWISE
                oResult := SUPER:Info(nOrdinal, oNewValue)
            END SWITCH
            RETURN oResult
            
            
            END CLASS
            
            
    /// <summary>FPT Memo class. Implements the FTP support.</summary>
    INTERNAL CLASS FPTMemo INHERIT BaseMemo IMPLEMENTS IMemo
        INTERNAL _hFile	    AS IntPtr
        INTERNAL _FileName  AS STRING
        INTERNAL _Open      AS LOGIC
        PROTECT _Shared     AS LOGIC
        PROTECT _ReadOnly   AS LOGIC
        PROTECT _oRDD       AS DBF
        PROTECT _blockSize  AS SHORT
        PROTECT _lockScheme AS DbfLocking
        PROPERTY Shared AS LOGIC GET _Shared
        STATIC PROPERTY DefExt AS STRING AUTO
        PROTECTED PROPERTY IsOpen AS LOGIC GET SELF:_hFile != F_ERROR  .AND. SELF:_hFile != IntPtr.Zero      

        STATIC CONSTRUCTOR
            DefExt := FPT_MEMOEXT
            
        PRIVATE METHOD GetMemoExtFromDbfExt(cDbfName AS STRING) AS STRING
            SWITCH System.IO.Path.GetExtension(cDbfName:ToLower())
            CASE ".vcx"         // Control Library
                RETURN ".VCT"
            CASE ".scx"         // Screen
                RETURN ".SCT"
            CASE ".pjx"         // Project
                RETURN ".PJT"
            CASE ".frx"         // Report
                RETURN ".FRT"
            CASE ".mnx"         // Menu
                RETURN ".MNT"
            CASE ".dbc"         // database container
                RETURN ".dct"
            END SWITCH
            RETURN DefExt
            
            
        CONSTRUCTOR (oRDD AS DBF)
            SUPER(oRDD)
            SELF:_oRdd := oRDD
            SELF:_hFile := IntPtr.Zero
            SELF:_Shared := SELF:_oRDD:_Shared
            SELF:_ReadOnly := SELF:_oRdd:_ReadOnly
            SELF:_Encoding := SELF:_oRdd:_Encoding
            
            
        VIRTUAL PROTECTED METHOD _initContext() AS VOID
            SELF:_lockScheme:Initialize( DbfLockingModel.FoxPro )
            IF ( SELF:BlockSize == 0 )
                //SELF:BlockSize := FPT_DEFBLOCKSIZE
                SELF:BlockSize := (SHORT) XSharp.RuntimeState.MemoBlockSize
            ENDIF
            
            
            /// <inheritdoc />
        METHOD Flush() 			AS LOGIC		
            LOCAL isOk AS LOGIC
            isOk := SELF:IsOpen
            IF isOk
                isOk := FFlush( SELF:_hFile )
            ENDIF
            RETURN isOk
            
            /// <inheritdoc />
        METHOD GetValue(nFldPos AS INT) AS OBJECT
            LOCAL blockNbr AS LONG
            LOCAL blockLen := 0 AS LONG
            LOCAL memoBlock := NULL AS BYTE[]
            //
            blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
            IF ( blockNbr > 0 )
                // Get the raw Length of the Memo
                blockLen := SELF:_getValueLength( nFldPos )
                IF blockLen > 0 
                    memoBlock := BYTE[]{ blockLen }
                    // Where do we start ?
                    LOCAL iOffset := blockNbr * SELF:BlockSize AS LONG
                    //
                    FSeek3( SELF:_hFile, iOffset, FS_SET )
                    // Max 512 Blocks
                    LOCAL isOk AS LOGIC
                    isOk := ( FRead3( SELF:_hFile, memoBlock, (DWORD)blockLen ) == blockLen )
                    IF ( !isOk )
                        memoBlock := NULL
                    ENDIF
                ENDIF
            ENDIF
            // At this level, the return value is the raw Data, in BYTE[]
            RETURN memoBlock
            
            /// <inheritdoc />
        METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
            THROW NotImplementedException{}
            
            /// <inheritdoc />
        METHOD GetValueLength(nFldPos AS INT) AS INT
            LOCAL blockLen := 0 AS LONG
            blockLen := SELF:_getValueLength( nFldPos )
            // Don't forget to remove the 8 Bytes
            blockLen := blockLen - 8
            RETURN blockLen
            
            // Get the raw data length
        VIRTUAL PROTECTED METHOD _getValueLength(nFldPos AS INT) AS INT
            // In FPT :
            // The first 4 Bytes contains the tpye of Memo Data
            // The next 4 Bytes contains the length of Memo data, including the first 8 bytes
            LOCAL blockNbr AS LONG
            LOCAL blockLen := 0 AS LONG
            // Where does the block start ?
            blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
            IF ( blockNbr > 0 ) 
                // File Open ?
                LOCAL isOk := ( SELF:_hFile != F_ERROR ) AS LOGIC
                IF isOk
                    //
                    LOCAL blockInfo AS BYTE[]
                    blockInfo := BYTE[]{8}
                    // Where do we start ?
                    LOCAL iOffset := blockNbr * SELF:_blockSize AS LONG
                    // Go to the blockNbr position
                    FSeek3( SELF:_hFile, iOffset, FS_SET )
                    // 
                    FRead3( SELF:_hFile, blockInfo, (DWORD) blockInfo:Length )
                    blockLen     := FoxToLong(blockInfo,4) +8
                    // The raw length include the 8 Bytes included in the Memo Block
                ENDIF
            ENDIF
            RETURN blockLen
            
            /// <inheritdoc />
        VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            // We receive hea the raw Data : We now have to put extra information on it
            // 4 Bytes for the "type" of memo data
            // 4 Bytes for the length of block
            LOCAL objType AS System.Type
            LOCAL objTypeCode AS System.TypeCode
            LOCAL str AS STRING
            LOCAL rawData AS BYTE[]
            LOCAL memoBlock AS BYTE[]
            //
            IF !SELF:IsOpen
                RETURN FALSE
            ENDIF
            //
            objType := oValue:GetType()
            objTypeCode := Type.GetTypeCode( objType )
            //
            IF ( objTypeCode == TypeCode.Char )
                str := STRING{ (CHAR)oValue, 1 }
            ELSEIF ( objTypeCode == TypeCode.String )
                str := oValue ASTYPE STRING
            ELSE
                str := NULL
            ENDIF
            // Not a Char, Not a String
            IF ( str == NULL )
                rawData := (BYTE[])oValue
            ELSE
                LOCAL encoding AS Encoding //ASCIIEncoding
                rawData := BYTE[]{str:Length}
                encoding := SELF:_Encoding //ASCIIEncoding{}
                encoding:GetBytes( str, 0, str:Length, rawData, 0 )
            ENDIF
            // Add the extra Info
            memoBlock := BYTE[]{ rawData:Length + 4 + 4 }
            // Put the Data
            Array.Copy(rawData,0, memoBlock, 8, rawData:Length)
            // now, put the length
            LongToFox(memoBlock:Length, memoBlock,4)
            // And finally, put the Data Type
            // 0 : Picture (on MacOS); 1: Memo; 2 : Object
            LOCAL nType := 2 AS LONG
            IF ( SELF:_oRdd:_FieldType( nFldPos ) == DbFieldType.Memo )
                nType := 1
            ELSEIF ( SELF:_oRdd:_FieldType( nFldPos ) == DbFieldType.Picture )
                nType := 0
            ENDIF
            LongToFox(nType, memoBlock,0)
            // Ok, we now have a FPT Memoblock, save
            RETURN SUPER:PutValue( nFldPos, memoBlock )
            
            /// <inheritdoc />
        VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
            THROW NotImplementedException{}
            
            /// <inheritdoc />
        VIRTUAL METHOD CloseMemFile( ) AS LOGIC
            LOCAL isOk := FALSE AS LOGIC
            IF SELF:IsOpen
                //
                TRY
                    isOk := FClose( SELF:_hFile )
                    
                CATCH ex AS Exception
                    isOk := FALSE
                    SELF:_oRDD:_dbfError(ex, SubCodes.ERDD_CLOSE_MEMO, GenCode.EG_CLOSE, "DBFDBT.CloseMemFile")
                    
                END TRY
                SELF:_hFile := F_ERROR
                SELF:_Open  := FALSE
            ENDIF
            RETURN isOk
            
            /// <inheritdoc />
        VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
            LOCAL isOk AS LOGIC
            SELF:_FileName  := info:FileName
            VAR cExt        := GetMemoExtFromDbfExt(SELF:_FileName)
            SELF:_FileName  := System.IO.Path.ChangeExtension( SELF:_FileName, cExt )
            SELF:_Shared    := info:Shared
            SELF:_ReadOnly  := info:ReadOnly
            //
            SELF:_hFile     := FCreate( SELF:_FileName) 
            isOk := SELF:IsOpen
            IF isOk
                // Per default, Header Block Size if 512
                LOCAL memoHeader AS BYTE[]
                LOCAL nextBlock AS LONG
                //
                memoHeader := BYTE[]{ DBTMemo.HeaderSize }
                nextBlock := 1
                Array.Copy(BitConverter.GetBytes(nextBlock),0, memoHeader, 0, SIZEOF(LONG))
                //
                FWrite3( SELF:_hFile, memoHeader, DBTMemo.HeaderSize )
                //
                SELF:_initContext()
            ELSE
                SELF:_oRDD:_DbfError( ERDD.CREATE_MEMO, XSharp.Gencode.EG_CREATE )
            ENDIF
            //
            RETURN isOk
            
            /// <inheritdoc />
        VIRTUAL METHOD OpenMemFile(info AS DbOpenInfo ) AS LOGIC
            LOCAL isOk AS LOGIC
            SELF:_FileName  := info:FileName
            VAR cExt        := GetMemoExtFromDbfExt(SELF:_FileName)
            SELF:_FileName  := System.IO.Path.ChangeExtension( SELF:_FileName, cExt )
            SELF:_Shared    := info:Shared
            SELF:_ReadOnly  := info:ReadOnly
            //
            SELF:_hFile     := Fopen(SELF:_FileName, info:FileMode)
            isOk := SELF:IsOpen
            IF isOk
                // Per default, Block Size if 512
                SELF:_initContext()
            ELSE
                SELF:_oRDD:_DbfError( ERDD.OPEN_MEMO, XSharp.Gencode.EG_OPEN )
                isOk := FALSE
            ENDIF
            //
            RETURN isOk
            
        VIRTUAL PROPERTY BlockSize 	 AS SHORT
            GET 
                LOCAL nSize := 0 AS SHORT
                 IF SELF:IsOpen
                    LOCAL _sizeOfBlock AS BYTE[]
                    // _sizeOfBlock is at the beginning of MemoFile, at posiion 6
                    _sizeOfBlock := BYTE[]{2}
                    LOCAL savedPos := FSeek3(SELF:_hFile, 0, FS_RELATIVE ) AS LONG
                    FSeek3( SELF:_hFile, 6, FS_SET )
                    IF ( FRead3( SELF:_hFile, _sizeOfBlock, 2 ) == 2 )
                        nSize := FoxToShort(_sizeOfBlock,0)
                    ENDIF
                    FSeek3(SELF:_hFile, savedPos, FS_SET )
                    SELF:_blockSize := nSize
                ENDIF
                RETURN nSize
            END GET
            
            SET 
                IF SELF:IsOpen
                    LOCAL _sizeOfBlock AS BYTE[]
                    _sizeOfBlock := BYTE[]{2}
                    ShortToFox( VALUE, _sizeOfBlock,0)
                    LOCAL savedPos := FSeek3(SELF:_hFile, 0, FS_RELATIVE ) AS LONG
                    FSeek3( SELF:_hFile, 6, FS_SET )
                    FWrite3( SELF:_hFile, _sizeOfBlock, 2 )
                    FSeek3(SELF:_hFile, savedPos, FS_SET )
                    SELF:_blockSize := VALUE
                ENDIF
            END SET
            
        END PROPERTY
        
        // Place a lock : <nOffset> indicate where the lock should be; <nLong> indicate the number bytes to lock
        // If it fails, the operation is tried <nTries> times, waiting 1ms between each operation.
        // Return the result of the operation
        PROTECTED METHOD _tryLock( nOffset AS UINT64, nLong AS LONG, nTries AS LONG  ) AS LOGIC
            LOCAL locked AS LOGIC
            IF ! SELF:IsOpen
                RETURN FALSE
            ENDIF
            REPEAT
                TRY
                    locked := FFLock( SELF:_hFile, (DWORD)nOffset, (DWORD)nLong )
                CATCH ex AS Exception
                    locked := FALSE
                    SELF:_oRDD:_dbfError(ex, SubCodes.ERDD_INIT_LOCK, GenCode.EG_LOCK_ERROR, "DBFDBT._tryLock")
                END TRY
                IF ( !locked )
                    nTries --
                    IF ( nTries > 0 )
                        System.Threading.Thread.Sleep( 1 )
                    ENDIF
                ENDIF
            UNTIL ( locked .OR. (nTries==0) )
            //
            RETURN locked
            
        PROTECTED METHOD _unlock( nOffset AS UINT64, nLong AS LONG ) AS LOGIC
            LOCAL unlocked AS LOGIC
            IF ! SELF:IsOpen
                RETURN FALSE
            ENDIF

            TRY
                unlocked := FFUnLock( SELF:_hFile, (DWORD)nOffset, (DWORD)nLong )
            CATCH ex AS Exception
                unlocked := FALSE
                SELF:_oRDD:_dbfError(ex, SubCodes.ERDD_UNLOCKED, GenCode.EG_UNLOCKED, "DBFDBT._unlock")
                
            END TRY
            RETURN unlocked
            
        VIRTUAL METHOD Zap() AS LOGIC
            THROW NotImplementedException{}
    END CLASS    
    
    
    END NAMESPACE
