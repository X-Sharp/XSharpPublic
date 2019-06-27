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

    BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFFPT RDD. For DBF/FPT. No index support at this level</summary>
    CLASS DBFFPT INHERIT DBF 
        PRIVATE _oFptMemo AS FptMemo
        CONSTRUCTOR   
            SUPER()
            SELF:_oMemo := _oFptMemo := FptMemo{SELF}
            /// <inheritdoc />	
        PROPERTY Driver AS STRING GET "DBFFPT"


        INTERNAL METHOD DecodeFlexArray(nType AS FlexFieldType, bData AS BYTE[], nOffset REF LONG) AS OBJECT
            LOCAL iLen AS Int32
            LOCAL aValues AS OBJECT[]
            IF nType == FlexFieldType.Array16 // 16 bits
                iLen := BitConverter.ToInt16(bData, nOffSet)
                nOffSet += 2
            ELSEIF nType == FlexFieldType.Array32
                iLen := BitConverter.ToInt32(bData, nOffSet)
                nOffSet += 4
            ELSE
                RETURN NULL
            ENDIF
            aValues := OBJECT[]{iLen}
            FOR VAR i := 0 TO iLen-1
                VAR nFldType := bData[nOffSet]
                LOCAL element AS OBJECT
                LOCAL length AS LONG
                nOffSet += 1
                VAR nArrType := (FlexArrayTypes) nFldType
                SWITCH nArrType
                CASE FlexArrayTypes.NIL
                    element := DBNull.Value
                CASE FlexArrayTypes.Char
                    element := (sByte) bData[ nOffSet]
                    nOffSet += 1
                CASE FlexArrayTypes.UChar
                    element := (BYTE) bData[ nOffSet]
                    nOffSet += 1
                CASE FlexArrayTypes.Short
                    element := BitConverter.ToInt16(bData,nOffSet)
                    nOffSet += 2
                CASE FlexArrayTypes.UShort
                    element := BitConverter.ToUInt16(bData,nOffSet)
                    nOffSet += 2
                CASE FlexArrayTypes.Long     
                    element := BitConverter.ToInt32(bData,nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.String32 
                    length := BitConverter.ToInt32(bData,nOffSet)
                    nOffSet += 4
                    element := _encoding:GetString(bData, nOffSet, length)
                    nOffSet += length
                CASE FlexArrayTypes.String16 
                    length := BitConverter.ToInt16(bData,nOffSet)
                    nOffSet += 2
                    element := _encoding:GetString(bData, nOffSet, length)
                    nOffSet += length
                CASE FlexArrayTypes.Float    
                    element := 0.0
                    nOffSet += 10
                CASE FlexArrayTypes.Double 
                    element := BitConverter.ToDouble(bData, nOffSet)
                    nOffSet += 8
                CASE FlexArrayTypes.Date     
                    element := BitConverter.ToInt32(bData, nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.Logic    
                    element := bData[nOffSet] != 0
                    nOffSet += 1
                CASE FlexArrayTypes.Array    
                    element := DecodeFlexArray(FlexFieldType.Array16, bData, REF nOffSet)

                CASE FlexArrayTypes.CodeBlock
                    element := NULL
                    SELF:_dbfError(NULL, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.DateJ    
                    element := BitConverter.ToInt32(bData, nOffSet)
                    nOffSet += 4

                CASE FlexArrayTypes.Double2  
                    element := BitConverter.ToDouble(bData, nOffSet)
                    nOffSet += 6

                CASE FlexArrayTypes.Cyclic
                    element := NULL
                    SELF:_dbfError(NULL, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.UCHar1  
                    element := (sByte) bData[ nOffSet]
                    nOffSet += 2

                CASE FlexArrayTypes.Char1    
                    element := (BYTE) bData[ nOffSet]
                    nOffSet += 2

                CASE FlexArrayTypes.Short1   
                    element := BitConverter.ToInt16(bData, nOffSet)
                    nOffSet += 3
                CASE FlexArrayTypes.UShort1  
                    element := BitConverter.ToUInt16(bData, nOffSet)
                    nOffSet += 3
                CASE FlexArrayTypes.Long1  
                    element := BitConverter.ToInt32(bData, nOffSet)
                    nOffSet += 5
                CASE FlexArrayTypes.Unused
                    element := NULL
                    SELF:_dbfError(NULL, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.Object
                    element := NULL
                    SELF:_dbfError(NULL, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.Null     
                    element := String.Empty

                CASE FlexArrayTypes.True     
                    element := TRUE

                CASE FlexArrayTypes.False    
                    element := FALSE

                CASE FlexArrayTypes.LDouble
                    element := NULL
                    SELF:_dbfError(NULL, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, __FUNCTION__)
                CASE FlexArrayTypes.UCHar2   
                    element := (SByte) bData[ nOffSet]
                    nOffSet += 3
                CASE FlexArrayTypes.CHar2    
                    element := (BYTE) bData[ nOffSet]
                    nOffSet += 3
                CASE FlexArrayTypes.Short2   
                    element := BitConverter.ToInt16(bData, nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.UShort2  
                    element := BitConverter.ToUInt16(bData, nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.Long2    
                    element := BitConverter.ToInt32(bData, nOffSet)
                    nOffSet += 6
                CASE FlexArrayTypes.ULong2   
                    element := BitConverter.ToUInt32(bData, nOffSet)
                    nOffSet += 6
                OTHERWISE
                    element := NULL
                    SELF:_dbfError(NULL, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, __FUNCTION__)
                END SWITCH
                aValues[i] := element
            NEXT
            RETURN aValues

        INTERNAL METHOD EncodeValue(oValue AS OBJECT) AS BYTE[]
            LOCAL token AS FtpMemoToken
            LOCAL otc AS TypeCode
            LOCAL oType AS System.Type
            LOCAL bData AS BYTE[]
            oType := oValue:GetType()
            otc   := System.Type.GetTypeCode(oType)
            SWITCH oTC
            CASE TypeCode.String
                VAR sValue := (STRING) oValue
                bData := BYTE[] { sValue:Length+8}
                token := FtpMemoToken{bData}
                token:DataType := FlexFieldType.String
                token:Length   := (DWORD) sValue:Length
                VAR bytes := SELF:_encoding:GetBytes(sValue)
                System.Array.Copy(bytes,0, bData,8, bytes:Length)
                RETURN bData
            CASE TypeCode.Boolean
                VAR lValue := (LOGIC) oValue
                bData := BYTE[] { 8}
                token := FtpMemoToken{bData}
                token:Length := 0
                token:DataType := IIF(lValue, FlexFieldType.LogicTrue, FlexFieldType.LogicFalse)
                RETURN bData
            END SWITCH
            RETURN NULL


        INTERNAL METHOD DecodeValue(bData AS BYTE[]) AS OBJECT
            // bData includes the header
            LOCAL encoding  AS Encoding
            LOCAL token AS FtpMemoToken
            LOCAL offset AS LONG
            token := FtpMemoToken{bData}
            encoding := SELF:_Encoding //ASCIIEncoding{}
            SWITCH token:DataType
            CASE FlexFieldType.Array16
            CASE FlexFieldType.Array32
                offset := 8
                RETURN DecodeFlexArray(token:DataType, bData, REF offset)
            CASE FlexFieldType.Picture
            CASE FlexFieldType.OleObject
                VAR buffer := BYTE[]{ bData:Length - 8}
                Array.Copy(bData,8, buffer, 0, buffer:Length)
                RETURN buffer
            CASE FlexFieldType.String
                RETURN encoding:GetString(bData,8, bData:Length-8)
            CASE FlexFieldType.IndexBlock
            CASE FlexFieldType.Delete
            CASE FlexFieldType.Object16
            CASE FlexFieldType.Object32
            CASE FlexFieldType.Nil
                RETURN NULL_OBJECT
            CASE FlexFieldType.LogicTrue
                RETURN TRUE
            CASE FlexFieldType.LogicFalse
                RETURN FALSE
            CASE FlexFieldType.JDate
                RETURN FALSE
            CASE FlexFieldType.SByte
                RETURN (Sbyte) bData[8]
            CASE FlexFieldType.Byte
                RETURN (BYTE) bData[8]
            CASE FlexFieldType.Short
                RETURN FoxToShort(bData, 8)
            CASE FlexFieldType.Word
                RETURN FoxToWord(bData, 8)
            CASE FlexFieldType.Long
                RETURN FoxToLong(bData, 8)
            CASE FlexFieldType.DWord
                RETURN FoxToDword(bData, 8)
            CASE FlexFieldType.Double
                RETURN BitConverter.ToDouble(bData, 8)
            CASE FlexFieldType.Double10
                RETURN 0.0
            CASE FlexFieldType.Compressed
                RETURN ""
            CASE FlexFieldType.StringLong
                RETURN encoding:GetString(bData,8, bData:Length-8)
            CASE FlexFieldType.CompressedLong
                RETURN ""
            CASE FlexFieldType.ItemClipper
                RETURN NULL
            CASE FlexFieldType.LogicLong
                RETURN FoxToLong(bData, 8) != 0
            CASE FlexFieldType.StringEmpty
                RETURN ""
            CASE FlexFieldType.Illegal
                RETURN NIL
            END SWITCH
            RETURN bData




        
        METHOD GetValue(nFldPos AS LONG) AS OBJECT
            IF SELF:_isMemoField( nFldPos )
                // At this level, the return value is the raw Data, in BYTE[]
                VAR rawData := (BYTE[])SUPER:GetValue(nFldPos)
                IF rawData != NULL
                    // So, extract the "real" Data
                    RETURN SELF:DecodeValue(rawData)
                ELSE
                    RETURN String.Empty
                ENDIF
            ENDIF
            RETURN SUPER:GetValue(nFldPos)

        METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
            SELF:ForceRel()
            IF SELF:_readRecord()
                // GoHot() must be called first because this saves the current index values
                IF ! SELF:_Hot
                    SELF:GoHot()
                ENDIF
                VAR oColumn := SELF:_GetColumn(nFldPos)
                IF oColumn:IsMemo
                    IF SELF:HasMemo
                        LOCAL bData AS BYTE[]
                        bData := SELF:EncodeValue(oValue)
                        IF _oMemo:PutValue(nFldPos, bData)
                            // Update the Field Info with the new MemoBlock Position
                            RETURN oColumn:PutValue(SELF:_oMemo:LastWrittenBlockNumber, SELF:_RecordBuffer)
                        ENDIF
                    ELSE
                        RETURN SUPER:PutValue(nFldPos, oValue)
                    ENDIF
                ELSE
                    RETURN oColumn:PutValue(oValue, SELF:_RecordBuffer)
                ENDIF
            ENDIF
            RETURN FALSE
            
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
                IF oNewValue IS STRING VAR strExt
                    FptMemo.DefExt := strExt
                ENDIF
            CASE DbInfo.DBI_MEMOBLOCKSIZE
                oResult := SELF:_oFptMemo:BlockSize
                IF oNewValue != NULL
                    TRY
                        LOCAL size AS LONG
                        size:= Convert.ToInt32(oNewValue)
                        SELF:_oFptMemo:BlockSize := (WORD) size
                    CATCH ex AS exception
                        oResult := ""   
                        SELF:_dbfError(ex, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, __FUNCTION__)
                    END TRY
                ENDIF

            CASE DBInfo.DBI_MEMOFIELD
                oResult := ""
                IF oNewValue != NULL
                    TRY
                        LOCAL fldPos AS LONG
                        fldPos := Convert.ToInt32(oNewValue)
                        oResult := SELF:GetValue(fldPos)
                    CATCH ex AS exception
                        oResult := ""   
                        SELF:_dbfError(ex, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, __FUNCTION__)
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
    // To also support FPT files created with FlexFile we also need to read the FlexFile header and decode that.

    INTERNAL CLASS FPTMemo INHERIT BaseMemo IMPLEMENTS IMemo
        INTERNAL _hFile	    AS IntPtr
        INTERNAL _FileName  AS STRING
        INTERNAL _Open      AS LOGIC
        PROTECT _Shared     AS LOGIC
        PROTECT _ReadOnly   AS LOGIC
        PROTECT _oRDD       AS DBF
        PROTECT _isFlex     AS LOGIC
        PROTECT _blockSize  AS WORD
        PROTECT _lockScheme AS DbfLocking
        PROPERTY Shared AS LOGIC GET _Shared
        PRIVATE _blockData AS BYTE[]
        PRIVATE _fptHeader AS FptHeader
        PRIVATE _flexHeader AS FlexHeader
        PRIVATE _lockCount AS LONG
        INTERNAL CONST FPT_HEADER_SIZE          := 512 AS LONG
        INTERNAL CONST MIN_FOXPRO_BLOCKSIZE     := 32 AS LONG
        INTERNAL CONST FLEX_HEADER_OFFSET       := 512 AS LONG
        INTERNAL CONST FLEX_HEADER_SIZE         := 512 AS LONG
        INTERNAL CONST VERSION_MAJOR := 2   AS LONG
        INTERNAL CONST VERSION_MINOR := 8   AS LONG
        PRIVATE _hotHeader AS LOGIC


        STATIC PROPERTY DefExt AS STRING AUTO
        PROTECTED PROPERTY IsOpen AS LOGIC GET SELF:_hFile != F_ERROR  .AND. SELF:_hFile != IntPtr.Zero      
        INTERNAL PROPERTY IsFlex AS LOGIC GET _isFlex
        STATIC CONSTRUCTOR
            DefExt := FPT_MEMOEXT


        PRIVATE METHOD LockHeader(refreshHeaders AS LOGIC) AS LOGIC
            LOCAL lOk := TRUE AS LOGIC
            IF SELF:Shared
                IF SELF:_lockCount == 0
                    lOk := SELF:_tryLock(0, 1, 10)
                    IF lOk .AND. refreshHeaders
                        SELF:_lockCount := 1
                        IF SELF:ReadHeader()
                            IF SELF:IsFlex
                                // Deal with indexes of deleted blocks
                            ENDIF
                        ELSE
                            SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_READ, GenCode.EG_READ, "FPTMemo.LockHeader")
                        ENDIF
                    ENDIF
                ELSE
                    SELF:_lockCount += 1
                ENDIF
            ELSE
                SELF:_lockCount += 1
            ENDIF
            RETURN lOk

        PRIVATE METHOD UnLockHeader(updated AS LOGIC) AS LOGIC
            IF updated 
               SELF:_hotHeader := TRUE
            ENDIF
            IF SELF:_lockCount == 1
                IF SELF:_hotHeader
                    SELF:WriteHeader()
                ENDIF
                IF SELF:Shared
                    FFlush(SELF:_hFile)
                    IF ! SELF:_unlock(0,1)
                    ENDIF
                ENDIF
                SELF:_lockCount := 0
            ELSE
                SELF:_lockCount -= 1
            ENDIF
            RETURN TRUE

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
            SELF:_blockData := BYTE[]{8}
            SELF:_fptHeader   := FptHeader{}
            SELF:_flexHeader  := FlexHeader{}
            SELF:_lockCount   := 0
            
            
        VIRTUAL PROTECTED METHOD _initContext() AS VOID
            SELF:_lockScheme:Initialize( DbfLockingModel.FoxPro )
            SELF:ReadHeader()
            IF ( SELF:BlockSize == 0 )
                //SELF:BlockSize := FPT_DEFBLOCKSIZE
                SELF:BlockSize := Convert.ToUInt16(XSharp.RuntimeState.MemoBlockSize)
                SELF:WriteHeader()
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
            LOCAL blockLen := 0 AS DWORD
            LOCAL block := NULL AS BYTE[]
            blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
            IF ( blockNbr > 0 )
                // Get the raw Length of the Memo, this included the token
                blockLen := SELF:_getValueLength( nFldPos )
                IF SELF:_setBlockPos(blockNbr)
                    block := BYTE[]{blockLen}
                    // Max 512 Blocks
                    IF FRead3( SELF:_hFile, block, (DWORD)blockLen ) != blockLen 
                        block := NULL
                    ENDIF
                ENDIF
            ENDIF
            // At this level, the return value is the raw Data, in BYTE[]
            RETURN block
            
            /// <inheritdoc />
        METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
            THROW NotImplementedException{}
            
            /// <inheritdoc />
        METHOD GetValueLength(nFldPos AS INT) AS LONG
            VAR blockLen := SELF:_getValueLength( nFldPos )
            // Don't forget to remove the 8 Bytes
            IF blockLen >= 8
                blockLen := blockLen - 8
            ENDIF
            RETURN (LONG) blockLen

        INTERNAL METHOD _setBlockPos(blockNbr AS LONG) AS LOGIC
            // Get the raw data length
            IF blockNbr > 0
                LOCAL iOffset := blockNbr * SELF:_blockSize AS LONG
                // Go to the blockNbr position
                RETURN FSeek3( SELF:_hFile, iOffset, FS_SET ) == iOffSet
            ENDIF
            RETURN FALSE

        VIRTUAL PROTECTED METHOD _getValueLength(nFldPos AS INT) AS DWORD
            // In FPT :
            // The first 4 Bytes contains the type of Memo Data
            // The next 4 Bytes contains the length of Memo data, including the first 8 bytes
            LOCAL blockNbr AS LONG
            LOCAL blockLen := 0 AS DWORD
            // File Open ?
            IF SELF:IsOpen
                // Where does the block start ?
                blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
                IF SELF:_setBlockPos(blockNbr)
                    LOCAL token AS FtpMemoToken
                    token := FtpMemoToken{SELF:_blockData}    
                    token:Read(SELF:_hFile)
                    blockLen     := token:Length+8
                ENDIF
            ENDIF
            RETURN blockLen
            
            /// <inheritdoc />

        PRIVATE METHOD WriteFiller(nToWrite AS DWORD) AS VOID
            LOCAL filler AS BYTE[]
            filler := BYTE[]{(LONG) nToWrite}
            filler[nToWrite-1] := 0xAF
            IF FWrite3(SELF:_hFile, filler, nToWrite) != nToWrite
                SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_WRITE, GenCode.EG_WRITE, "FPTMemo.PutValue")
            ENDIF
            RETURN

        VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            IF !SELF:IsOpen
                RETURN FALSE
            ENDIF
            VAR bytes := oValue ASTYPE BYTE[]
            IF bytes == NULL
                RETURN FALSE
            ENDIF
            LOCAL nCurrentLen AS DWORD
            LOCAL blockNbr AS LONG
            // length including header block
            blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
            IF blockNbr != 0
                nCurrentLen := SELF:_getValueLength(nFldPos)
                IF nCurrentLen >= (DWORD) bytes:Length
                    IF SELF:_setBlockPos(blockNbr)
                        IF SELF:LockHeader(TRUE)
                            IF FWrite3(SELF:_hFile, bytes, (DWORD) bytes:Length) != (DWORD) Bytes:Length
                                SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_WRITE, GenCode.EG_WRITE, "FPTMemo.PutValue")
                            ENDIF
                            // write remainder of block
                            LOCAL nToWrite AS DWORD
                            nToWrite := SELF:CalculateFillerSpace(nCurrentLen)
                            IF nToWrite != 0
                                SELF:WriteFiller(nToWrite)
                            ENDIF
                            SELF:UnLockHeader(TRUE)
                            RETURN TRUE
                        ENDIF
                    ELSE
                        SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_WRITE, GenCode.EG_WRITE, "FPTMemo.PutValue")
                    ENDIF
                ELSE
                    // Deallocate block
                ENDIF
            ELSE
                // Allocate block at the end or from free blocks
                // write the block
                IF SELF:LockHeader(TRUE)
                    LOCAL nPos AS DWORD
                    nPos := SELF:_fptHeader:NextFree * _blockSize
                    fSeek3(SELF:_hFile, (LONG) nPos, FS_SET)
                    IF FWrite3(SELF:_hFile, bytes, (DWORD) bytes:Length) != (DWORD) Bytes:Length
                        SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_WRITE, GenCode.EG_WRITE, "FPTMemo.PutValue")
                    ENDIF
                    LOCAL nToWrite AS DWORD
                    nToWrite := SELF:CalculateFillerSpace((DWORD) Bytes:Length)
                    IF nToWrite != 0
                        SELF:WriteFiller(nToWrite)
                    ENDIF
                    LOCAL nFileSize AS DWORD
                    nFileSize := FTell(SELF:_hFile)
                    SELF:_fptHeader:NextFree    := nFileSize / _blockSize
                    SELF:LastWrittenBlockNumber := (LONG) (nPos / _blockSize )
                    SELF:UnLockHeader(TRUE)
                ENDIF
                RETURN TRUE
            ENDIF
            // TODO: write the value to the file, including:
            // - check if the current block exists
            // - check if the current block has enough space
            // - add the current block to the 'deleted blocks' list when not enough space
            // - allocate a new block from either the list of deleted blocks or at the end of the file
            // - and of course locking the file.
            RETURN FALSE
            
            /// <inheritdoc />
        VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
            THROW NotImplementedException{}
            
            /// <inheritdoc />
        VIRTUAL METHOD CloseMemFile( ) AS LOGIC
            LOCAL isOk := FALSE AS LOGIC
            IF SELF:IsOpen
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

        METHOD RoundToBlockSize(nSize AS DWORD) AS DWORD
            IF SELF:_blockSize > 1
                VAR nDiff := nSize % _blockSize
                IF nDiff != 0
                    nSize += _blockSize - nDiff
                ENDIF
            ENDIF
            RETURN nSize
        METHOD CalculateFillerSpace(nSize AS DWORD) AS DWORD
            IF SELF:_blockSize > 1
                LOCAL nToFill AS DWORD
                nToFill := nSize %  SELF:_blockSize
                IF nToFill > 0
                    nToFill := SELF:_blockSize - nToFill
                    RETURN  nToFill
                ENDIF
            ENDIF
            RETURN 0U

            /// <inheritdoc />
        VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
            LOCAL isOk      AS LOGIC
            SELF:_FileName  := info:FileName
            VAR cExt        := GetMemoExtFromDbfExt(SELF:_FileName)
            SELF:_FileName  := System.IO.Path.ChangeExtension( SELF:_FileName, cExt )
            SELF:_Shared    := info:Shared
            SELF:_ReadOnly  := info:ReadOnly
            //
            SELF:_hFile     := FCreate( SELF:_FileName) 
            isOk := SELF:IsOpen
            IF isOk
                
                IF ! SELF:_fptHeader:Write(SELF:_hFile)
                    SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_CREATE_MEMO, GenCode.EG_WRITE, "FPTMemo.CreateMemFile")
                ENDIF
                SELF:_flexHeader:Create()
                IF ! SELF:_flexHeader:Write(SELF:_hFile)
                    SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_CREATE_MEMO, GenCode.EG_WRITE, "FPTMemo.CreateMemFile")
                ENDIF
                SELF:_initContext()
                SELF:_fptHeader:NextFree :=  RoundToBlockSize(_fptHeader:Size + _flexHeader:Size) / _blockSize
                SELF:WriteHeader()
            ELSE
                SELF:_oRDD:_DbfError( ERDD.CREATE_MEMO, XSharp.Gencode.EG_CREATE )
            ENDIF
            
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
                IF SELF:LockHeader(FALSE)
                    SELF:_initContext()
                    SELF:UnLockHeader(FALSE)
                ENDIF
            ELSE
                SELF:_oRDD:_DbfError( ERDD.OPEN_MEMO, XSharp.Gencode.EG_OPEN )
                isOk := FALSE
            ENDIF
            //
            RETURN isOk
            
        VIRTUAL PROPERTY BlockSize 	 AS WORD
            GET
                RETURN _BlockSize
            END GET
            SET
                _blockSize := VALUE
                IF VALUE >= MIN_FOXPRO_BLOCKSIZE
                    SELF:_fptHeader:Blocksize := VALUE
                ENDIF
                IF SELF:_isFlex
                    IF VALUE >= MIN_FOXPRO_BLOCKSIZE
                        SELF:_flexHeader:AltBlockSize := 0
                    ELSE
                        SELF:_flexHeader:AltBlockSize := VALUE
                    ENDIF
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
                    SELF:_oRDD:_dbfError(ex, SubCodes.ERDD_INIT_LOCK, GenCode.EG_LOCK_ERROR, "FPTMemo._tryLock")
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
                SELF:_oRDD:_dbfError(ex, SubCodes.ERDD_UNLOCKED, GenCode.EG_UNLOCKED, "FPTMemo._unlock")
                
            END TRY
            RETURN unlocked


        VIRTUAL METHOD Zap() AS LOGIC
            IF SELF:IsOpen
                IF SELF:Shared
                    SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_SHARED, GenCode.EG_LOCK, "FPTMemo.Zap")
                ENDIF
                IF ! FChSize(SELF:_hFile, 0)
                    SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_WRITE, GenCode.EG_WRITE, "FPTMemo.Zap")
                ENDIF
                SELF:WriteHeader()
                RETURN TRUE
            ELSE
                SELF:_oRDD:_dbfError(FException(), SubCodes.EDB_NOTABLE, GenCode.EG_NOTABLE, "FPTMemo.Zap")
            ENDIF
            RETURN FALSE

        PRIVATE METHOD ReadHeader() AS LOGIC
            LOCAL savedPos := FSeek3(SELF:_hFile, 0, FS_RELATIVE ) AS LONG
            LOCAL nFileLen AS LONG
            nFileLen := FSeek3(SELF:_hFile, 0, FS_END)
            IF ! SELF:_fptHeader:Read(SELF:_hFile)
                SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_READ, GenCode.EG_READ, "FPTMemo.ReadHeader")
            ENDIF
            _blockSize := SELF:_fptHeader:BlockSize
            // read Flex Header
            IF nFileLen >= 1024
                IF ! SELF:_flexHeader:Read(SELF:_hFile)
                     SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_READ, GenCode.EG_READ, "FPTMemo.ReadHeader")
                ENDIF
                _isFlex := SELF:_flexHeader:Valid
                 SELF:_blockSize     := SELF:_flexHeader:AltBlockSize
            ELSE
                _isFlex := FALSE
            ENDIF
            FSeek3(SELF:_hFile, savedPos, FS_SET )
            RETURN TRUE

        METHOD WriteHeader() AS VOID
            IF SELF:IsOpen
                IF ! SELF:_fptHeader:Write(SELF:_hFile)
                    SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_WRITE, GenCode.EG_WRITE, "FPTMemo.WriteHeader")
                ENDIF
                // write flex header
                IF SELF:IsFlex
                    IF ! SELF:_flexHeader:Write(SELF:_hFile)
                        SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_WRITE, GenCode.EG_WRITE, "FPTMemo.WriteHeader")
                     ENDIF
                ENDIF
            ENDIF
            RETURN
   

    END CLASS    
    
    
    END NAMESPACE
