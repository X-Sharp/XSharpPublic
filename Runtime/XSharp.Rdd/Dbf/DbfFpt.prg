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
BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFFPT RDD. For DBF/FPT. No index support at this level</summary>
    CLASS DBFFPT INHERIT DBF 
        PRIVATE _oFptMemo AS FPTMemo
        PROTECTED INTERNAL _iExportMode AS LONG
        CONSTRUCTOR   
            SUPER()
            SELF:_Memo := _oFptMemo := FPTMemo{SELF}
            SELF:_iExportMode := BLOB_EXPORT_APPEND
            
            /// <inheritdoc />	
        PROPERTY Driver AS STRING GET "DBFFPT"


        INTERNAL METHOD DecodeFlexArray(nType AS FlexFieldType, bData AS BYTE[], nOffset REF LONG) AS OBJECT
            LOCAL iLen AS Int32
            LOCAL aValues AS OBJECT[]
            IF nType == FlexFieldType.Array16 // 16 bits
                iLen := BitConverter.ToInt16(bData, nOffset)
                nOffset += 2
            ELSEIF nType == FlexFieldType.Array32
                iLen := BitConverter.ToInt32(bData, nOffset)
                nOffset += 4
            ELSE
                RETURN NULL
            ENDIF
            aValues := OBJECT[]{iLen}
            FOR VAR i := 0 TO iLen-1
                VAR nFldType := bData[nOffset]
                LOCAL element AS OBJECT
                LOCAL length AS LONG
                nOffset += 1
                VAR nArrType := (FlexArrayTypes) nFldType
                SWITCH nArrType
                CASE FlexArrayTypes.NIL
                    element := DBNull.Value
                CASE FlexArrayTypes.Char
                    element := (SByte) bData[ nOffset]
                    nOffset += 1
                CASE FlexArrayTypes.UChar
                    element := (BYTE) bData[ nOffset]
                    nOffset += 1
                CASE FlexArrayTypes.Short
                    element := BitConverter.ToInt16(bData,nOffset)
                    nOffset += 2
                CASE FlexArrayTypes.UShort
                    element := BitConverter.ToUInt16(bData,nOffset)
                    nOffset += 2
                CASE FlexArrayTypes.Long     
                    element := BitConverter.ToInt32(bData,nOffset)
                    nOffset += 4
                CASE FlexArrayTypes.String32 
                    length := BitConverter.ToInt32(bData,nOffset)
                    nOffset += 4
                    element := _Encoding:GetString(bData, nOffset, length)
                    nOffset += length
                CASE FlexArrayTypes.String16 
                    length := BitConverter.ToInt16(bData,nOffset)
                    nOffset += 2
                    element := _Encoding:GetString(bData, nOffset, length)
                    nOffset += length
                CASE FlexArrayTypes.Float    
                    element := 0.0
                    nOffset += 10
                CASE FlexArrayTypes.Double 
                    element := BitConverter.ToDouble(bData, nOffset)
                    nOffset += 8
                CASE FlexArrayTypes.Date     
                    element := BitConverter.ToInt32(bData, nOffset)
                    nOffset += 4
                CASE FlexArrayTypes.Logic    
                    element := bData[nOffset] != 0
                    nOffset += 1
                CASE FlexArrayTypes.Array    
                    element := DecodeFlexArray(FlexFieldType.Array16, bData, REF nOffset)

                CASE FlexArrayTypes.CodeBlock
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.DateJ    
                    element := BitConverter.ToInt32(bData, nOffset)
                    nOffset += 4

                CASE FlexArrayTypes.Double2  
                    element := BitConverter.ToDouble(bData, nOffset)
                    nOffset += 6

                CASE FlexArrayTypes.Cyclic
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.UCHar1  
                    element := (SByte) bData[ nOffset]
                    nOffset += 2

                CASE FlexArrayTypes.Char1    
                    element := (BYTE) bData[ nOffset]
                    nOffset += 2

                CASE FlexArrayTypes.Short1   
                    element := BitConverter.ToInt16(bData, nOffset)
                    nOffset += 3
                CASE FlexArrayTypes.UShort1  
                    element := BitConverter.ToUInt16(bData, nOffset)
                    nOffset += 3
                CASE FlexArrayTypes.Long1  
                    element := BitConverter.ToInt32(bData, nOffset)
                    nOffset += 5
                CASE FlexArrayTypes.Unused
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.Object
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.Null     
                    element := String.Empty

                CASE FlexArrayTypes.True     
                    element := TRUE

                CASE FlexArrayTypes.False    
                    element := FALSE

                CASE FlexArrayTypes.LDouble
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)
                CASE FlexArrayTypes.UCHar2   
                    element := (SByte) bData[ nOffset]
                    nOffset += 3
                CASE FlexArrayTypes.CHar2    
                    element := (BYTE) bData[ nOffset]
                    nOffset += 3
                CASE FlexArrayTypes.Short2   
                    element := BitConverter.ToInt16(bData, nOffset)
                    nOffset += 4
                CASE FlexArrayTypes.UShort2  
                    element := BitConverter.ToUInt16(bData, nOffset)
                    nOffset += 4
                CASE FlexArrayTypes.Long2    
                    element := BitConverter.ToInt32(bData, nOffset)
                    nOffset += 6
                CASE FlexArrayTypes.ULong2   
                    element := BitConverter.ToUInt32(bData, nOffset)
                    nOffset += 6
                OTHERWISE
                    element := NULL
                    SELF:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)
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
            SWITCH otc
            CASE TypeCode.String
                VAR sValue := (STRING) oValue
                bData := BYTE[] { sValue:Length+8}
                token := FtpMemoToken{bData}
                token:DataType := FlexFieldType.String
                token:Length   := (DWORD) sValue:Length
                VAR bytes := SELF:_Encoding:GetBytes(sValue)
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
            CASE FlexFieldType.StringLong
               // Some drivers are stupid enough to allocate blocks in the FPT with a zero length..        
                IF token:Length > 0
                    IF bData[bData:Length-1] == 0
                        RETURN encoding:GetString(bData,8, bData:Length-9)
                    ELSE
                        RETURN encoding:GetString(bData,8, bData:Length-8)
                    ENDIF
                ENDIF
                RETURN ""
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
                RETURN (SByte) bData[8]
            CASE FlexFieldType.Byte
                RETURN (BYTE) bData[8]
            CASE FlexFieldType.Short
                RETURN FoxToShort(bData, 8)
            CASE FlexFieldType.Word
                RETURN FoxToWord(bData, 8)
            CASE FlexFieldType.Long
                RETURN FoxToLong(bData, 8)
            CASE FlexFieldType.Dword
                RETURN FoxToDword(bData, 8)
            CASE FlexFieldType.Double
                RETURN BitConverter.ToDouble(bData, 8)
            CASE FlexFieldType.Double10
                RETURN 0.0
            CASE FlexFieldType.Compressed
                RETURN ""
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
            IF SELF:_ReadOnly
                SELF:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY )
            ENDIF
            SELF:ForceRel()
            IF SELF:_readRecord()
                // GoHot() must be called first because this saves the current index values
                IF ! SELF:_Hot
                    SELF:GoHot()
                ENDIF
                VAR oColumn := SELF:_GetColumn(nFldPos)
                IF oColumn != NULL
                    IF oColumn:IsMemo
                        IF SELF:HasMemo
                            LOCAL bData AS BYTE[]
                            bData := SELF:EncodeValue(oValue)
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
        VIRTUAL METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
            LOCAL oResult := NULL AS OBJECT
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
                // oNewValue should be object[] with 3 elements
                TRY
                    IF oNewValue IS OBJECT[] VAR oArray
                        IF oArray:Length >= 3
                            VAR nFld    := Convert.ToInt32(oArray[0])
                            VAR nOffset := Convert.ToInt32(oArray[1])
                            VAR nLen    := Convert.ToInt32(oArray[2])
                            VAR rawData := (BYTE[])SUPER:GetValue(nFld)
                            IF rawData != NULL .AND. rawData:Length > 8 // 1st 8 bytes are the header
                                VAR nDataLen := rawData:Length -8
                                nOffset += 8
                                IF nOffset <= rawData:Length 
                                    VAR nToCopy := nLen
                                    IF nToCopy == 0
                                        nToCopy := nDataLen
                                    ELSEIF nToCopy > nDataLen - nOffset + 1
                                        nToCopy := nDataLen - nOffset + 1
                                    ENDIF
                                    VAR result  := BYTE[]{nToCopy}
                                    System.Array.Copy(rawData, nOffset, result,0, nToCopy)
                                    oResult := SELF:_Encoding:GetString(result,0, nToCopy)
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF
                CATCH ex AS Exception
                    SELF:_dbfError(ex, Subcodes.ERDD_READ, Gencode.EG_CORRUPTION, "DBFFPT.BlobGet")
                END TRY
            CASE DbInfo.BLOB_NMODE
                IF oNewValue IS LONG VAR iExportMode
                    SELF:_iExportMode := iExportMode
                ENDIF
            OTHERWISE
                oResult := SUPER:Info(nOrdinal, oNewValue)
            END SWITCH
            RETURN oResult
            
        VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
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
            
            
    /// <summary>FPT Memo class. Implements the FTP support.</summary>
    // To also support FPT files created with FlexFile we also need to read the FlexFile header and decode that.

    INTERNAL CLASS FPTMemo INHERIT BaseMemo IMPLEMENTS IMemo
        INTERNAL _hFile	    AS IntPtr
        INTERNAL _FileName  AS STRING
        INTERNAL _Open      AS LOGIC
        PROTECT _Shared     AS LOGIC
        PROTECT _ReadOnly   AS LOGIC
        PROTECT _oRDD       AS DBFFPT
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
        PROPERTY Encoding AS Encoding GET SELF:_oRDD:_Encoding
        STATIC CONSTRUCTOR
            DefExt := FPT_MEMOEXT


        PRIVATE METHOD LockHeader(refreshHeaders AS LOGIC) AS LOGIC
            LOCAL lOk := TRUE AS LOGIC
            IF SELF:Shared
                IF SELF:_lockCount == 0
                    lOk := SELF:_tryLock(0, 1, 10)
                    IF lOk
                        SELF:_lockCount := 1
                        IF refreshHeaders
                            IF SELF:ReadHeader()
                                IF SELF:IsFlex
                                    // Deal with indexes of deleted blocks
                                ENDIF
                            ELSE
                                SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_READ, Gencode.EG_READ, "FPTMemo.LockHeader")
                            ENDIF
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
            SELF:_oRDD := (DBFFPT) oRDD
            SELF:_hFile := IntPtr.Zero
            SELF:_Shared := SELF:_oRDD:_Shared
            SELF:_ReadOnly := SELF:_oRDD:_ReadOnly
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
		        IF blockLen != UInt32.MaxValue
	                IF SELF:_setBlockPos(blockNbr)
        	            block := BYTE[]{blockLen}
                	    IF FRead3( SELF:_hFile, block, (DWORD)blockLen ) != blockLen 
                        	block := NULL
	                    ENDIF
			        ENDIF
                ELSE
                    SELF:_oRDD:_dbfError(Subcodes.ERDD_READ, Gencode.EG_CORRUPTION, "FPTMemo.GetValue")
                ENDIF
            ENDIF
            // At this level, the return value is the raw Data, in BYTE[]
            RETURN block
            
            /// <inheritdoc />
         OVERRIDE METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
          IF SELF:_oRDD:_isMemoField( nFldPos )
                // At this level, the return value is the raw Data, in BYTE[]
                TRY
                    IF File(fileName)
                        fileName := FPathName()
                    ENDIF
                    VAR rawData := (BYTE[])SELF:GetValue(nFldPos)
                    IF rawData != NULL
                        // So, extract the "real" Data
                        IF SELF:_oRDD:_iExportMode == BLOB_EXPORT_APPEND
                            LOCAL file := System.IO.File.OpenWrite(fileName) AS FileStream
                            file:Seek(0, SeekOrigin.End)
                            file:Write(rawData, 8, rawData:Length-8)
                            file:Close()
                        ELSE
                            LOCAL file := System.IO.File.Create(fileName) AS FileStream
                            file:Write(rawData, 8, rawData:Length-8)
                            file:Close()
                        ENDIF
                        RETURN TRUE
                    ENDIF
                CATCH ex AS Exception
                    SELF:_oRDD:_dbfError(ex, Subcodes.ERDD_READ, Gencode.EG_READ, "DBFFPT.GetValueFile")
                END TRY
            ENDIF
            RETURN SUPER:GetValueFile(nFldPos, fileName)
            
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
                RETURN FSeek3( SELF:_hFile, iOffset, FS_SET ) == iOffset
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
                    IF token:Read(SELF:_hFile)
                        blockLen     := token:Length+8
                    ELSE
                        blockLen     := UInt32.MaxValue
                    ENDIF
                ENDIF
            ENDIF
            RETURN blockLen
            
            /// <inheritdoc />

        PRIVATE METHOD WriteFiller(nToWrite AS DWORD, lDeleted AS LOGIC) AS VOID
            LOCAL filler AS BYTE[]
            LOCAL fillByte AS BYTE
            fillByte := (BYTE) IIF(lDeleted, 0xF0, 0xAF)
            filler := BYTE[]{(LONG) nToWrite}
            FOR VAR i := 0 TO nToWrite-1
                filler[i] := fillByte
            NEXT
            IF FWrite3(SELF:_hFile, filler, nToWrite) != nToWrite
                SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.PutValue")
            ENDIF
            RETURN

        PRIVATE METHOD DeleteBlock(blockNbr AS LONG) AS VOID
            // Todo: add deleted block to FlexFile deleted blocks list
            IF SELF:_setBlockPos(blockNbr)
                VAR block := BYTE[]{8}
                FRead3( SELF:_hFile, block, 8) 
                LOCAL token AS FtpMemoToken
                token := FtpMemoToken{block}
                token:DataType := FlexFieldType.Delete
                // Adjust the length to the whole of the block. Flexfile also does that
                token:Length   := SELF:RoundToBlockSize(token:Length +8)  - 8
                IF SELF:LockHeader(TRUE)
                    SELF:_setBlockPos(blockNbr)
                    IF FWrite3(SELF:_hFile, block, 8) != 8
                        SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.PutValue")
                    ENDIF
                    // Clear the data. FlexFiles does not do that, but I think it's better to clean up.
                    SELF:WriteFiller(token:Length, TRUE)
                    SELF:UnLockHeader(TRUE)
                ENDIF
            ENDIF
            RETURN

        METHOD _WriteBlock (bytes AS BYTE[]) AS LOGIC
            IF FWrite3(SELF:_hFile, bytes, (DWORD) bytes:Length) != (DWORD) bytes:Length
                SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.PutValue")
            ENDIF
            // write remainder of block
            LOCAL nToWrite AS DWORD
            nToWrite := SELF:CalculateFillerSpace((DWORD) bytes:Length)
            IF nToWrite != 0
                SELF:WriteFiller(nToWrite, FALSE)
            ENDIF
            RETURN TRUE

        VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            IF !SELF:IsOpen
                RETURN FALSE
            ENDIF
            VAR bytes := oValue ASTYPE BYTE[]
            IF bytes == NULL
                RETURN FALSE
            ENDIF
            // AT this level the bytes[] array already contains the header with type and length
            LOCAL nCurrentLen AS DWORD
            LOCAL blockNbr AS LONG
            LOCAL lNewBlock := FALSE AS LOGIC
            // length including header block
            blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
            IF blockNbr != 0
                nCurrentLen := SELF:_getValueLength(nFldPos)
                nCurrentLen := SELF:RoundToBlockSize(nCurrentLen)
                VAR needed := SELF:RoundToBlockSize((DWORD) bytes:Length)
                IF nCurrentLen >= needed
                    IF SELF:_setBlockPos(blockNbr)
                        IF SELF:LockHeader(TRUE)
                            SELF:_WriteBlock(bytes)
                            SELF:UnLockHeader(TRUE)
                            SELF:LastWrittenBlockNumber := blockNbr
                            RETURN TRUE
                        ENDIF
                    ELSE
                        SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.PutValue")
                    ENDIF
                ELSE
                    // Deallocate block and allocate new
                    DeleteBlock(blockNbr)
                    lNewBlock := TRUE
                ENDIF
            ELSE
                // Allocate block at the end or from free blocks
                // write the block
                lNewBlock := TRUE
            ENDIF
            IF lNewBlock
                IF SELF:LockHeader(TRUE)
                    LOCAL nPos AS DWORD
                    nPos := SELF:_fptHeader:NextFree * _blockSize
                    FSeek3(SELF:_hFile, (LONG) nPos, FS_SET)
                    SELF:_WriteBlock(bytes)
                    LOCAL nFileSize AS DWORD
                    nFileSize := FTell(SELF:_hFile)
                    SELF:_fptHeader:NextFree    := nFileSize / _blockSize
                    SELF:LastWrittenBlockNumber := (LONG) (nPos / _blockSize )
                    SELF:UnLockHeader(TRUE)
                ENDIF
                RETURN TRUE
            ENDIF
            RETURN FALSE
            
            /// <inheritdoc />
        VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
            TRY
                VAR oColumn := SELF:_oRDD:_GetColumn(nFldPos)
                IF oColumn != NULL .AND. oColumn:IsMemo 
                    LOCAL bFile AS BYTE[]
                    IF File(fileName)
                        fileName := FPathName()
                        bFile := System.IO.File.ReadAllBytes(fileName)
                        VAR bData := BYTE[] { bFile:Length+8}
                        VAR token := FtpMemoToken{bData}
                        IF bFile:Length > UInt16.MaxValue
                            token:DataType := FlexFieldType.StringLong
                        ELSE
                            token:DataType := FlexFieldType.String
                        ENDIF
                        token:Length   := (DWORD) bFile:Length
                        System.Array.Copy(bFile,0, bData,8, bFile:Length)
                        IF SELF:PutValue(nFldPos, bData)
                            // Update the Field Info with the new MemoBlock Position
                            RETURN oColumn:PutValue(SELF:LastWrittenBlockNumber, SELF:_oRDD:_RecordBuffer)
                        ENDIF
                    ENDIF
                ENDIF
            CATCH ex AS Exception
                SELF:_oRDD:_dbfError(ex, Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "DBFFPT.PutValueFile")
            END TRY
            RETURN FALSE            /// <inheritdoc />
        VIRTUAL METHOD CloseMemFile( ) AS LOGIC
            LOCAL isOk := FALSE AS LOGIC
            IF SELF:IsOpen
                TRY
                    isOk := FClose( SELF:_hFile )

                CATCH ex AS Exception
                    isOk := FALSE
                    SELF:_oRDD:_dbfError(ex, Subcodes.ERDD_CLOSE_MEMO, Gencode.EG_CLOSE, "DBFDBT.CloseMemFile")
                    
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
                    SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_CREATE_MEMO, Gencode.EG_WRITE, "FPTMemo.CreateMemFile")
                ENDIF
                SELF:_flexHeader:Create()
                IF ! SELF:_flexHeader:Write(SELF:_hFile)
                    SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_CREATE_MEMO, Gencode.EG_WRITE, "FPTMemo.CreateMemFile")
                ENDIF
                SELF:_initContext()
                SELF:_fptHeader:NextFree :=  RoundToBlockSize(_fptHeader:Size + _flexHeader:Size) / _blockSize
                SELF:WriteHeader()
            ELSE
                SELF:_oRDD:_dbfError( ERDD.CREATE_MEMO, XSharp.Gencode.EG_CREATE )
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
            SELF:_hFile     := FOpen(SELF:_FileName, info:FileMode)
            isOk := SELF:IsOpen
            IF isOk
                // Per default, Block Size if 512
                IF SELF:LockHeader(FALSE)
                    SELF:_initContext()
                    SELF:UnLockHeader(FALSE)
                ENDIF
            ELSE
                SELF:_oRDD:_dbfError( ERDD.OPEN_MEMO, XSharp.Gencode.EG_OPEN )
                isOk := FALSE
            ENDIF
            //
            RETURN isOk
            
        VIRTUAL PROPERTY BlockSize 	 AS WORD
            GET
                RETURN _blockSize
            END GET
            SET
                _blockSize := value
                IF value >= MIN_FOXPRO_BLOCKSIZE
                    SELF:_fptHeader:BlockSize := value
                ENDIF
                IF SELF:_isFlex
                    IF value >= MIN_FOXPRO_BLOCKSIZE
                        SELF:_flexHeader:AltBlockSize := 0
                    ELSE
                        SELF:_flexHeader:AltBlockSize := value
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
                    SELF:_oRDD:_dbfError(ex, Subcodes.ERDD_INIT_LOCK, Gencode.EG_LOCK_ERROR, "FPTMemo._tryLock")
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
                SELF:_oRDD:_dbfError(ex, Subcodes.ERDD_UNLOCKED, Gencode.EG_UNLOCKED, "FPTMemo._unlock")
                
            END TRY
            RETURN unlocked


        VIRTUAL METHOD Zap() AS LOGIC
            IF SELF:IsOpen
                IF SELF:Shared
                    SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_SHARED, Gencode.EG_LOCK, "FPTMemo.Zap")
                ENDIF
                IF ! FChSize(SELF:_hFile, 0)
                    SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.Zap")
                ENDIF
                SELF:WriteHeader()
                RETURN TRUE
            ELSE
                SELF:_oRDD:_dbfError(FException(), Subcodes.EDB_NOTABLE, Gencode.EG_NOTABLE, "FPTMemo.Zap")
            ENDIF
            RETURN FALSE

        PRIVATE METHOD ReadHeader() AS LOGIC
            LOCAL savedPos := FSeek3(SELF:_hFile, 0, FS_RELATIVE ) AS LONG
            LOCAL nFileLen AS LONG
            nFileLen := FSeek3(SELF:_hFile, 0, FS_END)
            IF ! SELF:_fptHeader:Read(SELF:_hFile)
                SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_READ, Gencode.EG_READ, "FPTMemo.ReadHeader")
            ENDIF
            _blockSize := SELF:_fptHeader:BlockSize
            // read Flex Header
            IF nFileLen >= 1024
                IF ! SELF:_flexHeader:Read(SELF:_hFile)
                     SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_READ, Gencode.EG_READ, "FPTMemo.ReadHeader")
                ENDIF
                _isFlex := SELF:_flexHeader:Valid
                if _blockSize == 0 .and. SELF:_flexHeader:AltBlockSize != 0
                    SELF:_blockSize     := SELF:_flexHeader:AltBlockSize
                endif
            ELSE
                _isFlex := FALSE
            ENDIF
            FSeek3(SELF:_hFile, savedPos, FS_SET )
            RETURN TRUE

        METHOD WriteHeader() AS VOID
            IF SELF:IsOpen .and. ! SELF:_oRDD:_ReadOnly
                IF ! SELF:_fptHeader:Write(SELF:_hFile)
                    SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.WriteHeader")
                ENDIF
                // write flex header
                IF SELF:IsFlex
                    IF ! SELF:_flexHeader:Write(SELF:_hFile)
                        SELF:_oRDD:_dbfError(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.WriteHeader")
                     ENDIF
                ENDIF
            ENDIF
            RETURN
   

    END CLASS    
    
    
    END NAMESPACE
