//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING XSharp.RDD.CDX


    BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFFPT RDD. For DBF/FPT. No index support at this level</summary>
    CLASS DBFFPT INHERIT DBF 
        PRIVATE _oFptMemo AS FptMemo
        CONSTRUCTOR   
            SUPER()
            SELF:_oMemo := _oFptMemo := FptMemo{SELF}
            /// <inheritdoc />	
        PROPERTY Driver AS STRING GET "DBFFPT"



    
        /*
        METHOD DecodeFlexArray(nType as LONG, bData as byte[]) as object
            local iLen as Int32
            local aValues as Object[]
            LOCAL nOffSet   as Int32
            local nDim as Int16
            nDim    := bData[ 0]
            nOffSet := 1
            IF nType == 1002 // 16 bits
                iLen := FoxToShort(bData, nOffSet)
                nOffSet += 2
            ELSE
                iLen := FoxToLong(bData, nOffSet)
                nOffSet += 4
            ENDIF
            aValues := OBJECT[]{iLen}
            FOR VAR i := 0 to iLen-1
                var nFldType := bData[nOffSet]
                local element as Object
                local length as Long
                nOffSet += 1
                VAR nArrType := (FlexArrayTypes) nFldType
                SWITCH nArrType
                CASE FlexArrayTypes.NIL
                    element := NULL
                CASE FlexArrayTypes.Char
                    element := (sByte) bData[ nOffSet]
                    nOffSet += 1
                CASE FlexArrayTypes.UChar
                    element := (Byte) bData[ nOffSet]
                    nOffSet += 1
                CASE FlexArrayTypes.Short
                    element := FoxToShort(bData,nOffSet)
                    nOffSet += 2
                CASE FlexArrayTypes.UShort
                    element := FoxToWord(bData,nOffSet)
                    nOffSet += 2
                CASE FlexArrayTypes.Long     
                    element := BitConverter.ToUInt32(bData,nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.String32 
                    length := BitConverter.ToUInt32(bData,nOffSet)
                    nOffSet += 4
                    element := _encoding:GetString(bData, nOffSet, length)
                    nOffSet += length
                CASE FlexArrayTypes.String16 
                    length := BitConverter.ToUInt16(bData,nOffSet)
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
                    element := bData[nOffSet] != 0
                    nOffSet += 1

                CASE FlexArrayTypes.CodeBlock 
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")

                CASE FlexArrayTypes.DateJ    
                    element := BitConverter.ToInt32(bData, nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.Double2  
                    element := BitConverter.ToDouble(bData, nOffSet)
                    nOffSet += 6

                CASE FlexArrayTypes.Cyclic   
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                CASE FlexArrayTypes.UCHar1  
                    element := (sByte) bData[ nOffSet]
                    nOffSet += 2
                CASE FlexArrayTypes.Char1    
                    element := (Byte) bData[ nOffSet]
                    nOffSet += 2
                CASE FlexArrayTypes.Short1   
                    element := FoxToShort(bData, nOffSet)
                    nOffSet += 3
                CASE FlexArrayTypes.UShort1  
                    element := FoxToWord(bData, nOffSet)
                    nOffSet += 3
                CASE FlexArrayTypes.Long1  
                    element := FoxToLong(bData, nOffSet)
                    nOffSet += 5
                CASE FlexArrayTypes.Unused   
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                CASE FlexArrayTypes.Object   
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                CASE FlexArrayTypes.Null     
                    element := NULL
                CASE FlexArrayTypes.True     
                    element := TRUE
                CASE FlexArrayTypes.False    
                    element := FALSE
                CASE FlexArrayTypes.LDouble
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                CASE FlexArrayTypes.UCHar2   
                    element := (SByte) bData[ nOffSet]
                    nOffSet += 3
                CASE FlexArrayTypes.CHar2    
                    element := (Byte) bData[ nOffSet]
                    nOffSet += 3
                CASE FlexArrayTypes.Short2   
                    element := FoxToShort(bData, nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.UShort2  
                    element := FoxToWord(bData, nOffSet)
                    nOffSet += 4
                CASE FlexArrayTypes.Long2    
                    element := FoxToLong(bData, nOffSet)
                    nOffSet += 6
                CASE FlexArrayTypes.ULong2   
                    element := FoxToDWord(bData, nOffSet)
                    nOffSet += 6
                OTHERWISE
                    SELF:_dbfError(null, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
                END SWITCH
                aValues[i] := element
            NEXT
            RETURN aValues
            */

        INTERNAL METHOD DecodeValue(bData as BYTE[]) as object
            // bData includes the header
            LOCAL encoding  as Encoding
            local token as FtpMemoToken
            token := FtpMemoToken{bData}
            encoding := SELF:_Encoding //ASCIIEncoding{}
            SWITCH token:DataType
            CASE FlexFieldType.Array16
                //return DecodeFlexArray(nType, bData)
            CASE FlexFieldType.Array32
                //return DecodeFlexArray(nType, bData)
            CASE FlexFieldType.Picture
            CASE FlexFieldType.OleObject
                VAR buffer := BYTE[]{ bData:Length - 8}
                Array.Copy(bData,8, buffer, 0, buffer:Length)
                return buffer
            CASE FlexFieldType.String
                return encoding:GetString(bData,8, bData:Length-8)
            CASE FlexFieldType.IndexBlock
            CASE FlexFieldType.Delete
            CASE FlexFieldType.Object16
            CASE FlexFieldType.Object32
            CASE FlexFieldType.Nil
                return NULL_OBJECT
            CASE FlexFieldType.LogicTrue
                return TRUE
            CASE FlexFieldType.LogicFalse
                return FALSE
            CASE FlexFieldType.JDate
                return FALSE
            CASE FlexFieldType.SByte
                return (Sbyte) bData[8]
            CASE FlexFieldType.Byte
                return (byte) bData[8]
            CASE FlexFieldType.Short
                return FoxToShort(bData, 8)
            CASE FlexFieldType.Word
                return FoxToWord(bData, 8)
            CASE FlexFieldType.Long
                return FoxToLong(bData, 8)
            CASE FlexFieldType.DWord
                return FoxToDword(bData, 8)
            CASE FlexFieldType.Double
                return BitConverter.ToDouble(bData, 8)
            CASE FlexFieldType.Double10
                return 0.0
            CASE FlexFieldType.Compressed
                return ""
            CASE FlexFieldType.StringLong
                return encoding:GetString(bData,8, bData:Length-8)
            CASE FlexFieldType.CompressedLong
                return ""
            case FlexFieldType.ItemClipper
                return NULL
            case FlexFieldType.LogicLong
                return FoxToLong(bData, 8) != 0
            case FlexFieldType.StringEmpty
                return ""
            case FlexFieldType.Illegal
                return NIL
            end switch
            RETURN bData


        
        METHOD GetValue(nFldPos AS LONG) AS OBJECT
            IF SELF:_isMemoField( nFldPos )
                // At this level, the return value is the raw Data, in BYTE[]
                var rawData := (BYTE[])SUPER:GetValue(nFldPos)
                IF rawData != NULL
                    // So, extract the "real" Data
                    RETURN SELF:DecodeValue(rawData)
                ELSE
                    RETURN String.Empty
                ENDIF
            ENDIF
            RETURN SUPER:GetValue(nFldPos)
            
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
                IF oNewValue != NULL
                    TRY
                        LOCAL size AS LONG
                        size:= Convert.ToInt32(oNewValue)
                        SELF:_oFptMemo:BlockSize := (WORD) size
                    CATCH ex AS exception
                        oResult := ""   
                        SELF:_dbfError(ex, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBFDBT.Info")
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
    // To also support FPT files created with FlexFile we also need to read the FlexFile header and decode that.

    INTERNAL CLASS FPTMemo INHERIT BaseMemo IMPLEMENTS IMemo
        INTERNAL _hFile	    AS IntPtr
        INTERNAL _FileName  AS STRING
        INTERNAL _Open      AS LOGIC
        PROTECT _Shared     AS LOGIC
        PROTECT _ReadOnly   AS LOGIC
        PROTECT _oRDD       AS DBF
        PROTECT _isFlex     AS LOGIC
        PROTECT _nextFree   AS LONG
        PROTECT _blockSize  AS WORD
        PROTECT _lockScheme AS DbfLocking
        PROPERTY Shared AS LOGIC GET _Shared
        private _blockData as byte[]
        private _fptHeader as FptHeader
        private _flexHeader as FlexHeader
        internal const FPT_HEADER_SIZE          := 512 AS LONG
        internal const MIN_FOXPRO_BLOCKSIZE     := 32 as LONG
        internal const FLEX_HEADER_OFFSET       := 512 as LONG
        internal const FLEX_HEADER_SIZE         := 512 as LONG
        internal const VERSION_MAJOR := 2   as LONG
        internal const VERSION_MINOR := 8   as LONG


        STATIC PROPERTY DefExt AS STRING AUTO
        PROTECTED PROPERTY IsOpen AS LOGIC GET SELF:_hFile != F_ERROR  .AND. SELF:_hFile != IntPtr.Zero      
        INTERNAL PROPERTY IsFlex as LOGIC GET _isFlex
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
            SELF:_blockData := Byte[]{8}
            SELF:_fptHeader   := FptHeader{}
            SELF:_flexHeader  := FlexHeader{}
            
            
        VIRTUAL PROTECTED METHOD _initContext() AS VOID
            SELF:_lockScheme:Initialize( DbfLockingModel.FoxPro )
            SELF:ReadHeader()
            IF ( SELF:BlockSize == 0 )
                //SELF:BlockSize := FPT_DEFBLOCKSIZE
                SELF:BlockSize := (WORD) XSharp.RuntimeState.MemoBlockSize
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
            LOCAL blockLen := 0 AS LONG
            local block := NULL as byte[]
            blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
            IF ( blockNbr > 0 )
                // Get the raw Length of the Memo, this included the token
                blockLen := SELF:_getValueLength( nFldPos )
                IF SELF:_setBlockPos(blockNbr)
                    block := byte[]{blockLen}
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
        METHOD GetValueLength(nFldPos AS INT) AS INT
            VAR blockLen := SELF:_getValueLength( nFldPos )
            // Don't forget to remove the 8 Bytes
            IF blockLen >= 8
                blockLen := blockLen - 8
            ENDIF
            RETURN blockLen

        INTERNAL METHOD _setBlockPos(blockNbr as LONG) AS LOGIC
            // Get the raw data length
            IF blockNbr > 0
                LOCAL iOffset := blockNbr * SELF:_blockSize AS LONG
                // Go to the blockNbr position
                return FSeek3( SELF:_hFile, iOffset, FS_SET ) == iOffSet
            ENDIF
            RETURN FALSE

        VIRTUAL PROTECTED METHOD _getValueLength(nFldPos AS INT) AS INT
            // In FPT :
            // The first 4 Bytes contains the type of Memo Data
            // The next 4 Bytes contains the length of Memo data, including the first 8 bytes
            LOCAL blockNbr AS LONG
            LOCAL blockLen := 0 AS LONG
            // File Open ?
            IF SELF:IsOpen
                // Where does the block start ?
                blockNbr := SELF:_oRDD:_getMemoBlockNumber( nFldPos )
                IF SELF:_setBlockPos(blockNbr)
                    LOCAL token as FtpMemoToken
                    token := FtpMemoToken{self:_blockData}    
                    token:Read(SELF:_hFile)
                    blockLen     := token:Length+8
                ENDIF
            ENDIF
            RETURN blockLen
            
            /// <inheritdoc />
        VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            // We receive the raw Data : We now have to put extra information on it
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
            var column := SELF:_oRdd:_GetColumn(nFldPos)
            IF ( column:FieldType == DbFieldType.Memo )
                nType := 1
            ELSEIF ( column:FieldType == DbFieldType.Picture )
                nType := 0
            ENDIF
            LongToFox(nType, memoBlock,0)
            // Ok, we now have a FPT Memoblock, save
            // TODO: write the value to the file, including:
            // - check if the current block exists
            // - check if the current block has enough space
            // - add the current block to the 'deleted blocks' list when not enough space
            // - allocate a new block from either the list of deleted blocks or at the end of the file
            // - and of course locking the file.
            RETURN SUPER:PutValue( nFldPos, memoBlock )
            
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

        METHOD RoundToBlockSize(nSize as LONG) AS LONG
            IF SELF:_blockSize > 1
                VAR nDiff := nSize % _blockSize
                IF nDiff != 0
                    nSize += _blockSize - nDiff
                ENDIF
            ENDIF
            RETURN nSize


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
                SELF:_fptHeader:NextFree :=  RoundToBlockSize(512 + 512 )
                IF ! SELF:_fptHeader:Write(SELF:_hFile)
                    SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_CREATE_MEMO, GenCode.EG_WRITE, "FPTMemo.CreateMemFile")
                ENDIF
                SELF:_flexHeader:Create()
                IF ! SELF:_flexHeader:Write(SELF:_hFile)
                    SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_CREATE_MEMO, GenCode.EG_WRITE, "FPTMemo.CreateMemFile")
                ENDIF
                SELF:_initContext()
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
                SELF:_initContext()
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
                _blockSize := value
                SELF:_fptHeader:Blocksize := value
                IF SELF:_isFlex
                    SELF:_flexHeader:AltBlockSize := value
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

        PRIVATE METHOD ReadHeader() AS LOGIC
            LOCAL savedPos := FSeek3(SELF:_hFile, 0, FS_RELATIVE ) AS LONG
            LOCAL nFileLen as LONG
            nFileLen := FSeek3(SELF:_hFile, 0, FS_END)
            IF ! SELF:_fptHeader:Read(self:_hFile)
                SELF:_oRDD:_dbfError(FException(), SubCodes.ERDD_READ, GenCode.EG_READ, "FPTMemo.ReadHeader")
            ENDIF
            _nextFree  := SELF:_fptHeader:NextFree
            _blockSize := SELF:_fptHeader:BlockSize
            // read Flex Header
            if nFileLen >= 1024
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
