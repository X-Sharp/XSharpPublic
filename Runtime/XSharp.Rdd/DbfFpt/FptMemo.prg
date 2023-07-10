// AbstractMemo.prg
// Created by    : robert
// Creation Date : 4/1/2020 10:21:25 AM
// Created for   :
// WorkStation   : ARTEMIS


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.IO

USING STATIC XSharp.Conversions

BEGIN NAMESPACE XSharp.RDD
    /// <summary>FPT Memo class. Implements the FTP support.</summary>
    // To also support FPT files created with FlexFile we also need to read the FlexFile header and decode that.

    INTERNAL CLASS FPTMemo INHERIT AbstractMemo IMPLEMENTS IMemo
        PROTECT _isFlex     AS LOGIC
        PRIVATE _foxHeader  AS FoxHeader
        PRIVATE _flexHeader AS FlexHeader
        PRIVATE _lockCount  AS LONG
        PRIVATE _nextFree   AS LONG
        INTERNAL CONST MIN_FOXPRO_BLOCKSIZE     := 32 AS LONG

        PRIVATE _hotHeader AS LOGIC


        STATIC PROPERTY DefExt AS STRING AUTO
        INTERNAL PROPERTY IsFlex AS LOGIC GET _isFlex
        INTERNAL PROPERTY ExportMode as INT AUTO
        STATIC CONSTRUCTOR
        DefExt := FPT_MEMOEXT


        PRIVATE METHOD LockHeader(refreshHeaders AS LOGIC) AS LOGIC
            LOCAL lOk := TRUE AS LOGIC
            IF SELF:Shared
                IF SELF:_lockCount == 0
                    DO WHILE ! SELF:_tryLock(0, 1, 10)
                        NOP
                    ENDDO
                    SELF:_lockCount := 1
                ELSE
                    SELF:_lockCount += 1
                ENDIF
                IF refreshHeaders
                    IF SELF:ReadHeader()
                        IF SELF:IsFlex
                            // Deal with indexes of deleted blocks
                            NOP
                        ENDIF
                    ELSE
                        SELF:Error(FException(), Subcodes.ERDD_READ, Gencode.EG_READ, "FPTMemo.LockHeader")
                    ENDIF
                ENDIF
            ELSE
                SELF:_lockCount += 1
            ENDIF
        RETURN lOk


        PRIVATE METHOD UnLockHeader(updated AS LOGIC) AS LOGIC
            IF updated
                SELF:_hotHeader := TRUE
            ENDIF
            IF SELF:_lockCount <= 1
                IF SELF:_hotHeader
                    SELF:WriteHeader()
                ENDIF
                IF SELF:Shared
                    _oStream:Flush()
                    IF ! SELF:_unlock(0,1)
                        NOP
                    ENDIF
                ENDIF
                SELF:_lockCount := 0
            ELSE
                SELF:_lockCount -= 1
            ENDIF
        RETURN TRUE

        PRIVATE METHOD _GetMemoExtFromDbfExt(cDbfName AS STRING) AS STRING
            SWITCH System.IO.Path.GetExtension(cDbfName:ToLower())
            CASE ".vcx"         // Control Library
                RETURN ".VCT"
            CASE ".scx"         // Screen
                RETURN ".SCT"
            CASE ".pjx"         // Project
                RETURN ".PJT"
            CASE ".frx"         // Report
                RETURN ".FRT"
            CASE ".lbx"         // Label
                RETURN ".LBT"
            CASE ".mnx"         // Menu
                RETURN ".MNT"
            CASE ".dbc"         // database container
                RETURN ".dct"
            END SWITCH
            VAR cExt := RuntimeState.GetValue<STRING>(Set.MemoExt)
            IF ! String.IsNullOrEmpty(cExt)
                IF ! cExt.StartsWith(".")
                    cExt := "."+cExt
                    RuntimeState.SetValue(Set.MemoExt, cExt)
                ENDIF
                RETURN cExt
            ENDIF
        RETURN DefExt


        CONSTRUCTOR (oRDD AS DBF)
            SUPER(oRDD)
            SELF:_foxHeader   := FoxHeader{oRDD}
            SELF:_flexHeader  := FlexHeader{oRDD}
            SELF:_lockCount   := 0
            SELF:ExportMode   := BLOB_EXPORT_APPEND


        VIRTUAL PROTECTED METHOD _initContext() AS VOID
            SELF:_lockScheme:Initialize( DbfLockingModel.FoxPro )
            SELF:ReadHeader()
            IF ( SELF:BlockSize == 0 )
                //SELF:BlockSize := FPT_DEFBLOCKSIZE
                SELF:BlockSize := Convert.ToUInt16(XSharp.RuntimeState.MemoBlockSize)
                SELF:WriteHeader()
            ENDIF


        ///<summary>Return the raw data of a block including the 8 byte token </summary>
        PRIVATE METHOD _getBlock(blockNbr AS INT) AS BYTE[]
            LOCAL block := NULL AS BYTE[]
            LOCAL blockLen := 0 AS LONG
            IF ( blockNbr > 0 )
                // Get the raw Length of the Memo, this included the token
                blockLen := SELF:_GetBlockLen(blockNbr)
                IF blockLen != -1
                    IF SELF:_setBlockPos(blockNbr)
                        block := BYTE[]{blockLen}
                        IF !_oStream:SafeRead( block, blockLen )
                            block := NULL
                        ENDIF
                    ENDIF
                ELSE
                    SELF:Error(FException(),Subcodes.ERDD_READ, Gencode.EG_CORRUPTION, "FPTMemo.GetValue")
                ENDIF
            ENDIF
            RETURN block

        INTERNAL METHOD GetRawValueWithHeader(nFldPos as INT) AS BYTE[]
            LOCAL blockNbr AS LONG
            LOCAL block := NULL AS BYTE[]
            IF SELF:IsOpen
                blockNbr := SELF:_oRdd:_getMemoBlockNumber( nFldPos )
                IF ( blockNbr > 0 )
                    block := SELF:_getBlock(blockNbr)
                ENDIF
            ENDIF
            RETURN block

        /// <summary></summary>
        /// <param name="nFldPos">One based field number</param>
        OVERRIDE METHOD GetValue(nFldPos AS INT) AS OBJECT
            LOCAL block := SELF:GetRawValueWithHeader(nFldPos) AS BYTE[]
            if block != NULL
                var result := BYTE[]{block:Length - 8}
                System.Array.Copy(block, 8, result,0, result:Length)
                block := result
            endif
            RETURN block
       PRIVATE METHOD _WriteBlockToFile(nBlockNr as INT, fileName as STRING) AS LOGIC
            local block as byte[]
            block := SELF:_getBlock(nBlockNr)
            IF block != NULL
                // So, extract the "real" Data
                IF SELF:ExportMode == BLOB_EXPORT_APPEND
                    LOCAL file := System.IO.File.OpenWrite(fileName) AS FileStream
                    file:Seek(0, SeekOrigin.End)
                    file:Write(block, 8, block:Length-8)
                    file:Close()
                ELSE
                    LOCAL file := System.IO.File.Create(fileName) AS FileStream
                    file:Write(block, 8, block:Length-8)
                    file:Close()
                ENDIF
                RETURN TRUE
            ENDIF
            RETURN FALSE
        /// <inheritdoc />
        /// <param name="nFldPos">One based field number</param>
        OVERRIDE METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
            IF SELF:_oRdd:_isMemoField( nFldPos )
                // At this level, the return value is the raw Data, in BYTE[]
                TRY
                    VAR blockNbr := SELF:_oRdd:_getMemoBlockNumber( nFldPos )
                    RETURN SELF:_WriteBlockToFile(blockNbr, fileName)
                CATCH ex AS Exception
                    SELF:Error(ex, Subcodes.ERDD_READ, Gencode.EG_READ, "DBFFPT.GetValueFile")
                END TRY
            ENDIF
        RETURN SUPER:GetValueFile(nFldPos, fileName)

        /// <inheritdoc />
        /// <param name="nFldPos">One based field number</param>
        OVERRIDE METHOD GetValueLength(nFldPos AS INT) AS LONG
            var blockNbr := SELF:_oRdd:_getMemoBlockNumber( nFldPos )
            var blockLen := SELF:_GetBlockLen(blockNbr)
            // Don't forget to remove the 8 Bytes
            if blockLen == -1
                blockLen := 0
            ELSEIF blockLen >= 8
                blockLen := blockLen - 8
            ENDIF
            RETURN blockLen

        INTERNAL METHOD _setBlockPos(blockNbr AS LONG) AS LOGIC
            // Get the raw data length
            IF blockNbr > 0
                LOCAL iOffset := blockNbr * SELF:_blockSize AS LONG
                // Go to the blockNbr position
                RETURN _oStream:SafeSetPos(iOffset)
            ENDIF
        RETURN FALSE

        PRIVATE METHOD _GetBlockLen(blockNbr AS LONG) AS LONG
            LOCAL blockLen := 0 AS INT
            IF SELF:_setBlockPos(blockNbr)
                VAR token := FlexMemoToken{NULL, _oStream}
                IF token:Read()
                    blockLen     := token:Length+8
                ELSE
                    blockLen     := -1
                ENDIF
            ENDIF
        RETURN blockLen


        /// <inheritdoc />

        PRIVATE METHOD WriteFiller(nToWrite AS LONG, lDeleted AS LOGIC) AS VOID
            LOCAL fillByte AS BYTE
            LOCAL lIsVfp   AS LOGIC
            lIsVfp   := SELF:_oRdd IS DBFVFP
            fillByte := (BYTE) IIF(lDeleted, 0xF0, IIF(lIsVfp, 0x00, 0xAF))
            FOR VAR i := 1 TO nToWrite
                IF ! _oStream:SafeWriteByte(fillByte)
                    SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.WriteFiller")
                ENDIF
            NEXT
        RETURN

        PROTECTED INTERNAL METHOD DeleteBlock(blockNbr AS LONG) AS VOID
            // Todo: add deleted block to FlexFile deleted blocks list
            IF SELF:_setBlockPos(blockNbr)
                VAR token := FlexMemoToken{NULL, _oStream}
                token:Read()
                token:DataType := FlexFieldType.Delete
                // Adjust the length to the whole of the block. Flexfile also does that
                token:Length   := SELF:_RoundToBlockSize(token:Length +8)  - 8
                IF SELF:LockHeader(TRUE)
                    SELF:_setBlockPos(blockNbr)
                    IF ! token:Write()
                        SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.DeleteBlock")
                    ENDIF
                    // Clear the data. FlexFiles does not do that, but I think it's better to clean up.
                    SELF:WriteFiller(token:Length, TRUE)
                    SELF:UnLockHeader(TRUE)
                ENDIF
            ENDIF
        RETURN


        METHOD _WriteBlock (bytes AS BYTE[]) AS LOGIC
            IF ! _oStream:SafeWrite(bytes)
                SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo._WriteBlock")
            ENDIF
            // write remainder of block
            LOCAL nToWrite AS LONG
            nToWrite := SELF:_CalculateFillerSpace( bytes:Length)
            IF nToWrite != 0
                SELF:WriteFiller(nToWrite, FALSE)
            ENDIF
            RETURN TRUE

        /// <summary>Write a block. When the existing block is 0 or the size is insufficient then a new block is allocated.</summary>
        /// <param name="nOldPtr">Pointer to existing block, or 0 when a new block must be allocated</param>
        /// <param name="bytes">The data to write, including the 8 byte header</param>
        /// <returns>The address of the original block (when it fits) or the address of the new block </returns>
        PRIVATE METHOD _putBlock(nOldPtr as INT, bytes as BYTE[]) AS INT
            LOCAL blockNr := nOldPtr as INT
            LOCAL nCurrentLen AS LONG
            LOCAL lNewBlock := FALSE AS LOGIC
            IF blockNr != 0
                nCurrentLen := SELF:_GetBlockLen(blockNr)
                nCurrentLen := SELF:_RoundToBlockSize(nCurrentLen)
                VAR needed  := SELF:_RoundToBlockSize(bytes:Length)
                IF nCurrentLen >= needed
                    IF SELF:_setBlockPos(nOldPtr)
                        IF SELF:LockHeader(TRUE)
                            SELF:_WriteBlock(bytes)
                            SELF:UnLockHeader(TRUE)
                            RETURN blockNr
                        ENDIF
                    ENDIF
                    SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.PutValue")
                ELSE
                    // Deallocate block and allocate new
                    SELF:DeleteBlock(blockNr)
                    lNewBlock := TRUE
                ENDIF
            ELSE
                // Allocate block at the end or from free blocks
                // write the block
                lNewBlock := TRUE
            ENDIF
            IF lNewBlock
                IF SELF:LockHeader(TRUE)
                    LOCAL nPos AS LONG
                    nPos := _nextFree  * _blockSize
                    _oStream:SafeSetPos(nPos)
                    SELF:_WriteBlock(bytes)
                    VAR nFileSize := _oStream:Length
                    SELF:_nextFree    := (LONG) nFileSize / _blockSize
                    SELF:UnLockHeader(TRUE)
                    blockNr := (LONG) (nPos / _blockSize )
                ENDIF
            ENDIF
            RETURN blockNr

        /// <summary>Write value to field.</summary>
        /// <param name="nFldPos">One based field number</param>
        /// <param name="oValue">Data to write. Should be BYTE[] and include the header with the type</param>
        OVERRIDE METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            IF SELF:IsOpen .and. oValue IS BYTE[] VAR bytes
                // AT this level the bytes[] array already contains the header with type and length
                var blockNbr := SELF:_oRdd:_getMemoBlockNumber( nFldPos )
                blockNbr     := SELF:_putBlock(blockNbr, bytes)
                SELF:LastWrittenBlockNumber := blockNbr
                RETURN TRUE
            ENDIF
            RETURN FALSE

       PRIVATE METHOD _ReadBlockFromFile(nBlock as LONG, fileName as STRING) AS LONG
            LOCAL bFile AS BYTE[]
            IF File(fileName)
                fileName := FPathName()
                bFile := System.IO.File.ReadAllBytes(fileName)
                VAR bData := BYTE[] { bFile:Length+8}
                VAR token := FlexMemoToken{bData, _oStream}
                IF bFile:Length > UInt16.MaxValue
                    token:DataType := FlexFieldType.StringLong
                ELSE
                    token:DataType := FlexFieldType.String
                ENDIF
                token:Length   := bFile:Length
                System.Array.Copy(bFile,0, bData,8, bFile:Length)
                nBlock     := SELF:_putBlock(nBlock, bData)
                RETURN nBlock
            ENDIF
            RETURN -1


        /// <inheritdoc />
        OVERRIDE METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
            TRY
                VAR oColumn := SELF:_oRdd:_GetColumn(nFldPos) ASTYPE DbfColumn
                IF oColumn != NULL .AND. oColumn:IsMemo
                    var blockNbr := SELF:_oRdd:_getMemoBlockNumber( nFldPos )
                    blockNbr := SELF:_ReadBlockFromFile(blockNbr, fileName)
                    IF blockNbr >= 0
                        RETURN oColumn:PutValue(blockNbr, SELF:_oRdd:RecordBuffer)
                    ENDIF
                ENDIF
            CATCH ex AS Exception
                SELF:Error(ex, Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.PutValueFile")
            END TRY
        RETURN FALSE

        PRIVATE METHOD _RoundToBlockSize(nSize AS LONG) AS LONG
            IF SELF:_blockSize > 1
                VAR nDiff := nSize % _blockSize
                IF nDiff != 0
                    nSize += _blockSize - nDiff
                ENDIF
            ENDIF
            RETURN nSize
        PRIVATE METHOD _CalculateFillerSpace(nSize AS LONG) AS LONG
            IF SELF:_blockSize > 1
                LOCAL nToFill AS LONG
                nToFill := nSize %  SELF:_blockSize
                IF nToFill > 0
                    nToFill := SELF:_blockSize - nToFill
                    RETURN  nToFill
                ENDIF
            ENDIF
            RETURN 0

        /// <inheritdoc />
        OVERRIDE METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
            LOCAL isOk      AS LOGIC
            SELF:Extension := SELF:_GetMemoExtFromDbfExt(info:FullName)
            isOk := SUPER:CreateMemFile(info)
            IF isOk
                SELF:_flexHeader:Stream := _oStream
                SELF:_foxHeader:Stream  := _oStream
                IF ! SELF:_foxHeader:Write()
                    SELF:Error(FException(), Subcodes.ERDD_CREATE_MEMO, Gencode.EG_WRITE, "FPTMemo.CreateMemFile")
                ENDIF
                SELF:_flexHeader:Create()
                IF ! SELF:_flexHeader:Write()
                    SELF:Error(FException(), Subcodes.ERDD_CREATE_MEMO, Gencode.EG_WRITE, "FPTMemo.CreateMemFile")
                ENDIF
                SELF:_initContext()
                _nextFree :=  SELF:_RoundToBlockSize(FoxHeader.FOXHEADER_LENGTH + FlexHeader.FLEXHEADER_LENGTH) / _blockSize
                SELF:WriteHeader()
            ELSE
                SELF:Error( FException(), ERDD.CREATE_MEMO, XSharp.Gencode.EG_CREATE, "FPTMemo.CreateMemFile")
            ENDIF

        RETURN isOk

        /// <inheritdoc />
        OVERRIDE METHOD OpenMemFile(info AS DbOpenInfo ) AS LOGIC
            LOCAL isOk AS LOGIC
            SELF:Extension := SELF:_GetMemoExtFromDbfExt(info:FullName)
            isOk := SUPER:OpenMemFile(info)
            IF isOk
                SELF:_flexHeader:Stream := _oStream
                SELF:_foxHeader:Stream  := _oStream
                // Per default, Block Size if 512
                IF SELF:LockHeader(FALSE)
                    SELF:_initContext()
                    SELF:UnLockHeader(FALSE)
                ENDIF
            ELSE
                SELF:Error( FException(),ERDD.OPEN_MEMO, XSharp.Gencode.EG_OPEN ,"FPTMemo.OpenMemFile")
                isOk := FALSE
            ENDIF
            //
        RETURN isOk

        OVERRIDE PROPERTY BlockSize 	 AS WORD
            GET
                RETURN _blockSize
            END GET
            SET
                _blockSize := value
                IF value >= MIN_FOXPRO_BLOCKSIZE
                    SELF:_foxHeader:BlockSize := value
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
        PROTECTED METHOD _tryLock( nOffset AS INT64, nLong AS LONG, nTries AS LONG  ) AS LOGIC
            LOCAL locked AS LOGIC
            IF ! SELF:IsOpen
                RETURN FALSE
            ENDIF
            REPEAT
                locked := _oStream:SafeLock(nOffset, nLong )
                //                IF ! locked
                //                    SELF:Error(FException(), Subcodes.ERDD_INIT_LOCK, Gencode.EG_LOCK_ERROR, "FPTMemo._tryLock")
                //                ENDIF
                IF ( !locked )
                    nTries --
                    IF ( nTries > 0 )
                        System.Threading.Thread.Sleep( 1 )
                    ENDIF
                ENDIF
            UNTIL ( locked .OR. (nTries==0) )
            //
        RETURN locked

        PROTECTED METHOD _unlock( nOffset AS INT64, nLong AS LONG ) AS LOGIC
            LOCAL unlocked AS LOGIC
            IF ! SELF:IsOpen
                RETURN FALSE
            ENDIF

            TRY
               _oStream:Unlock( nOffset, nLong )
                unlocked := TRUE
            CATCH ex AS Exception
                unlocked := FALSE
                SELF:Error(ex, Subcodes.ERDD_UNLOCKED, Gencode.EG_UNLOCKED, "FPTMemo._unlock")

            END TRY
        RETURN unlocked


        OVERRIDE METHOD Zap() AS LOGIC
            IF SELF:IsOpen
                IF SELF:Shared
                    SELF:Error(FException(), Subcodes.ERDD_SHARED, Gencode.EG_LOCK, "FPTMemo.Zap")
                ENDIF
                _oStream:SafeSetLength(0)
                SELF:_foxHeader:Clear()
                SELF:_flexHeader:Clear()
                SELF:WriteHeader()
                RETURN TRUE
            ELSE
                SELF:Error(FException(), Subcodes.EDB_NOTABLE, Gencode.EG_NOTABLE, "FPTMemo.Zap")
            ENDIF
        RETURN FALSE

        PRIVATE METHOD ReadHeader() AS LOGIC
            VAR savedPos := _oStream:Position
            VAR nFileLen := _oStream:Length

            IF ! SELF:_foxHeader:Read()
                SELF:Error(FException(), Subcodes.ERDD_READ, Gencode.EG_READ, "FPTMemo.ReadHeader")
            ENDIF
            _blockSize := SELF:_foxHeader:BlockSize
            _nextFree  := SELF:_foxHeader:NextFree
            // read Flex Header
            IF nFileLen >= 1024
                IF ! SELF:_flexHeader:Read()
                    SELF:Error(FException(), Subcodes.ERDD_READ, Gencode.EG_READ, "FPTMemo.ReadHeader")
                ENDIF
                _isFlex := SELF:_flexHeader:Valid
                IF _blockSize == 0 .AND. SELF:_flexHeader:AltBlockSize != 0
                    SELF:_blockSize     := SELF:_flexHeader:AltBlockSize
                ENDIF
            ELSE
                _isFlex := FALSE
            ENDIF
            _oStream:SafeSetPos(savedPos)
        RETURN TRUE

        METHOD WriteHeader() AS VOID
            IF SELF:IsOpen .AND. ! SELF:_oRdd:ReadOnly
                IF SELF:_blockSize >= MIN_FOXPRO_BLOCKSIZE
                    SELF:_foxHeader:BlockSize := _blockSize
                ENDIF
                SELF:_foxHeader:NextFree   := _nextFree
                IF ! SELF:_foxHeader:Write()
                    SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.WriteHeader")
                ENDIF
                // write flex header
                IF SELF:IsFlex
                    IF SELF:_blockSize >= MIN_FOXPRO_BLOCKSIZE
                        SELF:_flexHeader:AltBlockSize := 0
                    ELSE
                        SELF:_flexHeader:AltBlockSize := SELF:_blockSize
                    ENDIF
                    IF ! SELF:_flexHeader:Write()
                        SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FPTMemo.WriteHeader")
                    ENDIF
                ENDIF
            ENDIF
            SELF:_hotHeader := FALSE
            RETURN

        PRIVATE METHOD _GetBlockType(blockNbr as LONG) AS STRING
            LOCAL cDataType := "?" as STRING
            IF SELF:_setBlockPos(blockNbr)
                VAR token := FlexMemoToken{NULL, _oStream}
                IF token:Read()
                    cDataType := "N"
                    SWITCH token:DataType

                    case FlexFieldType.String
                    case FlexFieldType.StringEmpty
                    case FlexFieldType.Compressed

                        cDataType := "C"

                    CASE FlexFieldType.Array16
                    CASE FlexFieldType.Array32

                        cDataType := "A"

                    case FlexFieldType.Object16
                    case FlexFieldType.Object32
                    case FlexFieldType.Picture

                        cDataType := "O"

                    case FlexFieldType.Nil

                        cDataType := "U"

                    case FlexFieldType.LogicTrue
                    case FlexFieldType.LogicFalse

                        cDataType := "L"
                    case FlexFieldType.JDate

                        cDataType := "D"
                    case FlexFieldType.SByte
                    case FlexFieldType.Byte
                    case FlexFieldType.Short
                    case FlexFieldType.Word
                    case FlexFieldType.Long
                    case FlexFieldType.Dword
                    case FlexFieldType.Double
                    case FlexFieldType.Double10
                        cDataType := "N"

                    case FlexFieldType.Delete
                    case FlexFieldType.OleObject
                    case FlexFieldType.IndexBlock
                    case FlexFieldType.Illegal
                        cDataType := "?"

                    END SWITCH
                ENDIF
            ENDIF
        RETURN cDataType
        METHOD BlobInfo(nOrdinal AS INT, oBlobInfo as XSharp.RDD.IBlobData) AS OBJECT
            LOCAL blockNr as LONG
            LOCAL oResult := TRUE AS OBJECT
            local bData   as byte[]
            SWITCH nOrdinal

            CASE DbInfo.BLOB_GET
                VAR nFldPos := Convert.ToInt32(oBlobInfo:Data)
                IF SELF:_oRdd:_isMemoField( nFldPos )
                    VAR rawData := SELF:GetRawValueWithHeader(nFldPos)
                    oResult := SELF:DecodeValue(rawData)
                    IF oResult IS String VAR strResult
                        VAR nOffset := oBlobInfo:Start
                        VAR nLen    := oBlobInfo:Length
                        // Note that nOffSet is a 1 based offset !
                        nOffset -= 1
                        if nLen > strResult:Length - nOffset
                            nLen := strResult:Length - nOffset
                        endif
                       oResult := strResult:Substring(nOffset, nLen)
                    ENDIF
                ENDIF

            CASE DbFieldInfo.DBS_BLOB_DIRECT_TYPE
                blockNr := oBlobInfo:Pointer
                oResult := SELF:_GetBlockType(blockNr)

            CASE DbFieldInfo.DBS_BLOB_DIRECT_LEN
                blockNr := oBlobInfo:Pointer
                oResult := SELF:_GetBlockLen(blockNr)

            CASE DbInfo.BLOB_DIRECT_IMPORT
                blockNr := oBlobInfo:Pointer
                IF oBlobInfo:Data is STRING VAR strValue
                    oResult := SELF:_ReadBlockFromFile(blockNr, strValue)
                ENDIF
            CASE DbInfo.BLOB_DIRECT_EXPORT
                blockNr := oBlobInfo:Pointer
                IF oBlobInfo:Data is STRING VAR strValue
                    oResult := SELF:_WriteBlockToFile(blockNr, strValue)
                ENDIF
            CASE DbInfo.BLOB_DIRECT_GET
                blockNr := oBlobInfo:Pointer
                bData   := SELF:_getBlock(blockNr)
                oResult := SELF:DecodeValue(bData)
                if oBlobInfo:Start != 0 .or. oBlobInfo:Length != Int32.MaxValue
                    LOCAL nStart as LONG
                    LOCAL nLen   as LONG
                    IF oResult IS String VAR strValue
                        IF oBlobInfo:Start >= 0
                            nStart := oBlobInfo:Start-1
                        ELSE
                            nStart := strValue:Length + oBlobInfo:Start
                        ENDIF
                        nLen   := Math.Min(strValue:Length - nStart, oBlobInfo:Length)
                        oResult := strValue:Substring(nStart, nLen)
                    ELSEIF oResult IS Byte[] VAR bytes
                        IF oBlobInfo:Start >= 0
                            nStart := oBlobInfo:Start-1
                        ELSE
                            nStart := bytes:Length + oBlobInfo:Start
                        ENDIF
                        nLen := Math.Min(bytes:Length - nStart, oBlobInfo:Length)
                        var aResult := Byte[]{nLen}
                        System.Array.Copy(bytes,nStart,aResult,0, nLen)
                        oResult :=aResult
                    ENDIF
                ENDIF

            CASE DbInfo.BLOB_DIRECT_PUT
                blockNr := oBlobInfo:Pointer
                bData   := SELF:EncodeValue(oBlobInfo:Data)
                oResult := SELF:_putBlock(blockNr, bData)
            CASE DbInfo.BLOB_ROOT_GET
                oResult := NULL
                IF SELF:_isFlex
                    blockNr := SELF:_flexHeader:Root
                    if blockNr != 0
                        bData   := SELF:_getBlock(blockNr)
                        oResult := SELF:DecodeValue(bData)
                    endif

                ENDIF
            CASE DbInfo.BLOB_ROOT_PUT
                IF SELF:_isFlex
                    bData   := SELF:EncodeValue(oBlobInfo:Data)
                    blockNr := SELF:_flexHeader:Root
                    blockNr := SELF:_putBlock(blockNr, bData)
                    SELF:_flexHeader:Root := blockNr
                    SELF:WriteHeader()
                    oResult := TRUE
                ELSE
                    oResult := FALSE
                ENDIF
            CASE DbInfo.BLOB_ROOT_LOCK
                IF SELF:_isFlex
                    oResult := SELF:_flexHeader:RootLock()
                ENDIF
            CASE DbInfo.BLOB_ROOT_UNLOCK
                IF SELF:_isFlex
                    oResult := SELF:_flexHeader:RootUnLock()
                ENDIF
            OTHERWISE
                NOP
            END SWITCH
            RETURN oResult
       /// <summary>Decode the contents of a block, including the block header</summary>
       /// <param name="bData">The raw block including the 8 byte header</param>
       INTERNAL METHOD DecodeValue(bData AS BYTE[]) AS OBJECT
            // bData includes the header
            LOCAL offset AS LONG
            VAR token := FlexMemoToken{bData, _oStream}
            SWITCH token:DataType
            CASE FlexFieldType.Array16
            CASE FlexFieldType.Array32
                offset := 8
                RETURN SELF:DecodeFlexArray(token:DataType, bData, REF offset)
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
                        RETURN SELF:Encoding:GetString(bData,8, bData:Length-9)
                    ELSE
                        RETURN SELF:Encoding:GetString(bData,8, bData:Length-8)
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
                RETURN BuffToShortFox(bData, 8)
            CASE FlexFieldType.Word
                RETURN BuffToWordFox(bData, 8)
            CASE FlexFieldType.Long
                RETURN BuffToLongFox(bData, 8)
            CASE FlexFieldType.Dword
                RETURN BuffToDwordFox(bData, 8)
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
                RETURN BuffToLongFox(bData, 8) != 0
            CASE FlexFieldType.StringEmpty
                RETURN ""
            CASE FlexFieldType.Illegal
                RETURN NIL
            END SWITCH
            RETURN bData

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
                    element := SELF:Encoding:GetString(bData, nOffset, length)
                    nOffset += length
                CASE FlexArrayTypes.String16
                    length := BitConverter.ToInt16(bData,nOffset)
                    nOffset += 2
                    element := SELF:Encoding:GetString(bData, nOffset, length)
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
                    element := SELF:DecodeFlexArray(FlexFieldType.Array16, bData, REF nOffset)

                CASE FlexArrayTypes.CodeBlock
                    element := NULL
                    SELF:_oRdd:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.DateJ
                    element := BitConverter.ToInt32(bData, nOffset)
                    nOffset += 4

                CASE FlexArrayTypes.Double2
                    element := BitConverter.ToDouble(bData, nOffset)
                    nOffset += 6

                CASE FlexArrayTypes.Cyclic
                    element := NULL
                    SELF:_oRdd:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

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
                    SELF:_oRdd:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.Object
                    element := NULL
                    SELF:_oRdd:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)

                CASE FlexArrayTypes.Null
                    element := String.Empty

                CASE FlexArrayTypes.True
                    element := TRUE

                CASE FlexArrayTypes.False
                    element := FALSE

                CASE FlexArrayTypes.LDouble
                    element := NULL
                    SELF:_oRdd:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)
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
                    SELF:_oRdd:_dbfError(NULL, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, __FUNCTION__)
                END SWITCH
                aValues[i] := element
            NEXT
            RETURN aValues

        /// <summary>Encode a value into a FTP block</summary>
        /// <param name="oValue">The value to encode. Allowed are at this moment, String, Logic and byte[]</param>
        /// <returns>Byte array including the 8 byte header with the block length and type</returns>
        INTERNAL METHOD EncodeValue(oValue AS OBJECT) AS BYTE[]
            LOCAL token AS FlexMemoToken
            LOCAL otc AS TypeCode
            LOCAL oType AS System.Type
            LOCAL bData AS BYTE[]
            IF oValue == NULL
                RETURN NULL
            ENDIF
            oType := oValue:GetType()
            otc   := System.Type.GetTypeCode(oType)
            SWITCH otc
            CASE TypeCode.String
                VAR sValue := (STRING) oValue
                bData := BYTE[] { sValue:Length+FlexMemoToken.TokenLength}
                token := FlexMemoToken{bData, _oStream}
                token:DataType := FlexFieldType.String
                token:Length   := sValue:Length
                VAR bytes := SELF:Encoding:GetBytes(sValue)
                System.Array.Copy(bytes,0, bData,FlexMemoToken.TokenLength, bytes:Length)
                RETURN bData
            CASE TypeCode.Object
                IF oValue IS BYTE[] VAR bytes
                    // Store as Picture Type in the FPT
                    bData := BYTE[] { bytes:Length+FlexMemoToken.TokenLength}
                    token := FlexMemoToken{bData, _oStream}
                    token:DataType := FlexFieldType.Picture
                    token:Length   := bytes:Length
                    System.Array.Copy(bytes,0, bData,FlexMemoToken.TokenLength, bytes:Length)
                    RETURN bData
                ENDIF
            CASE TypeCode.Boolean
                VAR lValue := (LOGIC) oValue
                bData := BYTE[] {FlexMemoToken.TokenLength}
                token := FlexMemoToken{bData, _oStream}
                token:Length := 0
                token:DataType := IIF(lValue, FlexFieldType.LogicTrue, FlexFieldType.LogicFalse)
                RETURN bData
            END SWITCH
            RETURN NULL



    END CLASS

END NAMESPACE

