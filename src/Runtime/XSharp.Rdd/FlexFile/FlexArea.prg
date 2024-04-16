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
USING STATIC XSharp.Conversions

#undef DELETEDBLOCKS
#ifdef DEBUG
#define BLANK_DELETED
#else
#endif

BEGIN NAMESPACE XSharp.RDD.FlexFile
/// <summary>FlexArea class. Implements the FTP support.</summary>

INTERNAL CLASS FlexArea

    PROTECT _oRdd       AS Workarea
    PROTECT _isFlex     AS LOGIC
    PRIVATE _foxHeader  AS FoxHeader
    PRIVATE _flexHeader AS FlexHeader
    PRIVATE _lockCount  AS LONG
    PRIVATE _nextFree   AS DWORD
    PRIVATE _hotHeader AS LOGIC
    PRIVATE _hFile	    AS IntPtr
    PRIVATE _oStream   AS FileStream
#ifdef DELETEDBLOCKS
    PRIVATE _DeadIndexBlocks AS ULStack
    internal LocIndex as LocationIndex
    internal LenIndex as LengthIndex
#endif
    PROTECT _lockScheme AS DbfLocking
    PROTECT _blockSize  AS WORD
    internal property LenIndexRoot AS DWORD
        GET
            return (DWORD) _flexHeader:IndexLength
        END GET
        SET
            _flexHeader:IndexLength := (INT) value
            _hotHeader := TRUE
        END SET
    END PROPERTY
    internal property LocIndexRoot AS DWORD
        GET
            return (DWORD) _flexHeader:IndexLocation
        END GET
        SET
            _flexHeader:IndexLocation := (INT) value
            _hotHeader := TRUE
        END SET
    END PROPERTY



    internal property FileHandle as IntPtr get _hFile
    internal property Stream as FileStream get _oStream
#ifdef DELETEDBLOCKS
    internal property DeadIndexBlocks as ULStack get _DeadIndexBlocks
#endif
    INTERNAL PROPERTY IsOpen     AS LOGIC GET SELF:_hFile != F_ERROR  .AND. SELF:_hFile != IntPtr.Zero
    INTERNAL PROPERTY ReadOnly   AS LOGIC GET _oRdd:ReadOnly
    INTERNAL PROPERTY Shared     AS LOGIC GET _oRdd:Shared
    INTERNAL PROPERTY NextFree   AS DWORD GET _nextFree
    INTERNAL PROPERTY Encoding   AS Encoding
        GET
            IF _oRdd is DBF var oDbf
                RETURN oDbf:_Encoding
            ENDIF
            RETURN Encoding.Default
        END GET
    END PROPERTY

    INTERNAL PROPERTY IsFlex AS LOGIC GET _isFlex
    INTERNAL PROPERTY ExportMode as INT AUTO

    INTERNAL CONSTRUCTOR(oRdd as Workarea)
        _oRdd  := oRdd
        _hFile := F_ERROR
        SELF:_foxHeader   := FoxHeader{oRdd}
        SELF:_flexHeader  := FlexHeader{oRdd}
        SELF:_lockCount   := 0
        SELF:ExportMode   := BLOB_EXPORT_APPEND
        #ifdef DELETEDBLOCKS
        SELF:_DeadIndexBlocks := ULStack{}
        SELF:LenIndex     := LengthIndex{}
        SELF:LocIndex     := LocationIndex{}
        #endif

    INTERNAL METHOD Error(ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING) AS VOID
        SELF:_oRdd:_dbfError(ex, iSubCode, iGenCode,strFunction)

    INTERNAL METHOD Create(oStream as FileStream, hFile as IntPtr, blocksize as word) AS LOGIC
        _oStream := oStream
        _hFile   := hFile
        SELF:_flexHeader:Stream := oStream
        SELF:_foxHeader:Stream  := oStream
        IF ! SELF:_foxHeader:Write()
            SELF:Error(FException(), Subcodes.ERDD_CREATE_MEMO, Gencode.EG_WRITE, "FlexArea.CreateMemFile")
        ENDIF
        SELF:_flexHeader:Create()
        IF ! SELF:_flexHeader:Write()
            SELF:Error(FException(), Subcodes.ERDD_CREATE_MEMO, Gencode.EG_WRITE, "FlexArea.CreateMemFile")
        ENDIF
        SELF:_initContext()
        SELF:BlockSize := blocksize
        _nextFree :=  (DWORD) SELF:RoundToBlockSize(FoxHeader.FOXHEADER_LENGTH + FlexHeader.FLEXHEADER_LENGTH) / _blockSize
        SELF:WriteHeader()
        RETURN TRUE

    INTERNAL METHOD Open(oStream as FileStream, hFile as IntPtr) AS LOGIC
        _oStream := oStream
        _hFile   := hFile
        SELF:_flexHeader:Stream := oStream
        SELF:_foxHeader:Stream  := oStream
        // Per default, Block Size if 512
        IF SELF:LockHeader(FALSE)
            SELF:_initContext()
#ifdef DELETEDBLOCKS
            IF SELF:IsFlex
                 SELF:LenIndex:Init(SELF)
                 SELF:LocIndex:Init(SELF)
            ENDIF
#endif

            SELF:UnLockHeader(FALSE)
        ENDIF
        RETURN TRUE

    INTERNAL METHOD Close() AS LOGIC
        IF SELF:IsOpen
#ifdef DELETEDBLOCKS
            IF SELF:IsFlex
                 SELF:LocIndex:Close()
                 SELF:LenIndex:Close()
             ENDIF
#endif
IF SELF:_lockCount > 0
                SELF:UnLockHeader(FALSE)
            ENDIF
            _oStream:Close()
            _oStream := NULL
            _hFile   := F_ERROR
            RETURN TRUE
        ENDIF
        RETURN FALSE

    PRIVATE METHOD KillIndexes() AS VOID
        // This is called when an operation on indexes detects a problem.
        IF SELF:LockHeader(TRUE)
#ifdef DELETEDBLOCKS
             SELF:LocIndex:KillIndex()
             SELF:LenIndex:KillIndex()
             SELF:DeadIndexBlocks:Clear()
#endif
             SELF:_flexHeader:IndexDefect := TRUE
            SELF:_hotHeader := TRUE
            SELF:UnLockHeader(TRUE)
        ENDIF

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
#ifdef DELETEDBLOCKS
                        LocIndex:DiskCache:Clear()
                        LenIndex:DiskCache:Clear()
#else
			NOP
#endif
                    ENDIF
                ELSE
                    SELF:Error(FException(), Subcodes.ERDD_READ, Gencode.EG_READ, "FlexArea.LockHeader")
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

    PRIVATE METHOD _initContext() AS VOID
        SELF:_lockScheme:Initialize( DbfLockingModel.FoxPro )
        SELF:ReadHeader()
        IF ( SELF:BlockSize == 0 )
            SELF:BlockSize := Convert.ToUInt16(XSharp.RuntimeState.MemoBlockSize)
            SELF:WriteHeader()
        ENDIF


    ///<summary>Return the raw data of a block including the 8 byte token </summary>
    INTERNAL METHOD GetBlock(blockNbr AS INT) AS BYTE[]
        LOCAL block := NULL AS BYTE[]
        LOCAL blockLen := 0 AS LONG
        IF ( blockNbr > 0 )
            // Get the raw Length of the Memo, this included the token
            blockLen := SELF:GetBlockLen(blockNbr)
            IF blockLen != -1
                IF SELF:SetBlockPos(blockNbr)
                    block := BYTE[]{blockLen}
                    IF !_oStream:SafeRead( block, blockLen )
                        block := NULL
                    ENDIF
                ENDIF
            ELSE
                SELF:Error(FException(),Subcodes.ERDD_READ, Gencode.EG_CORRUPTION, "FlexArea.GetBlock")
            ENDIF
        ENDIF
        RETURN block
    ///<summary>Export a block to an external file</summary>
    INTERNAL METHOD WriteBlockToFile(nBlockNr as INT, FileName as STRING) AS LOGIC
        local block as byte[]
        block := SELF:GetBlock(nBlockNr)
        IF block != NULL
            TRY
                // So, extract the "real" Data
                IF SELF:ExportMode == BLOB_EXPORT_APPEND
                    LOCAL file := System.IO.File.OpenWrite(FileName) AS FileStream
                    file:Seek(0, SeekOrigin.End)
                    file:Write(block, 8, block:Length-8)
                    file:Close()
                ELSE
                    LOCAL file := System.IO.File.Create(FileName) AS FileStream
                    file:Write(block, 8, block:Length-8)
                    file:Close()
                ENDIF
                RETURN TRUE
            CATCH e as Exception
                SELF:Error(e, Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FlexArea.WriteBlockToFile")
            END TRY
        ENDIF
        RETURN FALSE

    INTERNAL METHOD SetBlockPos(blockNbr AS LONG) AS LOGIC
        // Get the raw data length
        IF blockNbr > 0
            LOCAL iOffset := SELF:GetBlockPos(blockNbr) AS LONG
            // Go to the blockNbr position
            RETURN _oStream:SafeSetPos(iOffset)
        ENDIF
        RETURN FALSE

    INTERNAL METHOD GetDataLen(blockNbr AS LONG) AS LONG
        LOCAL blockLen := 0 AS INT
        IF SELF:SetBlockPos(blockNbr)
            VAR token := FlexMemoToken{NULL, _oStream}
            IF token:Read()
                blockLen     := token:Length
            ELSE
                blockLen     := -1
            ENDIF
        ENDIF
        RETURN blockLen

    INTERNAL METHOD GetBlockLen(blockNbr AS LONG) AS LONG
        var nLen := GetDataLen(blockNbr)
        if nLen >= 0
            nLen += FlexMemoToken.TokenLength
        endif
        return nLen


    INTERNAL METHOD GetBlockPos(blockNbr AS LONG) AS LONG
        RETURN blockNbr * SELF:_blockSize


    INTERNAL METHOD WriteFiller(nToWrite AS LONG, fillByte as Byte) AS LOGIC
        local lOk := TRUE as LOGIC
        nToWrite -= 1
        if nToWrite > 0
            var bytes := BYTE[]{nToWrite}
            lOk := _oStream:SafeWrite(bytes, bytes:Length)
        endif
        if lOk
            lOk := _oStream:SafeWriteByte(fillByte)
        endif
        IF ! lOk
            SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FlexArea.WriteFiller")
        ENDIF
        RETURN lOk


    INTERNAL METHOD DeleteBlock(blockNbr AS LONG) AS LOGIC
        // Todo: add deleted block to FlexFile deleted blocks list
        LOCAL lOk := FALSE as LOGIC
        IF SELF:SetBlockPos(blockNbr)
            VAR token := FlexMemoToken{NULL, _oStream}
            token:Read()
            token:DataType := FlexFieldType.Delete
            // Adjust the length to the whole of the block. Flexfile also does that
            token:Length   := SELF:RoundToBlockSize(token:Length +8)  - 8
            IF SELF:LockHeader(TRUE)
                SELF:SetBlockPos(blockNbr)
                lOk := token:Write()
                if lOk
                    SELF:WriteFiller(token:Length, DELETED_DATA)
                endif
                IF ! lOk
                    SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FlexArea.DeleteBlock")
                ENDIF
            ENDIF
            IF !SELF:IsFlex
                SELF:UnLockHeader(TRUE)
                RETURN lOk
            ENDIF
#ifdef DELETEDBLOCKS


            LOCAL bDone := FALSE as LOGIC
            LOCAL uReleasePos := 0 as DWORD
            LOCAL uReleaseLen := 0 AS DWORD
            LOCAL bDeletingDeadBlocks := FALSE as LOGIC
            DO WHILE ! bDone
                if bDeletingDeadBlocks
                    uReleasePos := SELF:DeadIndexBlocks:Pop()
                    uReleaseLen := IndexNode.INDEXNODE_SIZE
                else
                    uReleasePos := (DWORD) blockNbr * SELF:BlockSize
                    if uReleasePos > (DWORD) SELF:Stream:Length
                        // No error, just exit
                        bDone := TRUE
                        LOOP
                    endif
                    if token:Length < 0
                        bDone := TRUE
                        loop
                    endif
                    uReleaseLen := (DWORD) (token:Length + FlexMemoToken.TokenLength)
                endif
                // Find Next block
                if SELF:LocIndex:SeekSoft(uReleasePos)
                    // when the block after the one deleted is found
                    if SELF:LocIndex:CurrentPos == uReleasePos + uReleaseLen
                        if SELF:LenIndex:Seek(SELF:LocIndex:CurrentLen, SELF:LocIndex:CurrentPos)
                            uReleaseLen += SELF:LocIndex:CurrentLen
                            IF ! (Self:LenIndex:Delete() .and. SELF:LocIndex:Delete())
                                SELF:KillIndexes()
                                bDone := TRUE
                            ENDIF
                        else
                            // the entry should be in both indices
                            bDone := TRUE
                            SELF:KillIndexes()
                        endif
                    endif
                endif
                // seek for deleted space to left
                IF LocIndex:SeekPrevious(uReleasePos)
                    IF LocIndex:CurrentPos + LocIndex:CurrentLen == uReleasePos
                        uReleasePos := LocIndex:CurrentPos
                        uReleaseLen += LocIndex:CurrentLen
                        IF LenIndex:Seek(LocIndex:CurrentLen, LocIndex:CurrentPos)
                            IF ! (LocIndex:Delete() .and. LenIndex:Delete())
                                SELF:KillIndexes()
                            ENDIF
                        ELSE
                            SELF:KillIndexes()
                        ENDIF
                    ENDIF
                ENDIF
                IF uReleasePos + uReleaseLen == SELF:NextFree * SELF:BlockSize
                    // The block is at the end of the file, so shrink the file
                    SELF:Stream:SetLength(uReleasePos)
                    SELF:SetNewFileLength(uReleasePos)
                ELSE
                    // write the data for the combined deleted block
                    var bytes := BYTE[]{uReleaseLen}
                    token := FlexMemoToken{bytes, SELF:Stream}
                    _oStream:SafeSetPos(uReleasePos)
                    token:DataType := FlexFieldType.Delete
                    token:Length   := (LONG) (uReleaseLen - FlexMemoToken.TokenLength)
                    lOk := token:Write(FlexMemoToken.TokenLength)
                    if lOk
                        lOk := SELF:WriteFiller(token:Length, DELETED_DATA)
                    ENDIF
                    if ! lOk
                        SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FlexArea.DeleteBlock")
                    endif
                    var ok1 := LocIndex:Insert(uReleasePos, uReleaseLen)
                    var ok2 := LenIndex:Insert(uReleaseLen, uReleasePos)
                    if (! ok1 .and. ok2)
                        SELF:KillIndexes()
                    ENDIF
                ENDIF
                if SELF:DeadIndexBlocks:Count > 0
                    bDeletingDeadBlocks := TRUE
                else
                    bDone := TRUE
                endif
            ENDDO
        ELSE
            lOk := FALSE
#endif
            SELF:UnLockHeader(TRUE)
        ENDIF

        RETURN lOk


    METHOD WriteBlock (bytes AS BYTE[]) AS LOGIC
        local lOk as LOGIC
        lOk := _oStream:SafeWrite(bytes)
        IF ! lOk
            SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FlexArea.WriteBlock")
        ENDIF
        return lOk
    END METHOD

    METHOD WriteBlockFiller(nBlockLength AS LONG, fillByte as Byte) AS LOGIC
        // write remainder of block
        LOCAL nToWrite AS LONG
        LOCAL lOk as LOGIC
        nToWrite := SELF:CalculateFillerSpace( nBlockLength)
        IF nToWrite != 0
            lOk := SELF:WriteFiller(nToWrite, fillByte)
        ELSE
            lOk := TRUE
        ENDIF
        RETURN lOk
    end method

    INTERNAL METHOD SetNewFileLength(nNewLength AS INT64) AS VOID
        IF SELF:IsOpen
            _oStream:SafeSetLength(nNewLength)
            SELF:_nextFree  := (DWORD) nNewLength / _blockSize
            SELF:_hotHeader := TRUE
        ENDIF
        RETURN


    /// <summary>Write a block. When the existing block is 0 or the size is insufficient then a new block is allocated.</summary>
    /// <param name="nOldPtr">Pointer to existing block, or 0 when a new block must be allocated</param>
    /// <param name="bytes">The data to write, including the 8 byte header</param>
    /// <returns>The address of the original block (when it fits) or the address of the new block </returns>
    INTERNAL METHOD PutBlock(nOldPtr as INT, bytes as BYTE[]) AS INT
        LOCAL blockNr := nOldPtr as INT
        LOCAL nCurrentLen AS LONG
        LOCAL lNewBlock := FALSE AS LOGIC
#ifdef DELETEDBLOCKS
        LOCAL liExcessLen := 0 AS DWORD
#endif
        LOCAL lDelete  := FALSE as LOGIC
        VAR neededLen  := (DWORD) SELF:RoundToBlockSize(bytes:Length)
        IF blockNr != 0
            nCurrentLen := SELF:GetBlockLen(blockNr)
            nCurrentLen := SELF:RoundToBlockSize(nCurrentLen)
            IF nCurrentLen >= neededLen
                IF SELF:SetBlockPos(nOldPtr)
                    IF SELF:LockHeader(TRUE)
                        SELF:WriteBlock(bytes)
                        WriteBlockFiller(bytes:Length, LEFTOVER_DATASPACE_PAD)
                        SELF:UnLockHeader(TRUE)
                        RETURN blockNr
                    ENDIF
                ENDIF
                SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FlexArea.PutBlock")
            ELSE
                // Deallocate block and allocate new
                lNewBlock := TRUE
                lDelete := TRUE
            ENDIF
        ELSE
            // Allocate block at the end or from free blocks
            // write the block
            lNewBlock := TRUE
        ENDIF
        IF lNewBlock
            IF SELF:LockHeader(TRUE)
                LOCAL nPos AS DWORD
                LOCAL lFoundDeletedBlock as LOGIC
#ifdef DELETEDBLOCKS
                LOCAL liDataPos as DWORD
                LOCAL liFoundLen as DWORD
                // Try to find block of the right size in the free list
                IF SELF:IsFlex
                     lFoundDeletedBlock := SELF:LenIndex:SeekSoft(neededLen)
                     liDataPos := SELF:LenIndex:CurrentPos
                     liFoundLen := SELF:LenIndex:CurrentLen
                 ELSE
#endif
                    lFoundDeletedBlock := FALSE
#ifdef DELETEDBLOCKS
                    liFoundLen := 0
                    liDataPos := 0
                ENDIF
                 IF lFoundDeletedBlock
                     liExcessLen := liFoundLen - neededLen
                     if liExcessLen > 0 .and. liExcessLen < FlexMemoToken.TokenLength +1
                         // If excess space was allocated but it isn't enough for
                         // the FPT token, search for another spot that has enough
                         // excess space to contain the token.
                         lFoundDeletedBlock := LenIndex:SeekSoft(neededLen + RoundToBlockSize(FlexMemoToken.TokenLength + 1))
                         liDataPos := LenIndex:CurrentPos
                         liFoundLen := LenIndex:CurrentLen
                     endif
                 ENDIF
#endif
                IF ! lFoundDeletedBlock
                    nPos := _nextFree  * _blockSize
                    _oStream:SafeSetPos(nPos)
                    IF SELF:WriteBlock(bytes)
                        SELF:WriteBlockFiller(bytes:Length, LEFTOVER_DATASPACE_PAD)
                    ENDIF
                    VAR nFileSize := _oStream:Length
                    SELF:SetNewFileLength(nFileSize)
                    blockNr := (LONG) (nPos / _blockSize )
#ifdef DELETEDBLOCKS
                ELSE
                     blockNr := (LONG) (liDataPos / _blockSize )
                     liExcessLen := liFoundLen - neededLen
                     _oStream:SafeSetPos(liDataPos)
                     if SELF:WriteBlock(bytes)
                         SELF:WriteBlockFiller(bytes:Length, LEFTOVER_DATASPACE_PAD)
                     endif
                     liDataPos := liDataPos + neededLen
                    // Now delete the block from the index
                     LOCAL lKill := FALSE as LOGIC
                     IF LocIndex:Seek(LenIndex:CurrentPos, LenIndex:CurrentLen)
                         IF !( LenIndex:Delete() .and. LocIndex:Delete())
                             lKill := TRUE
                         ENDIF
                     ELSE
                        lKill := TRUE
                    ENDIF
                     IF lKill
                         SELF:KillIndexes()
                     ENDIF
#endif
                ENDIF
#ifdef DELETEDBLOCKS
                IF liExcessLen > 0
                    // Create a new block with the excess space
                    VAR bData := BYTE[]{liExcessLen}
                    VAR token := FlexMemoToken{bData, _oStream}
                    _oStream:SafeSetPos(liDataPos)
                    token:DataType := FlexFieldType.Delete
                    token:Length   := (LONG) (liExcessLen - FlexMemoToken.TokenLength)
                    IF token:Write(FlexMemoToken.TokenLength)
                        WriteFiller(token:Length, EXCESS_DATASPACE_FILLER)
                    ENDIF
                     IF ! (LenIndex:Insert(liExcessLen, liDataPos) .and. LocIndex:Insert(liDataPos, liExcessLen))
                         SELF:KillIndexes()
                     ENDIF
                endif
#endif
                SELF:UnLockHeader(TRUE)
            ENDIF
        ENDIF
        IF lDelete
            SELF:DeleteBlock(nOldPtr)
        ENDIF
        RETURN blockNr

    INTERNAL METHOD ReadBlockFromFile(nBlock as LONG, FileName as STRING) AS LONG
        LOCAL bFile AS BYTE[]
        IF XSharp.Core.Functions.File(FileName)
            TRY
                FileName := FPathName()
                bFile := System.IO.File.ReadAllBytes(FileName)
                VAR bData := BYTE[] { bFile:Length+8}
                VAR token := FlexMemoToken{bData, _oStream}
                IF bFile:Length > UInt16.MaxValue
                    token:DataType := FlexFieldType.StringLong
                ELSE
                    token:DataType := FlexFieldType.String
                ENDIF
                token:Length   := bFile:Length
                System.Array.Copy(bFile,0, bData,8, bFile:Length)
                nBlock     := SELF:PutBlock(nBlock, bData)
                RETURN nBlock
            CATCH e as Exception
                SELF:Error(e, Subcodes.ERDD_READ, Gencode.EG_READ, "FlexArea.ReadBlockFromFile")
            END TRY
        ENDIF
        RETURN -1

    INTERNAL METHOD RoundToBlockSize(nSize AS LONG) AS LONG
        IF SELF:_blockSize > 1
            VAR nDiff := nSize % _blockSize
            IF nDiff != 0
                nSize += _blockSize - nDiff
            ENDIF
        ENDIF
        RETURN nSize

    INTERNAL METHOD CalculateFillerSpace(nSize AS LONG) AS LONG
        IF SELF:_blockSize > 1
            LOCAL nToFill AS LONG
            nToFill := nSize %  SELF:_blockSize
            IF nToFill > 0
                nToFill := SELF:_blockSize - nToFill
                RETURN nToFill
            ENDIF
        ENDIF
        RETURN 0





    PROPERTY BlockSize 	 AS WORD
        GET
            RETURN _blockSize
        END GET
        SET
            _blockSize := value
            IF value >= MIN_FOXPRO_BLOCKSIZE
                SELF:_foxHeader:BlockSize := value
            ELSEIF SELF:_isFlex
                SELF:_foxHeader:BlockSize := 0
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
    PROTECTED METHOD _tryLock( nOffset AS INT64, nLong AS LONG, nTries AS LONG ) AS LOGIC
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
            SELF:Error(ex, Subcodes.ERDD_UNLOCKED, Gencode.EG_UNLOCKED, "FlexArea._unlock")

        END TRY
        RETURN unlocked


    METHOD Zap() AS LOGIC
        IF SELF:IsOpen
            _oStream:SafeSetLength(0)
            SELF:_foxHeader:Clear()
            IF SELF:IsFlex
                SELF:_flexHeader:Clear()
//                 SELF:LenIndex:Clear()
//                 SELF:LenIndex:Clear()
            ENDIF
            SELF:WriteHeader()
//            IF SELF:IsFlex
//                 SELF:LenIndex:Init(SELF)
//                 SELF:LocIndex:Init(SELF)
//            ENDIF
            RETURN TRUE
        ENDIF
        RETURN FALSE

    PRIVATE METHOD ReadHeader() AS LOGIC
        VAR savedPos := _oStream:Position
        VAR nFileLen := _oStream:Length

        IF ! SELF:_foxHeader:Read()
            SELF:Error(FException(), Subcodes.ERDD_READ, Gencode.EG_READ, "FlexArea.ReadHeader")
        ENDIF
        _blockSize := SELF:_foxHeader:BlockSize
        _nextFree  := (DWORD) SELF:_foxHeader:NextFree
        // read Flex Header
        IF nFileLen >= 1024
            IF ! SELF:_flexHeader:Read()
                SELF:Error(FException(), Subcodes.ERDD_READ, Gencode.EG_READ, "FlexArea.ReadHeader")
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
        IF SELF:IsOpen .AND. ! SELF:ReadOnly
            IF SELF:_blockSize >= MIN_FOXPRO_BLOCKSIZE
                SELF:_foxHeader:BlockSize := _blockSize
            ENDIF
            SELF:_foxHeader:NextFree   := (LONG) _nextFree
            IF ! SELF:_foxHeader:Write()
                SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FlexArea.WriteHeader")
            ENDIF
            // write flex header
            IF SELF:IsFlex
                IF SELF:_blockSize >= MIN_FOXPRO_BLOCKSIZE
                    SELF:_flexHeader:AltBlockSize := 0
                ELSE
                    SELF:_flexHeader:AltBlockSize := SELF:_blockSize
                ENDIF
                IF ! SELF:_flexHeader:Write()
                    SELF:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "FlexArea.WriteHeader")
                ENDIF
            ENDIF
        ENDIF
        SELF:_hotHeader := FALSE
        RETURN


    INTERNAL METHOD GetBlockType(blockNbr as LONG) AS STRING
        LOCAL cDataType := "?" as STRING
        IF SELF:SetBlockPos(blockNbr)
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

        CASE DbFieldInfo.DBS_BLOB_DIRECT_TYPE
            blockNr := oBlobInfo:Pointer
            oResult := SELF:GetBlockType(blockNr)

        CASE DbFieldInfo.DBS_BLOB_DIRECT_LEN
            blockNr := oBlobInfo:Pointer
            oResult := SELF:GetBlockLen(blockNr)

        CASE DbInfo.BLOB_DIRECT_IMPORT
            blockNr := oBlobInfo:Pointer
            IF oBlobInfo:Data is STRING VAR strValue
                oResult := SELF:ReadBlockFromFile(blockNr, strValue)
            ENDIF
        CASE DbInfo.BLOB_DIRECT_EXPORT
            blockNr := oBlobInfo:Pointer
            IF oBlobInfo:Data is STRING VAR strValue
                oResult := SELF:WriteBlockToFile(blockNr, strValue)
            ENDIF
        CASE DbInfo.BLOB_DIRECT_GET
            blockNr := oBlobInfo:Pointer
            bData   := SELF:GetBlock(blockNr)
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
            oResult := SELF:PutBlock(blockNr, bData)
        CASE DbInfo.BLOB_ROOT_GET
            oResult := NULL
            IF SELF:_isFlex
                blockNr := SELF:_flexHeader:Root
                if blockNr != 0
                    bData   := SELF:GetBlock(blockNr)
                    oResult := SELF:DecodeValue(bData)
                endif

            ENDIF
        CASE DbInfo.BLOB_ROOT_PUT
            IF SELF:_isFlex
                bData   := SELF:EncodeValue(oBlobInfo:Data)
                blockNr := SELF:_flexHeader:Root
                blockNr := SELF:PutBlock(blockNr, bData)
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
            LOCAL Length AS LONG
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
                Length := BitConverter.ToInt32(bData,nOffset)
                nOffset += 4
                element := SELF:Encoding:GetString(bData, nOffset, Length)
                nOffset += Length
            CASE FlexArrayTypes.String16
                Length := BitConverter.ToInt16(bData,nOffset)
                nOffset += 2
                element := SELF:Encoding:GetString(bData, nOffset, Length)
                nOffset += Length
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
            ELSE
                SELF:Error(NULL, ERDD.VAR_TYPE, Gencode.EG_ARG,"FlexArea.EncodeValue")
            ENDIF

        CASE TypeCode.Boolean
            VAR lValue := (LOGIC) oValue
            bData := BYTE[] {FlexMemoToken.TokenLength}
            token := FlexMemoToken{bData, _oStream}
            token:Length := 0
            token:DataType := IIF(lValue, FlexFieldType.LogicTrue, FlexFieldType.LogicFalse)
            RETURN bData
        CASE TypeCode.Int32
        CASE TypeCode.Int16
            VAR lValue := Convert.ToInt32(oValue)
            bData := BYTE[] {FlexMemoToken.TokenLength + 4}
            token := FlexMemoToken{bData, _oStream}
            token:Length := 4
            token:DataType := FlexFieldType.Long
            LongToBuffFox(lValue, bData, FlexMemoToken.TokenLength)
            RETURN bData
        OTHERWISE
            SELF:Error(NULL, ERDD.VAR_TYPE, Gencode.EG_ARG,"FlexArea.EncodeValue")
        END SWITCH
        RETURN NULL
#region constants
    INTERNAL CONST MIN_FOXPRO_BLOCKSIZE     := 32 AS LONG
    INTERNAL CONST INDEX_NODE_PAD := 0xF2 as byte           // '≥'  // index node pad to block boundary
#ifdef BLANK_DELETED
    INTERNAL CONST LEFTOVER_DATASPACE_PAD   := 0xAF as byte  // '»'  // data pad to block boundary
    INTERNAL CONST EXCESS_DATASPACE_FILLER  := 0xFE as byte  // 'þ'  // excess data space returned to index
    INTERNAL CONST DELETED_DATA             := 0xF0 as byte  // '≡'  // deleted data
#else
    INTERNAL CONST LEFTOVER_DATASPACE_PAD   := 0 as byte
    INTERNAL CONST EXCESS_DATASPACE_FILLER  := 0 as byte
    INTERNAL CONST DELETED_DATA             := 0 as byte
#endif

#endregion
END CLASS
END NAMESPACE
