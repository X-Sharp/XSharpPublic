//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections
USING System.Collections.Generic
USING System.Diagnostics
USING System.Globalization
USING System.IO
USING System.Reflection
USING System.Text
USING System.Threading
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD.NTX


    INTERNAL PARTIAL SEALED CLASS NtxOrder
        // Methods for NTX Locking

        #region Constants
        INTERNAL CONST SPACES_SIZE  := 1024 AS INT                             // 1K of token space
        INTERNAL CONST GATE_SIZE    := 1 AS INT
        INTERNAL CONST LOT_SIZE     := SPACES_SIZE + GATE_SIZE AS DWORD        // All elements, including spaces and gate
        INTERNAL CONST TOKEN_AREA   := 1   AS DWORD                            // parking space tokens
        INTERNAL CONST ROOT_GATE    := TOKEN_AREA + SPACES_SIZE + 1  AS DWORD  // root parking lot gate
        INTERNAL CONST ROOT_LOT     := ROOT_GATE + GATE_SIZE AS DWORD          // root parking lot
        #endregion

        #region Locking Fields
        PRIVATE _HPLocking AS LOGIC
        PRIVATE _readLocks AS LONG
        PRIVATE _writeLocks AS LONG
        PRIVATE _tagNumber AS DWORD
        PRIVATE _maxLockTries AS INT
        PRIVATE _parkPlace AS DWORD
        INTERNAL _LockOffset AS INT64
        #endregion

        PRIVATE METHOD _initLockValues() as VOID
            // NTX has only one Tag index
            SELF:_tagNumber := 1
            SELF:_maxLockTries := 99 //(LONG)XSharp.RuntimeState.LockTries
            SELF:_readLocks := 0
            SELF:_writeLocks := 0
            IF  XSharp.RuntimeState.NewIndexLock
                SELF:_LockOffset := LOCKOFFSET_NEW
            ELSE
                SELF:_LockOffset := LOCKOFFSET_OLD
            ENDIF
            SELF:_HPLocking := XSharp.RuntimeState.HPLocking


        PRIVATE METHOD _genSeed() AS DWORD
            LOCAL dateTime AS DateTime

            dateTime := DateTime{Environment.TickCount}
            RETURN (DWORD)  (dateTime:Hour * 360000 + dateTime:Minute * 6000 + dateTime:Second * 100 + dateTime:Millisecond)

        PRIVATE METHOD _lockBytes( nOffset AS DWORD, nLong AS DWORD , retries AS DWORD ) AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL counter AS DWORD
            isOk := FALSE
            counter := 0
            isOk := SELF:_lockBytes( nOffset, nLong )
            DO WHILE !isOk .AND. counter++ < retries
                isOk := SELF:_lockBytes( nOffset, nLong )
                Thread.Sleep(1)
            ENDDO
            RETURN isOk

        PRIVATE METHOD _lockBytes( nOffset AS INT64, nLong AS INT64  ) AS LOGIC
            LOCAL locked AS LOGIC
            locked := _oStream:SafeLock(nOffset, nLong )
            IF ! locked
                var oEx := FException()
                var msg := iif(oEx != NULL, oEx:Message, "Unknown")
                Trace.WriteLine(i"Lock Error, position: {nOffset} length: {nLong} Message: {msg}")
            ENDIF
            RETURN locked

        PRIVATE METHOD _unlockBytes( nOffset AS INT64, nLong AS INT64  ) AS LOGIC
            LOCAL unlocked AS LOGIC
            unlocked := _oStream:SafeUnlock( nOffset, nLong )
            IF ! unlocked
                var oEx := FException()
                var msg := iif(oEx != NULL, oEx:Message, "Unknown")
                Trace.WriteLine(i"UnLock Error, position: {nOffset} length: {nLong} Message: {msg}")
            ENDIF
            RETURN unlocked

        PRIVATE METHOD _lockGate( tag AS DWORD ) AS LOGIC
            LOCAL count AS DWORD
            LOCAL isOk AS LOGIC
            LOCAL dwOffSet AS DWORD

            count := 0
            isOk := FALSE
            dwOffSet := SELF:_getParkLotGate( tag )
            DO WHILE count++ < SELF:_maxLockTries  .AND. !isOk
                isOk := SELF:_lockBytes(dwOffSet, 1)
            ENDDO
            RETURN isOk

        PRIVATE METHOD _lockInit() AS LOGIC
            LOCAL tries AS LONG
            LOCAL seed AS DWORD
            tries := 0
            seed := 0
            SELF:_parkPlace := 0
            seed := SELF:_genSeed()
            DO WHILE tries++ < MAX_TRIES  .AND. SELF:_parkPlace == 0
                IF seed <= 0
                    seed := 1
                ENDIF
                seed := (seed * 1221 + 1) % LOT_SIZE
                SELF:_parkPlace := seed
                IF !SELF:_lockBytes(  ~(SELF:_parkPlace + TOKEN_AREA), 1)
                    RETURN FALSE
                ENDIF
            ENDDO

            RETURN TRUE


        PRIVATE METHOD _getParkLotPlace( place AS DWORD ) AS DWORD
            RETURN (DWORD) (ROOT_LOT + LOT_SIZE * place)

        PRIVATE METHOD _getParkLotGate( tagNumber AS DWORD ) AS DWORD
            LOCAL dwResult as DWORD
            dwResult := ROOT_GATE + LOT_SIZE * tagNumber
            RETURN ~dwResult

        PRIVATE METHOD _lockExit() AS LOGIC
            RETURN SELF:_unlockBytes( (~(SELF:_parkPlace + 1)), 1)

        PRIVATE METHOD _TryReadLock() AS LOGIC
            LOCAL result AS LOGIC
            LOCAL dwOffSet AS DWORD

            result := FALSE
            IF SELF:_lockGate(SELF:_tagNumber)
                dwOffSet := ~(SELF:_getParkLotPlace(SELF:_tagNumber) + SELF:_parkPlace)
                IF !SELF:_lockBytes(dwOffSet, 1)
                    SELF:_unlockBytes( SELF:_getParkLotGate( SELF:_tagNumber ), 1)
                    SELF:_lockExit()
                    SELF:_lockInit()
                ELSE
                    SELF:_unlockBytes( SELF:_getParkLotGate( SELF:_tagNumber ), 1)
                    result := TRUE
                ENDIF
            ENDIF
            RETURN result


        PRIVATE METHOD _tryReadUnLock() AS LOGIC
            LOCAL dwOffSet AS DWORD
            dwOffSet := ~( SELF:_getParkLotPlace(SELF:_tagNumber) + SELF:_parkPlace )
            RETURN SELF:_unlockBytes( dwOffSet, 1)


        PRIVATE METHOD _tryWriteUnLock() AS LOGIC
            LOCAL dwOffSet AS DWORD
            dwOffSet := ~( SELF:_getParkLotPlace(SELF:_tagNumber) + LOT_SIZE )
            RETURN SELF:_unlockBytes( dwOffSet, 1)

        PRIVATE METHOD _TryWriteLock() AS LOGIC
            LOCAL dwOffSet AS DWORD
            LOCAL maxTries AS DWORD
            LOCAL isOk AS LOGIC

            dwOffSet := 0
            maxTries := 990
            isOk := FALSE
            IF SELF:_lockGate( SELF:_tagNumber )
                DO WHILE maxTries++ < SELF:_maxLockTries .AND. !isOk
                    dwOffSet := ~( SELF:_getParkLotPlace(SELF:_tagNumber) + LOT_SIZE )
                    isOk := SELF:_lockBytes(dwOffSet, LOT_SIZE)
                ENDDO
                IF !isOk
                    SELF:_unlockBytes( SELF:_getParkLotGate( SELF:_tagNumber ), 1)
                ENDIF
            ENDIF
            RETURN isOk


        PRIVATE METHOD _tryExclLock() AS LOGIC
            RETURN SELF:_lockBytes( (DWORD)SELF:_LockOffset, 1, (DWORD)SELF:_maxLockTries)


        PRIVATE METHOD _tryExclUnlock() AS LOGIC
            RETURN SELF:_unlockBytes( (DWORD)SELF:_LockOffset, 1)

        PRIVATE METHOD _lockForRead() AS LOGIC
            LOCAL locked AS LOGIC

            locked := TRUE
            IF SELF:Shared
                IF !SELF:_HPLocking
                    locked := SELF:_WriteLock()
                ELSE
                    locked := SELF:_ReadLock()
                ENDIF
                IF !locked
                    SELF:_oRdd:_dbfError(Subcodes.ERDD_READ_LOCK, Gencode.EG_LOCK, SELF:FileName)
                    RETURN FALSE
                ENDIF
            ENDIF
            RETURN locked


        PRIVATE METHOD _unLockForRead() AS LOGIC
            LOCAL result AS LOGIC

            result := TRUE
            IF SELF:Shared
                IF !SELF:_HPLocking
                    result := SELF:_WriteUnLock()
                ELSE
                    result := SELF:_ReadUnLock()
                ENDIF
            ENDIF
            RETURN result


        PRIVATE METHOD _ReadLock() AS LOGIC
            LOCAL isOk AS LOGIC
            isOk := TRUE
            Trace.Assert(SELF:_writeLocks == 0, "Attempting read lock while holding write lock")
            IF SELF:_readLocks != 0
                SELF:_readLocks++
            ELSE
                REPEAT
                    isOk := TRUE
                    IF SELF:_HPLocking
                        IF !SELF:_TryReadLock()
                            isOk := FALSE
                        ELSE
                            SELF:_readLocks++
                        ENDIF
                    ELSE
                        IF !SELF:_tryExclLock()
                            isOk := FALSE
                        ELSE
                            SELF:_readLocks++
                        ENDIF
                    ENDIF
                UNTIL isOk
                SELF:_LockStuff()
            ENDIF
            RETURN isOk


        PRIVATE METHOD _ReadUnLock() AS LOGIC
            LOCAL result AS LOGIC

            result := TRUE
            Trace.Assert(SELF:_readLocks != 0, "Attempting read unlock with no pending locks")
            IF SELF:_readLocks != 0
                SELF:_readLocks--
                IF SELF:_readLocks == 0
                    Trace.Assert(!SELF:_Hot, "ntx unlock hot")
                    IF SELF:_HPLocking
                        IF !SELF:_tryReadUnLock()
                            result := FALSE
                        ENDIF
                    ELSE
                        IF !SELF:_tryExclUnlock()
                            result := FALSE
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            RETURN result


        PRIVATE METHOD _WriteLock() AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := TRUE
            IF SELF:_writeLocks != 0
                SELF:_writeLocks++
            ELSE
                REPEAT
                    isOk := TRUE
                    IF SELF:_HPLocking
                        IF !SELF:_TryWriteLock()
                            isOk := FALSE
                        ELSE
                            SELF:_writeLocks++
                        ENDIF
                    ELSE
                        IF !SELF:_tryExclLock()
                            isOk := FALSE
                        ELSE
                            SELF:_writeLocks++
                        ENDIF
                    ENDIF
                UNTIL isOk
                SELF:_LockStuff()
            ENDIF
            RETURN isOk


        PRIVATE METHOD _WriteUnLock() AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := TRUE
            IF SELF:_writeLocks != 0
                SELF:_writeLocks--
                IF SELF:_writeLocks == 0
                    SELF:_PageList:Flush(FALSE) // just in case - this should not be required
                    IF SELF:_HPLocking
                        IF !SELF:_tryWriteUnLock()
                            isOk := FALSE
                        ENDIF
                    ELSE
                        IF !SELF:_tryExclUnlock()
                            isOk := FALSE
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            RETURN isOk


        PRIVATE METHOD _LockStuff() AS VOID
            IF SELF:_getHeader()
                SELF:_PageList:Clear()
                SELF:_fileSize  := (LONG) _oStream:Length
                SELF:ClearStack()
            ENDIF

        PRIVATE METHOD _getHeader() AS LOGIC
            LOCAL changed AS LOGIC

            changed := TRUE
            IF SELF:_Header:Read()
                changed := (SELF:_indexVersion != SELF:_Header:IndexingVersion)
                SELF:_indexVersion := SELF:_Header:IndexingVersion
                SELF:_firstPageOffset := SELF:_Header:FirstPageOffset
                SELF:_nextUnusedPageOffset := SELF:_Header:NextUnusedPageOffset
            ENDIF
            RETURN changed
    END CLASS


END NAMESPACE



