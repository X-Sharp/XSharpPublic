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

BEGIN NAMESPACE XSharp.RDD.CDX
    INTERNAL ENUM ParkingLot
        MEMBER SPACES_SIZE := 1024                              // 1K of token space
        MEMBER GATE_SIZE := 1
        MEMBER LOT_SIZE := SPACES_SIZE + GATE_SIZE              // All elements, including spaces and gate
        
        MEMBER TOKEN_AREA := 1                                  // parking space tokens
        MEMBER ROOT_GATE := TOKEN_AREA + SPACES_SIZE + 1        // root parking lot gate
        MEMBER ROOT_LOT := ROOT_GATE + GATE_SIZE                // root parking lot
    END ENUM

    INTERNAL PARTIAL CLASS CdxTag
        // Methods for NTX Locking
    
        PRIVATE METHOD _genSeed() AS LONG
            LOCAL dateTime AS DateTime
            
            dateTime := DateTime{Environment.TickCount}
            RETURN ((dateTime:Hour * 60 + dateTime:Minute) * 60 + dateTime:Second) * 100 + dateTime:Millisecond
            
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
            
        PRIVATE METHOD _lockBytes( nOffset AS DWORD, nLong AS DWORD  ) AS LOGIC
            LOCAL locked AS LOGIC
//            TRY
//                locked := FFLock( SELF:_hFile, nOffset, nLong )
//            CATCH ex AS Exception
//                Trace.WriteLine("Lock Error:" + ex:Message)
//                locked := FALSE
//            END TRY
            RETURN locked
            
        PRIVATE METHOD _unlockBytes( nOffset AS DWORD, nLong AS DWORD  ) AS LOGIC
            LOCAL unlocked AS LOGIC
//            TRY
//                unlocked := FFUnLock( SELF:_hFile, nOffset, nLong )
//            CATCH ex AS Exception
//                Trace.WriteLine("UnLock Error:" + ex:Message)
//                unlocked := FALSE
//            END TRY
            RETURN unlocked
            
        PRIVATE METHOD _lockGate( tag AS INT ) AS LOGIC
            LOCAL count AS DWORD
            LOCAL isOk AS LOGIC
            LOCAL liOffSet AS LONG
            
            count := 0
            isOk := FALSE
            liOffSet := ~ SELF:_getParkLotGate( tag )
            DO WHILE count++ < SELF:_maxLockTries  .AND. !isOk
                isOk := SELF:_lockBytes((DWORD)liOffSet, 1)
            ENDDO
            RETURN isOk
            
        PRIVATE METHOD _lockInit() AS LOGIC
            LOCAL tries AS LONG
            LOCAL seed AS LONG
            tries := 0
            seed := 0
            SELF:_parkPlace := 0
            seed := SELF:_genSeed()
            DO WHILE tries++ < MAX_TRIES  .AND. SELF:_parkPlace == 0
                IF seed <= 0
                    seed := 1
                ENDIF
                seed := (seed * 1221 + 1) % ParkingLot:LOT_SIZE 
                SELF:_parkPlace := seed
                IF !SELF:_lockBytes( ~(SELF:_parkPlace + ParkingLot.TOKEN_AREA), 1)
                    RETURN FALSE
                ENDIF
            ENDDO
            
            RETURN TRUE            
            
            
        PRIVATE METHOD _getParkLotPlace( place AS LONG ) AS LONG
            RETURN ParkingLot.ROOT_LOT + ParkingLot:LOT_SIZE * place
            
        PRIVATE METHOD _getParkLotGate( tagNumber AS LONG ) AS LONG
            RETURN ParkingLot.ROOT_GATE + ParkingLot:LOT_SIZE * tagNumber
            
        PRIVATE METHOD _lockExit() AS LOGIC
            RETURN SELF:_unLockBytes( (DWORD)~(SELF:_parkPlace + 1), 1)
            
        PRIVATE METHOD _TryReadLock() AS LOGIC
            LOCAL result AS LOGIC
            LOCAL liOffSet AS LONG
            
            result := FALSE
            IF _LockGate(SELF:_tagNumber)
                liOffSet := ~(SELF:_getParkLotPlace(SELF:_tagNumber) + SELF:_parkPlace)
                IF !SELF:_lockBytes((DWORD)liOffSet, 1)
                    SELF:_unlockBytes( (DWORD)~SELF:_getParkLotGate( SELF:_tagNumber ), 1)
                    SELF:_LockExit()
                    SELF:_LockInit()
                ELSE
                    SELF:_unlockBytes( (DWORD)~SELF:_getParkLotGate( SELF:_tagNumber ), 1)
                    result := TRUE
                ENDIF
            ENDIF
            RETURN result
            
            
        PRIVATE METHOD _tryReadUnLock() AS LOGIC
            LOCAL liOffSet AS LONG
            liOffSet := ~( SELF:_getParkLotPlace(SELF:_tagNumber) + SELF:_parkPlace )
            RETURN SELF:_unlockBytes( (DWORD)liOffSet, 1)
            
            
        PRIVATE METHOD _tryWriteUnLock() AS LOGIC
            LOCAL liOffSet AS LONG
            liOffSet := ~( SELF:_getParkLotPlace(SELF:_tagNumber) + ParkingLot.LOT_SIZE )
            RETURN SELF:_unlockBytes( (DWORD)liOffSet, 1)
            
        PRIVATE METHOD _TryWriteLock() AS LOGIC
            LOCAL liOffSet AS LONG
            LOCAL maxTries AS DWORD
            LOCAL isOk AS LOGIC
            
            liOffSet := 0
            maxTries := 990
            isOk := FALSE
            IF SELF:_lockGate( SELF:_tagNumber )
                DO WHILE maxTries++ < SELF:_maxLockTries .AND. !isOk
                    liOffSet := ~( SELF:_getParkLotPlace(SELF:_tagNumber) + ParkingLot.LOT_SIZE )
                    isOk := SELF:_lockBytes((DWORD)liOffSet, ParkingLot.LOT_SIZE)
                ENDDO
                IF !isOk
                    SELF:_unlockBytes( (DWORD)~SELF:_getParkLotGate( SELF:_tagNumber ), 1)
                ENDIF
            ENDIF
            RETURN isOk
            
            
        PRIVATE METHOD _tryExclLock() AS LOGIC
            RETURN SELF:_lockBytes( (DWORD)SELF:_lockOffset, 1, (DWORD)SELF:_maxLockTries)
            
            
        PRIVATE METHOD _tryExclUnlock() AS LOGIC
            RETURN SELF:_unlockBytes( (DWORD)SELF:_lockOffset, 1)
            
        PRIVATE METHOD _lockForRead() AS LOGIC
            LOCAL locked AS LOGIC
            
            locked := TRUE
            IF SELF:_Shared
                IF !SELF:_HPLocking
                    locked := SELF:_WriteLock()
                ELSE
                    locked := SELF:_ReadLock()
                ENDIF
                IF !locked
                    SELF:_oRdd:_dbfError(SubCodes.ERDD_READ_LOCK, GenCode.EG_LOCK, SELF:fileName)
                    RETURN FALSE
                ENDIF
            ENDIF
            RETURN locked
            
            
        PRIVATE METHOD _unLockForRead() AS LOGIC
            LOCAL result AS LOGIC
            
            result := TRUE
            IF SELF:_Shared
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
                        IF !SELF:_tryReadUNLOCK()
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
                        IF !SELF:_TryExclLock()
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
                    IF SELF:_HPLocking
                        IF !SELF:_TryWriteUNLOCK()
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
//            IF SELF:_getHeader()
//                SELF:_PageList:Flush(FALSE)
//                
//                FSeek3( SELF:_hFile, 0, FS_END )
//                SELF:_fileSize  := (LONG) FTell( SELF:_hFile ) 
//                SELF:ClearStack()
//            ENDIF
            
        PRIVATE METHOD _getHeader() AS LOGIC
            LOCAL changed AS LOGIC
            
            changed := TRUE
//            IF SELF:_Header:Read()
//                changed := (SELF:_indexVersion != SELF:_Header:IndexingVersion)
//                SELF:_indexVersion := SELF:_Header:IndexingVersion
//                SELF:_firstPageOffset := SELF:_Header:FirstPageOffset
//                SELF:_nextUnusedPageOffset := SELF:_Header:NextUnusedPageOffset
//            ENDIF
            RETURN changed            
    END CLASS
END NAMESPACE
