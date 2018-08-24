//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.IO
USING System.Text
USING System.Linq
USING XSharp.RDD.Enums
USING System.Globalization
USING System.Collections.Generic


BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBF RDD. Usually not used 'stand alone'</summary>
    CLASS DBF INHERIT Workarea  
        PROTECT _Header			AS DbfHeader    
        //PROTECT _HeaderLength	AS WORD  	// Size of header 
        PROTECT _BufferValid	AS LOGIC	// Current Record is Valid
        PROTECT _HasMemo		AS LOGIC
        PROTECT _HasTags		AS LOGIC
        PROTECT _HasAutoInc		AS LOGIC 
        PROTECT _HasTimeStamp	AS LOGIC
        //PROTECT _LastUpdate	    AS DateTime
        PROTECT _RecCount		AS LONG		
        PROTECT _RecNo			AS LONG
        //PROTECT _Temporary		AS LOGIC    
        //PROTECT _NullCount		AS LONG
        //PROTECT _NullOffSet		AS LONG
        PROTECT _RecordChanged	AS LOGIC 	// Current record has changed ?
        PROTECT _Positioned		AS LOGIC 	// 
        PROTECT _Appended		AS LOGIC	// Record has been added ?
        PROTECT _Deleted		AS LOGIC	// Record has been deleted ?
        PROTECT _HeaderDirty	AS LOGIC	// Header is dirty ?
        PROTECT _fLocked		AS LOGIC    // File Locked ?
        PROTECT _HeaderLocked	AS LOGIC
        PROTECT _PackMemo		AS LOGIC
        INTERNAL _OpenInfo		AS DbOpenInfo // current dbOpenInfo structure in OPEN/CREATE method
        PROTECT _ParentRelInfo	AS DbRelInfo  // parent rel struct   
        PROTECT _Locks			AS List<LONG>
        //PROTECT _DirtyRead		AS LONG
        //PROTECT _HasTrigger		AS LOGIC
        //PROTECT _Encrypted		AS LOGIC	// Current record Encrypted
        //PROTECT _TableEncrypted 	AS LOGIC	// Whole table encrypted
        //PROTECT _CryptKey		AS STRING       
        //PROTRECT _Trigger		as DbTriggerDelegate  
        PROTECT _oIndex			AS BaseIndex
        PROTECT _oMemo			AS BaseMemo
        PROTECT _Hot            AS LOGIC
        PROTECT _addFieldPos    AS LONG     // Used by AddFields Method, and SetFieldsExtent
        PROTECT _lockScheme     AS DbfLocking
        PROTECT _NewRecord      AS LOGIC
        //
        INTERNAL _LastCodeBlock    AS ICodeblock
        INTERNAL _EvalBlock        AS OBJECT
        
        CONSTRUCTOR()
            SELF:_hFile := F_ERROR
            SELF:_Header := DbfHeader{} // DbfHeader is a Structure, so the object is already created, no ?
            SELF:_Header:initialize()
            SELF:_lockScheme:Initialize( DbfLockingModel.Clipper52 )
            SELF:_Locks := List<LONG>{}
            
            
            //	METHOD DbEval(info AS DbEvalInfo) AS LOGIC
            /// <inheritdoc />
        METHOD GoTop() AS LOGIC
            IF ( SELF:_hFile != F_ERROR )
                BEGIN LOCK SELF
                    SELF:GoTo( 1 )
                    SELF:_Top := TRUE
                    SELF:_Bottom := FALSE
                    SELF:_BufferValid := FALSE
                    RETURN TRUE
                END LOCK
            ENDIF
            RETURN FALSE
            
            /// <inheritdoc />
        METHOD GoBottom() AS LOGIC
            IF ( SELF:_hFile != F_ERROR )
                BEGIN LOCK SELF
                    SELF:Goto( SELF:RecCount )
                    SELF:_Top := FALSE
                    SELF:_Bottom := TRUE
                    SELF:_BufferValid := FALSE
                    RETURN TRUE
                END LOCK
            ENDIF
            RETURN FALSE
            
            /// <inheritdoc />
        METHOD GoTo(nRec AS LONG) AS LOGIC
            IF ( SELF:_hFile != F_ERROR )
                BEGIN LOCK SELF
                    // Validate any pending change
                    SELF:GoCold()
                    // On Shared env, it can be correct to guess that some changes have been made
                    IF ( SELF:_Shared .AND. nRec > SELF:RecCount )
                        SELF:_RecCount := SELF:_calculateRecCount()
                    ENDIF
                    IF ( nRec <= SELF:RecCount ) .AND. ( nRec > 0 )
                        // virtual pos
                        SELF:_RecNo := nRec
                        SELF:_EOF := FALSE		
                        SELF:_Bof := FALSE
                        SELF:_Found :=TRUE
                        SELF:_BufferValid := FALSE
                    ELSE
                        // File empty, or trying to go outside ?
                        SELF:_RecNo := SELF:RecCount + 1
                        SELF:_EOF := TRUE		
                        SELF:_Bof := TRUE
                        SELF:_Found := FALSE
                        SELF:_BufferValid := FALSE
                    ENDIF
                    RETURN TRUE
                END LOCK
            ENDIF
            RETURN FALSE
            
            /// <inheritdoc />
        METHOD GoToId(oRec AS OBJECT) AS LOGIC
            LOCAL result AS LOGIC
            //
            BEGIN LOCK SELF
                TRY
                    VAR nRec := Convert.ToInt32( oRec )
                    result := SELF:Goto( nRec )
                CATCH
                    result := FALSE
                END TRY
            END LOCK
            RETURN result
            
            
            
            /// <inheritdoc />
        METHOD Skip(nToSkip AS INT) AS LOGIC
            LOCAL result AS LOGIC
            //
            IF ( SELF:_hFile != F_ERROR )
                SELF:_Top := FALSE
                SELF:_Bottom := FALSE
                // Refresh only
                IF ( nToSkip == 0 )
                    // FALSE because we have no Relation handling currently
                    RETURN FALSE
                ENDIF
                //
                result := SELF:Goto( SELF:RecNo + nToSKip )
                // We reached the top ?
                IF ( nToSkip < 0 ) .AND. SELF:_Bof .AND. result
                    SELF:GoTop()
                    SELF:_Bof := TRUE
                ENDIF
                //
                RETURN result
            ENDIF
            RETURN FALSE
            
            
            //	METHOD SkipFilter(nToSkip AS INT) AS LOGIC
            //	METHOD SkipRaw(nToSkip AS INT) AS LOGIC 
            //	METHOD SkipScope(nToSkip AS INT) AS LOGIC
            
            // Append and Delete
        METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
            LOCAL isOk := FALSE AS LOGIC
            IF ( SELF:_hFile != NULL )
                BEGIN LOCK SELF
                    // Validate
                    isOk := SELF:GoCold()
                    IF ( isOk )
                        //
                        IF ( SELF:_ReadOnly )
                            // Error !! Cannot be written !
                            SELF:_DbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
                            isOk := FALSE
                        ENDIF
                        IF ( SELF:_Shared )
                            IF ( SELF:_Locks:Count > 0 ) .AND. lReleaseLock
                                SELF:UnLock( 0 ) // Unlock All Records
                            ENDIF
                            SELF:AppendLock( DbLockMode.Lock )
                        ELSE
                            SELF:_HeaderLocked := FALSE
                        ENDIF
                        IF ( isOk )
                            // First, fill the Record Buffer with spaces
                            LOCAL i AS INT
                            LOCAL nStart AS INT
                            nStart := 1
                            IF __ARRAYBASE__ == 0
                                nStart -= 1
                            ENDIF
                            FOR i := nStart TO SELF:_RecordLength - ( 1 - nStart )
                                SELF:_RecordBuffer[ i ] := (BYTE)' '
                            NEXT
                            // Now, update state
                            SELF:_RecCount++
                            SELF:_RecNo := SELF:RecCount
                            SELF:_EOF := FALSE		
                            SELF:_Bof := FALSE
                            SELF:_Deleted := FALSE
                            SELF:_BufferValid := TRUE
                            SELF:_NewRecord := TRUE
                            // Mark RecordBuffer and Header as Hot
                            SELF:_Hot := TRUE
                            SELF:_Header:isHot := TRUE
                            // Now, Save
                            IF ( SELF:_HeaderLocked )
                                SELF:GoCold()
                                SELF:AppendLock( DbLockMode.UnLock )
                            ENDIF
                        ENDIF
                    ENDIF
                END LOCK
            ENDIF
            //
            RETURN isOk
            
            // Lock the "future" record (Recno+1) and the Header
            // Unlock the Header
        METHOD AppendLock( lockMode AS DbLockMode ) AS LOGIC
            LOCAL isOk := FALSE AS LOGIC
            //
            BEGIN LOCK SELF
                IF ( lockMode == DbLockMode.Lock )
                    // Try to Lock for 123 ms 
                    isOk := SELF:HeaderLock( lockMode )
                    IF ( isOk )
                        // Now Add the "new" record to the Locked Records ?
                        LOCAL newRecno := SELF:RecCount AS LONG
                        newRecno++
                        IF !SELF:_Locks:Contains( newRecno ) .AND. !SELF:_fLocked
                            isOk := SELF:_lockRecord( newRecno )
                        ENDIF
                        IF ( !isOk )
                            IF ( SELF:_HeaderLocked )
                                SELF:HeaderLock( DbLockMode.UnLock )
                            ENDIF
                            SELF:_dbfError( ERDD.APPENDLOCK, XSharp.Gencode.EG_APPENDLOCK )
                        ENDIF
                    ENDIF
                ELSE
                    SELF:HeaderLock( lockMode )
                ENDIF
            END LOCK
            //
            RETURN isOk
            
            // LockMethod.File      : Unlock all records and Lock the File
            // LockMethod.Exclusive : Unlock all records and lock the indicated record
            // LockMethod.Multiple  : Loc the indicated record
        METHOD Lock( lockInfo REF DbLockInfo ) AS LOGIC
            LOCAL isOk AS LOGIC
            //
            BEGIN LOCK SELF
                IF ( lockInfo:@@METHOD == DbLockInfo.LockMethod.Exclusive ) .OR. ;
                ( lockInfo:@@METHOD == DbLockInfo.LockMethod.Multiple )
                    isOk := SELF:_lockRecord( lockInfo )
                ELSEIF ( lockInfo:@@METHOD == DbLockInfo.LockMethod.File ) 
                    isOk := SELF:_lockDBFFile( )
                ELSE
                    isOk := TRUE
                ENDIF
            END LOCK
            RETURN isOk
            
            // Place a lock on the Header. The "real" offset locked depends on the Lock Scheme, defined by the DBF Type         
        METHOD HeaderLock( lockMode AS DbLockMode ) AS LOGIC
            //
            IF ( lockMode == DbLockMode.Lock )
                // Try to Lock for 123 ms 
                SELF:_HeaderLocked := SELF:_tryLock( SELF:_lockScheme:Offset, 1, (LONG)XSharp.RuntimeState.LockTries)
            ELSE
                TRY
                    SELF:_HeaderLocked := FFUnLock( SELF:_hFile, (DWORD)SELF:_lockScheme:Offset, 1 )
                CATCH
                    SELF:_HeaderLocked := FALSE
                END TRY
            ENDIF
            //
            RETURN SELF:_HeaderLocked
            
            // Unlock a indicated record number. If 0, Unlock ALL records
            // Then unlock the File if needed
        METHOD UnLock(oRecId AS OBJECT) AS LOGIC
            LOCAL recordNbr AS LONG
            LOCAL isOk AS LOGIC
            //
            IF ( SELF:_Shared )
                BEGIN LOCK SELF
                    //
                    SELF:GoCold()
                    TRY
                        recordNbr := Convert.ToInt32( oRecId )
                    CATCH
                        recordNbr := 0
                    END TRY
                    //
                    isOk := TRUE
                    IF ( SELF:_Locks:Count > 0 )
                        IF ( recordNbr == 0 )
                            FOREACH VAR nbr IN SELF:_Locks
                                isOk := isOk .AND. SELF:_unlockRecord( nbr )
                            NEXT
                            SELF:_Locks:Clear()  // Should be useless as the record is removed from the list in _unlockRecord
                        ELSE
                            isOk := SELF:_unlockRecord( recordNbr )
                        ENDIF
                    ENDIF
                    IF ( SELF:_fLocked ) .AND. ( recordNbr == 0 )
                        SELF:_fLocked := SELF:_unlockFile( )
                        isOk := isOk .AND. SELF:_fLocked
                    ENDIF
                END LOCK
            ELSE
                isOk := TRUE
            ENDIF
            RETURN isOk
            
            // Unlock file. The Offset depends on the LockScheme
        PROTECT METHOD _unlockFile( ) AS LOGIC
            LOCAL unlocked AS LOGIC
            LOCAL iOffset AS UINT64
            //
            iOffset := SELF:_lockScheme:Offset
            IF ( SELF:_lockScheme:Direction < 0 )
                iOffset -= SELF:_lockScheme:FileSize
            ELSE
                iOffset++
            ENDIF
            //
            TRY
                unlocked := FFUnLock( SELF:_hFile, (DWORD)iOffset, (DWORD)SELF:_lockScheme:FileSize )
            CATCH
                unlocked := FALSE
            END TRY
            RETURN unlocked
            
        // Unlock a record. The Offset depends on the LockScheme
        PROTECT METHOD _unlockRecord( recordNbr AS LONG ) AS LOGIC
            LOCAL unlocked AS LOGIC
            LOCAL iOffset AS UINT64
            //
            iOffset := SELF:_lockScheme:Offset
            IF ( SELF:_lockScheme:Direction < 0 )
                iOffset -= (UINT64)recordNbr
            ELSEIF( SELF:_lockScheme:Direction == 2 )
                iOffset += (UINT64)( ( recordNbr - 1 ) * SELF:_Header:RecordLen + SELF:_Header:HeaderLen )
            ELSE
                iOffset += (UINT64)recordNbr
            ENDIF
            //
            TRY
                unlocked := FFUnLock( SELF:_hFile, (DWORD)iOffset, (DWORD)SELF:_lockScheme:RecordSize )
            CATCH
                unlocked := FALSE
            END TRY
            IF( unlocked )
                SELF:_Locks:Remove( recordNbr )
            ENDIF
            RETURN unlocked
            
        // Lock the file. The Offset depends on the LockScheme
        PROTECT METHOD _lockFile( ) AS LOGIC
            LOCAL locked AS LOGIC
            LOCAL iOffset AS UINT64
            //
            iOffset := SELF:_lockScheme:Offset
            IF ( SELF:_lockScheme:Direction < 0 )
                iOffset -= SELF:_lockScheme:FileSize
            ELSE
                iOffset++
            ENDIF
            //
            TRY
                locked := FFLock( SELF:_hFile, (DWORD)iOffset, (DWORD)SELF:_lockScheme:FileSize )
            CATCH
                locked := FALSE
            END TRY
            RETURN locked            
            
            // Place a lock : <nOffset> indicate where the lock should be; <nLong> indicate the number bytes to lock
            // If it fails, the operation is tried <nTries> times, waiting 1ms between each operation.
        // Return the result of the operation
        PROTECTED METHOD _tryLock( nOffset AS UINT64, nLong AS LONG, nTries AS LONG  ) AS LOGIC
            LOCAL locked AS LOGIC
            //
            REPEAT
                TRY
                    locked := FFLock( SELF:_hFile, (DWORD)nOffset, (DWORD)nLong )
                CATCH
                    locked := FALSE
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
            
            // Lock the DBF File : All records are first unlocked, then the File is locked
        PROTECTED METHOD _lockDBFFile() AS LOGIC
            LOCAL isOk := TRUE AS LOGIC
            //
            IF ( SELF:_Shared .AND. !SELF:_fLocked )
                //
                SELF:GoCold()
                IF ( SELF:_Locks:Count > 0 )
                    //
                    FOREACH VAR nbr IN SELF:_Locks
                        SELF:_unlockRecord( nbr )
                    NEXT
                    SELF:_Locks:Clear()
                    //
                    SELF:_fLocked := SELF:_lockFile()
                    // Invalidate Buffer
                    SELF:Goto( SELF:RecNo )
                    isOk := SELF:_fLocked
                ENDIF
            ENDIF
            RETURN isOk
            
            // Lock a record number. The Offset depends on the LockScheme
        PROTECTED METHOD _lockRecord( recordNbr AS LONG ) AS LOGIC
            LOCAL locked AS LOGIC
            LOCAL iOffset AS UINT64
            //
            iOffset := SELF:_lockScheme:Offset
            IF ( SELF:_lockScheme:Direction < 0 )
                iOffset -= (UINT64)recordNbr
            ELSEIF( SELF:_lockScheme:Direction == 2 )
                iOffset += (UINT64)( ( recordNbr - 1 ) * SELF:_Header:RecordLen + SELF:_Header:HeaderLen )
            ELSE
                iOffset += (UINT64)recordNbr
            ENDIF
            //
            TRY
                locked := FFLock( SELF:_hFile, (DWORD)iOffset, (DWORD)SELF:_lockScheme:RecordSize )
            CATCH
                locked := FALSE
            END TRY
            IF locked
                SELF:_Locks:Add( recordNbr )
            ENDIF
            RETURN locked
            
            
            // LockMethod.Exclusive : Unlock all records and lock the indicated record
            // LockMethod.Multiple  : Loc the indicated record
        PROTECTED METHOD _lockRecord( lockInfo REF DbLockInfo ) AS LOGIC
            LOCAL nToLock := 0 AS UINT64
            LOCAL isOk AS LOGIC
            //
            isOk := TRUE
            IF ( lockInfo:RecId == NULL )
                nToLock := (UINT64)SELF:RecNo
            ELSE
                TRY
                    nToLock := Convert.ToUInt64( lockInfo:RecId )
                CATCH
                    SELF:_dbfError( ERDD.DATATYPE, XSharp.Gencode.EG_DATATYPE )
                    isOk := FALSE
                END TRY
                IF isOk
                    IF ( nToLock > SELF:RecCount ) .OR. ( nToLock < 1 )
                        isok := FALSE
                    ENDIF
                ENDIF
            ENDIF
            //
            IF ( isOk )
                // Already locked ?
                IF ( SELF:_Shared .AND. !SELF:_Locks:Contains( (LONG)nToLock ) )
                    IF ( lockInfo:@@METHOD == DbLockInfo.LockMethod.Multiple )
                        // Just add the lock to the list
                        isOk := SELF:_lockRecord( (LONG)nToLock )
                    ELSE // DbLockInfo.LockMethod.Exclusive
                        // One lock at a time
                        IF( SELF:_fLocked )
                            // Unlock all records and file
                            SELF:UnLock( 0 ) 
                        ENDIF
                        // Now, lock the one
                        isOk := SELF:_lockRecord( (LONG)nToLock )
                        // Go to there
                        SELF:Goto( (LONG)nToLock )
                    ENDIF
                ENDIF
            ENDIF
            //
            lockInfo:Result := isOk
            RETURN isOk
            
            
            // Un Delete the curretn Record            
        METHOD Recall() AS LOGIC
            LOCAL isOk AS LOGIC
            //
            isOk := SELF:_readRecord()
            IF isOk
                SELF:_RecordBuffer[ 0 ] := (BYTE)' ' 
                SELF:_Deleted := FALSE
                //
                SELF:GoHot()
            ELSE
                SELF:_DbfError( ERDD.READ, XSharp.Gencode.EG_READ )
            ENDIF
            RETURN isOk            
            
            // Mark the current record as DELETED
        METHOD Delete() AS LOGIC
            LOCAL isOk AS LOGIC
            //
            BEGIN LOCK SELF
                isOk := SELF:_readRecord()
                IF isOk
                    SELF:_RecordBuffer[ 0 ] := (BYTE)'*' 
                    SELF:_Deleted := TRUE
                    //
                    SELF:GoHot()
                ELSE
                    SELF:_DbfError( ERDD.READ, XSharp.Gencode.EG_READ )
                ENDIF
            END LOCK
            RETURN isOk
            
            // Retrieve the raw content of a record
        METHOD GetRec() AS BYTE[]  
            LOCAL records := NULL AS BYTE[]
            // Read Record to Buffer
            BEGIN LOCK SELF
                IF SELF:_readRecord()
                    //
                    records := BYTE[]{ SELF:_RecordLength }
                    Array.Copy( SELF:_RecordBuffer, 0, records, 0, SELF:_RecordLength)
                ENDIF
            END LOCK
            RETURN records
            
            // Put the content of a record as raw data
        METHOD PutRec(aRec AS BYTE[]) AS LOGIC 
            LOCAL isOk := FALSE AS LOGIC
            // First, Check the Size
            IF ( aRec:Length == SELF:_RecordLength )
                IF SELF:_readRecord()
                    Array.Copy( aRec, 0, SELF:_RecordBuffer, 0, SELF:_RecordLength)
                    isOk := SELF:GoHot()
                ENDIF
            ELSE
                SELF:_dbfError( ERDD.DATAWIDTH, XSharp.Gencode.EG_DATAWIDTH )
            ENDIF
            RETURN isOk
            
            // Suppress all DELETED record
        METHOD Pack() AS LOGIC
            LOCAL isOk AS LOGIC
            //
            IF ( SELF:_ReadOnly )
                // Error !! Cannot be written !
                SELF:_DbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
                RETURN FALSE
            ENDIF
            //
            IF ( SELF:_Shared )
                // Error !! Cannot be written !
                SELF:_DbfError( ERDD.SHARED, XSharp.Gencode.EG_SHARED )
                RETURN FALSE
            ENDIF
            //
            isOk := SELF:GoCold()
            IF isOk
                LOCAL nToRead AS LONG
                LOCAL nMoveTo AS LONG
                LOCAL nTotal AS LONG
                LOCAL lDeleted AS LOGIC
                //
                nToRead := 1
                nMoveTo := 1
                nTotal := 0
                WHILE ( nToRead <= SELF:RecCount )
                    // Move
                    SELF:GoTo( nToRead )
                    // and get Data
                    SELF:_readRecord()
                    lDeleted := SELF:_Deleted
                    //
                    IF ( !lDeleted )
                        nTotal++
                        IF ( nToRead != nMoveTo )
                            SELF:_RecNo := nMoveTo
                            SELF:_writeRecord()
                        ENDIF
                    ENDIF
                    // Next
                    nToRead ++
                    IF ( !lDeleted )
                        nMoveTo++
                    ENDIF
                ENDDO
                //
                SELF:_Hot := FALSE
                SELF:_RecCount := nTotal
                SELF:Flush()
                //
            ENDIF
            RETURN isOk
            
            
            // Remove all records 
        METHOD Zap() AS LOGIC
            LOCAL isOk AS LOGIC
            //
            IF ( SELF:_ReadOnly )
                // Error !! Cannot be written !
                SELF:_DbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
                RETURN FALSE
            ENDIF
            //
            IF ( SELF:_Shared )
                // Error !! Cannot be written !
                SELF:_DbfError( ERDD.SHARED, XSharp.Gencode.EG_SHARED )
                RETURN FALSE
            ENDIF
            //
            isOk := SELF:GoCold()
            IF isOk
                SELF:Goto(0)
                // Zap means, set the RecCount to zero, so any other write with overwrite datas
                SELF:_RecCount := 0
                SELF:_Header:isHot := TRUE
                SELF:_writeHeader()
                // Memo File ?
                IF ( SELF:_HasMemo )
                    // Zap Memo
                    IF _oMemo != NULL
                        RETURN _oMemo:Zap()
                    ELSE                            
                        RETURN SUPER:Zap()
                    ENDIF
                ENDIF
            ENDIF
            RETURN isOk
            
            // Open and Close   
            /// <inheritdoc />
        METHOD Close() 			AS LOGIC 
            LOCAL isOk := FALSE AS LOGIC
            IF ( SELF:_hFile != NULL )
                // Validate
                isOk := SELF:GoCold()
                //
                IF ( isOk )
                    SELF:UnLock(0)
                    //
                    IF ( !SELF:_ReadOnly )
                        SELF:Flush()
                    ENDIF
                    IF ( SELF:_HeaderLocked )
                        SELF:HeaderLock( DbLockMode.UnLock )
                    ENDIF
                    //
                    TRY
                        //SUPER:Close()
                        isOk := FClose( SELF:_hFile )
                        IF ( SELF:_HasMemo )
                            SELF:CloseMemFile()
                        ENDIF
                    CATCH
                        isOk := FALSE
                    END TRY
                    SELF:_hFile := NULL
                ENDIF
            ENDIF
            RETURN isOk
            
            // Move to the End of file, and place a End-Of-File Marker (0x1A)
        PRIVATE METHOD _putEndOfFileMarker() AS LOGIC
            // According to DBASE.com Knowledge base :
            // The end of the file is marked by a single byte, with the end-of-file marker, an OEM code page character value of 26 (0x1A).
            LOCAL lOffset := SELF:_Header:HeaderLen + SELF:_RecCount * SELF:_Header:RecordLen AS LONG
            LOCAL isOk := ( FSeek3( SELF:_hFile, lOffset, FS_SET ) == lOffset ) AS LOGIC
            LOCAL eofMarker := <BYTE>{ 26 } AS BYTE[]
            IF ( isOk )
                isOk := ( FWrite3( SELF:_hFile, eofMarker, 1 ) == 1 )
                IF isOk
                    // Fix length of File
                    isOk := FChSize( SELF:_hFile, (DWORD)lOffset+1)
                ENDIF
            ENDIF
            RETURN isOk
            
            // Create a DBF File, based on the DbOpenInfo Structure
            // Write the File Header, and the Fields Header; Create the Memo File if needed
        METHOD Create(info AS DbOpenInfo) AS LOGIC  
            LOCAL isOK AS LOGIC
            //
            isOk := FALSE
            IF ( SELF:_Fields:Length == 0 )
                RETURN FALSE
            ENDIF
            SELF:_OpenInfo := info
            // Should we set to .DBF per default ?
            IF String.IsNullOrEmpty(SELF:_OpenInfo:Extension)
                SELF:_OpenInfo:Extension := ".DBF"
                //
                SELF:_OpenInfo:FileName := System.IO.Path.ChangeExtension( SELF:_OpenInfo:FileName, SELF:_OpenInfo:Extension )
            ENDIF
            //
            SELF:_Hot := FALSE
            SELF:_FileName := SELF:_OpenInfo:FileName
            SELF:_Shared := SELF:_OpenInfo:Shared
            SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
            //
            SELF:_hFile    := FCreate( SELF:_FileName) 
            IF ( SELF:_hFile != F_ERROR )
                LOCAL fieldCount :=  SELF:_Fields:Length AS INT
                LOCAL fieldDefSize := fieldCount * FLDOFFSETS.SIZE AS INT
                // First, just the Header
                SELF:_Header:HeaderLen := HDROFFSETS.SIZE + fieldDefSize + 1
                SELF:_Header:isHot := TRUE
                // Init Header, should it be a parameter ?
                SELF:_Header:Version := DBFVersion.FoxBaseDBase3NoMemo
                IF ( SELF:_HasMemo )
                    IF ( SELF:_Header:Version == DBFVersion.dBase4 )
                        SELF:_Header:Version := DBFVersion.dBase4WithMemo_
                    ELSE
                        SELF:_Header:Version := ( SELF:_Header:Version | 0x80 ) // 0x80 == Memo Flag
                    ENDIF
                ENDIF
                // This had been initialized by AddFields()
                SELF:_Header:RecordLen := (SHORT)SELF:_RecordLength
                // This will fill the Date and RecCount
                isOK := SELF:_writeHeader()
                IF ( isOK )
                    isOk := SELF:_writeFieldsHeader()
                    IF ( isOk )
                        IF ( SELF:_HasMemo )
                            isOk := SELF:CreateMemFile( info )
                        ENDIF  
                        // Don't forget to allocate memory for Records
                        SELF:_RecordBuffer := BYTE[]{ SELF:_RecordLength}
                        // 
                    ENDIF
                ENDIF
                IF ( !isok )
                    IF ( SELF:_HasMemo )
                        SELF:CloseMemFile( )
                    ENDIF  
                    FClose( SELF:_hFile )
                ELSE
                    SELF:GoTop()
                ENDIF
            ELSE
                SELF:_DbfError( ERDD.CREATE_FILE, XSharp.Gencode.EG_CREATE )
            ENDIF
            RETURN isOK
            
            // Write the Fields Header, based on the _Fields List            
        PRIVATE METHOD _writeFieldsHeader() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL fieldCount :=  SELF:_Fields:Length AS INT
            LOCAL fieldDefSize := fieldCount * FLDOFFSETS.SIZE AS INT
            // Now, create the Structure
            LOCAL fieldsBuffer := BYTE[]{ fieldDefSize +1 } AS BYTE[] // +1 to add 0Dh stored as the field terminator. 
            LOCAL currentField := DbfField{} AS DbfField
            currentField:Initialize()
            LOCAL nStart AS INT
            nStart := 1
            IF __ARRAYBASE__ == 0
                nStart -= 1
            ENDIF
            FOR VAR i := nStart TO fieldCount - ( 1 - nStart )
                //
                currentField:Name := SELF:_Fields[ i ]:Name
                currentField:Type := SELF:_Fields[ i ]:FieldType
                currentField:Len := (BYTE)SELF:_Fields[ i ]:Length
                currentField:Dec := (BYTE)SELF:_Fields[ i ]:Decimals
                //
                Array.Copy( currentField:Buffer, 0, fieldsBuffer, i*FLDOFFSETS.SIZE, FLDOFFSETS.SIZE )
            NEXT
            // Terminator
            fieldsBuffer[fieldDefSize] := 13
            // Go end of Header
            isOk := ( FSeek3( SELF:_hFile, HDROFFSETS.SIZE, SeekOrigin.Begin ) == HDROFFSETS.SIZE )
            IF ( isOk )
                // Write Fields and Terminator
                TRY
                    isOk := ( FWrite3( SELF:_hFile, fieldsBuffer, (DWORD)fieldsBuffer:Length ) == (DWORD)fieldsBuffer:Length )
                CATCH 
                    SELF:_DbfError( ERDD.WRITE, XSharp.Gencode.EG_WRITE )
                END TRY
            ENDIF
            //
            RETURN isOk
            
            /// <inheritdoc />
        METHOD Open(info AS XSharp.RDD.DbOpenInfo) AS LOGIC
            LOCAL isOK AS LOGIC
            //
            isOk := FALSE
            SELF:_OpenInfo := info
            // Should we set to .DBF per default ?
            IF String.IsNullOrEmpty(SELF:_OpenInfo:Extension)
                SELF:_OpenInfo:Extension := ".DBF"
                //
                SELF:_OpenInfo:FileName := System.IO.Path.ChangeExtension( SELF:_OpenInfo:FileName, SELF:_OpenInfo:Extension )
            ENDIF
            //
            SELF:_Hot := FALSE
            SELF:_FileName := SELF:_OpenInfo:FileName
            SELF:_Shared := SELF:_OpenInfo:Shared
            SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
            SELF:_hFile    := Fopen(SELF:_FileName, SELF:_OpenInfo:FileMode) 
            IF ( SELF:_hFile != F_ERROR )
                isOk := SELF:_ReadHeader()
                IF ( isOk )
                    IF ( SELF:_HasMemo )
                        isOk := SELF:OpenMemFile( info )
                    ENDIF
                    SELF:GoTop()
                ELSE
                    SELF:_DbfError( ERDD.CORRUPT_HEADER, XSharp.Gencode.EG_CORRUPTION )
                ENDIF
            ELSE
                // Error or just FALSE ?
                isOK := FALSE
                //SELF:_DbfError( ERDD.OPEN_FILE, XSharp.Gencode.EG_OPEN )
            ENDIF
            //
            RETURN isOk
            
            // Read the DBF Header, retrieve RecCount, then read the Fields Header 
        PRIVATE METHOD _readHeader() AS LOGIC
            LOCAL isOk AS LOGIC
            //
            isOk := ( FRead3(SELF:_hFile, SELF:_Header:Buffer, HDROFFSETS.SIZE) == HDROFFSETS.SIZE )
            //
            IF ( isOk )
                LOCAL fieldCount := (( SELF:_Header:HeaderLen - HDROFFSETS.SIZE) / FLDOFFSETS.SIZE ) AS INT
                // Something wrong in Size...
                IF ( fieldCount <= 0 )
                    RETURN FALSE
                ENDIF
                SELF:_RecCount := SELF:_Header:RecCount
                // Move to top, after header
                isOk := ( FSeek3( SELF:_hFile, HDROFFSETS.SIZE, SeekOrigin.Begin ) == HDROFFSETS.SIZE )
                IF ( isOk )
                    isOk := _readFieldsHeader()
                ENDIF
            ENDIF
            RETURN isOk
            
            // Read the Fields Header, filling the _Fields List with RddFieldInfo            
        PRIVATE METHOD _readFieldsHeader() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL fieldCount := (( SELF:_Header:HeaderLen - HDROFFSETS.SIZE) / FLDOFFSETS.SIZE ) AS INT
            LOCAL fieldDefSize := fieldCount * FLDOFFSETS.SIZE AS INT
            //
            // Read full Fields Header
            VAR fieldsBuffer := BYTE[]{ fieldDefSize } 
            isOk := ( FRead3( SELF:_hFile, fieldsBuffer, (DWORD)fieldDefSize ) == (DWORD)fieldDefSize )
            IF ( isOk )
                SELF:_HasMemo := FALSE
                VAR currentField := DbfField{}
                currentField:Initialize()
                // Now, process
                SELF:_Fields := DbfRddFieldInfo[]{ fieldCount }
                LOCAL nStart AS INT
                nStart := 1
                IF __ARRAYBASE__ == 0
                    nStart -= 1
                ENDIF
                FOR VAR i := nStart TO fieldCount - ( 1 - nStart )
                    //
                    Array.Copy( fieldsBuffer, i*FLDOFFSETS.SIZE, currentField:Buffer, 0, FLDOFFSETS.SIZE )
                    SELF:_Fields[ i ] := DbfRddFieldInfo{ currentField:Name, currentField:Type, currentField:Len, currentField:Dec }
                    // !!! WARNING !!! The Field Index MUST NOT BE corrected by __ARRAYBASE__ when
                    // calling _isMemoField()
                    IF SELF:_isMemoField( i + ( 1 - nStart ))
                        SELF:_HasMemo := TRUE
                    ENDIF
                NEXT
                // Allocate the Buffer to read Records
                SELF:_RecordLength := (WORD)SELF:_Header:RecordLen
                SELF:_RecordBuffer := BYTE[]{ SELF:_RecordLength}
            ENDIF
            RETURN isOk
            
            // Write the DBF file Header : Last DateTime of modification (now), Current Reccount    
        PRIVATE METHOD _writeHeader() AS LOGIC
            LOCAL ret := TRUE AS LOGIC
            // Really ?
            IF ( SELF:_Header:isHot )
                //
                IF ( SELF:_ReadOnly )
                    // Error !! Cannot be written !
                    SELF:_DbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
                    RETURN FALSE
                ENDIF
                // Update the Date/Time information
                LOCAL dtInfo AS DateTime
                dtInfo := DateTime.Now
                SELF:_Header:Year := (BYTE)dtInfo:Year
                SELF:_Header:Month := (BYTE)dtInfo:Month
                SELF:_Header:Day := (BYTE)dtInfo:Day
                // Update the number of records
                SELF:_Header:RecCount := SELF:RecCount
                // Now Write
                // Go Top
                FSeek3( SELF:_hFile, 0, FS_SET )
                // Write just the File Header
                TRY
                    ret := ( FWrite3( SELF:_hFile, SELF:_Header:Buffer, (DWORD)HDROFFSETS.SIZE ) == (DWORD)HDROFFSETS.SIZE )
                CATCH 
                    SELF:_DbfError( ERDD.WRITE, XSharp.Gencode.EG_WRITE )
                    ret := FALSE
                END TRY
                // Ok, go Cold
                SELF:_Header:isHot := FALSE
            ENDIF
            //
            RETURN ret
            
            // Filtering and Scoping 
            //	METHOD ClearFilter() 	AS LOGIC
            //	METHOD ClearScope() 	AS LOGIC 
            //	METHOD Continue()		AS LOGIC     
            //	METHOD GetScope()		AS DbScopeInfo 
            //	METHOD ScopeInfo(nOrdinal AS LONG) AS OBJECT
            //	METHOD SetFilter(info AS DbFilterInfo) AS LOGIC 
            //	METHOD SetScope(info AS DbScopeInfo) AS LOGIC
            
            // Fields
            // Set the Number of Fields the AddField Method will add
        METHOD SetFieldExtent( fieldCount AS LONG ) AS LOGIC
            // Initialize the Fields array
            SELF:_Fields := DbfRddFieldInfo[]{ fieldCount }
            SELF:_addFieldPos := 0
            SELF:_RecordLength := 1 // 1 for DELETED
            SELF:_HasMemo := FALSE
            RETURN TRUE
            
            // Add a Field to the _Fields List. Fields are added in the order of method call
        METHOD AddField(info AS RddFieldInfo) AS LOGIC
            LOCAL isOk AS LOGIC
            // Check if the FieldName does already exist
            isok := TRUE
            LOCAL nStart AS INT
            nStart := 1
            IF __ARRAYBASE__ == 0
                nStart -= 1
            ENDIF
            FOR VAR i := nStart TO SELF:_Fields:Length - ( 1 - nStart )
                //
                IF ( SELF:_Fields[i] != NULL )
                    isOk := ( String.Compare( info:Name, SELF:_Fields[i]:Name, TRUE ) != 0 )
                    IF !isOk
                        EXIT
                    ENDIF
                ENDIF
            NEXT
            IF isOk
                IF ( SELF:_addFieldPos < SELF:_Fields:Length )
                    SELF:_checkFields( info )
                    SELF:_Fields[ SELF:_addFieldPos++ ] := DbfRddFieldInfo{ info:Name, info:FieldType, info:Length, info:Decimals } 
                    SELF:_RecordLength += (WORD)info:Length
                ELSE
                    isOk := FALSE
                ENDIF
            ENDIF
            IF ( isOk ) .AND. info:FieldType == DbFieldType.Memo 
                SELF:_HasMemo := TRUE
            ENDIF
            RETURN isOk
            
            // Check if a Field definition is correct :
            // Date Length must be 8, Number are long enough to store Dot anf Decs (if any), ...
        PROTECT METHOD _checkFields(info REF RddFieldInfo) AS VOID
            // FieldName
            info:Name := info:Name:ToUpper():Trim()
            IF ( info:Name:Length > 10 )
                info:Name := info:Name:Substring(0,10)
            ENDIF
            //
            SWITCH info:FieldType
                CASE DbFieldType.Character
                    IF ( info:Length == 0 ) .OR. ( info:Decimals > 0 ) .OR. (info:Length > System.UInt16.MaxValue )
                        SELF:_dbfError( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                    ENDIF
                CASE DbFieldType.Number
                    IF ( info:Length >= 1 ) .AND. ( info:Length <= 255 )
                        IF ( info:Decimals > 0 )
                            // We must check that we have enough space for DOT and decimal
                            IF ( info:Length <= 2 ) .OR. ( info:Decimals >= info:Length -1 )
                                SELF:_dbfError( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                            ENDIF
                        ENDIF
                    ELSE
                        SELF:_dbfError( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                    ENDIF
                CASE DbFieldType.Date
                    IF ( info:Length != 8 ) .OR. ( info:Decimals != 0 ) 
                        SELF:_dbfError( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                    ENDIF
                CASE DbFieldType.Logic
                    IF ( info:Length != 1 ) .OR. ( info:Decimals != 0 ) 
                        SELF:_dbfError( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                    ENDIF
                CASE DbFieldType.Memo
                    IF ( info:Length != 10 ) .OR. ( info:Decimals != 0 ) 
                        SELF:_dbfError( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                    ENDIF
                OTHERWISE
                    // To be done : Support of Fox Field Types, ....
                    info:FieldType := DbFieldType.Unknown
            END SWITCH
            RETURN
            
        // Add the list of Fields ot the DBF, by calling SetFieldExtent and AddField
        METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC
            // Ok, this will set the Fields for the current object
            // But what if the file is ALREADY opened ????
            LOCAL fieldCount AS LONG
            LOCAL isOk := FALSE AS LOGIC
            fieldCount := aFields:Length
            IF ( fieldCount > 0 )
                isOk := SELF:SetFieldExtent( fieldCount )
                IF isOk
                    LOCAL nStart AS INT
                    nStart := 1
                    IF __ARRAYBASE__ == 0
                        nStart -= 1
                    ENDIF
                    FOR VAR i := nStart TO fieldCount - ( 1 - nStart )
                        //
                        isOk := SELF:AddField( aFields[i] )
                        IF !isOk
                            EXIT
                        ENDIF
                    NEXT
                ENDIF
            ENDIF
            RETURN isOk
            
            /// <inheritdoc />
        METHOD FieldIndex(fieldName AS STRING) AS LONG
            IF ( SELF:_hFile != F_ERROR )
                LOCAL i AS LONG
                // FieldIndex is One-Based
                i := 1
                FOREACH VAR fld IN SELF:_Fields
                    IF ( String.Compare( fld:Name, fieldName, TRUE )==0 )
                        RETURN i
                    ENDIF
                    i++
                NEXT
            ENDIF
            RETURN 0
            
            /// <inheritdoc />
        METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
            LOCAL oResult AS OBJECT
            LOCAL nArrPos := nFldPos AS LONG
            BEGIN LOCK SELF
                IF SELF:_FieldIndexValidate(nArrPos)
                    IF __ARRAYBASE__ == 0
                        nArrPos -= 1
                    ENDIF
                ENDIF
                //
                SWITCH nOrdinal
                    CASE DbFieldInfo.DBS_NAME
                        oResult := SELF:_Fields[nArrPos]:Name
                    CASE DbFieldInfo.DBS_LEN
                        oResult := SELF:_Fields[nArrPos]:Length
                    CASE DbFieldInfo.DBS_DEC
                        oResult := SELF:_Fields[nArrPos]:Decimals
                    CASE DbFieldInfo.DBS_TYPE
                        oResult := SELF:_Fields[nArrPos]:FieldType:ToString():Substring(0,1)
                    CASE DbFieldInfo.DBS_ALIAS
                        oResult := SELF:_Fields[nArrPos]:Alias
                        
                    CASE DbFieldInfo.DBS_ISNULL
                    CASE DbFieldInfo.DBS_COUNTER
                    CASE DbFieldInfo.DBS_STEP    
                    
                CASE DbFieldInfo.DBS_BLOB_GET     
                    CASE DbFieldInfo.DBS_BLOB_TYPE	// Returns the data type of a BLOB (memo) field. This
                        // is more efficient than using Type() or ValType()
                        // since the data itself does not have to be retrieved
                        // from the BLOB file in order to determine the type.
                    CASE DbFieldInfo.DBS_BLOB_LEN	    // Returns the storage length of the data in a BLOB (memo) file	
                    CASE DbFieldInfo.DBS_BLOB_OFFSET	// Returns the file offset of the data in a BLOB (memo) file.
                    CASE DbFieldInfo.DBS_BLOB_POINTER	// Returns a numeric pointer to the data in a blob
                        // file. This pointer can be used with BLOBDirectGet(),
                        // BLOBDirectImport(), etc.
                        
                    CASE DbFieldInfo.DBS_BLOB_DIRECT_TYPE		
                    CASE DbFieldInfo.DBS_BLOB_DIRECT_LEN		
                    
                    CASE DbFieldInfo.DBS_STRUCT				
                    CASE DbFieldInfo.DBS_PROPERTIES			
                    CASE DbFieldInfo.DBS_USER		
                    
                    OTHERWISE
                        oResult := SUPER:FieldInfo(nFldPos, nOrdinal, oNewValue)
                    END SWITCH
            END LOCK
            RETURN oResult
            
            /// <inheritdoc />
        METHOD FieldName(nFldPos AS LONG) AS STRING
            RETURN SUPER:FieldName( nFldPos )
            
            
            // Read & Write		
            
            // Move to the current record, then read the raw Data into the internal RecordBuffer; Set the DELETED Flag 
        PRIVATE METHOD _readRecord() AS LOGIC
            LOCAL isOk AS LOGIC
            // Buffer is supposed to be correct
            IF ( SELF:_BufferValid == TRUE )
                RETURN TRUE
            ENDIF
            // File Ok ?
            isOk := ( SELF:_hFile != F_ERROR )
            //
            IF ( isOk )
                // Record pos is One-Based
                LOCAL lOffset := SELF:_Header:HeaderLen + ( SELF:_RecNo - 1 ) * SELF:_Header:RecordLen AS LONG
                isOk := ( FSeek3( SELF:_hFile, lOffset, FS_SET ) == lOffset )
                IF ( isOk )
                    // Read Record
                    isOk := ( FRead3( SELF:_hFile, SELF:_RecordBuffer, (DWORD)SELF:_Header:RecordLen ) == (DWORD)SELF:_Header:RecordLen )
                    IF ( isOk )
                        SELF:_BufferValid := TRUE
                        SELF:_Deleted := ( SELF:_RecordBuffer[ 0 ] == '*' )
                    ENDIF
                ENDIF
            ENDIF
            RETURN isOk
            
            // Move to the current record, write the raw Data, then update the Header (for DateTime mainly)
        PRIVATE METHOD _writeRecord() AS LOGIC
            LOCAL isOk AS LOGIC
            // File Ok ?
            isOk := ( SELF:_hFile != F_ERROR )
            //
            IF ( isOk )
                //
                IF ( SELF:_ReadOnly )
                    // Error !! Cannot be written !
                    SELF:_DbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
                    isOk := FALSE
                ELSE
                    // Write Current Data Buffer
                    // Record pos is One-Based
                    LOCAL recordPos := SELF:_Header:HeaderLen + ( SELF:_RecNo - 1 ) * SELF:_Header:RecordLen AS LONG
                    isOk := ( FSeek3( SELF:_hFile, recordPos, FS_SET ) == recordPos )
                    IF (isOk)
                        // Write Record
                        TRY
                            FWrite3( SELF:_hFile, SELF:_RecordBuffer, (DWORD)SELF:_Header:RecordLen )
                            // Don't forget to Update Header
                            SELF:_Header:isHot := TRUE
                            IF ( SELF:_Shared )
                                SELF:_writeHeader()
                            ENDIF
                        CATCH 
                            SELF:_DbfError( ERDD.WRITE, XSharp.Gencode.EG_WRITE )
                        END TRY
                    ENDIF 
                ENDIF
            ENDIF
            RETURN isOk
            
            // Convert a Julian Date to a System.DateTime Date
        PROTECT METHOD _julianToDateTime(julianDateAsLong AS INT64) AS System.DateTime
            LOCAL num2 AS REAL8
            LOCAL num3 AS REAL8
            LOCAL num4 AS REAL8
            LOCAL num5 AS REAL8
            LOCAL num6 AS REAL8
            LOCAL num7 AS REAL8
            LOCAL num8 AS REAL8
            LOCAL num9 AS REAL8
            LOCAL num10 AS REAL8
            LOCAL num11 AS REAL8
            //
            IF (julianDateAsLong == 0)
                //
                RETURN System.DateTime.MinValue
            ENDIF
            num2 := (System.Convert.ToDouble(julianDateAsLong) + 68569)
            num3 := System.Math.Floor((REAL8)((4 * num2) / 146097) )
            num4 := (num2 - System.Math.Floor((REAL8)(((146097 * num3) + 3) / 4) ))
            num5 := System.Math.Floor((REAL8)((4000 * (num4 + 1)) / 1461001) )
            num6 := ((num4 - System.Math.Floor((REAL8)((1461 * num5) / 4) )) + 31)
            num7 := System.Math.Floor((REAL8)((80 * num6) / 2447) )
            num8 := (num6 - System.Math.Floor((REAL8)((2447 * num7) / 80) ))
            num9 := System.Math.Floor((REAL8)(num7 / 11) )
            num10 := ((num7 + 2) - (12 * num9))
            num11 := (((100 * (num3 - 49)) + num5) + num9)
            RETURN System.DateTime{System.Convert.ToInt32(num11), System.Convert.ToInt32(num10), System.Convert.ToInt32(num8)}
            
        // Convert a System.DateTime Date to a Julian Date
        PROTECT METHOD _dateTimeToJulian( dt AS DateTime ) AS LONG
            LOCAL Month := dt:Month AS INT
            LOCAL Day := dt:Day AS INT
            LOCAL Year := dt:Year AS INT
            LOCAL result AS LONG
            //
            IF ( Month < 3 )
                Month := Month + 12
                Year := Year - 1
            ENDIF
            //modified Julian Date
            result := Day + (153 * Month - 457) / 5 + 365 * Year + (Year / 4) - (Year / 100) + (Year / 400) - 678882
            RETURN result
            
        // Convert the data stored in the buffer (BYTE[]) to an .NET Object. The convertion is drived by fieldType
        INTERNAL VIRTUAL METHOD _convertDataToField( buffer AS BYTE[], fieldType AS DbFieldType, nDec AS LONG) AS OBJECT
            LOCAL str AS STRING
            LOCAL data AS OBJECT
            LOCAL encoding AS ASCIIEncoding
            // Read actual Data
            encoding := ASCIIEncoding{}
            str :=  encoding:GetString(buffer)
            IF ( str == NULL )
                str := String.Empty
            ENDIF
            // !!! WARNING !!! Space char can be significant (specially in Memo!)
            // str := str:Trim()
            //
            data := NULL
            SWITCH fieldType
            CASE DbFieldType.Float
                CASE DbFieldType.Number
                CASE DbFieldType.Double
                    //
                    IF (! String.IsNullOrWhiteSpace(str))
                        //
                        IF ( fieldType == DbFieldType.Number ) .AND. (nDec == 0 )
                            data := System.Convert.ToInt32(str)
                        ELSE
                            data := System.Convert.ToDouble(str)
                        ENDIF
                    ENDIF
                    //					IF ((DbFieldType:Flags & DBFFieldFlags.AllowNullValues) != DBFFieldFlags.AllowNullValues)
                    //						//
                    //						data := 0.0
                    //					ENDIF
                CASE DbFieldType.Character
                    //
                    str := str:Trim()
                    data := str
                CASE DbFieldType.Date
                    //
                    IF (! String.IsNullOrWhiteSpace(str))
                        //
                        data := System.DateTime.ParseExact(str, "yyyyMMdd", System.Globalization.CultureInfo.InvariantCulture)
                    ENDIF
                    //                    IF ((FIELD:Flags & DBFFieldFlags.AllowNullValues) != DBFFieldFlags.AllowNullValues)
                    //                        //
                    //                        data := System.DateTime.MinValue
                    //                    ENDIF
                    
                CASE DbFieldType.Integer
                    //
                    IF (! String.IsNullOrWhiteSpace(str))
                        //
                        data := System.BitConverter.ToInt32(buffer, 0)
                    ENDIF
                    //                    IF ((FIELD:Flags & DBFFieldFlags.AllowNullValues) != DBFFieldFlags.AllowNullValues)
                    //                        //
                    //                        data := 0
                    //                    ENDIF
                CASE DbFieldType.Logic
                    //
                    IF (! String.IsNullOrWhiteSpace(str))
                        //
                        data := ( ( String.Compare( str, "T", TRUE ) == 0 ) .OR. ( String.Compare( str, "Y", TRUE ) == 0 ) )
                    ENDIF
                    //                    IF ((FIELD:Flags & DBFFieldFlags.AllowNullValues) != DBFFieldFlags.AllowNullValues)
                    //                        //
                    //                        data := FALSE
                    //                    ENDIF
                CASE DbFieldType.DateTime
                    //
                    IF (! (String.IsNullOrWhiteSpace(str) .OR. (System.BitConverter.ToInt64(buffer, 0) == 0)))
                        //
                        data := _julianToDateTime(System.BitConverter.ToInt64(buffer, 0))
                    ENDIF
                    //                    IF ((FIELD:Flags & DBFFieldFlags.AllowNullValues) != DBFFieldFlags.AllowNullValues)
                    //                        //
                    //                        data := System.DateTime.MinValue
                    //                    ENDIF
                CASE DbFieldType.Currency
                    IF (!String.IsNullOrWhiteSpace(str))
                        data := System.Convert.ToDecimal(str)
                        //                    ELSE
                        //                        IF ((FIELD:Flags & DBFFieldFlags.AllowNullValues) == DBFFieldFlags.AllowNullValues)
                        //                            //
                        //                            data := NULL
                        //                        ELSE
                        //                            data := 0.0
                        //                        ENDIF
                    ENDIF
                OTHERWISE
                    data := buffer
                END SWITCH
            RETURN Data
            
            // Convert the value (OBJECT) to the corresponding DBF Type (fieldType), and put the result in binary form (buffer)
        INTERNAL VIRTUAL METHOD _convertFieldToData( oValue AS OBJECT, buffer AS BYTE[], fieldType AS DbFieldType, dec AS LONG) AS LOGIC
            LOCAL objType AS System.Type
            LOCAL objTypeCode AS System.TypeCode
            LOCAL encoding AS ASCIIEncoding
            LOCAL isOk := FALSE AS LOGIC
            LOCAL str AS STRING
            // 
            encoding := ASCIIEncoding{}
            objType := oValue:GetType()
            objTypeCode := Type.GetTypeCode( objType )
            //
            SWITCH objTypeCode
            CASE TypeCode.String
                CASE TypeCode.Char
                    IF ( fieldType == DbFieldType.Character )
                        IF ( objTypeCode == TypeCode.Char )
                            str := STRING{ (CHAR)oValue, 1 }
                        ELSE
                            str := oValue ASTYPE STRING
                        ENDIF
                        // Get the smallest size
                        LOCAL len, i AS LONG
                        len := Math.Min( str:Length, buffer:Length )
                        encoding:GetBytes( str, 0, len, buffer, 0 )
                        // Pad with spaces (Is it a good idea ???)
                        FOR i := len TO buffer:Length -1
                            buffer[i] := (BYTE)' '
                        NEXT
                        //
                        isOk := TRUE
                    ELSE
                        // Type Error !
                        isOk := FALSE
                    ENDIF
                    
                CASE TypeCode.Decimal
                CASE TypeCode.Double
                CASE TypeCode.Single
                
                CASE TypeCode.Byte
                CASE TypeCode.SByte
                CASE TypeCode.Int16
                CASE TypeCode.Int32
                CASE TypeCode.Int64
                CASE TypeCode.UInt16
                CASE TypeCode.UInt32
                CASE TypeCode.UInt64   
                
                    IF ( fieldType == DbFieldType.Number ) .OR. ;
                    ( fieldType == DbFieldType.Float ) .OR. ;
                    ( fieldType == DbFieldType.Double ) .OR. ;
                    ( fieldType == DbFieldType.Integer )
                        LOCAL format AS NumberFormatInfo
                        //
                        format := NumberFormatInfo{}
                        format:NumberDecimalSeparator := "."
                        format:NumberDecimalDigits := dec
                        //
                        str := Convert.ToString( oValue, format )
                        IF ( str:Length > buffer:Length )
                            // Oversize Error !
                            isOk := FALSE
                        ELSE
                            str := str:PadLeft(buffer:Length)
                            encoding:GetBytes( str, 0, buffer:Length, buffer, 0 )
                            isOk := TRUE
                        ENDIF
                    ELSE
                        // Type Error !
                        isOk := FALSE
                    ENDIF
                    
                CASE TypeCode.Boolean
                    IF ( fieldType == DbFieldType.Logic )
                        buffer[0] := IIF( (LOGIC)oValue, (BYTE)'T', (BYTE)'F' )
                        isOk := TRUE
                    ELSE
                        // Type Error !
                        isOk := FALSE
                    ENDIF
                    
                CASE TypeCode.DateTime
                    IF ( fieldType == DbFieldType.Date )
                        LOCAL dt AS DateTime
                        dt := (DateTime)oValue
                        //
                        str := dt:ToString( "yyyyMMdd" )
                        encoding:GetBytes( str, 0, buffer:Length, buffer, 0 )
                        isOk := TRUE
                    ELSEIF ( fieldType == DbFieldType.DateTime )
                        LOCAL dat AS LONG
                        LOCAL tim AS LONG
                        LOCAL dt AS DateTime
                        dt := (DateTime)oValue
                        //
                        dat := _dateTimeToJulian( dt )
                        tim := dt:Hour * 3600000 + dt:Minute * 60000 + dt:Second * 1000
                        //
                        LOCAL datBytes := System.BitConverter.GetBytes( (UINT32)dat ) AS BYTE[]
                        LOCAL timBytes := System.BitConverter.GetBytes( (UINT32)tim ) AS BYTE[]
                        //
                        Array.Copy( datBytes, 0, buffer, 0, 4 )
                        Array.Copy( timBytes, 0, buffer, 4, 4 )
                        isOk := TRUE
                    ELSE
                        // Type Error !
                        isOk := FALSE
                    ENDIF
                    
                END SWITCH
            //
            RETURN isOk
            
            // Throw a Error, indicating the SubSystem Code and the General Code
        INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD) AS VOID
            SELF:_DbfError(iSubCode, iGenCode, String.Empty, String.Empty, XSharp.Severity.ES_ERROR)
            
        INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD, iSeverity AS DWORD) AS VOID
            SELF:_DbfError(iSubCode, iGenCode, String.Empty, String.Empty, iSeverity)
            
        INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING) AS VOID
            SELF:_DbfError(iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR)
            
        INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING) AS VOID
            SELF:_DbfError(iSubCode, iGenCode, strFunction,strMessage, XSharp.Severity.ES_ERROR)
            
        INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING, iSeverity AS DWORD) AS VOID
            LOCAL oError AS RddError
            //
            oError := RddError{}
            oError:SubCode := iSubCode
            oError:Gencode := iGenCode
            oError:SubSystem := SELF:SysName
            oError:Severity := iSeverity
            oError:FuncSym  := strFunction
            oError:FileName := SELF:_FileName
            oError:Description := strMessage
            //
            THROW oError
            
            // Calculate the position of the Field in the BYTE[] that holds the Data 
            // Offset is Zero-Based
        INTERNAL METHOD _getFieldOffset( nFldPos AS LONG ) AS LONG
            // 1 To Skip Deleted field
            LOCAL iOffset := 1 AS INT
            LOCAL fld AS DbfRddFieldInfo
            // 
            LOCAL nArrPos := nFldPos AS LONG
            LOCAL nStart := 1 AS LONG
            LOCAL i AS LONG
            IF __ARRAYBASE__ == 0
                nArrPos -= 1
                nStart -= 1
            ENDIF
            fld := SELF:_Fields[ nArrPos ] ASTYPE DbfRddFieldInfo
            IF ( fld:Offset == -1 )
                FOR i := nStart TO (nArrPos-1)
                    iOffset += SELF:_Fields[i]:Length
                NEXT
                fld:Offset := iOffset
            ELSE
                iOffset := fld:Offset
            ENDIF
            RETURN iOffset
            
            // Like FiedName, but on DbFieldType
        INTERNAL VIRTUAL METHOD _FieldType( nFldPos AS LONG ) AS DbFieldType
            LOCAL nArrPos := nFldPos AS LONG
            IF __ARRAYBASE__ == 0
                nArrPos -= 1
            ENDIF
            //
            RETURN SELF:_Fields[ nArrPos ]:FieldType
            
            // Indicate if a Field is a Memo
            // At DBF Level, TRUE only for DbFieldType.Memo
        INTERNAL VIRTUAL METHOD _isMemoField( nFldPos AS LONG ) AS LOGIC
            //
            RETURN SELF:_isMemoFieldType( SELF:_FieldType(nFldPos ) )
            
        INTERNAL VIRTUAL METHOD _isMemoFieldType( fieldType AS DbFieldType ) AS LOGIC
            RETURN ( fieldType == DbFieldType.Memo )
            
            // Retrieve the BlockNumber as it is written in the DBF
        INTERNAL METHOD _getMemoBlockNumber( nFldPos AS LONG ) AS LONG
            LOCAL blockNbr := 0 AS LONG
            //
            IF SELF:_isMemoField( nFldPos )
                //
                IF SELF:_readRecord()
                    //
                    LOCAL nArrPos := nFldPos AS LONG
                    IF __ARRAYBASE__ == 0
                        nArrPos -= 1
                    ENDIF
                    LOCAL iOffset := SELF:_getFieldOffset(nFldPos) AS LONG
                    //
                    LOCAL encoding AS ASCIIEncoding
                    // Read actual Data
                    encoding := ASCIIEncoding{}
                    VAR str :=  encoding:GetString( SELF:_RecordBuffer, iOffset, SELF:_Fields[nArrPos]:Length)
                    IF ( str == NULL )
                        str := String.Empty
                    ENDIF            
                    str := str:Trim()
                    IF String.IsNullOrEmpty(str)
                        blockNbr := 0
                    ELSE
                        blockNbr := System.Convert.ToInt32(str)
                    ENDIF
                    //
                ENDIF
            ENDIF
            RETURN blockNbr
            
            /// <inheritdoc />
        METHOD GetValue(nFldPos AS LONG) AS OBJECT
            LOCAL ret := NULL AS OBJECT
            // Read Record to Buffer
            IF SELF:_readRecord()
                //
                IF SELF:_isMemoField( nFldPos )
                    IF _oMemo != NULL
                        // At this level, the return value is the raw Data, in BYTE[]
                        RETURN _oMemo:GetValue(nFldPos)
                    ELSE                            
                        RETURN SUPER:GetValue(nFldPos)
                    ENDIF
                ELSE
                    //                    IF ( fieldType == DbFieldType.Number )
                    //                        IF (SELF:_Fields[nArrPos]:Decimals == 0 )
                    //                            fieldType := DbFieldType.Integer
                    //                        ENDIF
                    //                    ENDIF
                    LOCAL nArrPos := nFldPos AS LONG
                    IF __ARRAYBASE__ == 0
                        nArrPos -= 1
                    ENDIF
                    LOCAL iOffset := SELF:_getFieldOffset(nFldPos) AS LONG
                    //
                    VAR destArray := BYTE[]{SELF:_Fields[nArrPos]:Length}
                    Array.Copy( SELF:_RecordBuffer, iOffset, destArray, 0, SELF:_Fields[nArrPos]:Length)
                    // We ned the Decimals number to return an Integer or a Float
                    ret := SELF:_convertDataToField( destArray, SELF:_Fields[nArrPos]:FieldType, SELF:_Fields[nArrPos]:Decimals )
                ENDIF
            ELSE
                SELF:_DbfError( ERDD.READ, XSharp.Gencode.EG_READ )
            ENDIF
            RETURN ret
            
            /// <inheritdoc />
        METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
            IF _oMemo != NULL                    
                RETURN _oMemo:GetValueFile(nFldPos, fileName)
            ELSE                            
                RETURN SUPER:GetValueFile(nFldPos, fileName)
            ENDIF
            
            /// <inheritdoc />
        METHOD GetValueLength(nFldPos AS LONG) AS LONG
            IF _oMemo != NULL                    
                RETURN _oMemo:GetValueLength(nFldPos)
            ELSE                            
                RETURN SUPER:GetValueLength(nFldPos)
            ENDIF
            
            /// <inheritdoc />
        METHOD Flush() 			AS LOGIC
            LOCAL isOk AS LOGIC
            //
            IF ( SELF:_ReadOnly )
                // Error !! Cannot be written !
                SELF:_DbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
                RETURN FALSE
            ENDIF
            isOk := SELF:GoCold()
            IF isOk 
                IF ( SELF:_Shared )
                    SELF:HeaderLock( DbLockMode.Lock )
                ENDIF
                SELF:_putEndOfFileMarker()        
                SELF:_writeHeader()
                IF ( SELF:_Shared )
                    SELF:HeaderLock( DbLockMode.UnLock )
                ENDIF                
            ENDIF
            //
            FFlush( SELF:_hFile )
            IF _oMemo != NULL                 
                isOk := _oMemo:Flush()
            ENDIF
            RETURN isOk
            
            // Save any Pending Change            
        METHOD GoCold()			AS LOGIC
            LOCAL ret AS LOGIC
            //
            ret := TRUE
            IF ( SELF:_Hot )
                BEGIN LOCK SELF
                    SELF:_writeRecord()
                    SELF:_NewRecord := FALSE
                END LOCK
            ENDIF
            RETURN ret
            
            // Indicate that the content of the current buffer needs to be saved
        METHOD GoHot()			AS LOGIC   
            LOCAL ret AS LOGIC
            //
            ret := TRUE
            IF ( !SELF:_Hot )
                BEGIN LOCK SELF
                    IF SELF:_Shared .AND. !SELF:_fLocked .AND. !SELF:_Locks:Contains( (LONG)SELF:RecNo )
                        SELF:_DbfError( ERDD.UNLOCKED, XSharp.Gencode.EG_UNLOCKED )
                        ret := FALSE
                    ENDIF
                    IF ( SELF:_ReadOnly )
                        // Error !! Cannot be written !
                        SELF:_DbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
                        ret := FALSE
                    ELSE
                        SELF:_Hot := TRUE
                    ENDIF
                END LOCK 
            ENDIF
            RETURN ret

        PROPERTY IsHot AS LOGIC GET SELF:_Hot
        PROPERTY IsNewRecord AS LOGIC GET SELF:_NewRecord
            
            /// <inheritdoc />
        METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
            LOCAL iOffset := SELF:_getFieldOffset(nFldPos) AS LONG
            LOCAL nArrPos := nFldPos AS LONG
            IF __ARRAYBASE__ == 0
                nArrPos -= 1
            ENDIF
            // Ok, so the Data position in the RecordBuffer is iOffset, 
            // its Length is SELF:_Fields[nArrPos]:Length
            IF SELF:_isMemoField( nFldPos )
                IF _oMemo != NULL           
                    IF _oMemo:PutValue(nFldPos, oValue)
                        // Update the Field Info with the new MemoBlock Position
                        // Create a Destination buffer for the conversion
                        VAR destArray := BYTE[]{10}
                        SELF:_convertFieldToData( SELF:_oMemo:LastWrittenBlockNumber, destArray, DbFieldType.Integer, 0 )
                        // Put back into RecordBuffer
                        Array.Copy( destArray, 0, SELF:_RecordBuffer, iOffset, 10 )
                        //
                        SELF:GoHot()
                    ENDIF
                ELSE                            
                    RETURN SUPER:PutValue(nFldPos, oValue)
                ENDIF
            ELSE
                // Create a Destination buffer for the conversion
                VAR destArray := BYTE[]{SELF:_Fields[nArrPos]:Length}
                SELF:_convertFieldToData( oValue, destArray, SELF:_Fields[nArrPos]:FieldType, SELF:_Fields[nArrPos]:Decimals )
                // Put back into RecordBuffer
                Array.Copy( destArray, 0, SELF:_RecordBuffer, iOffset, SELF:_Fields[nArrPos]:Length)
                //
                SELF:GoHot()
            ENDIF
            RETURN TRUE
            
            /// <inheritdoc />
        METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
            IF _oMemo != NULL   
                RETURN _oMemo:PutValueFile(nFldPos, fileName)
            ELSE                            
                RETURN SUPER:PutValue(nFldPos, fileName)
            ENDIF
            
            
            // Locking
            //	METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC  
            //	METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC  
            //	METHOD Lock(uiMode AS DbLockMode) AS LOGIC 
            //	METHOD UnLock(oRecId AS OBJECT) AS LOGIC
            
            // Memo File Access 
            /// <inheritdoc />
        METHOD CloseMemFile() 	AS LOGIC    
            IF _oMemo != NULL                    
                RETURN _oMemo:CloseMemFile()
            ELSE                            
                RETURN SUPER:CloseMemFile()
            ENDIF
            /// <inheritdoc />
        METHOD CreateMemFile(info AS DbOpenInfo) 	AS LOGIC
            IF _oMemo != NULL                    
                RETURN _oMemo:CreateMemFile(info)
            ELSE                            
                RETURN SUPER:CreateMemFile(info)
            ENDIF
            
            /// <inheritdoc />
        METHOD OpenMemFile(info AS DbOpenInfo) 	AS LOGIC   
            IF _oMemo != NULL                    
                RETURN _oMemo:OpenMemFile(info)
            ELSE                            
                RETURN SUPER:OpenMemFile(info)
            ENDIF
            
            // Indexes
            /// <inheritdoc />
        METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            IF _oIndex != NULL
                RETURN _oIndex:OrderCondition(info)
            ELSE
                RETURN SUPER:OrderCondition(info)
            ENDIF
            
            /// <inheritdoc />
        METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC	
            IF _oIndex != NULL
                RETURN _oIndex:OrderCreate(info)
            ELSE
                RETURN SUPER:OrderCreate(info)
            ENDIF
            
            /// <inheritdoc />
        METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC    	
            IF _oIndex != NULL
                RETURN _oIndex:OrderDestroy(info)
            ELSE
                RETURN SUPER:OrderDestroy(info)
            ENDIF
            
            /// <inheritdoc />
        METHOD OrderInfo(nOrdinal AS LONG) AS OBJECT
            IF _oIndex != NULL
                RETURN _oIndex:OrderInfo(nOrdinal)
            ELSE
                RETURN SUPER:OrderInfo(nOrdinal)
            ENDIF
            
            /// <inheritdoc />
        METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
            IF _oIndex != NULL
                RETURN _oIndex:OrderListAdd(info)
            ELSE
                RETURN SUPER:OrderListAdd(info)
            ENDIF
            
            /// <inheritdoc />
        METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
            IF _oIndex != NULL
                RETURN _oIndex:OrderListDelete(info)
            ELSE
                RETURN SUPER:OrderListDelete(info)
            ENDIF
            /// <inheritdoc />
        METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
            IF _oIndex != NULL
                RETURN _oIndex:OrderListFocus(info)
            ELSE
                RETURN SUPER:OrderListFocus(info)
            ENDIF
            /// <inheritdoc />
        METHOD OrderListRebuild() AS LOGIC 
            IF _oIndex != NULL
                RETURN _oIndex:OrderListRebuild()
            ELSE
                RETURN SUPER:OrderListRebuild()
            ENDIF
            /// <inheritdoc />
        METHOD Seek(info AS DbSeekInfo) AS LOGIC
            IF _oIndex != NULL
                RETURN _oIndex:Seek(info)
            ELSE
                RETURN SUPER:Seek(info)
            ENDIF
            
            // Relations
            //	METHOD ChildEnd(info AS DbRelInfo) AS LOGIC
            //	METHOD ChildStart(info AS DbRelInfo) AS LOGIC
            //	METHOD ChildSync(info AS DbRelInfo) AS LOGIC
            //	METHOD ClearRel() AS LOGIC
            //	METHOD ForceRel() AS LOGIC  
            //	METHOD RelArea(nRelNum AS LONG) AS LONG 
            //	METHOD RelEval(info AS DbRelInfo) AS LOGIC
            //	METHOD RelText(nRelNum AS LONG) AS STRING
            //	METHOD SetRel(info AS DbRelInfo) AS LOGIC  
            //	METHOD SyncChildren() AS LOGIC
            
            // Trans	
            //    METHOD Trans(info AS DbTransInfo) 		AS LOGIC
            //    METHOD TransRec(info AS DbTransInfo) 	AS LOGIC
            
            // Blob
            //	METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT
            
            // CodeBlock Support
            
        PRIVATE METHOD _macroCompile(cMacro AS STRING ) AS ICodeblock
            VAR oMC := XSharp.RuntimeState.MacroCompiler
            IF oMC != NULL_OBJECT
                VAR oMod := XSharp.RuntimeState.AppModule
                IF oMod == NULL_OBJECT
                    XSharp.RuntimeState.AppModule := TYPEOF(XSharp.Core.Functions):Module
                ENDIF
                LOCAL iResult AS ICodeBlock
                LOCAL oResult AS XSharp.CodeBlock
                LOCAL lIsCodeblock AS LOGIC
                iResult := oMC:Compile(cMacro, TRUE, oMod, OUT lIsCodeBlock)
                oResult := XSharp._CodeBlock{iResult, cMacro, lIsCodeBlock}
                RETURN oResult
            ENDIF
            RETURN NULL_OBJECT	
            
        VIRTUAL METHOD Compile(sBlock AS STRING) AS LOGIC
            LOCAL isOk AS LOGIC
            //
            isOk := FALSE
            IF (!string.IsNullOrEmpty(sBlock))
                IF ( !sBlock:TrimStart():StartsWith("{") )
                    sBlock := "{||" + sBlock + "}"
                ENDIF
                TRY
                    IF (XSharp.RuntimeState.MacroCompiler == NULL)
                        SELF:_LastCodeBlock := NULL
                        SELF:_dbfError( ERDD.CORRUPT, GenCode.EG_CORRUPTION, "DBF.Compile")
                        RETURN isOk
                    ENDIF
                    SELF:_LastCodeBlock :=SELF:_macroCompile(sBlock)
                    isOk := TRUE
                    RETURN isOk
                    
                CATCH ex AS Exception
                    SELF:_LastCodeBlock := NULL
                    SELF:_dbfError( SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX, ex:Message)
                    RETURN isOk
                END TRY
            ENDIF
            SELF:_dbfError( SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX,"DBF.Compile")
            RETURN isOk
            
            //	METHOD EvalBlock(oBlock AS OBJECT) AS OBJECT	
        VIRTUAL METHOD EvalBlock( cbBlock AS ICodeblock ) AS OBJECT
            // Save Current WorkArea
            NOP
            // Switch to Current DBF/WorkArea ?
            NOP
            // 
            SELF:_EvalResult := NULL
            TRY
                SELF:_EvalResult := cbBlock:EvalBlock()
            CATCH ex AS Exception
                SELF:_dbfError(SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX, "DBF.EvalBlock", ex:Message)
                RETURN NULL
            FINALLY
                // Restore previous WorkArea
                NOP
            END TRY
            RETURN SELF:_EvalResult
            
            // Other
            /// <inheritdoc />
        VIRTUAL METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
            LOCAL oResult AS OBJECT
            oResult := NULL
            SWITCH nOrdinal
                CASE DbInfo.DBI_ISDBF
                CASE DbInfo.DBI_CANPUTREC
                    oResult := TRUE		
                CASE DbInfo.DBI_LASTUPDATE
                    oResult := SELF:_Header:LastUpdate
                CASE DbInfo.DBI_GETHEADERSIZE
                    oResult := SELF:_Header:HeaderLen 
                    // DbInfo.GETLOCKARRAY
                    // DbInfo.TABLEEXT
                    // DbInfo.FULLPATH
                    // DbInfo.MEMOTYPE 
                    // DbInfo.TABLETYPE
                    //                CASE DbInfo.FILEHANDLE
                    //                    oResult := SELF:_hFile
                    //                CASE DbInfo.MEMOHANDLE
                    //                    IF ( SELF:_oMemo != NULL )
                    //                        oResult := SELF:_oMemo:_hFile
                    //                    ENDIF
                    // DbInfo.TRANSREC
                    //                CASE DbInfo.SHARED
                    //                    oResult := SELF:_Shared
                    //                CASE DbInfo.ISFLOCK
                    //                    oResult := SELF:_fLocked
                CASE DbInfo.DBI_VALIDBUFFER 
                    oResult := SELF:_BufferValid
                    // DbInfo.POSITIONED 
                    // DbInfo.ISENCRYPTED
                    // DbInfo.DECRYPT
                    // DbInfo.ENCRYPT
                    // DbInfo.LOCKCOUNT 
                    // DbInfo.LOCKOFFSET
                    // DbInfo.LOCKTEST
                    // DbInfo.LOCKSCHEME
                    // DbInfo.ROLLBACK
                    // DbInfo.PASSWORD
                    // DbInfo.TRIGGER
                    // DbInfo.OPENINFO
                    // DbInfo.DIRTYREAD
                    // DbInfo.DB_VERSION
                    // DbInfo.RDD_VERSION
                    // DbInfo.CODEPAGE
                    // DbInfo.DOS_CODEPAGE
                    
                OTHERWISE
                    oResult := SUPER:Info(nOrdinal, oNewValue)
            END SWITCH
            RETURN oResult  
            
            
            
            
            /// <inheritdoc />
        VIRTUAL METHOD RecInfo(oRecID AS OBJECT, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT  
            LOCAL nCurrent := 0 AS LONG
            LOCAL isOk AS LOGIC
            //
            isOk := TRUE
            IF ( oRecID == NULL )
                nCurrent := SELF:RecNo
            ELSE
                TRY
                    nCurrent := Convert.ToInt32( oRecID )
                CATCH
                    isOk := FALSE
                END TRY
            ENDIF
            IF isOk
                //
                SWITCH nOrdinal
                    CASE DBRI_DELETED
                    CASE DBRI_ENCRYPTED
                    CASE DBRI_RAWRECORD
                    CASE DBRI_RAWMEMOS
                    CASE DBRI_RAWDATA
                        // Move
                        SELF:GoTo( nCurrent )
                        // and get Data
                        SELF:_readRecord()
                END SWITCH
                //
                SWITCH nOrdinal
                    CASE DBRI_DELETED
                        oNewValue := SELF:Deleted
                    CASE DBRI_RAWDATA
                        oNewValue := SELF:GetRec()
                    CASE DBRI_LOCKED
                        IF ( SELF:_Shared )
                            oNewValue := SELF:_Locks:Contains( nCurrent )
                        ELSE
                            oNewValue := TRUE
                        ENDIF
                    CASE DBRI_RECNO
                        oNewValue := SELF:RecNo
                    CASE DBRI_RECSIZE 
                        oNewValue := SELF:_RecordLength
                    CASE DBRI_RAWRECORD
                CASE DBRI_RAWMEMOS
                    CASE DBRI_ENCRYPTED
                    OTHERWISE
                        oNewValue := SUPER:Info(nOrdinal, oNewValue)
                    END SWITCH 
            ENDIF
            //
            RETURN isOk
            
            //	METHOD Sort(info AS DbSortInfo) AS LOGIC
            
            // Properties
            //	PROPERTY Alias 		AS STRING GET
            /// <inheritdoc />
        PROPERTY BoF 		AS LOGIC GET SELF:_Bof
        
        /// <inheritdoc />
        PROPERTY Deleted 	AS LOGIC 
            GET
                // Update ?
                SELF:_readRecord()
                RETURN SELF:_Deleted
            END GET
        END PROPERTY
        
        /// <inheritdoc />
        PROPERTY EoF 		AS LOGIC GET SELF:_Eof
        
        //PROPERTY Exclusive	AS LOGIC GET 
        
        /// <inheritdoc />
        PROPERTY FieldCount AS LONG 
            GET 
                LOCAL ret := 0 AS LONG
                IF ( SELF:_Fields != NULL )
                    ret := SELF:_Fields:Length
                ENDIF
                RETURN ret
            END GET
        END PROPERTY
        
        //	PROPERTY FilterText	AS STRING GET 
        PROPERTY Found		AS LOGIC GET SELF:_Found
        
        /// <inheritdoc />
        PROPERTY RecCount	AS LONG 
            GET
                IF ( SELF:_Shared )
                    SELF:_RecCount := SELF:_calculateRecCount()
                ENDIF
                RETURN SELF:_RecCount
            END GET
        END PROPERTY
        
        PRIVATE METHOD _calculateRecCount()	AS LONG 
            LOCAL reccount AS LONG
            //
            reccount := 0
            IF ( SELF:_hFile != F_ERROR )
                VAR current := FTell( SELF:_hFile )
                VAR fSize := FSeek3( SELF:_hFile, 0, FS_END )
                FSeek3( SELF:_hFile, (LONG)current, FS_SET )
                IF ( fSize != 0 ) // Just create file ?
                    reccount := ( fSize - SELF:_Header:HeaderLen ) / SELF:_Header:RecordLen
                ENDIF
            ENDIF
            RETURN reccount
            
            /// <inheritdoc />
        PROPERTY RecNo		AS INT GET SELF:_RecNo
        
        //	PROPERTY Shared		AS LOGIC GET
        /// <inheritdoc />
        VIRTUAL PROPERTY SysName AS STRING GET TYPEOF(Dbf):ToString()
        
        //	
        // Error Handling
        //	PROPERTY LastGenCode	AS LONG GET
        //	PROPERTY LastSubCode	AS LONG GET
        //	PROPERTY LastError		AS Exception GET
        
        /// <summary>Offsets in the header of a DBF.</summary>
        PUBLIC ENUM HDROFFSETS
            MEMBER SIG			:= 0
            MEMBER YEAR			:= 1
            MEMBER MONTH	    := 2
            MEMBER DAY          := 3
            MEMBER RECCOUNT     := 4
            MEMBER DATAOFFSET   := 8
            MEMBER RECSIZE      := 10
            MEMBER RESERVED1    := 12
            MEMBER TRANSACTION  := 14
            MEMBER ENCRYPTED    := 15
            MEMBER DBASELAN     := 16
            MEMBER MULTIUSER    := 20
            MEMBER RESERVED2   := 24
            MEMBER HASTAGS	    := 28
            MEMBER CODEPAGE     := 29
            MEMBER RESERVED3    := 30
            MEMBER SIZE         := 32
            
        END ENUM
        /// <summary>Offsets in the Field structure</summary>        
        PUBLIC ENUM FLDOFFSETS
            MEMBER NAME			:= 0
            MEMBER NAME_SIZE    := 11
            MEMBER TYPE			:= 11
            MEMBER OFFSET	    := 12
            MEMBER LEN          := 16
            MEMBER DEC          := 17
            MEMBER FLAGS        := 18
            MEMBER COUNTER      := 19
            MEMBER INCSTEP      := 23
            MEMBER RESERVED1    := 24
            MEMBER RESERVED2    := 25
            MEMBER RESERVED3    := 26
            MEMBER RESERVED4    := 27
            MEMBER RESERVED5    := 28
            MEMBER RESERVED6	:= 29
            MEMBER RESERVED7    := 30
            MEMBER HASTAG       := 31
            MEMBER SIZE         := 32
        END ENUM
        
        /// <summary>DBF Header.</summary>        
        STRUCTURE DbfHeader                     
            // Fixed Buffer of 32 bytes
            // Matches the DBF layout  
            // Read/Write to/from the Stream with the Buffer 
            // and access individual values using the other fields
            PUBLIC Buffer   AS BYTE[]
            // Hot ?  => Header has changed ?
            PUBLIC isHot	AS LOGIC
            
            PROPERTY Version    AS DBFVersion	;
            GET (DBFVersion) Buffer[HDROFFSETS.SIG] ;
            SET Buffer[HDROFFSETS.SIG] := (BYTE) VALUE
            
                PROPERTY Year		AS BYTE			;
                GET Buffer[HDROFFSETS.YEAR]	;
                SET Buffer[HDROFFSETS.YEAR] := VALUE, isHot := TRUE
                
                    PROPERTY Month		AS BYTE			;
                    GET Buffer[HDROFFSETS.MONTH]	;
                    SET Buffer[HDROFFSETS.MONTH] := VALUE, isHot := TRUE
                    
                    PROPERTY Day		AS BYTE			;
                    GET Buffer[HDROFFSETS.DAY]	;
                    SET Buffer[HDROFFSETS.DAY] := VALUE, isHot := TRUE
                    
                    PROPERTY RecCount	AS LONG			;
                    GET BitConverter.ToInt32(Buffer, HDROFFSETS.RECCOUNT) ;
                    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.RECCOUNT, SIZEOF(LONG)), isHot := TRUE
                    
                    PROPERTY HeaderLen	AS SHORT		;
                    GET BitConverter.ToInt16(Buffer, HDROFFSETS.DATAOFFSET);
                    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.DATAOFFSET, SIZEOF(SHORT)), isHot := TRUE
                    
                    // Length of one data record, including deleted flag
                    PROPERTY RecordLen	AS SHORT		;
                    GET BitConverter.ToInt16(Buffer, HDROFFSETS.RECSIZE);
                    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.RECSIZE, SIZEOF(SHORT)), isHot := TRUE
                    
                    PROPERTY Reserved1	AS SHORT		;
                    GET BitConverter.ToInt16(Buffer, HDROFFSETS.RESERVED1);
                    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.RESERVED1, SIZEOF(SHORT)), isHot := TRUE
                    
                    PROPERTY Transaction AS BYTE		;
                    GET Buffer[HDROFFSETS.TRANSACTION];
                    SET Buffer[HDROFFSETS.TRANSACTION] := VALUE, isHot := TRUE
                    
                    PROPERTY Encrypted	AS BYTE			;
                    GET Buffer[HDROFFSETS.ENCRYPTED];
                    SET Buffer[HDROFFSETS.ENCRYPTED] := VALUE, isHot := TRUE
                    
                    PROPERTY DbaseLan	AS LONG			;
                    GET BitConverter.ToInt32(Buffer, HDROFFSETS.DBASELAN) ;
                    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.DBASELAN, SIZEOF(LONG)), isHot := TRUE
                    
                    PROPERTY MultiUser	AS LONG			;
                    GET BitConverter.ToInt32(Buffer, HDROFFSETS.MULTIUSER)	;
                    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.MULTIUSER, SIZEOF(LONG)), isHot := TRUE
                    
                    PROPERTY Reserved2	AS LONG			;
                    GET BitConverter.ToInt32(Buffer, HDROFFSETS.RESERVED2);
                    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.RESERVED2, SIZEOF(LONG))
                    
                    PROPERTY HasTags	AS DBFTableFlags ;
                    GET (DBFTableFlags)Buffer[HDROFFSETS.HASTAGS] ;
                    SET Buffer[HDROFFSETS.HASTAGS] := (BYTE) VALUE, isHot := TRUE
                    
                    PROPERTY CodePage	AS BYTE			 ;
                    GET Buffer[HDROFFSETS.CODEPAGE]  ;
                    SET Buffer[HDROFFSETS.CODEPAGE] := (BYTE) VALUE, isHot := TRUE
                    
                    PROPERTY Reserved3	AS SHORT         ;
                    GET BitConverter.ToInt16(Buffer, HDROFFSETS.RESERVED3);
                    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.RESERVED3, SIZEOF(SHORT)), isHot := TRUE
                    
                    PROPERTY LastUpdate AS DateTime      ;
                    GET DateTime{1900+Year, Month, Day} ;
                    SET Year := (BYTE) VALUE:Year % 100, Month := (BYTE) VALUE:Month, Day := (BYTE) VALUE:Day, isHot := TRUE
                    
                    METHOD initialize() AS VOID STRICT
                        Buffer := BYTE[]{HDROFFSETS.SIZE}
                        isHot  := FALSE
                        RETURN
                        // Dbase (7?) Extends this with
                        // [FieldOffSet(31)] PUBLIC LanguageDriverName[32]	 as BYTE
                        // [FieldOffSet(63)] PUBLIC Reserved6 AS LONG    
                        /*
                        0x02   FoxBASE
                        0x03   FoxBASE+/Dbase III plus, no memo
                        0x04   dBase 4
                        0x05   dBase 5
                        0x07   VO/Vulcan Ansi encoding
                        0x13   FLagship dbv
                        0x23   Flagship 2/4/8
                        0x30   Visual FoxPro
                        0x31   Visual FoxPro, autoincrement enabled
                        0x33   Flagship 2/4/8 + dbv
                        0x43   dBASE IV SQL table files, no memo
                        0x63   dBASE IV SQL system files, no memo 
                        0x7B   dBASE IV, with memo
                        0x83   FoxBASE+/dBASE III PLUS, with memo
                        0x87   VO/Vulcan Ansi encoding with memo
                        0x8B   dBASE IV with memo
                        0xCB   dBASE IV SQL table files, with memo 
                        0xE5   Clipper SIX driver, with SMT memo
                        0xF5   FoxPro 2.x (or earlier) with memo
                        0xFB   FoxBASE
                        
                        FoxPro additional Table structure:
                        28 	Table flags:
                        0x01   file has a structural .cdx
                        0x02   file has a Memo field
                        0x04   file is a database (.dbc)
                        This byte can contain the sum of any of the above values. 
                        For example, the value 0x03 indicates the table has a structural .cdx and a 
                        Memo field.
                        29 	Code page mark
                        30 – 31 	Reserved, contains 0x00
                        32 – n 	Field subrecords
                        The number of fields determines the number of field subrecords. 
                        One field subrecord exists for each field in the table.
                        n+1 			Header record terminator (0x0D)
                        n+2 to n+264 	A 263-byte range that contains the backlink, which is the 
                        relative path of an associated database (.dbc) file, information. 
                        If the first byte is 0x00, the file is not associated with a database. 
                        Therefore, database files always contain 0x00.	
                        see also ftp://fship.com/pub/multisoft/flagship/docu/dbfspecs.txt
                        
                        */
                    END STRUCTURE
                /// <summary>DBF Field.</summary>                            
                STRUCTURE DbfField   
                    // Fixed Buffer of 32 bytes
                    // Matches the DBF layout
                    // Read/Write to/from the Stream with the Buffer 
                    // and access individual values using the other fields
                    METHOD initialize() AS VOID
                        SELF:Buffer := BYTE[]{FLDOFFSETS.SIZE}
                        
                    PUBLIC Buffer		 AS BYTE[]	  
                    
                    PROPERTY Name		 AS STRING
                    GET 
                        LOCAL fieldName := BYTE[]{FLDOFFSETS.NAME_SIZE} AS BYTE[]
                        Array.Copy( Buffer, FLDOFFSETS.NAME, fieldName, 0, FLDOFFSETS.NAME_SIZE )
                        LOCAL count := Array.FindIndex<BYTE>( fieldName, 0, { sz => sz == 0 } ) AS INT
                        IF count == -1
                            count := FLDOFFSETS.NAME_SIZE
                        ENDIF
                        LOCAL str := System.Text.Encoding.ASCII:GetString( fieldName,0, count ) AS STRING
                        IF ( str == NULL )
                            str := String.Empty
                        ENDIF
                        str := str:Trim()
                        RETURN str
                    END GET
                    SET
                        // Be sure to fill the Buffer with 0
                        Array.Clear( Buffer, FLDOFFSETS.NAME, FLDOFFSETS.NAME_SIZE )
                        System.Text.Encoding.ASCII:GetBytes( VALUE, 0, Math.Min(FLDOFFSETS.NAME_SIZE,VALUE:Length), Buffer, FLDOFFSETS.NAME )
                    END SET
                END PROPERTY
                
                PROPERTY Type		 AS DBFieldType ;
                GET (DBFieldType) Buffer[ FLDOFFSETS.TYPE ] ;
                SET Buffer[ FLDOFFSETS.TYPE ] := (BYTE) VALUE
                
                // Offset from record begin in FP
                PROPERTY Offset 	 AS LONG ;
                GET BitConverter.ToInt32(Buffer, FLDOFFSETS.OFFSET);
                SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, FLDOFFSETS.OFFSET, SIZEOF(LONG))
                
                PROPERTY Len		 AS BYTE;
                GET Buffer[FLDOFFSETS.Len]  ;
                SET Buffer[FLDOFFSETS.Len] := (BYTE) VALUE
                
                PROPERTY Dec		 AS BYTE;
                GET Buffer[FLDOFFSETS.Dec]  ;
                SET Buffer[FLDOFFSETS.Dec] := (BYTE) VALUE
                
                PROPERTY Flags		 AS DBFFieldFlags;
                GET (DBFFieldFlags)Buffer[FLDOFFSETS.Flags] ;
                SET Buffer[FLDOFFSETS.Flags] := (BYTE) VALUE
                
                PROPERTY Counter	 AS LONG;
                GET BitConverter.ToInt32(Buffer, FLDOFFSETS.Counter);
                SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, FLDOFFSETS.Counter, SIZEOF(LONG))
                
                PROPERTY IncStep	 AS BYTE;
                GET Buffer[FLDOFFSETS.IncStep]  ;
                SET Buffer[FLDOFFSETS.IncStep] := (BYTE) VALUE
                
                PROPERTY Reserved1   AS BYTE;
                GET Buffer[FLDOFFSETS.Reserved1]  ;
                SET Buffer[FLDOFFSETS.Reserved1] := (BYTE) VALUE
                
                PROPERTY Reserved2   AS BYTE;
                GET Buffer[FLDOFFSETS.Reserved2]  ;
                SET Buffer[FLDOFFSETS.Reserved2] := (BYTE) VALUE
                
                PROPERTY Reserved3   AS BYTE;
                GET Buffer[FLDOFFSETS.Reserved3]  ;
                SET Buffer[FLDOFFSETS.Reserved3] := (BYTE) VALUE
                
                PROPERTY Reserved4  AS BYTE;
                GET Buffer[FLDOFFSETS.Reserved4]  ;
                SET Buffer[FLDOFFSETS.Reserved4] := (BYTE) VALUE
                
                PROPERTY Reserved5   AS BYTE;
                GET Buffer[FLDOFFSETS.Reserved5]  ;
                SET Buffer[FLDOFFSETS.Reserved5] := (BYTE) VALUE
                
                PROPERTY Reserved6   AS BYTE;
                GET Buffer[FLDOFFSETS.Reserved6]  ;
                SET Buffer[FLDOFFSETS.Reserved6] := (BYTE) VALUE
                
                PROPERTY Reserved7   AS BYTE;
                GET Buffer[FLDOFFSETS.Reserved7]  ;
                SET Buffer[FLDOFFSETS.Reserved7] := (BYTE) VALUE
                
                PROPERTY HasTag		 AS BYTE;
                GET Buffer[FLDOFFSETS.HasTag]  ;
                SET Buffer[FLDOFFSETS.HasTag] := (BYTE) VALUE
            END STRUCTURE
            
            
            /// <summary>DBase 7 Field.</summary>                            
            [StructLayout(LayoutKind.Explicit)];
            STRUCTURE Dbf7Field   
                // Dbase 7 has 32 Bytes for Field Names
                // Fixed Buffer of 32 bytes
                // Matches the DBF layout
                // Read/Write to/from the Stream with the Buffer 
                // and access individual values using the other fields
                [FieldOffSet(00)] PUBLIC Buffer		 AS BYTE[]	
                [FieldOffSet(00)] PUBLIC Name		 AS BYTE[]    // Field name in ASCII (zero-filled).	  
                [FieldOffSet(32)] PUBLIC Type		 AS BYTE 	// Field type in ASCII (B, C, D, N, L, M, @, I, +, F, 0 or G).
                [FieldOffSet(33)] PUBLIC Len		 AS BYTE 	// Field length in binary.
                [FieldOffSet(34)] PUBLIC Dec		 AS BYTE
                [FieldOffSet(35)] PUBLIC Reserved1	 AS SHORT
                [FieldOffSet(37)] PUBLIC HasTag		 AS BYTE    // Production .MDX field flag; 0x01 if field has an index tag in the production .MDX file; 0x00 if the field is not indexed.
                [FieldOffSet(38)] PUBLIC Reserved2	 AS SHORT
                [FieldOffSet(40)] PUBLIC Counter	 AS LONG	// Next Autoincrement value, if the Field type is Autoincrement, 0x00 otherwise.
                [FieldOffSet(44)] PUBLIC Reserved3	 AS LONG	
                
            END STRUCTURE
            
            ENUM DBFVersion AS BYTE
                MEMBER FoxBase:=2
                MEMBER FoxBaseDBase3NoMemo:=3
                MEMBER dBase4 :=4
                MEMBER dBase5 :=5
                MEMBER VO :=7
                MEMBER Flagship := 0x13
                MEMBER Flagship248 := 0x23
                MEMBER VisualFoxPro:=0x30
                MEMBER VisualFoxProWithAutoIncrement:=0x31
                MEMBER Flagship248WithDBV := 0x33
                MEMBER dBase4SQLTableNoMemo:=0x43
                MEMBER dBase4SQLSystemNoMemo:=0x63
                MEMBER dBase4WithMemo_:=0x7b
                MEMBER FoxBaseDBase3WithMemo:=0x83
                MEMBER VOWithMemo := 0x87
                MEMBER dBase4WithMemo:=0x8b
                MEMBER dBase4SQLTableWithMemo:=0xcb
                MEMBER ClipperSixWithSMT:=0xe5
                MEMBER FoxPro2WithMemo:=0xf5
                MEMBER FoxBASE_:=0xfb
                
                MEMBER Unknown:=0
            END ENUM
            
            /// <summary>DBF Table flags.</summary>                            
            [Flags];
            ENUM DBFTableFlags AS BYTE
                MEMBER HasMemoField:=2
                MEMBER HasStructuralCDX:=1
                MEMBER IsDBC:=4
                MEMBER None:=0
            END ENUM
            /// <summary>DBF Field flags.</summary>                            
            [Flags];
            ENUM DBFFieldFlags AS BYTE
                MEMBER None:=0
                MEMBER System:=1
                MEMBER AllowNullValues:=2
                MEMBER Binary:=4
                MEMBER AutoIncrementing:=12
            END ENUM
            
            ENUM DbfLockingModel
                MEMBER Clipper52    // Clipper 5.2 locking scheme
                MEMBER Clipper53    // Clipper 5.3 locking scheme
                MEMBER FoxPro       // Visual FoxPro locking scheme
                MEMBER FoxProExt    // Visual FoxPro locking scheme
                MEMBER Clipper53Ext // Clipper 5.3 with Files up to 4GB
                MEMBER Harbour64    // Locking scheme for files > 4GB
            END ENUM
            
            // Inpired by Harbour
            STRUCTURE DbfLocking
                // Offset of the Locking
                PUBLIC Offset AS INT64
                // Length for File
                PUBLIC FileSize AS UINT64
                // Length for Record
                PUBLIC RecordSize AS UINT64
                // 
                PUBLIC Direction AS LONG
                
                METHOD Initialize( model AS DbfLockingModel ) AS VOID
                    SWITCH model
                        CASE DbfLockingModel.Clipper52
                            SELF:Offset := 1000000000U
                            SELF:FileSize := 1000000000U
                            SELF:RecordSize := 1U
                            SELF:Direction := 1
                        CASE DbfLockingModel.Clipper53
                            SELF:Offset := 1000000000U
                            SELF:FileSize := 1000000000U
                            SELF:RecordSize := 1U
                            SELF:Direction := 1
                        CASE DbfLockingModel.Clipper53Ext
                            SELF:Offset := 4000000000U
                            SELF:FileSize := 294967295U
                            SELF:RecordSize := 1U
                            SELF:Direction := 1                    
                        CASE DbfLockingModel.FoxPro
                            SELF:Offset := 0x40000000U
                            SELF:FileSize := 0x07ffffffU
                            SELF:RecordSize := 1U
                            SELF:Direction := 2
                        CASE DbfLockingModel.FoxProExt
                            SELF:Offset := 0x7ffffffeU
                            SELF:FileSize := 0x3ffffffdU
                            SELF:RecordSize := 1U
                            SELF:Direction := -1
                        CASE DbfLockingModel.Harbour64
                            SELF:Offset := 0x7FFFFFFF00000001U
                            SELF:FileSize := 0x7ffffffeU
                            SELF:RecordSize := 1U
                            SELF:Direction := 1                        
                    END SWITCH
                    
                    END STRUCTURE
                    
            END CLASS
            
            
        CLASS DbfRddFieldInfo INHERIT RddFieldInfo
            PROTECTED iOffset AS LONG
            
            CONSTRUCTOR(sName AS STRING, sType AS STRING, nLength AS LONG, nDecimals AS LONG)
                SUPER( sName, sType, nLength, nDecimals )
                SELF:iOffset := -1
                
            CONSTRUCTOR(sName AS STRING, nType AS DbFieldType, nLength AS LONG, nDecimals AS LONG)
                SUPER( sName, nType, nLength, nDecimals )
                SELF:iOffset := -1
                
            VIRTUAL PROPERTY Offset AS LONG
            GET
                RETURN SELF:iOffset
            END GET
            
            // Should be INTERNAL
            SET
                SELF:iOffset := VALUE
            END SET
        END PROPERTY
    END CLASS
END NAMESPACE
