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
USING XSharp.RDD.Support
USING System.Globalization
USING System.Collections.Generic


BEGIN NAMESPACE XSharp.RDD
	/// <summary>DBF RDD. Usually not used 'stand alone'</summary>
	PARTIAL CLASS DBF INHERIT Workarea IMPLEMENTS IRddSortWriter
		PROTECT _RelInfoPending  AS DbRelInfo

		PROTECT _Header			AS DbfHeader
		PROTECT _HeaderLength	AS LONG  	// Size of header
		PROTECT _BufferValid	AS LOGIC	// Current Record is Valid
        PROTECT _BlankBuffer    AS BYTE[]
		INTERNAL _isValid        AS LOGIC    // Current Position is Valid
		PROTECT _HasMemo		AS LOGIC
		//PROTECT _HasTags		AS LOGIC
		//PROTECT _HasAutoInc		AS LOGIC
		//PROTECT _HasTimeStamp	AS LOGIC
		//PROTECT _LastUpdate	    AS DateTime
		PROTECT _RecCount		AS LONG
		PROTECT _RecNo			AS LONG
		//PROTECT _Temporary		AS LOGIC
		//PROTECT _NullCount		AS LONG
		//PROTECT _NullOffSet		AS LONG
		PROTECT _RecordChanged	AS LOGIC 	// Current record has changed ?
		PROTECT _Positioned		AS LOGIC 	//
		//PROTECT _Appended		AS LOGIC	// Record has been added ?
		PROTECT _Deleted		AS LOGIC	// Record has been deleted ?
		//PROTECT _HeaderDirty	AS LOGIC	// Header is dirty ?
		PROTECT _fLocked		AS LOGIC    // File Locked ?
		PROTECT _HeaderLocked	AS LOGIC
		//PROTECT _PackMemo		AS LOGIC
		INTERNAL _OpenInfo		AS DbOpenInfo // current dbOpenInfo structure in OPEN/CREATE method
		//PROTECT _ParentRelInfo	AS DbRelInfo  // parent rel struct
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
		//PROTECT _addFieldPos    AS LONG     // Used by AddFields Method, and SetFieldsExtent
		PROTECT _lockScheme     AS DbfLocking
		PROTECT _NewRecord      AS LOGIC

        STATIC PROTECT _Extension := ".DBF" AS STRING
        INTERNAL PROPERTY FullPath AS STRING GET _FileName
		INTERNAL _Ansi          AS LOGIC
		//
		INTERNAL _Encoding      AS Encoding
		//
        STATIC PRIVATE  culture := System.Globalization.CultureInfo.InvariantCulture AS CultureInfo
        PRIVATE  _numformat AS NumberFormatInfo

        PRIVATE METHOD _AllocateBuffers() AS VOID
            SELF:_RecordBuffer  := BYTE[]{ SELF:_RecordLength}
            SELF:_BlankBuffer   := BYTE[]{ SELF:_RecordLength}
            FOR VAR  i := __ARRAYBASE__ TO SELF:_RecordLength - (1 -__ARRAYBASE__)
                SELF:_BlankBuffer[i] := 0x20 // space
            NEXT

		
		CONSTRUCTOR()
			SELF:_hFile := F_ERROR
			SELF:_Header := DbfHeader{} // DbfHeader is a Structure, so the object is already created, no ?
			SELF:_Header:initialize()
			SELF:_Locks     := List<LONG>{}
            SELF:_numformat := (NumberFormatInfo) culture:NumberFormat:Clone()
            SELF:_numformat:NumberDecimalSeparator := "."
    		SELF:_RelInfoPending    := NULL
			
			/// <inheritdoc />
		METHOD GoTop() AS LOGIC
			IF ( SELF:_hFile != F_ERROR )
				BEGIN LOCK SELF
					SELF:GoTo( 1 )
					SELF:_Top := TRUE
					SELF:_Bottom := FALSE
					SELF:_BufferValid := FALSE
                    // Apply Filter and SetDeleted
                    RETURN SkipFilter(1)
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
                    // Apply Filter and SetDeleted
                    RETURN SkipFilter(-1)
				END LOCK
			ENDIF
			RETURN FALSE
			
			/// <inheritdoc />
		METHOD GoTo(nRec AS LONG) AS LOGIC
			IF SELF:_hFile != F_ERROR 
				BEGIN LOCK SELF
					// Validate any pending change
					SELF:GoCold()
					// On Shared env, it can be correct to guess that some changes have been made
					IF SELF:Shared .AND. nRec > SELF:RecCount 
						SELF:_RecCount := SELF:_calculateRecCount()
					ENDIF
					IF ( nRec <= SELF:RecCount ) .AND. ( nRec > 0 )
						// Normal positioning
                        // VO does not set _Found to TRUE for a succesfull Goto. It does set _Found to false for a failed Goto
						SELF:_RecNo := nRec
						SELF:_EOF := FALSE
						SELF:_Bof := FALSE
						//SELF:_Found :=TRUE    
						SELF:_BufferValid := FALSE
						SELF:_isValid := TRUE
					ELSEIF nRec < 0
                        // skip to  BOF. Move to record 1
						SELF:_RecNo := 1
						SELF:_EOF := FALSE
						SELF:_Bof := TRUE
						SELF:_Found :=FALSE
						SELF:_BufferValid := FALSE
						SELF:_isValid := FALSE
					ELSE
						// File empty, or trying to go outside ?
						SELF:_RecNo := SELF:RecCount + 1
						SELF:_EOF := TRUE
						SELF:_Bof := TRUE
						SELF:_Found := FALSE
						SELF:_BufferValid := FALSE
						SELF:_isValid := FALSE
					ENDIF
					RETURN TRUE
				END LOCK
			ENDIF
			RETURN FALSE
			
			/// <inheritdoc />
		METHOD GoToId(oRec AS OBJECT) AS LOGIC
			LOCAL result AS LOGIC
			BEGIN LOCK SELF
				TRY
					VAR nRec := Convert.ToInt32( oRec )
					result := SELF:Goto( nRec )
				CATCH ex AS Exception
                    SELF:_dbfError(ex, SubCodes.EDB_GOTO,GenCode.EG_DATATYPE,  "DBF.GoToId") 
					result := FALSE
				END TRY
			END LOCK
			RETURN result
			
			
			
			/// <inheritdoc />
		METHOD Skip(nToSkip AS INT) AS LOGIC
			LOCAL result := FALSE AS LOGIC
			SELF:ForceRel()
			IF ( SELF:_hFile != F_ERROR )
				SELF:_Top := FALSE
				SELF:_Bottom := FALSE
				LOCAL delState := XSharp.RuntimeState.Deleted AS LOGIC
				// 
				IF ( nToSkip == 0 ) .OR. delState .OR. ( SELF:_FilterInfo != NULL .AND. SELF:_FilterInfo:Active )
					// 
					result := SUPER:Skip( nToSkip )
				ELSE
					result := SELF:SkipRaw( nToSkip )
					// We reached the top ?
					IF result .AND. ( nToSkip < 0 ) .AND. SELF:_Bof
						SELF:GoTop()
						SELF:_Bof := TRUE
					ENDIF
					IF nToSkip < 0 
						SELF:_Eof := FALSE
					ELSEIF nToSkip > 0 
						SELF:_Bof := FALSE
					ENDIF
				ENDIF
			ENDIF
			RETURN result
			
			
		/// <inheritdoc />
		METHOD SkipRaw(nToSkip AS INT) AS LOGIC 
			LOCAL isOk := TRUE AS LOGIC
			//
			IF ( nToSkip == 0 )
				// Refresh current Recno
				LOCAL currentBof := SELF:_Bof AS LOGIC
				LOCAL currentEof := SELF:_EOF AS LOGIC
				SELF:Goto( SELF:_Recno )
				SELF:_Bof := currentBof
				SELF:_Eof := currentEof
			ELSE
				isOk := SELF:Goto( SELF:_RecNo + nToSkip )
			ENDIF
			RETURN isOK 
			
			
			// Append and Delete
		METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
			LOCAL isOk := FALSE AS LOGIC
			IF ( SELF:_hFile != F_ERROR )
				BEGIN LOCK SELF
					// Validate
					isOk := SELF:GoCold()
					IF ( isOk )
						//
						IF SELF:_ReadOnly 
							// Error !! Cannot be written !
							SELF:_DbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
							isOk := FALSE
						ENDIF
						IF  SELF:Shared 
							IF  SELF:_Locks:Count > 0  .AND. lReleaseLock
								SELF:UnLock( 0 ) // Unlock All Records
							ENDIF
							SELF:AppendLock( DbLockMode.Lock )
						ELSE
							SELF:_HeaderLocked := FALSE
						ENDIF
						IF ( isOk )
                            Array.Copy(SELF:_BlankBuffer, SELF:_RecordBuffer, SELF:_RecordLength)
							// Now, update state
							SELF:_RecCount++
							SELF:_RecNo := SELF:_RecCount
							SELF:_EOF := FALSE
							SELF:_Bof := FALSE
							SELF:_Deleted := FALSE
							SELF:_BufferValid := TRUE
							SELF:_isValid := TRUE
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
                    VAR unlocked := FFUnlock( SELF:_hFile, (DWORD)SELF:_lockScheme:Offset, 1 )
                    IF unlocked
					    SELF:_HeaderLocked := FALSE
                    ENDIF
				CATCH ex AS Exception
					SELF:_HeaderLocked := FALSE
                    SELF:_dbfError(ex, SubCodes.ERDD_WRITE_UNLOCK,GenCode.EG_LOCK_ERROR,  "DBF.HeaderLock") 
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
			IF SELF:Shared 
				BEGIN LOCK SELF
					//
					SELF:GoCold()
					TRY
						recordNbr := Convert.ToInt32( oRecId )
					CATCH ex AS Exception
						recordNbr := 0
                        SELF:_dbfError(ex, SubCodes.ERDD_DATATYPE,GenCode.EG_LOCK_ERROR,  "DBF.UnLock") 
					END TRY
					//
					isOk := TRUE
					IF ( SELF:_Locks:Count > 0 )
						IF ( recordNbr == 0 )
                            // Create a copy with ToArray() because _unlockRecord modifies the collection
							FOREACH VAR nbr IN SELF:_Locks:ToArray()
								isOk := isOk .AND. SELF:_unlockRecord( nbr )
							NEXT
							SELF:_Locks:Clear()  // Should be useless as the record is removed from the list in _unlockRecord
						ELSE
							isOk := SELF:_unlockRecord( recordNbr )
						ENDIF
					ENDIF
					IF ( SELF:_fLocked ) .AND. ( recordNbr == 0 )
						isOk := SELF:_unlockFile( )
						IF isOk
                            SELF:_fLocked := FALSE
                        ENDIF
					ENDIF
				END LOCK
			ELSE
				isOk := TRUE
			ENDIF
			RETURN isOk
			
			// Unlock file. The Offset depends on the LockScheme
		PROTECT METHOD _unlockFile( ) AS LOGIC
			LOCAL unlocked AS LOGIC
			LOCAL iOffset AS INT64
			//
			iOffset := SELF:_lockScheme:Offset
			IF ( SELF:_lockScheme:Direction < 0 )
				iOffset -= (INT64)SELF:_lockScheme:FileSize
			ELSE
				iOffset++
			ENDIF
			//
			TRY
				unlocked := FFUnlock( SELF:_hFile, (DWORD)iOffset, (DWORD)SELF:_lockScheme:FileSize )
			CATCH ex AS Exception
				unlocked := FALSE
                SELF:_dbfError(ex, SubCodes.ERDD_WRITE_UNLOCK,GenCode.EG_LOCK_ERROR,  "DBF._unlockFile") 
			END TRY
			RETURN unlocked
			
			// Unlock a record. The Offset depends on the LockScheme
		PROTECT METHOD _unlockRecord( recordNbr AS LONG ) AS LOGIC
			LOCAL unlocked AS LOGIC
			LOCAL iOffset AS INT64
			//
			iOffset := SELF:_lockScheme:Offset
			IF ( SELF:_lockScheme:Direction < 0 )
				iOffset -= (INT64)recordNbr
			ELSEIF( SELF:_lockScheme:Direction == 2 )
				iOffset += (INT64)( ( recordNbr - 1 ) * SELF:_RecordLength + SELF:_HeaderLength )
			ELSE
				iOffset += (INT64)recordNbr
			ENDIF
			//
			TRY
				unlocked := FFUnlock( SELF:_hFile, (DWORD)iOffset, (DWORD)SELF:_lockScheme:RecordSize )
			CATCH ex AS Exception
				unlocked := FALSE
                SELF:_dbfError(ex, SubCodes.ERDD_WRITE_UNLOCK,GenCode.EG_LOCK_ERROR,  "DBF._unlockRecord") 
			END TRY
			IF( unlocked )
				SELF:_Locks:Remove( recordNbr )
			ENDIF
			RETURN unlocked
			
			// Lock the file. The Offset depends on the LockScheme
		PROTECT METHOD _lockFile( ) AS LOGIC
			LOCAL locked AS LOGIC
			LOCAL iOffset AS INT64
			//
			iOffset := SELF:_lockScheme:Offset
			IF ( SELF:_lockScheme:Direction < 0 )
				iOffset -= (INT64)SELF:_lockScheme:FileSize
			ELSE
				iOffset++
			ENDIF
			//
			TRY
				locked := FFLock( SELF:_hFile, (DWORD)iOffset, (DWORD)SELF:_lockScheme:FileSize )
			CATCH ex AS Exception
            	locked := FALSE
			    SELF:_dbfError(ex, SubCodes.ERDD_WRITE_LOCK,GenCode.EG_LOCK_ERROR,  "DBF._lockFile") 
			END TRY
			RETURN locked
			
			// Place a lock : <nOffset> indicate where the lock should be; <nLong> indicate the number bytes to lock
			// If it fails, the operation is tried <nTries> times, waiting 1ms between each operation.
			// Return the result of the operation
		PROTECTED METHOD _tryLock( nOffset AS INT64, nLong AS LONG, nTries AS LONG  ) AS LOGIC
			LOCAL locked AS LOGIC
			//
			REPEAT
				TRY
					locked := FFLock( SELF:_hFile, (DWORD)nOffset, (DWORD)nLong )
				CATCH ex AS Exception
					locked := FALSE
                    SELF:_dbfError(ex, SubCodes.ERDD_WRITE_LOCK,GenCode.EG_LOCK_ERROR,  "DBF._tryLock") 
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
			IF SELF:Shared .AND. !SELF:_fLocked 
				//
				SELF:GoCold()
				IF ( SELF:_Locks:Count > 0 )
					// create a copy of the collection by calling ToArray to avoid a runtime error
                    // because the collection will be changed by the call to _unlockRecord()
                    FOREACH VAR nbr IN SELF:_Locks:ToArray()
						SELF:_unlockRecord( nbr )
					NEXT
					SELF:_Locks:Clear()
                ENDIF
				SELF:_fLocked := SELF:_lockFile()
				// Invalidate Buffer
				SELF:Goto( SELF:RecNo )
				isOk := SELF:_fLocked
			ENDIF
			RETURN isOk
			
			// Lock a record number. The Offset depends on the LockScheme
		PROTECTED METHOD _lockRecord( recordNbr AS LONG ) AS LOGIC
			LOCAL locked AS LOGIC
			LOCAL iOffset AS INT64
			//
			iOffset := SELF:_lockScheme:Offset
			IF SELF:_lockScheme:Direction < 0 
				iOffset -= (INT64)recordNbr
			ELSEIF( SELF:_lockScheme:Direction == 2 )
				iOffset += (INT64)( ( recordNbr - 1 ) * SELF:_RecordLength + SELF:_HeaderLength )
			ELSE
				iOffset += (INT64)recordNbr
			ENDIF
			//
			TRY
				locked := FFLock( SELF:_hFile, (DWORD)iOffset, (DWORD)SELF:_lockScheme:RecordSize )
			CATCH ex AS Exception
				locked := FALSE
                SELF:_dbfError(ex, SubCodes.ERDD_WRITE_LOCK,GenCode.EG_LOCK_ERROR,  "DBF._lockRecord") 
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
				CATCH ex AS Exception
					SELF:_dbfError( ex, ERDD.DATATYPE, XSharp.Gencode.EG_DATATYPE )
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
				IF SELF:Shared .AND. !SELF:_Locks:Contains( (LONG)nToLock ) 
					IF ( lockInfo:@@METHOD == DbLockInfo.LockMethod.Multiple )
						// Just add the lock to the list
						isOk := SELF:_lockRecord( (LONG)nToLock )
					ELSE // DbLockInfo.LockMethod.Exclusive
						// Release the locks
                        SELF:UnLock(0)  
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
                IF ! SELF:_Hot
				    SELF:GoHot()
                ENDIF
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
					IF SELF:_isValid
						SELF:_RecordBuffer[ 0 ] := (BYTE)'*'
						SELF:_Deleted := TRUE
						//
                        IF ! SELF:_Hot
				            SELF:GoHot()
                        ENDIF
					ENDIF
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
                    Array.Copy(SELF:_RecordBuffer, records, SELF:_RecordLength)
				ENDIF
			END LOCK
			RETURN records
			
			// Put the content of a record as raw data
		METHOD PutRec(aRec AS BYTE[]) AS LOGIC
			LOCAL isOk := FALSE AS LOGIC
			// First, Check the Size
			IF ( aRec:Length == SELF:_RecordLength )
				IF SELF:_readRecord()
                    Array.Copy(aRec, SELF:_RecordBuffer, SELF:_RecordLength)
                    IF ! SELF:_Hot
				        isOk := SELF:GoHot()
                    ELSE
                        isOk := TRUE
                    ENDIF
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
			IF SELF:Shared 
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
			IF ( SELF:_hFile != F_ERROR )
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
						isOk := FClose( SELF:_hFile )
						IF ( SELF:_HasMemo )
							SELF:CloseMemFile()
						ENDIF

                        isOk := SUPER:Close() .AND. isOk
					CATCH ex AS Exception
						isOk := FALSE
                        SELF:_dbfError(ex, SubCodes.ERDD_CLOSE_FILE,GenCode.EG_CLOSE,  "DBF.Close") 

					END TRY
					SELF:_hFile := F_ERROR
				ENDIF
			ENDIF
			RETURN isOk
			
			// Move to the End of file, and place a End-Of-File Marker (0x1A)
		PRIVATE METHOD _putEndOfFileMarker() AS LOGIC
			// According to DBASE.com Knowledge base :
			// The end of the file is marked by a single byte, with the end-of-file marker, an OEM code page character value of 26 (0x1A).
			LOCAL lOffset := SELF:_HeaderLength + SELF:_RecCount * SELF:_RecordLength AS LONG
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
				SELF:_OpenInfo:Extension := _Extension
				//
			ENDIF
			SELF:_OpenInfo:FileName := System.IO.Path.ChangeExtension( SELF:_OpenInfo:FileName, SELF:_OpenInfo:Extension )
			//
			SELF:_Hot := FALSE
			SELF:_FileName := SELF:_OpenInfo:FileName
			SELF:_Alias := SELF:_OpenInfo:Alias
			SELF:_Shared := SELF:_OpenInfo:Shared
			SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
			//
			SELF:_hFile    := FCreate2( SELF:_FileName, FO_EXCLUSIVE)
			IF ( SELF:_hFile != F_ERROR )
				LOCAL fieldCount :=  SELF:_Fields:Length AS INT
				LOCAL fieldDefSize := fieldCount * DbfField.SIZE AS INT
				// First, just the Header
				SELF:_Header:HeaderLen := SHORT(DbfHeader.SIZE + fieldDefSize+ 2 ) 
				SELF:_Header:isHot := TRUE
				//
				LOCAL codePage AS LONG
				IF (XSharp.RuntimeState.Ansi)
					SELF:_Ansi := TRUE
					codePage := XSharp.RuntimeState.WinCodePage
				ELSE
					SELF:_Ansi := FALSE
					codePage := XSharp.RuntimeState.DosCodePage
				ENDIF
                IF SELF:_Ansi
                    SELF:_lockScheme:Initialize( DbfLockingModel.VoAnsi )
                ELSE
			        SELF:_lockScheme:Initialize( DbfLockingModel.Clipper52 )
                ENDIF

				SELF:_Encoding := System.Text.Encoding.GetEncoding( codePage ) 
				//
				IF ( SELF:_oMemo != NULL )
					SELF:_oMemo:_Encoding := SELF:_Encoding
				ENDIF
				// Convert the Windows CodePage to a DBF CodePage
				SELF:_Header:CodePage := CodePageExtensions.ToHeaderCodePage( (OsCodePage)codePage ) 
				// Init Header version, should it be a parameter ?
                IF SELF:_Ansi
				    SELF:_Header:Version := IIF(SELF:_HasMemo, DBFVersion.VOWithMemo , DBFVersion.VO )
                ELSE
                    SELF:_Header:Version := IIF(SELF:_HasMemo, DBFVersion.FoxBaseDBase3WithMemo , DBFVersion.FoxBaseDBase3NoMemo )
                ENDIF
				// This had been initialized by AddFields()
				SELF:_Header:RecordLen := (SHORT)SELF:_RecordLength
				// This will fill the Date and RecCount
				isOK := SELF:_writeHeader()
				IF ( isOK )
					SELF:_HeaderLength := SELF:_Header:HeaderLen
					isOk := SELF:_writeFieldsHeader()
					IF ( isOk )
						SELF:_RecordLength := SELF:_Header:RecordLen
						IF ( SELF:_HasMemo )
							isOk := SELF:CreateMemFile( info )
						ENDIF
						SELF:_AllocateBuffers()
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
                VAR ex := FException()
				SELF:_DbfError( ex, ERDD.CREATE_FILE, XSharp.Gencode.EG_CREATE )
			ENDIF
			RETURN isOK
			
			// Write the Fields Header, based on the _Fields List
		PRIVATE METHOD _writeFieldsHeader() AS LOGIC
			LOCAL isOk AS LOGIC
			LOCAL fieldCount :=  SELF:_Fields:Length AS INT
			LOCAL fieldDefSize := fieldCount * DbfField.SIZE AS INT
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
                Array.Copy(currentField:Buffer, 0, fieldsBuffer, i*DbfField.SIZE, DbfField.SIZE )
			NEXT
			// Terminator
			fieldsBuffer[fieldDefSize] := 13
			// Go end of Header
			isOk := ( FSeek3( SELF:_hFile, DbfHeader.SIZE, SeekOrigin.Begin ) == DbfHeader.SIZE )
			IF ( isOk )
				// Write Fields and Terminator
				TRY
					isOk := ( FWrite3( SELF:_hFile, fieldsBuffer, (DWORD)fieldsBuffer:Length ) == (DWORD)fieldsBuffer:Length )
				CATCH ex AS Exception
					SELF:_DbfError( ex, ERDD.WRITE, XSharp.Gencode.EG_WRITE )
				END TRY
			ENDIF
			//
			RETURN isOk
			
			/// <inheritdoc />
		METHOD Open(info AS XSharp.RDD.Support.DbOpenInfo) AS LOGIC
			LOCAL isOK AS LOGIC
			//
			isOk := FALSE
			SELF:_OpenInfo := info
			// Should we set to .DBF per default ?
			IF String.IsNullOrEmpty(SELF:_OpenInfo:Extension)
				SELF:_OpenInfo:Extension := _Extension
			ENDIF
			SELF:_OpenInfo:FileName := System.IO.Path.ChangeExtension( SELF:_OpenInfo:FileName, SELF:_OpenInfo:Extension )
			//
			SELF:_Hot := FALSE
			SELF:_FileName := SELF:_OpenInfo:FileName
			// Check that we have a FullPath
			IF (Path.GetDirectoryName(SELF:_FileName):Length == 0)
                IF File(SELF:_FileName)
				    SELF:_FileName := FPathName()
                ENDIF
			ENDIF
			SELF:_Alias := SELF:_OpenInfo:Alias
			SELF:_Shared := SELF:_OpenInfo:Shared
			SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
			SELF:_hFile    := FOpen(SELF:_FileName, SELF:_OpenInfo:FileMode)
			IF ( SELF:_hFile != F_ERROR )
				isOk := SELF:_ReadHeader()
				IF ( isOk )
					IF ( SELF:_HasMemo )
						isOk := SELF:OpenMemFile( info )
					ENDIF
					SELF:GoTop()
					//
					SELF:_Ansi := SELF:_Header:IsAnsi
					SELF:_Encoding := System.Text.Encoding.GetEncoding( CodePageExtensions.ToCodePage( SELF:_Header:CodePage )  )
					//
					IF ( SELF:_oMemo != NULL )
						SELF:_oMemo:_Encoding := SELF:_Encoding
					ENDIF
				ELSE
					SELF:_DbfError( ERDD.CORRUPT_HEADER, XSharp.Gencode.EG_CORRUPTION )
				ENDIF
			ELSE
				// Error or just FALSE ?
				isOK := FALSE
                LOCAL ex := FException() AS Exception
				SELF:_DbfError( ex, ERDD.OPEN_FILE, XSharp.Gencode.EG_OPEN )
			ENDIF
            IF SELF:_Ansi
                SELF:_lockScheme:Initialize( DbfLockingModel.VoAnsi )
            ELSE
			    SELF:_lockScheme:Initialize( DbfLockingModel.Clipper52 )
            ENDIF

			//
			RETURN isOk
			
			// Read the DBF Header, retrieve RecCount, then read the Fields Header
		PRIVATE METHOD _readHeader() AS LOGIC
			LOCAL isOk AS LOGIC
			//
			isOk := ( FRead3(SELF:_hFile, SELF:_Header:Buffer, DbfHeader.SIZE) == DbfHeader.SIZE )
			//
			IF ( isOk )
				SELF:_HeaderLength := SELF:_Header:HeaderLen
				//
				LOCAL fieldCount := (( SELF:_HeaderLength - DbfHeader.SIZE) / DbfField.SIZE ) AS INT
				// Something wrong in Size...
				IF ( fieldCount <= 0 )
					RETURN FALSE
				ENDIF
				SELF:_RecCount := SELF:_Header:RecCount
				// Move to top, after header
				isOk := ( FSeek3( SELF:_hFile, DbfHeader.SIZE, SeekOrigin.Begin ) == DbfHeader.SIZE )
				IF ( isOk )
					isOk := _readFieldsHeader()
				ENDIF
			ENDIF
			RETURN isOk
			
			// Read the Fields Header, filling the _Fields List with RddFieldInfo
		PRIVATE METHOD _readFieldsHeader() AS LOGIC
			LOCAL isOk AS LOGIC
			LOCAL fieldCount := (( SELF:_HeaderLength - DbfHeader.SIZE) / DbfField.SIZE ) AS INT
			LOCAL fieldDefSize := fieldCount * DbfField.SIZE AS INT
			//
			// Read full Fields Header
			VAR fieldsBuffer := BYTE[]{ fieldDefSize }
			isOk := ( FRead3( SELF:_hFile, fieldsBuffer, (DWORD)fieldDefSize ) == (DWORD)fieldDefSize )
			IF ( isOk )
				SELF:_HasMemo := FALSE
				VAR currentField := DbfField{}
				currentField:Initialize()
				// Now, process
				//SELF:_Fields := DbfRddFieldInfo[]{ fieldCount }
				SELF:SetFieldExtent( fieldCount )
				LOCAL nStart AS INT
				nStart := 1
				IF __ARRAYBASE__ == 0
					nStart -= 1
				ENDIF
				FOR VAR i := nStart TO fieldCount - ( 1 - nStart )
					//
                    Array.Copy(fieldsBuffer, i*DbfField.SIZE, currentField:Buffer, 0, DbfField.SIZE )
					LOCAL info AS RddFieldInfo
					info := RddFieldInfo{ currentField:Name, currentField:Type, currentField:Len, currentField:Dec }
					//SELF:_Fields[ i ] := info
					// DON'T FORGET TO FILL THE _fieldNames DICTIONNARY !!!!
					//SELF:_fieldNames:Add(info:Name:Trim(), i)
					SELF:AddField( info )
					// !!! WARNING !!! The Field Index MUST NOT BE corrected by __ARRAYBASE__ when
					// calling _isMemoField()
					IF SELF:_isMemoField( i + ( 1 - nStart ))
						SELF:_HasMemo := TRUE
					ENDIF
				NEXT
				// Allocate the Buffer to read Records
				SELF:_RecordLength := (WORD)SELF:_Header:RecordLen
                SELF:_AllocateBuffers()
                
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
				SELF:_Header:Year := (BYTE)(dtInfo:Year % 100)
				SELF:_Header:Month := (BYTE)dtInfo:Month
				SELF:_Header:Day := (BYTE)dtInfo:Day
				// Update the number of records
				SELF:_Header:RecCount := SELF:RecCount
				// Now Write
				// Go Top
				FSeek3( SELF:_hFile, 0, FS_SET )
				// Write just the File Header
				TRY
					ret := ( FWrite3( SELF:_hFile, SELF:_Header:Buffer, (DWORD)DbfHeader.SIZE ) == (DWORD)DbfHeader.SIZE )
				CATCH ex AS Exception
					SELF:_DbfError( ex, ERDD.WRITE, XSharp.Gencode.EG_WRITE )
					ret := FALSE
				END TRY
				// Ok, go Cold
				SELF:_Header:isHot := FALSE
			ENDIF
			//
			RETURN ret
			
		
			// Fields
			// Set the Number of Fields the AddField Method will add
			/// <inheritdoc />
		METHOD SetFieldExtent( fieldCount AS LONG ) AS LOGIC
			SELF:_HasMemo := FALSE
			RETURN SUPER:SetFieldExtent(fieldCount)
			
			// Add a Field to the _Fields List. Fields are added in the order of method call
			/// <inheritdoc />
		METHOD AddField(info AS RddFieldInfo) AS LOGIC
			LOCAL isOk AS LOGIC
 			isok := SUPER:AddField( info )
			IF ( isOk ) .AND. info:FieldType == DbFieldType.Memo
				SELF:_HasMemo := TRUE
            ENDIF
			RETURN isOk
			
			// Check if a Field definition is correct :
			// Date Length must be 8, Number are long enough to store Dot anf Decs (if any), ...
		PROTECT OVERRIDE METHOD _checkFields(info AS RddFieldInfo) AS LOGIC
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
			RETURN TRUE
			
			/// <inheritdoc />
		METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
			LOCAL oResult AS OBJECT
			LOCAL nArrPos := nFldPos AS LONG
			BEGIN LOCK SELF
                IF ! SELF:_readRecord()
                    oResult := SUPER:FieldInfo(nFldPos, nOrdinal, oNewValue)
                    RETURN oResult
                ENDIF
				IF SELF:_FieldIndexValidate(nArrPos)
					IF __ARRAYBASE__ == 0
						nArrPos -= 1
					ENDIF
				ENDIF
				//
				SWITCH nOrdinal
                    // These are handled in the parent class and also take care of aliases etc.
                    CASE DbFieldInfo.DBS_NAME
                    CASE DbFieldInfo.DBS_LEN
                    CASE DbFieldInfo.DBS_DEC
                    CASE DbFieldInfo.DBS_TYPE
                    CASE DbFieldInfo.DBS_ALIAS
                        oResult := SUPER:FieldInfo(nFldPos, nOrdinal, oNewValue)
                        
                    CASE DbFieldInfo.DBS_ISNULL
                    CASE DbFieldInfo.DBS_COUNTER
                    CASE DbFieldInfo.DBS_STEP
                    
                    CASE DbFieldInfo.DBS_BLOB_GET
                    CASE DbFieldInfo.DBS_BLOB_TYPE
                        // Returns the data type of a BLOB (memo) field. This
                        // is more efficient than using Type() or ValType()
                        // since the data itself does not have to be retrieved
                        // from the BLOB file in order to determine the type.
                    CASE DbFieldInfo.DBS_BLOB_LEN	    // Returns the storage length of the data in a BLOB (memo) file
                    CASE DbFieldInfo.DBS_BLOB_OFFSET	// Returns the file offset of the data in a BLOB (memo) file.
                    CASE DbFieldInfo.DBS_BLOB_POINTER
                        // Returns a numeric pointer to the data in a blob
                        // file. This pointer can be used with BLOBDirectGet(),
                        // BLOBDirectImport(), etc.
                        
                    CASE DbFieldInfo.DBS_BLOB_DIRECT_TYPE
                    CASE DbFieldInfo.DBS_BLOB_DIRECT_LEN
                    
                    CASE DbFieldInfo.DBS_STRUCT
                    CASE DbFieldInfo.DBS_PROPERTIES
                    CASE DbFieldInfo.DBS_USER
					OTHERWISE
                        // Everything falls through to parent at this moment
						oResult := SUPER:FieldInfo(nFldPos, nOrdinal, oNewValue)
					END SWITCH
			END LOCK
			RETURN oResult
			
			
			// Read & Write
			
			// Move to the current record, then read the raw Data into the internal RecordBuffer; Set the DELETED Flag
		PROTECTED METHOD _readRecord() AS LOGIC
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
				LOCAL lOffset := SELF:_HeaderLength + ( SELF:_RecNo - 1 ) * SELF:_RecordLength AS LONG
				isOk := ( FSeek3( SELF:_hFile, lOffset, FS_SET ) == lOffset )
				IF ( isOk )
					// Read Record
					isOk := ( FRead3( SELF:_hFile, SELF:_RecordBuffer, (DWORD)SELF:_RecordLength ) == (DWORD)SELF:_RecordLength )
					IF ( isOk )
						SELF:_BufferValid := TRUE
						SELF:_isValid := TRUE
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
					LOCAL recordPos AS LONG
					//
					recordPos := SELF:_HeaderLength + ( SELF:_RecNo - 1 ) * SELF:_RecordLength
					isOk := ( FSeek3( SELF:_hFile, recordPos, FS_SET ) == recordPos )
					IF (isOk)
						// Write Record
						TRY
							FWrite3( SELF:_hFile, SELF:_RecordBuffer, (DWORD)SELF:_RecordLength )
							// Don't forget to Update Header
							SELF:_Header:isHot := TRUE
							IF SELF:Shared 
								SELF:_writeHeader()
							ENDIF
						CATCH ex AS Exception
							SELF:_DbfError( ex, ERDD.WRITE, XSharp.Gencode.EG_WRITE )
						END TRY
					ENDIF
				ENDIF
			ENDIF
			RETURN isOk
			
			// Convert a Julian Date to a System.DateTime Date
		INTERNAL METHOD _julianToDateTime(julianDateAsLong AS INT64) AS System.DateTime
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
		INTERNAL METHOD _dateTimeToJulian( dt AS DateTime ) AS LONG
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
		INTERNAL VIRTUAL METHOD _convertDataToField( buffer AS BYTE[], offSet AS INT, fieldType AS DbFieldType, length AS LONG, nDec AS LONG) AS OBJECT
			LOCAL data := NULL AS OBJECT
			VAR str :=  SELF:_Encoding:GetString(buffer,offSet, length)
			// !!! WARNING !!! Space char can be significant (specially in Memo!)
			SWITCH fieldType
				CASE DbFieldType.Float
				CASE DbFieldType.Number
				CASE DbFieldType.Double
				CASE DbFieldType.Currency
				CASE DbFieldType.Integer
					LOCAL r8 AS REAL8
					// Default value is 0
					r8 := 0.0
					IF (! String.IsNullOrWhiteSpace(str))
						//

						IF ( ( fieldType == DbFieldType.Number ) .AND. (nDec == 0 ) ) .OR. ( fieldType == DbFieldType.Integer ) 
							r8 := System.Convert.ToInt32(str)
						ELSE
                            _numformat:NumberDecimalDigits := nDec
							r8 := System.Convert.ToDouble(str, _numFormat)
						ENDIF
					ENDIF
					data := DbFloat{r8, length, nDec} 
				CASE DbFieldType.Character
					// Keep the size ??
					IF !String.IsNullOrWhiteSpace(str)
						data := str
					ELSE
						// Default value is a empty string, with the correct length
						data := STRING{ ' ', length }
					ENDIF
				CASE DbFieldType.Date
					// Default Value is an EmptyDate
					IF !String.IsNullOrWhiteSpace(str)
						// WARNING !!! Only digits in DATE Field, unless return an EmptyDate
						LOCAL emptyDate := FALSE AS LOGIC
						FOREACH VAR ch IN str
							IF !Char.IsDigit( ch )
								emptyDate := TRUE
								EXIT
							ENDIF
						NEXT
						//
						IF !emptyDate 
                            VAR year  := Int32.Parse(str:Substring(0,4))
							VAR month := Int32.Parse(str:Substring(4,2))
                            VAR day   := Int32.Parse(str:Substring(6,2))
							data := DbDate{Year, Month, Day}
						ELSE
							data := DbDate{0,0,0}
						ENDIF
					ELSE
						data := DbDate{0,0,0}
					ENDIF
					//                    IF ((FIELD:Flags & DBFFieldFlags.AllowNullValues) != DBFFieldFlags.AllowNullValues)
						//                        //
						//                        data := System.DateTime.MinValue
					//                    ENDIF
					
				CASE DbFieldType.Logic
					// Default Value is FALSE
					data := FALSE
					IF ! String.IsNullOrWhiteSpace(str)
						SWITCH str[0]
                        CASE 'T'
                        CASE 't'
                        CASE 'Y'
                        CASE 'y'
						    data := TRUE
                        OTHERWISE
                            data := FALSE
                        END SWITCH
					ENDIF
					//                    IF ((FIELD:Flags & DBFFieldFlags.AllowNullValues) != DBFFieldFlags.AllowNullValues)
						//                        //
						//                        data := FALSE
					//                    ENDIF
				CASE DbFieldType.DateTime
					// Default value is Minium DateTime
					data := System.DateTime.MinValue
					IF (! (String.IsNullOrWhiteSpace(str) .OR. (System.BitConverter.ToInt64(buffer, 0) == 0)))
						//
						data := _julianToDateTime(System.BitConverter.ToInt64(buffer, 0))
					ENDIF
					//                    IF ((FIELD:Flags & DBFFieldFlags.AllowNullValues) != DBFFieldFlags.AllowNullValues)
						//                        //
						//                        data := System.DateTime.MinValue
					//                    ENDIF
				OTHERWISE
					// Unknown, bring back the buffer
					data := buffer
				END SWITCH
			RETURN Data
			
			// Convert the value (OBJECT) to the corresponding DBF Type (fieldType), and put the result in binary form (buffer)
		INTERNAL VIRTUAL METHOD _convertFieldToData( oValue AS OBJECT, buffer AS BYTE[], offset AS LONG, length AS LONG, fieldType AS DbFieldType, dec AS LONG) AS LOGIC
			LOCAL encoding AS Encoding //ASCIIEncoding
			LOCAL isOk := FALSE AS LOGIC
			LOCAL str AS STRING
			//
			encoding := SELF:_Encoding
            VAR type := SELF:_getUsualType(oValue)
			SWITCH type
			CASE __UsualType.String
				IF fieldType == DbFieldType.Character 
					IF ( oValue IS CHAR)
						str := STRING{ (CHAR)oValue, 1 }
					ELSE
						str := oValue ASTYPE STRING
                    ENDIF
                    IF str:Length < length
                        str := str:PadRight(length,' ')
                    ENDIF
					encoding:GetBytes( str, 0, length, buffer, offset )
					isOk := TRUE
				ELSE
					// Type Error !
					isOk := FALSE
				ENDIF
			CASE __UsualType.Float
			CASE __UsualType.Long
                IF oValue IS IFloat
                    oValue := ((IFloat) oValue):Value
                ENDIF

                _numformat:NumberDecimalDigits := dec
                SWITCH type
			    CASE __UsualType.Float
                    str := ((REAL8) oValue):ToString("F", _numformat)
			    CASE __UsualType.Long
                    str := ((INT) oValue):ToString("F", _numformat)
                OTHERWISE
                	str := Convert.ToString( oValue, _numformat )
                END SWITCH
				IF ( str:Length > length )
					str := STRING{'*', length}
				ELSE
					str := str:PadLeft(length)
				ENDIF
   				encoding:GetBytes( str, 0, length, buffer, offset )
				isOk := TRUE
				
			CASE __UsualType.Logic
				IF fieldType == DbFieldType.Logic 
					buffer[offset] := IIF( (LOGIC)oValue, (BYTE)'T', (BYTE)'F' )
					isOk := TRUE
				ELSE
					// Type Error !
					isOk := FALSE
				ENDIF
					
			CASE __UsualType.Date
			CASE __UsualType.DateTime
                IF oValue IS IDate
                    LOCAL oDate AS IDate
                    LOCAL dt AS DateTime
                    oDate := (IDate) oValue
                    IF oDate:IsEmpty
                        str := Space(8)
                    ELSE
						dt := DateTime{oDate:Year, oDate:Month, oDate:Day}
						str := dt:ToString( "yyyyMMdd" )
                    ENDIF
					encoding:GetBytes( str, 0, length, buffer, offset )
					isOk := TRUE
                ELSEIF fieldType == DbFieldType.Date 
					LOCAL dt AS DateTime
					dt := (DateTime)oValue
					//
					str := dt:ToString( "yyyyMMdd" )
					encoding:GetBytes( str, 0, length, buffer, offset )
					isOk := TRUE
				ELSEIF fieldType == DbFieldType.DateTime 
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
                    Array.Copy( datBytes, 0, buffer, offset, 4 )
					Array.Copy( timBytes, 0, buffer, offset+4, 4 )
					isOk := TRUE
				ELSE
					// Type Error !
					isOk := FALSE
				ENDIF
			END SWITCH


		RETURN isOk

        INTERNAL METHOD _dbfError(ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD) AS VOID
            SELF:_DbfError(ex, iSubCode, iGenCode, String.Empty, ex?:Message, XSharp.Severity.ES_ERROR)
        			
		INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD) AS VOID
			SELF:_DbfError(NULL, iSubCode, iGenCode, String.Empty, String.Empty, XSharp.Severity.ES_ERROR)
			
		INTERNAL METHOD _dbfError(ex AS Exception,iSubCode AS DWORD, iGenCode AS DWORD, iSeverity AS DWORD) AS VOID
			SELF:_DbfError(ex, iSubCode, iGenCode, String.Empty, String.Empty, iSeverity)

        INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD, iSeverity AS DWORD) AS VOID
			SELF:_DbfError(NULL, iSubCode, iGenCode, String.Empty, String.Empty, iSeverity)

		INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING) AS VOID
			SELF:_DbfError(NULL, iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR)

		INTERNAL METHOD _dbfError(ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING) AS VOID
			SELF:_DbfError(ex, iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR)
			
		INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING) AS VOID
			SELF:_DbfError(NULL, iSubCode, iGenCode, strFunction,strMessage, XSharp.Severity.ES_ERROR)
			
		INTERNAL METHOD _dbfError(ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING, iSeverity AS DWORD) AS VOID
			LOCAL oError AS RddError
			//
            IF ex != NULL
			    oError := RddError{ex}
            ELSE
                oError := RddError{}
            ENDIF
			oError:SubCode := iSubCode
			oError:Gencode := iGenCode
			oError:SubSystem := SELF:SysName
			oError:Severity := iSeverity
			oError:FuncSym  := IIF(strFunction == NULL, "", strFunction) // code in the SDK expects all string properties to be non-NULL
			oError:FileName := SELF:_FileName
            IF String.IsNullOrEmpty(strMessage)  .AND. ex != NULL
                strMessage := ex:Message
            ENDIF
			oError:Description := IIF(strMessage == NULL , "", strMessage)
            RuntimeState.LastRDDError := oError
			//
			THROW oError
		
        INTERNAL METHOD _getUsualType(oValue AS OBJECT) AS __UsualType
            LOCAL typeCde AS TypeCode
            IF oValue == NULL
                RETURN __UsualType.VOID
            ELSE
                typeCde := Type.GetTypeCode(oValue:GetType())
                SWITCH typeCde
                CASE TypeCode.SByte
                CASE TypeCode.Byte
                CASE TypeCode.Int16
                CASE TypeCode.UInt16
                CASE TypeCode.Int32
                    RETURN __UsualType.LONG
                CASE TypeCode.UInt32
                CASE TypeCode.Int64
                CASE TypeCode.UInt64
                CASE TypeCode.Single
                CASE TypeCode.Double
                    RETURN __UsualType.FLOAT
                CASE TypeCode.Boolean
                    RETURN __UsualType.LOGIC
                CASE TypeCode.String
                    RETURN __UsualType.STRING
                CASE TypeCode.DateTime
                    RETURN __UsualType.DateTime
                CASE TypeCode.Object
                    IF oValue IS IDate
                        RETURN __UsualType.DATE
                    ELSEIF  oValue IS IFloat
                        RETURN __UsualType.FLOAT
                    ENDIF
                END SWITCH
            ENDIF
            RETURN __UsualType.OBJECT
            
			

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
					LOCAL encoding AS Encoding //ASCIIEncoding
					// Read actual Data
					encoding := SELF:_Encoding //ASCIIEncoding{}
					VAR str :=  encoding:GetString( SELF:_RecordBuffer, SELF:_Fields[nArrPos]:OffSet, SELF:_Fields[nArrPos]:Length)
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
			LOCAL nArrPos := nFldPos AS LONG
			IF __ARRAYBASE__ == 0
				nArrPos -= 1
			ENDIF
			// Read Record to Buffer
			LOCAL iOffset := SELF:_Fields[nArrPos]:OffSet AS LONG
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
					// We need the Decimals number to return an Integer or a Float
					ret := SELF:_convertDataToField( SELF:_RecordBuffer, iOffset, SELF:_Fields[nArrPos]:FieldType, SELF:_Fields[nArrPos]:Length, SELF:_Fields[nArrPos]:Decimals )
				ENDIF
			ELSE
				IF SELF:EoF
					// Give us the default value
					ret := SELF:_convertDataToField( SELF:_BlankBuffer, iOffset, SELF:_Fields[nArrPos]:FieldType, SELF:_Fields[nArrPos]:Length, SELF:_Fields[nArrPos]:Decimals )
				ELSE
					SELF:_DbfError( ERDD.READ, XSharp.Gencode.EG_READ )
				ENDIF
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
				IF SELF:Shared 
					SELF:HeaderLock( DbLockMode.Lock )
				ENDIF
				SELF:_putEndOfFileMarker()
				SELF:_writeHeader()
				IF SELF:Shared 
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
                    SELF:_Hot := FALSE
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
					IF SELF:_Shared .AND. !SELF:_fLocked .AND. !SELF:_Locks:Contains( SELF:RecNo )
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
			LOCAL nArrPos := nFldPos AS LONG
			IF __ARRAYBASE__ == 0
				nArrPos -= 1
			ENDIF
			IF SELF:_readRecord()
                // GoHot() must be called first because this saves the current index values
                IF ! SELF:_Hot
                    SELF:GoHot()
                ENDIF
                VAR curField := SELF:_Fields[nArrPos]
			    LOCAL offSet := curField:OffSet AS LONG
                LOCAL length  := curField:Length AS LONG
			    IF SELF:_isMemoField( nFldPos )
				    IF _oMemo != NULL
					    IF _oMemo:PutValue(nFldPos, oValue)
						    // Update the Field Info with the new MemoBlock Position
						    SELF:_convertFieldToData( SELF:_oMemo:LastWrittenBlockNumber, SELF:_RecordBuffer, offSet,  length, DbFieldType.Integer, 0 )
					    ENDIF
				    ELSE
					    RETURN SUPER:PutValue(nFldPos, oValue)
				    ENDIF
			    ELSE
				    SELF:_convertFieldToData( oValue, SELF:_RecordBuffer, offSet,  length, curField:FieldType, curField:Decimals )
			    ENDIF
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
		METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT
			IF _oIndex != NULL
				RETURN _oIndex:OrderInfo(nOrdinal,info )
			ELSE
				RETURN SUPER:OrderInfo(nOrdinal,info )
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
        /// <inheritdoc />
		METHOD ChildEnd(info AS DbRelInfo) AS LOGIC
			SELF:ForceRel()
			RETURN SUPER:ChildEnd( info )
			
        /// <inheritdoc />
		METHOD ChildStart(info AS DbRelInfo) AS LOGIC
			SELF:ChildSync( info )
			RETURN SUPER:ChildStart( info )
			
        /// <inheritdoc />
		METHOD ChildSync(info AS DbRelInfo) AS LOGIC
			SELF:GoCold()
			SELF:_RelInfoPending := info
			SELF:SyncChildren()
			RETURN TRUE
			
        /// <inheritdoc />
		METHOD ForceRel() AS LOGIC
			LOCAL isOk    := TRUE AS LOGIC
			LOCAL gotoRec := 0 AS LONG
			IF SELF:_RelInfoPending != NULL
				// Save the current context
				LOCAL currentRelation := SELF:_RelInfoPending AS DbRelInfo
				SELF:_RelInfoPending := NULL
				//
				isOk := SELF:RelEval( currentRelation )
				IF isOk .AND. !((DBF)currentRelation:Parent):_Eof
					TRY
						gotoRec := Convert.ToInt32( SELF:_EvalResult )
					CATCH ex AS InvalidCastException
                        gotoRec := 0
                        SELF:_dbfError(ex, SubCodes.ERDD_DATATYPE,GenCode.EG_DATATYPE,  "DBF.ForceRel") 

					END TRY
				ENDIF
				isOk := SELF:Goto( gotoRec )
				SELF:_Found := SELF:_isValid
				SELF:_Bof := FALSE
			ENDIF
			RETURN isOk
			
			
			/// <inheritdoc />
			METHOD RelArea(nRelNum AS DWORD) AS DWORD
                RETURN SUPER:RelArea(nRelNum)

		/// <inheritdoc />
		    METHOD SyncChildren() AS LOGIC
			LOCAL isOk AS LOGIC
			//
			isOk := TRUE
			FOREACH info AS DbRelInfo IN SELF:_Relations
				IsOk := info:Child:ChildSync( info )
				IF !isOk
					EXIT
				ENDIF
			NEXT
			RETURN isOk
			
			
			
			// Codeblock Support
			
		VIRTUAL METHOD Compile(sBlock AS STRING) AS ICodeblock
			LOCAL result AS ICodeblock
			result := SUPER:Compile(sBlock)
			IF result == NULL
				SELF:_dbfError( SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX,"DBF.Compile")
			ENDIF
			RETURN result
			
		VIRTUAL METHOD EvalBlock( cbBlock AS ICodeblock ) AS OBJECT
			LOCAL result := NULL AS OBJECT
			TRY
				result := SUPER:EvalBlock(cbBlock)
            CATCH ex AS Exception
				SELF:_dbfError(ex, SubCodes.EDB_EXPRESSION, GenCode.EG_SYNTAX, "DBF.EvalBlock")
			END TRY
			RETURN result
			
			// Other
		/// <inheritdoc />
		VIRTUAL METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
			LOCAL oResult AS OBJECT
			oResult := NULL
			SWITCH nOrdinal
				CASE DbInfo.DBI_ISDBF
				CASE DbInfo.DBI_CANPUTREC
					oResult := TRUE
				CASE DbInfo.DBI_GETRECSIZE
					oResult := SELF:_RecordLength
				CASE DbInfo.DBI_LASTUPDATE
					oResult := SELF:_Header:LastUpdate
				CASE DbInfo.DBI_GETHEADERSIZE
					oResult := SELF:_Header:HeaderLen
                 CASE DbInfo.DBI_CODEPAGE
                CASE DbInfo.DBI_DOSCODEPAGE
                CASE DbInfo.DBI_CODEPAGE_HB
                    // DOS or Windows codepage based on DBF Codepage
					oResult := (INT) SELF:_Header:CodePage:ToCodePage()

				CASE DbInfo.DBI_GETLOCKARRAY
                    VAR aLocks := SELF:_Locks:ToArray()
                    System.Array.Sort(aLocks)
                    oResult := aLocks

				CASE DbInfo.DBI_LOCKCOUNT
                    oResult := SELF:_Locks:Count
                 CASE DbInfo.DBI_LOCKOFFSET
                    oResult := SELF:_lockScheme:Offset

                CASE DbInfo.DBI_FILEHANDLE
                    oResult := SELF:_hFile
                CASE DbInfo.DBI_FULLPATH
                    oResult := SELF:_FileName
                CASE DbInfo.DBI_TABLEEXT
                    IF SELF:_FileName != NULL
                        oResult := System.IO.Path.GetExtension(SELF:_FileName)
                    ELSE
                        oResult := _Extension
                    ENDIF
                    IF oNewValue IS STRING
                        _Extension := (STRING) oNewValue
                    ENDIF
  
                 CASE DbInfo.DBI_SHARED
                    oResult := SELF:Shared 

                 CASE DbInfo.DBI_READONLY
                 CASE DbInfo.DBI_ISREADONLY
                    oResult := SELF:_ReadOnly

                 CASE DbInfo.DBI_ISANSI
                    oResult := SELF:_Ansi


                 CASE DbInfo.DBI_ISFLOCK
                    oResult := SELF:_fLocked

                 CASE DbInfo.DBI_MEMOHANDLE
                    oResult := IntPtr.Zero      // Should be handled in the memo subclass
                 CASE DbInfo.DBI_MEMOEXT
                    oResult := ""               // Should be handled in the memo subclass
                 CASE DbInfo.DBI_MEMOBLOCKSIZE
                    oResult := 0
                 CASE DBInfo.DBI_MEMOFIELD
                    oResult := ""
						// DbInfo.TRANSREC
				CASE DbInfo.DBI_VALIDBUFFER
					oResult := SELF:_BufferValid
					// CASE DbInfo.DBI_POSITIONED
					
				CASE DbInfo.DBI_OPENINFO
                    oResult := SELF:_OpenInfo

				CASE DbInfo.DBI_DB_VERSION
				CASE DbInfo.DBI_RDD_VERSION
                    LOCAL oAsm AS System.Reflection.AssemblyName
                    LOCAL oType AS System.Type
                    oType := typeof(DBF)
                    oAsm := oType:Assembly:GetName()
                    RETURN oAsm:Version:ToString()

                // Harbour extensions. Some are supported. Other not yet
                // case DbInfo.DBI_ISREADONLY
				CASE DbInfo.DBI_LOCKSCHEME
                    RETURN 0
				CASE DbInfo.DBI_ROLLBACK
                    IF SELF:_Hot
                        IF SELF:_NewRecord
                            Array.Copy(SELF:_BlankBuffer, SELF:_RecordBuffer, SELF:_RecordLength)
                            SELF:_Deleted := FALSE
                        ELSE
                            SELF:_BufferValid := FALSE
                        ENDIF
                        SELF:_Hot := FALSE
                    ENDIF
				CASE DbInfo.DBI_PASSWORD
                    oResult := NULL             
                CASE DbInfo.DBI_ISENCRYPTED     
                    oResult := FALSE
				CASE DbInfo.DBI_MEMOTYPE
                    oResult := DB_MEMO_NONE
                CASE DbInfo.DBI_SEPARATOR
                    oResult := ""
                CASE DbInfo.DBI_MEMOVERSION
                    oResult := 0
			    CASE DbInfo.DBI_TABLETYPE
                    oResult := 0
                CASE DbInfo.DBI_SCOPEDRELATION
                    oResult := FALSE
                CASE DbInfo.DBI_TRIGGER
                    oResult := NULL     // Todo
                CASE DbInfo.DBI_DECRYPT         // Todo
                CASE DbInfo.DBI_ENCRYPT         // Todo
                CASE DbInfo.DBI_MEMOPACK
                CASE DbInfo.DBI_DIRTYREAD
                CASE DbInfo.DBI_POSITIONED
                CASE DbInfo.DBI_ISTEMPORARY
                CASE DbInfo.DBI_LOCKTEST
                CASE DbInfo.DBI_TRANSREC
                CASE DbInfo.DBI_SETHEADER
                //CASE DbInfo.DBI_CODEPAGE_HB    // defined above
                CASE DbInfo.DBI_RM_SUPPORTED
                CASE DbInfo.DBI_RM_CREATE
                CASE DbInfo.DBI_RM_REMOVE
                CASE DbInfo.DBI_RM_CLEAR 
                CASE DbInfo.DBI_RM_FILL  
                CASE DbInfo.DBI_RM_ADD   
                CASE DbInfo.DBI_RM_DROP  
                CASE DbInfo.DBI_RM_TEST  
                CASE DbInfo.DBI_RM_COUNT 
                CASE DbInfo.DBI_RM_HANDLE
                   RETURN FALSE
				OTHERWISE
					oResult := SUPER:Info(nOrdinal, oNewValue)
			END SWITCH
			RETURN oResult
			
			
			
			
		/// <inheritdoc />
		VIRTUAL METHOD RecInfo(nOrdinal AS LONG, oRecID AS OBJECT, oNewValue AS OBJECT) AS OBJECT
			LOCAL nNewRec := 0 AS LONG
            LOCAL oResult AS OBJECT
			LOCAL nOld := 0 AS LONG
            
			IF ( oRecID != NULL )
				TRY
					nNewRec := Convert.ToInt32( oRecID )
				CATCH ex AS exception
					nNewRec := SELF:Recno
				    SELF:_dbfError(ex, SubCodes.ERDD_DATATYPE, GenCode.EG_DATATYPE, "DBF.RecInfo")

    			END TRY
            ELSE
                nNewRec := SELF:Recno
			ENDIF
            // Some operations require the new record te be selected
          IF nNewRec != 0
              SWITCH nOrdinal
                CASE DBRI_DELETED
                CASE DBRI_ENCRYPTED
                CASE DBRI_RAWRECORD
                CASE DBRI_RAWMEMOS
                CASE DBRI_RAWDATA
                    nOld     := SELF:Recno
                    SELF:Goto(nNewRec)
                END SWITCH
            ENDIF
			SWITCH nOrdinal
				CASE DBRI_DELETED
                    oResult := SELF:Deleted
				CASE DBRI_LOCKED
					IF ( SELF:_Shared )
                        IF nNewRec == 0
                            nNewRec := SELF:RecNo
                        ENDIF
						oResult := SELF:_Locks:Contains( nNewRec )
					ELSE
						oResult := TRUE
					ENDIF
				CASE DBRI_RECNO
					oResult := SELF:RecNo
				CASE DBRI_RECSIZE
					oResult := SELF:_RecordLength
                CASE DBRI_BUFFPTR
                    SELF:_ReadRecord()
                    oResult := SELF:_RecordBuffer
				CASE DBRI_RAWRECORD
                    oResult := SELF:_Encoding:GetString(SELF:_RecordBuffer,0, SELF:_RecordLength)
                CASE DBRI_UPDATED
                    oResult := SELF:_Hot
                    IF oNewValue IS LOGIC
                        IF (LOGIC) oNewValue
                            SELF:_BufferValid := FALSE
                            SELF:_ReadRecord()
                        ENDIF
                    ENDIF
				CASE DBRI_RAWMEMOS
                CASE DBRI_RAWDATA
                    // RawData returns a string with the record + memos
                    // RawMemos returns just the memos
                    oResult := ""
				CASE DBRI_ENCRYPTED
                    oResult := FALSE
				OTHERWISE
					oResult := SUPER:Info(nOrdinal, oNewValue)
			END SWITCH
            IF nOld != 0
                SELF:Goto(nOld)
            ENDIF
			RETURN oResult
			
		METHOD Sort(info AS DbSortInfo) AS LOGIC
			LOCAL recordNumber AS LONG
			LOCAL trInfo AS DbTransInfo
			LOCAL hasWhile AS LOGIC
			LOCAL hasFor AS LOGIC
			LOCAL sort AS RddSortHelper
			LOCAL i AS DWORD
			LOCAL fieldPos AS LONG
			LOCAL isNum AS LONG
			LOCAL isOk AS LOGIC
			LOCAL isQualified AS LOGIC
			LOCAL readMore AS LOGIC
			LOCAL limit AS LOGIC
			LOCAL rec AS SortRecord
			LOCAL sc AS DbfSortCompare
			//
			recordNumber := 0
			trInfo := info:TransInfo
			trInfo:Scope:Compile(SELF)
			hasWhile := trInfo:Scope:WhileBlock != NULL
			hasFor   := trInfo:Scope:ForBlock != NULL
			sort := RddSortHelper{SELF, info, SELF:RecCount}
			// 
			i := 0
			WHILE i < info:Items:Length
				fieldPos := info:Items[i]:FieldNo
				isNum := 0
				IF (SELF:_Fields[fieldPos]:FieldType == DBFieldType.Number )
					isNum := 2
					info:Items[i]:Flags |= isNum
				ENDIF
				info:Items[i]:OffSet := SELF:_Fields[fieldPos]:OffSet
				info:Items[i]:Length := SELF:_Fields[fieldPos]:Length
				//Next Field
				i++
			ENDDO
			isOk := TRUE
			isQualified := TRUE
			readMore := TRUE
			limit := TRUE
			//
			//			IF ( SELF:_Relations:Count > 0)
			//				SELF:ForceRel()
			//			ENDIF
			//
			IF (trInfo:Scope:RecId != NULL)
				recordNumber := Convert.ToInt32(trInfo:Scope:RecId)
				isOk := SELF:GoTo(recordNumber)
				readMore := TRUE
				limit := TRUE
				recordNumber := 1
				ELSE
					IF (trInfo:Scope:NextCount != 0)
						limit := TRUE
						recordNumber := trInfo:Scope:NextCount
						IF (recordNumber < 1)
							readMore := FALSE
						ENDIF
					ELSE
						readMore := TRUE
						limit := FALSE
						IF ((trInfo:Scope:WhileBlock == NULL) .AND. (!trInfo:Scope:Rest))
							isOk := SELF:GoTop()
						ENDIF
				ENDIF
			ENDIF
			WHILE ( isOk .AND. !SELF:_Eof .AND. readMore)
				IF hasWhile
					readMore := (LOGIC) SELF:EvalBlock(trInfo:Scope:WhileBlock)
				ENDIF
				IF readMore .AND. hasFor
					isQualified := (LOGIC) SELF:EvalBlock(trInfo:Scope:ForBlock)
				ELSE
					isQualified := readMore
				ENDIF
				IF ((isOk) .AND. (isQualified))
					isOk := SELF:_readRecord()
					IF (isOk)
						rec := SortRecord{SELF:_RecordBuffer, SELF:_RecNo}
						isOk := sort:Add(rec)
					ENDIF
				ENDIF
				IF readMore .AND. limit
					readMore := (--recordNumber != 0)
				ENDIF
				IF isOk .AND. readMore
					isOk := SELF:Skip(1)
				ENDIF
			END WHILE
			IF isOk
				sc := DbfSortCompare{SELF, info}
				isOk := sort:Sort(sc)
			ENDIF
			IF isOk
				isOk := sort:Write(SELF)
			ENDIF
			RETURN isOk            
			
		// IRddSortWriter Interface, used by RddSortHelper
		PUBLIC METHOD WriteSorted( sortInfo AS DBSORTINFO , record AS SortRecord ) AS LOGIC
			Array.Copy(record:Data, SELF:_RecordBuffer, SELF:_RecordLength)
			RETURN SELF:TransRec(sortInfo:TransInfo)

        INTERNAL METHOD Validate() AS VOID
            IF !SELF:_BufferValid 
                SELF:_readRecord()
            ENDIF			
			// Properties
			//	PROPERTY Alias 		AS STRING GET
		/// <inheritdoc />
		PROPERTY BoF 		AS LOGIC
			GET 
				SELF:ForceRel()
				RETURN SELF:_Bof
			END GET
		END PROPERTY
		
		/// <inheritdoc />
		PROPERTY Deleted 	AS LOGIC 
			GET
				SELF:ForceRel()
				// Update ?
				SELF:_readRecord()
				RETURN SELF:_Deleted
			END GET
		END PROPERTY
		
		/// <inheritdoc />
		PROPERTY EoF 		AS LOGIC
			GET 
				SELF:ForceRel()
				RETURN SELF:_Eof
			END GET
		END PROPERTY
		
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
		PROPERTY Found		AS LOGIC
			GET 
				SELF:ForceRel()
				RETURN SELF:_Found
			END GET
		END PROPERTY
		
		/// <inheritdoc />
		PROPERTY RecCount	AS LONG
			GET
				IF SELF:Shared 
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
					reccount := ( fSize - SELF:_HeaderLength ) / SELF:_RecordLength
				ENDIF
			ENDIF
			RETURN reccount
			
		/// <inheritdoc />
		PROPERTY RecNo		AS INT
			GET
				SELF:ForceRel()
				RETURN SELF:_RecNo
			END GET
		END PROPERTY
		
		VIRTUAL PROPERTY Shared		AS LOGIC GET SELF:_Shared
		/// <inheritdoc />
		VIRTUAL PROPERTY SysName AS STRING GET "DBF"
		
			
		/// <summary>DBF Header.</summary>
		STRUCTURE DbfHeader
			// Fixed Buffer of 32 bytes
				// Matches the DBF layout
				// Read/Write to/from the Stream with the Buffer
			// and access individual values using the other fields
			
			PRIVATE CONST OFFSET_SIG			    := 0  AS BYTE
			PRIVATE CONST OFFSET_YEAR			:= 1  AS BYTE           // add 1900 so possible values are 1900 - 2155
			PRIVATE CONST OFFSET_MONTH	        := 2  AS BYTE
			PRIVATE CONST OFFSET_DAY             := 3  AS BYTE
			PRIVATE CONST OFFSET_RECCOUNT        := 4  AS BYTE
			PRIVATE CONST OFFSET_DATAOFFSET      := 8  AS BYTE
			PRIVATE CONST OFFSET_RECSIZE         := 10 AS BYTE
			PRIVATE CONST OFFSET_RESERVED1       := 12 AS BYTE
			PRIVATE CONST OFFSET_TRANSACTION     := 14 AS BYTE
			PRIVATE CONST OFFSET_ENCRYPTED       := 15 AS BYTE
			PRIVATE CONST OFFSET_DBASELAN        := 16 AS BYTE
			PRIVATE CONST OFFSET_MULTIUSER       := 20 AS BYTE
			PRIVATE CONST OFFSET_RESERVED2       := 24 AS BYTE
			PRIVATE CONST OFFSET_HASTAGS	     := 28 AS BYTE
			PRIVATE CONST OFFSET_CODEPAGE        := 29 AS BYTE
			PRIVATE CONST OFFSET_RESERVED3       := 30 AS BYTE
			INTERNAL CONST SIZE                  := 32 AS BYTE
			
			PUBLIC Buffer   AS BYTE[]
			// Hot ?  => Header has changed ?
			PUBLIC isHot	AS LOGIC
			
			PROPERTY Version    AS DBFVersion	;
			GET (DBFVersion) Buffer[OFFSET_SIG] ;
			SET Buffer[OFFSET_SIG] := (BYTE) VALUE

            // Date of last update; in YYMMDD format.  Each byte contains the number as a binary.
            // YY is added to a base of 1900 decimal to determine the actual year.
            // Therefore, YY has possible values from 0x00-0xFF, which allows for a range from 1900-2155.

			PROPERTY Year		AS LONG			
			    GET
                    LOCAL nYear AS LONG
                    nYear := DateTime.Now:Year
                    nYear := nYear - (nYear % 100)  // Get century
                    RETURN Buffer[OFFSET_YEAR] + nYear
                END GET
			    SET
                    Buffer[OFFSET_YEAR] := (BYTE) (VALUE% 100)
                    isHot := TRUE
                END SET
			END PROPERTY	
			PROPERTY Month		AS BYTE			;
			GET Buffer[OFFSET_MONTH]	;
			SET Buffer[OFFSET_MONTH] := VALUE, isHot := TRUE
					
			PROPERTY Day		AS BYTE			;
			GET Buffer[OFFSET_DAY]	;
			SET Buffer[OFFSET_DAY] := VALUE, isHot := TRUE
			// Number of records in the table. (Least significant byte first.)		
			PROPERTY RecCount	AS LONG			;
			GET BitConverter.ToInt32(Buffer, OFFSET_RECCOUNT) ;
			SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_RECCOUNT, SIZEOF(LONG)), isHot := TRUE
			// Number of bytes in the header. (Least significant byte first.)

			PROPERTY HeaderLen	AS SHORT		;
			GET BitConverter.ToInt16(Buffer, OFFSET_DATAOFFSET);
			SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_DATAOFFSET, SIZEOF(SHORT)), isHot := TRUE
					
			// Length of one data record, including deleted flag
			PROPERTY RecordLen	AS SHORT		;
			GET BitConverter.ToInt16(Buffer, OFFSET_RECSIZE);
			SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_RECSIZE, SIZEOF(SHORT)), isHot := TRUE

            // Reserved
			PROPERTY Reserved1	AS SHORT		;
			GET BitConverter.ToInt16(Buffer, OFFSET_RESERVED1);
			SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_RESERVED1, SIZEOF(SHORT)), isHot := TRUE

			// Flag indicating incomplete dBASE IV transaction.		
			PROPERTY Transaction AS BYTE		;
			GET Buffer[OFFSET_TRANSACTION];
			SET Buffer[OFFSET_TRANSACTION] := VALUE, isHot := TRUE

			// dBASE IV encryption flag.		
			PROPERTY Encrypted	AS BYTE			;
			GET Buffer[OFFSET_ENCRYPTED];
			SET Buffer[OFFSET_ENCRYPTED] := VALUE, isHot := TRUE
					
			PROPERTY DbaseLan	AS LONG			;
			GET BitConverter.ToInt32(Buffer, OFFSET_DBASELAN) ;
			SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_DBASELAN, SIZEOF(LONG)), isHot := TRUE
					
			PROPERTY MultiUser	AS LONG			;
			GET BitConverter.ToInt32(Buffer, OFFSET_MULTIUSER)	;
			SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_MULTIUSER, SIZEOF(LONG)), isHot := TRUE
					
			PROPERTY Reserved2	AS LONG			;
			GET BitConverter.ToInt32(Buffer, OFFSET_RESERVED2);
			SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_RESERVED2, SIZEOF(LONG))
					
			PROPERTY HasTags	AS DBFTableFlags ;
			GET (DBFTableFlags)Buffer[OFFSET_HASTAGS] ;
			SET Buffer[OFFSET_HASTAGS] := (BYTE) VALUE, isHot := TRUE
					
			PROPERTY CodePage	AS DbfHeaderCodepage			 ;
			GET (DbfHeaderCodepage) Buffer[OFFSET_CODEPAGE]  ;
			SET Buffer[OFFSET_CODEPAGE] := (BYTE) VALUE, isHot := TRUE
					
			PROPERTY Reserved3	AS SHORT         ;
			GET BitConverter.ToInt16(Buffer, OFFSET_RESERVED3);
			SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_RESERVED3, SIZEOF(SHORT)), isHot := TRUE

            // Note that the year property already does the 1900 offset calculation ! 
            PROPERTY LastUpdate AS DateTime      ;  
			    GET DateTime{Year, Month, Day} ;
			
					
			PROPERTY IsAnsi AS LOGIC GET CodePage:IsAnsi()
					
			METHOD initialize() AS VOID STRICT
				Buffer := BYTE[]{DbfHeader.SIZE}
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
				30 ? 31 	Reserved, contains 0x00
				32 ? n 	Field subrecords
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
				PRIVATE CONST OFFSET_NAME		   := 0    AS BYTE
				PRIVATE CONST OFFSET_TYPE		   := 11   AS BYTE
				PRIVATE CONST OFFSET_OFFSET	       := 12   AS BYTE
				PRIVATE CONST OFFSET_LEN          := 16   AS BYTE
				PRIVATE CONST OFFSET_DEC          := 17   AS BYTE
				PRIVATE CONST OFFSET_FLAGS        := 18   AS BYTE
				PRIVATE CONST OFFSET_COUNTER      := 19   AS BYTE
				PRIVATE CONST OFFSET_INCSTEP      := 23   AS BYTE
				PRIVATE CONST OFFSET_RESERVED1    := 24   AS BYTE
				PRIVATE CONST OFFSET_RESERVED2    := 25   AS BYTE
				PRIVATE CONST OFFSET_RESERVED3    := 26   AS BYTE
				PRIVATE CONST OFFSET_RESERVED4    := 27   AS BYTE
				PRIVATE CONST OFFSET_RESERVED5    := 28   AS BYTE
				PRIVATE CONST OFFSET_RESERVED6	   := 29  AS BYTE
				PRIVATE CONST OFFSET_RESERVED7    := 30   AS BYTE
				PRIVATE CONST OFFSET_HASTAG       := 31   AS BYTE
				INTERNAL CONST NAME_SIZE           := 11  AS BYTE
				INTERNAL CONST SIZE                := 32  AS BYTE
					
				// Fixed Buffer of 32 bytes
					// Matches the DBF layout
					// Read/Write to/from the Stream with the Buffer
				// and access individual values using the other fields
				METHOD initialize() AS VOID
					SELF:Buffer := BYTE[]{SIZE}
						
				PUBLIC Buffer		 AS BYTE[]
					
				PROPERTY Name		 AS STRING
				    GET
					    LOCAL fieldName := BYTE[]{DbfField.NAME_SIZE} AS BYTE[]
					    Array.Copy( Buffer, OFFSET_NAME, fieldName, 0, DbfField.NAME_SIZE )
					    LOCAL count := Array.FindIndex<BYTE>( fieldName, 0, { sz => sz == 0 } ) AS INT
					    IF count == -1
						    count := DbfField.NAME_SIZE
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
						Array.Clear( Buffer, OFFSET_NAME, DbfField.NAME_SIZE )
						System.Text.Encoding.ASCII:GetBytes( VALUE, 0, Math.Min(DbfField.NAME_SIZE,VALUE:Length), Buffer, OFFSET_NAME )
					END SET
				END PROPERTY
				
				PROPERTY Type		 AS DBFieldType ;
				GET (DBFieldType) Buffer[ OFFSET_TYPE ] ;
				SET Buffer[ OFFSET_TYPE ] := (BYTE) VALUE
				
				// Offset from record begin in FP
				PROPERTY Offset 	 AS LONG ;
				GET BitConverter.ToInt32(Buffer, OFFSET_OFFSET);
				SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_OFFSET, SIZEOF(LONG))
				
				PROPERTY Len		 AS BYTE;
				GET Buffer[OFFSET_Len]  ;
				SET Buffer[OFFSET_Len] := VALUE
				
				PROPERTY Dec		 AS BYTE;
				GET Buffer[OFFSET_Dec]  ;
				SET Buffer[OFFSET_Dec] := VALUE
				
				PROPERTY Flags		 AS DBFFieldFlags;
				GET (DBFFieldFlags)Buffer[OFFSET_Flags] ;
				SET Buffer[OFFSET_Flags] := (BYTE) VALUE
				
				PROPERTY Counter	 AS LONG;
				GET BitConverter.ToInt32(Buffer, OFFSET_Counter);
				SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_Counter, SIZEOF(LONG))
				
				PROPERTY IncStep	 AS BYTE;
				GET Buffer[OFFSET_IncStep]  ;
				SET Buffer[OFFSET_IncStep] :=  VALUE
				
				PROPERTY Reserved1   AS BYTE;
				GET Buffer[OFFSET_Reserved1]  ;
				SET Buffer[OFFSET_Reserved1] :=  VALUE
				
				PROPERTY Reserved2   AS BYTE;
				GET Buffer[OFFSET_Reserved2]  ;
				SET Buffer[OFFSET_Reserved2] := VALUE
				
				PROPERTY Reserved3   AS BYTE;
				GET Buffer[OFFSET_Reserved3]  ;
				SET Buffer[OFFSET_Reserved3] :=  VALUE
				
				PROPERTY Reserved4  AS BYTE;
				GET Buffer[OFFSET_Reserved4]  ;
				SET Buffer[OFFSET_Reserved4] :=  VALUE
				
				PROPERTY Reserved5   AS BYTE;
				GET Buffer[OFFSET_Reserved5]  ;
				SET Buffer[OFFSET_Reserved5] :=  VALUE
				
				PROPERTY Reserved6   AS BYTE;
				GET Buffer[OFFSET_Reserved6]  ;
				SET Buffer[OFFSET_Reserved6] :=  VALUE
				
				PROPERTY Reserved7   AS BYTE;
				GET Buffer[OFFSET_Reserved7]  ;
				SET Buffer[OFFSET_Reserved7] :=  VALUE
				
				PROPERTY HasTag		 AS BYTE;
				GET Buffer[OFFSET_HasTag]  ;
				SET Buffer[OFFSET_HasTag] :=  VALUE
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

		END CLASS
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
						SELF:Offset := 1000000000
						SELF:FileSize := 1000000000U
						SELF:RecordSize := 1U
						SELF:Direction := 1
					CASE DbfLockingModel.Clipper53
						SELF:Offset := 1000000000
						SELF:FileSize := 1000000000U
						SELF:RecordSize := 1U
						SELF:Direction := 1
					CASE DbfLockingModel.Clipper53Ext
						SELF:Offset := 4000000000
						SELF:FileSize := 294967295U
						SELF:RecordSize := 1U
						SELF:Direction := 1
					CASE DbfLockingModel.FoxPro
						SELF:Offset := 0x40000000U
						SELF:FileSize := 0x07ffffff
						SELF:RecordSize := 1U
						SELF:Direction := 2
					CASE DbfLockingModel.FoxProExt
						SELF:Offset := 0x7ffffffe
						SELF:FileSize := 0x3ffffffdU
						SELF:RecordSize := 1U
						SELF:Direction := -1
					CASE DbfLockingModel.Harbour64
						SELF:Offset := 0x7FFFFFFF00000001
						SELF:FileSize := 0x7ffffffeU
						SELF:RecordSize := 1U
						SELF:Direction := 1
					CASE DbfLockingModel.VOAnsi
						SELF:Offset     := 0x80000000
						SELF:FileSize   := 0x7fffffffU
						SELF:RecordSize := 1U
						SELF:Direction := 1
				END SWITCH
				
			END STRUCTURE
			
			
	
		
	INTERNAL CLASS DBFSortCompare IMPLEMENTS IComparer<SortRecord>
	
		PRIVATE _sortInfo AS DBSORTINFO
		PRIVATE _oRdd AS DBF
        PRIVATE _dataX AS BYTE[]
        PRIVATE _dataY AS BYTE[]
		
		INTERNAL CONSTRUCTOR( rdd AS DBF, info AS DBSORTINFO )
			SELF:_oRdd      := rdd
			SELF:_sortInfo  := info
            LOCAL max       := 0 AS INT
            FOREACH VAR item IN info:Items
                max := Math.Max(max, item:Length)
            NEXT
            SELF:_dataX := BYTE[]{ max}     
            SELF:_dataY := BYTE[]{ max}
			

		PUBLIC METHOD Compare(recordX AS SortRecord , recordY AS SortRecord ) AS LONG
			LOCAL dataBufferX AS BYTE[]
			LOCAL dataBufferY AS BYTE[]
			LOCAL diff AS LONG
			IF (recordX:Recno == recordY:Recno)
				RETURN 0
			ENDIF
			//
			dataBufferX := recordX:data
			dataBufferY := recordY:data
			diff := 0
            FOREACH VAR item IN SELF:_sortInfo:Items
				VAR start := item:OffSet
				VAR iLen := item:Length
				VAR flags := item:Flags
				// Long Value ?
				IF flags:HasFlag(DbSortFlags.Long)
					VAR longValue1 := BitConverter.ToInt32(dataBufferX, start)
					VAR longValue2 := BitConverter.ToInt32(dataBufferY, start)
					diff := longValue1 - longValue2
				ELSE
					// String Value ?
					IF flags:HasFlag(DbSortFlags.Ascii)
						// String ASCII : Use Runtime comparer
						Array.Copy(dataBufferX, start, _dataX, 0, iLen)
						Array.Copy(dataBufferY, start, _dataY, 0, iLen)
						diff := XSharp.RuntimeState.StringCompare(_dataX, _dataY, iLen)
						//
                    ELSE
						FOR VAR i := 0 TO iLen
							diff := dataBufferX[i + start] - dataBufferY[i + start]
							IF diff != 0
								EXIT
							ENDIF
						NEXT
					ENDIF
				ENDIF
                IF diff != 0
                    IF flags:HasFlag(DbSortFlags.Descending)
					    diff *= -1
				    ENDIF
                    EXIT
                ENDIF
			NEXT
			IF diff == 0
				diff := recordX:Recno - recordY:Recno
			ENDIF
			RETURN diff




	END CLASS
	
	
END NAMESPACE



