//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Please note that this code code expects zero based arrays
#pragma options ("az", ON)
USING System.Runtime.InteropServices
USING System.IO
USING System.Text
USING System.Linq
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Globalization
USING System.Collections.Generic
USING System.Diagnostics

#define INPUTBUFFER

BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBF RDD. Usually not used 'stand alone'</summary>
[DebuggerDisplay("DBF ({Alias,nq})")];
PARTIAL CLASS DBF INHERIT Workarea IMPLEMENTS IRddSortWriter
#region STATIC properties and fields
    STATIC PROTECT _Extension := ".DBF" AS STRING
    STATIC PRIVATE  culture := System.Globalization.CultureInfo.InvariantCulture AS CultureInfo

#endregion
    PROTECT _oStream        AS FileStream
	PROTECT _RelInfoPending AS DbRelInfo

	PROTECT _Header			AS DbfHeader
	PROTECT _HeaderLength	AS LONG  	// Size of header
	PROTECT _BufferValid	AS LOGIC	// Current Record is Valid
	PROTECT _BlankBuffer    AS BYTE[]
	INTERNAL _isValid        AS LOGIC    // Current Position is Valid
	PROTECT _HasMemo		AS LOGIC
	PROTECT _wasChanged     AS LOGIC

    //PROTECT _HasTags		AS LOGIC
    //PROTECT _HasAutoInc		AS LOGIC
    //PROTECT _HasTimeStamp	AS LOGIC
    //PROTECT _LastUpdate	    AS DateTime
	PROTECT _RecCount		AS LONG
	PROTECT _RecNo			AS LONG
    //PROTECT _Temporary		AS LOGIC
	PROTECT _RecordChanged	AS LOGIC 	// Current record has changed ?
	PROTECT _Positioned		AS LOGIC 	//
    //PROTECT _Appended		AS LOGIC	// Record has been added ?
    PROTECT _Deleted		AS LOGIC	// Record has been deleted ?
    //PROTECT _HeaderDirty	AS LOGIC	// Header is dirty ?
    PROTECT _fLocked		AS LOGIC    // File Locked ?
    PROTECT _HeaderLocked	AS LOGIC
    //PROTECT _PackMemo		AS LOGIC
    INTERNAL _OpenInfo		AS DbOpenInfo // current dbOpenInfo structure in OPEN/CREATE method
    PROTECT _Locks			AS List<LONG>
    PROTECT _AllowedFieldTypes AS STRING
    //PROTECT _DirtyRead		AS LONG
    //PROTECT _HasTrigger		AS LOGIC
    //PROTECT _Encrypted		AS LOGIC	// Current record Encrypted
    //PROTECT _TableEncrypted 	AS LOGIC	// Whole table encrypted
    //PROTECT _CryptKey		AS STRING
    //PROTRECT _Trigger		as DbTriggerDelegate
	PROTECT _oIndex			AS BaseIndex
	PROTECT _Hot            AS LOGIC
	PROTECT _lockScheme     AS DbfLocking
	PROTECT _NewRecord      AS LOGIC
    PROTECT INTERNAL _NullColumn    AS DbfNullColumn            // Column definition for _NullFlags, used in DBFVFP driver
    PROTECT INTERNAL _NullCount      := 0 AS LONG   // to count the NULL and Length bits for DBFVFP

    PROTECT INTERNAL PROPERTY FullPath AS STRING GET _FileName
    PROTECT INTERNAL PROPERTY Header AS DbfHeader GET _Header
    PROTECT INTERNAL _Ansi          AS LOGIC
    PROTECT INTERNAL _Encoding      AS Encoding
    PROTECT INTERNAL _numformat AS NumberFormatInfo
    PROTECT PROPERTY IsOpen AS LOGIC GET SELF:_hFile != F_ERROR
    PROTECT PROPERTY HasMemo AS LOGIC GET SELF:_HasMemo
    NEW PROTECT PROPERTY Memo AS BaseMemo GET (BaseMemo) SELF:_Memo
    INTERNAL PROPERTY Stream AS FileStream GET SELF:_oStream

#ifdef INPUTBUFFER
    INTERNAL _inBuffer AS InputBuffer
#endif

/*
PROTECTED METHOD ConvertToMemory() AS LOGIC
     IF !SELF:_OpenInfo:Shared
        FConvertToMemoryStream(SELF:_hFile)
        IF SELF:_Memo IS DBTMemo VAR dbtmemo
            FConvertToMemoryStream(dbtmemo:_hFile)
        ELSEIF SELF:_Memo IS FPTMemo VAR fptmemo
            FConvertToMemoryStream(fptmemo:_hFile)
        ENDIF
        RETURN TRUE
     ENDIF
     RETURN FALSE
*/
INTERNAL METHOD _CheckEofBof() AS VOID
    IF SELF:RecCount == 0
        SELF:_SetEOF(TRUE)
        SELF:_SetBOF(TRUE)
    ELSEIF SELF:RecNo > SELF:RecCount
        SELF:_SetEOF(TRUE)
    ENDIF


INTERNAL METHOD _SetBOF(lNewValue AS LOGIC) AS VOID
    IF lNewValue != SELF:BoF
        SELF:BoF := lNewValue
    ENDIF


INTERNAL METHOD _SetEOF(lNewValue AS LOGIC) AS VOID
    IF lNewValue != SELF:EoF
        SELF:EoF := lNewValue
        IF lNewValue
            Array.Copy(SELF:_BlankBuffer, SELF:_RecordBuffer, SELF:_RecordLength)
            SELF:_RecNo := SELF:_RecCount+1
        ENDIF
    ENDIF

PRIVATE METHOD _AllocateBuffers() AS VOID
	SELF:_RecordBuffer  := BYTE[]{ SELF:_RecordLength}
	SELF:_BlankBuffer   := BYTE[]{ SELF:_RecordLength}
	FOR VAR  i := 0 TO SELF:_RecordLength - 1
		SELF:_BlankBuffer[i] := 0x20 // space
	NEXT
	FOREACH oFld AS RddFieldInfo IN _Fields
		IF oFld IS DbfColumn VAR column
			column:InitValue(SELF:_BlankBuffer)
		ENDIF
	NEXT
#ifdef INPUTBUFFER
    IF (_inBuffer != NULL)
        _inBuffer:Close()
    ENDIF
	_inBuffer := InputBuffer{SELF:_oStream, SELF:_HeaderLength, SELF:_RecordLength, SELF:Shared}
#endif

CONSTRUCTOR()
	SELF:_hFile     := F_ERROR
	SELF:_oStream   := NULL
	SELF:_Header    := DbfHeader{SELF}
	SELF:_Locks     := List<LONG>{}
	SELF:_numformat := (NumberFormatInfo) culture:NumberFormat:Clone()
	SELF:_numformat:NumberDecimalSeparator := "."
	SELF:_RelInfoPending    := NULL
	SELF:_AllowedFieldTypes := "CDLMN"

            /// <inheritdoc />
OVERRIDE METHOD GoTop() AS LOGIC
	IF SELF:IsOpen
		BEGIN LOCK SELF
			SELF:GoTo( 1 )
			SELF:_Top := TRUE
			SELF:_Bottom := FALSE
			SELF:_BufferValid := FALSE
                // Apply Filter and SetDeleted
			VAR result := SELF:SkipFilter(1)
            SELF:_CheckEofBof()
            RETURN result
        END LOCK
	ENDIF
RETURN FALSE

    /// <inheritdoc />
OVERRIDE METHOD GoBottom() AS LOGIC
	IF SELF:IsOpen
		BEGIN LOCK SELF
			SELF:GoTo( SELF:RecCount )
			SELF:_Top := FALSE
			SELF:_Bottom := TRUE
			SELF:_BufferValid := FALSE
            // Apply Filter and SetDeleted
			VAR result := SELF:SkipFilter(-1)
            SELF:_CheckEofBof()
            RETURN result
        END LOCK
	ENDIF
RETURN FALSE

/// <inheritdoc />
OVERRIDE METHOD GoTo(nRec AS LONG) AS LOGIC
	IF SELF:IsOpen
		BEGIN LOCK SELF
        // Validate any pending change
			SELF:GoCold()
            // On Shared env, it can be correct to guess that some changes have been made
			IF SELF:Shared .AND. nRec > SELF:_RecCount
				SELF:_RecCount := SELF:_calculateRecCount()
			ENDIF
            LOCAL nCount := SELF:_RecCount AS LONG
            // Normal positioning, VO resets FOUND to FALSE after a recprd movement
            SELF:_Found := FALSE
      	    SELF:_BufferValid := FALSE
			IF  nRec <= nCount  .AND.  nRec > 0
				SELF:_RecNo := nRec
				SELF:_SetEOF(FALSE)
                SELF:_SetBOF(FALSE)
				SELF:_isValid := TRUE
			ELSEIF nRec < 0 .AND. nCount > 0
                // skip to BOF. Move to record 1.
				SELF:_RecNo := 1
                //? SELF:CurrentThreadId, "Set Recno to ", 1, "nRec", nRec, "nCount", nCount
				SELF:_SetEOF(FALSE)
                SELF:_SetBOF(TRUE)
				SELF:_isValid := FALSE
			ELSE
                // File empty, or move after last record
                //? SELF:CurrentThreadId, "Set Recno to nCount+1", nCount+1
				SELF:_RecNo := nCount + 1
				SELF:_SetEOF(TRUE)
                SELF:_SetBOF(nCount == 0)
				SELF:_isValid := FALSE
            ENDIF
            IF SELF:_Relations:Count != 0
                SELF:SyncChildren()
            ENDIF
            SELF:_CheckEofBof()
			RETURN TRUE
		END LOCK
    ENDIF
RETURN FALSE

/// <inheritdoc />
OVERRIDE METHOD GoToId(oRec AS OBJECT) AS LOGIC
	LOCAL result AS LOGIC
	BEGIN LOCK SELF
		TRY
			VAR nRec := Convert.ToInt32( oRec )
			result := SELF:GoTo( nRec )
		CATCH ex AS Exception
			SELF:_dbfError(ex, Subcodes.EDB_GOTO,Gencode.EG_DATATYPE,  "DBF.GoToId",FALSE)
			result := FALSE
		END TRY
    END LOCK
    SELF:_CheckEofBof()
RETURN result

/// <inheritdoc />
OVERRIDE METHOD SetFilter(info AS DbFilterInfo) AS LOGIC
	SELF:ForceRel()
RETURN SUPER:SetFilter(info)

    /// <inheritdoc />
OVERRIDE METHOD Skip(nToSkip AS INT) AS LOGIC
	LOCAL result := FALSE AS LOGIC
	SELF:ForceRel()
	IF SELF:IsOpen
        IF nToSkip != 0 .OR. !( SELF:_fLocked .OR. SELF:_Locks:Contains( SELF:RecNo ) ) .OR. !SELF:_BufferValid
		    SELF:_Top := FALSE
		    SELF:_Bottom := FALSE
        ENDIF
		LOCAL delState := XSharp.RuntimeState.Deleted AS LOGIC
        //
		IF nToSkip == 0  .OR. delState .OR. ( SELF:_FilterInfo != NULL .AND. SELF:_FilterInfo:Active )
            //
			result := SUPER:Skip( nToSkip )
		ELSE
			result := SELF:SkipRaw( nToSkip )
            // We reached the top ?
			IF result
                IF ( nToSkip < 0 ) .AND. SELF:_BoF
				    SELF:GoTop()
				    SELF:BoF := TRUE
                ENDIF
                // when we land at EOF then do not reset the EOF flag
			    IF nToSkip < 0 .and. SELF:RecNo < SELF:RecCount
				    SELF:_SetEOF(FALSE)
			    ELSEIF nToSkip > 0
				    SELF:BoF := FALSE
                ENDIF
             ENDIF
        ENDIF
	ENDIF
    SELF:_CheckEofBof()
RETURN result


    /// <inheritdoc />
OVERRIDE METHOD SkipRaw(nToSkip AS INT) AS LOGIC
	LOCAL isOK := TRUE AS LOGIC
    LOCAL nNewRec AS INT
    //
	IF nToSkip == 0
        // Refresh current Recno
		LOCAL currentBof := SELF:BoF AS LOGIC
		LOCAL currentEof := SELF:EoF AS LOGIC
		SELF:GoTo( SELF:_RecNo )
		SELF:_SetBOF(currentBof)
        SELF:_SetEOF(currentEof)
	ELSE
        nNewRec := SELF:_RecNo + nToSkip
        IF nNewRec != 0
		    isOK := SELF:GoTo( SELF:_RecNo + nToSkip )
        ELSE
            isOK := SELF:GoTo( 1 )
            SELF:_SetBOF(TRUE)
        ENDIF
    ENDIF
    SELF:_CheckEofBof()
RETURN isOK


    // Append and Delete
/// <inheritdoc />
OVERRIDE METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
	LOCAL isOK := FALSE AS LOGIC
	IF SELF:IsOpen
		BEGIN LOCK SELF
        // Validate
			isOK := SELF:GoCold()
			IF isOK
            //
				IF SELF:_ReadOnly
                // Error !! Cannot be written !
					SELF:_dbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY ,FALSE)
					isOK := FALSE
				ENDIF
				IF  SELF:Shared
					IF  SELF:_Locks:Count > 0  .AND. lReleaseLock
						SELF:UnLock( 0 ) // Unlock All Records
					ENDIF
					isOK := SELF:AppendLock( DbLockMode.Lock )  // Locks Header and then future new record. Sets _HeaderLocked to TRUE
                ELSE
					SELF:_HeaderLocked := FALSE
				ENDIF
				IF isOK
                    VAR nCount := SELF:_calculateRecCount()+1
					SELF:_UpdateRecCount(nCount)      // writes the reccount to the header as well
					SELF:_RecNo         := nCount
					Array.Copy(SELF:_BlankBuffer, SELF:_RecordBuffer, SELF:_RecordLength)
					FOREACH oFld AS RddFieldInfo IN _Fields
						IF oFld IS DbfColumn VAR column
							column:NewRecord(SELF:_RecordBuffer)
						ENDIF
					NEXT
                    SELF:_writeRecord()
                    IF SELF:_Shared
                        SELF:_putEndOfFileMarker()
                    ENDIF
                    // Now, update state
					SELF:_SetEOF(FALSE)
					SELF:BoF            := FALSE
					SELF:_Deleted       := FALSE
					SELF:_BufferValid   := TRUE
					SELF:_isValid       := TRUE
					SELF:_NewRecord     := TRUE
                    SELF:_wasChanged    := TRUE
                    // Mark RecordBuffer as Hot
					SELF:_Hot           := TRUE
                    // Now, Save
					IF SELF:_HeaderLocked
						SELF:GoCold()
						isOK := SELF:AppendLock( DbLockMode.UnLock ) // unlocks just header
					ENDIF
				ENDIF
			ENDIF
		END LOCK
	ENDIF
//
RETURN isOK

PRIVATE METHOD _UpdateRecCount(nCount AS LONG) AS LOGIC
	SELF:_RecCount          := nCount
    SELF:_Header:RecCount   := nCount
    SELF:_wasChanged        := TRUE
    SELF:_writeHeader()
RETURN TRUE


/// <inheritdoc />
OVERRIDE METHOD AppendLock( lockMode AS DbLockMode ) AS LOGIC
	LOCAL isOK := FALSE AS LOGIC
	BEGIN LOCK SELF
		IF lockMode == DbLockMode.Lock
            // Lock the "future" record (Recno+1) and the Header
			isOK := SELF:HeaderLock( lockMode )
			IF isOK
                VAR newRecno := SELF:_calculateRecCount() +1
				IF !SELF:_Locks:Contains( newRecno ) .AND. !SELF:_fLocked
					isOK := SELF:_lockRecord( newRecno )
				ENDIF
				IF !isOK
                    // when we fail to lock the record then also unlock the header
					IF SELF:_HeaderLocked
                        //? CurrentThreadId," Failed to lock the new record", newRecno, "Unlock the header"
						SELF:HeaderLock( DbLockMode.UnLock )
                    ELSE
                        NOP
                        //? CurrentThreadId," Failed to lock the new record", newRecno, "but header was not locked ?"
					ENDIF
					SELF:_dbfError( ERDD.APPENDLOCK, XSharp.Gencode.EG_APPENDLOCK )
                ELSE
                    SELF:_RecNo := newRecno
                    //? CurrentThreadId, "Appended and Locked record ", newRecno, "Recno = ", SELF:RecNo
                ENDIF
            ELSE
                NOP
                //? CurrentThreadId,"AppendLock failed, header can't be locked"
			ENDIF
		ELSE
            // Unlock the Header, new record remains locked
			isOK := SELF:HeaderLock( lockMode )
		ENDIF
	END LOCK
    //
RETURN isOK

    // LockMethod.File      : Unlock all records and Lock the File
    // LockMethod.Exclusive : Unlock all records and lock the indicated record
    // LockMethod.Multiple  : Loc the indicated record
    /// <inheritdoc />
OVERRIDE METHOD Lock( lockInfo REF DbLockInfo ) AS LOGIC
	LOCAL isOK AS LOGIC
	SELF:ForceRel()
	BEGIN LOCK SELF
		IF lockInfo:@@Method == DbLockInfo.LockMethod.Exclusive  .OR. ;
            lockInfo:@@Method == DbLockInfo.LockMethod.Multiple
			isOK := SELF:_lockRecord( REF lockInfo )
		ELSEIF lockInfo:@@Method == DbLockInfo.LockMethod.File
			isOK := SELF:_lockDBFFile( )
		ELSE
			isOK := TRUE
        ENDIF
        lockInfo:Result := isOK
	END LOCK
RETURN isOK

    // Place a lock on the Header. The "real" offset locked depends on the Lock Scheme, defined by the DBF Type
    /// <inheritdoc />
OVERRIDE METHOD HeaderLock( lockMode AS DbLockMode ) AS LOGIC
    //
	IF lockMode == DbLockMode.Lock
        //? CurrentThreadId, "Start Header Lock", ProcName(1)
        LOCAL nTries := 0 AS LONG
        VAR timer := LockTimer{}
        timer:Start()
        DO WHILE TRUE
            ++nTries
		    SELF:_HeaderLocked := SELF:_tryLock( SELF:_lockScheme:Offset, 1,FALSE)
            IF ! SELF:_HeaderLocked
                IF timer:TimeOut(SELF:FullPath, SELF:_lockScheme:Offset, 1)
                    RETURN FALSE
                ENDIF
                System.Threading.Thread.Sleep(1)

            ELSE
                EXIT
            ENDIF
        ENDDO
        //? CurrentThreadId, "Header Lock", SELF:_HeaderLocked , "tries", nTries
        RETURN SELF:_HeaderLocked
    ELSE

		TRY
            //? CurrentThreadId, "Start Header UnLock",ProcName(1)
            _oStream:SafeUnlock(SELF:_lockScheme:Offset, 1)
            SELF:_HeaderLocked := FALSE
            //? CurrentThreadId, "Header UnLock", unlocked
		CATCH ex AS Exception
			SELF:_HeaderLocked := FALSE
			SELF:_dbfError(ex, Subcodes.ERDD_WRITE_UNLOCK,Gencode.EG_LOCK_ERROR,  "DBF.HeaderLock",FALSE)
            RETURN FALSE
        END TRY
        RETURN TRUE
	ENDIF
    //


    // Unlock a indicated record number. If 0, Unlock ALL records
    // Then unlock the File if needed
/// <inheritdoc />
OVERRIDE METHOD UnLock(oRecId AS OBJECT) AS LOGIC
	LOCAL recordNbr AS LONG
	LOCAL isOK AS LOGIC
    //
	IF SELF:Shared
		BEGIN LOCK SELF
            //? CurrentThreadId, "UnLock", oRecId
			SELF:GoCold()
			TRY
				recordNbr := Convert.ToInt32( oRecId )
			CATCH ex AS Exception
				recordNbr := 0
				SELF:_dbfError(ex, Subcodes.ERDD_DATATYPE,Gencode.EG_LOCK_ERROR,  "DBF.UnLock",FALSE)
                RETURN FALSE
			END TRY
        //
			isOK := TRUE
            IF recordNbr != 0
                NOP
            ENDIF
			IF SELF:_Locks:Count > 0
				IF recordNbr == 0
                // Create a copy with ToArray() because _unlockRecord modifies the collection
					FOREACH VAR nbr IN SELF:_Locks:ToArray()
						isOK := isOK .AND. SELF:_unlockRecord( nbr )
					NEXT
					SELF:_Locks:Clear()  // Should be useless as the record is removed from the list in _unlockRecord
				ELSE
					isOK := SELF:_unlockRecord( recordNbr )
				ENDIF
			ENDIF
			IF SELF:_fLocked  .AND. recordNbr == 0
				isOK := SELF:_unlockFile( )
				IF isOK
					SELF:_fLocked := FALSE
				ENDIF
            ENDIF
		END LOCK
    ELSE
        //? CurrentThreadId, "UnLock nothing to do because not shared"
		isOK := TRUE
	ENDIF
RETURN isOK

    // Unlock file. The Offset depends on the LockScheme
PROTECT METHOD _unlockFile( ) AS LOGIC
	LOCAL unlocked AS LOGIC
    //
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF

	unlocked := _oStream:SafeUnlock(SELF:_lockScheme:FileLockOffSet, SELF:_lockScheme:FileSize)
	IF ! unlocked
		SELF:_dbfError(FException(), Subcodes.ERDD_WRITE_UNLOCK,Gencode.EG_LOCK_ERROR,  "DBF._unlockFile", FALSE)
	ENDIF
RETURN unlocked

PROTECTED PROPERTY CurrentThreadId AS STRING GET System.Threading.Thread.CurrentThread:ManagedThreadId:ToString()

    // Unlock a record. The Offset depends on the LockScheme
PROTECT METHOD _unlockRecord( recordNbr AS LONG ) AS LOGIC
	LOCAL unlocked AS LOGIC
	LOCAL iOffset AS INT64
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF
	iOffset := SELF:_lockScheme:RecnoOffSet(recordNbr, SELF:_RecordLength , SELF:_HeaderLength )
	unlocked :=  _oStream:SafeUnlock(iOffset, SELF:_lockScheme:RecordSize)
	IF ! unlocked
		SELF:_dbfError(FException(), Subcodes.ERDD_WRITE_UNLOCK,Gencode.EG_LOCK_ERROR,  "DBF._unlockRecord", FALSE)
	ENDIF
	IF( unlocked )
		SELF:_Locks:Remove( recordNbr )
	ENDIF
RETURN unlocked

    // Lock the file. The Offset depends on the LockScheme
PROTECT METHOD _lockFile( ) AS LOGIC
	LOCAL locked AS LOGIC
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF

	locked := _oStream:SafeLock(SELF:_lockScheme:FileLockOffSet, SELF:_lockScheme:FileSize )
	IF ! locked
		SELF:_dbfError(FException(), Subcodes.ERDD_WRITE_LOCK,Gencode.EG_LOCK_ERROR,  "DBF._lockFile", FALSE)
	ENDIF
RETURN locked

    // Place a lock : <nOffset> indicate where the lock should be; <nLong> indicate the number bytes to lock
    // If it fails, the operation is tried <nTries> times, waiting 1ms between each operation.
    // Return the result of the operation
PROTECTED METHOD _tryLock( nOffset AS INT64, nLong AS INT64, lGenError AS LOGIC) AS LOGIC
	LOCAL locked AS LOGIC
    LOCAL nTries AS DWORD
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF
	LOCAL lockEx := NULL AS Exception
	nTries := XSharp.RuntimeState.LockTries
	REPEAT
		locked := _oStream:SafeLock(nOffset, nLong)
		IF !locked
            LOCAL nError := FError() AS DWORD
            IF nError != 33     // Someone else has locked the file
                EXIT
            ENDIF
			nTries --
            //DebOut32(ProcName(1)+" Lock Failed "+nTries:ToString()+" tries left, offset: "+ nOffSet:ToString()+" length: "+nLong:ToString())
			IF nTries > 0
				System.Threading.Thread.Sleep( 1)
            ENDIF
		ENDIF
	UNTIL ( locked .OR. (nTries==0) )
    IF (! locked .AND. lGenError)
		 SELF:_dbfError(lockEx, Subcodes.ERDD_WRITE_LOCK,Gencode.EG_LOCK_ERROR,  "DBF._tryLock", FALSE)
    ENDIF

RETURN locked

    // Lock the DBF File : All records are first unlocked, then the File is locked
PROTECTED METHOD _lockDBFFile() AS LOGIC
	LOCAL isOK := TRUE AS LOGIC
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF

	IF SELF:Shared .AND. !SELF:_fLocked
        //
		SELF:GoCold()
		IF SELF:_Locks:Count > 0
            // create a copy of the collection by calling ToArray to avoid a runtime error
            // because the collection will be changed by the call to _unlockRecord()
			FOREACH VAR nbr IN SELF:_Locks:ToArray()
				SELF:_unlockRecord( nbr )
			NEXT
			SELF:_Locks:Clear()
		ENDIF
		SELF:_fLocked := SELF:_lockFile()
#ifdef INPUTBUFFER
		IF _inBuffer != NULL_OBJECT
			_inBuffer:Invalidate()
		ENDIF
#endif
        // Invalidate Buffer
		SELF:GoTo( SELF:RecNo )
		isOK := SELF:_fLocked
	ENDIF
RETURN isOK

    // Lock a record number. The Offset depends on the LockScheme
PROTECTED METHOD _lockRecord( recordNbr AS LONG ) AS LOGIC
	LOCAL locked AS LOGIC
	LOCAL iOffset AS INT64
    //
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF

    iOffset := SELF:_lockScheme:RecnoOffSet(recordNbr, SELF:_RecordLength , SELF:_HeaderLength )
    locked := SELF:_tryLock(iOffset, SELF:_lockScheme:RecordSize ,FALSE)
	IF locked
		SELF:_Locks:Add( recordNbr )
        //? CurrentThreadId, "Locked record ", recordNbr
    ELSE
        //? CurrentThreadId, "Failed to Lock record ", recordNbr
		SELF:_dbfError( Subcodes.ERDD_WRITE_LOCK,Gencode.EG_LOCK_ERROR,  "DBF._lockRecord", FALSE)
	ENDIF
RETURN locked


    // LockMethod.Exclusive : Unlock all records and lock the indicated record
    // LockMethod.Multiple  : Loc the indicated record
PROTECTED METHOD _lockRecord( lockInfo REF DbLockInfo ) AS LOGIC
	LOCAL nToLock := 0 AS UINT64
	LOCAL isOK AS LOGIC
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF

	isOK := TRUE
	IF lockInfo:RecId == NULL
		nToLock := (UINT64)SELF:RecNo
	ELSE
		TRY
			nToLock := Convert.ToUInt64( lockInfo:RecId )
        CATCH ex AS Exception
			SELF:_dbfError( ex, ERDD.DATATYPE, XSharp.Gencode.EG_DATATYPE, FALSE)
			isOK := FALSE
		END TRY
		IF isOK
			IF nToLock > SELF:RecCount  .OR. nToLock < 1
				isOK := FALSE
			ENDIF
		ENDIF
	ENDIF
    //
	IF isOK
        // Already locked ?
		IF SELF:Shared .AND. !SELF:_Locks:Contains( (LONG)nToLock )
            IF lockInfo:Method == DbLockInfo.LockMethod.Multiple
                // Just add the lock to the list
				isOK := SELF:_lockRecord( (LONG)nToLock )
			ELSE // DbLockInfo.LockMethod.Exclusive
                // Release the locks
				SELF:UnLock(0)
                // Now, lock the one
				isOK := SELF:_lockRecord( (LONG)nToLock )
                // Go to there
				SELF:GoTo( (LONG)nToLock )
			ENDIF
		ENDIF
	ENDIF
    //
	lockInfo:Result := isOK
RETURN isOK


    // Un Delete the curretn Record
/// <inheritdoc />
OVERRIDE METHOD Recall() AS LOGIC
	LOCAL isOK AS LOGIC
	SELF:ForceRel()
	isOK := SELF:_readRecord()
	IF isOK
		IF ! SELF:_Hot
			SELF:GoHot()
		ENDIF
		SELF:_RecordBuffer[ 0 ] := (BYTE)' '
		SELF:_Deleted := FALSE
	ELSE
		SELF:_dbfError( ERDD.READ, XSharp.Gencode.EG_READ )
	ENDIF
RETURN isOK

    // Mark the current record as DELETED
/// <inheritdoc />
OVERRIDE METHOD Delete() AS LOGIC
	LOCAL isOK AS LOGIC
	SELF:ForceRel()
	BEGIN LOCK SELF
		isOK := SELF:_readRecord()
		IF isOK
			IF SELF:_isValid
				IF ! SELF:_Hot
					SELF:GoHot()
				ENDIF
				SELF:_RecordBuffer[ 0 ] := (BYTE)'*'
				SELF:_Deleted := TRUE
            //
			ENDIF
		ELSE
        // VO does not report an error when deleting on an invalid record
			isOK := TRUE // SELF_DbfError( ERDD.READ, XSharp.Gencode.EG_READ )
		ENDIF
	END LOCK
RETURN isOK

    // Retrieve the raw content of a record
/// <inheritdoc />
OVERRIDE METHOD GetRec() AS BYTE[]
	LOCAL records := NULL AS BYTE[]
	SELF:ForceRel()
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
/// <inheritdoc />
OVERRIDE METHOD PutRec(aRec AS BYTE[]) AS LOGIC
	LOCAL isOK := FALSE AS LOGIC
    // First, Check the Size
	IF aRec:Length == SELF:_RecordLength
		IF SELF:_readRecord()
			IF ! SELF:_Hot
				isOK := SELF:GoHot()
			ELSE
				isOK := TRUE
			ENDIF
			Array.Copy(aRec, SELF:_RecordBuffer, SELF:_RecordLength)
		ENDIF
	ELSE
		SELF:_dbfError( ERDD.DATAWIDTH, XSharp.Gencode.EG_DATAWIDTH )
	ENDIF
RETURN isOK

    // Suppress all DELETED record
/// <inheritdoc />
OVERRIDE METHOD Pack() AS LOGIC
	LOCAL isOK AS LOGIC
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF

	IF SELF:_ReadOnly
        // Error !! Cannot be written !
		SELF:_dbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
		RETURN FALSE
	ENDIF
    //
	IF SELF:Shared
        // Error !! Cannot be written !
		SELF:_dbfError( ERDD.SHARED, XSharp.Gencode.EG_SHARED )
		RETURN FALSE
	ENDIF
    //
	isOK := SELF:GoCold()
	IF isOK
		LOCAL nToRead AS LONG
		LOCAL nMoveTo AS LONG
		LOCAL nTotal AS LONG
		LOCAL lDeleted AS LOGIC
        //
		nToRead := 1
		nMoveTo := 1
		nTotal := 0
		WHILE nToRead <= SELF:RecCount
            // Move
			SELF:GoTo( nToRead )
            // and get Data
			SELF:_readRecord()
			lDeleted := SELF:_Deleted
            //
			IF !lDeleted
				nTotal++
				IF nToRead != nMoveTo
					SELF:_RecNo := nMoveTo
					SELF:_writeRecord()
				ENDIF
			ENDIF
            // Next
			nToRead ++
			IF !lDeleted
				nMoveTo++
			ENDIF
		ENDDO
        //
		SELF:_Hot := FALSE
		SELF:_UpdateRecCount(nTotal) // writes the reccount to the header as well
		SELF:Flush()
        SELF:_CheckEofBof()
        //
	ENDIF
RETURN isOK


    // Remove all records
/// <inheritdoc />
OVERRIDE METHOD Zap() AS LOGIC
	LOCAL isOK AS LOGIC
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF

	IF SELF:_ReadOnly
		SELF:_dbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
		RETURN FALSE
	ENDIF
	IF SELF:_Shared
		SELF:_dbfError( ERDD.SHARED, XSharp.Gencode.EG_SHARED )
		RETURN FALSE
	ENDIF
    //
	isOK := SELF:GoCold()
	IF isOK
		SELF:GoTo(0)
        // Zap means, set the RecCount to zero, so any other write with overwrite datas
		SELF:_UpdateRecCount(0) // writes the reccount to the header as well
		SELF:Flush()
        // Memo File ?
		IF SELF:_HasMemo
            // Zap Memo
			IF SELF:HasMemo
				RETURN _Memo:Zap()
			ELSE
				RETURN SUPER:Zap()
			ENDIF
		ENDIF
        SELF:_CheckEofBof()
    ENDIF
RETURN isOK

    // Open and Close
    /// <inheritdoc />
OVERRIDE METHOD Close() 			AS LOGIC
	LOCAL isOK := FALSE AS LOGIC
	IF SELF:IsOpen
    // Validate
		isOK := SELF:GoCold()
    //
		IF isOK
			SELF:UnLock(0)
			IF !SELF:_ReadOnly
				SELF:Flush()
			ENDIF
			IF SELF:_HeaderLocked
				SELF:HeaderLock( DbLockMode.UnLock )
			ENDIF
        ENDIF
		TRY
			isOK := FClose( SELF:_hFile )
			IF SELF:_HasMemo
				SELF:CloseMemFile()
			ENDIF

			isOK := SUPER:Close() .AND. isOK
		CATCH ex AS Exception
			isOK := FALSE
			SELF:_dbfError(ex, Subcodes.ERDD_CLOSE_FILE,Gencode.EG_CLOSE,  "DBF.Close")

        END TRY
        #ifdef INPUTBUFFER
            IF (_inBuffer != NULL)
                _inBuffer:Close()
				_inBuffer := NULL_OBJECT
            ENDIF
        #endif
		SELF:_hFile := F_ERROR
        SELF:_oStream := NULL
	ENDIF
RETURN isOK

    // Move to the End of file, and place a End-Of-File Marker (0x1A)
PROTECTED METHOD _putEndOfFileMarker() AS LOGIC
	// According to DBASE.com Knowledge base :
	// The end of the file is marked by a single byte, with the end-of-file marker, an OEM code page character value of 26 (0x1A).
	LOCAL lOffset   := SELF:_HeaderLength + SELF:_RecCount * SELF:_RecordLength AS INT64
	// Note FoxPro does not write EOF character for files with 0 records
	RETURN _oStream:SafeSetPos(lOffset) .AND. _oStream:SafeWriteByte(26) .AND. _oStream:SafeSetLength(lOffset+1)


    // Create a DBF File, based on the DbOpenInfo Structure
    // Write the File Header, and the Fields Header; Create the Memo File if needed
OVERRIDE METHOD Create(info AS DbOpenInfo) AS LOGIC
	LOCAL isOK AS LOGIC
    //
	isOK := FALSE
	IF SELF:_Fields:Length == 0
		RETURN FALSE
	ENDIF
	SELF:_OpenInfo := info
    // Should we set to .DBF per default ?
	IF String.IsNullOrEmpty(SELF:_OpenInfo:Extension)
		SELF:_OpenInfo:Extension := _Extension
        //
	ENDIF
    //
	SELF:_Hot := FALSE
	SELF:_FileName := SELF:_OpenInfo:FullName
	SELF:_Alias := SELF:_OpenInfo:Alias
	SELF:_Shared := SELF:_OpenInfo:Shared
	SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
    //
#ifdef INPUTBUFFER
	SELF:_hFile    := FCreate2( SELF:_FileName, FO_EXCLUSIVE | FO_UNBUFFERED)
#else
	SELF:_hFile    := FCreate2( SELF:_FileName, FO_EXCLUSIVE)
#endif
    // Adjust path to be sure we handle DOS 8 name chars correctly
	IF SELF:IsOpen
        SELF:_oStream    := (FileStream) FGetStream(SELF:_hFile)
        SELF:_FileName   := _oStream:Name
		LOCAL fieldCount :=  SELF:_Fields:Length AS INT
		LOCAL fieldDefSize := fieldCount * DbfField.SIZE AS INT
		LOCAL codePage AS LONG
        IF XSharp.RuntimeState.Ansi
			SELF:_Ansi := TRUE
			codePage := XSharp.RuntimeState.WinCodePage
		ELSE
			SELF:_Ansi := FALSE
			codePage := XSharp.RuntimeState.DosCodePage
		ENDIF        // First, just the Header
		SELF:_Encoding := System.Text.Encoding.GetEncoding( codePage )

		SELF:_Header:HeaderLen := SHORT(DbfHeader.SIZE + fieldDefSize + 2 )
		SELF:_Header:isHot := TRUE
        //


		IF SELF:_Ansi
			SELF:_lockScheme:Initialize( DbfLockingModel.VoAnsi )
		ELSE
			SELF:_lockScheme:Initialize( DbfLockingModel.Clipper52 )
		ENDIF

        //
        // Convert the Windows CodePage to a DBF CodePage
		SELF:_Header:CodePage := CodePageExtensions.ToHeaderCodePage( (OsCodepage)codePage )
        // Init Header version, should it be a parameter ?
        LOCAL lSupportAnsi := FALSE AS LOGIC
        SWITCH RuntimeState.Dialect
            CASE XSharpDialect.VO
            CASE XSharpDialect.Vulcan
            CASE XSharpDialect.Core
                lSupportAnsi := TRUE
            OTHERWISE
                lSupportAnsi := FALSE
        END SWITCH

		IF SELF:_Ansi .and. lSupportAnsi
			SELF:_Header:Version := IIF(SELF:_HasMemo, DBFVersion.VOWithMemo , DBFVersion.VO )
		ELSE
			SELF:_Header:Version := IIF(SELF:_HasMemo, DBFVersion.FoxBaseDBase3WithMemo , DBFVersion.FoxBaseDBase3NoMemo )
		ENDIF
        // This had been initialized by AddFields()
		SELF:_Header:RecordLen := (WORD) SELF:_RecordLength
        // This will fill the Date and RecCount
		isOK := SELF:_writeHeader()
        SELF:_wasChanged := TRUE
		IF isOK
			SELF:_HeaderLength := SELF:_Header:HeaderLen
			isOK := SELF:_writeFieldsHeader()
			IF isOK
				SELF:_RecordLength := SELF:_Header:RecordLen
				IF SELF:_HasMemo
					isOK := SELF:CreateMemFile( info )
				ENDIF
				SELF:_AllocateBuffers()
                // Read fields again so the right column objects are allocated
                SELF:_readFieldsHeader()
            ENDIF
		ENDIF
		IF !isOK
			IF SELF:_HasMemo
				SELF:CloseMemFile( )
			ENDIF
			FClose( SELF:_hFile )
			SELF:_hFile	:= F_ERROR
			SELF:_oStream    := NULL
		ELSE
			SELF:GoTop()
		ENDIF
	ELSE
		VAR ex := FException()
		SELF:_dbfError( ex, ERDD.CREATE_FILE, XSharp.Gencode.EG_CREATE )
	ENDIF
RETURN isOK



// Allow subclass (VFP) to set Extra flags
PROTECTED VIRTUAL METHOD _checkField( dbffld REF DbfField) AS LOGIC
    RETURN dbffld:Type:IsStandard()


    // Write the Fields Header, based on the _Fields List
PROTECTED VIRTUAL METHOD _writeFieldsHeader() AS LOGIC
	LOCAL isOK AS LOGIC
	LOCAL fieldCount :=  SELF:_Fields:Length AS INT
	LOCAL fieldDefSize := fieldCount * DbfField.SIZE AS INT
    // Now, create the Structure
	LOCAL fieldsBuffer := BYTE[]{ fieldDefSize +1 } AS BYTE[] // +1 to add 0Dh stored as the field terminator.
	LOCAL currentField := DbfField{SELF:_Encoding} AS DbfField
    IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF

	LOCAL nOffSet AS LONG
	nOffSet := 0
	FOREACH VAR fld IN SELF:_Fields
		currentField:Offset := fld:Offset
		currentField:Name   := fld:Name
		currentField:Type   := fld:FieldType
		IF fld:FieldType != DbFieldType.Character
			currentField:Len := (BYTE) fld:Length
			currentField:Dec := (BYTE) fld:Decimals
		ELSE
			currentField:Len := (BYTE) (fld:Length % 256)
			IF fld:Length > Byte.MaxValue
				currentField:Dec := (BYTE) (fld:Length / 256)
			ELSE
				currentField:Dec := 0
			ENDIF
		ENDIF
		currentField:Flags := fld:Flags
		IF ! SELF:_checkField(REF currentField)
			SELF:_dbfError( ERDD.CREATE_FILE, XSharp.Gencode.EG_DATATYPE,"DBF:Create()", "Invalid "+fld:ToString())
			RETURN FALSE
		ENDIF
		Array.Copy(currentField:Buffer, 0, fieldsBuffer, nOffSet, DbfField.SIZE )
		nOffSet += DbfField.SIZE
	NEXT


    // Terminator
	fieldsBuffer[fieldDefSize] := 0x0D
    // Go end of Header
    isOK :=  _oStream:SafeWriteAt(DbfHeader.SIZE, fieldsBuffer,fieldDefSize+1 )
    IF ! isOK
		SELF:_dbfError( FException(), ERDD.WRITE, XSharp.Gencode.EG_WRITE )
    ENDIF
    //
RETURN isOK

    /// <inheritdoc />
OVERRIDE METHOD Open(info AS XSharp.RDD.Support.DbOpenInfo) AS LOGIC
	LOCAL isOK AS LOGIC
    //
	isOK := FALSE
	SELF:_OpenInfo := info
    // Should we set to .DBF per default ?
	IF String.IsNullOrEmpty(SELF:_OpenInfo:Extension)
		SELF:_OpenInfo:Extension := _Extension
	ENDIF
	SELF:_Hot := FALSE
	SELF:_FileName := SELF:_OpenInfo:FullName
	IF File(SELF:_FileName)
		SELF:_FileName := FPathName()
		SELF:_OpenInfo:FullName := SELF:_FileName
	ENDIF
	SELF:_Alias := SELF:_OpenInfo:Alias
	SELF:_Shared := SELF:_OpenInfo:Shared
	SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
#ifdef INPUTBUFFER
	SELF:_hFile    := FOpen(SELF:_FileName, SELF:_OpenInfo:FileMode | FO_UNBUFFERED)
#else
	SELF:_hFile    := FOpen(SELF:_FileName, SELF:_OpenInfo:FileMode)
#endif

	IF SELF:IsOpen
		SELF:_oStream    := (FileStream) FGetStream(SELF:_hFile)
		isOK := SELF:_readHeader()
		IF isOK
			IF SELF:_HasMemo
				isOK := SELF:OpenMemFile( info )
			ENDIF
			SELF:GoTop()
            //
			SELF:_Ansi := SELF:_Header:IsAnsi
			SELF:_Encoding := System.Text.Encoding.GetEncoding( CodePageExtensions.ToCodePage( SELF:_Header:CodePage )  )
            //
		ELSE
			SELF:_dbfError( ERDD.CORRUPT_HEADER, XSharp.Gencode.EG_CORRUPTION )
		ENDIF
	ELSE
        // Error or just FALSE ?
		isOK := FALSE
		LOCAL ex := FException() AS Exception
		SELF:_dbfError( ex, ERDD.OPEN_FILE, XSharp.Gencode.EG_OPEN )
	ENDIF
	IF SELF:_Ansi
		SELF:_lockScheme:Initialize( DbfLockingModel.VoAnsi )
	ELSE
		SELF:_lockScheme:Initialize( DbfLockingModel.Clipper52 )
	ENDIF

    //
RETURN isOK

    // Read the DBF Header, retrieve RecCount, then read the Fields Header
PRIVATE METHOD _readHeader() AS LOGIC
	LOCAL isOK AS LOGIC
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF
    isOK := SELF:_Header:Read()
    //
	IF isOK
		SELF:_HeaderLength := SELF:_Header:HeaderLen
        //
        // Something wrong in Size...
		IF SELF:_Header:FieldCount <= 0
			RETURN FALSE
		ENDIF
		SELF:_RecCount := SELF:_Header:RecCount
        SELF:_Encoding := System.Text.Encoding.GetEncoding( CodePageExtensions.ToCodePage( SELF:_Header:CodePage )  )

        // Move to top, after header
        isOK := SELF:_readFieldsHeader()
	ENDIF
RETURN isOK

    // Read the Fields Header, filling the _Fields List with RddFieldInfo
PROTECTED METHOD _readFieldsHeader() AS LOGIC
	LOCAL isOK AS LOGIC
	LOCAL fieldCount   := SELF:_Header:FieldCount AS INT
	LOCAL fieldDefSize := fieldCount * DbfField.SIZE AS INT
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF
	SELF:_NullCount := 0
    // Read full Fields Header
	VAR fieldsBuffer := BYTE[]{ fieldDefSize }
    isOK   := _oStream:SafeReadAt(DbfHeader.SIZE, fieldsBuffer,fieldDefSize)
	IF isOK
		SELF:_HasMemo := FALSE
		VAR currentField := DbfField{SELF:_Encoding}
        // Now, process
        //SELF:_Fields := DbfRddFieldInfo[]{ fieldCount }
        // count # of fields. When we see a 0x0D then the header has blank space for non fields
		fieldCount := 0
		FOR VAR i := 0 UPTO fieldDefSize - 1 STEP DbfField.SIZE
			IF fieldsBuffer[i] == 0x0D // last field
				EXIT
			ENDIF
			fieldCount++
		NEXT

		SELF:SetFieldExtent( fieldCount )
		LOCAL nStart AS INT
		nStart := 0
		FOR VAR i := nStart TO fieldCount - ( 1 - nStart )
			LOCAL nPos := i*DbfField.SIZE AS LONG
			Array.Copy(fieldsBuffer, nPos, currentField:Buffer, 0, DbfField.SIZE )
			IF ! SELF:Header:Version:UsesFlags()
			   currentField:ClearFlags()
			ENDIF
			VAR column := DbfColumn.Create(REF currentField, SELF, nPos + DbfHeader.SIZE)
			SELF:AddField( column)
			IF column:IsMemo
				SELF:_HasMemo := TRUE
			ENDIF
		NEXT
        // Allocate the Buffer to read Records
		SELF:_RecordLength := SELF:_Header:RecordLen
		SELF:_AllocateBuffers()
	ENDIF
RETURN isOK

INTERNAL METHOD _readField(nOffSet AS LONG, oField AS DbfField) AS LOGIC
    // Read single field. Called from AutoIncrement code to read the counter value
    RETURN _oStream:SafeReadAt(nOffSet,oField:Buffer,DbfField.SIZE)

INTERNAL METHOD _writeField(nOffSet AS LONG, oField AS DbfField) AS LOGIC
    // Write single field in header. Called from AutoIncrement code to update the counter value
    RETURN  _oStream:SafeWriteAt(nOffSet, oField:Buffer, DbfField.SIZE)



    // Write the DBF file Header : Last DateTime of modification (now), Current Reccount
PROTECTED METHOD _writeHeader() AS LOGIC
	LOCAL ret := TRUE AS LOGIC
    // Really ?
	IF SELF:_Header:isHot
		IF SELF:_ReadOnly
            // Error !! Cannot be written !
			SELF:_dbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
			RETURN FALSE
		ENDIF
        // Update the number of records
		SELF:_Header:RecCount := SELF:_RecCount
		TRY
            ret := SELF:_Header:Write()

		CATCH ex AS Exception
			SELF:_dbfError( ex, ERDD.WRITE, XSharp.Gencode.EG_WRITE )
			ret := FALSE
		END TRY
        // Ok, go Cold
		SELF:_Header:isHot := FALSE
	ENDIF
    //
RETURN ret


    // Fields
/// <inheritdoc />
OVERRIDE METHOD SetFieldExtent( fieldCount AS LONG ) AS LOGIC
	SELF:_HasMemo := FALSE
RETURN SUPER:SetFieldExtent(fieldCount)


    // Add a Field to the _Fields List. Fields are added in the order of method call
    /// <inheritdoc />
OVERRIDE METHOD AddField(info AS RddFieldInfo) AS LOGIC
	LOCAL isOK AS LOGIC
    // convert RddFieldInfo to DBFColumn
	IF ! (info IS DbfColumn)
		info := DbfColumn.Create(info, SELF)
	ENDIF
	isOK := SUPER:AddField( info )
	IF isOK  .AND. info:IsMemo
		SELF:_HasMemo := TRUE
	ENDIF
RETURN isOK

PROTECT OVERRIDE METHOD _checkFields(info AS RddFieldInfo) AS LOGIC
    // FieldName
	info:Name := info:Name:ToUpper():Trim()
    IF String.Compare(info:Name, _NULLFLAGS,TRUE) == 0
        info:Name := _NULLFLAGS
    ENDIF
	IF info:Name:Length > 10
		info:Name := info:Name:Substring(0,10)
	ENDIF
	IF ! info:Validate()
		SELF:_dbfError( ERDD.CORRUPT_HEADER, XSharp.Gencode.EG_ARG,"ValidateDbfStructure", i"Field '{info.Name}' is not valid"  , FALSE)
		RETURN FALSE
	ENDIF
	VAR cType := Chr( (BYTE) info:FieldType)
	IF ! cType $ SELF:_AllowedFieldTypes
		SELF:_dbfError( ERDD.CORRUPT_HEADER, XSharp.Gencode.EG_ARG,"ValidateDbfStructure", i"Field Type '{cType}' for field '{info.Name}' is not allowed for RDD '{SELF:Driver}'" , FALSE)
	ENDIF

RETURN TRUE


/// <inheritdoc />
OVERRIDE METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
	LOCAL oResult := NULL AS OBJECT
	IF SELF:_FieldIndexValidate(nFldPos)
		BEGIN LOCK SELF

            //
			SWITCH nOrdinal
            // These are handled in the parent class and also take care of aliases etc.
			CASE DbFieldInfo.DBS_NAME
			CASE DbFieldInfo.DBS_CAPTION
			CASE DbFieldInfo.DBS_LEN
			CASE DbFieldInfo.DBS_DEC
			CASE DbFieldInfo.DBS_TYPE
			CASE DbFieldInfo.DBS_ALIAS
			CASE DbFieldInfo.DBS_COLUMNINFO
				oResult := SUPER:FieldInfo(nFldPos, nOrdinal, oNewValue)

			CASE DbFieldInfo.DBS_ISNULL
			CASE DbFieldInfo.DBS_COUNTER
			CASE DbFieldInfo.DBS_STEP
				oResult := NULL
				LOCAL oColumn AS DbfColumn
				oColumn := SELF:_GetColumn(nFldPos) ASTYPE DbfColumn
				IF oColumn != NULL

					IF nOrdinal == DbFieldInfo.DBS_ISNULL
						oResult := oColumn:IsNull()
					ELSEIF nOrdinal == DbFieldInfo.DBS_COUNTER
						IF oColumn IS DbfAutoIncrementColumn VAR dbfac
							dbfac:Read()
							oResult := dbfac:Counter
							IF oNewValue != NULL
                                // update counter
								LOCAL iNewValue AS Int32
								iNewValue := Convert.ToInt32(oNewValue)
								IF SELF:HeaderLock(DbLockMode.Lock)
									dbfac:Counter := iNewValue
									dbfac:Write()
									SELF:HeaderLock(DbLockMode.UnLock)
								ENDIF
							ENDIF
						ENDIF
					ELSEIF nOrdinal == DbFieldInfo.DBS_STEP
						IF oColumn IS DbfAutoIncrementColumn VAR dbfac
							dbfac:Read()
							oResult := dbfac:IncrStep
							IF oNewValue != NULL
                                // update step
								LOCAL iNewValue AS Int32
								iNewValue := Convert.ToInt32(oNewValue)
								IF SELF:HeaderLock(DbLockMode.Lock)
									dbfac:IncrStep := iNewValue
									dbfac:Write()
									SELF:HeaderLock(DbLockMode.UnLock)
								ENDIF
							ENDIF
						ENDIF
					ENDIF
				ENDIF

			OTHERWISE
                // Everything falls through to parent at this moment
				oResult := SUPER:FieldInfo(nFldPos, nOrdinal, oNewValue)
			END SWITCH
		END LOCK
	ENDIF
RETURN oResult


    // Read & Write

// Move to the current record, then read the raw Data into the internal RecordBuffer; Set the DELETED Flag
VIRTUAL PROTECTED METHOD _readRecord() AS LOGIC
	LOCAL isOK AS LOGIC
    // Buffer is supposed to be correct
	IF SELF:_BufferValid == TRUE .OR. SELF:EoF .OR. SELF:RecNo > SELF:RecCount
		RETURN TRUE
	ENDIF
    // File Ok ?
	isOK := SELF:IsOpen
    //
	IF  isOK
        // Record pos is One-Based
		LOCAL lOffset := SELF:_HeaderLength + ( SELF:_RecNo - 1 ) * SELF:_RecordLength AS LONG
#ifdef INPUTBUFFER
		isOK := _inBuffer:Read(lOffset, SELF:_RecordBuffer, SELF:_RecordLength)
#else
		isOK := _oStream:SafeReadAt(lOffset, SELF:_RecordBuffer, SELF:_RecordLength)
#endif

		IF isOK
			// Read Record
			SELF:_BufferValid := TRUE
			SELF:_isValid := TRUE
			SELF:_Deleted := ( SELF:_RecordBuffer[ 0 ] == '*' )
		ENDIF
	ENDIF
RETURN isOK

    // Move to the current record, write the raw Data, then update the Header (for DateTime mainly)
VIRTUAL PROTECTED METHOD _writeRecord() AS LOGIC
	LOCAL isOK AS LOGIC
    // File Ok ?
	isOK := SELF:IsOpen
    //
	IF isOK
    //
		IF SELF:_ReadOnly
        // Error !! Cannot be written !
			SELF:_dbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
			isOK := FALSE
		ELSE
            SELF:_wasChanged := TRUE
            // Write Current Data Buffer
            // Record pos is One-Based
			LOCAL recordPos AS LONG
			recordPos := SELF:_HeaderLength + ( SELF:_RecNo - 1 ) * SELF:_RecordLength
#ifdef INPUTBUFFER
            isOK := _inBuffer:Write(recordPos, SELF:_RecordBuffer, SELF:_RecordLength)
#else
            isOK :=  _oStream:SafeWriteAt(recordPos, SELF:_RecordBuffer)
#endif
			IF isOK
			   // Write Record
				TRY
	                // Don't forget to Update Header (nkok: only if changed!)
			        IF SELF:Shared
				        SELF:_writeHeader()
					ENDIF
				CATCH ex AS Exception
					SELF:_dbfError( ex, ERDD.WRITE, XSharp.Gencode.EG_WRITE )
				END TRY
			ENDIF
		ENDIF
	ENDIF
RETURN isOK


INTERNAL METHOD _dbfError(ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, lThrow := TRUE AS LOGIC) AS VOID
	SELF:_dbfError(ex, iSubCode, iGenCode, String.Empty, ex?:Message, XSharp.Severity.ES_ERROR, lThrow)

INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD, lThrow := TRUE AS LOGIC) AS VOID
	SELF:_dbfError(NULL, iSubCode, iGenCode, String.Empty, String.Empty, XSharp.Severity.ES_ERROR, lThrow )

INTERNAL METHOD _dbfError(ex AS Exception,iSubCode AS DWORD, iGenCode AS DWORD, iSeverity AS DWORD, lThrow := TRUE AS LOGIC) AS VOID
	SELF:_dbfError(ex, iSubCode, iGenCode, String.Empty, String.Empty, iSeverity, lThrow)

INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD, iSeverity AS DWORD, lThrow := TRUE AS LOGIC) AS VOID
	SELF:_dbfError(NULL, iSubCode, iGenCode, String.Empty, String.Empty, iSeverity, lThrow)

INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, lThrow := TRUE AS LOGIC) AS VOID
	SELF:_dbfError(NULL, iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR, lThrow)

INTERNAL METHOD _dbfError(ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, lThrow := TRUE AS LOGIC) AS VOID
	SELF:_dbfError(ex, iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR, lThrow)

INTERNAL METHOD _dbfError(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING, lThrow := TRUE AS LOGIC) AS VOID
	SELF:_dbfError(NULL, iSubCode, iGenCode, strFunction,strMessage, XSharp.Severity.ES_ERROR, lThrow)

INTERNAL METHOD _dbfError(ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING, iSeverity AS DWORD, lThrow := TRUE AS LOGIC) AS VOID
	LOCAL oError AS RddError
    //
	IF ex != NULL
		oError := RddError{ex,iGenCode, iSubCode}
	ELSE
		oError := RddError{iGenCode, iSubCode}
    ENDIF
    oError:CanDefault := TRUE
	oError:SubSystem := SELF:Driver
	oError:Severity := iSeverity
	oError:FuncSym  := IIF(strFunction == NULL, "", strFunction) // code in the SDK expects all string properties to be non-NULL
	oError:FileName := SELF:_FileName
	IF String.IsNullOrEmpty(strMessage)  .AND. ex != NULL
		strMessage := ex:Message
    ENDIF
    IF String.IsNullOrEmpty(strMessage)
        IF oError:SubCode != 0
            oError:Description := oError:GenCodeText + " (" + oError:SubCodeText+")"
        ELSE
            oError:Description := oError:GenCodeText
        ENDIF
    ELSE
	    oError:Description := strMessage
    ENDIF
	RuntimeState.LastRddError := oError
    //
    IF lThrow
	    THROW oError
    ENDIF
    RETURN

INTERNAL METHOD _getUsualType(oValue AS OBJECT) AS __UsualType
	LOCAL typeCde AS TypeCode
	IF oValue == NULL
		RETURN __UsualType.Void
	ELSE
		typeCde := Type.GetTypeCode(oValue:GetType())
		SWITCH typeCde
		CASE TypeCode.SByte
		CASE TypeCode.Byte
		CASE TypeCode.Int16
		CASE TypeCode.UInt16
		CASE TypeCode.Int32
			RETURN __UsualType.Long
		CASE TypeCode.UInt32
		CASE TypeCode.Int64
		CASE TypeCode.UInt64
		CASE TypeCode.Single
		CASE TypeCode.Double
			RETURN __UsualType.Float
		CASE TypeCode.Boolean
			RETURN __UsualType.Logic
		CASE TypeCode.String
			RETURN __UsualType.String
		CASE TypeCode.DateTime
			RETURN __UsualType.DateTime
		CASE TypeCode.Object
			IF oValue IS IDate
				RETURN __UsualType.Date
			ELSEIF  oValue IS IFloat
				RETURN __UsualType.Float
			ENDIF
		END SWITCH
	ENDIF
RETURN __UsualType.Object


INTERNAL VIRTUAL METHOD _GetColumn(nFldPos AS LONG) AS RddFieldInfo
	LOCAL nArrPos := nFldPos -1 AS LONG
    IF nArrPos >= 0 .AND. nArrPos < SELF:_Fields:Length
        RETURN SELF:_Fields[ nArrPos ]
    ENDIF
    SELF:_dbfError(EDB_FIELDINDEX, EG_ARG)
    RETURN NULL

    // Indicate if a Field is a Memo
    // At DBF Level, TRUE only for DbFieldType.Memo
INTERNAL VIRTUAL METHOD _isMemoField( nFldPos AS LONG ) AS LOGIC
	VAR oColumn := SELF:_GetColumn(nFldPos)
    IF oColumn != NULL
        RETURN oColumn:IsMemo
    ENDIF
    RETURN FALSE

    // Retrieve the BlockNumber as it is written in the DBF
OVERRIDE METHOD _getMemoBlockNumber( nFldPos AS LONG ) AS LONG
	LOCAL blockNbr := 0 AS LONG
	SELF:ForceRel()
	VAR oColumn := SELF:_GetColumn(nFldPos) ASTYPE DbfColumn
	IF oColumn != NULL .AND. oColumn:IsMemo
		IF SELF:_readRecord()
            VAR blockNo := oColumn:GetValue(SELF:_RecordBuffer)
            IF blockNo != NULL .and. blockNo != DBNull.Value
			    blockNbr := (LONG) blockNo
            ENDIF
		ENDIF
	ENDIF
RETURN blockNbr

    /// <inheritdoc />
OVERRIDE METHOD GetValue(nFldPos AS LONG) AS OBJECT
	LOCAL ret := NULL AS OBJECT
	SELF:ForceRel()
    // Read Record to Buffer
	VAR oColumn := SELF:_GetColumn(nFldPos) ASTYPE DbfColumn
    IF oColumn == NULL
        // Getcolumn already sets the error
        RETURN NULL
    ENDIF
	IF SELF:_readRecord()
        //
		IF oColumn:IsMemo
			IF SELF:HasMemo
                // At this level, the return value is the raw Data, in BYTE[]
				RETURN _Memo:GetValue(nFldPos)
			ELSE
				RETURN SUPER:GetValue(nFldPos)
			ENDIF
		ELSE
			ret := oColumn:GetValue(SELF:_RecordBuffer )
		ENDIF
	ELSE
		IF SELF:EoF
            // do not call _Memo for empty values. Memo columns return an empty string for the empty value
			ret := oColumn:EmptyValue()
		ELSE
			SELF:_dbfError( Subcodes.ERDD_READ, XSharp.Gencode.EG_READ ,"DBF.GetValue")
		ENDIF
	ENDIF
RETURN ret

    /// <inheritdoc />
OVERRIDE METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
	SELF:ForceRel()
	IF SELF:HasMemo
		RETURN _Memo:GetValueFile(nFldPos, fileName)
	ELSE
		RETURN SUPER:GetValueFile(nFldPos, fileName)
	ENDIF

    /// <inheritdoc />
OVERRIDE METHOD GetValueLength(nFldPos AS LONG) AS LONG
	SELF:ForceRel()
	IF SELF:HasMemo
		RETURN _Memo:GetValueLength(nFldPos)
	ELSE
		RETURN SUPER:GetValueLength(nFldPos)
	ENDIF

    /// <inheritdoc />
OVERRIDE METHOD Flush() 			AS LOGIC
	LOCAL isOK AS LOGIC
    LOCAL locked := FALSE AS LOGIC
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF
	isOK := SELF:GoCold()

	IF isOK .and. SELF:_wasChanged
		IF SELF:Shared .AND. SELF:Header:isHot
			locked := SELF:HeaderLock( DbLockMode.Lock )
            // Another workstation may have added another record, so make sure we update the reccount
            SELF:_RecCount := SELF:_calculateRecCount()
            //? SELF:CurrentThreadId, "After CalcReccount"
        ENDIF
        IF ! SELF:_Shared
		    SELF:_putEndOfFileMarker()
        ENDIF
        //? SELF:CurrentThreadId, "After EOF"
        IF SELF:Header:isHot
		    SELF:_writeHeader()
        ENDIF
        //? SELF:CurrentThreadId, "After writeHeader"
        IF XSharp.RuntimeState.GetValue<LOGIC>(Set.HardCommit)
            _oStream:Flush(TRUE)
        ELSE
            _oStream:Flush(FALSE)
        ENDIF
        //? SELF:CurrentThreadId, "After FFlush"
	ENDIF
	IF SELF:Shared .AND. locked
		SELF:HeaderLock( DbLockMode.UnLock )
	ENDIF
    //
	IF SELF:HasMemo
		isOK := _Memo:Flush()
	ENDIF
RETURN isOK

    /// <inheritdoc />
OVERRIDE METHOD Refresh() 			AS LOGIC
	LOCAL isOK AS LOGIC
	IF ! SELF:IsOpen
		RETURN FALSE
	ENDIF
	IF SELF:_ReadOnly
		RETURN TRUE
	ENDIF
	SELF:_Hot := FALSE
	SELF:_BufferValid := FALSE
	IF SELF:_NewRecord
        VAR nCount := SELF:RecCount
		SELF:_NewRecord  := FALSE
        SELF:_UpdateRecCount(nCount-1)      // writes the reccount to the header as well
		isOK := SELF:GoBottom()
	ELSE
		isOK := TRUE
	ENDIF
    //
RETURN isOK
    // Save any Pending Change
    /// <inheritdoc />
OVERRIDE METHOD GoCold()			AS LOGIC
	LOCAL ret AS LOGIC
    //
	ret := TRUE
	IF SELF:_Hot
		BEGIN LOCK SELF
            //? CurrentThreadId, "GoCold Recno", SELF:RecNo
			SELF:_writeRecord()
			SELF:_NewRecord := FALSE
			SELF:_Hot := FALSE
		END LOCK
	ENDIF
RETURN ret

    // Indicate that the content of the current buffer needs to be saved
    /// <inheritdoc />
OVERRIDE METHOD GoHot()			AS LOGIC
	LOCAL ret AS LOGIC
    //
	ret := TRUE
	IF !SELF:_Hot
		BEGIN LOCK SELF
			IF SELF:_Shared .AND. !SELF:_fLocked .AND. !SELF:_Locks:Contains( SELF:RecNo )
				SELF:_dbfError( ERDD.UNLOCKED, XSharp.Gencode.EG_UNLOCKED )
				ret := FALSE
			ENDIF
			IF SELF:_ReadOnly
            // Error !! Cannot be written !
				SELF:_dbfError( ERDD.READONLY, XSharp.Gencode.EG_READONLY )
				ret := FALSE
			ELSE
				SELF:_Hot := TRUE
			ENDIF
		END LOCK
	ENDIF
RETURN ret

/// <summary>Is the current row </summary>
PROPERTY IsHot AS LOGIC GET SELF:_Hot
/// <summary>Is the current row a new record (the result of Append())</summary>
PROPERTY IsNewRecord AS LOGIC GET SELF:_NewRecord

/// <inheritdoc />
OVERRIDE METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
    LOCAL ret := FALSE AS LOGIC
    IF SELF:_ReadOnly
        SELF:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY )
    ENDIF
    IF SELF:EoF
        RETURN FALSE
    ENDIF
    SELF:ForceRel()
	IF SELF:_readRecord()
        // GoHot() must be called first because this saves the current index values
    	ret := TRUE
		IF ! SELF:_Hot
			SELF:GoHot()
		ENDIF
		VAR oColumn := SELF:_GetColumn(nFldPos) ASTYPE DbfColumn
        IF oColumn != NULL
		    IF oColumn:IsMemo
			    IF SELF:HasMemo
				    IF _Memo:PutValue(nFldPos, oValue)
                        // Update the Field Info with the new MemoBlock Position
					    oColumn:PutValue(SELF:Memo:LastWrittenBlockNumber, SELF:_RecordBuffer)
				    ENDIF
			    ELSE
				    ret := SUPER:PutValue(nFldPos, oValue)
			    ENDIF
		    ELSE
			    ret := oColumn:PutValue(oValue, SELF:_RecordBuffer)
            ENDIF
        ELSE
            // Getcolumn already sets the error
            RETURN FALSE
        ENDIF
    ENDIF
    IF ! ret
        SELF:_dbfError(Subcodes.ERDD_WRITE, Gencode.EG_WRITE,"DBF.PutValue")
    ENDIF
RETURN ret

    /// <inheritdoc />
OVERRIDE METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
    IF SELF:_ReadOnly
        SELF:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY )
    ENDIF
    IF SELF:EoF
        RETURN FALSE
    ENDIF
    IF SELF:HasMemo
		RETURN _Memo:PutValueFile(nFldPos, fileName)
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
OVERRIDE METHOD CloseMemFile() 	AS LOGIC
	IF SELF:HasMemo
		RETURN _Memo:CloseMemFile()
	ELSE
		RETURN SUPER:CloseMemFile()
	ENDIF
    /// <inheritdoc />
OVERRIDE METHOD CreateMemFile(info AS DbOpenInfo) 	AS LOGIC
	IF SELF:HasMemo
		RETURN _Memo:CreateMemFile(info)
	ELSE
		RETURN SUPER:CreateMemFile(info)
	ENDIF

    /// <inheritdoc />
OVERRIDE METHOD OpenMemFile(info AS DbOpenInfo) 	AS LOGIC
	IF SELF:HasMemo
		RETURN _Memo:OpenMemFile(info)
	ELSE
		RETURN SUPER:OpenMemFile(info)
	ENDIF

    // Indexes

    /// <inheritdoc />
OVERRIDE METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC
	IF _oIndex != NULL
		RETURN _oIndex:OrderCreate(info)
	ELSE
		RETURN SUPER:OrderCreate(info)
	ENDIF

    /// <inheritdoc />
OVERRIDE METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC
	IF _oIndex != NULL
		RETURN _oIndex:OrderDestroy(info)
	ELSE
		RETURN SUPER:OrderDestroy(info)
	ENDIF

    /// <inheritdoc />
OVERRIDE METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT
	IF _oIndex != NULL
		RETURN _oIndex:OrderInfo(nOrdinal,info )
	ELSE
		RETURN SUPER:OrderInfo(nOrdinal,info )
	ENDIF

    /// <inheritdoc />
OVERRIDE METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
	IF _oIndex != NULL
		RETURN _oIndex:OrderListAdd(info)
	ELSE
		RETURN SUPER:OrderListAdd(info)
	ENDIF

    /// <inheritdoc />
OVERRIDE METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
	IF _oIndex != NULL
		RETURN _oIndex:OrderListDelete(info)
	ELSE
		RETURN SUPER:OrderListDelete(info)
	ENDIF
    /// <inheritdoc />
OVERRIDE METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
	IF _oIndex != NULL
		RETURN _oIndex:OrderListFocus(info)
	ELSE
		RETURN SUPER:OrderListFocus(info)
	ENDIF
    /// <inheritdoc />
OVERRIDE METHOD OrderListRebuild() AS LOGIC
	IF _oIndex != NULL
		RETURN _oIndex:OrderListRebuild()
	ELSE
		RETURN SUPER:OrderListRebuild()
	ENDIF
    /// <inheritdoc />
OVERRIDE METHOD Seek(info AS DbSeekInfo) AS LOGIC
    LOCAL result AS LOGIC
	IF _oIndex != NULL
		result := _oIndex:Seek(info)
	ELSE
		result := SUPER:Seek(info)
    ENDIF
    SELF:_CheckEofBof()
    RETURN result

    // Relations
    /// <inheritdoc />
OVERRIDE METHOD ChildEnd(info AS DbRelInfo) AS LOGIC
	SELF:ForceRel()
RETURN SUPER:ChildEnd( info )

    /// <inheritdoc />
OVERRIDE METHOD ChildStart(info AS DbRelInfo) AS LOGIC
	SELF:ChildSync( info )
RETURN SUPER:ChildStart( info )

    /// <inheritdoc />
OVERRIDE METHOD ChildSync(info AS DbRelInfo) AS LOGIC
	SELF:GoCold()
	SELF:_RelInfoPending := info
	SELF:SyncChildren()
RETURN TRUE

    /// <inheritdoc />
OVERRIDE METHOD ForceRel() AS LOGIC
	LOCAL isOK    := TRUE AS LOGIC
	LOCAL gotoRec := 0 AS LONG
	IF SELF:_RelInfoPending != NULL
    // Save the current context
		LOCAL currentRelation := SELF:_RelInfoPending AS DbRelInfo
		SELF:_RelInfoPending := NULL
    //
		isOK := SELF:RelEval( currentRelation )
		IF isOK .AND. !((DBF)currentRelation:Parent):EoF
			TRY
				gotoRec := Convert.ToInt32( SELF:_EvalResult )
			CATCH ex AS InvalidCastException
				gotoRec := 0
				SELF:_dbfError(ex, Subcodes.ERDD_DATATYPE,Gencode.EG_DATATYPE,  "DBF.ForceRel", FALSE)
				RETURN FALSE
			END TRY
		ENDIF
		isOK := SELF:GoTo( gotoRec )
		SELF:_Found := SELF:_isValid
		SELF:_SetBOF(FALSE)
	ENDIF
RETURN isOK


    /// <inheritdoc />
OVERRIDE METHOD RelArea(nRelNum AS DWORD) AS DWORD
RETURN SUPER:RelArea(nRelNum)

    /// <inheritdoc />
OVERRIDE METHOD SyncChildren() AS LOGIC
	LOCAL isOK AS LOGIC
    //
	isOK := TRUE
	FOREACH info AS DbRelInfo IN SELF:_Relations
		isOK := info:Child:ChildSync( info )
		IF !isOK
			EXIT
		ENDIF
	NEXT
RETURN isOK



    // Codeblock Support
/// <inheritdoc />
OVERRIDE METHOD Compile(sBlock AS STRING) AS ICodeblock
	LOCAL result AS ICodeblock
	result := SUPER:Compile(sBlock)
	IF result == NULL
        IF (RuntimeState:LastRddError != NULL_OBJECT)
            SELF:_dbfError( RuntimeState:LastRddError, Subcodes.EDB_EXPRESSION, Gencode.EG_SYNTAX,"DBF.Compile")
        ELSE
            VAR msg := "Could not compile epression '"+sBlock+"'"
		    SELF:_dbfError( Subcodes.EDB_EXPRESSION, Gencode.EG_SYNTAX,"DBF.Compile", msg )
        ENDIF
	ENDIF
RETURN result

/// <inheritdoc />
OVERRIDE METHOD EvalBlock( cbBlock AS ICodeblock ) AS OBJECT
	LOCAL result := NULL AS OBJECT
	TRY
		result := SUPER:EvalBlock(cbBlock)
	CATCH ex AS Exception
		SELF:_dbfError(ex, Subcodes.EDB_EXPRESSION, Gencode.EG_SYNTAX, "DBF.EvalBlock")
	END TRY
RETURN result

    // Other
    /// <inheritdoc />
OVERRIDE METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
	LOCAL oResult AS OBJECT
	oResult := NULL
	SWITCH nOrdinal
	CASE DbInfo.DBI_ISDBF
		oResult := TRUE
	CASE DbInfo.DBI_CANPUTREC
		oResult := IIF(SELF:HasMemo, FALSE , TRUE)
	CASE DbInfo.DBI_GETRECSIZE
		oResult := SELF:_RecordLength
	CASE DbInfo.DBI_LASTUPDATE
		oResult := SELF:_Header:LastUpdate
	CASE DbInfo.DBI_GETHEADERSIZE
		oResult := (LONG) SELF:_Header:HeaderLen
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
	CASE DbInfo.DBI_FILESTREAM
		oResult := SELF:_oStream
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
	CASE DbInfo.DBI_MEMOFIELD
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
		oResult := NULL     // Todo Add DBI_TRIGGER
	CASE DbInfo.DBI_DECRYPT         // Todo Add DBI_DECRYPT
	CASE DbInfo.DBI_ENCRYPT         // Todo Add DBI_ENCRYPT
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
OVERRIDE METHOD RecInfo(nOrdinal AS LONG, oRecID AS OBJECT, oNewValue AS OBJECT) AS OBJECT
	LOCAL nNewRec := 0 AS LONG
	LOCAL oResult AS OBJECT
	LOCAL nOld := 0 AS LONG

	IF oRecID != NULL
		TRY
			nNewRec := Convert.ToInt32( oRecID )
		CATCH ex AS Exception
			nNewRec := SELF:RecNo
			SELF:_dbfError(ex, Subcodes.ERDD_DATATYPE, Gencode.EG_DATATYPE, "DBF.RecInfo")

		END TRY
	ELSE
		nNewRec := SELF:RecNo
	ENDIF

    // Some operations require the new record te be selected
	SELF:ForceRel()
	IF nNewRec != 0
		SWITCH nOrdinal
		CASE DBRI_DELETED
		CASE DBRI_ENCRYPTED
		CASE DBRI_RAWRECORD
		CASE DBRI_RAWMEMOS
		CASE DBRI_RAWDATA
			nOld     := SELF:RecNo
			SELF:GoTo(nNewRec)
		END SWITCH
	ENDIF
	SWITCH nOrdinal
	CASE DBRI_DELETED
		oResult := SELF:Deleted
	CASE DBRI_LOCKED
		IF SELF:_Shared
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
		SELF:_readRecord()
		oResult := SELF:_RecordBuffer
	CASE DBRI_RAWRECORD
		oResult := SELF:_Encoding:GetString(SELF:_RecordBuffer,0, SELF:_RecordLength)
	CASE DBRI_UPDATED
		oResult := SELF:_Hot
		IF oNewValue IS LOGIC VAR isNew
			IF isNew
				SELF:_BufferValid := FALSE
				SELF:_readRecord()
			ENDIF
		ENDIF
	CASE DBRI_RAWMEMOS
	CASE DBRI_RAWDATA
        // RawData returns a string with the record + memos
        // RawMemos returns just the memos
        // Todo Add DBRI_RAW..
		oResult := ""
	CASE DBRI_ENCRYPTED
        // Todo add DBRI_ENCRYPTED
		oResult := FALSE
	OTHERWISE
		oResult := SUPER:Info(nOrdinal, oNewValue)
	END SWITCH
	IF nOld != 0
		SELF:GoTo(nOld)
	ENDIF
RETURN oResult

/// <inheritdoc />
OVERRIDE METHOD Sort(info AS DbSortInfo) AS LOGIC
	LOCAL recordNumber AS LONG
	LOCAL trInfo AS DbTransInfo
	LOCAL hasWhile AS LOGIC
	LOCAL hasFor AS LOGIC
	LOCAL sort AS RddSortHelper
	LOCAL i AS DWORD
	LOCAL fieldPos AS LONG
	LOCAL isNum AS LONG
	LOCAL isOK AS LOGIC
	LOCAL isQualified AS LOGIC
	LOCAL readMore AS LOGIC
	LOCAL limit AS LOGIC
	LOCAL rec AS SortRecord
	LOCAL sc AS DBFSortCompare
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
		fieldPos := info:Items[i]:FieldNo - 1
		isNum := 0
		IF SELF:_Fields[fieldPos]:FieldType == DbFieldType.Number
			isNum := 2
			info:Items[i]:Flags |= isNum
		ENDIF
		info:Items[i]:OffSet := SELF:_Fields[fieldPos]:Offset
		info:Items[i]:Length := SELF:_Fields[fieldPos]:Length
        //Next Field
		i++
	ENDDO
	isOK := TRUE
	isQualified := TRUE
	readMore := TRUE
	limit := TRUE
    //
    //			IF ( SELF:_Relations:Count > 0)
    //				SELF:ForceRel()
    //			ENDIF
    //
	IF trInfo:Scope:RecId != NULL
		recordNumber := Convert.ToInt32(trInfo:Scope:RecId)
		isOK := SELF:GoTo(recordNumber)
		readMore := TRUE
		limit := TRUE
		recordNumber := 1
	ELSE
		IF trInfo:Scope:NextCount != 0
			limit := TRUE
			recordNumber := trInfo:Scope:NextCount
			IF recordNumber < 1
				readMore := FALSE
			ENDIF
		ELSE
			readMore := TRUE
			limit := FALSE
			IF trInfo:Scope:WhileBlock == NULL .AND. !trInfo:Scope:Rest
				isOK := SELF:GoTop()
			ENDIF
		ENDIF
	ENDIF
	WHILE isOK .AND. !SELF:EoF .AND. readMore
		IF hasWhile
			readMore := (LOGIC) SELF:EvalBlock(trInfo:Scope:WhileBlock)
		ENDIF
		IF readMore .AND. hasFor
			isQualified := (LOGIC) SELF:EvalBlock(trInfo:Scope:ForBlock)
		ELSE
			isQualified := readMore
		ENDIF
		IF isOK .AND. isQualified
			isOK := SELF:_readRecord()
			IF isOK
				rec := SortRecord{SELF:_RecordBuffer, SELF:_RecNo}
				isOK := sort:Add(rec)
			ENDIF
		ENDIF
		IF readMore .AND. limit
			readMore := (--recordNumber != 0)
		ENDIF
		IF isOK .AND. readMore
			isOK := SELF:Skip(1)
		ENDIF
	END WHILE
	IF isOK
		sc := DBFSortCompare{SELF, info}
		isOK := sort:Sort(sc)
	ENDIF
	IF isOK
		isOK := sort:Write(SELF)
	ENDIF
RETURN isOK

    // IRddSortWriter Interface, used by RddSortHelper
METHOD IRddSortWriter.WriteSorted( sortInfo AS DbSortInfo , record AS SortRecord ) AS LOGIC
	Array.Copy(record:Data, SELF:_RecordBuffer, SELF:_RecordLength)
RETURN SELF:TransRec(sortInfo:TransInfo)


/// <inheritdoc />
OVERRIDE METHOD TransRec(info AS DbTransInfo) AS LOGIC
LOCAL result AS LOGIC
IF FALSE .AND. info:Destination IS DBF VAR oDest
    LOCAL oValue AS OBJECT
    result := oDest:Append(TRUE)
    IF info:Flags:HasFlag(DbTransInfoFlags.SameStructure)
        IF info:Flags:HasFlag(DbTransInfoFlags.CanPutRec)
            VAR buffer  := SELF:GetRec()
            result      := oDest:PutRec(buffer)
        ELSE
            VAR buffer  := SELF:GetRec()
            result      := oDest:PutRec(buffer)
            FOR VAR nI := 1 TO SELF:FieldCount
                LOCAL oColumn AS DbfColumn
                oColumn := oDest:_GetColumn(nI) ASTYPE DbfColumn
                IF oColumn:IsMemo
                    oValue := SELF:GetValue(nI)
                    oColumn:PutValue(0, oDest:_RecordBuffer)
                    result := oDest:PutValue(nI, oValue)
                    IF ! result
                        EXIT
                    ENDIF
                ENDIF
            NEXT
        ENDIF
    ELSE
        FOREACH oItem AS DbTransItem IN info:Items
            oValue := SELF:GetValue(oItem:Source)
            result := oDest:PutValue(oItem:Destination, oValue)
            IF ! result
                EXIT
            ENDIF
        NEXT
    ENDIF
    IF result .AND. SELF:Deleted
        result := oDest:Delete()
    ENDIF
ELSE
    result := SUPER:TransRec(info)
ENDIF
RETURN result


INTERNAL METHOD Validate() AS VOID
	IF !SELF:_BufferValid
		SELF:_readRecord()
	ENDIF
    // Properties
    //	PROPERTY Alias 		AS STRING GET
    /// <inheritdoc />
OVERRIDE PROPERTY BoF 		AS LOGIC
	GET
		SELF:ForceRel()
		RETURN SUPER:BoF
	END GET
END PROPERTY

/// <inheritdoc />
OVERRIDE PROPERTY Deleted 	AS LOGIC
	GET
		SELF:ForceRel()
		SELF:_readRecord()
		RETURN SELF:_Deleted
	END GET
END PROPERTY

/// <inheritdoc />
OVERRIDE PROPERTY EoF 		AS LOGIC
	GET
		SELF:ForceRel()
		RETURN SUPER:EoF
	END GET
END PROPERTY

//PROPERTY Exclusive	AS LOGIC GET

/// <inheritdoc />
OVERRIDE PROPERTY FieldCount AS LONG GET SELF:_Fields:Length

//	PROPERTY FilterText	AS STRING GET
OVERRIDE PROPERTY Found		AS LOGIC
	GET
		SELF:ForceRel()
		RETURN SUPER:Found
	END GET
END PROPERTY

/// <inheritdoc />
OVERRIDE PROPERTY RecCount	AS LONG
	GET
		IF SELF:Shared
			SELF:_RecCount := SELF:_calculateRecCount()
		ENDIF
		RETURN SELF:_RecCount
	END GET
END PROPERTY

PRIVATE METHOD _calculateRecCount()	AS LONG
	LOCAL reccount := 0 AS LONG
    //
	IF SELF:IsOpen
        VAR fSize   := SELF:_oStream:Length
		IF fSize != 0  // Just created file ?
			reccount := (LONG) ( fSize - SELF:_HeaderLength ) / SELF:_RecordLength
        ENDIF
	ENDIF
RETURN reccount

    /// <inheritdoc />
OVERRIDE PROPERTY RecNo		AS INT
	GET
		SELF:ForceRel()
		RETURN SELF:_RecNo
	END GET
END PROPERTY

/// <inheritdoc />
OVERRIDE PROPERTY Driver AS STRING GET "DBF"



END CLASS



END NAMESPACE






