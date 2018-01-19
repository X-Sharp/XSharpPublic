//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp
USING XSharp.RDD
using AdvantageClientEngine

/// <summary>
/// The Ads class.
/// </summary>
CLASS XSharp.RDD.AdsRDD Inherit Workarea
#region Fields
    //PRIVATE m_CallbackFn AS CallbackFn
    //PRIVATE m_CalltraceFile AS System.IO.StreamWriter
    //PRIVATE m_iProgress AS Long
    PRIVATE m_palRlocks AS DWord[]
    PRIVATE m_dbfName as STRING
    PRIVATE _Recno  as DWORD
    PUBLIC m_Encoding AS System.Text.Encoding
    PUBLIC m_hConnection AS System.IntPtr
    PUBLIC m_hIndex AS System.IntPtr
    PUBLIC m_hTable AS System.IntPtr
    PUBLIC m_strCollation AS string
    PUBLIC m_strDriver AS string
    PUBLIC m_usCheckRights AS Word
    PUBLIC m_usLockType AS Word
    PUBLIC m_usTableType AS Word
    PUBLIC MAX_KEY_SIZE AS Word
#endregion
 
CONSTRUCTOR()
    SUPER()
    SELF:_Order         := AdsIndex{SELF}
    SELF:_Memo          := AdsMemo{SELF}
    SELF:m_hTable       := System.IntPtr.Zero
    SELF:m_hIndex       := System.IntPtr.Zero
    SELF:m_hConnection  := System.IntPtr.Zero
    SELF:m_strDriver    := String.Empty
    SELF:m_strCollation := String.Empty
    SELF:m_strDriver    := "ADSRDD"
    SELF:m_dbfName      := String.Empty
#region Helper Methods that check for error conditions

    INTERNAL METHOD ACECALL(ulRetCode AS DWORD) AS VOID
        IF (ulRetCode != 0)
            SELF:ADSERROR((int) ulRetCode, 27)
        ENDIF
        RETURN 


    METHOD ADSERROR(iSubCode AS INT, iGenCode AS INT) AS void
        SELF:ADSERROR(iSubCode, iGenCode, String.Empty, String.Empty, ES_ERROR)

    METHOD ADSERROR(iSubCode AS INT, iGenCode AS INT, iSeverity as INT) AS void
        SELF:ADSERROR(iSubCode, iGenCode, String.Empty, String.Empty, iSeverity)

    METHOD ADSERROR(iSubCode AS INT, iGenCode AS INT, strFunction AS string) AS void
        SELF:ADSERROR(iSubCode, iGenCode, strFunction, String.Empty, ES_ERROR)

    METHOD ADSERROR(iSubCode AS int, iGenCode AS Int, strFunction AS string, strMessage AS string) AS void
        SELF:ADSERROR(iSubCode, iGenCode, strFunction,strMessage, ES_ERROR)

    METHOD ADSERROR(iSubCode AS int, iGenCode AS int, strFunction AS string, strMessage AS string, iSeverity AS Int) AS void
        LOCAL lastError AS DWord
        LOCAL pucBuf AS Char[]
        LOCAL wBufLen AS Word
        LOCAL oError as RddError
        oError := RddError{}
        oError:SubCode := iSubCode
        oError:Gencode := iGenCode
        oError:SubSystem := SELF:m_strDriver
        oError:Severity := iSeverity
        oError:FuncSym  := strFunction
        oError:FileName := SELF:m_dbfName
        IF (strMessage == String.Empty)
            //
            pucBuf := Char[]{ACE.ADS_MAX_ERROR_LEN}
            wBufLen := (WORD) pucBuf:Length
            IF (((ACE.AdsGetLastError(OUT lastError, pucBuf, REF wBufLen) == 0) .AND. (lastError != 0)) .AND. (wBufLen > 0))
                oError:Description := string{pucBuf, 0, wBufLen}
            ENDIF
        ELSE
            //
            oError:Description := strMessage
        ENDIF
        Throw oError
        

    INTERNAL METHOD Unsupported(strFunctionName AS string) AS Logic
        SELF:ADSERROR(ERDD_UNSUPPORTED, EG_UNSUPPORTED, strFunctionName)
        RETURN FALSE


#endregion
#region Helper Methods
    PRIVATE METHOD ACEORDER() AS System.IntPtr
        IF (SELF:m_hIndex != System.IntPtr.Zero)
            RETURN SELF:m_hIndex
        ENDIF
        RETURN SELF:m_hTable

    PRIVATE METHOD AxCheckVODeletedFlag() AS DWord
        SELF:ACECALL(ACE.AdsShowDeleted(IIF(Runtime.State.Deleted,(Word)0 ,(Word)1 )))
        RETURN 0
  
    PRIVATE METHOD AxCheckVODateFormat() AS Logic
        SELF:ACECALL(ACE.AdsSetDateFormat(Runtime.State.DateFormat))
        SELF:ACECALL(ACE.AdsSetExact(IIF(Runtime.State.Exact,(Word)1 ,(Word)0 )))
        SELF:ACECALL(ACE.AdsSetDecimals((Word)Runtime.State.Decimals ))
        SELF:ACECALL(ACE.AdsSetEpoch((Word)Runtime.State.Epoch ))
        RETURN TRUE
    
    PRIVATE METHOD AxAnsi2Unicode(source AS Char[], iLength AS Long) AS string
        TRY
            RETURN SELF:m_Encoding:GetString(SELF:m_Encoding:GetBytes(source, 0, iLength))
        CATCH e as Exception
            System.Diagnostics.Debug.WriteLine("Error converting ansi 2 unicode:")
            System.Diagnostics.Debug.WriteLine(e:Message)
            RETURN String.Empty
        END TRY



#endregion

#region Open and Close
#endregion
#region Navigation
    METHOD RecordMovement() AS Logic
        LOCAL atBOF AS Word
        LOCAL atEOF AS Word
        LOCAL isFound AS Word
        LOCAL lRecno as DWORD
        SELF:ACECALL(ACE.AdsGetRecordNum(SELF:m_hTable, ACE.ADS_IGNOREFILTERS,  OUT lRecno))
        SELF:_Recno := lRecno
        SELF:ACECALL(ACE.AdsAtBOF(SELF:m_hTable, out atBOF))
        SUPER:_Bof := (atBOF == 1)
        SELF:ACECALL(ACE.AdsAtEOF(SELF:m_hTable, out atEOF))
        SUPER:_Eof := (atEOF == 1)
        SELF:ACECALL(ACE.AdsIsFound(SELF:m_hTable,  out isFound))
        SUPER:_Found := (isFound == 1)
        IF (atBOF == 1 .AND. atEOF == 0)
            SELF:GoTop()
            SUPER:_Bof := TRUE
        ENDIF
        //IF SUPER:m_lpdbRelations != nul)
            //FOREACH dbrelinfo AS DBRELINFO IN SUPER:m_lpdbRelations
                //(ADSRDD)dbrelinfo:lpaChild :RecordMovement()
            //NEXT
        //ENDIF
        RETURN TRUE

    VIRTUAL METHOD GoBottom() AS Logic
        SELF:ACECALL(SELF:AxCheckVODeletedFlag())
        SELF:ACECALL(ACE.AdsGotoBottom(SELF:ACEORDER()))
        RETURN SELF:RecordMovement()

    VIRTUAL METHOD GoTop() AS Logic
        SELF:ACECALL(SELF:AxCheckVODeletedFlag())
        SELF:ACECALL(ACE.AdsGotoTop(SELF:ACEORDER()))
        RETURN SELF:RecordMovement()

    VIRTUAL METHOD GoTo(lRec AS DWord) AS Logic
        LOCAL recordnum AS DWord
        LOCAL atEOF AS Word
        LOCAL atBOF AS Word
        SELF:ACECALL(ACE.AdsGetRecordNum(SELF:m_hTable, ACE.ADS_IGNOREFILTERS, out recordnum))
        IF (recordnum == lRec)
            SELF:ACECALL(ACE.AdsAtEOF(SELF:m_hTable, out atEOF))
            SELF:ACECALL(ACE.AdsAtBOF(SELF:m_hTable, out atBOF))
            IF (atEOF== 0 .AND. atBOF == 0)
                SELF:ACECALL(ACE.AdsWriteRecord(SELF:m_hTable))
                SELF:ACECALL(ACE.AdsRefreshRecord(SELF:m_hTable))
            ENDIF
        ELSE
            SELF:ACECALL(ACE.AdsGotoRecord(SELF:m_hTable, lRec))
        ENDIF
        RETURN SELF:RecordMovement()
    
    VIRTUAL METHOD GoToId(oRecnum AS Object) AS Logic
        LOCAL recNum AS DWord
        TRY
            recNum := System.Convert.ToUInt32(oRecnum)
        CATCH e as Exception
            SELF:ADSERROR(ERDD_DATATYPE, EG_DATATYPE, "GoToId",e:Message)
            RETURN FALSE
        END TRY
        RETURN SELF:GoTo(recNum)
    
    VIRTUAL METHOD Skip(lCount AS Long) AS Logic
        LOCAL result AS DWord
        LOCAL flag AS Logic
        SELF:ACECALL(SELF:AxCheckVODeletedFlag())
        IF (lCount == 0)
            SELF:ACECALL(ACE.AdsWriteRecord(SELF:m_hTable))
            result := ACE.AdsRefreshRecord(SELF:m_hTable)
        ELSE
            result := ACE.AdsSkip(SELF:ACEORDER(), lCount)
        ENDIF
        IF result != ACE.AE_NO_CURRENT_RECORD
            SELF:ACECALL(result)
        ENDIF
        flag := SELF:RecordMovement()
        IF (lCount > 0)
            SUPER:_Bof := FALSE
            RETURN flag
        ENDIF
        IF (lCount < 0)
            SUPER:_Eof := FALSE
        ENDIF
        RETURN flag

    VIRTUAL METHOD SkipFilter(lCount AS Long) AS Logic
        // No difference with normal skip
        RETURN SELF:Skip(lCount)

    VIRTUAL METHOD SkipRaw(lCount AS Long) AS Logic
        // No difference with normal skip
        RETURN SELF:Skip(lCount)


    VIRTUAL METHOD Seek(seekinfo AS DBSEEKINFO) AS Logic
        LOCAL mode AS Word
        LOCAL found AS Word
        LOCAL atEOF AS Word
        LOCAL atBOF AS Word
        LOCAL Key AS string
        IF (SELF:m_hIndex == System.IntPtr.Zero)
            SELF:ADSERROR(ERDD_DATATYPE, EG_NOORDER, "Seek")
        ENDIF
        SELF:ACECALL(SELF:AxCheckVODeletedFlag())
        Key := seekinfo:value:ToString()
        IF seekinfo:SoftSeek
            mode := ACE.ADS_SOFTSEEK
        ELSE
            mode := ACE.ADS_HARDSEEK
        ENDIF
        IF seekInfo:Last
            SELF:ACECALL(ACE.AdsSeekLast(SELF:m_hIndex, Key, (Word)Key:Length , ACE.ADS_STRINGKEY, out found))
            IF found== 0 .AND. seekinfo:SoftSeek  
                SELF:ACECALL(ACE.AdsSeek(SELF:m_hIndex, Key, (Word)Key:Length , ACE.ADS_STRINGKEY, mode, out found))
            ENDIF
        ELSE
            SELF:ACECALL(ACE.AdsSeek(SELF:m_hIndex, Key, (Word)Key:Length , ACE.ADS_STRINGKEY, mode, out found))
        ENDIF 
        SELF:ACECALL(ACE.AdsAtEOF(SELF:m_hTable, out atEOF))
        SELF:ACECALL(ACE.AdsAtBOF(SELF:m_hTable, out atBOF))
        SUPER:_Found := found != 0
        SUPER:_Eof := atEOF != 0
        SUPER:_Bof := atBOF != 0
        RETURN TRUE

    PRIVATE METHOD AxSetScope(hIndex AS System.IntPtr, usScopeOption AS Word, oScope as OBJECT ) AS OBJECT
        LOCAL aScope    AS Char[]
        LOCAL wBufLen   AS Word
        LOCAL ulRetCode AS DWord
        LOCAL str       AS string
        aScope    := Char[]{(SELF:MAX_KEY_SIZE + 1)}
        wBufLen   := (Word)(SELF:MAX_KEY_SIZE + 1) 
        ulRetCode := ACE.AdsGetScope(hIndex, usScopeOption, aScope, ref wBufLen)
        IF ulRetCode != ACE.AE_NO_SCOPE
            SELF:ACECALL(ulRetCode)
        ENDIF
        str := oScope:ToString()
        IF !String.IsNullOrEmpty(str)
            SELF:ACECALL(ACE.AdsSetScope(hIndex, usScopeOption, str, (Word)str:Length , ACE.ADS_STRINGKEY))
        ELSE
            SELF:ACECALL(ACE.AdsClearScope(hIndex, usScopeOption))
        ENDIF
        // return the previous scope
        IF (ulRetCode == ACE.AE_NO_SCOPE)
            return null
        ELSE
            return SELF:AxAnsi2Unicode(aScope, wBufLen)
        ENDIF
 

    #endregion
    #region Updates 
    
    VIRTUAL METHOD Flush() AS Logic
        RETURN SELF:GoCold()

    VIRTUAL METHOD GoCold() AS Logic
        SELF:ACECALL(ACE.AdsWriteRecord(SELF:m_hTable))
        RETURN SELF:RecordMovement()
 
    VIRTUAL METHOD GoHot() AS Logic
        LOCAL options AS DWord
        LOCAL isTableLocked AS Word
        LOCAL isRecordLocked AS Word
        LOCAL pulRec AS DWord
        LOCAL numRecs AS DWord
        LOCAL ulRetCode AS DWord
        //
        SELF:ACECALL(ACE.AdsGetTableOpenOptions(SELF:m_hTable, OUT options))
        // GoHot must have lock when not exclusive
        IF ((options & ACE.ADS_EXCLUSIVE) != ACE.ADS_EXCLUSIVE)
            // Only allowed when not Readonly
            IF ((options & ACE.ADS_READONLY) == ACE.ADS_READONLY)
                SELF:ADSERROR(ERDD_READONLY, EG_READONLY)
            ENDIF
            SELF:ACECALL(ACE.AdsIsTableLocked(SELF:m_hTable, out isTableLocked))
            IF isTableLocked == 0
                //
                SELF:ACECALL(ACE.AdsIsRecordLocked(SELF:m_hTable, 0, out isRecordLocked))
                IF isRecordLocked == 0
                    pulRec := 0
                    IF SELF:IsADT
                        //
                        ulRetCode := ACE.AdsGetRecordCount(SELF:m_hTable, ACE.ADS_IGNOREFILTERS, out numRecs)
                        IF ulRetCode == 0
                            //
                            ulRetCode := ACE.AdsGetRecordNum(SELF:m_hTable, ACE.ADS_IGNOREFILTERS, out pulRec)
                        ENDIF
                        IF ulRetCode == 0 .AND. numRecs < pulRec .AND. ! SUPER:_Bof .AND. ! SUPER:_Eof
                            //
                            ulRetCode := ACE.AdsLockRecord(SELF:m_hTable, 0)
                            SWITCH ulRetCode
                            case 0
                            CASE ACE.AE_TABLE_NOT_SHARED
                                RETURN TRUE
                            END SWITCH
                            SELF:ACECALL(ulRetCode)
                        ENDIF
                    ENDIF
                    SELF:ADSERROR(ERDD_UNLOCKED, EG_STROVERFLOW, ES_ERROR)
                ENDIF
            ENDIF
        ENDIF
        RETURN TRUE

    VIRTUAL METHOD Zap() AS Logic
        LOCAL options AS DWord
        SELF:ACECALL(ACE.AdsGetTableOpenOptions(SELF:m_hTable, out options))
        // Only allowed when opened exclusively
        IF (options & ACE.ADS_EXCLUSIVE) != ACE.ADS_EXCLUSIVE
            SELF:ADSERROR(ACE.AE_TABLE_NOT_EXCLUSIVE, EG_SHARED , "Zap")
        ENDIF
        // Only allowed when not Readonly
        IF ((options & ACE.ADS_READONLY) == ACE.ADS_READONLY)
            SELF:ADSERROR(ERDD_READONLY, EG_READONLY, "Zap")
        ENDIF
        SELF:ACECALL(ACE.AdsZapTable(SELF:m_hTable))
        RETURN SELF:GoTop()

 
    VIRTUAL METHOD Append(fReleaseLocks AS Logic) AS Logic
        LOCAL result AS DWord
        LOCAL handleType AS Word
        LOCAL isTableLocked AS Word
        //
        IF (fReleaseLocks)
            //
            SELF:ACECALL(ACE.AdsGetHandleType(SELF:m_hTable, OUT handleType))
            IF (handleType != ACE.ADS_CURSOR)
                //
                result := ACE.AdsIsTableLocked(SELF:m_hTable, out isTableLocked)
                IF (isTableLocked == 0)
                    //
                    result := ACE.AdsUnlockTable(SELF:m_hTable)
                    // When Unlock fails because not shared or because not locked, then no problem
                    IF result!= ACE.AE_TABLE_NOT_SHARED .AND. result!= ACE.AE_TABLE_NOT_LOCKED  
                        SELF:ACECALL(result)
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
        SELF:ACECALL(ACE.AdsAppendRecord(SELF:m_hTable))
        result := ACE.AdsLockRecord(SELF:m_hTable, 0)
        IF ((result != ACE.AE_TABLE_NOT_SHARED) )
            SELF:ACECALL(result)
        ENDIF
        RETURN SELF:RecordMovement()

    VIRTUAL METHOD DeleteRecord() AS Logic
        SELF:ACECALL(ACE.AdsDeleteRecord(SELF:m_hTable))
        RETURN TRUE

 
#endregion

#region Properties
    PROPERTY ACEConnectionHandle AS IntPtr GET m_hConnection
    PROPERTY ACEIndexHandle AS IntPtr GET m_hIndex
    PROPERTY ACETableHandle AS IntPtr GET m_hTable
    PROPERTY BOF AS Logic GET SUPER:_Bof
    PROPERTY EOF AS Logic GET SUPER:_Eof
    PROPERTY Found as LOGIC GET SUPER:_Found
    PRIVATE PROPERTY IsADT AS LOGIC GET m_usTableType == ACE.ADS_ADT
    VIRTUAL PROPERTY SysName AS STRING GET typeof(ADSRDD):ToString()


    PROPERTY Deleted as LOGIC
    GET
        LOCAL isDeleted AS Word
        LOCAL ulRetCode AS DWord
        //
        ulRetCode := ACE.AdsIsRecordDeleted(SELF:m_hTable, out isDeleted)
        IF (ulRetCode == ACE.AE_NO_CURRENT_RECORD)
            IF (SUPER:_Eof)
                return FALSE
            ELSE
                return TRUE
            ENDIF
        ELSE
            SELF:ACECALL(ulRetCode)
        ENDIF
        RETURN isDeleted != 0

    END GET 
    END PROPERTY


#endregion


#region Locking
 

    VIRTUAL METHOD Lock(lockInfo AS DBLOCKINFO) AS Logic
    LOCAL lRecno := 0 AS DWord
    LOCAL result := 0 AS DWord
    LOCAL handleType AS Word
    LOCAL isLocked  AS Word
    //
    lockInfo:Result := FALSE
    IF (lockInfo:recID == null)
        //
        lRecno := SELF:_Recno
    ELSE
        //
        TRY
            lRecno := System.Convert.ToUInt32(lockInfo:recID)
        CATCH e as Exception
            SELF:ADSERROR(ERDD_DATATYPE, EG_DATATYPE, "Lock", e:Message)
        END TRY
    ENDIF
    IF (lockInfo:@@Method == DBLockInfo.LockMethod.File)
        result := ACE.AdsLockTable(SELF:m_hTable)
        SELF:ACECALL(result);

    ENDIF
    SELF:ACECALL(ACE.AdsGetHandleType(SELF:m_hTable, out handleType))
    IF lRecno == 0 .AND. handleType != ACE.ADS_CURSOR
        //
        SELF:ACECALL(ACE.AdsIsTableLocked(SELF:m_hTable, out isLocked))
        IF isLocked == 0
            //
            result := ACE.AdsUnlockTable(SELF:m_hTable)
            if result != ACE.AE_TABLE_NOT_LOCKED  .and. ;
                result != ACE.AE_TABLE_NOT_SHARED
                SELF:ACECALL(result)
                result  := ACE.AdsLockRecord(SELF:m_hTable, lRecno)
            ENDIF
        ENDIF
    ENDIF

    IF result != ACE.AE_TABLE_NOT_SHARED
        SELF:ACECALL(result)
        lockInfo:Result := TRUE
    ENDIF
    RETURN TRUE

 

 
    VIRTUAL METHOD Unlock(recordID AS Object) AS Logic
    LOCAL result AS DWord
    LOCAL numRecord := 0 AS DWord
    TRY
        numRecord := System.Convert.ToUInt32(recordID)
    CATCH e as Exception
        //
        SELF:ADSERROR(ERDD_DATATYPE, EG_DATATYPE, "Unlock",e:Message)
    END TRY
    IF (numRecord == 0)
        result := ACE.AdsUnlockTable(SELF:m_hTable)
    ELSE
        result := ACE.AdsUnlockRecord(SELF:m_hTable, numRecord)
    ENDIF
    IF result != ACE.AE_TABLE_NOT_SHARED
        SELF:ACECALL(result)
    ENDIF
    RETURN TRUE

 

#endregion

#region Filters
    VIRTUAL METHOD ClearFilter() AS Logic
        LOCAL ulRetCode AS DWord
        IF (SELF:m_hTable != System.IntPtr.Zero)
            // Clear normal filter
            ulRetCode := ACE.AdsClearFilter(SELF:m_hTable)
            IF (ulRetCode != ACE.AE_NO_FILTER )
                SELF:ACECALL(ulRetCode)
            ENDIF
            // Clear optimized filter
            ulRetCode := ACE.AdsClearAOF(SELF:m_hTable)
            IF (ulRetCode != ACE.AE_NO_FILTER )
                SELF:ACECALL(ulRetCode)
            ENDIF
        ENDIF
        SELF:_FilterInfo := DbFilterInfo{}
        RETURN TRUE

    VIRTUAL METHOD SetFilter(fi AS DBFILTERINFO) AS Logic
        LOCAL ulRetCode AS DWord
        // Get the current date format so we can handle literal dates in the filter
        SELF:ACECALL(IIF(SELF:AxCheckVODateFormat(),0,1))
        IF String.IsNullOrEmpty(fi:FilterText)
            // clear filter
            // Ignore "No filter" error
            ulRetCode := ACE.AdsClearFilter(SELF:m_hTable)
            IF ulRetCode != ACE.AE_NO_FILTER
                SELF:ACECALL(ulRetCode)
            ENDIF
            ulRetCode := ACE.AdsClearAOF(SELF:m_hTable)
            IF ulRetCode != ACE.AE_NO_FILTER
                SELF:ACECALL(ulRetCode)
            ENDIF
        ELSE
            IF Runtime.State.Optimize
                SELF:ACECALL(ACE.AdsSetAOF(SELF:m_hTable, fi:FilterText, (WORD) ACE.ADS_RESOLVE_DYNAMIC | ACE.ADS_DYNAMIC_AOF))
            ELSE
                SELF:ACECALL(ACE.AdsSetFilter(SELF:m_hTable, fi:FilterText))
            ENDIF
        ENDIF
        fi:Active := TRUE
        SELF:_FilterInfo := fi
        RETURN TRUE

#endregion
#region Relations 
    VIRTUAL METHOD ClearRel() AS Logic
        IF SELF:m_hTable != System.IntPtr.Zero
            SELF:ACECALL(ACE.AdsClearRelation(SELF:m_hTable))
        ENDIF
        RETURN SUPER:ClearRel()

    VIRTUAL METHOD SetRel(relinfo AS DBRELINFO) AS Logic
        IF relinfo:Child:Driver != SELF:m_strDriver
            SELF:ADSERROR(ERDD_UNSUPPORTED, EG_UNSUPPORTED, "SetRel", "Related workareas must be opened with the same driver.")
            RETURN FALSE
        ENDIF
        VAR child := relInfo:Child astype ADSRDD
        SELF:ACECALL(ACE.AdsSetRelation(SELF:m_hTable, child:ACEIndexHandle, relinfo:Key))
        RETURN SUPER:SetRel(relInfo)

#endregion

#region Info
VIRTUAL METHOD FieldInfo(uiPos AS Long, uiOrdinal AS INT, oNewValue as OBJECT) AS OBJECT
    LOCAL fieldType AS Word
    LOCAL length    AS DWord
    LOCAL ulRetCode AS DWord
    SWITCH uiOrdinal
    CASE DBS_BLOB_TYPE
        SELF:ACECALL(ACE.AdsGetFieldType(SELF:m_hTable, (DWord)(uiPos + 1) ,  out fieldType))
        //
        SWITCH fieldType
        CASE ACE.ADS_MEMO
        CASE ACE.ADS_BINARY
        CASE ACE.ADS_IMAGE
            return "?"
        END SWITCH
    CASE DBS_BLOB_LEN
        //
        IF (SUPER:_fields[uiPos + 1]:fieldType != DBFieldType.Memo  )
            return -1
        ELSE
            ulRetCode := ACE.AdsGetMemoLength(SELF:m_hTable, (DWord)(uiPos + 1) , out length)
            IF (ulRetCode != ACE.AE_INVALID_FIELD_TYPE)
                SELF:ACECALL(ulRetCode)
            ELSE
                RETURN -1
            ENDIF
            return length
        ENDIF
        
    CASE DBS_BLOB_POINTER
        return null

    END SWITCH
    RETURN SUPER:FieldInfo(uiPos, uiOrdinal, oNewValue)

 
 VIRTUAL METHOD Info(uiOrdinal AS LONG, oNewValue as Object) AS OBJECT
    LOCAL aDate AS Char[]
    LOCAL DateLen AS Word
    LOCAL julDate AS real8
    LOCAL isLocked as WORD
    LOCAL isFound as Word
    LOCAL numLocks AS Word
    LOCAL options AS DWord
    LOCAL memoBlockSize AS Word
    LOCAL ulRetCode AS DWord
    IF uiOrdinal <= DBI_MEMOFIELD  
        SWITCH uiOrdinal
        CASE DBI_ISDBF
            return !SELF:IsADT
        CASE DBI_CANPUTREC
            return FALSE
        CASE DBI_GETHEADERSIZE
            IF SELF:IsADT
                return 400 + SUPER:_Fields:Length * 200
            ELSE
                return 2 + SUPER:_Fields:Length * 32 + 2
            ENDIF
        CASE DBI_LASTUPDATE
            aDate := Char[]{ACE.ADS_MAX_DATEMASK}
            DateLen := (WORD) aDate:Length
            SELF:ACECALL(ACE.AdsSetDateFormat("MM/DD/YY"))
            SELF:ACECALL(ACE.AdsGetLastTableUpdate(SELF:m_hTable, aDate, ref DateLen))
            SELF:ACECALL(ACEUNPUB.AdsConvertStringToJulian(aDate, DateLen, out julDate))
            if !SELF:AxCheckVODateFormat()
                SELF:ACECALL(1)
            ENDIF
            return (Long)julDate
        CASE DBI_SETDELIMITER 
        CASE DBI_VALIDBUFFER 
        CASE DBI_LOCKOFFSET 
        CASE DBI_MEMOHANDLE 
        CASE DBI_NEWINDEXLOCK 
        CASE DBI_MEMOFIELD 
            return NULL
        CASE DBI_GETRECSIZE
            RETURN SUPER:_RecordLength
        CASE DBI_GETLOCKARRAY
            SELF:ACECALL(ACE.AdsGetNumLocks(SELF:m_hTable, out numLocks))
            IF numLocks > 0
                IF SELF:m_palRlocks == null .OR. SELF:m_palRlocks:Length < numLocks
                    SELF:m_palRlocks := DWord[]{numLocks}
                ENDIF
                SELF:ACECALL(ACE.AdsGetAllLocks(SELF:m_hTable, SELF:m_palRlocks, ref numLocks))
                RETURN SELF:m_palRlocks
            ELSE
                return NULL
            ENDIF
        CASE DBI_TABLEEXT
            IF SELF:IsADT
                return ".ADT"
            ELSE
                return ".DBF"
            ENDIF
        CASE DBI_ISFLOCK
            SELF:ACECALL(ACE.AdsIsTableLocked(SELF:m_hTable, out isLocked))
            RETURN isLocked != 0
        CASE DBI_FILEHANDLE
            RETURN IntPtr.Zero
        CASE DBI_FULLPATH
            RETURN SUPER:_FileName
        CASE DBI_ISANSI
            RETURN FALSE
        CASE DBI_FOUND
            SELF:ACECALL(ACE.AdsIsFound(SELF:m_hTable, OUT isFound))
            return isFound != 0
        CASE DBI_LOCKCOUNT
            SELF:ACECALL(ACE.AdsGetNumLocks(SELF:m_hTable, out numLocks))
            return numLocks
        CASE DBI_SHARED
            SELF:ACECALL(ACE.AdsGetTableOpenOptions(SELF:m_hTable, out options))
            return (options & ACE.ADS_EXCLUSIVE) != ACE.ADS_EXCLUSIVE
        CASE DBI_MEMOEXT
            SWITCH SELF:m_usTableType 
            CASE ACE.ADS_ADT
                return ".ADM"
            CASE ACE.ADS_CDX
            CASE ACE.ADS_VFP
                return ".FPT"
            OTHERWISE
            return ".DBT"
            END SWITCH
            
        CASE DBI_MEMOBLOCKSIZE
            ulRetCode := ACE.AdsGetMemoBlockSize(SELF:m_hTable, out memoBlockSize)
            IF ulRetCode != ACE.AE_NO_MEMO_FILE 
                SELF:ACECALL(ulRetCode)
                RETURN memoBlockSize
            ELSE
                return NULL
            ENDIF
        CASE DBI_CODEPAGE
            return SELF:m_Encoding:CodePage
        CASE DBI_DB_VERSION
            return 0
        CASE DBI_RDD_VERSION
            return System.Reflection.Assembly.GetExecutingAssembly():GetName():Version:ToString()
        END SWITCH
    ELSE
        SWITCH uiOrdinal
        CASE BLOB_INFO_HANDLE
        CASE BLOB_FILE_RECOVER 
        CASE BLOB_FILE_INTEGRITY 
        CASE BLOB_OFFSET 
        CASE BLOB_POINTER 
        CASE BLOB_LEN 
        CASE BLOB_TYPE 
        CASE BLOB_EXPORT 
        CASE BLOB_ROOT_UNLOCK 
        CASE BLOB_ROOT_PUT 
        CASE BLOB_ROOT_GET 
        CASE BLOB_ROOT_LOCK 
        CASE BLOB_IMPORT 
        CASE BLOB_DIRECT_PUT 
        CASE BLOB_DIRECT_GET 
        CASE BLOB_GET 
        CASE BLOB_DIRECT_EXPORT 
        CASE BLOB_DIRECT_IMPORT 
        CASE BLOB_NMODE 
        CASE BLOB_EXPORT_APPEND 
        CASE BLOB_EXPORT_OVERWRITE 
            return null
        CASE  DBI_RL_AND 
        CASE DBI_RL_CLEAR 
        CASE DBI_RL_COUNT 
        CASE DBI_RL_DESTROY 
        CASE DBI_RL_EXFILTER 
        CASE DBI_RL_GETFILTER 
        CASE DBI_RL_HASMAYBE 
        CASE DBI_RL_LEN 
        CASE DBI_RL_MAYBEEVAL 
        CASE DBI_RL_NEW 
        CASE DBI_RL_NEWDUP 
        CASE DBI_RL_NEWQUERY 
        CASE DBI_RL_NEXTRECNO 
        CASE DBI_RL_NOT 
        CASE DBI_RL_OR 
        CASE DBI_RL_PREVRECNO 
        CASE DBI_RL_SET 
        CASE DBI_RL_SETFILTER 
        CASE DBI_RL_TEST  
            return null
        CASE DBI_GET_ACE_TABLE_HANDLE
            Return SELF:m_hTable
        END SWITCH

    ENDIF
    RETURN SUPER:Info(uiOrdinal, oNewValue)

     
#endregion

#region Unsupported
    VIRTUAL METHOD AppendLock(uiMode AS DbLockMode) AS Logic
        RETURN SELF:Unsupported("AppendLock")


    VIRTUAL METHOD BlobInfo(uiPos AS DWord, uiOrdinal AS DWord) AS OBJECT
        SELF:Unsupported("BlobInfo")
        RETURN NULL

    VIRTUAL METHOD ForceRel() AS Logic
        RETURN SELF:Unsupported("ForceRel")

    VIRTUAL METHOD GetRec() AS Byte[]
        SELF:Unsupported("GetRec")
        RETURN NULL

    VIRTUAL METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC  
        RETURN SELF:Unsupported("HeaderLock")


 


 

#endregion

END CLASS
