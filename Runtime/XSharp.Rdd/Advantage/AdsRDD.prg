//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD
USING XSharp.RDD.Enums

/// <summary>
/// The AdsRDD class. 
/// </summary>
CLASS XSharp.ADS.AdsRDD Inherit Workarea
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
            SELF:ADSERROR( ulRetCode, 27)
        ENDIF
        RETURN 


    METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD) AS void
        SELF:ADSERROR(iSubCode, iGenCode, String.Empty, String.Empty, XSharp.Severity.ES_ERROR)

    METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, iSeverity as DWORD) AS void
        SELF:ADSERROR(iSubCode, iGenCode, String.Empty, String.Empty, iSeverity)

    METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS string) AS void
        SELF:ADSERROR(iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR)

    METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS string, strMessage AS string) AS void
        SELF:ADSERROR(iSubCode, iGenCode, strFunction,strMessage, XSharp.Severity.ES_ERROR)

    METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS string, strMessage AS string, iSeverity AS DWORD) AS void
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
        SELF:ADSERROR(ERDD.UNSUPPORTED, XSharp.Gencode.EG_UnSupported, strFunctionName)
        RETURN FALSE


#endregion
#region Helper Methods
    PRIVATE METHOD ACEORDER() AS System.IntPtr
        IF (SELF:m_hIndex != System.IntPtr.Zero)
            RETURN SELF:m_hIndex
        ENDIF
        RETURN SELF:m_hTable

    PRIVATE METHOD AxCheckVODeletedFlag() AS DWord
        SELF:ACECALL(ACE.AdsShowDeleted(IIF(RuntimeState.Deleted,(Word)0 ,(Word)1 )))
        RETURN 0
  
    PRIVATE METHOD AxCheckVODateFormat() AS Logic
        SELF:ACECALL(ACE.AdsSetDateFormat(RuntimeState.DateFormat))
        SELF:ACECALL(ACE.AdsSetExact(IIF(RuntimeState.Exact,(Word)1 ,(Word)0 )))
        SELF:ACECALL(ACE.AdsSetDecimals((Word)RuntimeState.Decimals ))
        SELF:ACECALL(ACE.AdsSetEpoch((Word)RuntimeState.Epoch ))
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
            SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_DataType, "GoToId",e:Message)
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
            SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_NOORDER, "Seek")
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
                SELF:ADSERROR(ERDD.READONLY, XSharp.Gencode.EG_READONLY)
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
                    SELF:ADSERROR(ERDD.UNLOCKED, XSharp.Gencode.EG_STROVERFLOW, XSharp.Severity.ES_ERROR)
                ENDIF
            ENDIF
        ENDIF
        RETURN TRUE

    VIRTUAL METHOD Zap() AS Logic
        LOCAL options AS DWord
        SELF:ACECALL(ACE.AdsGetTableOpenOptions(SELF:m_hTable, out options))
        // Only allowed when opened exclusively
        IF (options & ACE.ADS_EXCLUSIVE) != ACE.ADS_EXCLUSIVE
            SELF:ADSERROR(ACE.AE_TABLE_NOT_EXCLUSIVE, XSharp.Gencode.EG_SHARED , "Zap")
        ENDIF
        // Only allowed when not Readonly
        IF ((options & ACE.ADS_READONLY) == ACE.ADS_READONLY)
            SELF:ADSERROR(ERDD.READONLY, XSharp.Gencode.EG_READONLY, "Zap")
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
            SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_DATATYPE, "Lock", e:Message)
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
        SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_DATATYPE, "Unlock",e:Message)
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
            IF RuntimeState.Optimize
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
            SELF:ADSERROR(ERDD.UNSUPPORTED, XSharp.Gencode.EG_UNSUPPORTED, "SetRel", "Related workareas must be opened with the same driver.")
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
    CASE DbFieldInfo.DBS_BLOB_TYPE
        SELF:ACECALL(ACE.AdsGetFieldType(SELF:m_hTable, (DWord)(uiPos + 1) ,  out fieldType))
        //
        SWITCH fieldType
        CASE ACE.ADS_MEMO
        CASE ACE.ADS_BINARY
        CASE ACE.ADS_IMAGE
            return "?"
        END SWITCH
    CASE DbFieldInfo.DBS_BLOB_LEN
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
        
    CASE DbFieldInfo.DBS_BLOB_POINTER
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
    IF uiOrdinal <= DbInfo.DBI_MEMOFIELD  
        SWITCH uiOrdinal
        CASE DbInfo.DBI_ISDBF
            return !SELF:IsADT
        CASE DbInfo.DBI_CANPUTREC
            return FALSE
        CASE DbInfo.DBI_GETHEADERSIZE
            IF SELF:IsADT
                return 400 + SUPER:_Fields:Length * 200
            ELSE
                return 2 + SUPER:_Fields:Length * 32 + 2
            ENDIF
        CASE DbInfo.DBI_LASTUPDATE
            aDate := Char[]{ACE.ADS_MAX_DATEMASK}
            DateLen := (WORD) aDate:Length
            SELF:ACECALL(ACE.AdsSetDateFormat("MM/DD/YY"))
            SELF:ACECALL(ACE.AdsGetLastTableUpdate(SELF:m_hTable, aDate, ref DateLen))
            SELF:ACECALL(ACEUNPUB.AdsConvertStringToJulian(aDate, DateLen, out julDate))
            if !SELF:AxCheckVODateFormat()
                SELF:ACECALL(1)
            ENDIF
            return (Long)julDate
        CASE DbInfo.DBI_SETDELIMITER 
        CASE DbInfo.DBI_VALIDBUFFER 
        CASE DbInfo.DBI_LOCKOFFSET 
        CASE DbInfo.DBI_MEMOHANDLE 
        CASE DbInfo.DBI_NEWINDEXLOCK 
        CASE DbInfo.DBI_MEMOFIELD 
            return NULL
        CASE DbInfo.DBI_GETRECSIZE
            RETURN SUPER:_RecordLength
        CASE DbInfo.DBI_GETLOCKARRAY
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
        CASE DbInfo.DBI_TABLEEXT
            IF SELF:IsADT
                return ".ADT"
            ELSE
                return ".DBF"
            ENDIF
        CASE DbInfo.DBI_ISFLOCK
            SELF:ACECALL(ACE.AdsIsTableLocked(SELF:m_hTable, out isLocked))
            RETURN isLocked != 0
        CASE DbInfo.DBI_FILEHANDLE
            RETURN IntPtr.Zero
        CASE DbInfo.DBI_FULLPATH
            RETURN SUPER:_FileName
        CASE DbInfo.DBI_ISANSI
            RETURN FALSE
        CASE DbInfo.DBI_FOUND
            SELF:ACECALL(ACE.AdsIsFound(SELF:m_hTable, OUT isFound))
            return isFound != 0
        CASE DbInfo.DBI_LOCKCOUNT
            SELF:ACECALL(ACE.AdsGetNumLocks(SELF:m_hTable, out numLocks))
            return numLocks
        CASE DbInfo.DBI_SHARED
            SELF:ACECALL(ACE.AdsGetTableOpenOptions(SELF:m_hTable, out options))
            return (options & ACE.ADS_EXCLUSIVE) != ACE.ADS_EXCLUSIVE
        CASE DbInfo.DBI_MEMOEXT
            SWITCH SELF:m_usTableType 
            CASE ACE.ADS_ADT
                return ".ADM"
            CASE ACE.ADS_CDX
            CASE ACE.ADS_VFP
                return ".FPT"
            OTHERWISE
            return ".DBT"
            END SWITCH
            
        CASE DbInfo.DBI_MEMOBLOCKSIZE
            ulRetCode := ACE.AdsGetMemoBlockSize(SELF:m_hTable, out memoBlockSize)
            IF ulRetCode != ACE.AE_NO_MEMO_FILE 
                SELF:ACECALL(ulRetCode)
                RETURN memoBlockSize
            ELSE
                return NULL
            ENDIF
        CASE DbInfo.DBI_CODEPAGE
            return SELF:m_Encoding:CodePage
        CASE DbInfo.DBI_DB_VERSION
            return 0
        CASE DbInfo.DBI_RDD_VERSION
            return System.Reflection.Assembly.GetExecutingAssembly():GetName():Version:ToString()
        END SWITCH
    ELSE
        SWITCH uiOrdinal
        CASE DbInfo.BLOB_INFO_HANDLE
        CASE DbInfo.BLOB_FILE_RECOVER 
        CASE DbInfo.BLOB_FILE_INTEGRITY 
        CASE DbInfo.BLOB_OFFSET 
        CASE DbInfo.BLOB_POINTER 
        CASE DbInfo.BLOB_LEN 
        CASE DbInfo.BLOB_TYPE 
        CASE DbInfo.BLOB_EXPORT 
        CASE DbInfo.BLOB_ROOT_UNLOCK 
        CASE DbInfo.BLOB_ROOT_PUT 
        CASE DbInfo.BLOB_ROOT_GET 
        CASE DbInfo.BLOB_ROOT_LOCK 
        CASE DbInfo.BLOB_IMPORT 
        CASE DbInfo.BLOB_DIRECT_PUT 
        CASE DbInfo.BLOB_DIRECT_GET 
        CASE DbInfo.BLOB_GET 
        CASE DbInfo.BLOB_DIRECT_EXPORT 
        CASE DbInfo.BLOB_DIRECT_IMPORT 
        CASE DbInfo.BLOB_NMODE 
        CASE DbInfo.BLOB_EXPORT_APPEND 
        CASE DbInfo.BLOB_EXPORT_OVERWRITE 
            return null
        CASE DbInfo.DBI_RL_AND 
        CASE DbInfo.DBI_RL_CLEAR 
        CASE DbInfo.DBI_RL_COUNT 
        CASE DbInfo.DBI_RL_DESTROY 
        CASE DbInfo.DBI_RL_EXFILTER 
        CASE DbInfo.DBI_RL_GETFILTER 
        CASE DbInfo.DBI_RL_HASMAYBE 
        CASE DbInfo.DBI_RL_LEN 
        CASE DbInfo.DBI_RL_MAYBEEVAL 
        CASE DbInfo.DBI_RL_NEW 
        CASE DbInfo.DBI_RL_NEWDUP 
        CASE DbInfo.DBI_RL_NEWQUERY 
        CASE DbInfo.DBI_RL_NEXTRECNO 
        CASE DbInfo.DBI_RL_NOT 
        CASE DbInfo.DBI_RL_OR 
        CASE DbInfo.DBI_RL_PREVRECNO 
        CASE DbInfo.DBI_RL_SET 
        CASE DbInfo.DBI_RL_SETFILTER 
        CASE DbInfo.DBI_RL_TEST  
            return null
        CASE DbInfo.DBI_GET_ACE_TABLE_HANDLE
            return self:m_hTable
		otherwise
			RETURN SUPER:Info(uiOrdinal, oNewValue)
        end switch


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
