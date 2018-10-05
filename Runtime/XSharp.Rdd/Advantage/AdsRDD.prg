//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.IO
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

/// <summary>
/// The AdsRDD class. 
/// </summary>
CLASS XSharp.ADS.AdsRDD INHERIT Workarea
  #region Fields
  //PRIVATE m_CallbackFn AS CallbackFn
  //PRIVATE m_CalltraceFile AS System.IO.StreamWriter
  //PRIVATE m_iProgress AS Long
  PRIVATE _aRlocks AS DWORD[]
  PRIVATE _dbfName AS STRING
  INTERNAL _Recno  AS DWORD
  INTERNAL _Encoding AS System.Text.Encoding
  INTERNAL _Connection AS System.IntPtr
  INTERNAL _Index AS System.IntPtr
  INTERNAL _Table AS System.IntPtr
  INTERNAL _Collation AS STRING
  INTERNAL _Driver AS STRING
  INTERNAL _CheckRights AS WORD
  INTERNAL _LockType AS WORD
  INTERNAL _TableType AS WORD
  INTERNAL _MaxKeySize AS WORD
  INTERNAL _Ansi  AS LOGIC
  INTERNAL _HasMemo AS LOGIC
  INTERNAL _fieldCount AS LONG
  //	PRIVATE _CalltraceFile AS StreamWriter
  
  #endregion
  
  /// <summary>Create instande of RDD </summary>
  CONSTRUCTOR()
    SUPER()
    SELF:_Order         := AdsIndex{SELF}
    SELF:_Memo          := AdsMemo{SELF}
    SELF:_Table       := System.IntPtr.Zero
    SELF:_Index       := System.IntPtr.Zero
    SELF:_Connection  := System.IntPtr.Zero
    SELF:_Driver    := String.Empty
    SELF:_Collation := String.Empty
    SELF:_Driver    := "ADSRDD"
    SELF:_dbfName      := String.Empty
    
    #region Helper Methods that check for error conditions
  INTERNAL STATIC METHOD _HasFlag(dw AS DWORD, flag AS DWORD) AS LOGIC
    RETURN _AND(dw, flag) == flag
    
  INTERNAL METHOD _CheckError(ulRetCode AS DWORD) AS LOGIC
    IF ulRetCode != 0
      SELF:ADSERROR( ulRetCode, EG_NOCLASS)
      RETURN FALSE
    ENDIF
    RETURN TRUE
    
  INTERNAL METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD) AS VOID
    SELF:ADSERROR(iSubCode, iGenCode, String.Empty, String.Empty, XSharp.Severity.ES_ERROR)
    
  INTERNAL METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, iSeverity AS DWORD) AS VOID
    SELF:ADSERROR(iSubCode, iGenCode, String.Empty, String.Empty, iSeverity)
    
  INTERNAL METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING) AS VOID
    SELF:ADSERROR(iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR)
    
  INTERNAL METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING) AS VOID
    SELF:ADSERROR(iSubCode, iGenCode, strFunction,strMessage, XSharp.Severity.ES_ERROR)
    
  INTERNAL METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING, iSeverity AS DWORD) AS VOID
    LOCAL lastError AS DWORD
    LOCAL message AS CHAR[]
    LOCAL wBufLen AS WORD
    LOCAL oError AS RddError
    IF strMessage == String.Empty
      //
      message := CHAR[]{ACE.ADS_MAX_ERROR_LEN}
      wBufLen := (WORD) message:Length
      IF ACE.AdsGetLastError(OUT lastError, message, REF wBufLen) == 0 .AND. lastError != 0 .AND. wBufLen > 0
        strMessage := STRING{message, 0, wBufLen}
      ENDIF
    ENDIF
    oError := RddError{strMessage}
    oError:SubCode := iSubCode
    oError:Gencode := iGenCode
    oError:SubSystem := SELF:_Driver
    oError:Severity := iSeverity
    oError:FuncSym  := strFunction
    oError:FileName := SELF:_dbfName
    THROW oError
    
    
    #endregion
  #region Helper Methods
  
  INTERNAL METHOD _CheckVODeletedFlag() AS DWORD
    RETURN ACE.AdsShowDeleted(IIF(RuntimeState.Deleted,(WORD)0 ,(WORD)1 ))
    
  INTERNAL METHOD _CheckVODateFormat() AS LOGIC
    ACE.AdsSetDateFormat(RuntimeState.DateFormat)
    ACE.AdsSetExact(IIF(RuntimeState.Exact,1 , 0 ))
    ACE.AdsSetDecimals((WORD)RuntimeState.Decimals )
    ACE.AdsSetEpoch((WORD)RuntimeState.Epoch )
    RETURN TRUE
    
  INTERNAL METHOD _CheckRDDInfo() AS VOID
    LOCAL oRet := NULL_OBJECT AS OBJECT
    IF AX_AXSLocking()
      SELF:_LockType := ACE.ADS_PROPRIETARY_LOCKING
    ELSE
      SELF:_LockType := ACE.ADS_COMPATIBLE_LOCKING
    ENDIF
    IF AX_RightsCheck()
      SELF:_CheckRights := ACE.ADS_CHECKRIGHTS
    ELSE
      SELF:_CheckRights := ACE.ADS_IGNORERIGHTS
    ENDIF
    IF CoreDb.RddInfo(_SET_CONNECTION_HANDLE, REF oRet) .AND. oRet != NULL_OBJECT
      IF oRet IS IntPtr
        SELF:_Connection := (intPtr) oRet
      ELSE
        SELF:_Connection := IntPtr.Zero
      ENDIF
    ELSE
      SELF:_Connection := IntPtr.Zero
    ENDIF
    IF SELF:_TableType == ACE.ADS_ADT .OR. SELF:_TableType == ACE.ADS_VFP
      oRet := NULL
      IF CoreDb.RddInfo(_SET_COLLATION_NAME, REF oRet) .AND. oRet != NULL
        SELF:_Collation := (STRING) oRet
      ELSE
        SELF:_Collation := string.Empty
      ENDIF
    ENDIF
    RETURN
    
  INTERNAL METHOD _Ansi2Unicode(source AS CHAR[], iLength AS LONG) AS STRING
    TRY
      VAR bytes := SELF:_Encoding:GetBytes(source, 0, iLength)
      RETURN SELF:_Encoding:GetString(bytes)
    CATCH 
      RETURN String.Empty
    END TRY
    
    #endregion
    
  #region Open and Close
  PRIVATE METHOD _FieldSub() AS LOGIC
    LOCAL num AS DWORD
    LOCAL sb AS CHAR[]
    LOCAL wLen AS WORD
    LOCAL fi AS RddFieldInfo
    LOCAL wDecimals AS WORD
    LOCAL wFields AS WORD
    SELF:_CheckError(ACE.AdsGetNumFields(SELF:_Table, OUT wFields))
    IF !SELF:SetFieldExtent(wFields)
      RETURN FALSE
    ENDIF
    _fieldCount := wFields
    sb := CHAR[]{ACE.ADS_MAX_FIELD_NAME+1}
    FOR num := 1u TO wFields STEP 1
      LOCAL usType AS WORD
      LOCAL cName AS STRING
      SELF:_CheckError(ACE.AdsGetFieldType(SELF:_Table, num, OUT usType))
      LOCAL ulLength AS DWORD
      SELF:_CheckError(ACE.AdsGetFieldLength(SELF:_Table, num, OUT ulLength))
      wLen := (WORD) sb:Length
      SELF:_CheckError(ACE.AdsGetFieldName(SELF:_Table, (WORD)num, sb, REF wLen))
      cName := STRING{sb,0, wLen}
      fi := RddFieldInfo{cName,DbFieldType.Character,1,0}
      fi:Length := (INT) ulLength
      
      SWITCH usType
    CASE ACE.ADS_TIMESTAMP
      CASE ACE.ADS_MODTIME
          fi:Length := 22
        fi:fieldType := DbFieldType.Character
      CASE ACE.ADS_TIME
          fi:Length := 11
        fi:fieldType := DbFieldType.Character
      CASE ACE.ADS_MONEY
      CASE ACE.ADS_ROWVERSION
          fi:Length := 22
        fi:fieldType := DbFieldType.Character
    CASE ACE.ADS_STRING
      CASE ACE.ADS_VARCHAR
      CASE ACE.ADS_CISTRING
      CASE ACE.ADS_VARCHAR_FOX
      CASE ACE.ADS_NCHAR
      CASE ACE.ADS_NVARCHAR
        fi:fieldType := DbFieldType.Character
    CASE ACE.ADS_MEMO
      CASE ACE.ADS_BINARY
      CASE ACE.ADS_IMAGE
      CASE ACE.ADS_NMEMO
          fi:fieldType := DbFieldType.Memo
        fi:Length := 10
      CASE ACE.ADS_RAW
      CASE ACE.ADS_VARBINARY_FOX
        fi:fieldType := DbFieldType.Memo
    CASE ACE.ADS_SHORTINT
    CASE ACE.ADS_AUTOINC
    CASE ACE.ADS_INTEGER
      CASE ACE.ADS_NUMERIC
          fi:fieldType := DbFieldType.Number
          IF usType == ACE.ADS_SHORTINT
            fi:Length := 6
          ELSEIF usType == ACE.ADS_AUTOINC
            fi:Length := 10
          ELSEIF usType == ACE.ADS_INTEGER
            fi:Length := 11
          ELSE
            fi:Length := (INT) ulLength
          ENDIF
          SELF:_CheckError(ACE.AdsGetFieldDecimals(SELF:_Table, num, OUT wDecimals))
        fi:Decimals := wDecimals
      CASE ACE.ADS_LOGICAL
        fi:fieldType := DbFieldType.Logic
    CASE ACE.ADS_DATE
      CASE ACE.ADS_COMPACTDATE
          fi:fieldType := DbFieldType.Date
        fi:Length := 8
    CASE ACE.ADS_DOUBLE
      CASE ACE.ADS_CURDOUBLE
          fi:Length := 20
          fi:fieldType := DbFieldType.Number
          SELF:_CheckError(ACE.AdsGetFieldDecimals(SELF:_Table, num, OUT wDecimals))
        fi:Decimals := wDecimals
      OTHERWISE
          SELF:ADSERROR(ERDD_CORRUPT_HEADER, EG_DATATYPE)
        RETURN FALSE
      END SWITCH
      fi:Alias := NULL
      IF ! SELF:AddField(fi)
        RETURN FALSE
      ENDIF
    NEXT
    RETURN TRUE
    
  PRIVATE METHOD _GetFieldInfo(strFieldDef REF STRING ) AS LOGIC
    strFieldDef := ""
    FOREACH fld AS RDDFieldInfo IN SUPER:_Fields
      strFieldDef += fld:Name+", "
      SWITCH fld:FieldType
      CASE DbFieldType.Character
          strFieldDef += "C,"
          // allow clipper field lengths
        strFieldDef += (fld:Length + fld:Decimals*256):ToString()+";" 
      CASE DbFieldType.Number
      CASE DbFieldType.Integer
        strFieldDef += "N;"+fld:Length:ToString()+", "+fld:Decimals:ToString()+";"
      CASE DbFieldType.Date
        strFieldDef += "D;"
      CASE DbFieldType.Logic
        strFieldDef += "L;"
      CASE DbFieldType.Memo
          IF fld:Length == 23 .AND. fld:Decimals == 4
            strFieldDef += "Money;"
          ELSE
            _HasMemo := TRUE
            strFieldDef += "M;"
        ENDIF
      OTHERWISE
          SELF:ADSERROR(ERDD_CORRUPT, EG_CORRUPTION)
        RETURN FALSE                
      END SWITCH
    NEXT
    RETURN TRUE
    
  INTERNAL METHOD _SetPaths() AS DWORD
    IF !string.IsNullOrEmpty(SetDefault()) 
      SELF:_CheckError(ACE.AdsSetDefault(SetDefault()))
    ENDIF
    IF !string.IsNullOrEmpty(SetPath()) 
      SELF:_CheckError(ACE.AdsSetSearchPath(SetPath()))
    ENDIF
    RETURN 0u
    
    /// <inheritdoc />
  VIRTUAL METHOD Open(info AS DbOpenInfo) AS LOGIC
    LOCAL openmode AS WORD
    LOCAL alias AS STRING
    LOCAL charset AS WORD
    LOCAL usTableType AS WORD
    LOCAL fileName AS STRING
    //
    openmode := 0
    SELF:_CheckRDDInfo()
    alias := Path.GetFileNameWithoutExtension(info:Alias)
    charset := IIF (RuntimeState.CollationMode == CollationMode.Clipper, ACE.ADS_OEM,ACE.ADS_ANSI)
    IF info:ReadOnly
      openmode |= (WORD) ACE.ADS_READONLY
    ENDIF
    IF !info:Shared
      openmode |= (WORD) ACE.ADS_EXCLUSIVE
    ENDIF
    IF SELF:_SetPaths() != 0
      RETURN FALSE
    ENDIF
    IF SELF:_Connection != IntPtr.Zero
      usTableType := ACE.ADS_DATABASE_TABLE
      fileName := Path.GetFileNameWithoutExtension(info:FileName)
    ELSE
      usTableType := SELF:_TableType
      fileName := info:FileName + info:Extension
    ENDIF
    IF alias:Length > 10
      alias := string.Empty
    ENDIF
    IF SELF:_TableType == ACE.ADS_ADT .AND. Path.GetExtension(fileName):ToUpper() == ".DBF"
      fileName := Path.ChangeExtension(fileName, ".ADT")
    ENDIF
    LOCAL result AS DWORD
    result := ACE.AdsOpenTable90(SELF:_Connection, fileName, alias, usTableType, openmode, SELF:_LockType, SELF:_CheckRights, charset, SELF:_Collation, OUT SELF:_Table)
    IF result != 0 .AND. SELF:_Connection != IntPtr.Zero
      usTableType := SELF:_TableType
      IF fileName[0] != '#'
        fileName := info:FileName + info:Extension
        IF SELF:_TableType == ACE.ADS_ADT .AND. Path.GetExtension(fileName):ToUpper() == ".DBF"
          fileName := Path.ChangeExtension(fileName, ".ADT")
        ENDIF
      ENDIF
      result := ACE.AdsOpenTable90(SELF:_Connection, fileName, alias, usTableType, openmode, SELF:_LockType, SELF:_CheckRights, charset, SELF:_Collation, OUT SELF:_Table)
    ENDIF
    IF result != 0
      NetErr(TRUE)
      SELF:Close()
      SELF:ADSERROR(ERDD_OPEN_FILE, EG_OPEN, "Open")
      RETURN FALSE
    ENDIF
    IF !SELF:_FieldSub()
      SELF:Close()
      RETURN FALSE
    ENDIF
    LOCAL length := MAX_PATH AS WORD
    LOCAL afileName AS CHAR[]
    afileName := CHAR[]{length}
    SELF:_CheckError(ACE.AdsGetTableFilename(SELF:_Table, ACE.ADS_FULLPATHNAME, afileName, REF length))
    SELF:_FileName := STRING {aFileName,0, length}
    IF result != 0
      SELF:Close()
      RETURN FALSE
    ENDIF
    LOCAL numIndexes AS WORD
    SELF:_CheckError(ACE.AdsGetNumIndexes(SELF:_Table, OUT numIndexes))
    IF numIndexes > 0 
      SELF:_CheckError(ACE.AdsGetIndexHandleByOrder(SELF:_Table, 1, OUT SELF:_Index))
    ENDIF
    SELF:_Encoding := Encoding.GetEncoding(IIF (charset == ACE.ADS_ANSI, RuntimeState.WinCodePage,RuntimeState.DosCodePage))
    LOCAL blockSize AS WORD
    IF ACE.AdsGetMemoBlockSize(SELF:_Table, OUT blockSize) == 0
      _HasMemo := TRUE
    ELSE
      _HasMemo := FALSE
    ENDIF
    LOCAL dwLength AS DWORD
    SELF:_CheckError(ACE.AdsGetRecordLength(SELF:_Table, OUT dwLength))
    SELF:_RecordLength := (LONG) dwLength
    SELF:Alias   := info:Alias
    SELF:Area    := info:WorkArea
    RETURN SELF:RecordMovement()
    
    /// <inheritdoc />
  VIRTUAL METHOD Close() AS LOGIC
    LOCAL result AS LOGIC
    result := SUPER:Close()
    IF SELF:_Table != IntPtr.Zero
      SELF:_CheckError(ACE.AdsCloseTable(SELF:_Table))
      SELF:_Table := IntPtr.Zero
    ENDIF
    SELF:_Index := IntPtr.Zero
    SELF:_CheckRights := 0
    SELF:_LockType := 0
    SELF:_TableType := 0
    SELF:Area  := 0
    RETURN result
    
    /// <inheritdoc />
  VIRTUAL METHOD Create(info AS DbOpenInfo) AS LOGIC
    LOCAL strFieldDef AS STRING
    LOCAL charset AS WORD
    LOCAL alias AS STRING
    LOCAL length AS WORD
    //
    strFieldDef     := string.Empty
    SELF:_Alias     := info:Alias
    SELF:_Area      := info:Workarea
    SELF:_Shared    := info:Shared
    SELF:_ReadOnly  := info:ReadOnly
    SELF:_Ansi      := RuntimeState.Ansi
    
    IF !SELF:_GetFieldInfo(REF strFieldDef)
      RETURN FALSE
    ENDIF
    SELF:_CheckRDDInfo()
    IF SELF:_SetPaths() != 0
      RETURN FALSE
    ENDIF
    charset := IIF (RuntimeState.CollationMode == CollationMode.Clipper, ACE.ADS_OEM,ACE.ADS_ANSI)
    alias := Path.GetFileNameWithoutExtension(info:Alias)
    IF alias:Length > 10
      alias := string.Empty
    ENDIF
    SELF:_CheckError(ACE.AdsCreateTable90(SELF:_Connection, info:FileName, alias, SELF:_TableType, charset, SELF:_LockType, SELF:_CheckRights, 0, strFieldDef, 0u, SELF:_Collation, OUT SELF:_Table))
    SELF:_Encoding := IIF (charset== ACE.ADS_ANSI, StringHelpers.WinEncoding, StringHelpers.DosEncoding))
    length := MAX_PATH
    LOCAL fileName AS CHAR[]
    fileName := CHAR[]{length}
    SELF:_CheckError(ACE.AdsGetTableFilename(SELF:_Table, ACE.ADS_FULLPATHNAME , fileName, REF length))
    SELF:_FileName := STRING{fileName,0, length}
    LOCAL dwLength AS DWORD
    SELF:_CheckError(ACE.AdsGetRecordLength(SELF:_Table, OUT dwLength))
    SELF:_RecordLength := (LONG) dwLength
    IF !SUPER:Create(info)
      SELF:Close()
      RETURN FALSE
    ENDIF
    RETURN SELF:RecordMovement()
    
    
    
    // Check if a Field definition is correct :
    // Date Length must be 8, Number are long enough to store Dot and Decs (if any), ...
  PROTECT OVERRIDE METHOD _checkFields(info AS RddFieldInfo) AS LOGIC
    // FieldName
    info:Name := info:Name:ToUpper():Trim()
    IF info:Name:Length > 10 
      info:Name := info:Name:Substring(0,10)
    ENDIF
    //
    SWITCH info:FieldType
    CASE DbFieldType.Character
        IF info:Length == 0  .OR.  info:Decimals > 0  .OR. info:Length > System.UInt16.MaxValue 
          SELF:ADSERROR(ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
      ENDIF
    CASE DbFieldType.Number
        IF info:Length >= 1  .AND.  info:Length <= 255 
          IF info:Decimals > 0 
            // We must check that we have enough space for DOT and decimal
            IF info:Length <= 2 .OR. info:Decimals >= info:Length -1 
              SELF:ADSERROR( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
            ENDIF
          ENDIF
        ELSE
          SELF:ADSERROR( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
      ENDIF
    CASE DbFieldType.Date
        IF info:Length != 8 .OR.  info:Decimals != 0 
          SELF:ADSERROR( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
      ENDIF
    CASE DbFieldType.Logic
        IF info:Length != 1 .OR. info:Decimals != 0 
          SELF:ADSERROR( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
      ENDIF
    CASE DbFieldType.Memo
        IF info:Length != 10 .OR. info:Decimals != 0 
          SELF:ADSERROR( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
      ENDIF
    OTHERWISE
        // To be done : Support of Fox Field Types, ....
      info:FieldType := DbFieldType.Unknown
    END SWITCH
    RETURN TRUE
    
  VIRTUAL METHOD AddField(info AS RddFieldInfo) AS LOGIC
    LOCAL isOk AS LOGIC
    isOk := SUPER:AddField(info)
    IF isOk  .AND. info:FieldType == DbFieldType.Memo
      SELF:_HasMemo := TRUE
    ENDIF
    RETURN isOk
    
    
    #endregion
  #region Navigation
  METHOD RecordMovement() AS LOGIC
    LOCAL atBOF AS WORD
    LOCAL atEOF AS WORD
    LOCAL isFound AS WORD
    LOCAL lRecno AS DWORD
    SELF:_CheckError(ACE.AdsGetRecordNum(SELF:_Table, ACE.ADS_IGNOREFILTERS,  OUT lRecno))
    SELF:_Recno := lRecno
    SELF:_CheckError(ACE.AdsAtBOF(SELF:_Table, OUT atBOF))
    SUPER:_Bof := (atBOF == 1)
    SELF:_CheckError(ACE.AdsAtEOF(SELF:_Table, OUT atEOF))
    SUPER:_Eof := (atEOF == 1)
    SELF:_CheckError(ACE.AdsIsFound(SELF:_Table,  OUT isFound))
    SUPER:_Found := (isFound == 1)
    IF atBOF == 1 .AND. atEOF == 0
      SELF:GoTop()
      SUPER:_Bof := TRUE
    ENDIF
    // Todo
    //IF SUPER:m_lpdbRelations != nul)
    //FOREACH dbrelinfo AS DBRELINFO IN SUPER:m_lpdbRelations
    //(ADSRDD)dbrelinfo:lpaChild :RecordMovement()
    //NEXT
    //ENDIF
    RETURN TRUE
    
    /// <inheritdoc />
  VIRTUAL METHOD GoBottom() AS LOGIC
    SELF:_CheckError(SELF:_CheckVODeletedFlag())
    SELF:_CheckError(ACE.AdsGotoBottom(SELF:CurrentOrder))
    RETURN SELF:RecordMovement()
    
    /// <inheritdoc />
  VIRTUAL METHOD GoTop() AS LOGIC
    SELF:_CheckError(SELF:_CheckVODeletedFlag())
    SELF:_CheckError(ACE.AdsGotoTop(SELF:CurrentOrder))
    RETURN SELF:RecordMovement()
    
    /// <inheritdoc />
  VIRTUAL METHOD GoTo(lRec AS DWORD) AS LOGIC
    LOCAL recordnum AS DWORD
    LOCAL atEOF     AS WORD
    LOCAL atBOF     AS WORD
    SELF:_CheckError(ACE.AdsGetRecordNum(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT recordnum))
    IF recordnum == lRec
      SELF:_CheckError(ACE.AdsAtEOF(SELF:_Table, OUT atEOF))
      SELF:_CheckError(ACE.AdsAtBOF(SELF:_Table, OUT atBOF))
      IF atEOF== 0 .AND. atBOF == 0
        SELF:_CheckError(ACE.AdsWriteRecord(SELF:_Table))
        SELF:_CheckError(ACE.AdsRefreshRecord(SELF:_Table))
      ENDIF
    ELSE
      SELF:_CheckError(ACE.AdsGotoRecord(SELF:_Table, lRec))
    ENDIF
    RETURN SELF:RecordMovement()
    
    /// <inheritdoc />
  VIRTUAL METHOD GoToId(oRecnum AS OBJECT) AS LOGIC
    LOCAL recNum AS DWORD
    TRY
      recNum := System.Convert.ToUInt32(oRecnum)
    CATCH e AS Exception
      SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_DataType, "GoToId",e:Message)
      RETURN FALSE
    END TRY
    RETURN SELF:GoTo(recNum)
    
    /// <inheritdoc />
  VIRTUAL METHOD Skip(lCount AS LONG) AS LOGIC
    LOCAL result AS DWORD
    LOCAL flag AS LOGIC
    SELF:_CheckError(SELF:_CheckVODeletedFlag())
    IF lCount == 0
      SELF:_CheckError(ACE.AdsWriteRecord(SELF:_Table))
      result := ACE.AdsRefreshRecord(SELF:_Table)
    ELSE
      result := ACE.AdsSkip(SELF:CurrentOrder, lCount)
    ENDIF
    IF result != ACE.AE_NO_CURRENT_RECORD .AND. result != 0
      SELF:_CheckError(result)
    ENDIF
    flag := SELF:RecordMovement()
    IF lCount > 0
      SUPER:_Bof := FALSE
      RETURN flag
    ENDIF
    IF lCount < 0
      SUPER:_Eof := FALSE
    ENDIF
    RETURN flag
    
    /// <inheritdoc />
  VIRTUAL METHOD SkipFilter(lCount AS LONG) AS LOGIC
    // ADS has no difference with normal skip
    RETURN SELF:Skip(lCount)
    
    /// <inheritdoc />
  VIRTUAL METHOD SkipRaw(lCount AS LONG) AS LOGIC
    // ADS has no difference with normal skip
    RETURN SELF:Skip(lCount)
    
    
    /// <inheritdoc />

    
  INTERNAL METHOD _SetScope(hIndex AS System.IntPtr, usScopeOption AS WORD, oScope AS OBJECT ) AS OBJECT
    LOCAL aScope    AS CHAR[]
    LOCAL wBufLen   AS WORD
    LOCAL result AS DWORD
    LOCAL str       AS STRING
    aScope    := CHAR[]{(SELF:_MaxKeySize + 1)}
    wBufLen   := (WORD)(SELF:_MaxKeySize + 1) 
    result := ACE.AdsGetScope(hIndex, usScopeOption, aScope, REF wBufLen)
    IF result != ACE.AE_NO_SCOPE .AND. result != 0
      SELF:_CheckError(result)
    ENDIF
    str := oScope:ToString()
    IF !String.IsNullOrEmpty(str)
      SELF:_CheckError(ACE.AdsSetScope(hIndex, usScopeOption, str, (WORD)str:Length , ACE.ADS_STRINGKEY))
    ELSE
      SELF:_CheckError(ACE.AdsClearScope(hIndex, usScopeOption))
    ENDIF
    // return the previous scope
    IF result == ACE.AE_NO_SCOPE
      RETURN NULL
    ELSE
      RETURN SELF:_Ansi2Unicode(aScope, wBufLen)
    ENDIF
    
    
    #endregion
  #region Updates 
  
  /// <inheritdoc />
  VIRTUAL METHOD Flush() AS LOGIC
    RETURN SELF:GoCold()
    
    /// <inheritdoc />
  VIRTUAL METHOD GoCold() AS LOGIC
    SELF:_CheckError(ACE.AdsWriteRecord(SELF:_Table))
    RETURN SELF:RecordMovement()
    
    /// <inheritdoc />
  VIRTUAL METHOD GoHot() AS LOGIC
    LOCAL options AS DWORD
    LOCAL isTableLocked AS WORD
    LOCAL isRecordLocked AS WORD
    LOCAL dwRecNo AS DWORD
    LOCAL numRecs AS DWORD
    LOCAL result AS DWORD
    //
    SELF:_CheckError(ACE.AdsGetTableOpenOptions(SELF:_Table, OUT options))
    // GoHot must have lock when not exclusive
    IF !_HasFlag(options, ACE.ADS_EXCLUSIVE) 
      // Only allowed when not Readonly
      IF _HasFlag(options,ACE.ADS_READONLY) 
        SELF:ADSERROR(ERDD.READONLY, XSharp.Gencode.EG_READONLY)
      ENDIF
      SELF:_CheckError(ACE.AdsIsTableLocked(SELF:_Table, OUT isTableLocked))
      IF isTableLocked == 0
        SELF:_CheckError(ACE.AdsIsRecordLocked(SELF:_Table, 0, OUT isRecordLocked))
        IF isRecordLocked == 0
          dwRecNo := 0
          IF SELF:IsADT
            result := ACE.AdsGetRecordCount(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT numRecs)
            IF result == 0
              result := ACE.AdsGetRecordNum(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT dwRecNo)
            ENDIF
            IF result == 0 .AND. numRecs < dwRecNo .AND. ! SUPER:_Bof .AND. ! SUPER:_Eof
              result := ACE.AdsLockRecord(SELF:_Table, 0)
              IF result == 0 .OR. result == ACE.AE_TABLE_NOT_SHARED
                RETURN TRUE
              ENDIF
              SELF:_CheckError(result)
            ENDIF
          ENDIF
          SELF:ADSERROR(ERDD.UNLOCKED, XSharp.Gencode.EG_STROVERFLOW, XSharp.Severity.ES_ERROR)
        ENDIF
      ENDIF
    ENDIF
    RETURN TRUE
    
    /// <inheritdoc />
  VIRTUAL METHOD Zap() AS LOGIC
    LOCAL options AS DWORD
    SELF:_CheckError(ACE.AdsGetTableOpenOptions(SELF:_Table, OUT options))
    // Only allowed when opened exclusively
    IF !_HasFlag(options, ACE.ADS_EXCLUSIVE) 
      SELF:ADSERROR(ACE.AE_TABLE_NOT_EXCLUSIVE, EG_SHARED , "Zap")
    ENDIF
    // Only allowed when not Readonly
    IF _HasFlag(options, ACE.ADS_READONLY) 
      SELF:ADSERROR(ERDD.READONLY, EG_READONLY, "Zap")
    ENDIF
    SELF:_CheckError(ACE.AdsZapTable(SELF:_Table))
    RETURN SELF:GoTop()
    
    
    /// <inheritdoc />
  VIRTUAL METHOD Append(fReleaseLocks AS LOGIC) AS LOGIC
    LOCAL result        AS DWORD
    LOCAL handleType    AS WORD
    LOCAL isTableLocked AS WORD
    //
    IF fReleaseLocks
      //
      SELF:_CheckError(ACE.AdsGetHandleType(SELF:_Table, OUT handleType))
      IF handleType != ACE.ADS_CURSOR
        result := ACE.AdsIsTableLocked(SELF:_Table, OUT isTableLocked)
        IF isTableLocked == 0
          result := ACE.AdsUnlockTable(SELF:_Table)
          // When Unlock fails because not shared or because not locked, then no problem
          IF result != ACE.AE_TABLE_NOT_SHARED .AND. result != ACE.AE_TABLE_NOT_LOCKED  .AND. result != 0
            SELF:_CheckError(result)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    SELF:_CheckError(ACE.AdsAppendRecord(SELF:_Table))
    result := ACE.AdsLockRecord(SELF:_Table, 0)
    IF result != ACE.AE_TABLE_NOT_SHARED .AND. result != 0
      SELF:_CheckError(result)
    ENDIF
    RETURN SELF:RecordMovement()
    
  VIRTUAL METHOD Delete() AS LOGIC
    SELF:_CheckError(ACE.AdsDeleteRecord(SELF:_Table))
    RETURN TRUE
    
  VIRTUAL METHOD Recall() AS LOGIC
    SELF:_CheckError(ACE.AdsRecallRecord(SELF:_Table))
    RETURN SELF:RecordMovement()
    
    #endregion
    
  #region Read and Write
  METHOD GetValue(nFldPos AS INT) AS OBJECT
    LOCAL result    := 0 AS DWORD
    LOCAL chars     := NULL AS CHAR[]
    LOCAL bytes     := NULL AS BYTE[]
    LOCAL length    := 0 AS DWORD
    LOCAL isEmpty   := 0 AS WORD
    LOCAL wType     AS WORD
    LOCAL dwField  := (DWORD) nFldPos + 1 AS DWORD
    LOCAL fld       := SELF:_Fields[nFldPos] AS RddFieldInfo
    length  := (DWORD) fld:Length +1
    SELF:_CheckError(ACE.AdsGetFieldType(SELF:_Table, dwField, OUT wType))
    SWITCH fld:FieldType
    CASE DbFieldType.Character
        SWITCH wType
      CASE ACE.ADS_TIME
      CASE ACE.ADS_TIMESTAMP
        CASE ACE.ADS_MONEY
      CASE ACE.ADS_ROWVERSION
        CASE ACE.ADS_MODTIME
            _CheckError(ACE.AdsIsEmpty(SELF:_Table,  dwField , OUT isEmpty))
            IF isEmpty == 1
              RETURN NULL
            ENDIF
            chars := CHAR[] {length}
          result := ACE.AdsGetField(SELF:_Table, dwField, chars, REF length ,0)
      CASE ACE.ADS_STRING
      CASE ACE.ADS_VARCHAR
        CASE ACE.ADS_CISTRING
        CASE ACE.ADS_VARCHAR_FOX
            chars := CHAR[] {length}
          result := ACE.AdsGetString(SELF:_Table, dwField, chars, REF length ,0)
        CASE ACE.ADS_NCHAR
        CASE ACE.ADS_NVARCHAR
            chars := CHAR[] {length}
          result := ACE.AdsGetStringW(SELF:_Table, dwField, chars, REF length ,0)
        OTHERWISE
          SELF:ADSERROR(ERDD_DATATYPE, EG_DATATYPE)
        END SWITCH
        SWITCH result
        CASE 0
            IF wType != ACE.ADS_NCHAR .AND. wType != ACE.ADS_NVARCHAR
              RETURN SELF:_Ansi2Unicode(chars, (INT) length)
            ELSE
              RETURN STRING{chars, 0, (INT) length}
          ENDIF
        CASE ACE.AE_NO_CURRENT_RECORD
          RETURN STRING{' ', (INT) length}
        OTHERWISE
          SELF:_CheckError(result)
      END SWITCH
    CASE DbFieldType.Memo
        SWITCH wType
        CASE ACE.ADS_MEMO
      CASE ACE.ADS_BINARY
        CASE ACE.ADS_IMAGE
        CASE ACE.ADS_NMEMO
            result := ACE.AdsGetMemoLength(SELF:_Table, dwField, OUT length )
            SWITCH result
            CASE 0
                IF length == 0
                  RETURN string.Empty
              ENDIF
            CASE ACE.AE_NO_CURRENT_RECORD
                IF wType == ACE.ADS_MEMO .OR. wType == ACE.ADS_NMEMO
                  RETURN string.Empty
                ELSE    // image and binary
                  RETURN NULL
              ENDIF
            OTHERWISE
              SELF:_CheckError(result)
            END SWITCH
            SWITCH wType
            CASE ACE.ADS_MEMO
                chars := CHAR[] {++length}
                SELF:_CheckError(ACE.AdsGetString(SELF:_Table, dwField, chars, REF length ,0))
              RETURN SELF:_Ansi2Unicode(chars, (INT) length)
            CASE ACE.ADS_NMEMO
                chars := CHAR[] {++length}
              SELF:_CheckError(ACE.AdsGetStringW(SELF:_Table, dwField, chars, REF length ,0))
          CASE ACE.ADS_BINARY
            CASE ACE.ADS_IMAGE
                bytes := BYTE[] {length}
                SELF:_CheckError(ACE.AdsGetBinary(SELF:_Table, dwField, 0, bytes, REF length ))
              RETURN bytes
            END SWITCH
            
      CASE ACE.ADS_RAW
        CASE ACE.ADS_VARBINARY_FOX
            result := ACE.AdsIsEmpty(SELF:_Table, dwField, OUT isEmpty)
            IF result == 0
              IF isEmpty == 1
                RETURN NULL
              ENDIF
            ELSE
              SELF:_CheckError(result)
            ENDIF
            length := (DWORD) fld:Length
            bytes := BYTE[] {length}
            SELF:_CheckError(ACE.AdsGetBinary(SELF:_Table, dwField, 0, bytes, REF length ))
          RETURN bytes
      END SWITCH
    CASE DbFieldType.Number
        SWITCH wType
        CASE ACE.ADS_NUMERIC
      CASE ACE.ADS_DOUBLE
        CASE ACE.ADS_INTEGER
        CASE ACE.ADS_SHORTINT
      CASE ACE.ADS_AUTOINC
        CASE ACE.ADS_CURDOUBLE
            LOCAL r8 AS REAL8
            result := ACE.AdsGetDouble(SELF:_Table, dwField, OUT r8)
            IF result == ACE.AE_NO_CURRENT_RECORD
              r8 := 0.0
            ENDIF
            IF wType == ACE.ADS_DOUBLE .OR. wType == ACE.ADS_CURDOUBLE
              VAR value := r8:ToString()
              VAR pos := VALUE:IndexOf('.')
              RETURN DbFloat{r8, fld:Length, IIF(pos > 0, fld:Length - pos -1, 0)}
            ELSE
              RETURN DbFloat{r8, fld:Length, fld:Decimals}
          ENDIF
        OTHERWISE
          SELF:ADSERROR(ERDD_DATATYPE, EG_DATATYPE)
        END SWITCH
        
    CASE DbFieldType.Logic
        LOCAL wValue AS WORD
        result := ACE.AdsGetLogical(SELF:_Table, dwField, OUT wValue)
        IF result ==  ACE.AE_NO_CURRENT_RECORD
          wValue := 0
        ELSEIF ! SELF:_CheckError(result)
          // Exception
        ENDIF
      RETURN wValue == 0
    CASE DbFieldType.Date
        LOCAL lJulian AS LONG
        result := ACE.AdsGetJulian(SELF:_Table, dwField, OUT lJulian)
        IF result == ACE.AE_NO_CURRENT_RECORD
            RETURN DbDate{0,0,0}
        ELSEIF result != 0
            SELF:_CheckError(result)
        ENDIF
        TRY
            IF lJulian == 0
                RETURN DbDate{0,0,0}
            ELSE
                // there is a function to convert julian to string but it does not always work correctly
                // this seems to always do the job
                LOCAL wLength := ACE.ADS_MAX_DATEMASK+1 AS DWORD
                chars := CHAR[]{wLength}
                result := ACE.AdsGetString(SELF:_Table, dwField, chars, REF wLength, 0)
                VAR cDate := STRING{chars, 0, 8}
                IF String.IsNullOrWhiteSpace(cDate)
                    RETURN DbDate{0,0,0}
                ENDIF
                VAR dt := DateTime.ParseExact(cDate, "yyyyMMdd",NULL)
                RETURN DbDate{dt:Year, dt:Month, dt:Day}
            ENDIF
        CATCH
            RETURN DbDate{0,0,0}
        END TRY
      
    OTHERWISE
      SELF:ADSERROR(ERDD_DATATYPE, EG_DATATYPE)
    END SWITCH
    RETURN NULL
    
  METHOD Pack () AS LOGIC
    LOCAL options AS DWORD
    SELF:_CheckError(ACE.AdsGetTableOpenOptions(SELF:_Table, OUT options))
    IF !_HasFlag(options, ACE.ADS_EXCLUSIVE)
      SELF:ADSERROR(ACE.AE_TABLE_NOT_EXCLUSIVE, EG_SHARED, "Pack")
      RETURN FALSE
    ENDIF
    IF _HasFlag(options , ACE.ADS_READONLY)
      SELF:ADSERROR(ERDD_READONLY, EG_READONLY, "Pack")
      RETURN FALSE
    ENDIF
    SELF:_CheckError(ACE.AdsPackTable(SELF:_Table))
    RETURN SELF:GoTop()
    
  METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
    LOCAL tc AS TypeCode
    LOCAL dwField  := (DWORD) nFldPos + 1 AS DWORD
    LOCAL fld := SELF:_Fields[nFldPos] AS RDDFieldInfo
    IF ! SELF:GoHot()
      RETURN FALSE
    ENDIF
    IF oValue == NULL
      SELF:_CheckError(ACE.AdsSetEmpty(SELF:_Table, dwField))
      RETURN TRUE
    ENDIF
    // Handle IDate and IFloat
    IF oValue IS IDate
      LOCAL oDate AS IDate
      tc := TypeCode.DateTime
      oDate := (IDate) oValue
      IF oDate:IsEmpty
        oValue := DateTime.MinValue
      ELSE
        oValue := DateTime{oDate:Year, oDate:Month, oDate:Day}
      ENDIF
    ELSEIF oValue IS IFloat
      tc := TypeCode.Double
      oValue := ((IFloat) oValue):Value
    ELSE
      tc := Type.GetTypeCode(oValue:GetType())
    ENDIF
    LOCAL wType AS WORD
    LOCAL result := 0 AS DWORD
    SELF:_CheckError(ACE.AdsGetFieldType(SELF:_Table, dwField, OUT wType))
    SWITCH fld:FieldType
    CASE DbFieldType.Character
    CASE DbFieldType.Memo
        LOCAL length AS DWORD
        LOCAL strValue AS STRING
        SWITCH wType
        CASE ACE.ADS_STRING
        CASE ACE.ADS_MEMO
      CASE ACE.ADS_VARCHAR
        CASE ACE.ADS_CISTRING
            IF tc != TypeCode.String
              SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String expected")
            ENDIF
            strValue := (STRING) oValue
            length := (DWORD) strValue:Length
            IF length == 0
              result := ACE.AdsSetEmpty(SELF:_Table, dwField)
            ELSE
              result := ACE.AdsSetString(SELF:_Table, dwField, strValue, length)
          ENDIF
        CASE ACE.ADS_NCHAR
      CASE ACE.ADS_NVARCHAR
        CASE ACE.ADS_NMEMO
            IF tc != TypeCode.String
              SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String expected")
            ENDIF
            strValue := (STRING) oValue
            length := (DWORD) strValue:Length
            IF length == 0
              result := ACE.AdsSetEmpty(SELF:_Table, dwField)
            ELSE
              result := ACE.AdsSetStringW(SELF:_Table, dwField, strValue, length)
          ENDIF
      CASE ACE.ADS_TIME
        CASE ACE.ADS_TIMESTAMP
      CASE ACE.ADS_MONEY
        CASE ACE.ADS_ROWVERSION
        CASE ACE.ADS_MODTIME
            IF tc != TypeCode.String
              SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String expected")
            ENDIF
            strValue := (STRING) oValue
            length := (DWORD) strValue:Length
          result := ACE.AdsSetField(SELF:_Table, dwField, strValue, length)
      CASE ACE.ADS_BINARY
      CASE ACE.ADS_IMAGE
        CASE ACE.ADS_RAW
        CASE ACE.ADS_VARBINARY_FOX
            IF tc != TypeCode.String .AND. tc != TypeCode.Object
              SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String or Object expected")
            ENDIF
            IF tc != TypeCode.String
              strValue := (STRING) oValue
              length := (DWORD) strValue:Length
              result := ACE.AdsSetString(SELF:_Table, dwField, strValue, length)
            ELSE
              LOCAL bytes AS BYTE[]
              bytes := (BYTE[]) oValue
              length := (DWORD) bytes:Length
              IF wType == ACE.ADS_RAW .OR. wType == ACE.ADS_VARBINARY_FOX
                result := ACE.AdsSetField(SELF:_Table, dwField, bytes, length)
              ELSE
                result := ACE.AdsSetBinary(SELF:_Table, dwField, wType, length, 0, bytes, length)
              ENDIF
          ENDIF
        OTHERWISE
          SELF:ADSError(ERDD_DATATYPE, EG_DataType, "PutValue")
        END SWITCH
        IF result != 0 .AND. result != ACE.AE_DATA_TRUNCATED
          SELF:ADSERROR(result, EG_WRITE, "PutValue")
      ENDIF
    CASE DbFieldType.Number
        IF tc != TypeCode.Double .AND. tc != TypeCode.Int32 .AND. tc != TypeCode.Decimal
          SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","Numeric value expected")
        ENDIF
        IF wType != ACE.ADS_AUTOINC
          LOCAL r8 AS REAL8
          r8 := Convert.ToDouble(oValue)
          SELF:_CheckError(ACE.AdsSetDouble(SELF:_Table, dwField, r8))
        ELSE
          NOP // Do not allow to write to AUTO INC field
      ENDIF
    CASE DbFieldType.Logic
        IF tc != TypeCode.Boolean
          SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","Logic value expected")
        ENDIF
      SELF:_CheckError(ACE.AdsSetLogical(SELF:_Table, dwField, IIF( (LOGIC) oValue, 1, 0)))
    CASE DbFieldType.Date
        IF tc != TypeCode.DateTime
          SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","Date or DateTime value expected")
        ENDIF
        LOCAL dt   := (DateTime) oValue AS DateTime
        LOCAL text := dt:ToString("yyyyMMdd") AS STRING
        LOCAL r8Julian AS REAL8
        SELF:_CheckError(AceUnPub.AdsConvertStringToJulian(text, (WORD) text:Length, OUT r8Julian))
      SELF:_CheckError(ACE.AdsSetJulian(SELF:_Table, dwField, (LONG) r8Julian))
    OTHERWISE
      SELF:ADSError(ERDD_DATATYPE, EG_DataType, "PutValue")
    END SWITCH
    
    RETURN FALSE
    
    #endregion
    
  #region Properties
  PROPERTY ACEConnectionHandle    AS IntPtr GET _Connection
  PROPERTY ACEIndexHandle         AS IntPtr GET _Index
  PROPERTY ACETableHandle         AS IntPtr GET _Table
  PRIVATE PROPERTY CurrentOrder AS System.IntPtr
    GET
      IF SELF:_Index != System.IntPtr.Zero
        RETURN SELF:_Index
      ENDIF
      RETURN SELF:_Table
    END GET
  END PROPERTY
  
  /// <inheritdoc />
  PROPERTY BOF AS LOGIC GET SUPER:_Bof
  /// <inheritdoc />
  PROPERTY EOF AS LOGIC GET SUPER:_Eof
  /// <inheritdoc />
  PROPERTY Found AS LOGIC GET SUPER:_Found
  INTERNAL PROPERTY IsADT AS LOGIC GET _TableType == ACE.ADS_ADT
    VIRTUAL PROPERTY SysName AS STRING GET _Driver
    
    
    
    
      /// <inheritdoc />
      PROPERTY Deleted AS LOGIC
        GET
          LOCAL isDeleted AS WORD
          LOCAL result AS DWORD
          result := ACE.AdsIsRecordDeleted(SELF:_Table, OUT isDeleted)
          IF result == ACE.AE_NO_CURRENT_RECORD 
            IF SELF:_Eof
              RETURN FALSE
            ELSE
              RETURN TRUE
            ENDIF
          ELSEIF result != 0
            SELF:_CheckError(result)
          ENDIF
          RETURN isDeleted != 0
          
        END GET 
      END PROPERTY
      
      PROPERTY Reccount AS LONG
        GET
          LOCAL dwCount AS DWORD
          SELF:_CheckError(ACE.AdsGetRecordCount(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT dwCount))
          RETURN (LONG) dwCount
        END GET
      END PROPERTY
      PROPERTY RecNo AS LONG
        GET
          LOCAL result AS DWORD
          LOCAL dwRecno AS DWORD
          result := ACE.AdsGetRecordNum(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT dwRecno)
          IF result == ACE.AE_NO_CURRENT_RECORD
            dwRecno := 0
          ELSEIF result != 0
            SELF:_CheckError(result)
          ENDIF
          RETURN (LONG) dwRecno
        END GET
      END PROPERTY
    #endregion
    
    
    #region Locking
    
    
    /// <inheritdoc />
    VIRTUAL METHOD Lock(lockInfo AS DBLOCKINFO) AS LOGIC
      LOCAL lRecno := 0 AS DWORD
      LOCAL result := 0 AS DWORD
      LOCAL handleType AS WORD
      LOCAL isLocked  AS WORD
      //
      lockInfo:Result := FALSE
      IF lockInfo:recID == NULL
        lRecno := SELF:_Recno
      ELSE
        TRY
          lRecno := System.Convert.ToUInt32(lockInfo:recID)
        CATCH e AS Exception
          SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_DATATYPE, "Lock", e:Message)
        END TRY
      ENDIF
      IF lockInfo:@@Method == DBLockInfo.LockMethod.File
        result := ACE.AdsLockTable(SELF:_Table)
        SELF:_CheckError(result)
      ENDIF
      SELF:_CheckError(ACE.AdsGetHandleType(SELF:_Table, OUT handleType))
      IF lRecno == 0 .AND. handleType != ACE.ADS_CURSOR
        SELF:_CheckError(ACE.AdsIsTableLocked(SELF:_Table, OUT isLocked))
        IF isLocked == 0
          result := ACE.AdsUnlockTable(SELF:_Table)
          IF result != ACE.AE_TABLE_NOT_LOCKED  .AND. result != ACE.AE_TABLE_NOT_SHARED .AND. result != 0
            SELF:_CheckError(result)
            result  := ACE.AdsLockRecord(SELF:_Table, lRecno)
          ENDIF
        ENDIF
      ENDIF
      
      IF result != ACE.AE_TABLE_NOT_SHARED .AND. result != 0
        SELF:_CheckError(result)
      ENDIF
      lockInfo:Result := TRUE
      RETURN TRUE
      
      /// <inheritdoc />
    VIRTUAL METHOD Unlock(recordID AS OBJECT) AS LOGIC
      LOCAL result AS DWORD
      LOCAL dwRecno := 0 AS DWORD
      TRY
        dwRecno := System.Convert.ToUInt32(recordID)
      CATCH e AS Exception
        SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_DATATYPE, "Unlock",e:Message)
      END TRY
      IF dwRecno == 0
        result := ACE.AdsUnlockTable(SELF:_Table)
      ELSE
        result := ACE.AdsUnlockRecord(SELF:_Table, dwRecno)
      ENDIF
      IF result != ACE.AE_TABLE_NOT_SHARED .AND. result != 0
        SELF:_CheckError(result)
      ENDIF
      RETURN TRUE
      
      
      
      #endregion
      
    #region Filters
    /// <inheritdoc />
    VIRTUAL METHOD ClearFilter() AS LOGIC
      LOCAL result AS DWORD
      IF SELF:_Table != System.IntPtr.Zero
        // Clear normal filter
        result := ACE.AdsClearFilter(SELF:_Table)
        IF result != ACE.AE_NO_FILTER .AND. result != 0
          SELF:_CheckError(result)
        ENDIF
        // Clear optimized filter
        result := ACE.AdsClearAOF(SELF:_Table)
        IF result != ACE.AE_NO_FILTER .AND. result != 0
          SELF:_CheckError(result)
        ENDIF
      ENDIF
      SELF:_FilterInfo := DbFilterInfo{}
      RETURN TRUE
      
    METHOD SetFieldExtent( fieldCount AS LONG ) AS LOGIC
      SELF:_HasMemo := FALSE
      RETURN SUPER:SetFieldExtent(fieldCount)
      
      
      /// <inheritdoc />
    VIRTUAL METHOD SetFilter(fi AS DBFILTERINFO) AS LOGIC
      LOCAL result AS DWORD
      // Get the current date format so we can handle literal dates in the filter
      SELF:_CheckError(IIF(SELF:_CheckVODateFormat(),0,1))
      IF String.IsNullOrEmpty(fi:FilterText)
        // clear filter
        // Ignore "No filter" error
        result := ACE.AdsClearFilter(SELF:_Table)
        IF result != ACE.AE_NO_FILTER
          SELF:_CheckError(result)
        ENDIF
        result := ACE.AdsClearAOF(SELF:_Table)
        IF result != ACE.AE_NO_FILTER
          SELF:_CheckError(result)
        ENDIF
      ELSE
        IF RuntimeState.Optimize
          SELF:_CheckError(ACE.AdsSetAOF(SELF:_Table, fi:FilterText, (WORD) ACE.ADS_RESOLVE_DYNAMIC | ACE.ADS_DYNAMIC_AOF))
        ELSE
          SELF:_CheckError(ACE.AdsSetFilter(SELF:_Table, fi:FilterText))
        ENDIF
      ENDIF
      fi:Active := TRUE
      SELF:_FilterInfo := fi
      RETURN TRUE
      
      #endregion
    #region Relations 
    /// <inheritdoc />
    VIRTUAL METHOD ClearRel() AS LOGIC
      IF SELF:_Table != System.IntPtr.Zero
        SELF:_CheckError(ACE.AdsClearRelation(SELF:_Table))
      ENDIF
      RETURN TRUE
      
      /// <inheritdoc />
    VIRTUAL METHOD SetRel(relinfo AS DBRELINFO) AS LOGIC
      IF relinfo:Child:Driver != SELF:_Driver
        SELF:ADSERROR(ERDD.UNSUPPORTED, XSharp.Gencode.EG_UNSUPPORTED, "SetRel", "Related workareas must be opened with the same driver.")
        RETURN FALSE
      ENDIF
      LOCAL child := (ADSRDD) relInfo:Child AS ADSRDD
      SELF:_CheckError(ACE.AdsSetRelation(SELF:_Table, child:ACEIndexHandle, relinfo:Key))
      RETURN TRUE
      
      #endregion
      
    #region Info
    /// <inheritdoc />
    VIRTUAL METHOD FieldInfo(uiPos AS LONG, uiOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
      LOCAL fieldType AS WORD
      LOCAL length    AS DWORD
      LOCAL result AS DWORD
      LOCAL dwFldPos := (DWORD)(uiPos + 1)AS DWORD
      SWITCH uiOrdinal
      CASE DBS_BLOB_TYPE
          SELF:_CheckError(ACE.AdsGetFieldType(SELF:_Table, dwFldPos ,  OUT fieldType))
          SWITCH fieldType
          CASE ACE.ADS_MEMO
          CASE ACE.ADS_BINARY
        CASE ACE.ADS_IMAGE
          CASE ACE.ADS_NMEMO
            RETURN "?"
        END SWITCH
      CASE DBS_BLOB_LEN
          IF SUPER:_fields[uiPos]:fieldType != DBFieldType.Memo  
            RETURN -1
          ELSE
            result := ACE.AdsGetMemoLength(SELF:_Table, dwFldPos , OUT length)
            IF result == ACE.AE_INVALID_FIELD_TYPE
              RETURN -1
            ENDIF
            IF result != 0
              SELF:_CheckError(result)
            ENDIF
            RETURN length
          ENDIF
          
      CASE DBS_BLOB_POINTER
          RETURN NULL
          
      END SWITCH
      RETURN SUPER:FieldInfo(uiPos, uiOrdinal, oNewValue)
      
    VIRTUAL METHOD RecInfo(iRecID AS OBJECT, uiOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
      LOCAL dwRecno AS DWORD
      //
      SWITCH (DBRecordInfo) uiOrdinal
      CASE DBRecordInfo.DBRI_LOCKED
          TRY
            dwRecno := Convert.ToUInt32(iRecID)
          CATCH
            SELF:ADSERROR(ERDD_DATATYPE, EG_DATATYPE, "RecInfo")
            RETURN FALSE
          END TRY
          LOCAL locked AS WORD
          _CheckError(ACE.AdsIsRecordLocked(SELF:_Table, dwRecno, OUT locked))
          RETURN locked != 0
          
      CASE DBRecordInfo.DBRI_RECSIZE
          RETURN SELF:_RecordLength
          
      CASE DBRecordInfo.DBRI_UPDATED
          SELF:_CheckError(ACE.AdsRefreshRecord(SELF:_Table))
        RETURN NULL
        
      CASE DBRecordInfo.DBRI_RECNO
          VAR result := ACE.AdsGetRecordNum(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT dwRecno)
          IF (result == ACE.AE_NO_CURRENT_RECORD)
            dwRecno := 0u
          ELSEIF result != 0
            SELF:_CheckError(result)
          ENDIF
        RETURN dwRecno
      OTHERWISE
        SELF:ADSERROR(ERDD_UNSUPPORTED, EG_UNSUPPORTED, "RecInfo")
      END SWITCH
      RETURN SUPER:RecInfo(iRecID, uiOrdinal, oNewValue)
      
      
      /// <inheritdoc />
    VIRTUAL METHOD Info(uiOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
      LOCAL options AS DWORD
      LOCAL result AS DWORD
      SWITCH (DbInfo)uiOrdinal
      CASE DbInfo.DBI_ISDBF
        RETURN !SELF:IsADT
      CASE DbInfo.DBI_CANPUTREC
        RETURN FALSE
      CASE DbInfo.DBI_GETRECSIZE
        RETURN SELF:_RecordLength
      CASE DbInfo.DBI_TABLEEXT
          IF SELF:IsADT
            RETURN ".ADT"
          ELSE
            RETURN ".DBF"
          ENDIF
          
      CASE DbInfo.DBI_GETHEADERSIZE
          LOCAL dwValue := 0 AS DWORD
          ACEUNPUB.AdsSetProperty90(SELF:_Table, ACE.ADS_GET_HEADER_LENGTH, dwValue)
        RETURN (LONG) dwValue
      CASE DbInfo.DBI_LASTUPDATE
          LOCAL julDate AS REAL8
          LOCAL aDate AS CHAR[]
          LOCAL DateLen AS WORD
          
          aDate := CHAR[]{ACE.ADS_MAX_DATEMASK+1}
          DateLen := (WORD) aDate:Length
          SELF:_CheckError(ACE.AdsSetDateFormat("MM/DD/YY"))
          SELF:_CheckError(ACE.AdsGetLastTableUpdate(SELF:_Table, aDate, REF DateLen))
          SELF:_CheckError(ACEUNPUB.AdsConvertStringToJulian(aDate, DateLen, OUT julDate))
          IF !SELF:_CheckVODateFormat()
            SELF:_CheckError(1)
          ENDIF
        RETURN (LONG)julDate
    CASE DbInfo.DBI_GETLOCKARRAY
      CASE DbInfo.DBI_LOCKCOUNT
          LOCAL numLocks AS WORD
          SELF:_CheckError(ACE.AdsGetNumLocks(SELF:_Table, OUT numLocks))
          IF (uiOrdinal == DbInfo.DBI_LOCKCOUNT)
            RETURN numLocks
          ENDIF
          IF numLocks > 0
            IF SELF:_aRlocks == NULL .OR. SELF:_aRlocks:Length < numLocks
              SELF:_aRlocks := DWORD[]{numLocks}
            ENDIF
            SELF:_CheckError(ACE.AdsGetAllLocks(SELF:_Table, SELF:_aRlocks, REF numLocks))
            RETURN SELF:_aRlocks
          ELSE
            RETURN NULL
        ENDIF
      CASE DbInfo.DBI_ISFLOCK
          LOCAL isLocked AS WORD
          SELF:_CheckError(ACE.AdsIsTableLocked(SELF:_Table, OUT isLocked))
        RETURN isLocked != 0
      CASE DbInfo.DBI_FILEHANDLE
        RETURN IntPtr.Zero
      CASE DbInfo.DBI_ISANSI
        RETURN FALSE
      CASE DbInfo.DBI_FOUND
          LOCAL isFound AS WORD
          SELF:_CheckError(ACE.AdsIsFound(SELF:_Table, OUT isFound))
        RETURN isFound != 0
      CASE DbInfo.DBI_SHARED
          SELF:_CheckError(ACE.AdsGetTableOpenOptions(SELF:_Table, OUT options))
        RETURN !_HasFlag(options,ACE.ADS_EXCLUSIVE) 
      CASE DbInfo.DBI_MEMOEXT
          SWITCH SELF:_TableType 
          CASE ACE.ADS_ADT
            RETURN ".ADM"
        CASE ACE.ADS_CDX
          CASE ACE.ADS_VFP
            RETURN ".FPT"
          OTHERWISE
            RETURN ".DBT"
        END SWITCH
      CASE DbInfo.DBI_RDD_VERSION
        RETURN System.Reflection.Assembly.GetExecutingAssembly():GetName():Version:ToString()
      CASE DbInfo.DBI_GET_ACE_TABLE_HANDLE
        RETURN SELF:_Table
      CASE DbInfo.DBI_MEMOBLOCKSIZE
          LOCAL memoBlockSize AS WORD
          result := ACE.AdsGetMemoBlockSize(SELF:_Table, OUT memoBlockSize)
          IF result != ACE.AE_NO_MEMO_FILE
            RETURN 0
          ELSEIF result != 0
            SELF:_CheckError(result)
          ENDIF
          RETURN memoBlockSize
          
      CASE DbInfo.DBI_CODEPAGE
          RETURN SELF:_Encoding:CodePage
          
      CASE DbInfo.DBI_DB_VERSION
          RETURN 0
          
      CASE DbInfo.DBI_FULLPATH
          RETURN SUPER:_FileName
          
      CASE DbInfo.DBI_SETDELIMITER 
      CASE DbInfo.DBI_VALIDBUFFER 
      CASE DbInfo.DBI_LOCKOFFSET 
    CASE DbInfo.DBI_MEMOHANDLE 
    CASE DbInfo.DBI_NEWINDEXLOCK 
      CASE DbInfo.DBI_MEMOFIELD 
        RETURN NULL
      CASE DbInfo.DBI_READONLY
      CASE DbInfo.DBI_CHILDCOUNT
    CASE DbInfo.DBI_BOF
    CASE DbInfo.DBI_EOF
    CASE DbInfo.DBI_DBFILTER
      CASE DbInfo.DBI_FCOUNT
    CASE DbInfo.DBI_ALIAS
      CASE DbInfo.DBI_GETSCOPE
          RETURN SUPER:Info(uiOrdinal,oNewValue )
          
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
        RETURN NULL
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
        RETURN NULL
      END SWITCH
      RETURN SUPER:Info(uiOrdinal, oNewValue)
      
      
      #endregion
      
    #region Unsupported
    /// <inheritdoc />
    
    
    
    
    INTERNAL METHOD Unsupported(strFunctionName AS STRING) AS LOGIC
      SELF:ADSERROR(ERDD.UNSUPPORTED, XSharp.Gencode.EG_UnSupported, strFunctionName)
      RETURN FALSE
      
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC
      RETURN SELF:Unsupported("AppendLock")
      
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT
      SELF:Unsupported("BlobInfo")
      RETURN NULL
      
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD ForceRel() AS LOGIC
      RETURN SELF:Unsupported("ForceRel")
      
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD GetRec() AS BYTE[]
      SELF:Unsupported("GetRec")
      RETURN NULL
      
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD GetValueFile(nFldPos AS INT, cFileName AS STRING) AS LOGIC
      SELF:Unsupported("GetValueFile")
      RETURN FALSE
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC  
      RETURN SELF:Unsupported("HeaderLock")
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD PutRec(aRec AS BYTE[])			AS LOGIC
      RETURN SELF:Unsupported("PutRec")
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD PutValueFile(nFldPos AS INT, cFileName AS STRING) AS LOGIC
      SELF:Unsupported("PutValueFile")
      RETURN FALSE
      
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD ScopeInfo(nOrdinal AS INT) AS OBJECT
      SELF:Unsupported("ScopeInfo")
      RETURN NULL
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD Sort(info AS DbSortInfo) AS LOGIC
      RETURN SELF:Unsupported("Sort")
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD Trans(info AS DbTransInfo) AS LOGIC
      RETURN SELF:Unsupported("Trans")
      /// <summary>This method is not supported by the AdsRDD class </summary>
    VIRTUAL METHOD TransRec(info AS DbTransInfo) AS LOGIC
      RETURN SELF:Unsupported("TransRec")
      
      #endregion
      
      
 
  
      

      
      
      

END CLASS

