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
USING System.Diagnostics

/// <summary>
/// The base AdsRDD class from which all Advantage RDDs for X# inherit.
/// </summary>
[DebuggerDisplay("AdsRDD ({Alias,nq})")];
CLASS XSharp.ADS.ADSRDD INHERIT Workarea
  #region Fields
    PRIVATE  _aRlocks AS DWORD[]
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
    PRIVATE  _syncSettings AS LOGIC
    PRIVATE  _syncDeleted  AS LOGIC
    PRIVATE  _syncFolders  AS LOGIC
    PROTECT _Hot            AS LOGIC
    PRIVATE CONST CLIPPER_MIN_DATE := 2415386U AS DWORD	// 1901-01-01
    PRIVATE CONST CLIPPER_MAX_DATE := 4606840U AS DWORD	// 7900-12-31

  #endregion

  /// <summary>Create instande of RDD </summary>
CONSTRUCTOR()
    SUPER()
    SELF:_Order         := ADSIndex{SELF}
    SELF:_Memo          := ADSMemo{SELF}
    SELF:_Table         := System.IntPtr.Zero
    SELF:_Index         := System.IntPtr.Zero
    SELF:_Connection    := System.IntPtr.Zero
    SELF:_Driver        := String.Empty
    SELF:_Collation     := String.Empty
    SELF:_Driver        := "ADSRDD"
    SELF:_FileName       := String.Empty
    SELF:_syncDeleted    := TRUE
    SELF:_syncSettings   := TRUE
    SELF:_syncFolders    := TRUE


    RuntimeState.StateChanged += StateChanged

PRIVATE METHOD StateChanged(e AS StateChangedEventArgs) AS VOID
    SWITCH e:Setting
        CASE Set.Deleted
            SELF:_syncDeleted := TRUE
        CASE Set.DateFormat
        CASE Set.Decimals
        CASE Set.Exact
        CASE Set.Epoch
            SELF:_syncSettings := TRUE
        CASE Set.Default
        CASE Set.Path
            SELF:_syncFolders := TRUE
    END SWITCH
    RETURN
    #region Helper Methods that check for error conditions

INTERNAL STATIC METHOD _HasFlag(dw AS DWORD, flag AS DWORD) AS LOGIC
    RETURN _AND(dw, flag) == flag

INTERNAL METHOD _CheckError(ulRetCode AS DWORD, dwGenCode := EG_NOCLASS AS DWORD, strFunction := "" AS STRING) AS LOGIC
    IF ulRetCode != 0
        IF String.IsNullOrEmpty(strFunction)
            strFunction := ProcName(1)
        ENDIF
        SELF:ADSERROR( ulRetCode, dwGenCode, strFunction)
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
    IF String.IsNullOrEmpty(strMessage)
      //
        message := CHAR[]{ACE.ADS_MAX_ERROR_LEN}
        wBufLen := (WORD) message:Length
        IF ACE.AdsGetLastError(OUT lastError, message, REF wBufLen) == 0 .AND. lastError != 0 .AND. wBufLen > 0
            strMessage := STRING{message, 0, wBufLen}
        ENDIF
    ENDIF
    IF String.IsNullOrEmpty(strMessage) .AND. iSubCode != 0
        strMessage := ErrString(iGenCode)
    ENDIF
    oError   := AdsError{strMessage, iGenCode, iSubCode,_Driver, iSeverity, strFunction, _FileName}
    RuntimeState.LastRddError := oError
    THROW oError


    #endregion
  #region Helper Methods

INTERNAL METHOD _SetBOF(lNewValue AS LOGIC) AS VOID
    IF lNewValue != SELF:BoF
        SELF:BoF := lNewValue
    ENDIF


INTERNAL METHOD _SetEOF(lNewValue AS LOGIC) AS VOID
    IF lNewValue != SELF:EoF
        SELF:EoF := lNewValue
    ENDIF

INTERNAL METHOD _SynchronizeVODeletedFlag() AS DWORD
    IF _syncDeleted
        _syncDeleted := FALSE
        RETURN ACE.AdsShowDeleted(IIF(RuntimeState.Deleted,(WORD)0 ,(WORD)1 ))
    ENDIF
    RETURN 0

INTERNAL METHOD _SynchronizeSettings() AS VOID
    IF _syncSettings
        _syncSettings := FALSE
        ACE.AdsSetDateFormat(RuntimeState.DateFormat)
        ACE.AdsSetExact((WORD) IIF(RuntimeState.Exact,1 , 0 ))
        ACE.AdsSetDecimals((WORD)RuntimeState.Decimals )
        ACE.AdsSetEpoch((WORD)RuntimeState.Epoch )
    ENDIF
RETURN

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
        IF oRet IS IntPtr VAR p
            SELF:_Connection := p
        ELSEIF oRet IS DWORD VAR d
            SELF:_Connection := IntPtr{d}
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
            SELF:_Collation := String.Empty
        ENDIF
    ENDIF
RETURN

INTERNAL METHOD _Ansi2Unicode(source AS CHAR[], iLength AS LONG) AS STRING
    TRY
        VAR bytes := SELF:_Encoding:GetBytes(source, 0, iLength)
        // replace trailing chr(0) with space
        iLength -= 1
        do while iLength >= 0 && bytes[iLength] == 0
            bytes[iLength] := 32
            iLength -= 1
        enddo
        return SELF:_Encoding:GetString(bytes)
    CATCH
        RETURN String.Empty
    END TRY

    #endregion

  #region Open and Close
PROTECTED METHOD _FieldSub() AS LOGIC
    LOCAL num AS DWORD
    LOCAL sb AS CHAR[]
    LOCAL wLen AS WORD
    LOCAL fi AS RddFieldInfo
    LOCAL wDecimals AS WORD
    LOCAL wFields AS WORD
    SELF:_CheckError(ACE.AdsGetNumFields(SELF:_Table, OUT wFields),EG_OPEN)
    IF !SELF:SetFieldExtent(wFields) .or. SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    _fieldCount := wFields
    sb := CHAR[]{ACE.ADS_MAX_FIELD_NAME+1}
    FOR num := 1u TO wFields STEP 1
        LOCAL cName AS STRING
        SELF:_CheckError(ACE.AdsGetFieldType(SELF:_Table, num, OUT VAR usType),EG_OPEN)
        SELF:_CheckError(ACE.AdsGetFieldLength(SELF:_Table, num, OUT VAR ulLength),EG_OPEN)
        wLen := (WORD) sb:Length
        SELF:_CheckError(ACE.AdsGetFieldName(SELF:_Table, (WORD)num, sb, REF wLen),EG_OPEN)
        cName := STRING{sb,0, wLen}
        fi := RddFieldInfo{cName,DbFieldType.Character,1,0}
        fi:Length := (INT) ulLength
        LOCAL eType := (AdsFieldType) usType AS AdsFieldType
        SWITCH eType
        CASE AdsFieldType.TIMESTAMP
        CASE AdsFieldType.MODTIME
            fi:Length := 22
            fi:FieldType := DbFieldType.Character
        CASE AdsFieldType.TIME
            fi:Length := 11
            fi:FieldType := DbFieldType.Character
        CASE AdsFieldType.MONEY
        CASE AdsFieldType.ROWVERSION
            fi:Length := 22
            fi:FieldType := DbFieldType.Character
        CASE AdsFieldType.STRING
        CASE AdsFieldType.VARCHAR
        CASE AdsFieldType.CISTRING
        CASE AdsFieldType.VARCHAR_FOX
        CASE AdsFieldType.NCHAR
        CASE AdsFieldType.NVARCHAR
            fi:FieldType := DbFieldType.Character
        CASE AdsFieldType.GUID
            fi:FieldType := DbFieldType.Character
            fi:Length := 36
        CASE AdsFieldType.MEMO
        CASE AdsFieldType.BINARY
        CASE AdsFieldType.IMAGE
        CASE AdsFieldType.NMEMO
            fi:FieldType := DbFieldType.Memo
            fi:Length := 10
        CASE AdsFieldType.RAW
        CASE AdsFieldType.VARBINARY_FOX
            fi:FieldType := DbFieldType.Memo
        CASE AdsFieldType.SHORTINT
        CASE AdsFieldType.AUTOINC
        CASE AdsFieldType.INTEGER
        CASE AdsFieldType.NUMERIC
            fi:FieldType := DbFieldType.Number
            SWITCH eType
            CASE AdsFieldType.SHORTINT
                fi:Length := 6
            CASE AdsFieldType.AUTOINC
                fi:Length := 10
            CASE AdsFieldType.INTEGER
                fi:Length := 11
            OTHERWISE // Numeric
                fi:Length := (INT) ulLength
            END SWITCH
            SELF:_CheckError(ACE.AdsGetFieldDecimals(SELF:_Table, num, OUT wDecimals),EG_OPEN)
            fi:Decimals := wDecimals
        CASE AdsFieldType.LOGICAL
            fi:FieldType := DbFieldType.Logic
        CASE AdsFieldType.DATE
        CASE AdsFieldType.COMPACTDATE
            fi:FieldType := DbFieldType.Date
            fi:Length := 8
        CASE AdsFieldType.DOUBLE
        CASE AdsFieldType.CURDOUBLE
            fi:Length := 20
            fi:FieldType := DbFieldType.Number
            SELF:_CheckError(ACE.AdsGetFieldDecimals(SELF:_Table, num, OUT wDecimals),EG_OPEN)
            fi:Decimals := wDecimals
        OTHERWISE
            SELF:ADSERROR(ERDD_CORRUPT_HEADER, EG_DATATYPE)
            RETURN FALSE
        END SWITCH
        fi:Alias := NULL
        LOCAL oCol AS AdsColumn
        oCol := AdsColumn.Create(fi, SELF, eType, num)
        IF ! SELF:AddField(oCol)
            RETURN FALSE
        ENDIF
    NEXT
RETURN TRUE

PRIVATE METHOD _GetFieldInfo(strFieldDef REF STRING ) AS LOGIC
    strFieldDef := ""
    FOREACH fld AS RddFieldInfo IN SUPER:_Fields
        strFieldDef += fld:Name+", "
        SWITCH fld:FieldType
        CASE DbFieldType.Character
            strFieldDef += "C,"
          // allow clipper field lengths
            strFieldDef += (fld:Length + fld:Decimals*256):ToString()+";"
        CASE DbFieldType.Number
        CASE DbFieldType.Integer
            strFieldDef += "N,"+fld:Length:ToString()+", "+fld:Decimals:ToString()+";"
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

INTERNAL METHOD _SetPaths(dwGenCode AS DWORD) AS DWORD
    IF _syncFolders
        _syncFolders := FALSE
        IF !String.IsNullOrEmpty(SetDefault())
            SELF:_CheckError(ACE.AdsSetDefault(SetDefault()),dwGenCode)
        ENDIF
        IF !String.IsNullOrEmpty(SetPath())
            SELF:_CheckError(ACE.AdsSetSearchPath(SetPath()),dwGenCode)
        ENDIF
    ENDIF
RETURN 0u

    /// <inheritdoc />
OVERRIDE METHOD Open(info AS DbOpenInfo) AS LOGIC
    LOCAL openmode AS WORD
    LOCAL alias AS STRING
    LOCAL charset AS WORD
    LOCAL usTableType AS WORD
    LOCAL fileName AS STRING
    //
    openmode := 0
    SELF:_CheckRDDInfo()
    alias := Path.GetFileNameWithoutExtension(info:Alias)
    // both Clipper and XPP use weight tables
    IF RuntimeState.CollationMode == CollationMode.Clipper .OR. RuntimeState.CollationMode == CollationMode.Xpp
        charset := ACE.ADS_OEM
    ELSE
        charset := ACE.ADS_ANSI
    ENDIF
    IF info:ReadOnly
        openmode |= (WORD) ACE.ADS_READONLY
    ENDIF
    IF !info:Shared
        openmode |= (WORD) ACE.ADS_EXCLUSIVE
    ENDIF
    IF SELF:_SetPaths(EG_OPEN) != 0
        RETURN FALSE
    ENDIF
    IF SELF:_Connection != IntPtr.Zero
        usTableType := ACE.ADS_DATABASE_TABLE
        fileName := Path.GetFileNameWithoutExtension(info:FullName)
    ELSE
        usTableType := SELF:_TableType
        fileName    := info:FullName
    ENDIF
    IF alias:Length > 10
        alias := String.Empty
    ENDIF
    IF SELF:_TableType == ACE.ADS_ADT .AND. Path.GetExtension(fileName):ToUpper() == ".DBF"
        fileName := Path.ChangeExtension(fileName, ".ADT")
    ENDIF
    LOCAL result AS DWORD
    LOCAL tries := 0 AS LONG
    REPEAT
        // wait 100 ms before retrying
        IF tries > 0
            System.Threading.Thread.Sleep(100)
        ENDIF
        tries += 1

        result := ACE.AdsOpenTable90(SELF:_Connection, fileName, alias, usTableType, charset, SELF:_LockType, SELF:_CheckRights, openmode,SELF:_Collation, OUT SELF:_Table)
        IF result != 0 .AND. SELF:_Connection != IntPtr.Zero
            usTableType := SELF:_TableType
            IF fileName[0] != '#'
                fileName := info:FullName
                IF SELF:_TableType == ACE.ADS_ADT .AND. Path.GetExtension(fileName):ToUpper() == ".DBF"
                    fileName := Path.ChangeExtension(fileName, ".ADT")
                ENDIF
            ENDIF
            result := ACE.AdsOpenTable90(SELF:_Connection, fileName, alias, usTableType, charset, SELF:_LockType, SELF:_CheckRights, openmode, SELF:_Collation, OUT SELF:_Table)
        ENDIF
    UNTIL tries == 10 .OR. SELF:_Table != IntPtr.Zero
    IF result != 0
        NetErr(TRUE)
        SELF:Close()
        SELF:_FileName := fileName
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
    SELF:_CheckError(ACE.AdsGetTableFilename(SELF:_Table, ACE.ADS_FULLPATHNAME, afileName, REF length),EG_OPEN)
    SELF:_FileName := STRING {afileName,0, length}
    IF result != 0
        SELF:Close()
        RETURN FALSE
    ENDIF
    SELF:_CheckError(ACE.AdsGetNumIndexes(SELF:_Table, OUT VAR numIndexes),EG_OPEN)
    IF numIndexes > 0
        SELF:_CheckError(ACE.AdsGetIndexHandleByOrder(SELF:_Table, 1, OUT SELF:_Index))
    ENDIF
    SELF:_Encoding := Encoding.GetEncoding(IIF (charset == ACE.ADS_ANSI, RuntimeState.WinCodePage,RuntimeState.DosCodePage))
    IF ACE.AdsGetMemoBlockSize(SELF:_Table, OUT VAR blockSize) == 0 .AND. blockSize > 0
        _HasMemo := TRUE
    ELSE
        _HasMemo := FALSE
    ENDIF
    SELF:_CheckError(ACE.AdsGetRecordLength(SELF:_Table, OUT VAR dwLength),EG_OPEN)
    SELF:_RecordLength := (LONG) dwLength
    SELF:Alias   := info:Alias
    SELF:Area    := info:Workarea
RETURN SELF:RecordMovement()

    /// <inheritdoc />
OVERRIDE METHOD Close() AS LOGIC
    LOCAL result AS LOGIC
    SELF:GoCold()
    result := SUPER:Close()
    TRY
        IF SELF:_Table != IntPtr.Zero
            SELF:_CheckError(ACE.AdsCloseTable(SELF:_Table),EG_CLOSE)
        ENDIF
    CATCH e as Exception
        THROW e
    FINALLY
        SELF:_Table := IntPtr.Zero
        SELF:_Index := IntPtr.Zero
        SELF:_CheckRights := 0
        SELF:_LockType := 0
        SELF:_TableType := 0
        SELF:Area  := 0
        RuntimeState.StateChanged -= StateChanged
    END TRY
RETURN result

    /// <inheritdoc />
OVERRIDE METHOD Create(info AS DbOpenInfo) AS LOGIC
    LOCAL strFieldDef AS STRING
    LOCAL charset AS WORD
    LOCAL alias AS STRING
    LOCAL length AS WORD
    //
    strFieldDef     := String.Empty
    SELF:_Alias     := info:Alias
    SELF:_Area      := info:Workarea
    SELF:_Shared    := info:Shared
    SELF:_ReadOnly  := info:ReadOnly
    SELF:_Ansi      := RuntimeState.Ansi

    IF !SELF:_GetFieldInfo(REF strFieldDef)
        RETURN FALSE
    ENDIF
    SELF:_CheckRDDInfo()
    IF SELF:_SetPaths(EG_CREATE) != 0
        RETURN FALSE
    ENDIF
    // both Clipper and XPP use weight tables
    IF RuntimeState.CollationMode == CollationMode.Clipper .OR. RuntimeState.CollationMode == CollationMode.Xpp
        charset := ACE.ADS_OEM
    ELSE
        charset := ACE.ADS_ANSI
    ENDIF
    alias := Path.GetFileNameWithoutExtension(info:Alias)
    IF alias:Length > 10
        alias := String.Empty
    ENDIF
    SELF:_FileName := info:FullName
    SELF:_CheckError(ACE.AdsCreateTable90(SELF:_Connection, info:FullName, alias, SELF:_TableType, charset, SELF:_LockType, SELF:_CheckRights, 0, strFieldDef, 0u, SELF:_Collation, OUT SELF:_Table),EG_CREATE)
    SELF:_Encoding := IIF (charset== ACE.ADS_ANSI, StringHelpers.WinEncoding, StringHelpers.DosEncoding)
    length := MAX_PATH
    LOCAL fileName AS CHAR[]
    fileName := CHAR[]{length}
    SELF:_CheckError(ACE.AdsGetTableFilename(SELF:_Table, ACE.ADS_FULLPATHNAME , fileName, REF length),EG_CREATE)
    SELF:_FileName := STRING{fileName,0, length}
    SELF:_CheckError(ACE.AdsGetRecordLength(SELF:_Table, OUT VAR dwLength),EG_CREATE)
    SELF:_RecordLength := (LONG) dwLength
    // Workarea has no implementation of Create
    //    IF !SUPER:Create(info)
    //      SELF:Close()
    //      RETURN FALSE
    //    ENDIF
RETURN SELF:RecordMovement()



    // Check if a Field definition is correct :
    // Date Length must be 8, Number are long enough to store Dot and Decs (if any), ...
PROTECT OVERRIDE METHOD _checkFields(info AS RddFieldInfo) AS LOGIC
    // FieldName
    info:Name := info:Name:Trim()
    IF info:Name:Length > 10 .and. SELF:_TableType != ACE.ADS_ADT
        info:Name := info:Name:ToUpper()
        info:Name := info:Name:Substring(0,10)
    ENDIF
    IF ! info:Validate()
        SELF:ADSERROR(ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
    ENDIF
    RETURN TRUE

OVERRIDE METHOD AddField(info AS RddFieldInfo) AS LOGIC
    LOCAL isOk AS LOGIC
    isOk := SUPER:AddField(info)
    IF isOk  .AND. info:FieldType == DbFieldType.Memo
        SELF:_HasMemo := TRUE
    ENDIF
RETURN isOk


    #endregion
  #region Navigation
METHOD RecordMovement() AS LOGIC
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    SELF:_CheckError(ACE.AdsAtBOF(SELF:_Table, OUT VAR atBOF),EG_READ)
    SUPER:BoF := (atBOF == 1)
    SELF:_CheckError(ACE.AdsAtEOF(SELF:_Table, OUT VAR atEOF),EG_READ)
    SUPER:EoF := (atEOF == 1)
    SELF:_CheckError(ACE.AdsIsFound(SELF:_Table,  OUT VAR isFound),EG_READ)
    SUPER:_Found := (isFound == 1)
    IF atBOF == 1 .AND. atEOF == 0
        // Do not call self:GoTop() to avoid recursion
        SELF:_CheckError(ACE.AdsGotoTop(SELF:CurrentOrder),EG_READ)
        SUPER:BoF := TRUE
    ENDIF
    IF SUPER:_Relations != NULL
        FOREACH dbrelinfo AS DbRelInfo IN SUPER:_Relations
            IF dbrelinfo:Child IS ADSRDD VAR ChildRDD
                ChildRDD:RecordMovement()
            ENDIF
        NEXT
    ENDIF
RETURN TRUE

    /// <inheritdoc />
OVERRIDE METHOD GoBottom() AS LOGIC
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    SELF:_SynchronizeVODeletedFlag()
    SELF:_CheckError(ACE.AdsGotoBottom(SELF:CurrentOrder),EG_READ)
RETURN SELF:RecordMovement()

    /// <inheritdoc />
OVERRIDE METHOD GoTop() AS LOGIC
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    SELF:_SynchronizeVODeletedFlag()
    SELF:_CheckError(ACE.AdsGotoTop(SELF:CurrentOrder),EG_READ)
RETURN SELF:RecordMovement()

    /// <inheritdoc />
OVERRIDE METHOD GoTo(lRec AS LONG) AS LOGIC
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    SELF:GoCold()
    SELF:_CheckError(ACE.AdsGetRecordNum(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT VAR recordnum),EG_READ)
    IF recordnum == lRec
        // check if we need to write
        SELF:_CheckError(ACE.AdsAtEOF(SELF:_Table, OUT VAR atEOF),EG_READ)
        SELF:_CheckError(ACE.AdsAtBOF(SELF:_Table, OUT VAR atBOF),EG_READ)
        IF atEOF == 0 .AND. atBOF == 0
            SELF:_CheckError(ACE.AdsRefreshRecord(SELF:_Table),EG_READ)
        ENDIF
    ELSE
        SELF:_CheckError(ACE.AdsGotoRecord(SELF:_Table, (DWORD) lRec),EG_READ)
    ENDIF
RETURN SELF:RecordMovement()

    /// <inheritdoc />
OVERRIDE METHOD GoToId(oRecnum AS OBJECT) AS LOGIC
    LOCAL recNum AS LONG
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    TRY
        recNum := System.Convert.ToInt32(oRecnum)
    CATCH e AS Exception
        SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_DATATYPE, "GoToId",e:Message)
        RETURN FALSE
    END TRY
RETURN SELF:GoTo(recNum)

    /// <inheritdoc />
OVERRIDE METHOD Skip(lCount AS LONG) AS LOGIC
    LOCAL result AS DWORD
    LOCAL flag AS LOGIC
    SELF:GoCold()
    SELF:_SynchronizeVODeletedFlag()
    IF lCount == 0
        SELF:_CheckError(ACE.AdsWriteRecord(SELF:_Table),EG_WRITE)
        result := ACE.AdsRefreshRecord(SELF:_Table)
    ELSE
        result := ACE.AdsSkip(SELF:CurrentOrder, lCount)
    ENDIF
    IF result != ACE.AE_NO_CURRENT_RECORD .AND. result != 0
        SELF:_CheckError(result,EG_READ)
    ENDIF
    flag := SELF:RecordMovement()
    IF lCount > 0
        SUPER:BoF := FALSE
        RETURN flag
    ENDIF
    IF lCount < 0
        SUPER:_EoF := FALSE
    ENDIF
RETURN flag

    /// <inheritdoc />
OVERRIDE METHOD SkipFilter(lCount AS LONG) AS LOGIC
    // ADS has no difference with normal skip
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
RETURN SELF:Skip(lCount)

    /// <inheritdoc />
OVERRIDE METHOD SkipRaw(lCount AS LONG) AS LOGIC
    // ADS has no difference with normal skip
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
RETURN SELF:Skip(lCount)


    /// <inheritdoc />

INTERNAL METHOD _GetScope(hIndex AS System.IntPtr, usScopeOption AS WORD) AS OBJECT
    LOCAL aScope    AS CHAR[]
    LOCAL wBufLen   AS WORD
    LOCAL result AS DWORD
    aScope    := CHAR[]{(SELF:_MaxKeySize + 1)}
    wBufLen   := (WORD)(SELF:_MaxKeySize + 1)
    result := ACE.AdsGetScope(hIndex, usScopeOption, aScope, REF wBufLen)
    IF result == 0
        RETURN SELF:_Ansi2Unicode(aScope, wBufLen)
    ENDIF
    IF result != ACE.AE_NO_SCOPE
        SELF:_CheckError(result,EG_READ)
    ENDIF
    RETURN NULL

INTERNAL METHOD _SetScope(hIndex AS System.IntPtr, usScopeOption AS WORD, lClear AS LOGIC, oScope AS OBJECT ) AS OBJECT
    LOCAL oldVal AS OBJECT
    LOCAL result AS DWORD
    oldVal    := SELF:_GetScope(hIndex, usScopeOption)
    IF lClear .OR. oScope == NULL
        result := ACE.AdsClearScope(hIndex, usScopeOption)
    ELSE
        VAR str := oScope:ToString()
        IF !String.IsNullOrEmpty(str)
            result := ACE.AdsSetScope(hIndex, usScopeOption, str, (WORD)str:Length , ACE.ADS_STRINGKEY)
            SELF:_CheckError(result,EG_READ)
        ELSE
            result := ACE.AdsClearScope(hIndex, usScopeOption)
        ENDIF
    ENDIF
    RETURN oldVal


    #endregion
  #region Updates

  /// <inheritdoc />
OVERRIDE METHOD Flush() AS LOGIC
IF SELF:_Table == IntPtr.Zero
    RETURN FALSE
ENDIF
RETURN SELF:GoCold()

  /// <inheritdoc />
OVERRIDE METHOD Refresh() AS LOGIC
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    SELF:_CheckError(ACE.AdsRefreshRecord(SELF:_Table),EG_READ)
    SELF:_Hot           := FALSE
RETURN SELF:RecordMovement()

    /// <inheritdoc />
OVERRIDE METHOD GoCold() AS LOGIC
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    IF SELF:_Hot
        SELF:_CheckError(ACE.AdsWriteRecord(SELF:_Table),EG_READ)
    ENDIF
    SELF:_Hot           := FALSE
RETURN SELF:RecordMovement()

    /// <inheritdoc />
OVERRIDE METHOD GoHot() AS LOGIC
    LOCAL dwRecNo AS DWORD
    LOCAL result AS DWORD
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    //
    SELF:_CheckError(ACE.AdsGetTableOpenOptions(SELF:_Table, OUT VAR options),EG_WRITE)
    // GoHot must have lock when not exclusive
    IF !_HasFlag(options, ACE.ADS_EXCLUSIVE)
      // Only allowed when not Readonly
        IF _HasFlag(options,ACE.ADS_READONLY)
            SELF:ADSERROR(ERDD.READONLY, XSharp.Gencode.EG_READONLY)
        ENDIF
        SELF:_CheckError(ACE.AdsIsTableLocked(SELF:_Table, OUT VAR isTableLocked),EG_LOCK)
        IF isTableLocked == 0
            SELF:_CheckError(ACE.AdsIsRecordLocked(SELF:_Table, 0, OUT VAR isRecordLocked),EG_LOCK)
            IF isRecordLocked == 0
                dwRecNo := 0
                IF SELF:IsADT
                    result := ACE.AdsGetRecordCount(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT VAR numRecs)
                    IF result == 0
                        result := ACE.AdsGetRecordNum(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT dwRecNo)
                    ENDIF
                    IF result == 0 .AND. numRecs < dwRecNo .AND. ! SUPER:BoF .AND. ! SUPER:EoF
                        result := ACE.AdsLockRecord(SELF:_Table, 0)
                        IF result == 0 .OR. result == ACE.AE_TABLE_NOT_SHARED
                            RETURN TRUE
                        ENDIF
                        SELF:_CheckError(result,EG_LOCK)
                    ENDIF
                ENDIF
                SELF:ADSERROR(ERDD.UNLOCKED, XSharp.Gencode.EG_LOCK, XSharp.Severity.ES_ERROR)
            ENDIF
        ENDIF
    ENDIF
    SELF:_Hot           := TRUE
RETURN TRUE

    /// <inheritdoc />
OVERRIDE METHOD Zap() AS LOGIC
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF

    SELF:_CheckError(ACE.AdsGetTableOpenOptions(SELF:_Table, OUT VAR options),EG_WRITE)
    // Only allowed when opened exclusively
    IF !_HasFlag(options, ACE.ADS_EXCLUSIVE)
        SELF:ADSERROR(ACE.AE_TABLE_NOT_EXCLUSIVE, EG_SHARED , "Zap")
    ENDIF
    // Only allowed when not Readonly
    IF _HasFlag(options, ACE.ADS_READONLY)
        SELF:ADSERROR(ERDD.READONLY, EG_READONLY, "Zap")
    ENDIF
    SELF:_CheckError(ACE.AdsZapTable(SELF:_Table),EG_WRITE)
RETURN SELF:GoTop()


    /// <inheritdoc />
OVERRIDE METHOD Append(fReleaseLocks AS LOGIC) AS LOGIC
    LOCAL result        AS DWORD
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    SELF:GoCold()
    IF fReleaseLocks
        SELF:_CheckError(ACE.AdsGetHandleType(SELF:_Table, OUT VAR handleType),EG_APPENDLOCK)
        IF handleType != ACE.ADS_CURSOR
            result := ACE.AdsIsTableLocked(SELF:_Table, OUT VAR isTableLocked)
            IF isTableLocked == 0
                result := ACE.AdsUnlockTable(SELF:_Table)
          // When Unlock fails because not shared or because not locked, then no problem
                IF result != ACE.AE_TABLE_NOT_SHARED .AND. result != ACE.AE_TABLE_NOT_LOCKED  .AND. result != 0
                    SELF:_CheckError(result,EG_SHARED)
                ENDIF
            ENDIF
        ENDIF
    ENDIF
    SELF:_CheckError(ACE.AdsAppendRecord(SELF:_Table),EG_APPENDLOCK)
    result := ACE.AdsLockRecord(SELF:_Table, 0)
    IF result != ACE.AE_TABLE_NOT_SHARED .AND. result != 0
        SELF:_CheckError(result,EG_SHARED)
    ENDIF
    SELF:_Hot           := TRUE
RETURN SELF:RecordMovement()

OVERRIDE METHOD Delete() AS LOGIC
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    SELF:_CheckError(ACE.AdsDeleteRecord(SELF:_Table),EG_WRITE)
    SELF:_Hot           := TRUE
RETURN TRUE

OVERRIDE METHOD Recall() AS LOGIC
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    SELF:_CheckError(ACE.AdsRecallRecord(SELF:_Table),EG_WRITE)
    SELF:_Hot           := TRUE
RETURN SELF:RecordMovement()

    #endregion

    #region Read and Write
INTERNAL VIRTUAL METHOD _GetColumn(nFldPos AS LONG) AS AdsColumn
	LOCAL nArrPos := nFldPos -1 AS LONG
    IF nArrPos >= 0 .AND. nArrPos < SELF:_Fields:Length
        RETURN (AdsColumn) SELF:_Fields[ nArrPos ]
    ENDIF
    SELF:ADSERROR(EDB_FIELDINDEX, EG_ARG)
    RETURN NULL
OVERRIDE METHOD GetValue(nFldPos AS INT) AS OBJECT
    IF SELF:_Table == IntPtr.Zero
       RETURN NULL
    ENDIF
    var column := SELF:_GetColumn(nFldPos)
    if column != null
        RETURN column:GetValue()
    endif
    return NULL

OVERRIDE METHOD Pack () AS LOGIC
    IF SELF:_Table == IntPtr.Zero
       RETURN FALSE
    ENDIF
    SELF:GoCold()
    SELF:_CheckError(ACE.AdsGetTableOpenOptions(SELF:_Table, OUT VAR options),EG_READ)
    IF !_HasFlag(options, ACE.ADS_EXCLUSIVE)
        SELF:ADSERROR(ACE.AE_TABLE_NOT_EXCLUSIVE, EG_SHARED, "Pack")
        RETURN FALSE
    ENDIF
    IF _HasFlag(options , ACE.ADS_READONLY)
        SELF:ADSERROR(ERDD_READONLY, EG_READONLY, "Pack")
        RETURN FALSE
    ENDIF
    SELF:_CheckError(ACE.AdsPackTable(SELF:_Table),EG_WRITE)
RETURN SELF:GoTop()

OVERRIDE METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
    IF SELF:_Table == IntPtr.Zero
       RETURN FALSE
    ENDIF
    IF ! SELF:GoHot()
        RETURN FALSE
    ENDIF
    var column := SELF:_GetColumn(nFldPos)
    if column != null
        RETURN column:PutValue(oValue)
    ENDIF
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
OVERRIDE PROPERTY Found AS LOGIC GET SUPER:_Found
INTERNAL PROPERTY IsADT AS LOGIC GET _TableType == ACE.ADS_ADT
OVERRIDE PROPERTY Driver AS STRING GET _Driver




      /// <inheritdoc />
OVERRIDE PROPERTY Deleted AS LOGIC
    GET
        LOCAL result AS DWORD
        result := ACE.AdsIsRecordDeleted(SELF:_Table, OUT VAR isDeleted)
        IF result == ACE.AE_NO_CURRENT_RECORD
            IF SELF:EoF
                RETURN FALSE
            ELSE
                RETURN TRUE
            ENDIF
        ELSEIF result != 0
            SELF:_CheckError(result,EG_READ)
        ENDIF
        RETURN isDeleted != 0

    END GET
END PROPERTY

OVERRIDE PROPERTY RecCount AS LONG
    GET
        IF SELF:_Table == IntPtr.Zero
           RETURN 0
        ENDIF
        SELF:_CheckError(ACE.AdsGetRecordCount(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT VAR dwCount),EG_READ)
        RETURN (LONG) dwCount
    END GET
END PROPERTY
OVERRIDE PROPERTY RecNo AS LONG GET (LONG) SELF:_RecNo
PROPERTY _RecNo AS DWORD
    GET
        LOCAL result AS DWORD
        IF SELF:_Table == IntPtr.Zero
           RETURN 0
        ENDIF
        result := ACE.AdsGetRecordNum(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT VAR dwRecno)
        IF result == ACE.AE_NO_CURRENT_RECORD
            dwRecno := 0
        ELSEIF result != 0
            SELF:_CheckError(result,EG_READ)
        ENDIF
        RETURN dwRecno
    END GET
END PROPERTY

#endregion


    #region Locking


    /// <inheritdoc />
OVERRIDE METHOD Lock(lockInfo REF DbLockInfo) AS LOGIC
    LOCAL lRecno := 0 AS DWORD
    LOCAL result := 0 AS DWORD
      //
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    lockInfo:Result := FALSE
    IF lockInfo:RecId == NULL
        lRecno := SELF:_RecNo
    ELSE
        TRY
            lRecno := System.Convert.ToUInt32(lockInfo:RecId)
        CATCH e AS Exception
            SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_DATATYPE, "Lock", e:Message)
        END TRY
    ENDIF
    IF lockInfo:@@Method == DbLockInfo.LockMethod.File
        result := ACE.AdsLockTable(SELF:_Table)
//      SELF:_CheckError(result)
    ELSE
        SELF:_CheckError(ACE.AdsGetHandleType(SELF:_Table, OUT VAR handleType),EG_LOCK)
        IF lRecno == 0 .AND. handleType != ACE.ADS_CURSOR
            SELF:_CheckError(ACE.AdsIsTableLocked(SELF:_Table, OUT VAR isLocked),EG_LOCK)
            IF isLocked == 0
                result := ACE.AdsUnlockTable(SELF:_Table)
                IF result != ACE.AE_TABLE_NOT_LOCKED  .AND. result != ACE.AE_TABLE_NOT_SHARED .AND. result != 0
                    SELF:_CheckError(result,EG_LOCK)
                ENDIF
            ENDIF
        ENDIF
        result  := ACE.AdsLockRecord(SELF:_Table, lRecno)
    ENDIF

    IF result != ACE.AE_TABLE_NOT_SHARED .AND. result != 0
        SELF:_CheckError(result,EG_SHARED)
    ENDIF
    lockInfo:Result := TRUE
RETURN TRUE

      /// <inheritdoc />
OVERRIDE METHOD UnLock(recordID AS OBJECT) AS LOGIC
    LOCAL result AS DWORD
    LOCAL dwRecno := 0 AS DWORD
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
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
    IF result != ACE.AE_TABLE_NOT_SHARED .AND. result != ACE.AE_TABLE_NOT_LOCKED .AND. result != 0
        SELF:_CheckError(result)
    ENDIF
RETURN TRUE



      #endregion

    #region Filters
    /// <inheritdoc />
OVERRIDE METHOD ClearFilter() AS LOGIC
    LOCAL result AS DWORD
    SELF:GoCold()
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

OVERRIDE METHOD SetFieldExtent( fieldCount AS LONG ) AS LOGIC
    SELF:_HasMemo := FALSE
RETURN SUPER:SetFieldExtent(fieldCount)


      /// <inheritdoc />
OVERRIDE METHOD SetFilter(fi AS DbFilterInfo) AS LOGIC
    LOCAL result AS DWORD
      // Get the current date format so we can handle literal dates in the filter
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    SELF:_SynchronizeSettings()
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
            SELF:_CheckError(ACE.AdsSetAOF(SELF:_Table, fi:FilterText, (WORD) (ACE.ADS_RESOLVE_DYNAMIC | ACE.ADS_DYNAMIC_AOF)))
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
OVERRIDE METHOD ClearRel() AS LOGIC
    VAR lOk := SUPER:ClearRel()
    IF SELF:_Table == IntPtr.Zero
        RETURN FALSE
    ENDIF
    IF SELF:_Table != System.IntPtr.Zero
        SELF:_CheckError(ACE.AdsClearRelation(SELF:_Table))
    ENDIF
RETURN lOk

      /// <inheritdoc />
OVERRIDE METHOD SetRel(relinfo AS DbRelInfo) AS LOGIC
      // Needs a better solution of course. SELF:_Driver currently returns the fullname of the class, while Driver property is not implemented yet
    IF relinfo:Child:Driver != SELF:Driver
        SELF:ADSERROR(ERDD.UNSUPPORTED, XSharp.Gencode.EG_UNSUPPORTED, "SetRel", "Related workareas must be opened with the same driver.")
        RETURN FALSE
    ENDIF
    VAR lOk :=  SUPER:SetRel(relinfo)
    IF lOk
        LOCAL child := (ADSRDD) relinfo:Child AS ADSRDD
        SELF:_CheckError(ACE.AdsSetRelation(SELF:_Table, child:ACEIndexHandle, relinfo:Key))
    ENDIF
RETURN lOk

      #endregion

    #region Info
    /// <inheritdoc />
OVERRIDE METHOD FieldInfo(uiPos AS LONG, uiOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
    LOCAL result AS DWORD
    IF SELF:_Table == IntPtr.Zero
        RETURN NULL
    ENDIF
    SWITCH uiOrdinal
    CASE DBS_BLOB_TYPE
        SELF:_CheckError(ACE.AdsGetFieldType(SELF:_Table, (DWORD)uiPos  ,  OUT VAR FieldType))
        SWITCH FieldType
        CASE ACE.ADS_MEMO
        CASE ACE.ADS_BINARY
        CASE ACE.ADS_IMAGE
        CASE ACE.ADS_NMEMO
            RETURN "?"
        END SWITCH
    CASE DBS_BLOB_LEN
        IF SUPER:_Fields[uiPos-1]:FieldType != DbFieldType.Memo
            RETURN -1
        ELSE
            result := ACE.AdsGetMemoLength(SELF:_Table, (DWORD)uiPos , OUT VAR length)
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

OVERRIDE METHOD RecInfo(uiOrdinal AS LONG, iRecID AS OBJECT, oNewValue AS OBJECT) AS OBJECT
    LOCAL dwRecno AS DWORD
    IF SELF:_Table == IntPtr.Zero
        RETURN NULL
    ENDIF
      //
    SWITCH (DbRecordInfo) uiOrdinal
    CASE DbRecordInfo.DBRI_LOCKED
        TRY
            dwRecno := Convert.ToUInt32(iRecID)
/*          IF dwRecNo == 0
                dwRecNo := SELF:RecNo // looks like the dll func does this anyway
            END IF*/
        CATCH
            SELF:ADSERROR(ERDD_DATATYPE, EG_DATATYPE, "RecInfo")
            RETURN FALSE
        END TRY
        SELF:_CheckError(ACE.AdsIsRecordLocked(SELF:_Table, dwRecno, OUT VAR locked))
        RETURN locked != 0

    CASE DbRecordInfo.DBRI_RECSIZE
        RETURN SELF:_RecordLength

    CASE DbRecordInfo.DBRI_UPDATED
        SELF:_CheckError(ACE.AdsRefreshRecord(SELF:_Table))
        RETURN NULL

    CASE DbRecordInfo.DBRI_RECNO
        RETURN SELF:RecNo
    OTHERWISE
        SELF:ADSERROR(ERDD_UNSUPPORTED, EG_UNSUPPORTED, "RecInfo")
    END SWITCH
RETURN SUPER:RecInfo( uiOrdinal, iRecID, oNewValue)


      /// <inheritdoc />
OVERRIDE METHOD Info(uiOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
    LOCAL result AS DWORD
    IF SELF:_Table == IntPtr.Zero
        RETURN SUPER:Info(uiOrdinal, oNewValue)
    ENDIF
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
        LOCAL dwValue := 0 AS LONG
        dwValue := SELF:FieldCount * 32 + 32
        RETURN (LONG) dwValue

    CASE DbInfo.DBI_LASTUPDATE
        LOCAL aDate AS CHAR[]
        LOCAL DateLen AS WORD

        aDate := CHAR[]{ACE.ADS_MAX_DATEMASK+1}
        DateLen := (WORD) aDate:Length
        SELF:_CheckError(ACE.AdsSetDateFormat("MM/DD/YYYY"))
        SELF:_CheckError(ACE.AdsGetLastTableUpdate(SELF:_Table, aDate, REF DateLen))
        SELF:_CheckError(ACE.AdsSetDateFormat(RuntimeState.DateFormat))
        local sDate as string
        sDate := String{aDate, 0, DateLen}
        local month, day, year as int
        month := Convert.ToInt32(sDate:Substring(0,2))
        day   := Convert.ToInt32(sDate:Substring(3,2))
        year  := Convert.ToInt32(sDate:Substring(6,4))
        return DbDate{year, month, day}

    CASE DbInfo.DBI_GETLOCKARRAY
    CASE DbInfo.DBI_LOCKCOUNT
        SELF:_CheckError(ACE.AdsGetNumLocks(SELF:_Table, OUT VAR numLocks))
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
        SELF:_CheckError(ACE.AdsIsTableLocked(SELF:_Table, OUT VAR isLocked))
        RETURN isLocked != 0

    CASE DbInfo.DBI_FILEHANDLE
        RETURN IntPtr.Zero

    CASE DbInfo.DBI_ISANSI
        RETURN FALSE

    CASE DbInfo.DBI_FOUND
        SELF:_CheckError(ACE.AdsIsFound(SELF:_Table, OUT VAR isFound))
        RETURN isFound != 0

    CASE DbInfo.DBI_SHARED
        SELF:_CheckError(ACE.AdsGetTableOpenOptions(SELF:_Table, OUT VAR options))
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

/// <inheritdoc/>
OVERRIDE METHOD Trans(info AS DbTransInfo) AS LOGIC
	RETURN SUPER:Trans(info)
/// <inheritdoc/>
OVERRIDE METHOD TransRec(info AS DbTransInfo) AS LOGIC
	RETURN SUPER:TransRec(info)


      #endregion

    #region Unsupported
    /// <inheritdoc />




INTERNAL METHOD Unsupported(strFunctionName AS STRING) AS LOGIC
    SELF:ADSERROR(ERDD.UNSUPPORTED, XSharp.Gencode.EG_UNSUPPORTED, strFunctionName)
RETURN FALSE

      /// <summary>This method is not supported by the AdsRDD class </summary>
OVERRIDE METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC
RETURN SELF:Unsupported("AppendLock")

      /// <summary>This method is not supported by the AdsRDD class </summary>
OVERRIDE METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT
    SELF:Unsupported("BlobInfo")
RETURN NULL

      /// <summary>This method is not supported by the AdsRDD class </summary>
OVERRIDE METHOD ForceRel() AS LOGIC
RETURN SELF:Unsupported("ForceRel")

      /// <summary>This method is not supported by the AdsRDD class </summary>
OVERRIDE METHOD GetRec() AS BYTE[]
    SELF:Unsupported("GetRec")
RETURN NULL

      /// <summary>This method is not supported by the AdsRDD class </summary>
OVERRIDE METHOD GetValueFile(nFldPos AS INT, cFileName AS STRING) AS LOGIC
    SELF:Unsupported("GetValueFile")
RETURN FALSE
      /// <summary>This method is not supported by the AdsRDD class </summary>
OVERRIDE METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC
RETURN SELF:Unsupported("HeaderLock")
      /// <summary>This method is not supported by the AdsRDD class </summary>
OVERRIDE METHOD PutRec(aRec AS BYTE[])			AS LOGIC
RETURN SELF:Unsupported("PutRec")
      /// <summary>This method is not supported by the AdsRDD class </summary>
OVERRIDE METHOD PutValueFile(nFldPos AS INT, cFileName AS STRING) AS LOGIC
    SELF:Unsupported("PutValueFile")
RETURN FALSE

      /// <summary>This method is not supported by the AdsRDD class </summary>
OVERRIDE METHOD Sort(info AS DbSortInfo) AS LOGIC
RETURN SELF:Unsupported("Sort")

      #endregion
END CLASS

