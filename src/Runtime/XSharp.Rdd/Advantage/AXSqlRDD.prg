//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING XSharp.RDD
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Text
USING System.Diagnostics

/// <summary>Base class for all advantage SQL RDDs.</summary>
[DebuggerDisplay("AXDBFVFP ({Alias,nq})")];
CLASS XSharp.ADS.AXSQLRDD INHERIT ADSRDD
    PUBLIC _hStatement AS System.IntPtr

    /// <summary>Create instance of the RDD </summary>
    CONSTRUCTOR()
        SELF:_hStatement := IntPtr.Zero
        SUPER:_Driver := "AXSQLRDD"

    /// <inheritdoc />
    OVERRIDE METHOD Close() AS LOGIC
        IF ! SUPER:Close()
            RETURN FALSE
        ENDIF
        IF SELF:_hStatement != IntPtr.Zero
            SUPER:_CheckError(ACE.AdsCloseSQLStatement(SELF:_hStatement))
            SELF:_hStatement := IntPtr.Zero
        ENDIF
        RETURN TRUE


    PRIVATE METHOD ACEORDER() AS System.IntPtr
        RETURN SUPER:_Table

    PROPERTY ACESQLStatementHandle AS System.IntPtr GET SELF:_hStatement

    /// <inheritdoc />
    OVERRIDE METHOD Info(uiOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
        IF (uiOrdinal == DbInfo.DBI_GET_ACE_STMT_HANDLE )
            RETURN SELF:_hStatement
        ENDIF
        IF (uiOrdinal == _SET_SQL_PARAMETERS)
            LOCAL oP := NULL as Object
            CoreDb.RddInfo(_SET_SQL_PARAMETERS, REF oP)
            if oP is object[] VAR oParams
                local nRes := 0 AS DWORD
                SELF:GoCold()
                IF SELF:_Table != IntPtr.Zero

                    nRes := ACE.AdsCloseTable(SELF:_Table)
                    SELF:_Table := IntPtr.Zero
                ENDIF
                if nRes == 0
                    nRes := SetParameters(oParams)
                ENDIF
                if nRes == 0
                    nRes := ACE.AdsExecuteSQL(SELF:_hStatement, OUT _Table)
                endif
                IF nRes != 0
                    local err := NULL as Exception
                    TRY
                        SELF:_CheckError(nRes, EG_OPEN,"AdsExecuteSQL")
                    CATCH oError as Exception
                        err := oError
                    FINALLY
                        SELF:Close()
                        RuntimeState.LastRddError := err
                    END TRY
                endif
            ENDIF
        ENDIF
        RETURN SUPER:Info(uiOrdinal, oNewValue)

    /// <inheritdoc />
    OVERRIDE METHOD Open(openInfo AS DbOpenInfo) AS LOGIC
        LOCAL sName AS string
        LOCAL query AS object

        SUPER:_CheckRDDInfo()
        IF SELF:_Connection == IntPtr.Zero
            SUPER:_CheckError(ACEUNPUB.AdsSetLastError(5036, "The SQL driver requires a connection to Advantage."),EG_OPEN,__ENTITY__)
            RETURN FALSE
        ENDIF
        query := NULL_OBJECT
        IF CoreDb.RddInfo(_SET_SQL_QUERY, REF query) .and. query is STRING
            sName := (STRING) query
        ELSE
            sName := openInfo:FullName
        ENDIF
        IF SELF:_SetPaths(EG_OPEN) != 0
            RETURN FALSE
        ENDIF

        SELF:_CheckError(ACE.AdsCreateSQLStatement(SELF:_Connection, OUT SELF:_hStatement),EG_OPEN,"AdsCreateSQLStatement")

        SELF:_CheckError(ACE.AdsStmtSetTableType(SELF:_hStatement, SELF:_TableType),EG_OPEN,"AdsStmtSetTableType")

        LOCAL charset AS WORD
        // both Clipper and XPP use weight tables
        IF RuntimeState.CollationMode == CollationMode.Clipper .OR. RuntimeState.CollationMode == CollationMode.Xpp
            charset := ACE.ADS_OEM
        ELSE
            charset := ACE.ADS_ANSI
        ENDIF
        SELF:_CheckError(ACE.AdsStmtSetTableCharType(SELF:_hStatement, charset),EG_OPEN,"AdsStmtSetTableCharType")

        SELF:_CheckError(ACE.AdsStmtSetTableLockType(SELF:_hStatement, SUPER:_LockType),EG_OPEN,"AdsStmtSetTableLockType")
        var cursorMode := IIF (openInfo:ReadOnly, ACE.ADS_CURSOR_READONLY , ACE.ADS_CURSOR_READWRITE)

        SELF:_CheckError(ACE.AdsStmtSetTableReadOnly(SELF:_hStatement, cursorMode),EG_OPEN,"AdsStmtSetTableReadOnly")

        SELF:_CheckError(ACE.AdsStmtSetTableRights(SELF:_hStatement, SELF:_CheckRights),EG_OPEN,"AdsStmtSetTableRights")

        IF ! String.IsNullOrEmpty(SELF:_Collation) .and. (SELF:_TableType == ACE.ADS_ADT .or. SELF:_TableType == ACE.ADS_VFP)
            SELF:_CheckError(ACE.AdsStmtSetTableCollation(SELF:_hStatement, SELF:_Collation),EG_OPEN,"AdsStmtSetTableCollation")
        ENDIF
        LOCAL password := NULL_OBJECT AS OBJECT
        IF CoreDb.RddInfo(_SET_SQL_TABLE_PASSWORDS, ref password) .and. password is object[] var oInfo
            FOREACH var element in oInfo
                var oSub := (object[]) element
                VAR cTableName := (String) oSub[0]
                VAR cPassword  := (String) oSub[1]
                SELF:_CheckError(ACE.AdsStmtSetTablePassword(SELF:_hStatement, cTableName, cPassword),EG_OPEN,"AdsStmtSetTablePassword")

            NEXT
        ENDIF

        LOCAL nRes as DWORD
        LOCAL oP := NULL as Object
        CoreDb.RddInfo(_SET_SQL_PARAMETERS, REF oP)
        nRes := ACE.AdsPrepareSQL(SELF:_hStatement, sName)
        if nRes == 0 .and. oP is object[] VAR oParams
            nRes := SetParameters(oParams)
        endif
        if nRes == 0
            nRes := ACE.AdsExecuteSQL(SELF:_hStatement, OUT _Table)
        endif
        IF nRes != 0
            local err := NULL as Exception
            TRY
                SELF:_CheckError(nRes, EG_OPEN,"AdsExecuteSQL")
            CATCH oError as Exception
                err := oError
            FINALLY
                SELF:Close()
                RuntimeState.LastRddError := err
            END TRY
        ENDIF
        IF SELF:_Table == IntPtr.Zero
            SELF:Close()
            RETURN FALSE
        ENDIF
        SELF:_CheckError(ACE.AdsGetTableType(SELF:_Table, OUT _TableType),EG_OPEN,"AdsGetTableType")
        IF ! SELF:_FieldSub()
            SELF:Close()
            RETURN FALSE
        ENDIF
        LOCAL length := MAX_PATH AS WORD
        LOCAL afileName AS CHAR[]
        afileName := CHAR[]{length}
        SELF:_CheckError(ACE.AdsGetTableFilename(SELF:_Table, ACE.ADS_FULLPATHNAME, afileName, REF length),EG_OPEN,"AdsGetTableFilename")
        SELF:_FileName := STRING {afileName,0, length}
        SELF:_Encoding := Encoding.GetEncoding(IIF (charset == ACE.ADS_ANSI, RuntimeState.WinCodePage,RuntimeState.DosCodePage))
        SELF:Alias   := openInfo:Alias
        SELF:Area    := openInfo:Workarea
        RETURN SUPER:RecordMovement()


    METHOD SetParameters(oParams as OBJECT[]) AS DWORD
        local nRes := 0 as DWORD
        foreach VAR oParam in oParams
            if oParam is object[] VAR oTuple
                if oTuple:Length >= 2
                    var oName  := oTuple[0]
                    var oValue := oTuple[1]
                    IF oName IS String VAR strName
                        if oValue IS String VAR strValue
                            nRes := ACE.AdsSetString(SELF:_hStatement, strName, strValue, (DWORD) strValue:Length)
                        elseif oValue is LONG var liValue
                            nRes := ACE.AdsSetLong(SELF:_hStatement, strName, liValue)
                        elseif oValue is IFloat var flValue
                            nRes := ACE.AdsSetDouble(SELF:_hStatement, strName, flValue:Value)
                        elseif oValue is IDate var oDate
                            var dt := DateTime{oDate:Year, oDate:Month, oDate:Day}
                            VAR strValue := dt:ToString("yyyyMMdd")
                            nRes := ACEUNPUB.AdsConvertStringToJulian(strValue, (WORD) strValue:Length, OUT var r8Julian)
                            nRes := ACE.AdsSetJulian(SELF:_hStatement, strName, (LONG) r8Julian)
                        endif
                    ENDIF
                    IF oName is IFloat VAR flName
                        oName := (LONG) flName:Value
                    ENDIF
                    IF oName IS Long VAR liName
                        var dwName := (DWORD) liName
                        if oValue IS String VAR strValue
                            nRes := ACE.AdsSetString(SELF:_hStatement, dwName, strValue, (DWORD) strValue:Length)
                        elseif oValue is LONG var liValue
                            nRes := ACE.AdsSetLong(SELF:_hStatement, dwName, liValue)
                        elseif oValue is IFloat var flValue
                            nRes := ACE.AdsSetDouble(SELF:_hStatement, dwName, flValue:Value)
                        elseif oValue is IDate var oDate
                            var dt := DateTime{oDate:Year, oDate:Month, oDate:Day}
                            VAR strValue := dt:ToString("yyyyMMdd")
                            nRes := ACEUNPUB.AdsConvertStringToJulian(strValue, (WORD) strValue:Length, OUT var r8Julian)
                            nRes := ACE.AdsSetJulian(SELF:_hStatement, dwName, (LONG) r8Julian)
                        endif
                    ENDIF
                    if nRes != 0
                        RETURN nRes
                    ENDIF
                endif
            endif
        next
        RETURN nRes

    /// <inheritdoc />
    OVERRIDE METHOD RecInfo( uiOrdinal AS INT, iRecID AS OBJECT, oNewValue AS OBJECT) AS OBJECT
        LOCAL isLive AS BYTE
        LOCAL recNum AS DWORD

        IF uiOrdinal != DbRecordInfo.DBRI_UPDATED .or. SELF:_Table == IntPtr.Zero
            RETURN SUPER:RecInfo(uiOrdinal, iRecID, oNewValue)
        ENDIF
        IF ACEUNPUB.AdsSqlPeekStatement(SUPER:_Table, OUT isLive) == 0 .AND. isLive == 0
            SUPER:_CheckError(ACE.AdsGetRecordNum(SUPER:_Table, ACE.ADS_IGNOREFILTERS, OUT recNum))
            SUPER:_CheckError(ACE.AdsGetRecordCRC(SUPER:_Table, OUT VAR dwCRC, 1))
            ACE.AdsCloseTable(SUPER:_Table)
            SUPER:_Table := System.IntPtr.Zero
            SUPER:_Index := System.IntPtr.Zero
            SUPER:_CheckError(ACE.AdsExecuteSQL(SELF:_hStatement, OUT SELF:_Table))
            IF ACE.AdsGotoRecord(SUPER:_Table, recNum) == 0
                IF ACE.AdsGetRecordCRC(SUPER:_Table, OUT VAR dwCRC2, 1) == 0 .AND. dwCRC == dwCRC2
                    SUPER:RecordMovement()
                    SUPER:_Found := TRUE
                ELSE
                    SUPER:RecordMovement()
                ENDIF
            ELSE
                SUPER:_CheckError(ACE.AdsGotoTop(SUPER:_Table))
                SUPER:RecordMovement()
            ENDIF
            SUPER:_CheckError(ACE.AdsGetIndexHandle(SUPER:_Table, NULL, OUT SELF:_Index))
        ELSE
            SUPER:_CheckError(ACE.AdsRefreshRecord(SUPER:_Table))
        ENDIF
        RETURN TRUE

END CLASS
