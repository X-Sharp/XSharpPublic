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

[DebuggerDisplay("AXDBFVFP ({Alias,nq})")];
CLASS XSharp.ADS.AXSQLRDD INHERIT ADSRDD
     PUBLIC _hStatement AS System.IntPtr

	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SELF:_hStatement := System.IntPtr.Zero
        SUPER:_Driver := "AXSQLRDD"

	/// <inheritdoc />
    VIRTUAL METHOD Close() AS LOGIC
        IF ! SUPER:Close()
            RETURN FALSE
        ENDIF
        IF SELF:_hStatement != System.IntPtr.Zero
            SUPER:_CheckError(ACE.AdsCloseSQLStatement(SELF:_hStatement))
            SELF:_hStatement := System.IntPtr.Zero
        ENDIF
        RETURN TRUE


    PRIVATE METHOD ACEORDER() AS System.IntPtr
        RETURN SUPER:_Table

    PROPERTY ACESQLStatementHandle AS System.IntPtr GET SELF:_hStatement

	/// <inheritdoc />
    VIRTUAL METHOD Info(uiOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
        IF (uiOrdinal == DBInfo.DBI_GET_ACE_STMT_HANDLE )
            RETURN SELF:_hStatement
        ENDIF
        RETURN SUPER:Info(uiOrdinal, oNewValue)

	/// <inheritdoc />
    VIRTUAL METHOD Open(openInfo AS DBOPENINFO) AS LOGIC
        LOCAL sName AS string
        LOCAL query AS object

        SUPER:_CheckRDDInfo()
        IF SELF:_Connection == IntPtr.Zero
            SUPER:_CheckError(ACEUNPUB.AdsSetLastError(5036, "The SQL driver requires a connection to Advantage."),EG_OPEN,__ENTITY__)
            RETURN FALSE
        ENDIF
        query := NULL_OBJECT
        IF CoreDb.RDDInfo(_SET_SQL_QUERY, REF query) .and. query is STRING
            sName := (STRING) query
        ELSE
            IF (openInfo:Extension != ".DBF")
                sName := openInfo:FileName + openInfo:Extension
            ELSE
                sName := openInfo:FileName
            ENDIF
        ENDIF
	    IF SELF:_SetPaths() != 0
		    RETURN FALSE
	    ENDIF

        SELF:_CheckError(ACE.AdsCreateSQLStatement(SELF:_Connection, REF SELF:_hStatement),EG_OPEN,"AdsCreateSQLStatement")

        SELF:_CheckError(ACE.AdsStmtSetTableType(SELF:_hStatement, SELF:_TableType),EG_OPEN,"AdsStmtSetTableType")

        var charset := IIF (RuntimeState.CollationMode == CollationMode.Clipper, ACE.ADS_OEM,ACE.ADS_ANSI)
        SELF:_CheckError(ACE.AdsStmtSetTableCharType(SELF:_hStatement, charset),EG_OPEN,"AdsStmtSetTableCharType")

        SELF:_CheckError(ACE.AdsStmtSetTableLockType(SELF:_hStatement, SUPER:_LockType),EG_OPEN,"AdsStmtSetTableLockType")
        var cursorMode := IIF (openInfo:ReadOnly, ACE.ADS_CURSOR_READONLY , ACE.ADS_CURSOR_READWRITE)

        SELF:_CheckError(ACE.AdsStmtSetTableReadOnly(SELF:_hStatement, cursorMode),EG_OPEN,"AdsStmtSetTableReadOnly")

        SELF:_CheckError(ACE.AdsStmtSetTableRights(SELF:_hStatement, SELF:_CheckRights),EG_OPEN,"AdsStmtSetTableRights")

        IF ! String.IsNullOrEmpty(SELF:_Collation) .and. (SELF:_TableType == ACE.ADS_ADT .or. SELF:_TableType == ACE.ADS_VFP)
            SELF:_CheckError(ACE.AdsStmtSetTableCollation(SELF:_hStatement, SELF:_Collation),EG_OPEN,"AdsStmtSetTableCollation")
        ENDIF
        LOCAL password := NULL_OBJECT AS OBJECT
        IF CoreDb.RDDInfo(_SET_SQL_TABLE_PASSWORDS, ref password) .and. password is object[] var oInfo
            FOREACH var element in oInfo
                var oSub := (object[]) element
                VAR cTableName := (String) oSub[0]
                VAR cPassword  := (String) oSub[1]
                SELF:_CheckError(ACE.AdsStmtSetTablePassword(SELF:_hStatement, cTableName, cPassword),EG_OPEN,"AdsStmtSetTablePassword")

            NEXT
        ENDIF
        LOCAL nRes := ACE.AdsExecuteSQLDirect(SELF:_hStatement, sName, REF _Table) as DWORD
        IF nRes != 0
            SELF:Close()
            SELF:_CheckError(nRes, EG_OPEN,"AdsExecuteSQLDirect")
        ENDIF
        IF SELF:_Table == IntPtr.Zero
            SELF:Close()
            RETURN FALSE
        ENDIF
        SELF:_CheckError(ACE.AdsGetTableType(SELF:_Table, REF _TableType),EG_OPEN,"AdsGetTableType")
        IF ! SELF:_FieldSub()
            SELF:Close()
            RETURN FALSE
        ENDIF
        LOCAL length := MAX_PATH AS WORD
	    LOCAL afileName AS CHAR[]
	    afileName := CHAR[]{length}
        SELF:_CheckError(ACE.AdsGetTableFilename(SELF:_Table, ACE.ADS_FULLPATHNAME, afileName, REF length),EG_OPEN,"AdsGetTableFilename")
    	SELF:_FileName := STRING {aFileName,0, length}
        SELF:_Encoding := Encoding.GetEncoding(IIF (charset == ACE.ADS_ANSI, RuntimeState.WinCodePage,RuntimeState.DosCodePage))
        SELF:Alias   := openInfo:Alias
	    SELF:Area    := openInfo:WorkArea
        RETURN SUPER:RecordMovement()
 

	/// <inheritdoc />
   VIRTUAL METHOD RecInfo( uiOrdinal AS INT, iRecID AS OBJECT, oNewValue AS OBJECT) AS OBJECT
    LOCAL isLive AS BYTE
    LOCAL recNum AS DWORD
    LOCAL dwCRC AS DWORD
    LOCAL dwCRC2 AS DWORD
    IF uiOrdinal != DBRecordInfo.DBRI_UPDATED  
        RETURN SUPER:RecInfo(uiOrdinal, iRecID, oNewValue)
    ENDIF
    IF ACEUNPUB.AdsSqlPeekStatement(SUPER:_Table, OUT isLive) == 0 .AND. isLive == 0
        SUPER:_CheckError(ACE.AdsGetRecordNum(SUPER:_Table, ACE.ADS_IGNOREFILTERS, OUT recNum))
        SUPER:_CheckError(ACE.AdsGetRecordCRC(SUPER:_Table, OUT dwCRC, 1))
        ACE.AdsCloseTable(SUPER:_Table)
        SUPER:_Table := System.IntPtr.Zero
        SUPER:_Index := System.IntPtr.Zero
        SUPER:_CheckError(ACE.AdsExecuteSQL(SELF:_hStatement, OUT SELF:_Table))
        IF ACE.AdsGotoRecord(SUPER:_Table, recNum) == 0
            IF ACE.AdsGetRecordCRC(SUPER:_Table, OUT dwCRC2, 1) == 0 .AND. dwCRC == dwCRC2
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
