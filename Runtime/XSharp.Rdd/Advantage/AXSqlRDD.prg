//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING XSharp.RDD
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
CLASS XSharp.ADS.AXSQLRDD INHERIT ADSRDD
     PUBLIC m_hStatement AS System.IntPtr

	/// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SELF:m_hStatement := System.IntPtr.Zero
        SUPER:_Driver := "AXSQLRDD"

	/// <inheritdoc />
    VIRTUAL METHOD Close() AS LOGIC
        IF (! SUPER:Close())
            RETURN FALSE
        ENDIF
        IF (SELF:m_hStatement != System.IntPtr.Zero)
            SUPER:_CheckError(ACE.AdsCloseSQLStatement(SELF:m_hStatement))
            SELF:m_hStatement := System.IntPtr.Zero
        ENDIF
        RETURN TRUE


    PRIVATE METHOD ACEORDER() AS System.IntPtr
        RETURN SUPER:_Table

    PROPERTY ACESQLStatementHandle AS System.IntPtr GET SELF:m_hStatement

	/// <inheritdoc />
    VIRTUAL METHOD Info(uiOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
        IF (uiOrdinal == DBInfo.DBI_GET_ACE_STMT_HANDLE )
            RETURN SELF:m_hStatement
        ENDIF
        RETURN SUPER:Info(uiOrdinal, oNewValue)
	/// <inheritdoc />
    VIRTUAL METHOD Open(lpOpenInfo AS DBOPENINFO) AS LOGIC
        //LOCAL sName AS string
        //LOCAL usual3 AS Usual
        //LOCAL usual AS Usual
        //LOCAL @@array AS Array
        //LOCAL i AS Long
        //LOCAL array2 AS Array
        //LOCAL pucTableName AS string
        //LOCAL pucPassword AS string
        //LOCAL pucName AS Char[]
        //LOCAL length AS Word
        //LOCAL usual2 AS Usual
        ////
        //SUPER:PrintCallTrace(<string>{"Open"})
        //SUPER:AxCheckRDDInfo()
        //IF (SUPER:m_hConnection == System.IntPtr.Zero)
            ////
            //SUPER:_CheckError(ACEUNPUB.AdsSetLastError(5036, "The SQL driver requires a connection to Advantage."))
            //RETURN FALSE
        //ENDIF
        //usual3 := Usual._NIL
        //IF (Functions.VODBRDDInfo(205, @(usual3)))
            ////
            //sName := usual3
        //ELSE
            ////
            //IF (lpOpenInfo:sExt != ".DBF")
                ////
                //sName := String.Concat(lpOpenInfo:sName, lpOpenInfo:sExt)
            //ELSE
                ////
                //sName := lpOpenInfo:sName
            //ENDIF
        //ENDIF
        //IF (SUPER:SetPaths() != 0)
            ////
            //RETURN FALSE
        //ENDIF
        //IF (! SUPER:_CheckError(ACE.AdsCreateSQLStatement(SUPER:m_hConnection, @(SELF:m_hStatement))))
            ////
            //RETURN FALSE
        //ENDIF
        //IF (! SUPER:_CheckError(ACE.AdsStmtSetTableType(SELF:m_hStatement, SUPER:m_usTableType)))
            ////
            //RETURN FALSE
        //ENDIF
        //IF (RuntimeState.Collation == 1)
            ////
            //IF (! SUPER:_CheckError(ACE.AdsStmtSetTableCharType(SELF:m_hStatement, 2)))
                ////
                //RETURN FALSE
            //ENDIF
        //ELSE
            ////
            //IF (! SUPER:_CheckError(ACE.AdsStmtSetTableCharType(SELF:m_hStatement, 1)))
                ////
                //RETURN FALSE
            //ENDIF
        //ENDIF
        //IF (! SUPER:_CheckError(ACE.AdsStmtSetTableLockType(SELF:m_hStatement, SUPER:m_usLockType)))
            ////
            //RETURN FALSE
        //ENDIF
        //IF (lpOpenInfo:fReadOnly)
            ////
            //IF (! SUPER:_CheckError(ACE.AdsStmtSetTableReadOnly(SELF:m_hStatement, 1)))
                ////
                //RETURN FALSE
            //ENDIF
        //ELSE
            ////
            //IF (! SUPER:_CheckError(ACE.AdsStmtSetTableReadOnly(SELF:m_hStatement, 2)))
                ////
                //RETURN FALSE
            //ENDIF
        //ENDIF
        //IF (! SUPER:_CheckError(ACE.AdsStmtSetTableRights(SELF:m_hStatement, SUPER:m_usCheckRights)))
            ////
            //RETURN FALSE
        //ENDIF
        //IF ((! String.IsNullOrEmpty(SUPER:m_strCollation) .AND. ((SUPER:m_usTableType == 3) .OR. (SUPER:m_usTableType == 4))) .AND. ! SUPER:_CheckError(ACE.AdsStmtSetTableCollation(SELF:m_hStatement, SUPER:m_strCollation)))
            ////
            //RETURN FALSE
        //ENDIF
        //usual := Usual._NIL
        //IF (Functions.VODBRDDInfo(206, @(usual)) .AND. (usual != Usual._NIL))
            ////
            //TRY
                ////
                //@@array := usual
                //i := 0
                //WHILE ((i < @@array:get_Length()))
                    ////
                    //array2 := @@array:__GetElement(i)
                    //pucTableName := array2:__GetElement(0)
                    //pucPassword := array2:__GetElement(1)
                    //IF (! SUPER:_CheckError(ACE.AdsStmtSetTablePassword(SELF:m_hStatement, pucTableName, pucPassword)))
                        ////
                        //RETURN FALSE
                    //ENDIF
                    //i++
                //ENDDO
            //CATCH obj1 as Object
//
            //END TRY
        //ENDIF
        //IF (! SUPER:_CheckError(ACE.AdsExecuteSQLDirect(SELF:m_hStatement, sName, @(SELF:_Table))))
            ////
            //SUPER:PrintCallTrace(<string>{"AdsExecuteSQLDirect failed"})
            //SELF:Close()
            //RDDBase.SetNetErr(TRUE)
            //RETURN FALSE
        //ENDIF
        //IF (! (SUPER:_Table != System.IntPtr.Zero))
            ////
            //RETURN SELF:Close()
        //ENDIF
        //pucName := Char[]{261}
        //length := (Word)pucName:Length 
        //IF (! SUPER:_CheckError(ACE.AdsGetTableType(SUPER:_Table, @(SELF:m_usTableType))))
            ////
            //SELF:Close()
            //RDDBase.SetNetErr(TRUE)
            //RETURN FALSE
        //ENDIF
        //IF (! SUPER:AxFieldSub())
            ////
            //SELF:Close()
            //RDDBase.SetNetErr(TRUE)
            //RETURN FALSE
        //ENDIF
        //IF (ACE.AdsGetTableFilename(SUPER:_Table, 3, pucName, @(length)) == 0)
            ////
            //SUPER:m_dbfName := string{pucName, 0, length}
            //SUPER:m_fileName := SUPER:m_dbfName
        //ENDIF
        //IF (RuntimeState.Collation == 1)
            ////
            //SUPER:m_Encoding := System.Text.Encoding.GetEncoding((Long)SUPER:GetDosCodePage() )
        //ELSE
            ////
            //SUPER:m_Encoding := System.Text.Encoding.GetEncoding((Long)SUPER:GetWinCodePage() )
        //ENDIF
        //SUPER:m_uiArea := lpOpenInfo:uiArea
        //usual2 := (Usual)lpOpenInfo:sAlias:Substring(0, lpOpenInfo:sAlias:IndexOf(' ')) 
        //Functions.Default(@(usual2), "")
        //SUPER:m_alias := Functions.__ConstructUniqueAlias(usual2)
        //RDDBase.aliases[((Long)RuntimeState.get_CurrentWorkarea()  - 1) + 1] := SUPER:m_alias
        //ACE.AdsGetIndexHandle(SUPER:_Table, null, @(SELF:m_hIndex))
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
        SUPER:_CheckError(ACE.AdsExecuteSQL(SELF:m_hStatement, OUT SELF:_Table))
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
