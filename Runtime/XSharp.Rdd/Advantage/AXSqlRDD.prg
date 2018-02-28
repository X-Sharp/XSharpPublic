//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING XSharp.RDD



CLASS XSharp.Ads.AXSQLRDD INHERIT ADSRDD
     PUBLIC m_hStatement AS System.IntPtr

    CONSTRUCTOR()
        SELF:m_hStatement := System.IntPtr.Zero
        SUPER:m_strDriver := "Advantage.AXSQLRDD"

    VIRTUAL PROPERTY SysName AS STRING GET typeof(AXSQLRDD):ToString()

    VIRTUAL METHOD Close() AS Logic
        IF (! SUPER:Close())
            RETURN FALSE
        ENDIF
        IF (SELF:m_hStatement != System.IntPtr.Zero)
            SUPER:ACECALL(ACE.AdsCloseSQLStatement(SELF:m_hStatement))
            SELF:m_hStatement := System.IntPtr.Zero
        ENDIF
        RETURN TRUE


    PRIVATE METHOD ACEORDER() AS System.IntPtr
        RETURN SUPER:m_hTable

    PROPERTY ACESQLStatementHandle AS System.IntPtr GET SELF:m_hStatement

    VIRTUAL METHOD Info(uiOrdinal AS Int, oNewValue as Object) AS Object
        IF (uiOrdinal == DBI_GET_ACE_STMT_HANDLE )
            Return SELF:m_hStatement
        ENDIF
        RETURN SUPER:Info(uiOrdinal, oNewValue)

    VIRTUAL METHOD Open(lpOpenInfo AS DBOPENINFO) AS Logic
        //LOCAL sName AS string
        //LOCAL usual3 AS __Usual
        //LOCAL usual AS __Usual
        //LOCAL @@array AS __Array
        //LOCAL i AS Long
        //LOCAL array2 AS __Array
        //LOCAL pucTableName AS string
        //LOCAL pucPassword AS string
        //LOCAL pucName AS Char[]
        //LOCAL length AS Word
        //LOCAL usual2 AS __Usual
        ////
        //SUPER:PrintCallTrace(<string>{"Open"})
        //SUPER:AxCheckRDDInfo()
        //IF (SUPER:m_hConnection == System.IntPtr.Zero)
            ////
            //SUPER:ACECALL(ACEUNPUB.AdsSetLastError(5036, "The SQL driver requires a connection to Advantage."))
            //RETURN FALSE
        //ENDIF
        //usual3 := __Usual._NIL
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
        //IF (! SUPER:ACECALL(ACE.AdsCreateSQLStatement(SUPER:m_hConnection, @(SELF:m_hStatement))))
            ////
            //RETURN FALSE
        //ENDIF
        //IF (! SUPER:ACECALL(ACE.AdsStmtSetTableType(SELF:m_hStatement, SUPER:m_usTableType)))
            ////
            //RETURN FALSE
        //ENDIF
        //IF (RuntimeState.Collation == 1)
            ////
            //IF (! SUPER:ACECALL(ACE.AdsStmtSetTableCharType(SELF:m_hStatement, 2)))
                ////
                //RETURN FALSE
            //ENDIF
        //ELSE
            ////
            //IF (! SUPER:ACECALL(ACE.AdsStmtSetTableCharType(SELF:m_hStatement, 1)))
                ////
                //RETURN FALSE
            //ENDIF
        //ENDIF
        //IF (! SUPER:ACECALL(ACE.AdsStmtSetTableLockType(SELF:m_hStatement, SUPER:m_usLockType)))
            ////
            //RETURN FALSE
        //ENDIF
        //IF (lpOpenInfo:fReadOnly)
            ////
            //IF (! SUPER:ACECALL(ACE.AdsStmtSetTableReadOnly(SELF:m_hStatement, 1)))
                ////
                //RETURN FALSE
            //ENDIF
        //ELSE
            ////
            //IF (! SUPER:ACECALL(ACE.AdsStmtSetTableReadOnly(SELF:m_hStatement, 2)))
                ////
                //RETURN FALSE
            //ENDIF
        //ENDIF
        //IF (! SUPER:ACECALL(ACE.AdsStmtSetTableRights(SELF:m_hStatement, SUPER:m_usCheckRights)))
            ////
            //RETURN FALSE
        //ENDIF
        //IF ((! String.IsNullOrEmpty(SUPER:m_strCollation) .AND. ((SUPER:m_usTableType == 3) .OR. (SUPER:m_usTableType == 4))) .AND. ! SUPER:ACECALL(ACE.AdsStmtSetTableCollation(SELF:m_hStatement, SUPER:m_strCollation)))
            ////
            //RETURN FALSE
        //ENDIF
        //usual := __Usual._NIL
        //IF (Functions.VODBRDDInfo(206, @(usual)) .AND. (usual != __Usual._NIL))
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
                    //IF (! SUPER:ACECALL(ACE.AdsStmtSetTablePassword(SELF:m_hStatement, pucTableName, pucPassword)))
                        ////
                        //RETURN FALSE
                    //ENDIF
                    //i++
                //ENDDO
            //CATCH obj1 as Object
//
            //END TRY
        //ENDIF
        //IF (! SUPER:ACECALL(ACE.AdsExecuteSQLDirect(SELF:m_hStatement, sName, @(SELF:m_hTable))))
            ////
            //SUPER:PrintCallTrace(<string>{"AdsExecuteSQLDirect failed"})
            //SELF:Close()
            //RDDBase.SetNetErr(TRUE)
            //RETURN FALSE
        //ENDIF
        //IF (! (SUPER:m_hTable != System.IntPtr.Zero))
            ////
            //RETURN SELF:Close()
        //ENDIF
        //pucName := Char[]{261}
        //length := (Word)pucName:Length 
        //IF (! SUPER:ACECALL(ACE.AdsGetTableType(SUPER:m_hTable, @(SELF:m_usTableType))))
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
        //IF (ACE.AdsGetTableFilename(SUPER:m_hTable, 3, pucName, @(length)) == 0)
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
        //usual2 := (__Usual)lpOpenInfo:sAlias:Substring(0, lpOpenInfo:sAlias:IndexOf(' ')) 
        //Functions.Default(@(usual2), "")
        //SUPER:m_alias := Functions.__ConstructUniqueAlias(usual2)
        //RDDBase.aliases[((Long)RuntimeState.get_CurrentWorkarea()  - 1) + 1] := SUPER:m_alias
        //ACE.AdsGetIndexHandle(SUPER:m_hTable, null, @(SELF:m_hIndex))
        RETURN SUPER:RecordMovement()
 

   VIRTUAL METHOD RecInfo(iRecID AS Object, uiOrdinal AS Int, oNewValue as OBJECT) AS OBJECT
    LOCAL isLive AS Byte
    LOCAL recNum AS DWord
    LOCAL dwCRC AS DWord
    LOCAL dwCRC2 AS DWord
    IF uiOrdinal != DBRI_UPDATED  
        RETURN SUPER:RecInfo(iRecID, uiOrdinal, oNewValue)
    ENDIF
    IF ACEUNPUB.AdsSqlPeekStatement(SUPER:m_hTable, out isLive) == 0 .AND. isLive == 0
        SUPER:ACECALL(ACE.AdsGetRecordNum(SUPER:m_hTable, ACE.ADS_IGNOREFILTERS, out recNum))
        SUPER:ACECALL(ACE.AdsGetRecordCRC(SUPER:m_hTable, out dwCRC, 1))
        ACE.AdsCloseTable(SUPER:m_hTable)
        SUPER:m_hTable := System.IntPtr.Zero
        SUPER:m_hIndex := System.IntPtr.Zero
        SUPER:ACECALL(ACE.AdsExecuteSQL(SELF:m_hStatement, OUT SELF:m_hTable))
        IF ACE.AdsGotoRecord(SUPER:m_hTable, recNum) == 0
            IF ACE.AdsGetRecordCRC(SUPER:m_hTable, out dwCRC2, 1) == 0 .AND. dwCRC == dwCRC2
                SUPER:RecordMovement()
                SUPER:_Found := TRUE
            ELSE
                SUPER:RecordMovement()
            ENDIF
        ELSE
            SUPER:ACECALL(ACE.AdsGotoTop(SUPER:m_hTable))
            SUPER:RecordMovement()
        ENDIF
        SUPER:ACECALL(ACE.AdsGetIndexHandle(SUPER:m_hTable, null, OUT SELF:m_hIndex))
    ELSE
        SUPER:ACECALL(ACE.AdsRefreshRecord(SUPER:m_hTable))
    ENDIF
    RETURN TRUE

 


END CLASS
