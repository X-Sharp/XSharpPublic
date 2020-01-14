//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.ADS
using System.Runtime.InteropServices
using System.Runtime.CompilerServices

/// <summary>Return the AXS locking status.</summary>
FUNCTION AX_AXSLocking( ) AS LOGIC 
    RETURN AX_RddHelper(_SET_AXSLOCKING, TRUE)
    
    /// <summary>Return and set the AXS locking status.</summary>
FUNCTION AX_AXSLocking( bMode AS LOGIC) AS LOGIC 
    RETURN AX_RddHelper(_SET_AXSLOCKING, bMode, TRUE)
    
    /// <summary>copy a BLOB to a file.</summary>
FUNCTION AX_BLOB2File( cFileName AS STRING, cFieldName AS STRING ) AS LOGIC 
    LOCAL hTable AS DWORD
    LOCAL ulRetCode AS DWORD
    
    hTable := AX_GetAceTableHandle()
    ulRetCode := ACE.AdsBinaryToFile( hTable, cFieldName , cFileName )
    RETURN ulRetCode == 0 
    
    /// <summary>copy a file into a BLOB.</summary>
FUNCTION AX_File2BLOB( cFileName AS STRING, cFieldName AS STRING ) AS LOGIC 
    LOCAL hTable AS DWORD
    LOCAL ulRetCode AS DWORD
    hTable := AX_GetAceTableHandle()
    ulRetCode := ACE.AdsFileToBinary( hTable, cFieldName , ACE.ADS_BINARY , cFileName )
    RETURN ulRetCode == 0 
    
    /// <summary>Returns an  index handle for the current workarea.  This handle can be used
        /// to call the Advantage Client Engine directly.</summary>
    /// <returns> Returns a 0 if there is a problem or if no index was found..</returns>
FUNCTION AX_GetAceIndexHandle( uIndexFile AS OBJECT, uOrder AS OBJECT) AS DWORD

    // uIndexFile -- filename or NIL
    // uOrder -- order name, number, or NIL
    LOCAL oRet := NULL AS OBJECT
    IF CoreDb.OrderInfo(DBOI_GET_ACE_INDEX_HANDLE, "", uOrder, REF oRet)
        IF oRet IS IntPtr VAR pHandle
            RETURN (DWORD) pHandle:ToInt32()
        ENDIF
    ENDIF
    RETURN 0


Function GetAceIndexHandle() AS DWORD
   return Ax_GetAceIndexHandle(NULL_OBJECT, NULL_OBJECT)
    
    
/// <summary>Returns the statement handle for the current workarea.  This handle can be used
/// to call the Advantage Client Engine directly.  Only for use with the AXSQL RDDs.</summary>
/// <returns> Returns a 0 if there is a problem.</returns>
FUNCTION AX_GetAceStmtHandle() AS DWORD
    LOCAL oHandle := NULL AS OBJECT
    IF CoreDb.Info( DBI_GET_ACE_STMT_HANDLE , REF oHandle)
        IF oHandle IS IntPtr VAR pHandle
            RETURN (DWORD) pHandle:ToInt32()
        ENDIF
    ENDIF
    RETURN 0
    
/// <summary>Returns the table handle for the current workarea.  This handle can be used to call the Advantage Client Engine directly.</summary>
/// <returns> Returns a 0 if there is a problem.</returns>
FUNCTION AX_GetAceTableHandle() AS DWORD
    LOCAL oHandle := NULL AS OBJECT
    IF CoreDb.Info( DBI_GET_ACE_TABLE_HANDLE , REF oHandle)
        IF oHandle IS IntPtr VAR pHandle
            RETURN (DWORD) pHandle:ToInt32()
        ENDIF
    ENDIF
    RETURN 0

Function GetAceTableHandle() AS DWORD
   return AX_GetAceTableHandle()


/// <summary>Return .T. if Advantage is loaded.</summary>
/// <remarks>cFileName must start with a drive letter ("X:\") or a UNC path ("\\server\volume\path\")</remarks>
FUNCTION AX_IsServerLoaded( cFileName AS STRING ) AS LOGIC // 
    LOCAL usLoaded AS WORD
    usLoaded := 0
    ACE.AdsIsServerLoaded  (  cFileName , REF usLoaded )
    RETURN ( usLoaded == ACE.ADS_REMOTE_SERVER  .OR. usLoaded = ACE.ADS_AIS_SERVER )
    
    /// <summary>Return the percentage of keys added to a currently building index</summary>
FUNCTION AX_PercentIndexed() AS INT 
    LOCAL oRet := NULL AS OBJECT
    IF CoreDb.OrderInfo(DBOI_AXS_PERCENT_INDEXED, NULL, NULL, REF oRet)
        RETURN Convert.ToInt32(oRet)
    ENDIF
    RETURN 0
    
    
    
    /// <summary>Return the AXS Rights Checking status.</summary>
FUNCTION AX_RightsCheck( ) AS LOGIC 
    RETURN AX_RddHelper(_SET_RIGHTSCHECKING, TRUE)
    
/// <summary>Return and set the AXS Rights Checking status.</summary>
FUNCTION AX_RightsCheck( bMode AS LOGIC) AS LOGIC 
    RETURN AX_RddHelper(_SET_RIGHTSCHECKING, bMode, TRUE)
    
FUNCTION AX_SetCollation( strCollation AS STRING ) AS STRING
    LOCAL oldCollation := strCollation AS OBJECT
    CoreDb.RddInfo( _SET_COLLATION_NAME, REF oldCollation )
    RETURN (STRING) oldCollation
    
PROCEDURE AX_SetConnectionHandle( lHandle AS DWORD ) 
    CoreDb.RddInfo( _SET_CONNECTION_HANDLE, lHandle )
    RETURN 
    
FUNCTION AX_SetExactKeyPos( ) AS LOGIC
    RETURN AX_RddHelper(_SET_EXACTKEYPOS, TRUE)
    
FUNCTION AX_SetExactKeyPos( bMode AS LOGIC) AS LOGIC
    RETURN AX_RddHelper(_SET_EXACTKEYPOS, bMode, TRUE)
    
FUNCTION AX_RddHelper(iInfo AS INT, lDefault AS LOGIC) AS LOGIC
    LOCAL bRetVal := NULL AS OBJECT
    LOCAL lRetVal := lDefault AS LOGIC
    IF CoreDb.RddInfo( (DWORD) iInfo , REF bRetVal)
        IF ! bRetVal IS LOGIC
            lRetVal := TRUE
        ELSE
            lRetVal := (LOGIC) bRetVal
        ENDIF
    ENDIF
    RETURN lRetVal
    
    
FUNCTION AX_RddHelper(iInfo AS INT, lNewValue AS LOGIC, lDefault AS LOGIC) AS LOGIC
    LOCAL bRetVal AS LOGIC
    bRetVal := AX_RddHelper(iInfo, lDefault)
    CoreDb.RddInfo( (DWORD) iInfo , lNewValue)
    RETURN bRetVal
    
    
PROCEDURE AX_SetPassword( szEncodeKey AS STRING ) // Set password for record encryption
    IF String.IsNullOrEmpty(szEncodeKey)
        ACE.AdsDisableEncryption( AX_GetAceTableHandle() )
    ELSE
        ACE.AdsEnableEncryption( AX_GetAceTableHandle(), szEncodeKey  )
    ENDIF
    RETURN
    
FUNCTION AX_SetServerType( lUseRemoteServer AS LOGIC, lUseInternetServer AS LOGIC, lUseLocalServer AS LOGIC) AS LOGIC // determine which Advantage server to connect to
    LOCAL usServerTypes AS WORD
    LOCAL ulRetCode AS DWORD
    
    usServerTypes := 0
    IF( lUseRemoteServer )
        usServerTypes :=  _OR( usServerTypes, ACE.ADS_REMOTE_SERVER )
    ENDIF
    IF( lUseInternetServer )
        usServerTypes :=  _OR( usServerTypes, ACE.ADS_AIS_SERVER )
    ENDIF
    IF( lUseLocalServer )
        usServerTypes :=  _OR( usServerTypes, ACE.ADS_LOCAL_SERVER )
    ENDIF
    
    ulRetCode := ACE.AdsSetServerType( usServerTypes )
    
    RETURN  ulRetCode == 0 
    
    
FUNCTION AX_SetSQLTablePasswords( aPasswords AS OBJECT ) AS VOID
    CoreDb.RddInfo( _SET_SQL_TABLE_PASSWORDS, aPasswords )
    RETURN 
    
FUNCTION AX_Transaction( iAction AS INT) AS LOGIC // Transaction call
    LOCAL usInTrans AS WORD
    LOCAL ulRetVal AS DWORD
    //
    // Transaction Processing function.  The parameter can be
    //   AX_BEGIN_TRANSACTION
    //   AX_COMMIT_TRANSACTION
    //   AX_ROLLBACK_TRANSACTION
    //   AX_ISACTIVE_TRANSACTION
    //
    usInTrans := 0
    
    SWITCH iAction
        CASE AX_BEGIN_TRANSACTION
            ulRetVal := ACE.AdsBeginTransaction( 0 )
        CASE AX_COMMIT_TRANSACTION
            ulRetVal := ACE.AdsCommitTransaction( 0 )
        CASE AX_ROLLBACK_TRANSACTION
            ulRetVal := ACE.AdsRollbackTransaction( 0 )
        CASE AX_ISACTIVE_TRANSACTION
            ulRetVal := ACE.AdsInTransaction( 0, REF usInTrans )
            RETURN ( ulRetVal == 0 .AND. usInTrans != 0 )
        OTHERWISE
            ulRetVal := 1
    END SWITCH
    
    RETURN  ulRetVal == 0 
    
    
FUNCTION AX_Transaction( ) AS LOGIC // Transaction call
    LOCAL usInTrans := 0 AS WORD
    LOCAL ulRetVal AS DWORD
    ulRetVal := ACE.AdsInTransaction( 0, REF usInTrans )
    RETURN ( ulRetVal == 0 .AND. usInTrans != 0 )
    
    
    /// <summary>return .T. if the current workarea is using Advantage Server or AIS Server and
    /// .F. IF USING Advantage RDD IN a LOCAL mode</summary>
FUNCTION AX_UsingClientServer( ) AS LOGIC
    LOCAL ulRetCode AS DWORD
    LOCAL ConnectionHandle := 0 AS IntPtr
    LOCAL usServerType := 0 AS WORD
    LOCAL strFileName AS STRING
    LOCAL oFileName := NULL AS OBJECT
    IF CoreDb.Info(DBI_FULLPATH , REF oFileName)
        strFileName := (STRING) oFileName
        ulRetCode := ACE.AdsFindConnection(  strFileName , OUT ConnectionHandle )
        IF (ulRetCode == 0)
            ulRetCode := ACE.AdsGetConnectionType( ConnectionHandle, OUT usServerType )
        ENDIF
        IF (ulRetCode == 0)
            RETURN ( usServerType == ACE.ADS_REMOTE_SERVER ) .OR. ( usServerType == ACE.ADS_AIS_SERVER )
        ENDIF
    ENDIF
    RETURN FALSE
    
    
#region Wrapper functions found in dbfaxs.prg in VO    

FUNCTION DBFAXSAdsCommitTransaction ( hConnect AS IntPtr ) AS DWORD
    RETURN Ace.AdsCommitTransaction(hConnect)

FUNCTION DBFAXSAdsIsServerLoaded ( strServer AS STRING, pbLoaded REF WORD ) AS DWORD 
    RETURN Ace.AdsIsServerLoaded(strServer,OUT pbLoaded)

FUNCTION DBFAXSAdsBeginTransaction ( hConnect AS IntPtr ) AS DWORD 
    RETURN Ace.AdsBeginTransaction(hConnect)

FUNCTION DBFAXSAdsRollbackTransaction ( hConnect AS IntPtr ) AS DWORD
    RETURN Ace.AdsRollbackTransaction(hConnect)

FUNCTION DBFAXSAdsInTransaction ( hConnect AS IntPtr, pbInTrans REF WORD ) AS DWORD
    RETURN Ace.AdsInTransaction(hConnect, OUT pbInTrans)

FUNCTION DBFAXSAdsFileToBinary ( hTbl AS IntPtr, pucFldName AS STRING, usBinaryType AS WORD, strFileName AS STRING ) AS DWORD
    RETURN Ace.AdsFileToBinary(hTbl, pucFldName, usBinaryType, strFileName)

FUNCTION DBFAXSAdsDisableEncryption( hTbl AS IntPtr ) AS DWORD
    RETURN Ace.AdsDisableEncryption(hTbl)

FUNCTION DBFAXSAdsEnableEncryption( hTbl AS IntPtr, strPassword AS STRING ) AS DWORD PASCAL
    RETURN Ace.AdsEnableEncryption(hTbl,strPassword)

FUNCTION DBFAXSAdsSetServerType ( usServerOptions AS WORD ) AS DWORD
    RETURN Ace.AdsSetServerType(usServerOptions)

FUNCTION DBFAXSAdsGetConnectionType ( hConnect AS IntPtr, pusConnectType REF WORD ) AS DWORD
    RETURN Ace.AdsGetConnectionType (hConnect, OUT pusConnectType)

FUNCTION DBFAXSAdsFindConnection ( strServerName AS STRING, phConnect REF IntPtr ) AS DWORD
    RETURN Ace.AdsFindConnection(strServerName,OUT phConnect)

FUNCTION DBFAXSAdsBinaryToFile ( hTbl AS IntPtr, strFldName AS STRING, strFileName AS STRING ) AS DWORD 
    RETURN Ace.AdsBinaryToFile(hTbl, strFldName, strFileName)
#endregion

DEFINE AX_BEGIN_TRANSACTION  := 1 
DEFINE AX_COMMIT_TRANSACTION  := 2 
DEFINE AX_ROLLBACK_TRANSACTION  := 3 
DEFINE AX_ISACTIVE_TRANSACTION  := 4 
DEFINE DBFAXS_ADS_LOCAL_SERVER := 1
DEFINE DBFAXS_ADS_REMOTE_SERVER := 2
DEFINE DBFAXS_ADS_AIS_SERVER := 4




FUNCTION AdsAddCustomKey(hIndex AS IntPtr ) AS DWORD 
    RETURN Ace.AdsAddCustomKey(hIndex)
    

FUNCTION AdsAppendRecord(hTable AS IntPtr ) AS DWORD
    RETURN Ace.AdsAppendRecord(hTable)
    
    

FUNCTION AdsApplicationExit() AS DWORD
    RETURN Ace.AdsApplicationExit()
    

FUNCTION AdsAtBOF(hTable AS IntPtr , pbBof OUT WORD ) AS DWORD
    RETURN Ace.AdsAtBOF(hTable, OUT pbBof)
    

FUNCTION AdsAtEOF(hTable AS IntPtr , pbEof OUT WORD ) AS DWORD 
    RETURN Ace.AdsAtEOF(hTable , OUT pbEof  )
    

FUNCTION AdsBeginTransaction(hConnect AS IntPtr) AS DWORD
    RETURN Ace.AdsBeginTransaction(hConnect)
    

FUNCTION AdsBinaryToFile(hTable AS IntPtr , strFldName AS STRING , strFileName AS STRING ) AS DWORD
    RETURN Ace.AdsBinaryToFile(hTable, strFldName , strFileName )
    

FUNCTION AdsBinaryToFile(hTable AS IntPtr , lFieldOrdinal AS DWORD, strFileName AS STRING ) AS DWORD
    RETURN Ace.AdsBinaryToFile(hTable, lFieldOrdinal, strFileName)
    

FUNCTION AdsCacheOpenCursors(usOpen AS WORD) AS DWORD
    RETURN Ace.AdsCacheOpenCursors(usOpen)
    

FUNCTION AdsCacheOpenTables(usOpen AS WORD) AS DWORD
    RETURN Ace.AdsCacheOpenTables(usOpen)
    

FUNCTION AdsCacheRecords(hTable AS IntPtr , usNumRecords AS WORD ) AS DWORD
    RETURN Ace.AdsCacheRecords(hTable, usNumRecords)
    

FUNCTION AdsCancelUpdate(hTable AS IntPtr ) AS DWORD
    RETURN Ace.AdsCancelUpdate(hTable)
    

FUNCTION AdsClearAllScopes(hTable AS IntPtr ) AS DWORD
    RETURN Ace.AdsClearAllScopes(hTable)
    
    

FUNCTION AdsClearDefault() AS DWORD
    RETURN Ace.AdsClearDefault()
    

FUNCTION AdsClearFilter(hTable AS IntPtr ) AS DWORD
    RETURN Ace.AdsClearFilter(hTable)
    
    

FUNCTION AdsClearRelation(hTableParent AS IntPtr ) AS DWORD 
    RETURN Ace.AdsClearRelation(hTableParent)
    

FUNCTION AdsClearScope(hIndex AS IntPtr , usScopeOption AS WORD) AS DWORD
    RETURN Ace.AdsClearScope(hIndex, usScopeOption)
    

FUNCTION AdsCloneTable(hTable AS IntPtr , phClone OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsCloneTable(hTable, OUT phClone)
    

FUNCTION AdsCloseAllIndexes(hTable AS IntPtr ) AS DWORD
    RETURN Ace.AdsCloseAllIndexes(hTable)
    

FUNCTION AdsCloseAllTables() AS DWORD 
    RETURN Ace.AdsCloseAllTables()
    

FUNCTION AdsCloseIndex(hIndex AS IntPtr ) AS DWORD
    RETURN Ace.AdsCloseIndex(hIndex)
    

FUNCTION AdsCloseTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsCloseTable(hTable)
    

FUNCTION AdsCloseCachedTables(hConnection AS IntPtr) AS DWORD
    RETURN Ace.AdsCloseCachedTables(hConnection)
    

FUNCTION AdsCommitTransaction(hConnect AS IntPtr) AS DWORD
    RETURN Ace.AdsCommitTransaction(hConnect)
    

FUNCTION AdsContinue(hTable AS IntPtr , pbFound OUT WORD) AS DWORD
    RETURN Ace.AdsContinue(hTable, OUT pbFound)
    

FUNCTION AdsConnect60(pucServerPath AS STRING , usServerTypes AS WORD, pucUserName AS STRING , pucPassword AS STRING , ulOptions AS DWORD, phConnect OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsConnect60(pucServerPath, usServerTypes, pucUserName, pucPassword, ulOptions, OUT phConnect)
    

FUNCTION AdsCopyTableStructure(hTable AS IntPtr , strFile AS STRING ) AS DWORD 
    RETURN Ace.AdsCopyTableStructure(hTable, strFile)
    

FUNCTION AdsCreateFTSIndex(hTable AS IntPtr , strFileName AS STRING , strTag AS STRING , strField AS STRING , ulPageSize AS DWORD , ulMinWordLen AS DWORD , ulMaxWordLen AS DWORD , usUseDefaultDelim AS WORD , strDelimiters AS STRING , usUseDefaultNoise AS WORD , strNoiseWords AS STRING , usUseDefaultDrop AS WORD , strDropChars AS STRING , usUseDefaultConditionals AS WORD , strConditionalChars AS STRING , strReserved1 AS STRING , strReserved2 AS STRING , ulOptions AS DWORD) AS DWORD 
    RETURN Ace.AdsCreateFTSIndex(hTable, strFileName, strTag, strField, ulPageSize , ulMinWordLen , ulMaxWordLen , usUseDefaultDelim , strDelimiters , usUseDefaultNoise , strNoiseWords , usUseDefaultDrop , strDropChars , usUseDefaultConditionals , strConditionalChars , strReserved1 , strReserved2 , ulOptions )
    

FUNCTION AdsDecryptRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDecryptRecord(hTable)
    

FUNCTION AdsDecryptTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDecryptTable(hTable)
    

FUNCTION AdsDeleteCustomKey(hIndex AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDeleteCustomKey(hIndex)
    

FUNCTION AdsDeleteIndex(hIndex AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDeleteIndex(hIndex)
    

FUNCTION AdsDeleteRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDeleteRecord(hTable)
    

FUNCTION AdsDisableEncryption(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDisableEncryption(hTable)
    

FUNCTION AdsDisableLocalConnections() AS DWORD 
    RETURN Ace.AdsDisableLocalConnections()
    

FUNCTION AdsDisconnect(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsDisconnect(hConnect)
    

FUNCTION AdsEnableEncryption(hTable AS IntPtr , strPassword AS STRING ) AS DWORD 
    RETURN Ace.AdsEnableEncryption(hTable, strPassword)
    

FUNCTION AdsEncryptRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsEncryptRecord(hTable)
    

FUNCTION AdsEncryptTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsEncryptTable(hTable)
    

FUNCTION AdsEvalLogicalExpr(hTable AS IntPtr , strExpr AS STRING , pbResult OUT WORD ) AS DWORD 
    RETURN Ace.AdsEvalLogicalExpr(hTable, strExpr, OUT pbResult)
    

FUNCTION AdsEvalNumericExpr(hTable AS IntPtr , strExpr AS STRING , pdResult OUT double ) AS DWORD 
    RETURN Ace.AdsEvalNumericExpr(hTable, strExpr, OUT pdResult)
    

FUNCTION AdsEvalStringExpr(hTable AS IntPtr , strExpr AS STRING , [IN] [OUT] strResult AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsEvalStringExpr(hTable, strExpr, strResult, REF wLen)
    
    
FUNCTION AdsEvalTestExpr(hTable AS IntPtr , strExpr AS STRING , pusType OUT WORD ) AS DWORD 
        RETURN Ace.AdsEvalTestExpr(hTable, strExpr, OUT pusType)
        
    
FUNCTION AdsFileToBinary(hTable AS IntPtr , strFldName AS STRING , usBinaryType AS WORD , strFileName AS STRING ) AS DWORD 
        RETURN Ace.AdsFileToBinary(hTable, strFldName, usBinaryType, strFileName)
        
    
FUNCTION AdsFileToBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, usBinaryType AS WORD , strFileName AS STRING ) AS DWORD 
        RETURN Ace.AdsFileToBinary(hTable, lFieldOrdinal, usBinaryType, strFileName)
        
    
FUNCTION AdsFindConnection(strServerName AS STRING , phConnect OUT IntPtr ) AS DWORD 
        RETURN Ace.AdsFindConnection(strServerName, OUT phConnect)
        

FUNCTION AdsGetAllIndexes(hTable AS IntPtr , [IN] [OUT] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetAllIndexes(hTable, ahIndex, REF pusArrayLen)
    
    

FUNCTION AdsGetFTSIndexes(hTable AS IntPtr , [IN] [OUT] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetFTSIndexes(hTable, ahIndex, REF pusArrayLen)
    

FUNCTION AdsGetAllLocks(hTable AS IntPtr , [IN] [OUT] aulLocks AS DWORD[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetAllLocks(hTable, aulLocks, REF pusArrayLen)
    

FUNCTION AdsGetAllTables([IN] [OUT] ahTable AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetAllTables(ahTable, REF pusArrayLen)
    

FUNCTION AdsGetBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, ulOffset AS DWORD , [IN] [OUT] strBuf AS BYTE[] , pulLen REF DWORD ) AS DWORD 
    RETURN Ace.AdsGetBinary(hTable, lFieldOrdinal, ulOffSet, strBuf, REF pulLen)
    
    
FUNCTION AdsGetBinaryLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD 
        RETURN Ace.AdsGetBinaryLength(hTable, lFieldOrdinal, OUT pulLength)
        
    
FUNCTION AdsGetBookmark(hTable AS IntPtr , phBookmark OUT IntPtr ) AS DWORD 
        RETURN Ace.AdsGetBookmark(hTable, OUT phBookmark)
        
    
FUNCTION AdsGetConnectionType(hConnect AS IntPtr, pusConnectType OUT WORD ) AS DWORD 
        RETURN Ace.AdsGetConnectionType(hConnect, OUT pusConnectType)
        
        

FUNCTION AdsGetDate(hTable AS IntPtr , lFieldOrdinal AS DWORD, [IN] [OUT] strBuf AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetDate(hTable, lFieldOrdinal, strBuf, REF wLen)
    
    

FUNCTION AdsGetDateFormat([IN] [OUT] strFormat AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetDateFormat(strFormat, REF wLen)
    

FUNCTION AdsGetDouble(hTable AS IntPtr , lFieldOrdinal AS DWORD, pdValue OUT REAL8 ) AS DWORD 
    RETURN Ace.AdsGetDouble(hTable, lFieldOrdinal, OUT pdValue)
        

FUNCTION AdsGetDouble(hTable AS IntPtr , strFieldName AS STRING, pdValue OUT REAL8 ) AS DWORD 
    RETURN Ace.AdsGetDouble(hTable, strFieldName, OUT pdValue)


FUNCTION AdsGetFieldLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetFieldLength(hTable, lFieldOrdinal, OUT pulLength)
        

FUNCTION AdsGetFieldType(hTable AS IntPtr , lFieldOrdinal AS DWORD, pusType OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetFieldType(hTable, lFieldOrdinal, OUT pusType)
        

FUNCTION AdsGetHandleType(hObj AS IntPtr , pusType OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetHandleType(hObj, OUT pusType)
        

FUNCTION AdsGetIndexHandle(hTable AS IntPtr , strIndexOrder AS STRING , phIndex OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsGetIndexHandle(hTable, strIndexOrder, OUT phIndex)
        

FUNCTION AdsGetLastError(pulErrCode OUT DWORD , [IN] [OUT] strBuf AS CHAR[] , pusBufLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetLastError(OUT pulErrCode, strBuf, REF pusBufLen)
    

FUNCTION AdsGetMemoLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetMemoLength(hTable, lFieldOrdinal, OUT pulLength)
        

FUNCTION AdsGetMemoBlockSize(hTable AS IntPtr , pusBlockSize OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetMemoBlockSize(hTable, OUT pusBlockSize)
        

FUNCTION AdsGetNumLocks(hTable AS IntPtr , pusNum OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetNumLocks(hTable, OUT pusNum)
        

FUNCTION AdsGetRecordCount(hTable AS IntPtr , usFilterOption AS WORD, pulCount OUT DWORD) AS DWORD 
    RETURN Ace.AdsGetRecordCount(hTable, usFilterOption, OUT pulCount)
        

FUNCTION AdsGetRecordNum(hTable AS IntPtr , usFilterOption AS WORD, pulRec OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetRecordNum(hTable, usFilterOption , OUT pulRec)
        

FUNCTION AdsGetRecordCRC(hTable AS IntPtr , pulCRC OUT DWORD , ulOptions AS DWORD) AS DWORD 
    RETURN Ace.AdsGetRecordCRC(hTable, OUT pulCRC, ulOptions)
        

FUNCTION AdsGetScope(hIndex AS IntPtr , usScopeOption AS WORD, [IN] [OUT] strScope AS CHAR[] , pusBufLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetScope(hIndex, usScopeOption, strScope, REF pusBufLen)
    

FUNCTION AdsGetTableOpenOptions(hTable AS IntPtr , pulOptions OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetTableOpenOptions(hTable, OUT pulOptions)
        

FUNCTION AdsGetTableType(hTable as IntPtr , pusType out WORD) AS DWORD 
    RETURN Ace.AdsGetTableType(hTable, OUT pusType)
        

FUNCTION AdsGetLastTableUpdate(hTable AS IntPtr , [IN] [OUT] strDate AS CHAR[] , pusDateLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetLastTableUpdate(hTable , strDate , REF pusDateLen ) 
    

FUNCTION AdsGotoBottom(hObj AS IntPtr ) AS DWORD 
    RETURN Ace.AdsGotoBottom(hObj)
        

FUNCTION AdsGotoTop(hObj AS IntPtr ) AS DWORD
    RETURN Ace.AdsGotoTop(hObj)
        

FUNCTION AdsGotoRecord(hObj AS IntPtr, nRecord AS DWORD ) AS DWORD
    RETURN Ace.AdsGotoRecord(hObj, nRecord)
        
        

FUNCTION AdsInTransaction(hConnect AS IntPtr, pbInTrans OUT WORD ) AS DWORD 
    RETURN Ace.AdsInTransaction(hConnect, OUT pbInTrans)
        
FUNCTION AdsIsFound(hObj AS IntPtr , pbFound OUT WORD) AS DWORD 
    RETURN Ace.AdsIsFound(hObj, OUT pbFound)
        
FUNCTION AdsIsTableLocked(hTable AS IntPtr , pbLocked OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsTableLocked(hTable, OUT pbLocked)
        
FUNCTION AdsIsRecordLocked(hTable AS IntPtr , ulRec AS DWORD , pbLocked OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsRecordLocked(hTable, ulRec, OUT pbLocked)
        
FUNCTION AdsIsRecordVisible(hObj AS IntPtr , pbVisible OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsRecordVisible(hObj, OUT pbVisible)
        
FUNCTION AdsIsServerLoaded(strServer AS STRING , pbLoaded OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsServerLoaded(strServer, OUT pbLoaded)
        
FUNCTION AdsIsRecordDeleted(hTable AS IntPtr , pbDeleted OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsRecordDeleted(hTable, OUT pbDeleted)
        
FUNCTION AdsLockRecord(hTable AS IntPtr , ulRec AS DWORD ) AS DWORD 
    RETURN Ace.AdsLockRecord(hTable, ulRec)
        
FUNCTION AdsLockTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsLockTable(hTable)
        
FUNCTION AdsPackTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsPackTable(hTable)
        
FUNCTION AdsRecallRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsRecallRecord(hTable)
        
FUNCTION AdsRecallAllRecords(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsRecallAllRecords(hTable)
        
FUNCTION AdsRefreshRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsRefreshRecord(hTable)
        
FUNCTION AdsClearProgressCallback() AS DWORD 
    RETURN Ace.AdsClearProgressCallback()
        
FUNCTION AdsClearCallbackFunction() AS DWORD 
    RETURN Ace.AdsClearCallbackFunction()
        
FUNCTION AdsResetConnection(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsResetConnection(hConnect)
        
FUNCTION AdsRollbackTransaction(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsRollbackTransaction(hConnect)
        
FUNCTION AdsSeek(hIndex AS IntPtr , strKey AS STRING , usKeyLen AS WORD, usDataType AS WORD, usSeekType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN Ace.AdsSeek(hIndex, strKey, usKeyLen, usDataType, usSeekType, OUT pbFound)
        
        
FUNCTION AdsSeek(hIndex AS IntPtr , abKey AS BYTE[] , usKeyLen AS WORD, usDataType AS WORD, usSeekType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN Ace.AdsSeek(hIndex, abKey, usKeyLen, usDataType, usSeekType, OUT pbFound)
        
FUNCTION AdsSeekLast(hIndex AS IntPtr , strKey AS STRING , usKeyLen AS WORD, usDataType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN Ace.AdsSeekLast(hIndex, strKey, usKeyLen, usDataType, OUT pbFound)
        
FUNCTION AdsSeekLast(hIndex AS IntPtr , abKey AS BYTE[] , usKeyLen AS WORD, usDataType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN Ace.AdsSeekLast(hIndex, abKey, usKeyLen, usDataType, OUT pbFound)
        
FUNCTION AdsSetDateFormat(strFormat AS STRING ) AS DWORD 
    RETURN Ace.AdsSetDateFormat(strFormat)
        

FUNCTION AdsSetDecimals(usDecimals AS WORD ) AS DWORD 
    RETURN Ace.AdsSetDecimals(usDecimals)
        

FUNCTION AdsShowDeleted(bShowDeleted AS WORD ) AS DWORD 
    RETURN Ace.AdsShowDeleted(bShowDeleted)
        

FUNCTION AdsSetEpoch(usCentury AS WORD ) AS DWORD 
    RETURN Ace.AdsSetEpoch(usCentury)
        

FUNCTION AdsSetExact(bExact AS WORD ) AS DWORD 
    RETURN Ace.AdsSetExact(bExact)
        

FUNCTION AdsSetFilter(hTable AS IntPtr , strFilter AS STRING ) AS DWORD 
    RETURN Ace.AdsSetFilter(hTable, strFilter)
        

FUNCTION AdsSetRelation(hTableParent AS IntPtr , hIndexChild AS IntPtr , strExpr AS STRING ) AS DWORD 
    RETURN Ace.AdsSetRelation(hTableParent, hIndexChild, strExpr)
        

FUNCTION AdsSetServerType(usServerOptions AS WORD ) AS DWORD 
    RETURN Ace.AdsSetServerType(usServerOptions)
        

FUNCTION AdsSetScope(hIndex AS IntPtr , usScopeOption AS WORD, strScope AS STRING , usScopeLen AS WORD , usDataType AS WORD) AS DWORD 
    RETURN Ace.AdsSetScope(hIndex, usScopeOption, strScope, usScopeLen, usDataType)
        

FUNCTION AdsSetScope(hIndex AS IntPtr , usScopeOption AS WORD, abScope AS BYTE[] , usScopeLen AS WORD , usDataType AS WORD) AS DWORD 
    RETURN Ace.AdsSetScope(hIndex, usScopeOption, abScope, usScopeLen, usDataType)
        

FUNCTION AdsSkip(hObj AS IntPtr , lRecs AS INT) AS DWORD 
    RETURN Ace.AdsSkip(hObj, lRecs)
        

FUNCTION AdsThreadExit() AS DWORD 
    RETURN Ace.AdsThreadExit()
        

FUNCTION AdsUnlockRecord(hTable AS IntPtr , ulRec AS DWORD) AS DWORD 
    RETURN Ace.AdsUnlockRecord(hTable, ulRec)
        

FUNCTION AdsUnlockTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsUnlockTable(hTable)
        

FUNCTION AdsWriteAllRecords() AS DWORD 
    RETURN Ace.AdsWriteAllRecords()
        

FUNCTION AdsWriteRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsWriteRecord(hTable)
        

FUNCTION AdsZapTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsZapTable(hTable)
        

FUNCTION AdsSetAOF(hTable AS IntPtr , strFilter AS STRING , usOptions AS WORD ) AS DWORD 
    RETURN Ace.AdsSetAOF(hTable, strFilter, usOptions)
        
        

FUNCTION AdsClearAOF(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsClearAOF(hTable)
        

FUNCTION AdsRefreshAOF(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsRefreshAOF(hTable)
        

FUNCTION AdsInitRawKey(hIndex AS IntPtr ) AS DWORD 
    RETURN Ace.AdsInitRawKey(hIndex)
        

FUNCTION AdsExecuteSQL(hStatement AS IntPtr , phCursor OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsExecuteSQL(hStatement, OUT phCursor)
        
        

FUNCTION AdsCloseSQLStatement(hStatement AS IntPtr ) AS DWORD 
    RETURN Ace.AdsCloseSQLStatement(hStatement)
        
        

FUNCTION AdsStmtDisableEncryption(hStatement AS IntPtr ) AS DWORD 
    RETURN Ace.AdsStmtDisableEncryption(hStatement)
        
        

FUNCTION AdsStmtClearTablePasswords(hStatement AS IntPtr ) AS DWORD 
    RETURN Ace.AdsStmtClearTablePasswords(hStatement)
        

FUNCTION AdsClearSQLParams(hStatement AS IntPtr ) AS DWORD 
    RETURN Ace.AdsClearSQLParams(hStatement)
        

FUNCTION AdsClearSQLAbortFunc() AS DWORD 
    RETURN Ace.AdsClearSQLAbortFunc()
        

FUNCTION AdsFlushFileBuffers(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsFlushFileBuffers(hTable)
        

FUNCTION AdsDisableUniqueEnforcement(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsDisableUniqueEnforcement(hConnect)
        

FUNCTION AdsEnableUniqueEnforcement(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsEnableUniqueEnforcement(hConnect)
        

FUNCTION AdsDisableRI(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsDisableRI(hConnect)
        

FUNCTION AdsEnableRI(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsEnableRI(hConnect)
        

FUNCTION AdsDisableAutoIncEnforcement(hConnection AS IntPtr) AS DWORD 
    RETURN Ace.AdsDisableAutoIncEnforcement(hConnection)
        

FUNCTION AdsEnableAutoIncEnforcement(hConnection AS IntPtr) AS DWORD 
    RETURN Ace.AdsEnableAutoIncEnforcement(hConnection)
        

FUNCTION AdsGetIndexHandleByOrder(hTable AS IntPtr , usOrderNum AS WORD , phIndex OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsGetIndexHandleByOrder(hTable, usOrderNum, OUT phIndex)
        

FUNCTION AdsGetNumIndexes(hTable AS IntPtr , pusNum OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetNumIndexes(hTable, OUT pusNum)
        

FUNCTION AdsGetRecordLength(hTable AS IntPtr , pulLength OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetRecordLength(hTable, OUT pulLength)
        
        

FUNCTION AdsGetTableFilename(hTable AS IntPtr , usOption AS WORD , strName AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetTableFilename(hTable, usOption, strName , REF wLen)
        

FUNCTION AdsOpenTable90(hConnect AS IntPtr, strName AS STRING , strAlias AS STRING , usTableType AS WORD, usCharType AS WORD , usLockType AS WORD , usCheckRights AS WORD , ulOptions AS DWORD, strCollation AS STRING , phTable OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsOpenTable90(hConnect, strName, strAlias, usTableType, usCharType, usLockType, usCheckRights, ulOptions, strCollation, OUT phTable)
        

FUNCTION AdsCreateTable90(hConnect AS IntPtr, strName AS STRING , strDBObjName AS STRING , usTableType AS WORD, usCharType AS WORD , usLockType AS WORD , usCheckRights AS WORD , usMemoSize AS WORD , strFields AS STRING , ulOptions AS DWORD, strCollation AS STRING , phTable OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsCreateTable90(hConnect, strName, strDBObjName, usTableType, usCharType, usLockType, usCheckRights, usMemoSize, strFields, ulOptions, strCollation, OUT phTable)
        

FUNCTION AdsSetDefault(strDefault AS STRING ) AS DWORD 
    RETURN Ace.AdsSetDefault(strDefault)
        

FUNCTION AdsSetSearchPath(strPath AS STRING ) AS DWORD 
    RETURN Ace.AdsSetSearchPath(strPath)
        

FUNCTION AdsGetFieldDecimals(hTable AS IntPtr , lFieldOrdinal AS DWORD, pusDecimals OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetFieldDecimals(hTable, lFieldOrdinal, OUT pusDecimals)
        

FUNCTION AdsGetFieldName(hTable AS IntPtr , usFld AS WORD , [IN] [OUT] strName AS CHAR[] , pusBufLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetFieldName(hTable, usFld, strName, REF pusBufLen)
    

FUNCTION AdsGetNumFields(hTable AS IntPtr , pusCount OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetNumFields(hTable, OUT pusCount)
        

FUNCTION AdsGetField(hTable AS IntPtr , lFieldOrdinal AS DWORD, [IN] [OUT] abBuf AS BYTE[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD 
    RETURN Ace.AdsGetField(hTable, lFieldOrdinal, abBuf, REF pulLen, usOption)
    

FUNCTION AdsGetField(hTable AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD 
    RETURN Ace.AdsGetField(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)
        

FUNCTION AdsIsEmpty(hTable AS IntPtr , lFieldOrdinal AS DWORD, pbEmpty OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsEmpty(hTable, lFieldOrdinal, OUT pbEmpty)
        

FUNCTION AdsIsEmpty(hTable AS IntPtr , strFldName AS STRING , pbEmpty OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsEmpty(hTable, strFldName, OUT pbEmpty)
        

FUNCTION AdsGetString(hTable AS IntPtr , lFieldOrdinal AS DWORD, [IN] [OUT] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD) AS DWORD 
    RETURN Ace.AdsGetString(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)


FUNCTION AdsGetString(hTable AS IntPtr , strFieldName AS STRING, [IN] [OUT] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD 
    RETURN Ace.AdsGetString(hTable, strFieldName, strBuf, REF pulLen, usOption)


FUNCTION AdsGetStringW(hTable AS IntPtr , lFieldOrdinal AS DWORD, [IN] [OUT] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD) AS DWORD 
    RETURN Ace.AdsGetString(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)


FUNCTION AdsGetStringW(hTable AS IntPtr , strFieldName AS STRING, [IN] [OUT] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD 
    RETURN Ace.AdsGetStringW(hTable, strFieldName,  strBuf, REF pulLen, usOption)


FUNCTION AdsGetJulian(hTable AS IntPtr , lFieldOrdinal AS DWORD, plDate OUT INT ) AS DWORD 
    RETURN Ace.AdsGetJulian(hTable, lFieldOrdinal, OUT plDate)
        

FUNCTION AdsGetJulian(hTable AS IntPtr , strFldName AS STRING , plDate OUT INT ) AS DWORD 
    RETURN Ace.AdsGetJulian(hTable, strFldName, OUT plDate)
        

FUNCTION AdsGetLogical(hTable AS IntPtr , lFieldOrdinal AS DWORD, pbValue OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetLogical(hTable, lFieldOrdinal, OUT pbValue)
        

FUNCTION AdsGetLogical(hTable AS IntPtr , strFldName AS STRING , pbValue OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetLogical(hTable, strFldName, OUT pbValue)



FUNCTION AdsSetEmpty(hObj AS IntPtr , lFieldOrdinal AS DWORD) AS DWORD 
    RETURN Ace.AdsSetEmpty(hObj, lFieldOrdinal)
        

FUNCTION AdsSetEmpty(hObj AS IntPtr , strFldName AS STRING ) AS DWORD 
    RETURN Ace.AdsSetEmpty(hObj, strFldName)
        
        

FUNCTION AdsSetField(hObj AS IntPtr , lFieldOrdinal AS DWORD, abBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetField(hObj, lFieldOrdinal, abBuf, ulLen)


FUNCTION AdsSetField(hObj AS IntPtr , strFldName AS STRING, abBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetField(hObj, strFldName, abBuf, ulLen)
   

FUNCTION AdsSetField(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetField(hObj, lFieldOrdinal, strBuf, ulLen)


FUNCTION AdsSetField(hObj AS IntPtr , strFldName AS STRING, strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetField(hObj, strFldName, strBuf, ulLen)


FUNCTION AdsSetString(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetString(hObj, lFieldOrdinal, strBuf, ulLen)


FUNCTION AdsSetString(hObj AS IntPtr , strFldName AS STRING , strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetString(hObj, strFldName, strBuf, ulLen)
        

FUNCTION AdsSetStringW(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetString(hObj, lFieldOrdinal, strBuf, ulLen)
        

FUNCTION AdsSetStringW(hObj AS IntPtr , strFldName AS STRING , strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetString(hObj, strFldName, strBuf, ulLen)
        

FUNCTION AdsSetBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, usBinaryType AS WORD , ulTotalLength AS DWORD , ulOffset AS DWORD , strBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetBinary(hTable, lFieldOrdinal, usBinaryType, ulTotalLength, ulOffSet, strBuf, ulLen)


FUNCTION AdsSetBinary(hTable AS IntPtr , strFldName AS STRING , usBinaryType AS WORD , ulTotalLength AS DWORD , ulOffset AS DWORD , strBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetBinary(hTable, strFldName, usBinaryType, ulTotalLength, ulOffset, strBuf, ulLen)
        

FUNCTION AdsSetDouble(hObj AS IntPtr , lFieldOrdinal AS DWORD, dValue AS REAL8) AS DWORD 
    RETURN Ace.AdsSetDouble(hObj, lFieldOrdinal, dValue)
        

FUNCTION AdsSetDouble(hObj AS IntPtr , strFldName AS STRING, dValue AS REAL8) AS DWORD 
    RETURN Ace.AdsSetDouble(hObj, strFldName, dValue)


FUNCTION AdsSetJulian(hObj AS IntPtr , lFieldOrdinal AS DWORD, lDate AS INT) AS DWORD 
    RETURN Ace.AdsSetJulian(hObj, lFieldOrdinal, lDate)


FUNCTION AdsSetJulian(hObj AS IntPtr , strFldName AS STRING , lDate AS INT ) AS DWORD 
    RETURN Ace.AdsSetJulian(hObj, strFldName, lDate)


FUNCTION AdsSetLogical(hObj AS IntPtr , lFieldOrdinal AS DWORD, bValue AS WORD ) AS DWORD 
    RETURN Ace.AdsSetLogical(hObj, lFieldOrdinal, bValue)
        

FUNCTION AdsSetLogical(hObj AS IntPtr , strFldName AS STRING , bValue AS WORD ) AS DWORD 
    RETURN Ace.AdsSetLogical(hObj, strFldName, bValue)


FUNCTION AdsSetMoney(hObj AS IntPtr , lFieldOrdinal AS DWORD, qValue AS INT64 ) AS DWORD 
    RETURN Ace.AdsSetMoney(hObj, lFieldOrdinal, qValue)
        

FUNCTION AdsSetMoney(hObj AS IntPtr , strFldName AS STRING , qValue AS INT64 ) AS DWORD 
    RETURN Ace.AdsSetMoney(hObj, strFldName, qValue)


FUNCTION AdsSetMilliseconds(hObj AS IntPtr , lFieldOrdinal AS DWORD, lTime AS INT ) AS DWORD 
    RETURN Ace.AdsSetMilliseconds(hObj, lFieldOrdinal, lTime)
        

FUNCTION AdsSetMilliseconds(hObj AS IntPtr , strFldName AS STRING , lTime AS INT ) AS DWORD 
    RETURN Ace.AdsSetMilliseconds(hObj, strFldName, lTime)


FUNCTION AdsSetNull(hObj AS IntPtr , lFieldOrdinal AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetNull(hObj, lFieldOrdinal)


FUNCTION AdsSetNull(hObj AS IntPtr , strFldName AS STRING  ) AS DWORD 
    RETURN Ace.AdsSetNull(hObj, strFldName)


FUNCTION AdsSetShort(hObj AS IntPtr , lFieldOrdinal AS DWORD, sValue AS SHORT ) AS DWORD 
    RETURN Ace.AdsSetShort(hObj, lFieldOrdinal, sValue)
        

FUNCTION AdsSetShort(hObj AS IntPtr , strFldName AS STRING , sValue AS SHORT ) AS DWORD 
    RETURN Ace.AdsSetShort(hObj, strFldName, sValue)


FUNCTION AdsSetTime(hObj AS IntPtr , lFieldOrdinal AS DWORD, strValue AS STRING , wLen AS WORD  ) AS DWORD 
    RETURN Ace.AdsSetTime(hObj, lFieldOrdinal, strValue, wLen)
        

FUNCTION AdsSetTime(hObj AS IntPtr , strFldName AS STRING , strValue AS STRING , wLen AS WORD ) AS DWORD 
    RETURN Ace.AdsSetTime(hObj, strFldName, strValue, wLen)


FUNCTION AdsSetTimeStamp(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD   ) AS DWORD 
    RETURN Ace.AdsSetTimeStamp(hObj, lFieldOrdinal, strBuf, ulLen)
        

FUNCTION AdsSetTimeStamp(hObj AS IntPtr , strFldName AS STRING , strBuf AS STRING , ulLen AS DWORD  ) AS DWORD 
    RETURN Ace.AdsSetTimeStamp(hObj, strFldName, strBuf, ulLen)


FUNCTION AdsReindex(hObject AS IntPtr) AS DWORD 
    RETURN Ace.AdsReindex(hObject)
        

FUNCTION AdsGetIndexFilename(hIndex AS IntPtr , usOption AS WORD , [IN] [OUT] strName AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetIndexFilename(hIndex, usOption, strName , REF wLen)
    

FUNCTION AdsOpenIndex(hTable AS IntPtr , strName AS STRING , [IN] [OUT] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN Ace.AdsOpenIndex(hTable,  strName , ahIndex, REF pusArrayLen )
    

FUNCTION AdsCreateIndex(hObj AS IntPtr , strFileName AS STRING , strTag AS STRING , strExpr AS STRING , strCondition AS STRING , strWhile AS STRING , ulOptions AS DWORD, phIndex OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsCreateIndex(hObj,  strFileName , strTag, strExpr , strCondition , strWhile , ulOptions , OUT phIndex ) 


FUNCTION AdsCreateIndex90(hObj AS IntPtr , strFileName AS STRING , strTag AS STRING , strExpr AS STRING , strCondition AS STRING , strWhile AS STRING , ulOptions AS DWORD, ulPageSize AS DWORD , strCollation AS STRING , phIndex OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsCreateIndex90(hObj,  strFileName , strTag, strExpr , strCondition , strWhile , ulOptions , ulPageSize, strCollation, OUT phIndex ) 
        

FUNCTION AdsRegisterCallbackFunction(pfn AS CallbackFn , ulCallBackID AS DWORD ) AS DWORD 
    RETURN Ace.AdsRegisterCallbackFunction(pfn,  ulCallBackID  )
        

FUNCTION AdsExtractKey(hIndex AS IntPtr , [IN] [OUT] strKey AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsExtractKey(hIndex,  strKey , REF wLen )
    

FUNCTION AdsGetAOFOptLevel(hTable AS IntPtr , pusOptLevel OUT WORD , [IN] [OUT] strNonOpt AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetAOFOptLevel(hTable,  OUT pusOptLevel , strNonOpt , REF wLen)
    

FUNCTION AdsGetIndexCondition(hIndex AS IntPtr , [IN] [OUT] strExpr AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetIndexCondition(hIndex,  strExpr , REF wLen)
    

FUNCTION AdsGetIndexExpr(hIndex AS IntPtr , [IN] [OUT] strExpr AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetIndexExpr(hIndex,  strExpr , REF wLen)
    

FUNCTION AdsGetIndexName(hIndex AS IntPtr , [IN] [OUT] strName AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetIndexName(hIndex,  strName , REF wLen)


FUNCTION AdsGetIndexOrderByHandle(hIndex AS IntPtr , pusIndexOrder OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetIndexOrderByHandle(hIndex,  OUT pusIndexOrder)
    

FUNCTION AdsSkipUnique(hIndex AS IntPtr , lRecs AS INT) AS DWORD 
    RETURN Ace.AdsSkipUnique(hIndex,  lRecs)
    

FUNCTION AdsGetKeyCount(hIndex AS IntPtr , usFilterOption AS WORD, pulCount OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetKeyCount(hIndex, usFilterOption, OUT pulCount)
    

FUNCTION AdsGetKeyLength(hIndex AS IntPtr , pusKeyLength OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetKeyLength(hIndex, OUT pusKeyLength)
    

FUNCTION AdsGetKeyNum(hIndex AS IntPtr , usFilterOption AS WORD, pulKey OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetKeyNum(hIndex, usFilterOption, OUT pulKey)
    

FUNCTION AdsGetKeyType(hIndex AS IntPtr , usKeyType OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetKeyType(hIndex,  OUT usKeyType)
    

FUNCTION AdsGetRelKeyPos(hIndex AS IntPtr , pdPos OUT double ) AS DWORD 
    RETURN Ace.AdsGetRelKeyPos(hIndex,  OUT pdPos)
    

FUNCTION AdsIsIndexCustom(hIndex AS IntPtr , pbCustom OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsIndexCustom(hIndex,  OUT pbCustom)
    

FUNCTION AdsIsIndexDescending(hIndex AS IntPtr , pbDescending OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsIndexDescending(hIndex,  OUT pbDescending)
    

FUNCTION AdsIsIndexUnique(hIndex AS IntPtr , pbUnique OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsIndexUnique(hIndex,  OUT pbUnique)
    

FUNCTION AdsCreateSQLStatement(hConnect as IntPtr, phStatement out IntPtr ) as DWORD 
    RETURN Ace.AdsCreateSQLStatement(hConnect,  OUT phStatement)
    

FUNCTION AdsStmtSetTableType(hStatement as IntPtr , usTableType as WORD) as DWORD 
    RETURN Ace.AdsStmtSetTableType(hStatement,  usTableType)
    

FUNCTION AdsStmtSetTableCharType(hStatement as IntPtr , usCharType as WORD ) as DWORD 
    RETURN Ace.AdsStmtSetTableCharType(hStatement,  usCharType)
    

FUNCTION AdsStmtSetTableCollation(hStatement as IntPtr , strCollation as string ) as DWORD 
    RETURN Ace.AdsStmtSetTableCollation(hStatement,  strCollation)
    

FUNCTION AdsStmtSetTableLockType(hStatement as IntPtr , usLockType as WORD ) as DWORD 
    RETURN Ace.AdsStmtSetTableLockType(hStatement,  usLockType)
    

FUNCTION AdsStmtSetTablePassword(hStatement as IntPtr , strTableName as string , strPassword as string ) as DWORD 
    RETURN Ace.AdsStmtSetTablePassword(hStatement,  strTableName, strPassword)
    

FUNCTION AdsStmtSetTableReadOnly(hStatement as IntPtr , usReadOnly as WORD ) as DWORD 
    RETURN Ace.AdsStmtSetTableReadOnly(hStatement,  usReadOnly)
    

FUNCTION AdsStmtSetTableRights(hStatement as IntPtr , usCheckRights as WORD ) as DWORD 
    RETURN Ace.AdsStmtSetTableRights(hStatement,  usCheckRights)
    

FUNCTION AdsExecuteSQLDirect(hStatement as IntPtr , strSQL as string , phCursor out IntPtr ) as DWORD 
    RETURN Ace.AdsExecuteSQLDirect(hStatement,  strSQL, OUT phCursor)
