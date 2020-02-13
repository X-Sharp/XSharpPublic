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
   return AX_GetAceIndexHandle(NULL_OBJECT, NULL_OBJECT)
    
    
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
    RETURN ACE.AdsCommitTransaction(hConnect)

FUNCTION DBFAXSAdsIsServerLoaded ( strServer AS STRING, pbLoaded REF WORD ) AS DWORD 
    RETURN ACE.AdsIsServerLoaded(strServer,OUT pbLoaded)

FUNCTION DBFAXSAdsBeginTransaction ( hConnect AS IntPtr ) AS DWORD 
    RETURN ACE.AdsBeginTransaction(hConnect)

FUNCTION DBFAXSAdsRollbackTransaction ( hConnect AS IntPtr ) AS DWORD
    RETURN ACE.AdsRollbackTransaction(hConnect)

FUNCTION DBFAXSAdsInTransaction ( hConnect AS IntPtr, pbInTrans REF WORD ) AS DWORD
    RETURN ACE.AdsInTransaction(hConnect, OUT pbInTrans)

FUNCTION DBFAXSAdsFileToBinary ( hTbl AS IntPtr, pucFldName AS STRING, usBinaryType AS WORD, strFileName AS STRING ) AS DWORD
    RETURN ACE.AdsFileToBinary(hTbl, pucFldName, usBinaryType, strFileName)

FUNCTION DBFAXSAdsDisableEncryption( hTbl AS IntPtr ) AS DWORD
    RETURN ACE.AdsDisableEncryption(hTbl)

FUNCTION DBFAXSAdsEnableEncryption( hTbl AS IntPtr, strPassword AS STRING ) AS DWORD PASCAL
    RETURN ACE.AdsEnableEncryption(hTbl,strPassword)

FUNCTION DBFAXSAdsSetServerType ( usServerOptions AS WORD ) AS DWORD
    RETURN ACE.AdsSetServerType(usServerOptions)

FUNCTION DBFAXSAdsGetConnectionType ( hConnect AS IntPtr, pusConnectType REF WORD ) AS DWORD
    RETURN ACE.AdsGetConnectionType (hConnect, OUT pusConnectType)

FUNCTION DBFAXSAdsFindConnection ( strServerName AS STRING, phConnect REF IntPtr ) AS DWORD
    RETURN ACE.AdsFindConnection(strServerName,OUT phConnect)

FUNCTION DBFAXSAdsBinaryToFile ( hTbl AS IntPtr, strFldName AS STRING, strFileName AS STRING ) AS DWORD 
    RETURN ACE.AdsBinaryToFile(hTbl, strFldName, strFileName)
#endregion

DEFINE AX_BEGIN_TRANSACTION  := 1 
DEFINE AX_COMMIT_TRANSACTION  := 2 
DEFINE AX_ROLLBACK_TRANSACTION  := 3 
DEFINE AX_ISACTIVE_TRANSACTION  := 4 
DEFINE DBFAXS_ADS_LOCAL_SERVER := 1
DEFINE DBFAXS_ADS_REMOTE_SERVER := 2
DEFINE DBFAXS_ADS_AIS_SERVER := 4




FUNCTION AdsAddCustomKey(hIndex AS IntPtr ) AS DWORD 
    RETURN ACE.AdsAddCustomKey(hIndex)
    

FUNCTION AdsAppendRecord(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsAppendRecord(hTable)
    
    

FUNCTION AdsApplicationExit() AS DWORD
    RETURN ACE.AdsApplicationExit()
    

FUNCTION AdsAtBOF(hTable AS IntPtr , pbBof OUT WORD ) AS DWORD
    RETURN ACE.AdsAtBOF(hTable, OUT pbBof)
    

FUNCTION AdsAtEOF(hTable AS IntPtr , pbEof OUT WORD ) AS DWORD 
    RETURN ACE.AdsAtEOF(hTable , OUT pbEof  )
    

FUNCTION AdsBeginTransaction(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsBeginTransaction(hConnect)
    

FUNCTION AdsBinaryToFile(hTable AS IntPtr , strFldName AS STRING , strFileName AS STRING ) AS DWORD
    RETURN ACE.AdsBinaryToFile(hTable, strFldName , strFileName )
    

FUNCTION AdsBinaryToFile(hTable AS IntPtr , lFieldOrdinal AS DWORD, strFileName AS STRING ) AS DWORD
    RETURN ACE.AdsBinaryToFile(hTable, lFieldOrdinal, strFileName)
    

FUNCTION AdsCacheOpenCursors(usOpen AS WORD) AS DWORD
    RETURN ACE.AdsCacheOpenCursors(usOpen)
    

FUNCTION AdsCacheOpenTables(usOpen AS WORD) AS DWORD
    RETURN ACE.AdsCacheOpenTables(usOpen)
    

FUNCTION AdsCacheRecords(hTable AS IntPtr , usNumRecords AS WORD ) AS DWORD
    RETURN ACE.AdsCacheRecords(hTable, usNumRecords)
    

FUNCTION AdsCancelUpdate(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsCancelUpdate(hTable)
    

FUNCTION AdsClearAllScopes(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsClearAllScopes(hTable)
    
    

FUNCTION AdsClearDefault() AS DWORD
    RETURN ACE.AdsClearDefault()
    

FUNCTION AdsClearFilter(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsClearFilter(hTable)
    
    

FUNCTION AdsClearRelation(hTableParent AS IntPtr ) AS DWORD 
    RETURN ACE.AdsClearRelation(hTableParent)
    

FUNCTION AdsClearScope(hIndex AS IntPtr , usScopeOption AS WORD) AS DWORD
    RETURN ACE.AdsClearScope(hIndex, usScopeOption)
    

FUNCTION AdsCloneTable(hTable AS IntPtr , phClone OUT IntPtr ) AS DWORD 
    RETURN ACE.AdsCloneTable(hTable, OUT phClone)
    

FUNCTION AdsCloseAllIndexes(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsCloseAllIndexes(hTable)
    

FUNCTION AdsCloseAllTables() AS DWORD 
    RETURN ACE.AdsCloseAllTables()
    

FUNCTION AdsCloseIndex(hIndex AS IntPtr ) AS DWORD
    RETURN ACE.AdsCloseIndex(hIndex)
    

FUNCTION AdsCloseTable(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsCloseTable(hTable)
    

FUNCTION AdsCloseCachedTables(hConnection AS IntPtr) AS DWORD
    RETURN ACE.AdsCloseCachedTables(hConnection)
    

FUNCTION AdsCommitTransaction(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsCommitTransaction(hConnect)
    

FUNCTION AdsContinue(hTable AS IntPtr , pbFound OUT WORD) AS DWORD
    RETURN ACE.AdsContinue(hTable, OUT pbFound)
    

FUNCTION AdsConnect60(pucServerPath AS STRING , usServerTypes AS WORD, pucUserName AS STRING , pucPassword AS STRING , ulOptions AS DWORD, phConnect OUT IntPtr ) AS DWORD 
    RETURN ACE.AdsConnect60(pucServerPath, usServerTypes, pucUserName, pucPassword, ulOptions, OUT phConnect)
    

FUNCTION AdsCopyTableStructure(hTable AS IntPtr , strFile AS STRING ) AS DWORD 
    RETURN ACE.AdsCopyTableStructure(hTable, strFile)
    

FUNCTION AdsCreateFTSIndex(hTable AS IntPtr , strFileName AS STRING , strTag AS STRING , strField AS STRING , ulPageSize AS DWORD , ulMinWordLen AS DWORD , ulMaxWordLen AS DWORD , usUseDefaultDelim AS WORD , strDelimiters AS STRING , usUseDefaultNoise AS WORD , strNoiseWords AS STRING , usUseDefaultDrop AS WORD , strDropChars AS STRING , usUseDefaultConditionals AS WORD , strConditionalChars AS STRING , strReserved1 AS STRING , strReserved2 AS STRING , ulOptions AS DWORD) AS DWORD 
    RETURN ACE.AdsCreateFTSIndex(hTable, strFileName, strTag, strField, ulPageSize , ulMinWordLen , ulMaxWordLen , usUseDefaultDelim , strDelimiters , usUseDefaultNoise , strNoiseWords , usUseDefaultDrop , strDropChars , usUseDefaultConditionals , strConditionalChars , strReserved1 , strReserved2 , ulOptions )
    

FUNCTION AdsDecryptRecord(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsDecryptRecord(hTable)
    

FUNCTION AdsDecryptTable(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsDecryptTable(hTable)
    

FUNCTION AdsDeleteCustomKey(hIndex AS IntPtr ) AS DWORD 
    RETURN ACE.AdsDeleteCustomKey(hIndex)
    

FUNCTION AdsDeleteIndex(hIndex AS IntPtr ) AS DWORD 
    RETURN ACE.AdsDeleteIndex(hIndex)
    

FUNCTION AdsDeleteRecord(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsDeleteRecord(hTable)
    

FUNCTION AdsDisableEncryption(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsDisableEncryption(hTable)
    

FUNCTION AdsDisableLocalConnections() AS DWORD 
    RETURN ACE.AdsDisableLocalConnections()
    

FUNCTION AdsDisconnect(hConnect AS IntPtr) AS DWORD 
    RETURN ACE.AdsDisconnect(hConnect)
    

FUNCTION AdsEnableEncryption(hTable AS IntPtr , strPassword AS STRING ) AS DWORD 
    RETURN ACE.AdsEnableEncryption(hTable, strPassword)
    

FUNCTION AdsEncryptRecord(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsEncryptRecord(hTable)
    

FUNCTION AdsEncryptTable(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsEncryptTable(hTable)
    

FUNCTION AdsEvalLogicalExpr(hTable AS IntPtr , strExpr AS STRING , pbResult OUT WORD ) AS DWORD 
    RETURN ACE.AdsEvalLogicalExpr(hTable, strExpr, OUT pbResult)
    

FUNCTION AdsEvalNumericExpr(hTable AS IntPtr , strExpr AS STRING , pdResult OUT System.Double ) AS DWORD 
    RETURN ACE.AdsEvalNumericExpr(hTable, strExpr, OUT pdResult)
    

FUNCTION AdsEvalStringExpr(hTable AS IntPtr , strExpr AS STRING , [InAttribute] [OutAttribute] strResult AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN ACE.AdsEvalStringExpr(hTable, strExpr, strResult, REF wLen)
    
    
FUNCTION AdsEvalTestExpr(hTable AS IntPtr , strExpr AS STRING , pusType OUT WORD ) AS DWORD 
        RETURN ACE.AdsEvalTestExpr(hTable, strExpr, OUT pusType)
        
    
FUNCTION AdsFileToBinary(hTable AS IntPtr , strFldName AS STRING , usBinaryType AS WORD , strFileName AS STRING ) AS DWORD 
        RETURN ACE.AdsFileToBinary(hTable, strFldName, usBinaryType, strFileName)
        
    
FUNCTION AdsFileToBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, usBinaryType AS WORD , strFileName AS STRING ) AS DWORD 
        RETURN ACE.AdsFileToBinary(hTable, lFieldOrdinal, usBinaryType, strFileName)
        
    
FUNCTION AdsFindConnection(strServerName AS STRING , phConnect OUT IntPtr ) AS DWORD 
        RETURN ACE.AdsFindConnection(strServerName, OUT phConnect)
        

FUNCTION AdsGetAllIndexes(hTable AS IntPtr , [InAttribute] [OutAttribute] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetAllIndexes(hTable, ahIndex, REF pusArrayLen)
    
    

FUNCTION AdsGetFTSIndexes(hTable AS IntPtr , [InAttribute] [OutAttribute] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetFTSIndexes(hTable, ahIndex, REF pusArrayLen)
    

FUNCTION AdsGetAllLocks(hTable AS IntPtr , [InAttribute] [OutAttribute] aulLocks AS DWORD[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetAllLocks(hTable, aulLocks, REF pusArrayLen)
    

FUNCTION AdsGetAllTables([InAttribute] [OutAttribute] ahTable AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetAllTables(ahTable, REF pusArrayLen)
    

FUNCTION AdsGetBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, ulOffset AS DWORD , [InAttribute] [OutAttribute] strBuf AS BYTE[] , pulLen REF DWORD ) AS DWORD 
    RETURN ACE.AdsGetBinary(hTable, lFieldOrdinal, ulOffset, strBuf, REF pulLen)
    
    
FUNCTION AdsGetBinaryLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD 
        RETURN ACE.AdsGetBinaryLength(hTable, lFieldOrdinal, OUT pulLength)
        
    
FUNCTION AdsGetBookmark(hTable AS IntPtr , phBookmark OUT IntPtr ) AS DWORD 
        RETURN ACE.AdsGetBookmark(hTable, OUT phBookmark)
        
    
FUNCTION AdsGetConnectionType(hConnect AS IntPtr, pusConnectType OUT WORD ) AS DWORD 
        RETURN ACE.AdsGetConnectionType(hConnect, OUT pusConnectType)
        
        

FUNCTION AdsGetDate(hTable AS IntPtr , lFieldOrdinal AS DWORD, [InAttribute] [OutAttribute] strBuf AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetDate(hTable, lFieldOrdinal, strBuf, REF wLen)
    
    

FUNCTION AdsGetDateFormat([InAttribute] [OutAttribute] strFormat AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetDateFormat(strFormat, REF wLen)
    

FUNCTION AdsGetDouble(hTable AS IntPtr , lFieldOrdinal AS DWORD, pdValue OUT REAL8 ) AS DWORD 
    RETURN ACE.AdsGetDouble(hTable, lFieldOrdinal, OUT pdValue)
        

FUNCTION AdsGetDouble(hTable AS IntPtr , strFieldName AS STRING, pdValue OUT REAL8 ) AS DWORD 
    RETURN ACE.AdsGetDouble(hTable, strFieldName, OUT pdValue)


FUNCTION AdsGetFieldLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD 
    RETURN ACE.AdsGetFieldLength(hTable, lFieldOrdinal, OUT pulLength)
        

FUNCTION AdsGetFieldType(hTable AS IntPtr , lFieldOrdinal AS DWORD, pusType OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetFieldType(hTable, lFieldOrdinal, OUT pusType)
        

FUNCTION AdsGetHandleType(hObj AS IntPtr , pusType OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetHandleType(hObj, OUT pusType)
        

FUNCTION AdsGetIndexHandle(hTable AS IntPtr , strIndexOrder AS STRING , phIndex OUT IntPtr ) AS DWORD 
    RETURN ACE.AdsGetIndexHandle(hTable, strIndexOrder, OUT phIndex)
        

FUNCTION AdsGetLastError(pulErrCode OUT DWORD , [InAttribute] [OutAttribute] strBuf AS CHAR[] , pusBufLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetLastError(OUT pulErrCode, strBuf, REF pusBufLen)
    

FUNCTION AdsGetMemoLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD 
    RETURN ACE.AdsGetMemoLength(hTable, lFieldOrdinal, OUT pulLength)
        

FUNCTION AdsGetMemoBlockSize(hTable AS IntPtr , pusBlockSize OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetMemoBlockSize(hTable, OUT pusBlockSize)
        

FUNCTION AdsGetNumLocks(hTable AS IntPtr , pusNum OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetNumLocks(hTable, OUT pusNum)
        

FUNCTION AdsGetRecordCount(hTable AS IntPtr , usFilterOption AS WORD, pulCount OUT DWORD) AS DWORD 
    RETURN ACE.AdsGetRecordCount(hTable, usFilterOption, OUT pulCount)
        

FUNCTION AdsGetRecordNum(hTable AS IntPtr , usFilterOption AS WORD, pulRec OUT DWORD ) AS DWORD 
    RETURN ACE.AdsGetRecordNum(hTable, usFilterOption , OUT pulRec)
        

FUNCTION AdsGetRecordCRC(hTable AS IntPtr , pulCRC OUT DWORD , ulOptions AS DWORD) AS DWORD 
    RETURN ACE.AdsGetRecordCRC(hTable, OUT pulCRC, ulOptions)
        

FUNCTION AdsGetScope(hIndex AS IntPtr , usScopeOption AS WORD, [InAttribute] [OutAttribute] strScope AS CHAR[] , pusBufLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetScope(hIndex, usScopeOption, strScope, REF pusBufLen)
    

FUNCTION AdsGetTableOpenOptions(hTable AS IntPtr , pulOptions OUT DWORD ) AS DWORD 
    RETURN ACE.AdsGetTableOpenOptions(hTable, OUT pulOptions)
        

FUNCTION AdsGetTableType(hTable as IntPtr , pusType out WORD) AS DWORD 
    RETURN ACE.AdsGetTableType(hTable, OUT pusType)
        

FUNCTION AdsGetLastTableUpdate(hTable AS IntPtr , [InAttribute] [OutAttribute] strDate AS CHAR[] , pusDateLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetLastTableUpdate(hTable , strDate , REF pusDateLen ) 
    

FUNCTION AdsGotoBottom(hObj AS IntPtr ) AS DWORD 
    RETURN ACE.AdsGotoBottom(hObj)
        

FUNCTION AdsGotoTop(hObj AS IntPtr ) AS DWORD
    RETURN ACE.AdsGotoTop(hObj)
        

FUNCTION AdsGotoRecord(hObj AS IntPtr, nRecord AS DWORD ) AS DWORD
    RETURN ACE.AdsGotoRecord(hObj, nRecord)
        
        

FUNCTION AdsInTransaction(hConnect AS IntPtr, pbInTrans OUT WORD ) AS DWORD 
    RETURN ACE.AdsInTransaction(hConnect, OUT pbInTrans)
        
FUNCTION AdsIsFound(hObj AS IntPtr , pbFound OUT WORD) AS DWORD 
    RETURN ACE.AdsIsFound(hObj, OUT pbFound)
        
FUNCTION AdsIsTableLocked(hTable AS IntPtr , pbLocked OUT WORD ) AS DWORD 
    RETURN ACE.AdsIsTableLocked(hTable, OUT pbLocked)
        
FUNCTION AdsIsRecordLocked(hTable AS IntPtr , ulRec AS DWORD , pbLocked OUT WORD ) AS DWORD 
    RETURN ACE.AdsIsRecordLocked(hTable, ulRec, OUT pbLocked)
        
FUNCTION AdsIsRecordVisible(hObj AS IntPtr , pbVisible OUT WORD ) AS DWORD 
    RETURN ACE.AdsIsRecordVisible(hObj, OUT pbVisible)
        
FUNCTION AdsIsServerLoaded(strServer AS STRING , pbLoaded OUT WORD ) AS DWORD 
    RETURN ACE.AdsIsServerLoaded(strServer, OUT pbLoaded)
        
FUNCTION AdsIsRecordDeleted(hTable AS IntPtr , pbDeleted OUT WORD ) AS DWORD 
    RETURN ACE.AdsIsRecordDeleted(hTable, OUT pbDeleted)
        
FUNCTION AdsLockRecord(hTable AS IntPtr , ulRec AS DWORD ) AS DWORD 
    RETURN ACE.AdsLockRecord(hTable, ulRec)
        
FUNCTION AdsLockTable(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsLockTable(hTable)
        
FUNCTION AdsPackTable(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsPackTable(hTable)
        
FUNCTION AdsRecallRecord(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsRecallRecord(hTable)
        
FUNCTION AdsRecallAllRecords(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsRecallAllRecords(hTable)
        
FUNCTION AdsRefreshRecord(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsRefreshRecord(hTable)
        
FUNCTION AdsClearProgressCallback() AS DWORD 
    RETURN ACE.AdsClearProgressCallback()
        
FUNCTION AdsClearCallbackFunction() AS DWORD 
    RETURN ACE.AdsClearCallbackFunction()
        
FUNCTION AdsResetConnection(hConnect AS IntPtr) AS DWORD 
    RETURN ACE.AdsResetConnection(hConnect)
        
FUNCTION AdsRollbackTransaction(hConnect AS IntPtr) AS DWORD 
    RETURN ACE.AdsRollbackTransaction(hConnect)
        
FUNCTION AdsSeek(hIndex AS IntPtr , strKey AS STRING , usKeyLen AS WORD, usDataType AS WORD, usSeekType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN ACE.AdsSeek(hIndex, strKey, usKeyLen, usDataType, usSeekType, OUT pbFound)
        
        
FUNCTION AdsSeek(hIndex AS IntPtr , abKey AS BYTE[] , usKeyLen AS WORD, usDataType AS WORD, usSeekType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN ACE.AdsSeek(hIndex, abKey, usKeyLen, usDataType, usSeekType, OUT pbFound)
        
FUNCTION AdsSeekLast(hIndex AS IntPtr , strKey AS STRING , usKeyLen AS WORD, usDataType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN ACE.AdsSeekLast(hIndex, strKey, usKeyLen, usDataType, OUT pbFound)
        
FUNCTION AdsSeekLast(hIndex AS IntPtr , abKey AS BYTE[] , usKeyLen AS WORD, usDataType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN ACE.AdsSeekLast(hIndex, abKey, usKeyLen, usDataType, OUT pbFound)
        
FUNCTION AdsSetDateFormat(strFormat AS STRING ) AS DWORD 
    RETURN ACE.AdsSetDateFormat(strFormat)
        

FUNCTION AdsSetDecimals(usDecimals AS WORD ) AS DWORD 
    RETURN ACE.AdsSetDecimals(usDecimals)
        

FUNCTION AdsShowDeleted(bShowDeleted AS WORD ) AS DWORD 
    RETURN ACE.AdsShowDeleted(bShowDeleted)
        

FUNCTION AdsSetEpoch(usCentury AS WORD ) AS DWORD 
    RETURN ACE.AdsSetEpoch(usCentury)
        

FUNCTION AdsSetExact(bExact AS WORD ) AS DWORD 
    RETURN ACE.AdsSetExact(bExact)
        

FUNCTION AdsSetFilter(hTable AS IntPtr , strFilter AS STRING ) AS DWORD 
    RETURN ACE.AdsSetFilter(hTable, strFilter)
        

FUNCTION AdsSetRelation(hTableParent AS IntPtr , hIndexChild AS IntPtr , strExpr AS STRING ) AS DWORD 
    RETURN ACE.AdsSetRelation(hTableParent, hIndexChild, strExpr)
        

FUNCTION AdsSetServerType(usServerOptions AS WORD ) AS DWORD 
    RETURN ACE.AdsSetServerType(usServerOptions)
        

FUNCTION AdsSetScope(hIndex AS IntPtr , usScopeOption AS WORD, strScope AS STRING , usScopeLen AS WORD , usDataType AS WORD) AS DWORD 
    RETURN ACE.AdsSetScope(hIndex, usScopeOption, strScope, usScopeLen, usDataType)
        

FUNCTION AdsSetScope(hIndex AS IntPtr , usScopeOption AS WORD, abScope AS BYTE[] , usScopeLen AS WORD , usDataType AS WORD) AS DWORD 
    RETURN ACE.AdsSetScope(hIndex, usScopeOption, abScope, usScopeLen, usDataType)
        

FUNCTION AdsSkip(hObj AS IntPtr , lRecs AS INT) AS DWORD 
    RETURN ACE.AdsSkip(hObj, lRecs)
        

FUNCTION AdsThreadExit() AS DWORD 
    RETURN ACE.AdsThreadExit()
        

FUNCTION AdsUnlockRecord(hTable AS IntPtr , ulRec AS DWORD) AS DWORD 
    RETURN ACE.AdsUnlockRecord(hTable, ulRec)
        

FUNCTION AdsUnlockTable(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsUnlockTable(hTable)
        

FUNCTION AdsWriteAllRecords() AS DWORD 
    RETURN ACE.AdsWriteAllRecords()
        

FUNCTION AdsWriteRecord(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsWriteRecord(hTable)
        

FUNCTION AdsZapTable(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsZapTable(hTable)
        

FUNCTION AdsSetAOF(hTable AS IntPtr , strFilter AS STRING , usOptions AS WORD ) AS DWORD 
    RETURN ACE.AdsSetAOF(hTable, strFilter, usOptions)
        
        

FUNCTION AdsClearAOF(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsClearAOF(hTable)
        

FUNCTION AdsRefreshAOF(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsRefreshAOF(hTable)
        

FUNCTION AdsInitRawKey(hIndex AS IntPtr ) AS DWORD 
    RETURN ACE.AdsInitRawKey(hIndex)
        

FUNCTION AdsExecuteSQL(hStatement AS IntPtr , phCursor OUT IntPtr ) AS DWORD 
    RETURN ACE.AdsExecuteSQL(hStatement, OUT phCursor)
        
        

FUNCTION AdsCloseSQLStatement(hStatement AS IntPtr ) AS DWORD 
    RETURN ACE.AdsCloseSQLStatement(hStatement)
        
        

FUNCTION AdsStmtDisableEncryption(hStatement AS IntPtr ) AS DWORD 
    RETURN ACE.AdsStmtDisableEncryption(hStatement)
        
        

FUNCTION AdsStmtClearTablePasswords(hStatement AS IntPtr ) AS DWORD 
    RETURN ACE.AdsStmtClearTablePasswords(hStatement)
        

FUNCTION AdsClearSQLParams(hStatement AS IntPtr ) AS DWORD 
    RETURN ACE.AdsClearSQLParams(hStatement)
        

FUNCTION AdsClearSQLAbortFunc() AS DWORD 
    RETURN ACE.AdsClearSQLAbortFunc()
        

FUNCTION AdsFlushFileBuffers(hTable AS IntPtr ) AS DWORD 
    RETURN ACE.AdsFlushFileBuffers(hTable)
        

FUNCTION AdsDisableUniqueEnforcement(hConnect AS IntPtr) AS DWORD 
    RETURN ACE.AdsDisableUniqueEnforcement(hConnect)
        

FUNCTION AdsEnableUniqueEnforcement(hConnect AS IntPtr) AS DWORD 
    RETURN ACE.AdsEnableUniqueEnforcement(hConnect)
        

FUNCTION AdsDisableRI(hConnect AS IntPtr) AS DWORD 
    RETURN ACE.AdsDisableRI(hConnect)
        

FUNCTION AdsEnableRI(hConnect AS IntPtr) AS DWORD 
    RETURN ACE.AdsEnableRI(hConnect)
        

FUNCTION AdsDisableAutoIncEnforcement(hConnection AS IntPtr) AS DWORD 
    RETURN ACE.AdsDisableAutoIncEnforcement(hConnection)
        

FUNCTION AdsEnableAutoIncEnforcement(hConnection AS IntPtr) AS DWORD 
    RETURN ACE.AdsEnableAutoIncEnforcement(hConnection)
        

FUNCTION AdsGetIndexHandleByOrder(hTable AS IntPtr , usOrderNum AS WORD , phIndex OUT IntPtr ) AS DWORD 
    RETURN ACE.AdsGetIndexHandleByOrder(hTable, usOrderNum, OUT phIndex)
        

FUNCTION AdsGetNumIndexes(hTable AS IntPtr , pusNum OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetNumIndexes(hTable, OUT pusNum)
        

FUNCTION AdsGetRecordLength(hTable AS IntPtr , pulLength OUT DWORD ) AS DWORD 
    RETURN ACE.AdsGetRecordLength(hTable, OUT pulLength)
        
        

FUNCTION AdsGetTableFilename(hTable AS IntPtr , usOption AS WORD , strName AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetTableFilename(hTable, usOption, strName , REF wLen)
        

FUNCTION AdsOpenTable90(hConnect AS IntPtr, strName AS STRING , strAlias AS STRING , usTableType AS WORD, usCharType AS WORD , usLockType AS WORD , usCheckRights AS WORD , ulOptions AS DWORD, strCollation AS STRING , phTable OUT IntPtr ) AS DWORD 
    RETURN ACE.AdsOpenTable90(hConnect, strName, strAlias, usTableType, usCharType, usLockType, usCheckRights, ulOptions, strCollation, OUT phTable)
        

FUNCTION AdsCreateTable90(hConnect AS IntPtr, strName AS STRING , strDBObjName AS STRING , usTableType AS WORD, usCharType AS WORD , usLockType AS WORD , usCheckRights AS WORD , usMemoSize AS WORD , strFields AS STRING , ulOptions AS DWORD, strCollation AS STRING , phTable OUT IntPtr ) AS DWORD 
    RETURN ACE.AdsCreateTable90(hConnect, strName, strDBObjName, usTableType, usCharType, usLockType, usCheckRights, usMemoSize, strFields, ulOptions, strCollation, OUT phTable)
        

FUNCTION AdsSetDefault(strDefault AS STRING ) AS DWORD 
    RETURN ACE.AdsSetDefault(strDefault)
        

FUNCTION AdsSetSearchPath(strPath AS STRING ) AS DWORD 
    RETURN ACE.AdsSetSearchPath(strPath)
        

FUNCTION AdsGetFieldDecimals(hTable AS IntPtr , lFieldOrdinal AS DWORD, pusDecimals OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetFieldDecimals(hTable, lFieldOrdinal, OUT pusDecimals)
        

FUNCTION AdsGetFieldName(hTable AS IntPtr , usFld AS WORD , [InAttribute] [OutAttribute] strName AS CHAR[] , pusBufLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetFieldName(hTable, usFld, strName, REF pusBufLen)
    

FUNCTION AdsGetNumFields(hTable AS IntPtr , pusCount OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetNumFields(hTable, OUT pusCount)
        

FUNCTION AdsGetField(hTable AS IntPtr , lFieldOrdinal AS DWORD, [InAttribute] [OutAttribute] abBuf AS BYTE[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD 
    RETURN ACE.AdsGetField(hTable, lFieldOrdinal, abBuf, REF pulLen, usOption)
    

FUNCTION AdsGetField(hTable AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD 
    RETURN ACE.AdsGetField(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)
        

FUNCTION AdsIsEmpty(hTable AS IntPtr , lFieldOrdinal AS DWORD, pbEmpty OUT WORD ) AS DWORD 
    RETURN ACE.AdsIsEmpty(hTable, lFieldOrdinal, OUT pbEmpty)
        

FUNCTION AdsIsEmpty(hTable AS IntPtr , strFldName AS STRING , pbEmpty OUT WORD ) AS DWORD 
    RETURN ACE.AdsIsEmpty(hTable, strFldName, OUT pbEmpty)
        

FUNCTION AdsGetString(hTable AS IntPtr , lFieldOrdinal AS DWORD, [InAttribute] [OutAttribute] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD) AS DWORD 
    RETURN ACE.AdsGetString(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)


FUNCTION AdsGetString(hTable AS IntPtr , strFieldName AS STRING, [InAttribute] [OutAttribute] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD 
    RETURN ACE.AdsGetString(hTable, strFieldName, strBuf, REF pulLen, usOption)


FUNCTION AdsGetStringW(hTable AS IntPtr , lFieldOrdinal AS DWORD, [InAttribute] [OutAttribute] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD) AS DWORD 
    RETURN ACE.AdsGetString(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)


FUNCTION AdsGetStringW(hTable AS IntPtr , strFieldName AS STRING, [InAttribute] [OutAttribute] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD 
    RETURN ACE.AdsGetStringW(hTable, strFieldName,  strBuf, REF pulLen, usOption)


FUNCTION AdsGetJulian(hTable AS IntPtr , lFieldOrdinal AS DWORD, plDate OUT INT ) AS DWORD 
    RETURN ACE.AdsGetJulian(hTable, lFieldOrdinal, OUT plDate)
        

FUNCTION AdsGetJulian(hTable AS IntPtr , strFldName AS STRING , plDate OUT INT ) AS DWORD 
    RETURN ACE.AdsGetJulian(hTable, strFldName, OUT plDate)
        

FUNCTION AdsGetLogical(hTable AS IntPtr , lFieldOrdinal AS DWORD, pbValue OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetLogical(hTable, lFieldOrdinal, OUT pbValue)
        

FUNCTION AdsGetLogical(hTable AS IntPtr , strFldName AS STRING , pbValue OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetLogical(hTable, strFldName, OUT pbValue)



FUNCTION AdsSetEmpty(hObj AS IntPtr , lFieldOrdinal AS DWORD) AS DWORD 
    RETURN ACE.AdsSetEmpty(hObj, lFieldOrdinal)
        

FUNCTION AdsSetEmpty(hObj AS IntPtr , strFldName AS STRING ) AS DWORD 
    RETURN ACE.AdsSetEmpty(hObj, strFldName)
        
        

FUNCTION AdsSetField(hObj AS IntPtr , lFieldOrdinal AS DWORD, abBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
    RETURN ACE.AdsSetField(hObj, lFieldOrdinal, abBuf, ulLen)


FUNCTION AdsSetField(hObj AS IntPtr , strFldName AS STRING, abBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
    RETURN ACE.AdsSetField(hObj, strFldName, abBuf, ulLen)
   

FUNCTION AdsSetField(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN ACE.AdsSetField(hObj, lFieldOrdinal, strBuf, ulLen)


FUNCTION AdsSetField(hObj AS IntPtr , strFldName AS STRING, strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN ACE.AdsSetField(hObj, strFldName, strBuf, ulLen)


FUNCTION AdsSetString(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN ACE.AdsSetString(hObj, lFieldOrdinal, strBuf, ulLen)


FUNCTION AdsSetString(hObj AS IntPtr , strFldName AS STRING , strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN ACE.AdsSetString(hObj, strFldName, strBuf, ulLen)
        

FUNCTION AdsSetStringW(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN ACE.AdsSetString(hObj, lFieldOrdinal, strBuf, ulLen)
        

FUNCTION AdsSetStringW(hObj AS IntPtr , strFldName AS STRING , strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN ACE.AdsSetString(hObj, strFldName, strBuf, ulLen)
        

FUNCTION AdsSetBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, usBinaryType AS WORD , ulTotalLength AS DWORD , ulOffset AS DWORD , strBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
    RETURN ACE.AdsSetBinary(hTable, lFieldOrdinal, usBinaryType, ulTotalLength, ulOffset, strBuf, ulLen)


FUNCTION AdsSetBinary(hTable AS IntPtr , strFldName AS STRING , usBinaryType AS WORD , ulTotalLength AS DWORD , ulOffset AS DWORD , strBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
    RETURN ACE.AdsSetBinary(hTable, strFldName, usBinaryType, ulTotalLength, ulOffset, strBuf, ulLen)
        

FUNCTION AdsSetDouble(hObj AS IntPtr , lFieldOrdinal AS DWORD, dValue AS REAL8) AS DWORD 
    RETURN ACE.AdsSetDouble(hObj, lFieldOrdinal, dValue)
        

FUNCTION AdsSetDouble(hObj AS IntPtr , strFldName AS STRING, dValue AS REAL8) AS DWORD 
    RETURN ACE.AdsSetDouble(hObj, strFldName, dValue)


FUNCTION AdsSetJulian(hObj AS IntPtr , lFieldOrdinal AS DWORD, lDate AS INT) AS DWORD 
    RETURN ACE.AdsSetJulian(hObj, lFieldOrdinal, lDate)


FUNCTION AdsSetJulian(hObj AS IntPtr , strFldName AS STRING , lDate AS INT ) AS DWORD 
    RETURN ACE.AdsSetJulian(hObj, strFldName, lDate)


FUNCTION AdsSetLogical(hObj AS IntPtr , lFieldOrdinal AS DWORD, bValue AS WORD ) AS DWORD 
    RETURN ACE.AdsSetLogical(hObj, lFieldOrdinal, bValue)
        

FUNCTION AdsSetLogical(hObj AS IntPtr , strFldName AS STRING , bValue AS WORD ) AS DWORD 
    RETURN ACE.AdsSetLogical(hObj, strFldName, bValue)


FUNCTION AdsSetMoney(hObj AS IntPtr , lFieldOrdinal AS DWORD, qValue AS INT64 ) AS DWORD 
    RETURN ACE.AdsSetMoney(hObj, lFieldOrdinal, qValue)
        

FUNCTION AdsSetMoney(hObj AS IntPtr , strFldName AS STRING , qValue AS INT64 ) AS DWORD 
    RETURN ACE.AdsSetMoney(hObj, strFldName, qValue)


FUNCTION AdsSetMilliseconds(hObj AS IntPtr , lFieldOrdinal AS DWORD, lTime AS INT ) AS DWORD 
    RETURN ACE.AdsSetMilliseconds(hObj, lFieldOrdinal, lTime)
        

FUNCTION AdsSetMilliseconds(hObj AS IntPtr , strFldName AS STRING , lTime AS INT ) AS DWORD 
    RETURN ACE.AdsSetMilliseconds(hObj, strFldName, lTime)


FUNCTION AdsSetNull(hObj AS IntPtr , lFieldOrdinal AS DWORD ) AS DWORD 
    RETURN ACE.AdsSetNull(hObj, lFieldOrdinal)


FUNCTION AdsSetNull(hObj AS IntPtr , strFldName AS STRING  ) AS DWORD 
    RETURN ACE.AdsSetNull(hObj, strFldName)


FUNCTION AdsSetShort(hObj AS IntPtr , lFieldOrdinal AS DWORD, sValue AS SHORT ) AS DWORD 
    RETURN ACE.AdsSetShort(hObj, lFieldOrdinal, sValue)
        

FUNCTION AdsSetShort(hObj AS IntPtr , strFldName AS STRING , sValue AS SHORT ) AS DWORD 
    RETURN ACE.AdsSetShort(hObj, strFldName, sValue)


FUNCTION AdsSetTime(hObj AS IntPtr , lFieldOrdinal AS DWORD, strValue AS STRING , wLen AS WORD  ) AS DWORD 
    RETURN ACE.AdsSetTime(hObj, lFieldOrdinal, strValue, wLen)
        

FUNCTION AdsSetTime(hObj AS IntPtr , strFldName AS STRING , strValue AS STRING , wLen AS WORD ) AS DWORD 
    RETURN ACE.AdsSetTime(hObj, strFldName, strValue, wLen)


FUNCTION AdsSetTimeStamp(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD   ) AS DWORD 
    RETURN ACE.AdsSetTimeStamp(hObj, lFieldOrdinal, strBuf, ulLen)
        

FUNCTION AdsSetTimeStamp(hObj AS IntPtr , strFldName AS STRING , strBuf AS STRING , ulLen AS DWORD  ) AS DWORD 
    RETURN ACE.AdsSetTimeStamp(hObj, strFldName, strBuf, ulLen)


FUNCTION AdsReindex(hObject AS IntPtr) AS DWORD 
    RETURN ACE.AdsReindex(hObject)
        

FUNCTION AdsGetIndexFilename(hIndex AS IntPtr , usOption AS WORD , [InAttribute] [OutAttribute] strName AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetIndexFilename(hIndex, usOption, strName , REF wLen)
    

FUNCTION AdsOpenIndex(hTable AS IntPtr , strName AS STRING , [InAttribute] [OutAttribute] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN ACE.AdsOpenIndex(hTable,  strName , ahIndex, REF pusArrayLen )
    

FUNCTION AdsCreateIndex(hObj AS IntPtr , strFileName AS STRING , strTag AS STRING , strExpr AS STRING , strCondition AS STRING , strWhile AS STRING , ulOptions AS DWORD, phIndex OUT IntPtr ) AS DWORD 
    RETURN ACE.AdsCreateIndex(hObj,  strFileName , strTag, strExpr , strCondition , strWhile , ulOptions , OUT phIndex ) 


FUNCTION AdsCreateIndex90(hObj AS IntPtr , strFileName AS STRING , strTag AS STRING , strExpr AS STRING , strCondition AS STRING , strWhile AS STRING , ulOptions AS DWORD, ulPageSize AS DWORD , strCollation AS STRING , phIndex OUT IntPtr ) AS DWORD 
    RETURN ACE.AdsCreateIndex90(hObj,  strFileName , strTag, strExpr , strCondition , strWhile , ulOptions , ulPageSize, strCollation, OUT phIndex ) 
        

FUNCTION AdsRegisterCallbackFunction(pfn AS CallbackFn , ulCallBackID AS DWORD ) AS DWORD 
    RETURN ACE.AdsRegisterCallbackFunction(pfn,  ulCallBackID  )
        

FUNCTION AdsExtractKey(hIndex AS IntPtr , [InAttribute] [OutAttribute] strKey AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN ACE.AdsExtractKey(hIndex,  strKey , REF wLen )
    

FUNCTION AdsGetAOFOptLevel(hTable AS IntPtr , pusOptLevel OUT WORD , [InAttribute] [OutAttribute] strNonOpt AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetAOFOptLevel(hTable,  OUT pusOptLevel , strNonOpt , REF wLen)
    

FUNCTION AdsGetIndexCondition(hIndex AS IntPtr , [InAttribute] [OutAttribute] strExpr AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetIndexCondition(hIndex,  strExpr , REF wLen)
    

FUNCTION AdsGetIndexExpr(hIndex AS IntPtr , [InAttribute] [OutAttribute] strExpr AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetIndexExpr(hIndex,  strExpr , REF wLen)
    

FUNCTION AdsGetIndexName(hIndex AS IntPtr , [InAttribute] [OutAttribute] strName AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN ACE.AdsGetIndexName(hIndex,  strName , REF wLen)


FUNCTION AdsGetIndexOrderByHandle(hIndex AS IntPtr , pusIndexOrder OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetIndexOrderByHandle(hIndex,  OUT pusIndexOrder)
    

FUNCTION AdsSkipUnique(hIndex AS IntPtr , lRecs AS INT) AS DWORD 
    RETURN ACE.AdsSkipUnique(hIndex,  lRecs)
    

FUNCTION AdsGetKeyCount(hIndex AS IntPtr , usFilterOption AS WORD, pulCount OUT DWORD ) AS DWORD 
    RETURN ACE.AdsGetKeyCount(hIndex, usFilterOption, OUT pulCount)
    

FUNCTION AdsGetKeyLength(hIndex AS IntPtr , pusKeyLength OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetKeyLength(hIndex, OUT pusKeyLength)
    

FUNCTION AdsGetKeyNum(hIndex AS IntPtr , usFilterOption AS WORD, pulKey OUT DWORD ) AS DWORD 
    RETURN ACE.AdsGetKeyNum(hIndex, usFilterOption, OUT pulKey)
    

FUNCTION AdsGetKeyType(hIndex AS IntPtr , usKeyType OUT WORD ) AS DWORD 
    RETURN ACE.AdsGetKeyType(hIndex,  OUT usKeyType)
    

FUNCTION AdsGetRelKeyPos(hIndex AS IntPtr , pdPos OUT System.Double ) AS DWORD 
    RETURN ACE.AdsGetRelKeyPos(hIndex,  OUT pdPos)
    

FUNCTION AdsIsIndexCustom(hIndex AS IntPtr , pbCustom OUT WORD ) AS DWORD 
    RETURN ACE.AdsIsIndexCustom(hIndex,  OUT pbCustom)
    

FUNCTION AdsIsIndexDescending(hIndex AS IntPtr , pbDescending OUT WORD ) AS DWORD 
    RETURN ACE.AdsIsIndexDescending(hIndex,  OUT pbDescending)
    

FUNCTION AdsIsIndexUnique(hIndex AS IntPtr , pbUnique OUT WORD ) AS DWORD 
    RETURN ACE.AdsIsIndexUnique(hIndex,  OUT pbUnique)
    

FUNCTION AdsCreateSQLStatement(hConnect as IntPtr, phStatement out IntPtr ) as DWORD 
    RETURN ACE.AdsCreateSQLStatement(hConnect,  OUT phStatement)
    

FUNCTION AdsStmtSetTableType(hStatement as IntPtr , usTableType as WORD) as DWORD 
    RETURN ACE.AdsStmtSetTableType(hStatement,  usTableType)
    

FUNCTION AdsStmtSetTableCharType(hStatement as IntPtr , usCharType as WORD ) as DWORD 
    RETURN ACE.AdsStmtSetTableCharType(hStatement,  usCharType)
    

FUNCTION AdsStmtSetTableCollation(hStatement as IntPtr , strCollation as string ) as DWORD 
    RETURN ACE.AdsStmtSetTableCollation(hStatement,  strCollation)
    

FUNCTION AdsStmtSetTableLockType(hStatement as IntPtr , usLockType as WORD ) as DWORD 
    RETURN ACE.AdsStmtSetTableLockType(hStatement,  usLockType)
    

FUNCTION AdsStmtSetTablePassword(hStatement as IntPtr , strTableName as string , strPassword as string ) as DWORD 
    RETURN ACE.AdsStmtSetTablePassword(hStatement,  strTableName, strPassword)
    

FUNCTION AdsStmtSetTableReadOnly(hStatement as IntPtr , usReadOnly as WORD ) as DWORD 
    RETURN ACE.AdsStmtSetTableReadOnly(hStatement,  usReadOnly)
    

FUNCTION AdsStmtSetTableRights(hStatement as IntPtr , usCheckRights as WORD ) as DWORD 
    RETURN ACE.AdsStmtSetTableRights(hStatement,  usCheckRights)
    

FUNCTION AdsExecuteSQLDirect(hStatement as IntPtr , strSQL as string , phCursor out IntPtr ) as DWORD 
    RETURN ACE.AdsExecuteSQLDirect(hStatement,  strSQL, OUT phCursor)
