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
        RETURN Convert.ToUint32(oRet)
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
        RETURN Convert.ToUint32(oHandle)
    ENDIF
    RETURN 0
    
/// <summary>Returns the table handle for the current workarea.  This handle can be used to call the Advantage Client Engine directly.</summary>
/// <returns> Returns a 0 if there is a problem.</returns>
FUNCTION AX_GetAceTableHandle() AS DWORD
    LOCAL oHandle := NULL AS OBJECT
    IF CoreDb.Info( DBI_GET_ACE_TABLE_HANDLE , REF oHandle)
        RETURN Convert.ToUint32(oHandle)
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
    
    
    
_DLL FUNC DBFAXSAdsCommitTransaction ( hConnect AS IntPtr ) AS DWORD PASCAL:ACE32.AdsCommitTransaction

_DLL FUNC DBFAXSAdsIsServerLoaded ( pucServer AS STRING, pbLoaded REF WORD ) AS DWORD PASCAL:ACE32.AdsIsServerLoaded ANSI

_DLL FUNC DBFAXSAdsBeginTransaction ( hConnect AS IntPtr ) AS DWORD PASCAL:ACE32.AdsBeginTransaction

_DLL FUNC DBFAXSAdsRollbackTransaction ( hConnect AS IntPtr ) AS DWORD PASCAL:ACE32.AdsRollbackTransaction

_DLL FUNC DBFAXSAdsInTransaction ( hConnect AS IntPtr, pbInTrans REF WORD ) AS DWORD PASCAL:ACE32.AdsInTransaction

_DLL FUNC DBFAXSAdsFileToBinary ( hTbl AS IntPtr, pucFldName AS STRING, usBinaryType AS WORD, pucFileName AS STRING ) AS DWORD PASCAL:ACE32.AdsFileToBinary ANSI

_DLL FUNC DBFAXSAdsDisableEncryption( hTbl AS IntPtr ) AS DWORD PASCAL:ACE32.AdsDisableEncryption

_DLL FUNC DBFAXSAdsEnableEncryption( hTbl AS IntPtr, pucPassword AS STRING ) AS DWORD PASCAL:ACE32.AdsEnableEncryption ANSI

_DLL FUNC DBFAXSAdsSetServerType ( usServerOptions AS WORD ) AS DWORD PASCAL:ACE32.AdsSetServerType

_DLL FUNC DBFAXSAdsGetConnectionType ( hConnect AS IntPtr, pusConnectType REF WORD ) AS DWORD PASCAL:ACE32.AdsGetConnectionType

_DLL FUNC DBFAXSAdsFindConnection ( pucServerName AS STRING, phConnect REF IntPtr ) AS DWORD PASCAL:ACE32.AdsFindConnection ANSI

_DLL FUNC DBFAXSAdsBinaryToFile ( hTbl AS IntPtr, pucFldName AS STRING, pucFileName AS STRING ) AS DWORD PASCAL:ACE32.AdsBinaryToFile ANSI



DEFINE AX_BEGIN_TRANSACTION  := 1 
DEFINE AX_COMMIT_TRANSACTION  := 2 
DEFINE AX_ROLLBACK_TRANSACTION  := 3 
DEFINE AX_ISACTIVE_TRANSACTION  := 4 
DEFINE DBFAXS_ADS_LOCAL_SERVER := 1
DEFINE DBFAXS_ADS_REMOTE_SERVER := 2
DEFINE DBFAXS_ADS_AIS_SERVER := 4



[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsAddCustomKey(hIndex AS IntPtr ) AS DWORD 
    RETURN Ace.AdsAddCustomKey(hIndex)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsAppendRecord(hTable AS IntPtr ) AS DWORD
    RETURN Ace.AdsAppendRecord(hTable)
    
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsApplicationExit() AS DWORD
    RETURN Ace.AdsApplicationExit()
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsAtBOF(hTable AS IntPtr , pbBof OUT WORD ) AS DWORD
    RETURN Ace.AdsAtBOF(hTable, OUT pbBof)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsAtEOF(hTable AS IntPtr , pbEof OUT WORD ) AS DWORD 
    RETURN Ace.AdsAtEOF(hTable , OUT pbEof  )
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsBeginTransaction(hConnect AS IntPtr) AS DWORD
    RETURN Ace.AdsBeginTransaction(hConnect)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsBinaryToFile(hTable AS IntPtr , strFldName AS STRING , strFileName AS STRING ) AS DWORD
    RETURN Ace.AdsBinaryToFile(hTable, strFldName , strFileName )
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsBinaryToFile(hTable AS IntPtr , lFieldOrdinal AS DWORD, strFileName AS STRING ) AS DWORD
    RETURN Ace.AdsBinaryToFile(hTable, lFieldOrdinal, strFileName)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCacheOpenCursors(usOpen AS WORD) AS DWORD
    RETURN Ace.AdsCacheOpenCursors(usOpen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCacheOpenTables(usOpen AS WORD) AS DWORD
    RETURN Ace.AdsCacheOpenTables(usOpen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCacheRecords(hTable AS IntPtr , usNumRecords AS WORD ) AS DWORD
    RETURN Ace.AdsCacheRecords(hTable, usNumRecords)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCancelUpdate(hTable AS IntPtr ) AS DWORD
    RETURN Ace.AdsCancelUpdate(hTable)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsClearAllScopes(hTable AS IntPtr ) AS DWORD
    RETURN Ace.AdsClearAllScopes(hTable)
    
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsClearDefault() AS DWORD
    RETURN Ace.AdsClearDefault()
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsClearFilter(hTable AS IntPtr ) AS DWORD
    RETURN Ace.AdsClearFilter(hTable)
    
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsClearRelation(hTableParent AS IntPtr ) AS DWORD 
    RETURN Ace.AdsClearRelation(hTableParent)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsClearScope(hIndex AS IntPtr , usScopeOption AS WORD) AS DWORD
    RETURN Ace.AdsClearScope(hIndex, usScopeOption)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCloneTable(hTable AS IntPtr , phClone OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsCloneTable(hTable, OUT phClone)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCloseAllIndexes(hTable AS IntPtr ) AS DWORD
    RETURN Ace.AdsCloseAllIndexes(hTable)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCloseAllTables() AS DWORD 
    RETURN Ace.AdsCloseAllTables()
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCloseIndex(hIndex AS IntPtr ) AS DWORD
    RETURN Ace.AdsCloseIndex(hIndex)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCloseTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsCloseTable(hTable)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCloseCachedTables(hConnection AS IntPtr) AS DWORD
    RETURN Ace.AdsCloseCachedTables(hConnection)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCommitTransaction(hConnect AS IntPtr) AS DWORD
    RETURN Ace.AdsCommitTransaction(hConnect)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsContinue(hTable AS IntPtr , pbFound OUT WORD) AS DWORD
    RETURN Ace.AdsContinue(hTable, OUT pbFound)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsConnect60(pucServerPath AS STRING , usServerTypes AS WORD, pucUserName AS STRING , pucPassword AS STRING , ulOptions AS DWORD, phConnect OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsConnect60(pucServerPath, usServerTypes, pucUserName, pucPassword, ulOptions, OUT phConnect)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCopyTableStructure(hTable AS IntPtr , strFile AS STRING ) AS DWORD 
    RETURN Ace.AdsCopyTableStructure(hTable, strFile)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCreateFTSIndex(hTable AS IntPtr , strFileName AS STRING , strTag AS STRING , strField AS STRING , ulPageSize AS DWORD , ulMinWordLen AS DWORD , ulMaxWordLen AS DWORD , usUseDefaultDelim AS WORD , strDelimiters AS STRING , usUseDefaultNoise AS WORD , strNoiseWords AS STRING , usUseDefaultDrop AS WORD , strDropChars AS STRING , usUseDefaultConditionals AS WORD , strConditionalChars AS STRING , strReserved1 AS STRING , strReserved2 AS STRING , ulOptions AS DWORD) AS DWORD 
    RETURN Ace.AdsCreateFTSIndex(hTable, strFileName, strTag, strField, ulPageSize , ulMinWordLen , ulMaxWordLen , usUseDefaultDelim , strDelimiters , usUseDefaultNoise , strNoiseWords , usUseDefaultDrop , strDropChars , usUseDefaultConditionals , strConditionalChars , strReserved1 , strReserved2 , ulOptions )
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsDecryptRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDecryptRecord(hTable)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsDecryptTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDecryptTable(hTable)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsDeleteCustomKey(hIndex AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDeleteCustomKey(hIndex)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsDeleteIndex(hIndex AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDeleteIndex(hIndex)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsDeleteRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDeleteRecord(hTable)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsDisableEncryption(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsDisableEncryption(hTable)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsDisableLocalConnections() AS DWORD 
    RETURN Ace.AdsDisableLocalConnections()
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsDisconnect(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsDisconnect(hConnect)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsEnableEncryption(hTable AS IntPtr , strPassword AS STRING ) AS DWORD 
    RETURN Ace.AdsEnableEncryption(hTable, strPassword)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsEncryptRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsEncryptRecord(hTable)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsEncryptTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsEncryptTable(hTable)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsEvalLogicalExpr(hTable AS IntPtr , strExpr AS STRING , pbResult OUT WORD ) AS DWORD 
    RETURN Ace.AdsEvalLogicalExpr(hTable, strExpr, OUT pbResult)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsEvalNumericExpr(hTable AS IntPtr , strExpr AS STRING , pdResult OUT double ) AS DWORD 
    RETURN Ace.AdsEvalNumericExpr(hTable, strExpr, OUT pdResult)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsEvalStringExpr(hTable AS IntPtr , strExpr AS STRING , [IN] [OUT] strResult AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsEvalStringExpr(hTable, strExpr, strResult, REF wLen)
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsEvalTestExpr(hTable AS IntPtr , strExpr AS STRING , pusType OUT WORD ) AS DWORD 
        RETURN Ace.AdsEvalTestExpr(hTable, strExpr, OUT pusType)
        
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsFileToBinary(hTable AS IntPtr , strFldName AS STRING , usBinaryType AS WORD , strFileName AS STRING ) AS DWORD 
        RETURN Ace.AdsFileToBinary(hTable, strFldName, usBinaryType, strFileName)
        
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsFileToBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, usBinaryType AS WORD , strFileName AS STRING ) AS DWORD 
        RETURN Ace.AdsFileToBinary(hTable, lFieldOrdinal, usBinaryType, strFileName)
        
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsFindConnection(strServerName AS STRING , phConnect OUT IntPtr ) AS DWORD 
        RETURN Ace.AdsFindConnection(strServerName, OUT phConnect)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetAllIndexes(hTable AS IntPtr , [IN] [OUT] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetAllIndexes(hTable, ahIndex, REF pusArrayLen)
    
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetFTSIndexes(hTable AS IntPtr , [IN] [OUT] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetFTSIndexes(hTable, ahIndex, REF pusArrayLen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetAllLocks(hTable AS IntPtr , [IN] [OUT] aulLocks AS DWORD[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetAllLocks(hTable, aulLocks, REF pusArrayLen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetAllTables([IN] [OUT] ahTable AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetAllTables(ahTable, REF pusArrayLen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, ulOffset AS DWORD , [IN] [OUT] strBuf AS BYTE[] , pulLen REF DWORD ) AS DWORD 
    RETURN Ace.AdsGetBinary(hTable, lFieldOrdinal, ulOffSet, strBuf, REF pulLen)
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetBinaryLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD 
        RETURN Ace.AdsGetBinaryLength(hTable, lFieldOrdinal, OUT pulLength)
        
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetBookmark(hTable AS IntPtr , phBookmark OUT IntPtr ) AS DWORD 
        RETURN Ace.AdsGetBookmark(hTable, OUT phBookmark)
        
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetConnectionType(hConnect AS IntPtr, pusConnectType OUT WORD ) AS DWORD 
        RETURN Ace.AdsGetConnectionType(hConnect, OUT pusConnectType)
        
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetDate(hTable AS IntPtr , lFieldOrdinal AS DWORD, [IN] [OUT] strBuf AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetDate(hTable, lFieldOrdinal, strBuf, REF wLen)
    
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetDateFormat([IN] [OUT] strFormat AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetDateFormat(strFormat, REF wLen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetDouble(hTable AS IntPtr , lFieldOrdinal AS DWORD, pdValue OUT REAL8 ) AS DWORD 
    RETURN Ace.AdsGetDouble(hTable, lFieldOrdinal, OUT pdValue)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetFieldLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetFieldLength(hTable, lFieldOrdinal, OUT pulLength)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetFieldType(hTable AS IntPtr , lFieldOrdinal AS DWORD, pusType OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetFieldType(hTable, lFieldOrdinal, OUT pusType)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetHandleType(hObj AS IntPtr , pusType OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetHandleType(hObj, OUT pusType)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetIndexHandle(hTable AS IntPtr , strIndexOrder AS STRING , phIndex OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsGetIndexHandle(hTable, strIndexOrder, OUT phIndex)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetLastError(pulErrCode OUT DWORD , [IN] [OUT] strBuf AS CHAR[] , pusBufLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetLastError(OUT pulErrCode, strBuf, REF pusBufLen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetMemoLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetMemoLength(hTable, lFieldOrdinal, OUT pulLength)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetMemoBlockSize(hTable AS IntPtr , pusBlockSize OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetMemoBlockSize(hTable, OUT pusBlockSize)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetNumLocks(hTable AS IntPtr , pusNum OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetNumLocks(hTable, OUT pusNum)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetRecordCount(hTable AS IntPtr , usFilterOption AS WORD, pulCount OUT DWORD) AS DWORD 
    RETURN Ace.AdsGetRecordCount(hTable, usFilterOption, OUT pulCount)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetRecordNum(hTable AS IntPtr , usFilterOption AS WORD, pulRec OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetRecordNum(hTable, usFilterOption , OUT pulRec)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetRecordCRC(hTable AS IntPtr , pulCRC OUT DWORD , ulOptions AS DWORD) AS DWORD 
    RETURN Ace.AdsGetRecordCRC(hTable, OUT pulCRC, ulOptions)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetScope(hIndex AS IntPtr , usScopeOption AS WORD, [IN] [OUT] strScope AS CHAR[] , pusBufLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetScope(hIndex, usScopeOption, strScope, REF pusBufLen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetTableOpenOptions(hTable AS IntPtr , pulOptions OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetTableOpenOptions(hTable, OUT pulOptions)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetTableType(hTable as IntPtr , pusType out WORD) AS DWORD 
    RETURN Ace.AdsGetTableType(hTable, OUT pusType)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetLastTableUpdate(hTable AS IntPtr , [IN] [OUT] strDate AS CHAR[] , pusDateLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetLastTableUpdate(hTable , strDate , REF pusDateLen ) 
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGotoBottom(hObj AS IntPtr ) AS DWORD 
    RETURN Ace.AdsGotoBottom(hObj)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGotoTop(hObj AS IntPtr ) AS DWORD
    RETURN Ace.AdsGotoTop(hObj)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGotoRecord(hObj AS IntPtr, nRecord AS DWORD ) AS DWORD
    RETURN Ace.AdsGotoRecord(hObj, nRecord)
        
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsInTransaction(hConnect AS IntPtr, pbInTrans OUT WORD ) AS DWORD 
    RETURN Ace.AdsInTransaction(hConnect, OUT pbInTrans)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsIsFound(hObj AS IntPtr , pbFound OUT WORD) AS DWORD 
    RETURN Ace.AdsIsFound(hObj, OUT pbFound)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsIsTableLocked(hTable AS IntPtr , pbLocked OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsTableLocked(hTable, OUT pbLocked)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsIsRecordLocked(hTable AS IntPtr , ulRec AS DWORD , pbLocked OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsRecordLocked(hTable, ulRec, OUT pbLocked)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsIsRecordVisible(hObj AS IntPtr , pbVisible OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsRecordVisible(hObj, OUT pbVisible)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsIsServerLoaded(strServer AS STRING , pbLoaded OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsServerLoaded(strServer, OUT pbLoaded)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsIsRecordDeleted(hTable AS IntPtr , pbDeleted OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsRecordDeleted(hTable, OUT pbDeleted)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsLockRecord(hTable AS IntPtr , ulRec AS DWORD ) AS DWORD 
    RETURN Ace.AdsLockRecord(hTable, ulRec)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsLockTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsLockTable(hTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsPackTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsPackTable(hTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsRecallRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsRecallRecord(hTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsRecallAllRecords(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsRecallAllRecords(hTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsRefreshRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsRefreshRecord(hTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsClearProgressCallback() AS DWORD 
    RETURN Ace.AdsClearProgressCallback()
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsClearCallbackFunction() AS DWORD 
    RETURN Ace.AdsClearCallbackFunction()
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsResetConnection(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsResetConnection(hConnect)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsRollbackTransaction(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsRollbackTransaction(hConnect)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSeek(hIndex AS IntPtr , strKey AS STRING , usKeyLen AS WORD, usDataType AS WORD, usSeekType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN Ace.AdsSeek(hIndex, strKey, usKeyLen, usDataType, usSeekType, OUT pbFound)
        
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSeek(hIndex AS IntPtr , abKey AS BYTE[] , usKeyLen AS WORD, usDataType AS WORD, usSeekType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN Ace.AdsSeek(hIndex, abKey, usKeyLen, usDataType, usSeekType, OUT pbFound)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSeekLast(hIndex AS IntPtr , strKey AS STRING , usKeyLen AS WORD, usDataType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN Ace.AdsSeekLast(hIndex, strKey, usKeyLen, usDataType, OUT pbFound)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSeekLast(hIndex AS IntPtr , abKey AS BYTE[] , usKeyLen AS WORD, usDataType AS WORD, pbFound OUT WORD) AS DWORD 
    RETURN Ace.AdsSeekLast(hIndex, abKey, usKeyLen, usDataType, OUT pbFound)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetDateFormat(strFormat AS STRING ) AS DWORD 
    RETURN Ace.AdsSetDateFormat(strFormat)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetDecimals(usDecimals AS WORD ) AS DWORD 
    RETURN Ace.AdsSetDecimals(usDecimals)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsShowDeleted(bShowDeleted AS WORD ) AS DWORD 
    RETURN Ace.AdsShowDeleted(bShowDeleted)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetEpoch(usCentury AS WORD ) AS DWORD 
    RETURN Ace.AdsSetEpoch(usCentury)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetExact(bExact AS WORD ) AS DWORD 
    RETURN Ace.AdsSetExact(bExact)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetFilter(hTable AS IntPtr , strFilter AS STRING ) AS DWORD 
    RETURN Ace.AdsSetFilter(hTable, strFilter)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetRelation(hTableParent AS IntPtr , hIndexChild AS IntPtr , strExpr AS STRING ) AS DWORD 
    RETURN Ace.AdsSetRelation(hTableParent, hIndexChild, strExpr)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetServerType(usServerOptions AS WORD ) AS DWORD 
    RETURN Ace.AdsSetServerType(usServerOptions)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetScope(hIndex AS IntPtr , usScopeOption AS WORD, strScope AS STRING , usScopeLen AS WORD , usDataType AS WORD) AS DWORD 
    RETURN Ace.AdsSetScope(hIndex, usScopeOption, strScope, usScopeLen, usDataType)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetScope(hIndex AS IntPtr , usScopeOption AS WORD, abScope AS BYTE[] , usScopeLen AS WORD , usDataType AS WORD) AS DWORD 
    RETURN Ace.AdsSetScope(hIndex, usScopeOption, abScope, usScopeLen, usDataType)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSkip(hObj AS IntPtr , lRecs AS INT) AS DWORD 
    RETURN Ace.AdsSkip(hObj, lRecs)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsThreadExit() AS DWORD 
    RETURN Ace.AdsThreadExit()
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsUnlockRecord(hTable AS IntPtr , ulRec AS DWORD) AS DWORD 
    RETURN Ace.AdsUnlockRecord(hTable, ulRec)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsUnlockTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsUnlockTable(hTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsWriteAllRecords() AS DWORD 
    RETURN Ace.AdsWriteAllRecords()
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsWriteRecord(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsWriteRecord(hTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsZapTable(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsZapTable(hTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetAOF(hTable AS IntPtr , strFilter AS STRING , usOptions AS WORD ) AS DWORD 
    RETURN Ace.AdsSetAOF(hTable, strFilter, usOptions)
        
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsClearAOF(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsClearAOF(hTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsRefreshAOF(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsRefreshAOF(hTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsInitRawKey(hIndex AS IntPtr ) AS DWORD 
    RETURN Ace.AdsInitRawKey(hIndex)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsExecuteSQL(hStatement AS IntPtr , phCursor OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsExecuteSQL(hStatement, OUT phCursor)
        
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCloseSQLStatement(hStatement AS IntPtr ) AS DWORD 
    RETURN Ace.AdsCloseSQLStatement(hStatement)
        
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsStmtDisableEncryption(hStatement AS IntPtr ) AS DWORD 
    RETURN Ace.AdsStmtDisableEncryption(hStatement)
        
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsStmtClearTablePasswords(hStatement AS IntPtr ) AS DWORD 
    RETURN Ace.AdsStmtClearTablePasswords(hStatement)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsClearSQLParams(hStatement AS IntPtr ) AS DWORD 
    RETURN Ace.AdsClearSQLParams(hStatement)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsClearSQLAbortFunc() AS DWORD 
    RETURN Ace.AdsClearSQLAbortFunc()
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsFlushFileBuffers(hTable AS IntPtr ) AS DWORD 
    RETURN Ace.AdsFlushFileBuffers(hTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsDisableUniqueEnforcement(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsDisableUniqueEnforcement(hConnect)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsEnableUniqueEnforcement(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsEnableUniqueEnforcement(hConnect)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsDisableRI(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsDisableRI(hConnect)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsEnableRI(hConnect AS IntPtr) AS DWORD 
    RETURN Ace.AdsEnableRI(hConnect)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsDisableAutoIncEnforcement(hConnection AS IntPtr) AS DWORD 
    RETURN Ace.AdsDisableAutoIncEnforcement(hConnection)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsEnableAutoIncEnforcement(hConnection AS IntPtr) AS DWORD 
    RETURN Ace.AdsEnableAutoIncEnforcement(hConnection)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetIndexHandleByOrder(hTable AS IntPtr , usOrderNum AS WORD , phIndex OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsGetIndexHandleByOrder(hTable, usOrderNum, OUT phIndex)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetNumIndexes(hTable AS IntPtr , pusNum OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetNumIndexes(hTable, OUT pusNum)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetRecordLength(hTable AS IntPtr , pulLength OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetRecordLength(hTable, OUT pulLength)
        
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetTableFilename(hTable AS IntPtr , usOption AS WORD , strName AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetTableFilename(hTable, usOption, strName , REF wLen)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsOpenTable90(hConnect AS IntPtr, strName AS STRING , strAlias AS STRING , usTableType AS WORD, usCharType AS WORD , usLockType AS WORD , usCheckRights AS WORD , ulOptions AS DWORD, strCollation AS STRING , phTable OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsOpenTable90(hConnect, strName, strAlias, usTableType, usCharType, usLockType, usCheckRights, ulOptions, strCollation, OUT phTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCreateTable90(hConnect AS IntPtr, strName AS STRING , strDBObjName AS STRING , usTableType AS WORD, usCharType AS WORD , usLockType AS WORD , usCheckRights AS WORD , usMemoSize AS WORD , strFields AS STRING , ulOptions AS DWORD, strCollation AS STRING , phTable OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsCreateTable90(hConnect, strName, strDBObjName, usTableType, usCharType, usLockType, usCheckRights, usMemoSize, strFields, ulOptions, strCollation, OUT phTable)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetDefault(strDefault AS STRING ) AS DWORD 
    RETURN Ace.AdsSetDefault(strDefault)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetSearchPath(strPath AS STRING ) AS DWORD 
    RETURN Ace.AdsSetSearchPath(strPath)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetFieldDecimals(hTable AS IntPtr , lFieldOrdinal AS DWORD, pusDecimals OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetFieldDecimals(hTable, lFieldOrdinal, OUT pusDecimals)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetFieldName(hTable AS IntPtr , usFld AS WORD , [IN] [OUT] strName AS CHAR[] , pusBufLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetFieldName(hTable, usFld, strName, REF pusBufLen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetNumFields(hTable AS IntPtr , pusCount OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetNumFields(hTable, OUT pusCount)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetField(hTable AS IntPtr , lFieldOrdinal AS DWORD, [IN] [OUT] abBuf AS BYTE[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD 
    RETURN Ace.AdsGetField(hTable, lFieldOrdinal, abBuf, REF pulLen, usOption)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetField(hTable AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD 
    RETURN Ace.AdsGetField(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsIsEmpty(hTable AS IntPtr , lFieldOrdinal AS DWORD, pbEmpty OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsEmpty(hTable, lFieldOrdinal, OUT pbEmpty)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsIsEmpty(hTable AS IntPtr , strFldName AS STRING , pbEmpty OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsEmpty(hTable, strFldName, OUT pbEmpty)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetString(hTable AS IntPtr , lFieldOrdinal AS DWORD, [IN] [OUT] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD) AS DWORD 
    RETURN Ace.AdsGetString(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetStringW(hTable AS IntPtr , lFieldOrdinal AS DWORD, [IN] [OUT] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD) AS DWORD 
    RETURN Ace.AdsGetString(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetJulian(hTable AS IntPtr , lFieldOrdinal AS DWORD, plDate OUT INT ) AS DWORD 
    RETURN Ace.AdsGetJulian(hTable, lFieldOrdinal, OUT plDate)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetJulian(hTable AS IntPtr , strFldName AS STRING , plDate OUT INT ) AS DWORD 
    RETURN Ace.AdsGetJulian(hTable, strFldName, OUT plDate)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetLogical(hTable AS IntPtr , lFieldOrdinal AS DWORD, pbValue OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetLogical(hTable, lFieldOrdinal, OUT pbValue)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetLogical(hTable AS IntPtr , strFldName AS STRING , pbValue OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetLogical(hTable, strFldName, OUT pbValue)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetEmpty(hObj AS IntPtr , lFieldOrdinal AS DWORD) AS DWORD 
    RETURN Ace.AdsSetEmpty(hObj, lFieldOrdinal)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetEmpty(hObj AS IntPtr , strFldName AS STRING ) AS DWORD 
    RETURN Ace.AdsSetEmpty(hObj, strFldName)
        
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetField(hObj AS IntPtr , lFieldOrdinal AS DWORD, abBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetField(hObj, lFieldOrdinal, abBuf, ulLen)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetField(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetField(hObj, lFieldOrdinal, strBuf, ulLen)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetString(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetString(hObj, lFieldOrdinal, strBuf, ulLen)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetString(hObj AS IntPtr , strFldName AS STRING , strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetString(hObj, strFldName, strBuf, ulLen)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetStringW(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetString(hObj, lFieldOrdinal, strBuf, ulLen)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetStringW(hObj AS IntPtr , strFldName AS STRING , strBuf AS STRING , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetString(hObj, strFldName, strBuf, ulLen)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, usBinaryType AS WORD , ulTotalLength AS DWORD , ulOffset AS DWORD , strBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
    RETURN Ace.AdsSetBinary(hTable, lFieldOrdinal, usBinaryType, ulTotalLength, ulOffSet, strBuf, ulLen)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetDouble(hObj AS IntPtr , lFieldOrdinal AS DWORD, dValue AS REAL8) AS DWORD 
    RETURN Ace.AdsSetDouble(hObj, lFieldOrdinal, dValue)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetJulian(hObj AS IntPtr , lFieldOrdinal AS DWORD, lDate AS INT) AS DWORD 
    RETURN Ace.AdsSetJulian(hObj, lFieldOrdinal, lDate)
        
FUNCTION AdsSetJulian(hObj AS IntPtr , strFldName AS STRING , lDate AS INT ) AS DWORD 
    RETURN Ace.AdsSetJulian(hObj, strFldName, lDate)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetLogical(hObj AS IntPtr , lFieldOrdinal AS DWORD, bValue AS WORD ) AS DWORD 
    RETURN Ace.AdsSetLogical(hObj, lFieldOrdinal, bValue)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSetLogical(hObj AS IntPtr , strFldName AS STRING , bValue AS WORD ) AS DWORD 
    RETURN Ace.AdsSetLogical(hObj, strFldName, bValue)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsReindex(hObject AS IntPtr) AS DWORD 
    RETURN Ace.AdsReindex(hObject)
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetIndexFilename(hIndex AS IntPtr , usOption AS WORD , [IN] [OUT] strName AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetIndexFilename(hIndex, usOption, strName , REF wLen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsOpenIndex(hTable AS IntPtr , strName AS STRING , [IN] [OUT] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD 
    RETURN Ace.AdsOpenIndex(hTable,  strName , ahIndex, REF pusArrayLen )
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCreateIndex(hObj AS IntPtr , strFileName AS STRING , strTag AS STRING , strExpr AS STRING , strCondition AS STRING , strWhile AS STRING , ulOptions AS DWORD, phIndex OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsCreateIndex(hObj,  strFileName , strTag, strExpr , strCondition , strWhile , ulOptions , OUT phIndex ) 

[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCreateIndex90(hObj AS IntPtr , strFileName AS STRING , strTag AS STRING , strExpr AS STRING , strCondition AS STRING , strWhile AS STRING , ulOptions AS DWORD, ulPageSize AS DWORD , strCollation AS STRING , phIndex OUT IntPtr ) AS DWORD 
    RETURN Ace.AdsCreateIndex90(hObj,  strFileName , strTag, strExpr , strCondition , strWhile , ulOptions , ulPageSize, strCollation, OUT phIndex ) 
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsRegisterCallbackFunction(pfn AS CallbackFn , ulCallBackID AS DWORD ) AS DWORD 
    RETURN Ace.AdsRegisterCallbackFunction(pfn,  ulCallBackID  )
        
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsExtractKey(hIndex AS IntPtr , [IN] [OUT] strKey AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsExtractKey(hIndex,  strKey , REF wLen )
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetAOFOptLevel(hTable AS IntPtr , pusOptLevel OUT WORD , [IN] [OUT] strNonOpt AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetAOFOptLevel(hTable,  OUT pusOptLevel , strNonOpt , REF wLen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetIndexCondition(hIndex AS IntPtr , [IN] [OUT] strExpr AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetIndexCondition(hIndex,  strExpr , REF wLen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetIndexExpr(hIndex AS IntPtr , [IN] [OUT] strExpr AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetIndexExpr(hIndex,  strExpr , REF wLen)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetIndexName(hIndex AS IntPtr , [IN] [OUT] strName AS CHAR[] , wLen REF WORD ) AS DWORD 
    RETURN Ace.AdsGetIndexName(hIndex,  strName , REF wLen)

[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetIndexOrderByHandle(hIndex AS IntPtr , pusIndexOrder OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetIndexOrderByHandle(hIndex,  OUT pusIndexOrder)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsSkipUnique(hIndex AS IntPtr , lRecs AS INT) AS DWORD 
    RETURN Ace.AdsSkipUnique(hIndex,  lRecs)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetKeyCount(hIndex AS IntPtr , usFilterOption AS WORD, pulCount OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetKeyCount(hIndex, usFilterOption, OUT pulCount)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetKeyLength(hIndex AS IntPtr , pusKeyLength OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetKeyLength(hIndex, OUT pusKeyLength)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetKeyNum(hIndex AS IntPtr , usFilterOption AS WORD, pulKey OUT DWORD ) AS DWORD 
    RETURN Ace.AdsGetKeyNum(hIndex, usFilterOption, OUT pulKey)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetKeyType(hIndex AS IntPtr , usKeyType OUT WORD ) AS DWORD 
    RETURN Ace.AdsGetKeyType(hIndex,  OUT usKeyType)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsGetRelKeyPos(hIndex AS IntPtr , pdPos OUT double ) AS DWORD 
    RETURN Ace.AdsGetRelKeyPos(hIndex,  OUT pdPos)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsIsIndexCustom(hIndex AS IntPtr , pbCustom OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsIndexCustom(hIndex,  OUT pbCustom)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsIsIndexDescending(hIndex AS IntPtr , pbDescending OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsIndexDescending(hIndex,  OUT pbDescending)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsIsIndexUnique(hIndex AS IntPtr , pbUnique OUT WORD ) AS DWORD 
    RETURN Ace.AdsIsIndexUnique(hIndex,  OUT pbUnique)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsCreateSQLStatement(hConnect as IntPtr, phStatement out IntPtr ) as DWORD 
    RETURN Ace.AdsCreateSQLStatement(hConnect,  OUT phStatement)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsStmtSetTableType(hStatement as IntPtr , usTableType as WORD) as DWORD 
    RETURN Ace.AdsStmtSetTableType(hStatement,  usTableType)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsStmtSetTableCharType(hStatement as IntPtr , usCharType as WORD ) as DWORD 
    RETURN Ace.AdsStmtSetTableCharType(hStatement,  usCharType)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsStmtSetTableCollation(hStatement as IntPtr , strCollation as string ) as DWORD 
    RETURN Ace.AdsStmtSetTableCollation(hStatement,  strCollation)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsStmtSetTableLockType(hStatement as IntPtr , usLockType as WORD ) as DWORD 
    RETURN Ace.AdsStmtSetTableLockType(hStatement,  usLockType)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsStmtSetTablePassword(hStatement as IntPtr , strTableName as string , strPassword as string ) as DWORD 
    RETURN Ace.AdsStmtSetTablePassword(hStatement,  strTableName, strPassword)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsStmtSetTableReadOnly(hStatement as IntPtr , usReadOnly as WORD ) as DWORD 
    RETURN Ace.AdsStmtSetTableReadOnly(hStatement,  usReadOnly)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsStmtSetTableRights(hStatement as IntPtr , usCheckRights as WORD ) as DWORD 
    RETURN Ace.AdsStmtSetTableRights(hStatement,  usCheckRights)
    
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION AdsExecuteSQLDirect(hStatement as IntPtr , strSQL as string , phCursor out IntPtr ) as DWORD 
    RETURN Ace.AdsExecuteSQLDirect(hStatement,  strSQL, OUT phCursor)
