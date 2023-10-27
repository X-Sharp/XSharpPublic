//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp.ADS
using System.Runtime.InteropServices
USING System.Runtime.CompilerServices

/// <summary>Return the AXS locking status.</summary>
///<returns>The current Advantage Locking setting.</returns>
FUNCTION AX_AXSLocking( ) AS LOGIC
    RETURN AX_RddHelper(_SET_AXSLOCKING, TRUE)

/// <summary>Return and set the AXS locking status.</summary>
/// <returns>The previous Advantage Locking setting.</returns>
/// <param name="bMode">The new Advantage Locking setting.</param>
FUNCTION AX_AXSLocking( bMode AS LOGIC) AS LOGIC
    RETURN AX_RddHelper(_SET_AXSLOCKING, bMode, TRUE)

/// <summary>Writes a BLOB contained in a memo field to a file.</summary>
/// <returns>Returns True (.T.) if the BLOB is written to a file, returns False (.F.) if not.</returns>
/// <param name="cFileName">File name to write to.</param>
/// <param name="cFieldName">Field name to read from.</param>
FUNCTION AX_BLOB2File( cFileName AS STRING, cFieldName AS STRING ) AS LOGIC
    LOCAL hTable AS DWORD
    LOCAL ulRetCode AS DWORD

    hTable := AX_GetAceTableHandle()
    ulRetCode := ACE.AdsBinaryToFile( hTable, cFieldName , cFileName )
    RETURN ulRetCode == 0

/// <summary>Stores the contents of a file into a memo field.</summary>
/// <returns>Returns True (.T.) if the file contents are copied into the memo field; returns False (.F.) if not.</returns>
/// <param name="cFileName">File name to read from.</param>
/// <param name="cFieldName">Field name to write to.</param>
FUNCTION AX_File2BLOB( cFileName AS STRING, cFieldName AS STRING ) AS LOGIC
    LOCAL hTable AS DWORD
    LOCAL ulRetCode AS DWORD
    hTable := AX_GetAceTableHandle()
    ulRetCode := ACE.AdsFileToBinary( hTable, cFieldName , ACE.ADS_BINARY , cFileName )
    RETURN ulRetCode == 0

/// <summary>Returns the Advantage Client Engine index handle that corresponds with the specified index in the current work area.
/// The handle can be used to call any Advantage Client Engine API directly.</summary>
/// <returns>An Advantage Client Engine index order handle or 0 if no index was found.</returns>
/// <param name="uIndexFile">filename or NIL</param>
/// <param name="uOrder">order name, number, or NIL</param>
FUNCTION AX_GetAceIndexHandle( uIndexFile AS OBJECT, uOrder AS OBJECT) AS IntPtr
    LOCAL oRet := NULL AS OBJECT
    IF CoreDb.OrderInfo(DBOI_GET_ACE_INDEX_HANDLE, "", uOrder, REF oRet)
        IF oRet IS IntPtr VAR pHandle
            RETURN pHandle
        ENDIF
    ENDIF
    RETURN 0

/// <summary>Returns the Advantage Client Engine index handle that corresponds with the specified index in the current work area.
/// The handle can be used to call any Advantage Client Engine API directly.</summary>
/// <returns>An Advantage Client Engine index order handle or 0 if no index was found.</returns>
FUNCTION GetAceIndexHandle() AS IntPtr
    RETURN AX_GetAceIndexHandle(NULL_OBJECT, NULL_OBJECT)


/// <summary>Returns the statement handle for the current workarea.  This handle can be used
/// to call the Advantage Client Engine directly.  Only for use with the AXSQL RDDs.</summary>
/// <returns> Returns a 0 if there is a problem.</returns>
FUNCTION AX_GetAceStmtHandle() AS IntPtr
    LOCAL oHandle := NULL AS OBJECT
    IF CoreDb.Info( DBI_GET_ACE_STMT_HANDLE , REF oHandle)
        IF oHandle IS IntPtr VAR pHandle
            RETURN pHandle
        ENDIF
    ENDIF
    RETURN 0

/// <summary>Returns the table handle for the current workarea.  This handle can be used to call the Advantage Client Engine directly.</summary>
/// <returns>Returns a 0 if there is a problem.</returns>
FUNCTION AX_GetAceTableHandle() AS IntPtr
    LOCAL oHandle := NULL AS OBJECT
    IF CoreDb.Info( DBI_GET_ACE_TABLE_HANDLE , REF oHandle)
        IF oHandle IS IntPtr VAR pHandle
            RETURN pHandle
        ENDIF
    ENDIF
    RETURN 0

/// <inheritdoc cref="AX_GetAceTableHandle" />
FUNCTION GetAceTableHandle() AS IntPtr
    RETURN AX_GetAceTableHandle()


/// <summary>Return .T. if Advantage is loaded on the specified location.</summary>
/// <remarks>cFileName must start with a drive letter ("X:\") or a UNC path ("\\server\volume\path\")</remarks>
/// <param name="cFileName">String containing drive letter or server name to check.
/// If the application uses a server name as the parameter, it must include the share or
/// volume name as well. For example, use "\\server\share" or "\\server\vol:"..</param>
/// <seealso cref="AdsIsServerLoaded">AdsIsServerLoaded Function</seealso>
FUNCTION AX_IsServerLoaded( cFileName AS STRING ) AS LOGIC //
    ACE.AdsIsServerLoaded  (  cFileName , OUT VAR usLoaded )
    RETURN ( usLoaded == ACE.ADS_REMOTE_SERVER  .OR. usLoaded = ACE.ADS_AIS_SERVER )

/// <summary>Indicates the percent completion of an index build</summary>
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

/// <summary>This function specifies which collation language to use when opening tables, opening cursors, or creating tables.</summary>
FUNCTION AX_SetCollation( strCollation AS STRING ) AS STRING
    LOCAL oldCollation := strCollation AS OBJECT
    CoreDb.RddInfo( _SET_COLLATION_NAME, REF oldCollation )
    RETURN (STRING) oldCollation

/// <summary>Sets the connection handle for all successive table opens.</summary>
/// <param name="ptrHandle">Connection handle that was returned by AdsConnect60</param>
/// <seealso cref="AdsConnect60">AdsConnect60 Function</seealso>
/// <seealso cref='AdsGetLastError'>AdsGetLastError FUNCTION</seealso>
PROCEDURE AX_SetConnectionHandle( ptrHandle AS IntPtr )
    CoreDb.RddInfo( _SET_CONNECTION_HANDLE, ptrHandle )
    RETURN


/// <summary>Returns the Exact Key Position Flag.</summary>
FUNCTION AX_SetExactKeyPos( ) AS LOGIC
    RETURN AX_RddHelper(_SET_EXACTKEYPOS, TRUE)

/// <summary>Returns and sets the Exact Key Position Flag.</summary>
FUNCTION AX_SetExactKeyPos( bMode AS LOGIC) AS LOGIC
    RETURN AX_RddHelper(_SET_EXACTKEYPOS, bMode, TRUE)


INTERNAL FUNCTION AX_RddHelper(iInfo AS INT, lDefault AS LOGIC) AS LOGIC
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



INTERNAL FUNCTION AX_RddHelper(iInfo AS INT, lNewValue AS LOGIC, lDefault AS LOGIC) AS LOGIC
    LOCAL bRetVal AS LOGIC
    bRetVal := AX_RddHelper(iInfo, lDefault)
    CoreDb.RddInfo( (DWORD) iInfo , lNewValue)
RETURN bRetVal


/// <summary>Sets the password to be used for subsequent encryption/decryption operations.</summary>
PROCEDURE AX_SetPassword( szEncodeKey AS STRING ) // Set password for record encryption
    IF String.IsNullOrEmpty(szEncodeKey)
        ACE.AdsDisableEncryption( AX_GetAceTableHandle() )
    ELSE
        ACE.AdsEnableEncryption( AX_GetAceTableHandle(), szEncodeKey  )
    ENDIF
    RETURN

/// <summary>Determines which type of Advantage server the client application can use.</summary>
FUNCTION AX_SetServerType( lUseRemoteServer AS LOGIC, lUseInternetServer AS LOGIC, lUseLocalServer AS LOGIC) AS LOGIC // determine which Advantage server to connect to
    LOCAL usServerTypes AS WORD
    LOCAL ulRetCode AS DWORD

    usServerTypes := 0
    IF lUseRemoteServer
        usServerTypes :=  _OR( usServerTypes, ACE.ADS_REMOTE_SERVER )
    ENDIF
    IF lUseInternetServer
        usServerTypes :=  _OR( usServerTypes, ACE.ADS_AIS_SERVER )
    ENDIF
    IF lUseLocalServer
        usServerTypes :=  _OR( usServerTypes, ACE.ADS_LOCAL_SERVER )
    ENDIF

    ulRetCode := ACE.AdsSetServerType( usServerTypes )

    RETURN  ulRetCode == 0


/// <summary>Sets passwords for encrypted tables for use with the AXSQL RDDs.</summary>
FUNCTION AX_SetSQLTablePasswords( aPasswords AS OBJECT ) AS VOID
    CoreDb.RddInfo( _SET_SQL_TABLE_PASSWORDS, aPasswords )
    RETURN

/// <summary>Begins, commits, rolls back, or shows the state of a transaction</summary>
/// <param name="iAction">The parameter can be: AX_BEGIN_TRANSACTION, AX_COMMIT_TRANSACTION, AX_ROLLBACK_TRANSACTION, AX_ISACTIVE_TRANSACTION</param>
/// <returns>True if the command was successful, False if not.</returns>
FUNCTION AX_Transaction( iAction AS INT) AS LOGIC // Transaction call
    LOCAL ulRetVal AS DWORD
    //
    // Transaction Processing function.  The parameter can be
    //   AX_BEGIN_TRANSACTION
    //   AX_COMMIT_TRANSACTION
    //   AX_ROLLBACK_TRANSACTION
    //   AX_ISACTIVE_TRANSACTION
    //

    SWITCH iAction
        CASE AX_BEGIN_TRANSACTION
            ulRetVal := ACE.AdsBeginTransaction( 0 )
        CASE AX_COMMIT_TRANSACTION
            ulRetVal := ACE.AdsCommitTransaction( 0 )
        CASE AX_ROLLBACK_TRANSACTION
            ulRetVal := ACE.AdsRollbackTransaction( 0 )
        CASE AX_ISACTIVE_TRANSACTION
            ulRetVal := ACE.AdsInTransaction( 0, OUT VAR usInTrans )
            RETURN ( ulRetVal == 0 .AND. usInTrans != 0 )
        OTHERWISE
            ulRetVal := 1
    END SWITCH

    RETURN  ulRetVal == 0


/// <summary>Is a transaction pending </summary>
FUNCTION AX_Transaction( ) AS LOGIC // Transaction call
    LOCAL ulRetVal AS DWORD
    ulRetVal := ACE.AdsInTransaction( 0, OUT VAR usInTrans )
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
        IF ulRetCode == 0
            ulRetCode := ACE.AdsGetConnectionType( ConnectionHandle, OUT usServerType )
        ENDIF
        IF ulRetCode == 0
            RETURN ( usServerType == ACE.ADS_REMOTE_SERVER ) .OR. ( usServerType == ACE.ADS_AIS_SERVER )
        ENDIF
    ENDIF
    RETURN FALSE


    #region Wrapper functions found in dbfaxs.prg in VO
/// <summary>Commits an active transaction on the given connection.</summary>
FUNCTION DBFAXSAdsCommitTransaction ( hConnect AS IntPtr ) AS DWORD
    RETURN ACE.AdsCommitTransaction(hConnect)

/// <summary>Determines if an Advantage server is available.</summary>
FUNCTION DBFAXSAdsIsServerLoaded ( strServer AS STRING, pbLoaded REF WORD ) AS DWORD
    RETURN ACE.AdsIsServerLoaded(strServer,OUT pbLoaded)

/// <summary>Begins a transaction for all connected servers or for the given server.</summary>
FUNCTION DBFAXSAdsBeginTransaction ( hConnect AS IntPtr ) AS DWORD
    RETURN ACE.AdsBeginTransaction(hConnect)

/// <summary>Rolls back the active transaction on the given connection.</summary>
FUNCTION DBFAXSAdsRollbackTransaction ( hConnect AS IntPtr ) AS DWORD
    RETURN ACE.AdsRollbackTransaction(hConnect)

/// <summary>Returns a flag to the caller to indicate if the given connection has an active transaction.</summary>
FUNCTION DBFAXSAdsInTransaction ( hConnect AS IntPtr, pbInTrans REF WORD ) AS DWORD
    RETURN ACE.AdsInTransaction(hConnect, OUT pbInTrans)

/// <summary>Stores the contents of the given file as a binary object in the specified field.</summary>
FUNCTION DBFAXSAdsFileToBinary ( hTbl AS IntPtr, pucFldName AS STRING, usBinaryType AS WORD, strFileName AS STRING ) AS DWORD
    RETURN ACE.AdsFileToBinary(hTbl, pucFldName, usBinaryType, strFileName)

/// <summary>Turns off Advantage encryption</summary>
FUNCTION DBFAXSAdsDisableEncryption( hTbl AS IntPtr ) AS DWORD
    RETURN ACE.AdsDisableEncryption(hTbl)

/// <summary>Turns on Advantage encryption</summary>
FUNCTION DBFAXSAdsEnableEncryption( hTbl AS IntPtr, strPassword AS STRING ) AS DWORD PASCAL
    RETURN ACE.AdsEnableEncryption(hTbl,strPassword)

/// <summary>Controls the types of Advantage Database Servers to which the client application can connect.</summary>
FUNCTION DBFAXSAdsSetServerType ( usServerOptions AS WORD ) AS DWORD
    RETURN ACE.AdsSetServerType(usServerOptions)

/// <summary>Returns the type of Advantage Database Server a connection uses.</summary>
FUNCTION DBFAXSAdsGetConnectionType ( hConnect AS IntPtr, pusConnectType REF WORD ) AS DWORD
    RETURN ACE.AdsGetConnectionType (hConnect, OUT pusConnectType)

/// <summary>Finds a connection handle associated with the server name.</summary>
FUNCTION DBFAXSAdsFindConnection ( strServerName AS STRING, phConnect REF IntPtr ) AS DWORD
    RETURN ACE.AdsFindConnection(strServerName,OUT phConnect)

/// <summary>Retrieves the binary object from the given field and stores it in the specified file.</summary>
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



/// <include file="ads.xml" path="functions/ads/*" />

FUNCTION AdsAddCustomKey(hIndex AS IntPtr ) AS DWORD
    RETURN ACE.AdsAddCustomKey(hIndex)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsAppendRecord(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsAppendRecord(hTable)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsApplicationExit() AS DWORD
    RETURN ACE.AdsApplicationExit()


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsAtBOF(hTable AS IntPtr , pbBof OUT WORD ) AS DWORD
    RETURN ACE.AdsAtBOF(hTable, OUT pbBof)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsAtEOF(hTable AS IntPtr , pbEof OUT WORD ) AS DWORD
    RETURN ACE.AdsAtEOF(hTable , OUT pbEof  )


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsBeginTransaction(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsBeginTransaction(hConnect)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsBinaryToFile(hTable AS IntPtr , strFldName AS STRING , strFileName AS STRING ) AS DWORD
    RETURN ACE.AdsBinaryToFile(hTable, strFldName , strFileName )


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsBinaryToFile(hTable AS IntPtr , lFieldOrdinal AS DWORD, strFileName AS STRING ) AS DWORD
    RETURN ACE.AdsBinaryToFile(hTable, lFieldOrdinal, strFileName)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCacheOpenCursors(usOpen AS WORD) AS DWORD
    RETURN ACE.AdsCacheOpenCursors(usOpen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCacheOpenTables(usOpen AS WORD) AS DWORD
    RETURN ACE.AdsCacheOpenTables(usOpen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCacheRecords(hTable AS IntPtr , usNumRecords AS WORD ) AS DWORD
    RETURN ACE.AdsCacheRecords(hTable, usNumRecords)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCancelUpdate(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsCancelUpdate(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsClearAllScopes(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsClearAllScopes(hTable)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsClearDefault() AS DWORD
    RETURN ACE.AdsClearDefault()


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsClearFilter(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsClearFilter(hTable)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsClearRelation(hTableParent AS IntPtr ) AS DWORD
    RETURN ACE.AdsClearRelation(hTableParent)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsClearScope(hIndex AS IntPtr , usScopeOption AS WORD) AS DWORD
    RETURN ACE.AdsClearScope(hIndex, usScopeOption)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCloneTable(hTable AS IntPtr , phClone OUT IntPtr ) AS DWORD
    RETURN ACE.AdsCloneTable(hTable, OUT phClone)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCloseAllIndexes(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsCloseAllIndexes(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCloseAllTables() AS DWORD
    RETURN ACE.AdsCloseAllTables()


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCloseIndex(hIndex AS IntPtr ) AS DWORD
    RETURN ACE.AdsCloseIndex(hIndex)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCloseTable(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsCloseTable(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCloseCachedTables(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsCloseCachedTables(hConnect)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCommitTransaction(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsCommitTransaction(hConnect)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsContinue(hTable AS IntPtr , pbFound OUT WORD) AS DWORD
    RETURN ACE.AdsContinue(hTable, OUT pbFound)


/// <include file="ads.xml" path="functions/ads/*" />
/// <seealso cref="AX_SetConnectionHandle">AX_SetConnectionHandle Function</seealso>
FUNCTION AdsConnect60(pucServerPath AS STRING , usServerTypes AS WORD, pucUserName AS STRING , pucPassword AS STRING , ulOptions AS DWORD, phConnect OUT IntPtr ) AS DWORD
    RETURN ACE.AdsConnect60(pucServerPath, usServerTypes, pucUserName, pucPassword, ulOptions, OUT phConnect)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCopyTableStructure(hTable AS IntPtr , strFile AS STRING ) AS DWORD
    RETURN ACE.AdsCopyTableStructure(hTable, strFile)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCreateFTSIndex(hTable AS IntPtr , strFileName AS STRING , strTag AS STRING , strField AS STRING , ulPageSize AS DWORD , ulMinWordLen AS DWORD , ulMaxWordLen AS DWORD , usUseDefaultDelim AS WORD , strDelimiters AS STRING , usUseDefaultNoise AS WORD , strNoiseWords AS STRING , usUseDefaultDrop AS WORD , strDropChars AS STRING , usUseDefaultConditionals AS WORD , strConditionalChars AS STRING , strReserved1 AS STRING , strReserved2 AS STRING , ulOptions AS DWORD) AS DWORD
    RETURN ACE.AdsCreateFTSIndex(hTable, strFileName, strTag, strField, ulPageSize , ulMinWordLen , ulMaxWordLen , usUseDefaultDelim , strDelimiters , usUseDefaultNoise , strNoiseWords , usUseDefaultDrop , strDropChars , usUseDefaultConditionals , strConditionalChars , strReserved1 , strReserved2 , ulOptions )


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsDecryptRecord(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsDecryptRecord(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsDecryptTable(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsDecryptTable(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsDeleteCustomKey(hIndex AS IntPtr ) AS DWORD
    RETURN ACE.AdsDeleteCustomKey(hIndex)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsDeleteIndex(hIndex AS IntPtr ) AS DWORD
    RETURN ACE.AdsDeleteIndex(hIndex)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsDeleteRecord(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsDeleteRecord(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsDisableEncryption(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsDisableEncryption(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsDisableLocalConnections() AS DWORD
    RETURN ACE.AdsDisableLocalConnections()


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsDisconnect(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsDisconnect(hConnect)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsEnableEncryption(hTable AS IntPtr , strPassword AS STRING ) AS DWORD
    RETURN ACE.AdsEnableEncryption(hTable, strPassword)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsEncryptRecord(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsEncryptRecord(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsEncryptTable(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsEncryptTable(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsEvalLogicalExpr(hTable AS IntPtr , strExpr AS STRING , pbResult OUT WORD ) AS DWORD
    RETURN ACE.AdsEvalLogicalExpr(hTable, strExpr, OUT pbResult)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsEvalNumericExpr(hTable AS IntPtr , strExpr AS STRING , pdResult OUT System.Double ) AS DWORD
    RETURN ACE.AdsEvalNumericExpr(hTable, strExpr, OUT pdResult)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsEvalStringExpr(hTable AS IntPtr , strExpr AS STRING , [InAttribute] [OutAttribute] strResult AS CHAR[] , wLen REF WORD ) AS DWORD
    RETURN ACE.AdsEvalStringExpr(hTable, strExpr, strResult, REF wLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsEvalTestExpr(hTable AS IntPtr , strExpr AS STRING , pusType OUT WORD ) AS DWORD
    RETURN ACE.AdsEvalTestExpr(hTable, strExpr, OUT pusType)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsFileToBinary(hTable AS IntPtr , strFldName AS STRING , usBinaryType AS WORD , strFileName AS STRING ) AS DWORD
    RETURN ACE.AdsFileToBinary(hTable, strFldName, usBinaryType, strFileName)

/// <include file="ads.xml" path="functions/ads/*" />

FUNCTION AdsFileToBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, usBinaryType AS WORD , strFileName AS STRING ) AS DWORD
    RETURN ACE.AdsFileToBinary(hTable, lFieldOrdinal, usBinaryType, strFileName)

/// <include file="ads.xml" path="functions/ads/*" />

FUNCTION AdsFindConnection(strServerName AS STRING , phConnect OUT IntPtr ) AS DWORD
    RETURN ACE.AdsFindConnection(strServerName, OUT phConnect)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetAllIndexes(hTable AS IntPtr , [InAttribute] [OutAttribute] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetAllIndexes(hTable, ahIndex, REF pusArrayLen)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetFTSIndexes(hTable AS IntPtr , [InAttribute] [OutAttribute] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetFTSIndexes(hTable, ahIndex, REF pusArrayLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetAllLocks(hTable AS IntPtr , [InAttribute] [OutAttribute] aulLocks AS DWORD[] , pusArrayLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetAllLocks(hTable, aulLocks, REF pusArrayLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetAllTables([InAttribute] [OutAttribute] ahTable AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetAllTables(ahTable, REF pusArrayLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, ulOffset AS DWORD , [InAttribute] [OutAttribute] strBuf AS BYTE[] , pulLen REF DWORD ) AS DWORD
    RETURN ACE.AdsGetBinary(hTable, lFieldOrdinal, ulOffset, strBuf, REF pulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetBinaryLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD
    RETURN ACE.AdsGetBinaryLength(hTable, lFieldOrdinal, OUT pulLength)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetBookmark(hTable AS IntPtr , phBookmark OUT IntPtr ) AS DWORD
    RETURN ACE.AdsGetBookmark(hTable, OUT phBookmark)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetConnectionType(hConnect AS IntPtr, pusConnectType OUT WORD ) AS DWORD
    RETURN ACE.AdsGetConnectionType(hConnect, OUT pusConnectType)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetDate(hTable AS IntPtr , lFieldOrdinal AS DWORD, [InAttribute] [OutAttribute] strBuf AS CHAR[] , wLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetDate(hTable, lFieldOrdinal, strBuf, REF wLen)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetDateFormat([InAttribute] [OutAttribute] strFormat AS CHAR[] , wLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetDateFormat(strFormat, REF wLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetDouble(hTable AS IntPtr , lFieldOrdinal AS DWORD, pdValue OUT REAL8 ) AS DWORD
    RETURN ACE.AdsGetDouble(hTable, lFieldOrdinal, OUT pdValue)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetDouble(hTable AS IntPtr , strFieldName AS STRING, pdValue OUT REAL8 ) AS DWORD
    RETURN ACE.AdsGetDouble(hTable, strFieldName, OUT pdValue)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetFieldLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD
    RETURN ACE.AdsGetFieldLength(hTable, lFieldOrdinal, OUT pulLength)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetFieldLength(hTable AS IntPtr , cField AS string, pulLength OUT DWORD ) AS DWORD
    RETURN ACE.AdsGetFieldLength(hTable, cField, OUT pulLength)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetFieldType(hTable AS IntPtr , lFieldOrdinal AS DWORD, pusType OUT WORD ) AS DWORD
    RETURN ACE.AdsGetFieldType(hTable, lFieldOrdinal, OUT pusType)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetFieldType(hTable AS IntPtr , cField AS string, pusType OUT WORD ) AS DWORD
    RETURN ACE.AdsGetFieldType(hTable, cField, OUT pusType)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetHandleType(hObj AS IntPtr , pusType OUT WORD ) AS DWORD
    RETURN ACE.AdsGetHandleType(hObj, OUT pusType)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetIndexHandle(hTable AS IntPtr , strIndexOrder AS STRING , phIndex OUT IntPtr ) AS DWORD
    RETURN ACE.AdsGetIndexHandle(hTable, strIndexOrder, OUT phIndex)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetLastError(pulErrCode OUT DWORD , [InAttribute] [OutAttribute] strBuf AS CHAR[] , pusBufLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetLastError(OUT pulErrCode, strBuf, REF pusBufLen)


/// <summary>Return the Last error message for an ADS operation.</summary>
/// <remarks>You have to call this immediately after an error occurs. Otherwise the error message may have been reset by a subsequent operation.</remarks>
FUNCTION AdsGetErrorMessage( ) AS STRING
    LOCAL strMessage AS STRING
    LOCAL message AS CHAR[]
    LOCAL wBufLen AS WORD
    message := CHAR[]{ACE.ADS_MAX_ERROR_LEN}
    wBufLen := (WORD) message:Length
    IF ACE.AdsGetLastError(OUT VAR _, message, REF wBufLen) == 0
        strMessage := STRING{message, 0, wBufLen}
    ELSE
        strMessage := "Unknown Error"
    ENDIF
    RETURN strMessage

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetMemoLength(hTable AS IntPtr , lFieldOrdinal AS DWORD, pulLength OUT DWORD ) AS DWORD
    RETURN ACE.AdsGetMemoLength(hTable, lFieldOrdinal, OUT pulLength)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetMemoLength(hTable AS IntPtr , cField as STRING , pulLength OUT DWORD ) AS DWORD
    RETURN ACE.AdsGetMemoLength(hTable, cField, OUT pulLength)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetMemoBlockSize(hTable AS IntPtr , pusBlockSize OUT WORD ) AS DWORD
    RETURN ACE.AdsGetMemoBlockSize(hTable, OUT pusBlockSize)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetNumLocks(hTable AS IntPtr , pusNum OUT WORD ) AS DWORD
    RETURN ACE.AdsGetNumLocks(hTable, OUT pusNum)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetRecordCount(hTable AS IntPtr , usFilterOption AS WORD, pulCount OUT DWORD) AS DWORD
    RETURN ACE.AdsGetRecordCount(hTable, usFilterOption, OUT pulCount)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetRecordNum(hTable AS IntPtr , usFilterOption AS WORD, pulRec OUT DWORD ) AS DWORD
    RETURN ACE.AdsGetRecordNum(hTable, usFilterOption , OUT pulRec)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetRecordCRC(hTable AS IntPtr , pulCRC OUT DWORD , ulOptions AS DWORD) AS DWORD
    RETURN ACE.AdsGetRecordCRC(hTable, OUT pulCRC, ulOptions)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetScope(hIndex AS IntPtr , usScopeOption AS WORD, [InAttribute] [OutAttribute] strScope AS CHAR[] , pusBufLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetScope(hIndex, usScopeOption, strScope, REF pusBufLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetTableOpenOptions(hTable AS IntPtr , pulOptions OUT DWORD ) AS DWORD
    RETURN ACE.AdsGetTableOpenOptions(hTable, OUT pulOptions)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetTableType(hTable AS IntPtr , pusType OUT WORD) AS DWORD
    RETURN ACE.AdsGetTableType(hTable, OUT pusType)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetLastTableUpdate(hTable AS IntPtr , [InAttribute] [OutAttribute] strDate AS CHAR[] , pusDateLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetLastTableUpdate(hTable , strDate , REF pusDateLen )


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGotoBottom(hObj AS IntPtr ) AS DWORD
    RETURN ACE.AdsGotoBottom(hObj)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGotoTop(hObj AS IntPtr ) AS DWORD
    RETURN ACE.AdsGotoTop(hObj)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGotoRecord(hObj AS IntPtr, nRecord AS DWORD ) AS DWORD
    RETURN ACE.AdsGotoRecord(hObj, nRecord)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsInTransaction(hConnect AS IntPtr, pbInTrans OUT WORD ) AS DWORD
    RETURN ACE.AdsInTransaction(hConnect, OUT pbInTrans)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsIsFound(hObj AS IntPtr , pbFound OUT WORD) AS DWORD
    RETURN ACE.AdsIsFound(hObj, OUT pbFound)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsIsTableLocked(hTable AS IntPtr , pbLocked OUT WORD ) AS DWORD
    RETURN ACE.AdsIsTableLocked(hTable, OUT pbLocked)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsIsRecordLocked(hTable AS IntPtr , ulRec AS DWORD , pbLocked OUT WORD ) AS DWORD
    RETURN ACE.AdsIsRecordLocked(hTable, ulRec, OUT pbLocked)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsIsRecordVisible(hObj AS IntPtr , pbVisible OUT WORD ) AS DWORD
    RETURN ACE.AdsIsRecordVisible(hObj, OUT pbVisible)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsIsServerLoaded(strServer AS STRING , pbLoaded OUT WORD ) AS DWORD
    RETURN ACE.AdsIsServerLoaded(strServer, OUT pbLoaded)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsIsRecordDeleted(hTable AS IntPtr , pbDeleted OUT WORD ) AS DWORD
    RETURN ACE.AdsIsRecordDeleted(hTable, OUT pbDeleted)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsLockRecord(hTable AS IntPtr , ulRec AS DWORD ) AS DWORD
    RETURN ACE.AdsLockRecord(hTable, ulRec)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsLockTable(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsLockTable(hTable)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsPackTable(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsPackTable(hTable)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsRecallRecord(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsRecallRecord(hTable)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsRecallAllRecords(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsRecallAllRecords(hTable)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsRefreshRecord(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsRefreshRecord(hTable)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsClearProgressCallback() AS DWORD
    RETURN ACE.AdsClearProgressCallback()

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsClearCallbackFunction() AS DWORD
    RETURN ACE.AdsClearCallbackFunction()

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsResetConnection(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsResetConnection(hConnect)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsRollbackTransaction(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsRollbackTransaction(hConnect)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSeek(hIndex AS IntPtr , strKey AS STRING , usKeyLen AS WORD, usDataType AS WORD, usSeekType AS WORD, pbFound OUT WORD) AS DWORD
    RETURN ACE.AdsSeek(hIndex, strKey, usKeyLen, usDataType, usSeekType, OUT pbFound)

/// <include file="ads.xml" path="functions/ads/*" />

FUNCTION AdsSeek(hIndex AS IntPtr , abKey AS BYTE[] , usKeyLen AS WORD, usDataType AS WORD, usSeekType AS WORD, pbFound OUT WORD) AS DWORD
    RETURN ACE.AdsSeek(hIndex, abKey, usKeyLen, usDataType, usSeekType, OUT pbFound)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSeekLast(hIndex AS IntPtr , strKey AS STRING , usKeyLen AS WORD, usDataType AS WORD, pbFound OUT WORD) AS DWORD
    RETURN ACE.AdsSeekLast(hIndex, strKey, usKeyLen, usDataType, OUT pbFound)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSeekLast(hIndex AS IntPtr , abKey AS BYTE[] , usKeyLen AS WORD, usDataType AS WORD, pbFound OUT WORD) AS DWORD
    RETURN ACE.AdsSeekLast(hIndex, abKey, usKeyLen, usDataType, OUT pbFound)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetDateFormat(strFormat AS STRING ) AS DWORD
    RETURN ACE.AdsSetDateFormat(strFormat)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetDecimals(usDecimals AS WORD ) AS DWORD
    RETURN ACE.AdsSetDecimals(usDecimals)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsShowDeleted(bShowDeleted AS WORD ) AS DWORD
    RETURN ACE.AdsShowDeleted(bShowDeleted)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetEpoch(usCentury AS WORD ) AS DWORD
    RETURN ACE.AdsSetEpoch(usCentury)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetExact(bExact AS WORD ) AS DWORD
    RETURN ACE.AdsSetExact(bExact)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetFilter(hTable AS IntPtr , strFilter AS STRING ) AS DWORD
    RETURN ACE.AdsSetFilter(hTable, strFilter)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetRelation(hTableParent AS IntPtr , hIndexChild AS IntPtr , strExpr AS STRING ) AS DWORD
    RETURN ACE.AdsSetRelation(hTableParent, hIndexChild, strExpr)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetServerType(usServerOptions AS WORD ) AS DWORD
    RETURN ACE.AdsSetServerType(usServerOptions)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetScope(hIndex AS IntPtr , usScopeOption AS WORD, strScope AS STRING , usScopeLen AS WORD , usDataType AS WORD) AS DWORD
    RETURN ACE.AdsSetScope(hIndex, usScopeOption, strScope, usScopeLen, usDataType)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetScope(hIndex AS IntPtr , usScopeOption AS WORD, abScope AS BYTE[] , usScopeLen AS WORD , usDataType AS WORD) AS DWORD
    RETURN ACE.AdsSetScope(hIndex, usScopeOption, abScope, usScopeLen, usDataType)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSkip(hObj AS IntPtr , lRecs AS INT) AS DWORD
    RETURN ACE.AdsSkip(hObj, lRecs)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsThreadExit() AS DWORD
    RETURN ACE.AdsThreadExit()


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsUnlockRecord(hTable AS IntPtr , ulRec AS DWORD) AS DWORD
    RETURN ACE.AdsUnlockRecord(hTable, ulRec)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsUnlockTable(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsUnlockTable(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsWriteAllRecords() AS DWORD
    RETURN ACE.AdsWriteAllRecords()


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsWriteRecord(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsWriteRecord(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsZapTable(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsZapTable(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetAOF(hTable AS IntPtr , strFilter AS STRING , usOptions AS WORD ) AS DWORD
    RETURN ACE.AdsSetAOF(hTable, strFilter, usOptions)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsClearAOF(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsClearAOF(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsRefreshAOF(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsRefreshAOF(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsInitRawKey(hIndex AS IntPtr ) AS DWORD
    RETURN ACE.AdsInitRawKey(hIndex)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsExecuteSQL(hStatement AS IntPtr , phCursor OUT IntPtr ) AS DWORD
    RETURN ACE.AdsExecuteSQL(hStatement, OUT phCursor)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCloseSQLStatement(hStatement AS IntPtr ) AS DWORD
    RETURN ACE.AdsCloseSQLStatement(hStatement)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsStmtDisableEncryption(hStatement AS IntPtr ) AS DWORD
    RETURN ACE.AdsStmtDisableEncryption(hStatement)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsStmtClearTablePasswords(hStatement AS IntPtr ) AS DWORD
    RETURN ACE.AdsStmtClearTablePasswords(hStatement)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsClearSQLParams(hStatement AS IntPtr ) AS DWORD
    RETURN ACE.AdsClearSQLParams(hStatement)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsClearSQLAbortFunc() AS DWORD
    RETURN ACE.AdsClearSQLAbortFunc()


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsFlushFileBuffers(hTable AS IntPtr ) AS DWORD
    RETURN ACE.AdsFlushFileBuffers(hTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsDisableUniqueEnforcement(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsDisableUniqueEnforcement(hConnect)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsEnableUniqueEnforcement(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsEnableUniqueEnforcement(hConnect)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsDisableRI(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsDisableRI(hConnect)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsEnableRI(hConnect AS IntPtr) AS DWORD
    RETURN ACE.AdsEnableRI(hConnect)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsDisableAutoIncEnforcement(hConnection AS IntPtr) AS DWORD
    RETURN ACE.AdsDisableAutoIncEnforcement(hConnection)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsEnableAutoIncEnforcement(hConnection AS IntPtr) AS DWORD
    RETURN ACE.AdsEnableAutoIncEnforcement(hConnection)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetIndexHandleByOrder(hTable AS IntPtr , usOrderNum AS WORD , phIndex OUT IntPtr ) AS DWORD
    RETURN ACE.AdsGetIndexHandleByOrder(hTable, usOrderNum, OUT phIndex)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetNumIndexes(hTable AS IntPtr , pusNum OUT WORD ) AS DWORD
    RETURN ACE.AdsGetNumIndexes(hTable, OUT pusNum)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetRecordLength(hTable AS IntPtr , pulLength OUT DWORD ) AS DWORD
    RETURN ACE.AdsGetRecordLength(hTable, OUT pulLength)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetTableFilename(hTable AS IntPtr , usOption AS WORD , strName AS CHAR[] , wLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetTableFilename(hTable, usOption, strName , REF wLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsOpenTable90(hConnect AS IntPtr, strName AS STRING , strAlias AS STRING , usTableType AS WORD, usCharType AS WORD , usLockType AS WORD , usCheckRights AS WORD , ulOptions AS DWORD, strCollation AS STRING , phTable OUT IntPtr ) AS DWORD
    RETURN ACE.AdsOpenTable90(hConnect, strName, strAlias, usTableType, usCharType, usLockType, usCheckRights, ulOptions, strCollation, OUT phTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCreateTable90(hConnect AS IntPtr, strName AS STRING , strDBObjName AS STRING , usTableType AS WORD, usCharType AS WORD , usLockType AS WORD , usCheckRights AS WORD , usMemoSize AS WORD , strFields AS STRING , ulOptions AS DWORD, strCollation AS STRING , phTable OUT IntPtr ) AS DWORD
    RETURN ACE.AdsCreateTable90(hConnect, strName, strDBObjName, usTableType, usCharType, usLockType, usCheckRights, usMemoSize, strFields, ulOptions, strCollation, OUT phTable)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetDefault(strDefault AS STRING ) AS DWORD
    RETURN ACE.AdsSetDefault(strDefault)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetSearchPath(strPath AS STRING ) AS DWORD
    RETURN ACE.AdsSetSearchPath(strPath)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetFieldDecimals(hTable AS IntPtr , lFieldOrdinal AS DWORD, pusDecimals OUT WORD ) AS DWORD
    RETURN ACE.AdsGetFieldDecimals(hTable, lFieldOrdinal, OUT pusDecimals)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetFieldDecimals(hTable AS IntPtr , cField as STRING, pusDecimals OUT WORD ) AS DWORD
    RETURN ACE.AdsGetFieldDecimals(hTable, cField, OUT pusDecimals)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetFieldName(hTable AS IntPtr , usFld AS WORD , [InAttribute] [OutAttribute] strName AS CHAR[] , pusBufLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetFieldName(hTable, usFld, strName, REF pusBufLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetNumFields(hTable AS IntPtr , pusCount OUT WORD ) AS DWORD
    RETURN ACE.AdsGetNumFields(hTable, OUT pusCount)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetField(hTable AS IntPtr , lFieldOrdinal AS DWORD, [InAttribute] [OutAttribute] abBuf AS BYTE[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD
    RETURN ACE.AdsGetField(hTable, lFieldOrdinal, abBuf, REF pulLen, usOption)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetField(hTable AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD
    RETURN ACE.AdsGetField(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsIsEmpty(hTable AS IntPtr , lFieldOrdinal AS DWORD, pbEmpty OUT WORD ) AS DWORD
    RETURN ACE.AdsIsEmpty(hTable, lFieldOrdinal, OUT pbEmpty)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsIsEmpty(hTable AS IntPtr , strFldName AS STRING , pbEmpty OUT WORD ) AS DWORD
    RETURN ACE.AdsIsEmpty(hTable, strFldName, OUT pbEmpty)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetString(hTable AS IntPtr , lFieldOrdinal AS DWORD, [InAttribute] [OutAttribute] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD) AS DWORD
    RETURN ACE.AdsGetString(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetString(hTable AS IntPtr , strFieldName AS STRING, [InAttribute] [OutAttribute] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD
    RETURN ACE.AdsGetString(hTable, strFieldName, strBuf, REF pulLen, usOption)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetStringW(hTable AS IntPtr , lFieldOrdinal AS DWORD, [InAttribute] [OutAttribute] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD) AS DWORD
    RETURN ACE.AdsGetString(hTable, lFieldOrdinal, strBuf, REF pulLen, usOption)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetStringW(hTable AS IntPtr , strFieldName AS STRING, [InAttribute] [OutAttribute] strBuf AS CHAR[] , pulLen REF DWORD , usOption AS WORD ) AS DWORD
    RETURN ACE.AdsGetStringW(hTable, strFieldName,  strBuf, REF pulLen, usOption)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetJulian(hTable AS IntPtr , lFieldOrdinal AS DWORD, plDate OUT INT ) AS DWORD
    RETURN ACE.AdsGetJulian(hTable, lFieldOrdinal, OUT plDate)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetJulian(hTable AS IntPtr , strFldName AS STRING , plDate OUT INT ) AS DWORD
    RETURN ACE.AdsGetJulian(hTable, strFldName, OUT plDate)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetLogical(hTable AS IntPtr , lFieldOrdinal AS DWORD, pbValue OUT WORD ) AS DWORD
    RETURN ACE.AdsGetLogical(hTable, lFieldOrdinal, OUT pbValue)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetLogical(hTable AS IntPtr , strFldName AS STRING , pbValue OUT WORD ) AS DWORD
    RETURN ACE.AdsGetLogical(hTable, strFldName, OUT pbValue)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetEmpty(hObj AS IntPtr , lFieldOrdinal AS DWORD) AS DWORD
    RETURN ACE.AdsSetEmpty(hObj, lFieldOrdinal)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetEmpty(hObj AS IntPtr , strFldName AS STRING ) AS DWORD
    RETURN ACE.AdsSetEmpty(hObj, strFldName)



/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetField(hObj AS IntPtr , lFieldOrdinal AS DWORD, abBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD
    RETURN ACE.AdsSetField(hObj, lFieldOrdinal, abBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetField(hObj AS IntPtr , strFldName AS STRING, abBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD
    RETURN ACE.AdsSetField(hObj, strFldName, abBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetField(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD
    RETURN ACE.AdsSetField(hObj, lFieldOrdinal, strBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetField(hObj AS IntPtr , strFldName AS STRING, strBuf AS STRING , ulLen AS DWORD ) AS DWORD
    RETURN ACE.AdsSetField(hObj, strFldName, strBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetString(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD
    RETURN ACE.AdsSetString(hObj, lFieldOrdinal, strBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetString(hObj AS IntPtr , strFldName AS STRING , strBuf AS STRING , ulLen AS DWORD ) AS DWORD
    RETURN ACE.AdsSetString(hObj, strFldName, strBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetStringW(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD ) AS DWORD
    RETURN ACE.AdsSetString(hObj, lFieldOrdinal, strBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetStringW(hObj AS IntPtr , strFldName AS STRING , strBuf AS STRING , ulLen AS DWORD ) AS DWORD
    RETURN ACE.AdsSetString(hObj, strFldName, strBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetBinary(hTable AS IntPtr , lFieldOrdinal AS DWORD, usBinaryType AS WORD , ulTotalLength AS DWORD , ulOffset AS DWORD , strBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD
    RETURN ACE.AdsSetBinary(hTable, lFieldOrdinal, usBinaryType, ulTotalLength, ulOffset, strBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetBinary(hTable AS IntPtr , strFldName AS STRING , usBinaryType AS WORD , ulTotalLength AS DWORD , ulOffset AS DWORD , strBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD
    RETURN ACE.AdsSetBinary(hTable, strFldName, usBinaryType, ulTotalLength, ulOffset, strBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetDouble(hObj AS IntPtr , lFieldOrdinal AS DWORD, dValue AS REAL8) AS DWORD
    RETURN ACE.AdsSetDouble(hObj, lFieldOrdinal, dValue)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetDouble(hObj AS IntPtr , strFldName AS STRING, dValue AS REAL8) AS DWORD
    RETURN ACE.AdsSetDouble(hObj, strFldName, dValue)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetJulian(hObj AS IntPtr , lFieldOrdinal AS DWORD, lDate AS INT) AS DWORD
    RETURN ACE.AdsSetJulian(hObj, lFieldOrdinal, lDate)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetJulian(hObj AS IntPtr , strFldName AS STRING , lDate AS INT ) AS DWORD
    RETURN ACE.AdsSetJulian(hObj, strFldName, lDate)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetLogical(hObj AS IntPtr , lFieldOrdinal AS DWORD, bValue AS WORD ) AS DWORD
    RETURN ACE.AdsSetLogical(hObj, lFieldOrdinal, bValue)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetLogical(hObj AS IntPtr , strFldName AS STRING , bValue AS WORD ) AS DWORD
    RETURN ACE.AdsSetLogical(hObj, strFldName, bValue)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetMoney(hObj AS IntPtr , lFieldOrdinal AS DWORD, qValue AS INT64 ) AS DWORD
    RETURN ACE.AdsSetMoney(hObj, lFieldOrdinal, qValue)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetMoney(hObj AS IntPtr , strFldName AS STRING , qValue AS INT64 ) AS DWORD
    RETURN ACE.AdsSetMoney(hObj, strFldName, qValue)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetMilliseconds(hObj AS IntPtr , lFieldOrdinal AS DWORD, lTime AS INT ) AS DWORD
    RETURN ACE.AdsSetMilliseconds(hObj, lFieldOrdinal, lTime)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetMilliseconds(hObj AS IntPtr , strFldName AS STRING , lTime AS INT ) AS DWORD
    RETURN ACE.AdsSetMilliseconds(hObj, strFldName, lTime)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetNull(hObj AS IntPtr , lFieldOrdinal AS DWORD ) AS DWORD
    RETURN ACE.AdsSetNull(hObj, lFieldOrdinal)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetNull(hObj AS IntPtr , strFldName AS STRING  ) AS DWORD
    RETURN ACE.AdsSetNull(hObj, strFldName)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetShort(hObj AS IntPtr , lFieldOrdinal AS DWORD, sValue AS SHORT ) AS DWORD
    RETURN ACE.AdsSetShort(hObj, lFieldOrdinal, sValue)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetShort(hObj AS IntPtr , strFldName AS STRING , sValue AS SHORT ) AS DWORD
    RETURN ACE.AdsSetShort(hObj, strFldName, sValue)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetTime(hObj AS IntPtr , lFieldOrdinal AS DWORD, strValue AS STRING , wLen AS WORD  ) AS DWORD
    RETURN ACE.AdsSetTime(hObj, lFieldOrdinal, strValue, wLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetTime(hObj AS IntPtr , strFldName AS STRING , strValue AS STRING , wLen AS WORD ) AS DWORD
    RETURN ACE.AdsSetTime(hObj, strFldName, strValue, wLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetTimeStamp(hObj AS IntPtr , lFieldOrdinal AS DWORD, strBuf AS STRING , ulLen AS DWORD   ) AS DWORD
    RETURN ACE.AdsSetTimeStamp(hObj, lFieldOrdinal, strBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSetTimeStamp(hObj AS IntPtr , strFldName AS STRING , strBuf AS STRING , ulLen AS DWORD  ) AS DWORD
    RETURN ACE.AdsSetTimeStamp(hObj, strFldName, strBuf, ulLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsReindex(hObj AS IntPtr) AS DWORD
    RETURN ACE.AdsReindex(hObj)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetIndexFilename(hIndex AS IntPtr , usOption AS WORD , [InAttribute] [OutAttribute] strName AS CHAR[] , wLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetIndexFilename(hIndex, usOption, strName , REF wLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsOpenIndex(hTable AS IntPtr , strName AS STRING , [InAttribute] [OutAttribute] ahIndex AS IntPtr[] , pusArrayLen REF WORD ) AS DWORD
    RETURN ACE.AdsOpenIndex(hTable,  strName , ahIndex, REF pusArrayLen )


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCreateIndex(hObj AS IntPtr , strFileName AS STRING , strTag AS STRING , strExpr AS STRING , strCondition AS STRING , strWhile AS STRING , ulOptions AS DWORD, phIndex OUT IntPtr ) AS DWORD
    RETURN ACE.AdsCreateIndex(hObj,  strFileName , strTag, strExpr , strCondition , strWhile , ulOptions , OUT phIndex )


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCreateIndex90(hObj AS IntPtr , strFileName AS STRING , strTag AS STRING , strExpr AS STRING , strCondition AS STRING , strWhile AS STRING , ulOptions AS DWORD, ulPageSize AS DWORD , strCollation AS STRING , phIndex OUT IntPtr ) AS DWORD
    RETURN ACE.AdsCreateIndex90(hObj,  strFileName , strTag, strExpr , strCondition , strWhile , ulOptions , ulPageSize, strCollation, OUT phIndex )


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsRegisterCallbackFunction(pfn AS CallbackFn , ulCallBackID AS DWORD ) AS DWORD
    RETURN ACE.AdsRegisterCallbackFunction(pfn,  ulCallBackID  )


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsExtractKey(hIndex AS IntPtr , [InAttribute] [OutAttribute] strKey AS CHAR[] , wLen REF WORD ) AS DWORD
    RETURN ACE.AdsExtractKey(hIndex,  strKey , REF wLen )


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetAOFOptLevel(hTable AS IntPtr , pusOptLevel OUT WORD , [InAttribute] [OutAttribute] strNonOpt AS CHAR[] , wLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetAOFOptLevel(hTable,  OUT pusOptLevel , strNonOpt , REF wLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetIndexCondition(hIndex AS IntPtr , [InAttribute] [OutAttribute] strExpr AS CHAR[] , wLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetIndexCondition(hIndex,  strExpr , REF wLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetIndexExpr(hIndex AS IntPtr , [InAttribute] [OutAttribute] strExpr AS CHAR[] , wLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetIndexExpr(hIndex,  strExpr , REF wLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetIndexName(hIndex AS IntPtr , [InAttribute] [OutAttribute] strName AS CHAR[] , wLen REF WORD ) AS DWORD
    RETURN ACE.AdsGetIndexName(hIndex,  strName , REF wLen)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetIndexOrderByHandle(hIndex AS IntPtr , pusIndexOrder OUT WORD ) AS DWORD
    RETURN ACE.AdsGetIndexOrderByHandle(hIndex,  OUT pusIndexOrder)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsSkipUnique(hIndex AS IntPtr , lRecs AS INT) AS DWORD
    RETURN ACE.AdsSkipUnique(hIndex,  lRecs)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetKeyCount(hIndex AS IntPtr , usFilterOption AS WORD, pulCount OUT DWORD ) AS DWORD
    RETURN ACE.AdsGetKeyCount(hIndex, usFilterOption, OUT pulCount)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetKeyLength(hIndex AS IntPtr , pusKeyLength OUT WORD ) AS DWORD
    RETURN ACE.AdsGetKeyLength(hIndex, OUT pusKeyLength)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetKeyNum(hIndex AS IntPtr , usFilterOption AS WORD, pulKey OUT DWORD ) AS DWORD
    RETURN ACE.AdsGetKeyNum(hIndex, usFilterOption, OUT pulKey)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetKeyType(hIndex AS IntPtr , usKeyType OUT WORD ) AS DWORD
    RETURN ACE.AdsGetKeyType(hIndex,  OUT usKeyType)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsGetRelKeyPos(hIndex AS IntPtr , pdPos OUT System.Double ) AS DWORD
    RETURN ACE.AdsGetRelKeyPos(hIndex,  OUT pdPos)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsIsIndexCustom(hIndex AS IntPtr , pbCustom OUT WORD ) AS DWORD
    RETURN ACE.AdsIsIndexCustom(hIndex,  OUT pbCustom)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsIsIndexDescending(hIndex AS IntPtr , pbDescending OUT WORD ) AS DWORD
    RETURN ACE.AdsIsIndexDescending(hIndex,  OUT pbDescending)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsIsIndexUnique(hIndex AS IntPtr , pbUnique OUT WORD ) AS DWORD
    RETURN ACE.AdsIsIndexUnique(hIndex,  OUT pbUnique)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsCreateSQLStatement(hConnect AS IntPtr, phStatement OUT IntPtr ) AS DWORD
    RETURN ACE.AdsCreateSQLStatement(hConnect,  OUT phStatement)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsStmtSetTableType(hStatement AS IntPtr , usTableType AS WORD) AS DWORD
    RETURN ACE.AdsStmtSetTableType(hStatement,  usTableType)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsStmtSetTableCharType(hStatement AS IntPtr , usCharType AS WORD ) AS DWORD
    RETURN ACE.AdsStmtSetTableCharType(hStatement,  usCharType)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsStmtSetTableCollation(hStatement AS IntPtr , strCollation AS STRING ) AS DWORD
    RETURN ACE.AdsStmtSetTableCollation(hStatement,  strCollation)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsStmtSetTableLockType(hStatement AS IntPtr , usLockType AS WORD ) AS DWORD
    RETURN ACE.AdsStmtSetTableLockType(hStatement,  usLockType)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsStmtSetTablePassword(hStatement AS IntPtr , strTableName AS STRING , strPassword AS STRING ) AS DWORD
    RETURN ACE.AdsStmtSetTablePassword(hStatement,  strTableName, strPassword)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsStmtSetTableReadOnly(hStatement AS IntPtr , usReadOnly AS WORD ) AS DWORD
    RETURN ACE.AdsStmtSetTableReadOnly(hStatement,  usReadOnly)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsStmtSetTableRights(hStatement AS IntPtr , usCheckRights AS WORD ) AS DWORD
    RETURN ACE.AdsStmtSetTableRights(hStatement,  usCheckRights)


/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsExecuteSQLDirect(hStatement AS IntPtr , strSQL AS STRING , phCursor OUT IntPtr ) AS DWORD
    RETURN ACE.AdsExecuteSQLDirect(hStatement,  strSQL, OUT phCursor)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgConnect( pucServerName AS STRING, pucUserName AS STRING, pucPassword AS STRING, phMgmtHandle OUT IntPtr ) AS DWORD
    RETURN ACE.AdsMgConnect(pucServerName, pucUserName, pucPassword, OUT phMgmtHandle)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgDisconnect( hMgmtHandle AS IntPtr ) AS DWORD
    RETURN ACE.AdsMgDisconnect( hMgmtHandle)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgDumpInternalTables( hMgmtHandle AS IntPtr ) AS DWORD
    RETURN ACE.AdsMgDumpInternalTables( hMgmtHandle)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgGetActivityInfo( hMgmtHandle AS IntPtr, pstActivityInfo AS IntPtr, pusStructSize REF WORD ) AS DWORD
    RETURN ACE.AdsMgGetActivityInfo( hMgmtHandle, pstActivityInfo, REF pusStructSize)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgGetCommStats( hMgmtHandle AS IntPtr, pstCommStats AS IntPtr, pusStructSize REF WORD ) AS DWORD
    RETURN ACE32.AdsMgGetCommStats ( hMgmtHandle, pstCommStats, REF pusStructSize)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgGetConfigInfo( hMgmtHandle AS IntPtr, pstConfigValues AS IntPtr, pusConfigValuesStructSize REF WORD, pstConfigMemory AS IntPtr, pusConfigMemoryStructSize REF WORD ) AS DWORD
    RETURN ACE32.AdsMgGetConfigInfo( hMgmtHandle, pstConfigValues, REF pusConfigValuesStructSize, pstConfigMemory, REF pusConfigMemoryStructSize)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgGetInstallInfo( hMgmtHandle AS IntPtr, pstInstallInfo AS IntPtr, pusStructSize REF WORD ) AS DWORD
    RETURN ACE.AdsMgGetInstallInfo(  hMgmtHandle, pstInstallInfo, REF pusStructSize)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgGetLockOwner( hMgmtHandle AS IntPtr, pucTableName AS STRING, ulRecordNumber AS DWORD, pstUserInfo AS IntPtr, pusStructSize REF WORD, pusLockType REF WORD ) AS DWORD
    RETURN ACE.AdsMgGetLockOwner( hMgmtHandle, pucTableName, ulRecordNumber, pstUserInfo, REF pusStructSize, REF pusLockType)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgGetLocks( hMgmtHandle AS IntPtr, pucTableName AS STRING, pucUserName AS STRING, usConnNumber AS WORD, astRecordInfo AS IntPtr, pusArrayLen REF WORD, pusStructSize REF WORD ) AS DWORD
    RETURN ACE.AdsMgGetLocks( hMgmtHandle, pucTableName,  pucUserName, usConnNumber, astRecordInfo, REF pusArrayLen, REF pusStructSize)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgGetOpenIndexes( hMgmtHandle AS IntPtr, pucTableName AS STRING, pucUserName AS STRING, usConnNumber AS WORD, astOpenIndexInfo AS IntPtr, pusArrayLen REF WORD, pusStructSize REF WORD ) AS DWORD
    RETURN ACE.AdsMgGetOpenIndexes( hMgmtHandle, pucTableName, pucUserName , usConnNumber, astOpenIndexInfo, REF pusArrayLen, REF pusStructSize)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgGetOpenTables( hMgmtHandle AS IntPtr, pucUserName AS STRING, usConnNumber AS WORD, astOpenTableInfo AS IntPtr, pusArrayLen REF WORD, pusStructSize REF WORD ) AS DWORD
    RETURN ACE.AdsMgGetOpenTables( hMgmtHandle, pucUserName, usConnNumber, astOpenTableInfo, REF pusArrayLen, REF pusStructSize)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgGetServerType( hMgmtHandle AS IntPtr, pusServerType REF WORD ) AS DWORD
    RETURN ACE.AdsMgGetServerType( hMgmtHandle, REF pusServerType)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgGetUserNames( hMgmtHandle AS IntPtr, pucFileName AS STRING, astUserInfo AS IntPtr, pusArrayLen REF WORD, pusStructSize REF WORD ) AS DWORD
    RETURN ACE.AdsMgGetUserNames(hMgmtHandle, pucFileName, astUserInfo, REF pusArrayLen , REF pusStructSize)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgGetWorkerThreadActivity( hMgmtHandle AS IntPtr, astWorkerThreadActivity AS IntPtr, pusArrayLen REF WORD, pusStructSize REF WORD ) AS DWORD
    RETURN ACE.AdsMgGetWorkerThreadActivity(hMgmtHandle, astWorkerThreadActivity, REF pusArrayLen, REF pusStructSize)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgKillUser( hMgmtHandle AS IntPtr, pucUserName AS STRING, usConnNumber AS WORD ) AS DWORD
    RETURN ACE.AdsMgKillUser( hMgmtHandle,pucUserName,  usConnNumber)

/// <include file="ads.xml" path="functions/ads/*" />
FUNCTION AdsMgResetCommStats( hMgmtHandle AS IntPtr ) AS DWORD
    RETURN ACE.AdsMgResetCommStats( hMgmtHandle )

