//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using XSharp.ADS
#ifdef NOTIMPLEMENTED 

// Return the AXS locking status
FUNCTION AX_AXSLocking( ) AS LOGIC 
    RETURN AX_RddHelper(_SET_AXSLOCKING)

// Return and set the AXS locking status
FUNCTION AX_AXSLocking( bMode AS LOGIC) AS LOGIC 
    RETURN AX_RddHelper(_SET_AXSLOCKING, bMode)


FUNCTION AX_BLOB2File( cFileName AS STRING, cFieldName AS STRING ) AS LOGIC // copy a BLOB to a file
    LOCAL hTable AS DWORD
    LOCAL ulRetCode AS DWORD

    hTable := AX_GetAceTableHandle()
    ulRetCode := ACE.AdsBinaryToFile( hTable, cFieldName , cFileName )
    RETURN ulRetCode == 0 

FUNCTION AX_File2BLOB( cFileName AS STRING, cFieldName AS STRING ) AS LOGIC // copy a file into a BLOB
    LOCAL hTable AS DWORD
    LOCAL ulRetCode AS DWORD

    hTable := AX_GetAceTableHandle()
    ulRetCode := ACE.AdsFileToBinary( hTable, cFieldName , ACE.ADS_BINARY , cFileName )
    RETURN ulRetCode == 0 


FUNCTION AX_GetAceIndexHandle( uIndexFile as OBJECT, uOrder as OBJECT) AS DWORD
    // Returns an  index handle for the current workarea.  This handle can be used
    // to call the Advantage Client Engine directly.
    // Returns a 0 if there is a problem or if no index was found.

    // uIndexFile -- filename or NIL
    // uOrder -- order name, number, or NIL
    RETURN (DWORD) DBORDERINFO( DBOI_GET_ACE_INDEX_HANDLE, uIndexFile, uOrder )



FUNCTION AX_GetAceStmtHandle() AS DWORD
    // Returns the statement handle for the current workarea.  This handle can be used
    // to call the Advantage Client Engine directly.  Only for use with the AXSQL RDDs.
    // Returns a 0 if there is a problem.
    RETURN (DWORD) DBINFO( DBI_GET_ACE_STMT_HANDLE )

FUNCTION AX_GetAceTableHandle() AS DWORD
    // Returns the table handle for the current workarea.  This handle can be used
    // to call the Advantage Client Engine directly.
    // Returns a 0 if there is a problem.
    RETURN (DWORD) DBINFO( DBI_GET_ACE_TABLE_HANDLE )

FUNCTION AX_IsServerLoaded( cFileName AS STRING ) AS LOGIC // Return .T. if Advantage is loaded.
    // cFileName must start with a drive letter ("X:\") or a UNC path ("\\server\volume\path\")
    LOCAL usLoaded AS WORD
    usLoaded := 0
    ACE.AdsIsServerLoaded  (  cFileName , REF usLoaded )
    RETURN ( usLoaded == ACE.ADS_REMOTE_SERVER  .OR. usLoaded = ACE.ADS_AIS_SERVER )


FUNCTION AX_PercentIndexed() AS INT // Return the percentage of keys added to a currently building index
    RETURN (INT) DBORDERINFO(  DBOI_AXS_PERCENT_INDEXED )


// Return the AXS Rights Checking status
FUNCTION AX_RightsCheck( ) AS LOGIC 
    RETURN AX_RddHelper(_SET_RIGHTSCHECKING)

// Return and set the AXS Rights Checking status
FUNCTION AX_RightsCheck( bMode AS LOGIC) AS LOGIC 
    RETURN AX_RddHelper(_SET_RIGHTSCHECKING, bMode)

FUNCTION AX_SetCollation( strCollation AS STRING ) AS OBJECT
   RETURN RDDINFO( _SET_COLLATION_NAME, strCollation )

PROCEDURE AX_SetConnectionHandle( lHandle AS DWORD ) 
   RDDINFO( _SET_CONNECTION_HANDLE, lHandle )
RETURN 

FUNCTION AX_SetExactKeyPos( ) AS LOGIC
    RETURN AX_RddHelper(_SET_EXACTKEYPOS )

FUNCTION AX_SetExactKeyPos( bMode as LOGIC) AS LOGIC
    RETURN AX_RddHelper(_SET_EXACTKEYPOS, bMode)

FUNCTION AX_RddHelper(iInfo as INT) AS LOGIC
    LOCAL bRetVal as OBJECT
    bRetVal := RDDINFO( iInfo )
    IF ! bRetVal is LOGIC
         bRetVal := TRUE
    ENDIF
    RETURN (logic) bRetVal


FUNCTION AX_RddHelper(iInfo as INT, lNewValue as LOGIC) AS LOGIC
    LOCAL bRetVal as LOGIC
    bRetVal := AX_RddHelper(iInfo)
    RDDINFO( iInfo , lNewValue)
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
      usServerTypes := _OR( usServerTypes, ACE.ADS_REMOTE_SERVER )
    ENDIF
    IF( lUseInternetServer )
      usServerTypes := _OR( usServerTypes, ACE.ADS_AIS_SERVER )
    ENDIF
    IF( lUseLocalServer )
      usServerTypes := _OR( usServerTypes, ACE.ADS_LOCAL_SERVER )
    ENDIF

    ulRetCode := ACE.AdsSetServerType( usServerTypes )

    RETURN  ulRetCode == 0 


FUNCTION AX_SetSQLTablePasswords( aPasswords AS OBJECT ) AS VOID
    RDDINFO( _SET_SQL_TABLE_PASSWORDS, aPasswords )
    RETURN 

FUNCTION AX_Transaction( iAction as INT) AS LOGIC // Transaction call
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

    DO CASE
    CASE iAction = ACE.AX_BEGIN_TRANSACTION
       ulRetVal := ACE.AdsBeginTransaction( 0 )
    CASE iAction = ACE.AX_COMMIT_TRANSACTION
       ulRetVal := ACE.AdsCommitTransaction( 0 )
    CASE iAction = ACE.AX_ROLLBACK_TRANSACTION
       ulRetVal := ACE.AdsRollbackTransaction( 0 )
    CASE iAction = ACE.AX_ISACTIVE_TRANSACTION
       ulRetVal := ACE.AdsInTransaction( 0, REF usInTrans )
    ENDCASE

    RETURN ( usInTrans != 0 )


FUNCTION AX_Transaction( ) AS LOGIC // Transaction call
    LOCAL usInTrans := 0 AS WORD
    LOCAL ulRetVal AS DWORD
    ulRetVal := ACE.AdsInTransaction( 0, REF usInTrans )
    RETURN ( usInTrans != 0 )


FUNCTION AX_UsingClientServer( ) AS LOGIC
    // return .T. if the current workarea is using Advantage Server or AIS Server and
    // .F. IF USING Advantage RDD IN a LOCAL mode
	THROW NotImplementedException{}
	/*
    LOCAL ulRetCode AS DWORD
    LOCAL ConnectionHandle := 0 AS IntPtr
    LOCAL usServerType := 0 AS WORD
    LOCAL strFileName AS STRING
    strFileName := (String) XSharp.Core.Functions.DBINFO( DBInfo.DBI_FULLPATH )
    ulRetCode := ACE.AdsFindConnection(  strFileName , OUT ConnectionHandle )
    ulRetCode := ACE.AdsGetConnectionType( ConnectionHandle, OUT usServerType )
    RETURN ( usServerType == ACE.ADS_REMOTE_SERVER ) .OR. ( usServerType == ACE.ADS_AIS_SERVER )
	*/

#endif