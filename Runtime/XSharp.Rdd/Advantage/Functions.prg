//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.ADS


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
    CASE ACE.AX_BEGIN_TRANSACTION
       ulRetVal := ACE.AdsBeginTransaction( 0 )
    CASE ACE.AX_COMMIT_TRANSACTION
       ulRetVal := ACE.AdsCommitTransaction( 0 )
    CASE ACE.AX_ROLLBACK_TRANSACTION
       ulRetVal := ACE.AdsRollbackTransaction( 0 )
    CASE ACE.AX_ISACTIVE_TRANSACTION
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
    
