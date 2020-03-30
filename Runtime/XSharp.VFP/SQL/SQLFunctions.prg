//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic 
USING System
USING XSharp.VFP


/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlconnect/*" />
FUNCTION SqlConnect(uSource , cUserID , cPassword , lShared) AS LONG
    // uSource may be either a DSN or a nStatementHandle
    LOCAL nHandle := -1 AS LONG
    LOCAL cSource AS STRING
    LOCAL oConnection AS XSharp.VFP.SQLConnection
    IF IsNumeric(uSource)
        IF ! IsNil(cUserID) .OR. ! IsNil(cPassword) .OR. ! IsNil(lShared)
           THROW Error{"Number or type of parameters is not correct"}
        ENDIF
        nHandle     := uSource
        oConnection := SQLSupport.FindConnection(nHandle)
        IF oConnection == NULL_OBJECT
            THROW Error{"Connection Handle ("+nHandle:ToString()+") is invalid"}
        ENDIF
        // This should open a connection automatically since the previous connection as already validated
        IF ! oConnection:Shared
            THROW Error{"Connection Handle ("+nHandle:ToString()+") is not a shared connection"}
        ENDIF
        oConnection := XSharp.VFP.SQLConnection{oConnection:ConnectionString, oConnection:Shared}
        IF oConnection:Connected
            nHandle := SQLSupport.AddConnection(oConnection)
        ENDIF

    ELSEIF IsString(uSource)
        // if one or more parameters are missing then the connect dialog is shown
        cSource := uSource
        IF ! IsString(cUserID) ; cUserID := "" ; ENDIF
        IF ! IsString(cPassword) ; cPassword := "" ; ENDIF
        IF ! IsLogic(lShared) ; lShared := TRUE ; ENDIF
        oConnection := XSharp.VFP.SQLConnection{cSource, cUserID, cPassword, lShared}
        IF oConnection:Connected
            nHandle := SQLSupport.AddConnection(oConnection)
        ENDIF
    ENDIF
    RETURN nHandle
    

/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlstringconnect/*" />

FUNCTION SqlStringConnect( uSharedOrConnectString, lSharable) AS LONG
    //  uSharedOrConnectString may be either lShared or a connection string
    // in FoxPro when passing lShared as TRUE then the "Select Connection or Data Source Dialog Box"  is shown
    // This is the normal 'DriverConnect' dialog for ODBC drivers
    LOCAL nHandle := -1 AS LONG
    LOCAL oConnection AS XSharp.VFP.SQLConnection
    IF IsLogic(uSharedOrConnectString)
        // show connection string dialog
       oConnection := XSharp.VFP.SQLConnection{"", uSharedOrConnectString}
    ELSEIF IsString(uSharedOrConnectString)
       IF ! IsLogic(lSharable) ; lSharable := TRUE ; ENDIF
       oConnection := XSharp.VFP.SQLConnection{uSharedOrConnectString,lSharable}
    ELSE
       THROW Error{"Number or type of parameters is not correct"}
    ENDIF
    IF oConnection:Connected
        nHandle := SQLSupport.AddConnection(oConnection)
    ENDIF
    RETURN nHandle
  
    



/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlcancel/*" />

FUNCTION SQLCANCEL( nStatementHandle) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlcolumns/*" />

FUNCTION SqlColumns( nStatementHandle, cTableName, cType, cCursorName) AS USUAL
    THROW NotImplementedException{}
    // RETURN NIL

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlcommit/*" />

FUNCTION SqlCommit( nStatementHandle ) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlconnect/*" />



/// <include file="VFPDocs.xml" path="Runtimefunctions/sqldisconnect/*" />
FUNCTION SqlDisconnect( nStatementHandle AS LONG) AS LONG
    LOCAL oConnection AS XSharp.VFP.SQLConnection
    oConnection := SQLSupport.FindConnection(nStatementHandle)
    IF oConnection != NULL
        SQLSupport.RemoveObject(nStatementHandle)
        oConnection:DisConnect()
        RETURN 1
    ENDIF
    RETURN -1

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlexec/*" />

FUNCTION SqlExec( nStatementHandle , cSQLCommand , cCursorName, aCountInfo) AS LONG
    THROW NotImplementedException{}
    // RETURN 0
    
/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlgetprop/*" />

FUNCTION SqlGetProp( nStatementHandle, cSetting ) AS USUAL
    THROW NotImplementedException{}
    // RETURN NIL

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlidledisconnect/*" />

FUNCTION SqlIdleDisconnect( nStatementHandle) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlmoreresults/*" />

FUNCTION SqlMoreResults( nStatementHandle , cCursorName , aCountInfo) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlprepare/*" />

FUNCTION SqlPrepare( nStatementHandle, cSQLCommand, cCursorName) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlrollback/*" />

FUNCTION SqlRollBack( nStatementHandle ) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlsetprop/*" />

FUNCTION SqlSetProp( nStatementHandle, cSetting , eExpression) AS LONG
    THROW NotImplementedException{}
    // RETURN 0



/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqltables/*" />

FUNCTION SqlTables( nStatementHandle , cTableTypes , cCursorName) AS LONG
    THROW NotImplementedException{}
    // RETURN 0


