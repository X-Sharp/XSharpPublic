//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text

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

FUNCTION SqlConnect(uSource , cUserID , cPassword , lShared) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqldisconnect/*" />

FUNCTION SqlDisconnect( nStatementHandle ) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

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
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlstringconnect/*" />

FUNCTION SqlStringConnect( uSharedOrConnectString, lSharable) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqltables/*" />

FUNCTION SqlTables( nStatementHandle , cTableTypes , cCursorName) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

