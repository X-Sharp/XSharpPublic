//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.RDD.SqlRDD
using System.Collections.Generic
using System.Text
USING System.Data

FUNCTION SqlDbCreateSQLStatement(hConn as IntPtr) AS IntPtr
    LOCAL oConn as SqlDbConnection
    LOCAL oCmd  as SqlDbCommand
    oConn := SqlDbGetConnection(hConn)
    if oConn != NULL
        oCmd := SqlDbCommand{"CMD", oConn}
        RETURN oCmd:Handle
    ENDIF
    RETURN IntPtr.Zero

FUNCTION SqlDbGetStatement(hStmt as IntPtr) AS SqlDbCommand
    LOCAL oCmd  as SqlDbCommand
    oCmd := SqlDbCommand.FindByHandle(hStmt)
    RETURN oCmd

FUNCTION SqlDbExecuteSQLDirect(hStmt as IntPtr, sCommandText as STRING) AS Object
    VAR oCmd := SqlDbGetStatement(hStmt)
    IF oCmd != NULL
        oCmd:CommandText := sCommandText
        RETURN oCmd:ExecuteScalar()
    ENDIF
    RETURN NULL

FUNCTION SqlDbExecuteQueryDirect(hStmt as IntPtr, sCommandText as STRING) AS DataTable
    VAR oCmd := SqlDbGetStatement(hStmt)
    IF oCmd != NULL
        oCmd:CommandText := sCommandText
        RETURN oCmd:GetDataTable("Table")
    ENDIF
    RETURN NULL
FUNCTION SqlDbCloseStatement(hStmt as IntPtr) AS LOGIC
    VAR oCmd := SqlDbGetStatement(hStmt)
    IF oCmd != NULL
        oCmd:Close()
    ENDIF
    RETURN oCmd != NULL

