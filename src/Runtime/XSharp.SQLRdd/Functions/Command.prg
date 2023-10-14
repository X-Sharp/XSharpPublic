//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.RDD.SqlRDD
using System.Collections.Generic
using System.Text
using System.Data

function SqlDbCreateSQLStatement(hConn as IntPtr) as IntPtr
    local oConn as SqlDbConnection
    local oCmd  as SqlDbCommand
    oConn := SqlDbGetConnection(hConn)
    if oConn != null
        oCmd := SqlDbCommand{"CMD", oConn}
        return oCmd:Handle
    endif
    return IntPtr.Zero

function SqlDbGetStatement(hStmt as IntPtr) as SqlDbCommand
    local oCmd  as SqlDbCommand
    oCmd := SqlDbCommand.FindByHandle(hStmt)
    return oCmd

function SqlDbExecuteSQLDirect(hStmt as IntPtr, sCommandText as string) as object
    var oCmd := SqlDbGetStatement(hStmt)
    if oCmd != null
        oCmd:CommandText := oCmd:Connection:RaiseStringEvent(oCmd,SqlRDDEventReason.CommandText,"",sCommandText)
        return oCmd:ExecuteScalar()
    endif
    return null

function SqlDbExecuteQueryDirect(hStmt as IntPtr, sCommandText as string) as DataTable
    var oCmd := SqlDbGetStatement(hStmt)
    if oCmd != null
        oCmd:CommandText := oCmd:Connection:RaiseStringEvent(oCmd,SqlRDDEventReason.CommandText,"",sCommandText)
        return oCmd:GetDataTable("Table")
    endif
    return null
function SqlDbCloseStatement(hStmt as IntPtr) as logic
    var oCmd := SqlDbGetStatement(hStmt)
    if oCmd != null
        oCmd:Close()
    endif
    return oCmd != null


