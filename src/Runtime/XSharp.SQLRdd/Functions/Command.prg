//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.RDD.SqlRDD
using System.Collections.Generic
using System.Text
using System.Data
partial static class XSharp.SQLRDD.Functions
/// <summary>
/// Create a new SqlDbCommand object for the given connection
/// </summary>
/// <param name="hConn">Handle of the connection</param>
/// <param name="hConn">Name of the connection</param>
/// <param name="oConn">The connection object</param>
/// <returns>The handle to a new SqlDbCommand object for the connection</returns>
/// <seealso cref="T:SqlDbCommand"/>
static method SqlDbCreateSQLCommand(hConn as IntPtr) as IntPtr
    local oConn as SqlDbConnection
    local oCmd  as SqlDbCommand
    oConn := SqlDbGetConnection(hConn)
    if oConn != null
        oCmd := SqlDbCommand{"CMD", oConn}
        return oCmd:Handle
    endif
    return IntPtr.Zero
end method

/// <inheritdoc cref="SqlDbCreateSQLStatement(System.IntPtr)" />
static method SqlDbCreateSQLCommand(name as STRING) as IntPtr
    local oConn as SqlDbConnection
    local oCmd  as SqlDbCommand
    oConn := SqlDbGetConnection(name)
    if oConn != null
        oCmd := SqlDbCommand{"CMD", oConn}
        return oCmd:Handle
    endif
    return IntPtr.Zero
end method

/// <summary>
/// Create a new SqlDbCommand object for the given connection
/// </summary>
/// <param name="oConn">The connection object</param>
/// <returns>The DbCommand object</returns>
/// <seealso cref="T:SqlDbCommand"/>
/// <seealso cref="T:SqlDbConnection"/>
static method SqlDbCreateSQLCommand(oConn as SqlDbConnection) as SqlDbCommand
    local oCmd  as SqlDbCommand
    if oConn != null
        oCmd := SqlDbCommand{"CMD", oConn}
        return oCmd
    endif
    return null
end method

/// <summary>
/// Get a SqlDbCommand object from a Handle
/// </summary>
/// <param name="hCmd">Handle of a SqlDbCommand object that was previously created</param>
/// <returns>The SqlDbCommand object or NULL when the handle is invalid</returns>
/// <seealso cref="T:SqlDbCommand"/>
static method SqlDbGetCommand(hCmd as IntPtr) as SqlDbCommand
    local oCmd  as SqlDbCommand
    oCmd := SqlDbCommand.FindByHandle(hCmd)
    return oCmd
end method

static method SqlDbExecuteSQLDirect(hCmd as IntPtr, sCommandText as string) as object
    var oCmd := SqlDbGetCommand(hCmd)
    if oCmd != null
        oCmd:CommandText := oCmd:Connection:RaiseStringEvent(oCmd,SqlRDDEventReason.CommandText,"",sCommandText)
        return oCmd:ExecuteScalar()
    endif
    return null
end method



/// <summary>
/// Execute a SQL query and return the result as a DataTable
/// </summary>
/// <param name="hCmd">Handle of a SqlDbCommand object that was previously created</param>
/// <param name="sCommandText">Query to execute</param>
/// <returns> a Datatable or Null when an error occurred</returns>
static method SqlDbExecuteQueryDirect(hCmd as IntPtr, sCommandText as string) as DataTable
    var oCmd := SqlDbGetCommand(hCmd)
    if oCmd != null
        oCmd:CommandText := oCmd:Connection:RaiseStringEvent(oCmd,SqlRDDEventReason.CommandText,"",sCommandText)
        return oCmd:GetDataTable("Table")
    endif
    return null
end method

/// <summary>
/// Close a SqlDbCommand object
/// </summary>
/// <param name="hCmd">Handle of a SqlDbCommand object that was previously created</param>
/// <returns>TRUE when the handle was correct and the command was closed.</returns>
static method SqlDbCloseCommand(hCmd as IntPtr) as logic
    var oCmd := SqlDbGetCommand(hCmd)
    local lOk := false as logic
    if oCmd != null
        lOk := true
        oCmd:Close()
    endif
    return lOk
end method
end class


