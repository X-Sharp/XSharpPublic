//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.RDD.SqlRDD
using XSharp.RDD.Support
using System.Collections.Generic
using System.Text
/// <summary>
/// Open a connection for the X# SQL RDD
/// </summary>
/// <param name="ConnectionString">Connection string in the right format for the Ado.Net dataprovider.</param>
/// <param name="ConnectionName">Name for the connection. Defaults to 'DEFAULT'.</param>
/// <returns>A Handle to the connection, or IntPtr.Zero when opening the connection fails/</returns>
/// <remarks>This will open the default connection.
/// If the connection string needs a UserName and/or Password then these need to be included in
/// the connection string.</remarks>

function SqlDbOpenConnection(ConnectionString as string) as IntPtr
    var oConn := SqlDbConnection{SqlDbConnection.DefaultConnection, ConnectionString}
    return oConn:Handle

function SqlDbOpenConnection(ConnectionString as string, @@CallBack as SqlRDDEventHandler) as IntPtr
    var oConn := SqlDbConnection{SqlDbConnection.DefaultConnection, ConnectionString, @@CallBack}
    return oConn:Handle

/// <inheritdoc cref="SqlDbOpenConnection(System.String)" />
function SqlDbOpenConnection(ConnectionName as string, ConnectionString as string) as IntPtr
    var oConn := SqlDbConnection{ConnectionName, ConnectionString}
    return oConn:Handle

/// <inheritdoc cref="SqlDbOpenConnection(System.String)" />
function SqlDbOpenConnection(ConnectionName as string, ConnectionString as string, @@CallBack as SqlRDDEventHandler) as IntPtr
    var oConn := SqlDbConnection{ConnectionName, ConnectionString, @@CallBack}
    return oConn:Handle

/// <summary>
/// Close a SQLRDD Connection.
/// </summary>
/// <param name="ConnectionName">Name of the connection to close.</param>
/// <param name="Handle">Handle of the connection to close.</param>
/// <returns>TRUE when the connection was successfully closed. FALSE if the connection did not exist or was already closed</returns>
function SqlDbCloseConnection(ConnectionName as string) as logic
    var oConn := SqlDbConnection.FindByName(ConnectionName)
    if (oConn != null)
        oConn:Close()
        return true
    endif
    return false
/// <inheritdoc cref="SqlDbCloseConnection(System.String)" />
function SqlDbCloseConnection(Handle as IntPtr) as logic
    var oConn := SqlDbConnection.FindByHandle(Handle)
    if oConn != null
        return oConn:Close()
    endif
    return oConn != null

function SqlDbGetConnection(Handle as IntPtr) as SqlDbConnection
    return SqlDbConnection.FindByHandle(Handle)

function SqlDbGetConnection(ConnectionName as string) as SqlDbConnection
    return SqlDbConnection.FindByName(ConnectionName)


/// <summary>
/// Specify if connections should remain open during the lifetime of an app.
/// </summary>
/// <param name="ShouldCache">When TRUE then connections remain open. The default is TRUE.</param>
/// <returns>The previous value for the setting</returns>
/// <remarks>
/// <para>SqlDb connections are implemented using <see cref='T:System.Data.Common.DbConnection'>DbConnection</see>
/// objects from the underlying Ado.Net SQL Provider. <br/>
/// Most desktop applications will open a connection at startup and keep that connection open.
/// However web apps are expected to open a connection, fetch data and then close the connection.
/// The Ado.Net infrastructure supports what is called "Connection Pooling". This means that if you
/// close a connection, it will not immediately return the connection back to the database provider but
/// will keep it open a bit longer.
/// </para>
/// <para>
/// If you open a new connection in your app later with the same connectionstring, username and password
/// as a previously closed connection, then you may receive a the connection object from a previous connection.
/// This helps to keep the # of open connection used by apps low. <br/>
/// </para>
/// <para>
/// If you set the ShouldCache value to FALSE then when a table or query is opened by the RDD, this
/// may trigger the opening of the connection. This connection will then remain open until the last table or query
/// using the connection is closed. The connection object itself (containing name, connection string etc) remains
/// in memory. The next time the connection is needed then the underlying DbConnection object will be opened again.
/// </para>
/// </remarks>

function SqlDbCacheConnection(ShouldCache as logic) as logic
    var old := SqlDbConnection.DefaultCached
    SqlDbConnection.DefaultCached := ShouldCache
    return  old




function List2String(list as IList<string>) as string
    var sb := StringBuilder{}
    var first := true
    foreach var item in list
        if first
            first := false
        else
            sb:Append(", ")
        endif
        sb:Append(item)
    next
    return sb:ToString()

function String2List(names as string) as IList<string>
    var list := names:Split(<char>{','})
    var result := List<string>{}
    foreach var element in list
        result:Add(element:Trim())
    next
    return result


function XsValueToSqlValue(oValue as object)  as string
    switch oValue
    case strValue as string
        return "'"+strValue:Replace("'","''")+"'"
    case dDate as IDate
        return "'"+DToS(dDate)+"'"
    case iValue as long
        return iValue:ToString()
    case dFloat as IFloat
        return dFloat:Value:ToString()
    end switch
    return oValue:ToString()

    function DToS(dDate as IDate) as string
        return dDate:Year:ToString()+"-"+dDate:Month:ToString()+"-"+dDate:Day:ToString()


