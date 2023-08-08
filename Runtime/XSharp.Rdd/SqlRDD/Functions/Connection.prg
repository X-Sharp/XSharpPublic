//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.RDD.SqlRDD

/// <summary>
/// Open a connection for the X# SQL RDD
/// </summary>
/// <param name="ConnectionString">Connection string in the right format for the Ado.Net dataprovider.</param>
/// <param name="UserName">Username to login to the database.</param>
/// <param name="Password">Password to login to the database.</param>
/// <param name="ConnectionName">Name for the connection. Defaults to 'DEFAULT'.</param>
/// <returns>A Handle to the connection, or IntPtr.Zero when opening the connection fails/</returns>
/// <remarks>This will open the default connection. UserName and Passwords are either not needed or part of
/// the connection string.</remarks>

FUNCTION SqlDbOpenConnection(ConnectionString as STRING) as IntPtr
    var oConn := SqlDbConnection{SqlDbConnection.DefaultConnection, ConnectionString, "",""}
    RETURN oConn:Handle

/// <inheritdoc cref="SqlDbOpenConnection(System.String)" />
FUNCTION SqlDbOpenConnection(ConnectionString as STRING, UserName as STRING, Password as String) as IntPtr
    var oConn := SqlDbConnection{SqlDbConnection.DefaultConnection, ConnectionString, UserName,Password}
    RETURN oConn:Handle
/// <inheritdoc cref="SqlDbOpenConnection(System.String)" />
FUNCTION SqlDbOpenConnection(ConnectionName as String, ConnectionString as STRING) as IntPtr
    var oConn := SqlDbConnection{ConnectionName, ConnectionString, "",""}
    RETURN oConn:Handle

/// <inheritdoc cref="SqlDbOpenConnection(System.String)" />
FUNCTION SqlDbOpenConnection(ConnectionName as String, ConnectionString as STRING, UserName as STRING, Password as String) as IntPtr
    var oConn := SqlDbConnection{ConnectionName, ConnectionString, UserName,Password}
    RETURN oConn:Handle

    /// <summary>
    /// Close a SQLRDD Connection.
    /// </summary>
    /// <param name="ConnectionName">Name of the connection to close.</param>
    /// <param name="Handle">Handle of the connection to close.</param>
    /// <returns>TRUE when the connection was successfully closed. FALSE if the connection did not exist or was already closed</returns>
FUNCTION SqlDbCloseConnection(ConnectionName as String) as LOGIC
    var oConn := SqlDbConnection.FindByName(ConnectionName)
    if (oConn != NULL)
        oConn:Close()
        RETURN TRUE
    endif
    RETURN FALSE
/// <inheritdoc cref="SqlDbCloseConnection(System.String)" />
FUNCTION SqlDbCloseConnection(Handle as IntPtr) as LOGIC
    var oConn := SqlDbConnection.FindByHandle(Handle)
    if (oConn != NULL)
        oConn:Close()
        RETURN TRUE
    endif
RETURN FALSE

FUNCTION SqlDbGetConnection(Handle as IntPtr) as SqlDbConnection
    RETURN SqlDbConnection.FindByHandle(Handle)

FUNCTION SqlDbGetConnection(ConnectionName as String) as SqlDbConnection
    RETURN SqlDbConnection.FindByName(ConnectionName)


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

FUNCTION SqlDbCacheConnection(ShouldCache as LOGIC) AS LOGIC
    var old := SqlDbConnection.DefaultCached
    SqlDbConnection.DefaultCached := ShouldCache
    RETURN  old

