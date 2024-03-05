//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Data.Common
using System.Collections.Generic
using XSharp.RDD.Support
begin namespace XSharp.RDD.SqlRDD.Providers

    /// <summary>
    /// Interface for SQL Database Providers
    /// </summary>
interface ISqlDbProvider
    /// <summary>
    /// Name of the provider
    /// </summary>
    property Name as string get

    /// <summary>
    /// DbProviderFactory for this provider
    /// </summary>
    property Factory as DbProviderFactory GET
    /// <summary>
    /// Name of the DLL that contains the DbProviderFactory
    /// </summary>
    property DllName as string get
    /// <summary>
    /// Type name of the DbProviderFactory
    /// </summary>
    property TypeName as string get

    /// <summary>
    /// Syntax for a CREATE TABLE statement.
    /// </summary>
    property CreateTableStatement   as string get
    /// <summary>
    /// Syntax for a CREATE INDEX statement.
    /// </summary>
    property CreateIndexStatement   as string get
    /// <summary>
    /// Syntax for a DROP TABLE statement.
    /// </summary>
    property DropTableStatement     as string get
    /// <summary>
    /// Syntax for a DROP INDEX statement.
    /// </summary>
    property DropIndexStatement     as string get
    /// <summary>
    /// Syntax for the statement to delete all rows from a table (ZAP)
    /// </summary>
    property DeleteAllRowsStatement as string get
    /// <summary>
    /// Syntax for the statement to select a limited number of rows from the table
    /// </summary>
    property SelectTopStatement     as string get
    /// <summary>
    /// Syntax for the INSERT statement
    /// </summary>
    property InsertStatement        as string get
    /// <summary>
    /// Syntax for the UPDATE statement
    /// </summary>
    property UpdateStatement        as string get
    /// <summary>
    /// Syntax for the DELETE statement
    /// </summary>
    property DeleteStatement        as string get
    /// <summary>
    /// Syntax for the ORDER BY clause
    /// </summary>
    property OrderByClause          as string get
    /// <summary>
    /// Syntax for the statement to retrieve the identity value of the last inserted row
    /// </summary>
    property GetIdentity            as string get
    /// <summary>
    /// Syntax for the statement to retrieve the number of rows updated by the last statement
    /// </summary>
    property GetRowCount            as string get


    /// <summary>
    /// Return a list of function translations for this provider
    /// </summary>
    /// <returns>Dictionary with XBase functions mapped to SQL functions</returns>

    method GetFunctions() as Dictionary<string, string>

    /// <summary>
    /// Create a provider specific DbCommand Object
    /// </summary>
    /// <returns>A new DbCommand object</returns>
    method CreateCommand() as DbCommand
    /// <summary>
    /// Create a provider specific DbCommandBuilder Object
    /// </summary>
    /// <returns>A new DbCommandBuilder object</returns>
    method CreateCommandBuilder() as DbCommandBuilder
    /// <summary>
    /// Create a provider specific DbConnection Object
    /// </summary>
    /// <returns>A new DbConnection object</returns>
    method CreateConnection() as DbConnection
    /// <summary>
    /// Create a provider specific DbConnectionStringBuilder Object
    /// </summary>
    /// <returns>A new DbConnectionStringBuilder object</returns>
    method CreateConnectionStringBuilder() as DbConnectionStringBuilder
    /// <summary>
    /// Create a provider specific DbParameter Object
    /// </summary>
    /// <returns>A new DbParameter object</returns>
    method CreateParameter() as DbParameter
    /// <summary>
    /// Create a provider specific DbDataSourceEnumerator Object
    /// </summary>
    /// <returns>A new DbDataSourceEnumerator object</returns>
    method CreateDataSourceEnumerator() as DbDataSourceEnumerator

    /// <summary>
    /// Surround an identifier with the correct quotes for the provider
    /// </summary>
    /// <param name="cId">Identifier</param>
    /// <returns>Quoted Identifier</returns>
    /// <example>
    /// <code>
    /// var cId := "MyTable"
    /// var cQuoted := provider:QuoteIdentifier(cId)
    /// // cQuoted will be "[MyTable]" for SQLServer
    /// </code>
    /// </example>

    method QuoteIdentifier(cId as string) as string

    /// <summary>
    /// Create the column definition (in SQL syntax) from a RDDFieldInfo object
    /// </summary>
    /// <param name="oInfo">Object that contains the field definition</param>
    /// <param name="oConn">Connection to the database</param>
    /// <returns>a string in SQL syntax</returns>
    /// <remarks>
    /// This is used when creating a SQL table with DbCreate()
    /// </remarks>
    method GetSqlColumnInfo(oInfo as RddFieldInfo, oConn as SqlDbConnection) as string
    /// <summary>
    /// Get the translation for an XBase function to a SQL function
    /// </summary>
    /// <param name="sFunction">Function. Parameters are replaced with %1%, %2% etc.</param>
    /// <returns>The translation of the function.</returns>
    method GetFunction(sFunction as string) as string


end interface
end namespace
