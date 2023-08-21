//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data.Common
USING System.Data
USING XSharp.Data

/// <summary>This interface declares the common behavior for all XSharp Data Factory classes.</summary>
/// <seealso cref="M:XSharp.VFP.Functions.SqlSetFactory">SqlSetFactory()</seealso>
/// <seealso cref="SetSqlFactory">SetSqlFactory()</seealso>
/// <seealso cref="GetSqlFactory">GetSqlFactory()</seealso>

INTERFACE XSharp.Data.ISqlFactory

    /// <summary>Return the quote character for table and column names.</summary>
    PROPERTY QuoteChar AS STRING GET
    /// <summary>Return the name of the factory.</summary>
    PROPERTY Name      AS STRING GET

    /// <summary>Does the factory support datasource enumeration.</summary>
    PROPERTY CanCreateDataSourceEnumerator AS LOGIC GET
    /// <summary>Define the character with which parameters are prefixed in queries. For example '?' (ODBC, OLEDB) or '@' (SQLServer) </summary>
    PROPERTY ParameterPrefix AS CHAR GET

    /// <summary>Should the parameter name be included in the query. Defaults to FALSE for ODBC and OLEDB and TRUE for SQLServer </summary>
    PROPERTY ParameterNameInQuery AS LOGIC GET

    /// <summary>Return the name of the factory.</summary>
    METHOD GetName(oConn AS DbConnection) AS STRING  

    /// <inheritdoc cref="DbProviderFactory.CreateConnection" />
    /// <seealso cref='DbConnection' />
    METHOD CreateConnection AS DbConnection

    /// <inheritdoc cref="DbProviderFactory.CreateCommand" />
    /// <seealso cref='DbCommand' />
    METHOD CreateCommand                        AS DbCommand

    /// <inheritdoc cref="DbProviderFactory.CreateCommandBuilder" />
    /// <seealso cref='DbCommandBuilder' />
    METHOD CreateCommandBuilder    AS DbCommandBuilder  

    /// <inheritdoc cref="DbProviderFactory.CreateConnectionStringBuilder" />
    /// <seealso cref='DbConnectionStringBuilder' />
    METHOD CreateConnectionStringBuilder    AS DbConnectionStringBuilder

    /// <inheritdoc cref="DbProviderFactory.CreateParameter" />
    /// <seealso cref='DbParameter' />
    METHOD CreateParameter                  AS DbParameter

    /// <inheritdoc cref="DbProviderFactory.CreateDataAdapter" />
    /// <seealso cref='DbDataAdapter' />
    METHOD CreateDataAdapter                AS DbDataAdapter

    /// <inheritdoc cref="DbProviderFactory.CreateDataSourceEnumerator" />
    /// <seealso cref='DbDataSourceEnumerator' />
    METHOD CreateDataSourceEnumerator() AS DbDataSourceEnumerator



    /// <summary>This method gets called before connecting. This allows to adjust the connection string.</summary>
    /// <returns>The connection string that may be altered by the factory.</returns>
    /// <param name="cString">Connection String.</param>
    /// <param name="cUser">User Name (may be blank).</param>
    /// <param name="cPassword">Pass word (may be blank).</param>
    METHOD BeforeConnect(cString AS STRING, cUser AS STRING, cPassword AS STRING) AS STRING

    /// <summary>This method is called after a connection was opened.</summary>
    /// <param name="oConnection">The connection object that was opened.</param>
    METHOD AfterConnect(oConnection AS DbConnection) AS VOID

    /// <summary>This method is called before a connection is disconnected.</summary>
    /// <param name="oConnection">The connection object.</param>
    METHOD BeforeDisConnect(oConnection AS DbConnection) AS VOID

    /// <summary>This method is called after a connection was disconnected.</summary>
    /// <param name="oConnection">The connection object.</param>
    METHOD AfterDisConnect(oConnection AS DbConnection) AS VOID

    /// <summary>This method is called before a transaction is rolled back.</summary>
    /// <param name="oTransaction">The transaction object.</param>
    METHOD BeforeRollBack(oTransaction AS DbTransaction) AS VOID

    /// <summary>This method is called after a transaction was rolled back.</summary>
    /// <param name="oTransaction">The transaction object.</param>
    METHOD AfterRollBack(oTransaction AS DbTransaction) AS VOID

    /// <summary>This method is called before a transaction is committed.</summary>
    /// <param name="oTransaction">The transaction object.</param>
    METHOD BeforeCommit(oTransaction AS DbTransaction) AS VOID

    /// <summary>This method is called after a transaction was committed.</summary>
    /// <param name="oTransaction">The transaction object.</param>
    METHOD AfterCommit(oTransaction AS DbTransaction) AS VOID

    /// <summary>This method is called to show an interactive dialog to select a connection.</summary>
    /// <param name="hWindow">An optional window handle that will be the owner of the dialog.</param>
    /// <param name="nCompletion">A numeric value that indicates what can be changed. For example SQL_DRIVER_PROMPT or SQL_DRIVER_COMPLETE. See the ODBC Docs for more info.</param>
    /// <param name="cConnectionString">A connection string that indicates the start values for the dialog.</param>
    /// <returns>The connection string that is the result of the dialog.</returns>
    METHOD DriverConnect(hWindow AS IntPtr, nCompletion AS OBJECT, cConnectionString AS OBJECT) AS STRING

    /// <summary>This method is called after a transaction was committed.</summary>
    METHOD EnhanceException(oEx AS System.Exception)  AS System.Exception

    /// <summary>This method is called to translate result values for a column.</summary>
    /// <param name="oValue">The value as returned by the dataprovider.</param>
    /// <param name="oFS">The fieldspec for the column.</param>
    /// <param name="lDateTimeAsDate">A setting that indicates if the server wants DateTime values as Date.</param>
    /// <returns>The value that must be passed to the program.</returns>
    METHOD HandleSpecialValue(oValue AS OBJECT,oFS AS OBJECT, lDateTimeAsDate AS LOGIC) AS OBJECT

    /// <summary>This method is called before a statement is sent to the server.</summary>
    /// <param name="cStatement">The original statement.</param>
    /// <returns>A translated version of the cStatement.</returns>
    METHOD TranslateStatement(cStatement AS STRING) AS STRING
        
    /// <summary>This method is called after a datareader was opened.</summary>
    /// <param name="oDataReader">The datareader that was created by the provider.</param>
    /// <remarks>Sometimes you may want to change the datareader before using it in the program. </remarks>
    /// <returns>The datareader as should be used by the SqlSelect class.</returns>
    METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader

    /// <summary>Create an object array of column properties from the current datarow in the schema rowset</summary> 
    METHOD GetMetaDataColumnValues(oRow AS DataRow) AS OBJECT[]
    
    /// <summary>Create an object array of table properties from the current datarow in the schema rowset</summary>
    METHOD GetMetaDataTableValues(oRow AS DataRow) AS OBJECT[]

END INTERFACE    


/// <summary>Set a new default SQL factory.</summary>
/// <seealso cref="ISqlFactory">ISqlFactory</seealso>
/// <seealso cref="O:XSharp.VFP.Functions.SqlSetFactory">SqlSetFactory()</seealso>
/// <seealso cref="SetSqlFactory">SetSqlFactory()</seealso>
/// <param name="oFactory">New default factory</param>
/// <returns>The current default factory.</returns>
FUNCTION SetSqlFactory(oFactory AS ISqlFactory) AS ISqlFactory
    LOCAL oOld AS ISqlFactory
    oOld := XSharp.Data.AbstractSqlFactory.DefaultFactory
    XSharp.Data.AbstractSqlFactory.DefaultFactory := oFactory
    RETURN oOld

/// <summary>Retrieve the default SQL factory.</summary>
/// <seealso cref="ISqlFactory">ISqlFactory</seealso>
/// <seealso cref="O:XSharp.VFP.Functions.SqlSetFactory">SqlSetFactory()</seealso>
/// <seealso cref="SetSqlFactory">SetSqlFactory()</seealso>
/// <returns>The current default factory.</returns>
FUNCTION GetSqlFactory() AS ISqlFactory STRICT
    LOCAL oOld AS ISqlFactory
    oOld := XSharp.Data.AbstractSqlFactory.DefaultFactory
    RETURN oOld

