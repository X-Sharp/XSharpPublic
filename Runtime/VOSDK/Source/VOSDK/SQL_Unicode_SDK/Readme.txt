XSSqlclasses

This assembly is meant as replacement for the VOSQLClasses.
It is based on the Ado.Net dataproviders and should work in AnyCPU and support unicode (if the server supports unicode).
The class names are the same as in the VOSQLClasses as are most fields and properties.
Some methods are implemented as "stubs" and marked with the [Obsolete] attribute.

By default the classes use the ODBC dataprovider for .Net, to be compatible with VO.
We have also added a mechanism to switch the dataprovider.
At this moment there is support for 2 providers:
1) ODBC
2) SQL Server
Other providers that are planned for the future are MySql and Oracle.

To switch to SQL Server you need to add the following to your code before creating a connection:

SetSqlFactory(SqlServerFactory{})

The source for these classes is on 
https://github.com/X-Sharp/XSharpPublic/tree/feature/Runtime/Runtime/VOSDK/Source/VOSDK/SQL_Unicode_SDK

The Factory class is the link between the SQLConnection object and the Ado.Net dataproviders.

It implements the ISqlFactory interface (see below).

The Factory class is a wrapper around System.Data.Common.DbProviderFactory.
It provides similar methods like this class but also has some methods that are called by the SqlConnection, 
SqlStatement and SqlSelect classes, to allow the Factory to override or implement specific behavior.

AbstractSqlFactory has a default "Abstract" implementation of most methods in the interface. 
Both SqlServerFactory and ODBCFactory inherit from the AbstractSqlFactory class.

The SQLServer provider for .Net does not know a mechanism for "DriverConnect". 
We therefore use the ODBC version of this dialog and read connectionstring information from this dialog.
This assumes that the "Sql Server" ODBC driver is installed on the users machine.


INTERFACE ISqlFactory

    /// <summary>Return the quote character for table and column names.</summary>
    PROPERTY QuoteChar AS STRING GET

    /// <summary>Does the factory support datasource enumeration.</summary>
    PROPERTY CanCreateDataSourceEnumerator AS LOGIC GET

    /// <summary>Return the name of the factory.</summary>
    METHOD GetName(oConn AS DbConnection) AS STRING  

    /// <summary>Create a connection object.</summary>
    METHOD CreateConnection AS DbConnection

    /// <summary>Create a command object.</summary>
    METHOD CreateCommand                        AS DbCommand

    /// <summary>Create a connectionstring builder object.</summary>
    METHOD CreateConnectionStringBuilder    AS DbConnectionStringBuilder

    /// <summary>Create a parameter object.</summary>
    METHOD CreateParameter                  AS DbParameter

    /// <summary>Create a dataadapter object.</summary>
    METHOD CreateDataAdapter                AS DbDataAdapter

    /// <summary>Create a datasource enumerator object.</summary>
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
    METHOD DriverConnect(hWindow AS USUAL, nCompletion AS USUAL, cConnectionString AS USUAL) AS STRING

    /// <summary>This method is called after a transaction was committed.</summary>
    METHOD EnhanceException(oEx AS SYstem.Exception)  AS SYstem.Exception

    /// <summary>This method is called to translate result values for a column.</summary>
    /// <param name="oValue">The value as returned by the dataprovider.</param>
    /// <param name="oFS">The fieldspec for the column.</param>
    /// <param name="lDateTimeAsDate">A setting that indicates if the server wants DateTime values as Date.</param>
    /// <returns>The value that must be passed to the program.</returns>
    METHOD HandleSpecialValue(oValue AS OBJECT,oFS AS FieldSpec, lDateTimeAsDate AS LOGIC) AS USUAL

    /// <summary>This method is called before a statement is sent to the server.</summary>
    /// <param name="cStatement">The original statement.</param>
    /// <returns>A translated version of the cStatement.</returns>
    METHOD TranslateStatement(cStatement AS STRING) AS STRING
        
    /// <summary>This method is called after a datareader was opened.</summary>
    /// <param name="oDataReader">The datareader that was created by the provider.</param>
    /// <remarks>Sometimes you may want to change the datareader before using it in the program. </remarks>
    /// <returns>The datareader as should be used by the SqlSelect class.</returns>
    METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader

    /// <summary>This method is called to create Field information for the SQLSelect class for a specific datacolumn.</summary>
    /// <param name="oSchema">The DataTable containing the schema for the opened resultset.</param>
    /// <param name="oColumn">The datacolumn that describes the field.</param>
    /// <param name="cFieldName">The name of the field. This may be different from the column name, for example if there are 2 columns with the same name.</param>
    /// <param name="oFS">A fieldspec that was created for the column. </param>
    /// <returns>A 'DbStruct' array of 4 elements with the name, type, length and decimals. For example {"LASTNAME","C",40,0}.</returns>
    METHOD DotNetType2VOType(oSchema AS DataTable, oColumn AS DataColumn, cFieldName AS STRING, oFS REF FieldSpec) AS ARRAY

END INTERFACE    