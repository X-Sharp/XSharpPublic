# Some notes about the implementation of the SQL RDD

This RDD combines work that was already done in the past for:
- X# AnyCPU/Unicode SQL Classes
- X# FoxPro DBFVFPSQL RDD (which is used for result sets returned by SqlExec())
- The VO2Ado RDD

The RDD will not only be an RDD but will also have several helper classes:
# 1. Connection Class. 
   This contains the UserName/Password and connection string of the connection
   Connections have a Name and an ID (an IntPtr like ADS)

   This will also have a reference to the Provider Class (see 2) 
   ## Name
   One application can have multiple open connections to the same or to different types of databases
   One of the connections is the Default Connection. 
   When a table or resultset is opened without specifying a connection then the default connection is used
   To specify a connection the table name or query must be prefixed with the connection name followed by a double colon
   
   `USE MyConn::Customers`
   
   ## Events
   When the connection is not defined then a runtime error will be generated.
   The connection class will also be called with so called "Callback events". 
   By registering your callback class you can intercept these and change the behaviour of the RDD. 
   Callback events are for example:
   - BeforeConnect
   - AfterConnect
   - ConnectionString
   - BeforeDisconnect
   - AfterDisconnect
   - CreateIndex
   - CreateTable
   - DropIndex
   - DropTable
   - DeleteAllRecords
   - DetermineFieldType
   - UserId
   - Password
   - PreExecute
   
   Each of these callback events will retrieve the table name and the generated SQL code. You can modify the SQL in the event when you want.
   The default syntax for some of these values will come from the Provider (see 2))
   ## Functions
   There will be several functions to manage connections:
   1. ` SqlDbOpenConnection(ConnectionString as STRING) as IntPtr`
   
   This will open the default connection. UserName and Passwords are either not needed or part of 
   the connection string
   The return value is the Connetion Handler, or IntPtr.Zero when opening the connection failed.
   
   2. ` SqlDbOpenConnection(ConnectionString as STRING, UserName as STRING, Password as String) AS IntPtr`
   
   Like  1 , but with an explicit username, password
   
   3. ` SqlDbOpenConnection(ConnectionName as String, ConnectionString as STRING) as IntPtr`
   
   Like 1, but for an alternative connection
   
   4. ` SqlDbOpenConnection(ConnectionName as String, ConnectionString as STRING, UserName as STRING, Password as String) AS IntPtr`
   
   Like 2, but for an alternative connection

   5. `SqlDbCloseConnection(ConnectionName as String) as LOGIC`

   Closes a connection by name.
   Returns TRUE when succesfull.

   6. `SqlDbCloseConnection(Handle as IntPtr) AS LOGIC`

   Closes a connection by handle (returned from SqlDbOpenConnection)
   Returns TRUE when succesfull.


## Caching Connections
By default a connection will remain open for the duration of the application.
This bypasses the connection pooling in Ado.Net.
You can also open a connection *when needed* which will use the Ado.Net connection pooling.
To enable that you can call 

    `SqlDbCacheConnection(ShouldCache as LOGIC) AS LOGIC`

Call this function at the start of your app to enable/disable caching. By default caching is enabled.
If you call this after the first connection was opened, then you need to "manually" close that connection first


## Fields and Properties of the connection class
| Name | Description |
|--------------------|
| Name | Unique Name. The default connection is called 'DEFAULT' |
| ConnectionString | Ado.Net Connection String |
| UserName | May be blank when password is part of Connection String |
| Password | May be blank when password is part of Connection String |
| Handle | Unique id|
| Cached | When true then more than one area can use the connection |
| Provider | Provider object associated with Connection |
| Areas |  list of areas | 
| Connections (Static) | List of open Connections |



# 2. Provider Class
   This class contains driver specific settings, needed by the SQL RDD. 
   Most of these will serve as default values for the callbacks that are listed under 1). 
   
   This class also contains the mapping between the drivers column types and X# column types and a table that is used to translate XBase functions to SQL functions.
   We will try deliver the following providers in the RDD:
   - ODBC
   - OleDb
   - SQL Server
   - MySql 
   - Oracle
   - SQLite
   We may need help from the community for providers for other databases, such as Postgres.
   ## Properties and Methods of the ProviderClass
   | Name | Description |
   |--------------------|
   | Name | Unique Name |
   | Functions | Table with Function replacements |
   | TopSyntax | Syntax to get top <n> elements |
   | QuoteChar | Quote Character for the provider |
   | ParameterPrefix | Prefix to use for parameter names |
   | ParameternameInQuery| Should the parameter name be included in the query, or should just a placemarker (e.g. ?) be used |
   | Factory | Fully Qualified Type Name of the Ado.Net DbbProviderFactory class. The assembly containing the type should be loaded |
   | CreateConnection | Method to create a connection object |
   | CreateParameter | Method to create a parameter object |
   | CreateCommand | Method to create a command object |
   

   
# 3. RDD Class   
   This class will alow to open data in 2 different modes:
   1) **Table mode**,  USE MyConn::Customers
   2) **Query mode**, DbServer{"MyConn::Select * From Customers where State = 'NY'"}
   
   When opened in table mode, then updates are enabled.  
   When opened in select mode then updates are DISabled by default

At the connection level you can specify if you want to map the columns to "VO" style columns (CDMLN) or to all of the FoxPro column types,
including Binary, DateTime, AutoInc etc.
To be able to properly emulate Recno and Delete the table must have special columns. 
To be able to properly emulate opening indexes when a table is opened, the RDD must retrieve information about the available indexes, tags and expressions per table

To resolve this extra "metadata" there will be 3 different mechanisms available. 

1. Read information from special tables in the RDD
2. Read information from disk files (a kind of INI files) 
3. Get information from a callback in the application. This callback can be registered at the connection level

1) and 2) will be included in the RDD itself.

There will be an enum in the RDD that specifies the kind of "callback" values that are needed
	- Index Count
	- Index Expression
	- Index Name
	- Index Condition
	- Index Unique
	- Index Descending
	- Recno Field Name
	- Deleted Field Name
	- Is a column updateable
	- Should "Long fieldsnames" (> 10 chars) be allowed
	- and many more

The callback will have to implement a method that receives:
	-	Value of the Enum (type of information needed)
	-	String (table name) for which the information is needed
	-	Number (index number) for which the information is needed
	-	Returns Object. This is either a number (for IndexCount for example) or a string (Index Name, condition, Field Names etc)	


Internally in the RDD itself, the data which is retrieved from the server will be locally stored inside a ADO.Net DataTable
The Deleted state is emulated by setting the Deleted field to TRUE 

## Indexes
When creating an index in Table Mode then there will be an index created at the server. Also the metadata for the index needs to be stored.
The RDD is smart enough to parse the index expressions and translate an expression like CUSTNO+DTOS(OrderDate) into a column list CUSTNO, ORDERDATE
The index on the server will then be created on CUSTNO, ORDERDATE.

When you select and index and then perform a Seek then the RDD needs to translate the SEEK expression into a WHERE clause for the server.
In that case a translation is needed.
The provider class will have a method that receives the original function name (for example DTOS) and needs to return a function on the server.


Assume the index is created like this:
	INDEX ON STR(CUSTOMERID,10,0) + DTOS(ORDERDATE) TO ORD_DATE

This will create an index (for SQL Server) like this:
	CREATE INDEX ORDER_ORD_DATE ON ORDER (CUSTOMERID, ORDERDATE)


A search on the value "     123452023" (so a partial date search that matches customer number 12345 and all the orders for 2023)
This gets translated into an expression for SQL Server like this:

    WHERE CUSTNO = 12345 AND Convert(Char,ORDERDATE,112) = '2023'
	
## Descending indexes
We will support descending indexes, but limited:

1) When the DESCEND clause is included in the INDEX ON statements
2) When the Descend function is part of the index expression as OUTER function.
   So this is supported:
   INDEX on CUSTOMERID+DESCEND(DTOS(ORDERDATE)) TO Order1
   
   But this is NOT supported, because DESCEND is nested inside DTOS
   INDEX ON CUSTOMERID+DTOS(DESCEND(ORDERDATE)) TO Order1

Creating indexes in "Select" mode is also supported. 
These indexes will be created after the result set is opened and will be created on the local result set and will be written as CDX indexes 
If you want the "Select" data to be sorted, the easiest approach is to include an ORDER BY class in your select statement.
Seeking "Select" data is only supported when you have added a local index.

	
## Limiting result sets for TableMode

In general it is not a good idea when opening a table to immediately fetch all the rows. 
So the RDD will first create an empty result set with all the columns, so the field names and field types are immediately available.
Real data will only be retrieved when:
- A record movement occurs  (Go Top, Skip)
- A Seek is done (the RDD will then create a WHERE clause)

Still retrieving all columns and all rows can be a bit too much. Therefore the RDD will also try to limit the number of rows and columns
The RDD will call the callback for the # of rows to return  (MaxRecords) and will ask the provider for the syntax to limit the # of rows.
For SQL Server that could be a prefix "Select TOP <n>" but for example for MySql that would be suffix "Limit <n>" and for Oracle " where rowNum <= <n>"
The correct syntax for this will have to be provided by the provider class.
The RDD will also ask the callback class for the list of columns to retrieve. By default all columns will be retrieved. 

## Updates to the data
When running in table mode then the RDD will be R/w. You can choose to either:
- Write back to the server when the record pointer changes or when DbCommit() is called
- Cache the changes locally inside the DataTable and write them in a batch 

- Deletes will be emulated by writing TRUE to the "deleted" column.
- Record number columns will have an incrementing negative value after an append. 
  The real value in the local table will be updated with the server generated number after the data has been written.
  This can be either immediately after a single row update, or after a batch was updated.
  
When running in "Query Mode" then the data in the DataTable is normally Read/Only.
We will provide a mechanism that allows you to write to the DataTable.
However you will have to provide the RDD a mechanism then (again through a callback) with which the data is written to the server.







