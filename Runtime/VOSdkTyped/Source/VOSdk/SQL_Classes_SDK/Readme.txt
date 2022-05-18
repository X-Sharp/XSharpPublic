Typed SQL Classes

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

The Factory classes are declared inside XSharp.Data (they are also used in the FoxPro SQL Layer).
There are 3 factory classes at this moment.
ODBCFactory - the default, uses ODBC connection strings
SqlServerFactory - MS SQL Server, uses SQLServer Client connection strings
OleDbFactory - uses OleDb connection strings

The source for these classes is on 
https://github.com/X-Sharp/XSharpPublic/tree/main/Runtime/VOSdkTyped/Source/VOSdk/SQL_Classes_SDK


To connect to a local server with a trusted connection you use different connection strings depending on the factory:
Driver=SQL Server;Trusted_Connection=yes;Server=(local);Database=DatabaseName

A similar connection using the Sql Server factory looks like this:
Server=(local);Database=DatabaseName;Trusted_Connection=True;

We recommend to use a dedicated provider, such as the SqlServerClient when possible because they are usually MUCH faster








