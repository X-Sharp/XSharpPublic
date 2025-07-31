static global providerName := "ODBC" as string
static global ConnectionString := "Driver={SQL Server};Server=LEDA;Database=Northwind;Trusted_Connection=Yes;"+options as STRING
BEGIN NAMESPACE SqlRDD.ODBCTests

#include "testclass.xh"
END NAMESPACE
