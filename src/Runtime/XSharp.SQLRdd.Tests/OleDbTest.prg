static global providerName := "OleDb" as string
static global ConnectionString := "Provider=sqloledb;Data Source=LEDA;Initial Catalog=Northwind;Integrated Security=SSPI;"+options as STRING
BEGIN NAMESPACE SqlRDD.OLEDBTests

#include "testclass.xh"
END NAMESPACE
