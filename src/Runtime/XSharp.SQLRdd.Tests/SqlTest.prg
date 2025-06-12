static global providerName := "SQLServer" as string
static global ConnectionString := "Server=(local);Initial catalog=Northwind;Trusted_Connection=True;LegacyFieldTypes=False;"+options as STRING

BEGIN NAMESPACE SqlRDD.SQLTests

#include "testclass.xh"
END NAMESPACE
