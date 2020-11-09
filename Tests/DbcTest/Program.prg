USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

BEGIN NAMESPACE ConsoleApplication1

	FUNCTION Start() AS VOID STRICT
        CoreDb.RddSetDefault("DBFVFP")
        SetDefault("c:\VFF\Samples\data")
        ? "Opening database"
        DbcOpen("Northwind",TRUE,TRUE)
        ? "database opened"
        DbcDump("Northwind")
        DbcClose("Northwind")
        ? "database closed"
        DbcCreate("C:\test\test")
        Console.ReadLine()
	
END NAMESPACE
