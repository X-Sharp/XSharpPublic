USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

BEGIN NAMESPACE tester

	FUNCTION Start() AS VOID
		var @@local := TimeZoneInfo.Local 
		VAR info := @@local:BaseUtcOffSet
        Console.WriteLine(info:TotalHours)   
        Console.ReadKey()
	
END NAMESPACE
