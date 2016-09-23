//
// The SWITCH statement is a replacement for the DO CASE statement
// The biggest difference is that the expression (in this case sDeveloper) is only evaluated once.
// which will have a performance benefit over the DO CASE statement
//
using System.Collections.Generic

Function Start() as void
    FOREACH VAR sDeveloper in GetDevelopers()
		SWITCH sDeveloper:ToUpper()
		CASE "FABRICE"
			? sDeveloper, "France"
		CASE "CHRIS"
		CASE "NIKOS"
			? sDeveloper, "Greece"
		CASE "ROBERT"
			? sDeveloper, "The Netherlands"
		OTHERWISE
			? sDeveloper, "Earth"
		END SWITCH
	NEXT
    Console.ReadKey()
	RETURN	


FUNCTION GetDevelopers as List<String>
VAR aList := List<String>{}
aList:Add("Chris")
aList:Add("Fabrice")
aList:Add("Nikos")
aList:Add("Robert")
aList:Add("The Roslyn Developers")
RETURN aList
