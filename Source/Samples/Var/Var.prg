//
// The VAR keyword has been added to the language because in many situations
// the result of an expression will be directly assigned to a local, and the expression
// will already describe the type of the variable
// VAR is a synonym for LOCAL IMPLIED in Vulcan
using System.Collections.Generic

FUNCTION Start AS VOID
// In the next line the compiler "knows" that today is a DateTime
VAR today := System.DateTime.Now
? today

// In the next line the compiler "knows" that text is a String
VAR text  := Convert.ToString(123)
? text

// In the next line the compiler "knows" that s is a string
FOREACH VAR s in GetList()
	? s
NEXT

Console.ReadLine()


RETURN

FUNCTION GetList AS List<String>
VAR aList := List<String>{}
aList:Add("abc")
aList:Add("def")
aList:Add("ghi")
return aList
