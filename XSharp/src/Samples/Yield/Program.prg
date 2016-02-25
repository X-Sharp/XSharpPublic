using System.Collections.Generic

FUNCTION Start AS VOID
FOREACH VAR nValue IN ListOfInt()
   Console.WriteLine( nValue )
NEXT
Console.ReadLine()
RETURN

FUNCTION ListOfInt AS IEnumerable<Int>
LOCAL nLoop as LONG
// This function returns a sequence of numbers
// Note that there is NEVER a collection in memory
FOR nLoop := 1 to 1000
   YIELD RETURN nLoop
NEXT
