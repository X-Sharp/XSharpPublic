// 658. error XS1622: Cannot return a value from an iterator. Use the yield return statement to return a value, or yield break to end the iteration.
// (following is the sample for YIELD in the help file. Error occurs when /vo9+ is enabled

USING System.Collections.Generic
 
// The Yield return statement allows you to create code that returns a 
// collection of values without having to create the collection in memory first.
// The compiler will create code that "remembers" where you were inside the 
// loop and returns to that spot. 
FUNCTION Start AS VOID
  FOREACH nYear AS INT IN GetAllLeapYears(1896, 2040)
     ? "Year", nYear, "is a leap year."
  NEXT
  Console.ReadLine()
RETURN
 
FUNCTION GetAllLeapYears(nMin AS INT, nMax AS INT) AS IEnumerable<INT>
  FOR LOCAL nYear := nMin AS INT UPTO nMax
    IF nYear % 4 == 0 .and. (nYear % 100 != 0 .or. nYear % 400 == 0)
        YIELD RETURN nYear
    END IF
    IF nYear == 2012
        YIELD EXIT     // Exit the loop
    ENDIF
  NEXT

