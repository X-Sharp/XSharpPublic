// 985. Problems with ARRAY<->FOXARRAY #2090
// https://github.com/X-Sharp/XSharpPublic/issues/2090

FUNCTION Start( ) AS VOID
	LOCAL MyArray[1]
	
	DIMENSION myArray[3,2]
	
	myArray[1,1] := "Matt"
	myArray[1,2] := "Slay"
	myArray[2,1] := "Jimm"
	myArray[2,2] := "Smith"
	myArray[3,1] := "Nancy"
	myArray[3,2] := "Adams"
	
	FOR n := 1 TO ALen(myArray, 1) // error XS1503: Argument 1: cannot convert from 'array' to 'foxarray'
		? myArray[n, 1] + " " + myArray[n, 2] 
	ENDFOR

