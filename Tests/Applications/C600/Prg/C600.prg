// 600. error XS0208: Cannot take the address of, get the size of, or declare a pointer to a managed type ('object')
/*
Compiling with /unsafe, allows the code to compile. 

Note also that declaring a DIM inside Start() causes a warning XS0162: Unreachable code detected
to be shown, but this is very minor
*/
FUNCTION Start( ) AS VOID
	LOCAL DIM arr[100] AS OBJECT
	arr[5] := 123
	? arr[5]
RETURN

FUNCTION Test() AS VOID
	LOCAL DIM arr[100] AS OBJECT
	arr[15] := 321
	? arr[15]
RETURN 

