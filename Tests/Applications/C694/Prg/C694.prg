// 694. Compiler crash with undeclared vars passed by reference
FUNCTION Start() AS VOID
undeclared := 12
? undeclared
TestUntyped(undeclared)
? undeclared
TestUntyped(@undeclared)
? undeclared
RETURN

//FUNCTION Test(n REF INT) AS VOID
//	? n
//	n := 123
//RETURN


FUNCTION TestUntyped(n ) AS VOID
	? n
	n := 123
RETURN
