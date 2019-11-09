// 694. Compiler crash with undeclared vars passed by reference
FUNCTION Start() AS VOID
undeclared := 12
? undeclared
Test(undeclared)
? undeclared
RETURN

FUNCTION Test(n REF INT) AS VOID
	? n
	n := 123
RETURN
