// 782. Compiler error with PRIVATE/PUBLIC class vars defined mid class in core dialect
/*
C782.prg(9,17): warning XS9230: The 'AS <DataType>' clause is not supported and ignored.
C782.prg(9,2): error XS9007: Feature 'Dynamic Memory Variables' is not available in the selected dialect Core
C782.prg(14,18): warning XS9230: The 'AS <DataType>' clause is not supported and ignored.
C782.prg(14,2): error XS9007: Feature 'Dynamic Memory Variables' is not available in the selected dialect Core
C782.prg(15,18): warning XS9230: The 'AS <DataType>' clause is not supported and ignored.
C782.prg(15,2): error XS9007: Feature 'Dynamic Memory Variables' is not available in the selected dialect Core
*/
#pragma warnings(169, off) //   field never used
CLASS Foo
	PRIVATE pre AS INT

	CONSTRUCTOR()
		SELF:post := 1
		SELF:post2 := 2
	RETURN

	PUBLIC pre2 AS INT

	METHOD mid() AS INT
	RETURN SELF:post + SELF:post2

	PRIVATE post AS INT
	PUBLIC post2 AS INT

END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS Foo
	o := Foo{}
	? o:mid()
RETURN
