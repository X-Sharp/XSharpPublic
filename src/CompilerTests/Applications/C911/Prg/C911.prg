// 911. enforceself compiler option is being ignored
// https://github.com/X-Sharp/XSharpPublic/issues/1503

// must report errors

#pragma options (enforceself, on)

FUNCTION Start( ) AS VOID

CLASS TestClass
	PROTECT n AS INT
	EXPORT o AS OBJECT
	CONSTRUCTOR()
		? o == NULL
		? TestMethod()
	RETURN

	METHOD Test() AS VOID
		n := 1
		? n
		? TestMethod()
	RETURN
	METHOD TestMethod AS INT=> 1
END CLASS
