// 649. The error messages about namespace/type Abc.Def not found points to line 3 (Start() function) instead of correct lines 8,10,12

FUNCTION Start( ) AS VOID

RETURN

CLASS TestClass
	STATIC METHOD Dummy() AS Abc.Def
	RETURN NULL
	STATIC METHOD Dummy2() AS Abc.Def
	RETURN NULL
	METHOD Dummy3() AS Abc.Def
	RETURN NULL
END CLASS
