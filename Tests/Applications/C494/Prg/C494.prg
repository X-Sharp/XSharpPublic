// 494. Compiler crash with ASSIGN without parameters
FUNCTION Start() AS VOID

RETURN

// vulcan reports "ASSIGN methods must have at least one parameter"
CLASS TestClass
ASSIGN testassign1()

ASSIGN testassign2()
ACCESS testaccess2()
RETURN NIL
END CLASS
