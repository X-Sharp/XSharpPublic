// 364. Various issues with defining classs members outside of PARTIAL CLASS...END CLASS
// error XS9002: Parser: unexpected input 'CONSTRUCTOR'
// error XS0116: A namespace cannot directly contain members such as fields or methods
// error XS0122: 'TestClass.CursorPos()' is inaccessible due to its protection level
FUNCTION Start() AS VOID
TestClass{}:CursorPos := "Assign called" // error XS0122: 'TestClass.CursorPos()' is inaccessible due to its protection level

PARTIAL CLASS TestClass
END CLASS

METHOD Test() AS VOID CLASS TestClass // OK

ACCESS CursorPos()  CLASS TestClass // OK
RETURN NIL

ASSIGN CursorPos( oNewPos) CLASS TestClass // error XS0116: A namespace cannot directly contain members such as fields or methods
? oNewPos
RETURN oNewPos // compiles ok without this

METHOD Init() CLASS TestClass // error XS9002: Parser: unexpected input 'CONSTRUCTOR'
	? "Init called"

