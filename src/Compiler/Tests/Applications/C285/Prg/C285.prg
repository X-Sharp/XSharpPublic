// 285. error XS1656: Cannot assign to 'Left' because it is a 'method group'
// it works ok from within the function, but not in the method code
GLOBAL Left AS INT

FUNCTION Start() AS VOID
Left := 123 // ok
? Left // ok

CLASS TestClass
METHOD Test() AS VOID
Left := 135 // error XS1656
? Left // error XS1656
END CLASS


