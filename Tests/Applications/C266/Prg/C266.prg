// compiler crash
// vulcan dialect
FUNCTION Start() AS VOID
LOCAL u AS USUAL
u := TestClass{}
((TestClass)u):TestMethod()

CLASS TestClass
METHOD TestMethod() AS VOID STRICT
? "test"
END CLASS 
