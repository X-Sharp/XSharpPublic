// 929. Problem with defining a class with /fox1- disabled
// https://github.com/X-Sharp/XSharpPublic/issues/1611

// /fox1-
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}

// error XS9231: The 'AS BaseType' clause is mandatory in the FoxPro dialect, unless you compile with /fox1-.
DEFINE CLASS TestClass
ENDDEFINE
