// 140. warning XS0549: 'TestClass.Test()' is a new virtual member in sealed class 'TestClass'
// /vo3
// Instead of warning, method should be emitted as non-VIRTUAL, even though /vo3 is specified
// No other way for the user to do it..
SEALED CLASS TestClass
METHOD Test() AS VOID
END CLASS

FUNCTION Start() AS VOID

RETURN
