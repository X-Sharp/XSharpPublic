// 23. Vulcan incompatibility - error XS0673: System.Void cannot be used from X# -- use typeof(void) to get the void type object

CLASS TestClass1
METHOD Test() AS System.Void
END CLASS

FUNCTION Start() AS VOID
