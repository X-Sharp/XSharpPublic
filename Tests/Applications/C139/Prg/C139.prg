// 139. error XS0050: Inconsistent accessibility: return type 'IntClass' is less accessible than method 'TestClass.Test2()'
#pragma warnings(50, off) // inconsistend accessibility
CLASS TestClass
PROTECTED METHOD Test() AS IntClass
RETURN NULL
PUBLIC METHOD Test2() AS IntClass
RETURN NULL
END CLASS

INTERNAL CLASS IntClass
END CLASS

FUNCTION Start() AS VOID
