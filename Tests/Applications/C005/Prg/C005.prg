// 5. error XS0558: User-defined operator 'TestClass.operator +(TestClass, TestClass)' must be declared static and public
// In vulcan the STATIC keyword is not required.

FUNCTION Start() AS VOID

RETURN

CLASS TEstClass
	OPERATOR +( a AS TEstClass, b AS TEstClass ) AS TEstClass
	RETURN NULL
END CLASS
