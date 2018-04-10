// 598. (1,153): error XS1715: 'TestClass.TestProp': type must be 'object' to match overridden member 'ParentClass.TestProp'
// no line/col information shown for the error message
FUNCTION Start() AS VOID

RETURN

CLASS ParentClass
	VIRTUAL PROPERTY TestProp AS OBJECT GET NULL
END CLASS

PARTIAL CLASS TestClass INHERIT ParentClass
	ACCESS TestProp
	RETURN NULL
END CLASS



