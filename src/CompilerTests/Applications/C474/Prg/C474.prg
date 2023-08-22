// 474. No compiler error with incorrect PROPERTY syntax
#pragma warnings(9047, off) // auto property
FUNCTION Start( ) AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o:Test1
	o:Test3 := 123
	? o:Test3
RETURN

// is this not a syntax error? :
CLASS TestClass
	PROPERTY Test1 AS INT GET
	//PROPERTY Test2 AS INT SET // error, ok
	PROPERTY Test3 AS INT GET SET
END CLASS

