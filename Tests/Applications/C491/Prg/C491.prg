// 491. Some issues with interfaces

// No error, should we allow this?
#pragma warnings(9047, off) // generate auto property
#pragma warnings(9051, off) // generate auto property
#pragma warnings(9032, off) // return value

INTERFACE IEmpty
END INTERFACE

CLASS TestClass
	PROPERTY Prop1 AS INT GET

	PROPERTY Prop2 AS INT GET 0

	// error XS8051: Auto-implemented properties must have get accessors.
	// There should only be a warning here for missing SET body, isn't that right?
	PROPERTY Prop3 AS INT SET

	PROPERTY Prop4 AS INT SET GET

	PROPERTY Prop5 AS INT SET GET 0
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o:Prop1
	? o:Prop2
	o:Prop3 := 123
	o:Prop4 := 123
	? o:Prop4
	o:Prop5 := 123
	? o:Prop5
RETURN
