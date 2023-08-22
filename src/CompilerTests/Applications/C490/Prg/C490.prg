// 490. Bogus warning XS9047: A get or set accessor must have a body, an auto property will be generated
// warnings as errors
INTERFACE ITest
PROPERTY Prop1 AS INT GET
PROPERTY Prop2 AS INT SET
PROPERTY Prop3 AS INT GET SET
END INTERFACE

CLASS TestClass IMPLEMENTS ITest
	EXPORT n AS INT
	PROPERTY Prop1 AS INT GET n
	PROPERTY Prop2 AS INT SET n := VALUE
	PROPERTY Prop3 AS INT SET SELF:n := VALUE GET n
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	o:Prop2 := 123
	? o:Prop1
	o:Prop3 := 456
	? o:Prop3
RETURN

