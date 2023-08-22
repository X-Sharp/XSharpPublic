// 595. error XS0026: Keyword 'SELF' is not valid in a static property, static method, or static field initializer
// error XS0120: An object reference is required for the non-static field, method, or property 'Foo.n'
FUNCTION Start( ) AS VOID
	LOCAL o AS Foo
	o := Foo{}
	? o:TestAcc1
	? o:TestAcc2
	o:TestMethod()
	o:TestAssign := 123
RETURN

PARTIAL CLASS Foo
	EXPORT n AS INT
	VIRTUAL ACCESS TestAcc1 AS INT
		? SUPER:ToString()
	RETURN SELF:n // XS0026
	VIRTUAL ACCESS TestAcc2 AS LOGIC
	RETURN n == 1 // XS0120
	VIRTUAL METHOD TestMethod() AS VOID
		? SUPER:ToString()
		? SELF:TestAcc1
		? SELF:n
	RETURN
	
	ASSIGN TestAssign(o AS INT)
		? SUPER:ToString()
		? SELF:n // ok
	RETURN
	
END CLASS
