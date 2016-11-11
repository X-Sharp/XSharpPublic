// 294. error XS0127: Since 'TestClass.Xs$Assign$TestAssign(int)' returns void, a return keyword must not be followed by an object expression
// vulcan reports warning property ASSIGN / SET methods do not return a value; return value ignored
FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
o:TestAssign := 123
? o:n

CLASS TestClass
	EXPORT n AS INT
	ASSIGN TestAssign(v AS INT)
	RETURN SELF:n := v
END CLASS
