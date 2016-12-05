// 318. error XS0136: A local or parameter named 'value' cannot be declared in this scope because that name is used in an enclosing local scope to define a local or parameter
FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
o:MyProp := 123
? o:MyProp

o:UsualProp := "asd"
? o:UsualPropNew

CLASS TestClass
	PROTECT n AS INT
	PROTECT u
	ACCESS MyProp AS INT
	RETURN SELF:n
	ASSIGN MyProp(value AS INT)
	SELF:n := value

	ASSIGN UsualProp(value)
	SELF:u := value

	ACCESS UsualPropNew
	RETURN SELF:u
END CLASS
