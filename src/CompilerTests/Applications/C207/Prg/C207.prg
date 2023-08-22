// 207. error XS9002: Parser: no viable alternative at input 'Default'
FUNCTION Start() AS VOID
	TestClass{}:Default()

CLASS TestClass
	METHOD Default() AS VOID
	METHOD Test() AS VOID
		SELF:Default()
END CLASS

