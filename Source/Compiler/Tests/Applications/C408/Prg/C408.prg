// 408. error XS0127: Since 'TestClass.Test1(int)' returns void, a return keyword must not be followed by an object expression
// error XS0127: Since 'TestClass.Test2(params __Usual[])' returns void, a return keyword must not be followed by an object expression

// /vo9+ (allow missing RETURN)

// NOTE: Problem is because return type is typed AS System.Void. Changing it to VOID works well

CLASS TestClass
METHOD Test1(n AS INT) AS System.Void STRICT
	? n
RETURN
METHOD Test2() AS System.Void CLIPPER
	? "test2"
RETURN
END CLASS

FUNCTION Test() AS System.Void
	? "test"
RETURN 

FUNCTION Start() AS VOID
TestClass{}:Test1(1)
TestClass{}:Test2(1)
Test()

