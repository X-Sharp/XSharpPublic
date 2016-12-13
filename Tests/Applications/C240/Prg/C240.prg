// 240. error XS0103: The name 'Xs$StaticLocal$StaticLocal$0' does not exist in the current context
// compile with /ns:TestNS
FUNCTION Start() AS VOID
STATIC LOCAL StaticLocal := 1 AS INT
? StaticLocal


CLASS Foo
END CLASS	
