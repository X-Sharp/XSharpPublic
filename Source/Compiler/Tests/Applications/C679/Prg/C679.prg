// error XS9999: An internal compiler error has occurred: 'Object reference not set to an instance of an object.'
// vo2+
FUNCTION Start() AS VOID
	Test()
RETURN

FUNCTION Test() AS VOID
	STATIC LOCAL n1 := 1 // ok
	STATIC LOCAL n2 AS USUAL // ok
	STATIC LOCAL n3 // ICE
	STATIC n4 // ICE
	
	? n1,n2,n3,n4
RETURN
