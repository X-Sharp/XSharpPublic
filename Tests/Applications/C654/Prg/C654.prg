// 654. Compiler crash with function with negative asnd postfixed default value 
FUNCTION Start() AS VOID
	Test1()
	Test1(100)
	Test2()
	Test2(100)
	Test3()
	Test3(100)
RETURN

FUNCTION Test1(n := -1L AS INT) AS VOID
	? n
FUNCTION Test2(n := -2L) // that should be an error?
	? n
RETURN NIL
FUNCTION Test3(n := -3U AS INT) AS VOID
	? n

