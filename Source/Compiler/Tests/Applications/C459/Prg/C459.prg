// 459. error XS1501: No overload for method 'Test' takes 3 arguments

// low priority of course, but the error is misleading, as there actually _is_ an overload that takes 3 arguments

FUNCTION Start() AS VOID
Test(1,,3)
RETURN

FUNCTION Test(a AS INT , b AS INT) AS VOID
	? a,b
RETURN 
FUNCTION Test(a AS INT , b AS INT, c AS INT) AS VOID
	? a,b,c
RETURN 

