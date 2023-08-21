// 171. error XS1501: No overload for method 'Eval' takes 2 arguments
// function call is not being resolved to the Eval() function
#pragma warnings(9066, disable)
CLASS TestClass
METHOD Test() AS VOID
	LOCAL u := 1 AS USUAL
	// eval() is also a function
	Eval(u,u)
RETURN
METHOD Eval(n AS INT) AS VOID
	LOCAL u := 1 AS USUAL
	Eval(u,n)
RETURN
END CLASS

FUNCTION Start() AS VOID
