// 249. error XS1643: Not all code paths return a value in lambda expression of type '<>F<__Usual>'
FUNCTION Test(u AS USUAL) AS VOID
	? Eval(u)
FUNCTION Start() AS VOID
Test({||})

