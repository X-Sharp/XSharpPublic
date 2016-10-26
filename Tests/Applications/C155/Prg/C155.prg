// 155. error XS1736: Default parameter value for 'u' must be a compile-time constant
FUNCTION Test(u := NIL AS USUAL) AS VOID
FUNCTION Start() AS VOID
Test(1)
Test(NIL)
Test()

