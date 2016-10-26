// 162. error XS1763: 'o' is of type 'object'. A default parameter value of a reference type other than string can only be initialized with null
FUNCTION Test(o := 1 AS OBJECT) AS VOID
FUNCTION Start() AS VOID
Test()
Test(1)

