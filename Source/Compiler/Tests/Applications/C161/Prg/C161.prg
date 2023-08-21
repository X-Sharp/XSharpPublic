// 161. error XS1750: A value of type 'int' cannot be used as a default parameter because there are no standard conversions to type '__Usual'
FUNCTION Test(u := 1 AS USUAL) AS VOID
FUNCTION Start() AS VOID
Test()
Test(1)

