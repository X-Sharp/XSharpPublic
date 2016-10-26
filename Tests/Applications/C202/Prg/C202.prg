//202. error XS1501: No overload for method 'Left' takes 2 arguments
// vulcan dialect
// does not resolve to the vulcan function
FUNCTION Start() AS VOID
TestClass{}:Left(321)

CLASS TestClass
	METHOD Left(n AS INT) AS VOID
	? Left(n:ToString(),1)
END CLASS

