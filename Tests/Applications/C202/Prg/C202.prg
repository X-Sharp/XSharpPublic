//202. error XS1501: No overload for method 'Left' takes 2 arguments
// vulcan dialect
// does not resolve to the vulcan function
FUNCTION Start() AS VOID
TestClass{}:Left(123)
#pragma warnings (9066, disable)
// error XS9066: Symbol 'Left' is ambiguous. Could be 'VulcanRTFuncs.Functions.Left(string, dword)' or 'TestClass.Left(int)'.
// Using the function because in X# functions take precedence over methods. To call the method use the fully qualified name (for static methods) or prefix the call with 'SELF:' for instance methods.
CLASS TestClass
	METHOD Left(n as int) AS VOID
	? Left(n:ToString(),1)
END CLASS

