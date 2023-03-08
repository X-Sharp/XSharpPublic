// 883. Compiler incorrectly allows accessing static members with a colon instead of dot
// There should be 4 errors reported on the following code:
FUNCTION Start( ) AS VOID
	TestClass:StaticExport := 123
	TestClass:StaticMethod(123)
	? System.Environment:CurrentDirectory
	? System.Environment:CurrentDirectory:Length

CLASS TestClass
	STATIC METHOD StaticMethod(n AS INT) AS INT
	RETURN n * 2
	STATIC EXPORT StaticExport AS INT
END CLASS
