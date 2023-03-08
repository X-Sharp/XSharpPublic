// 883. Compiler incorrectly allows accessing static members with a colon instead of dot
// There should be 6 errors reported on the following code:
#pragma options("allowdot", false)
FUNCTION Start( ) AS VOID
	TestClass:StaticExport := 123
	TestClass:StaticProperty := 123
	TestClass:StaticMethod(123)
	TestClass:DoSomething += MyEventHandler
	? System.Environment:CurrentDirectory
	? System.Environment:CurrentDirectory:Length

FUNCTION MyEventHandler(sender as object, args as EventArgs) as void

CLASS TestClass
	STATIC METHOD StaticMethod(n AS INT) AS INT
	    DoSomething(n, EventArgs.Empty)
	RETURN n * 2
	STATIC EXPORT StaticExport AS INT
	STATIC PROPERTY StaticProperty AS INT AUTO GET SET
	STATIC EVENT DoSomething AS EventHandler
END CLASS
