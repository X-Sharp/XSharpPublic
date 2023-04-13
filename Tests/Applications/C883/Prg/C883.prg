// 883. Compiler incorrectly allows accessing static members with a colon instead of dot
// There should be 8 errors reported on the following code:
#pragma options("allowdot", false)
FUNCTION Start( ) AS VOID
    TestClass2.TestExport:Invoke(123) // OK
    TestClass2.TestExport.Invoke(123) // error, OK
	TestClass2.TestExport(123) // no Error, OK
	TestClass2.TestProperty:Invoke(123) // OK
	TestClass2.TestProperty.Invoke(123) // error, OK
	TestClass2.TestProperty(123) // no Error, OK

	TestClass:StaticExport := 123 // error, OK
	TestClass:StaticProperty := 123 // error, OK
	TestClass:StaticMethod(123) // error, OK
	TestClass.StaticExport := 123 // no error, OK
	TestClass.StaticProperty := 123 // no error, OK
	TestClass.StaticMethod(123) // no error, OK

	TestClass:DoSomething += MyEventHandler // error, OK
	? System.Environment:CurrentDirectory // error, OK
	? System.Environment:CurrentDirectory:Length // error, OK

FUNCTION MyEventHandler(sender AS OBJECT, args AS EventArgs) AS VOID

CLASS TestClass
	STATIC METHOD StaticMethod(n AS INT) AS INT
	    DoSomething(n, EventArgs.Empty)
	RETURN n * 2
	STATIC EXPORT StaticExport AS INT
	STATIC PROPERTY StaticProperty AS INT AUTO GET SET
	STATIC EVENT DoSomething AS EventHandler
END CLASS

CLASS TestClass2
	STATIC EXPORT TestExport AS TestDelegate
	STATIC PROPERTY TestProperty AS TestDelegate AUTO
END CLASS
DELEGATE TestDelegate(n AS INT) AS VOID

CLASS TestClass3
EXPORT Test1 := DateTime.Now AS DateTime
END CLASS
