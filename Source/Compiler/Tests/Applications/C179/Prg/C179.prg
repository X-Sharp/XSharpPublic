// 179. Assertion failed and compiler crash with event in vulcan dialect
// works ok in core, crash in vulcan/vo
#pragma warnings(67, off) // event is never used
CLASS TestClass
	STATIC EVENT TestEvent AS EventHandler
END CLASS
FUNCTION Start( ) AS VOID
Foo{}

CLASS Foo
CONSTRUCTOR()
TestClass.TestEvent += Eventhandler{SELF, @Handler()}
TestClass.TestEvent += Handler
METHOD Handler(sender AS OBJECT, e AS EventArgs) AS VOID
END CLASS

