// 344. error XS0103: The name 'WinSockExitHandler' does not exist in the current context
#pragma warnings(219, off)
FUNCTION Start( ) AS VOID
	AppDomain.CurrentDomain:ProcessExit += System.EventHandler{ NULL, @WinSockExitHandler() }
RETURN

STATIC FUNCTION WinSockExitHandler( o AS OBJECT, args AS EventArgs ) AS VOID



// there's also a problem with the "new" syntax:

CLASS Foo
	CONSTRUCTOR(oAction AS Action<OBJECT>)
END CLASS

CLASS TestClass
	METHOD Test() AS VOID
		LOCAL o AS Foo
		o := Foo{AlternativeSyntax}
	RETURN
	METHOD AlternativeSyntax(o AS OBJECT) AS VOID
END CLASS
