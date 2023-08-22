USING System.Runtime.CompilerServices
USING System.Reflection

FUNCTION Start( ) AS VOID
	System.Console.WriteLine("Hello x#!")
	LOCAL oDialect AS CompilerDialect
	oDialect := CompilerDialect{}
	oDialect:SomeMethod()
	oDialect:SomeMethod1()
RETURN


CLASS CompilerDialect
	
CONSTRUCTOR()
	
	RETURN
	
METHOD SomeMethod( [CallerMemberName] cName := NULL AS STRING, [CallerFilePath] cFile := NULL AS STRING, [CallerLineNumber] nLine := 0 AS LONG ) AS VOID
	Console.WriteLine("{0} {1} {2}", cFile, nLine,cName)
	RETURN

METHOD SomeMethod1( cName := "VulcanDefault"  AS STRING ) AS VOID
	Console.WriteLine(cName)
	RETURN


END CLASS	
