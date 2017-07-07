FUNCTION Start AS VOID
	LOCAL oFoo AS Foo
	oFOO :=  Foo{}
	oFoo:Name := "Robert"    
	? oFoo:Name
	Console.ReadLine()
RETURN

PARTIAL CLASS Foo INHERIT Bar
	PUBLIC ASSIGN Name (cName AS STRING)
		? "Foo", cName
		SUPER:Name := cName
		RETURN 
END CLASS

PARTIAL CLASS Bar
	PUBLIC ASSIGN Name (cName AS STRING)
		? "Bar", cName
		RETURN 
END CLASS
