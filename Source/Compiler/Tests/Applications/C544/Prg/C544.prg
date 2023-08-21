// Robert, I think that behavior (error that property hides property) is corrrect, the error goes away when you enable /vo3 (virtual instance methods)
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
	PUBLIC ACCESS Name AS STRING
		RETURN SUPER:Name
END CLASS

PARTIAL CLASS Bar
	PUBLIC ASSIGN Name (cName AS STRING)
		? "Bar", cName
		RETURN 
END CLASS
