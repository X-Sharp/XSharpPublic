//Test static constructors on interfaces -net sdk
FUNCTION Start AS VOID         
	xAssert(Person.hello()=="HELLO")
	xAssert(Person.x == 3)
RETURN

PUBLIC INTERFACE Person
	STATIC CONSTRUCTOR()
		x:=3
	END CONSTRUCTOR
	STATIC x AS INT
	STATIC PUBLIC METHOD hello() AS STRING
		RETURN "HELLO"
END INTERFACE

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
