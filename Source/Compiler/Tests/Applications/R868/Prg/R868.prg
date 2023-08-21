USING System

FUNCTION Start() AS VOID STRICT
? "Hello World! Today is ",Date()
local cName as string
cName := "myPublicVar"
public &cName := 42    AS LONG
private MyPrivate := 4242
? &cName
? myPrivate

xAssert(m->myPublicVar == 42)
xAssert(MyPrivate == 4242)
xAssert(&cName  == 42)


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
