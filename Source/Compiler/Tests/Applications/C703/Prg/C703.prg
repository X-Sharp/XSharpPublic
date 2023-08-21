// 703. Name conflicts between memvars and class name

CLASS Test
END CLASS

FUNCTION Start() AS VOID
	LOCAL cKey AS STRING               
	PUBLIC Test
	Test := "abc" // PUBLIC // error XS0118: 'Test' is a type but is used like a variable
	cKey := "b"
	? cKey $ Test // error XS0118
	xAssert(cKey $ Test)
	
	LOCAL cb AS CODEBLOCK
	cb := {|| Test = "abc" } // error XS0118
	xAssert(Eval(cb))
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
