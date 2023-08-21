// 746. error XS0131: The left-hand side of an assignment must be a variable, property or indexer
FUNCTION Start() AS VOID
	
	LOCAL variable1, macrostring AS STRING
	variable1 = "old string"
	macrostring = "variable1"
	
	&macrostring := "new string" // ok
	? variable1
	xAssert(variable1 == "new string")
	
	&macrostring = "newer string" // error
	? variable1
	xAssert(variable1 == "newer string")
	
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

