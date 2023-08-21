FUNCTION Start() AS VOID
	LOCAL x 
	x := "old"     
	TestParameters( @x )
    xAssert(x == "PARAMETERS")

FUNCTION TestParameters()
	PARAMETERS a          
	a := "PARAMETERS"
RETURN 0	
PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
