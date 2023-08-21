FUNCTION Start() AS VOID
	LOCAL a := "Robert" AS STRING
    LOCAL b := 1 AS LONG
    xAssert(Type("b") == "N")
    xAssert(Type("a") == "C")
    xAssert(Type("b") == "N")
    ? a
    ? b            
    ? &("b := 42")        
    xAssert(a == "Robert")
    xAssert(b == 42)
RETURN

PROC xAssert(l AS LOGIC) 
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN
