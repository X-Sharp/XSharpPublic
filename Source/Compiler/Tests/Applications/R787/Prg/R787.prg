
FUNCTION Start( ) AS VOID
    LOCAL u AS USUAL
    u := {//}
    xAssert(u == NULL_DATE)
    u := {..}
    xAssert(u == NULL_DATE)
    u := {--}              
    xAssert(u == NULL_DATE)    
RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
