
FUNCTION Start( ) AS VOID
    LOCAL M.var1 
    PRIVATE M.var2
    M.var1 = 10
    M.var2 = M.var1
    xAssert (M.var1 = 10)
    xAssert (M.var2 = 10)
    xAssert (M.var1 = M.Var2)
   
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

