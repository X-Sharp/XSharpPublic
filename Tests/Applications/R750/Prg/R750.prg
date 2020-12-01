// R750 ByRef dword passed to Clipper Calling Convention
FUNCTION Start() AS VOID STRICT    
    LOCAL iRow AS DWORD
    
    iRow := 100
    ? iRow   
    
    Header(@iRow)   
    xAssert(iRow == 0)
    
    ? iRow
    
    wait
    
    RETURN	
   
FUNCTION Header(iRow)
    ? iRow
    xAssert(iRow == 100)    
    iRow := 0
    ? iRow
    
    RETURN NIL
    

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"    
