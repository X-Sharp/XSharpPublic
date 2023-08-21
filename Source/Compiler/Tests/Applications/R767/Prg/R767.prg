// R767
FUNCTION Start( ) AS VOID
    xAssert(GetSize(TRUE) == 1)
    xAssert(GetSize(FALSE) == 2)
RETURN

FUNCTION GetSize(lLandscape AS LOGIC) AS LONG
    VAR aValues := {1,2,3} 
    RETURN aValues[IIF(lLandscape, 1,2)]
    
    
PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN     

