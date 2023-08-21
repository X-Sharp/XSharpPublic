// https://github.com/X-Sharp/XSharpPublic/issues/911

FUNCTION Start( ) AS VOID
LOCAL aList
LOCAL M.iterator  
LOCAL i AS INT 
aList := {41,42,43}

FOREACH M.iterator  IN aList
    ++ i
    SWITCH i
    CASE 1
        xAssert(M.iterator == 41)        
    CASE 2
        xAssert(M.iterator == 42)
    CASE 3                       
        xAssert(M.iterator == 43)
    END SWITCH            
    
ENDFOR

RETURN 



PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN 
