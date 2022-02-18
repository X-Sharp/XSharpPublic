// https://github.com/X-Sharp/XSharpPublic/issues/951
FUNCTION Start( ) AS VOID
    LOCAL f := -1343.345 AS FLOAT  // "AS DOUBLE" throws an compile error
    
    try
        TestFloat ( (BYTE)  f ) 
        xAssert(FALSE)
    catch  
    // this is expected
    xAssert(TRUE)
    end try        
    
    try
        f := 12345673.89
        xAssert(FALSE)
        
        TestFloat ( (BYTE) f ) 
    catch  
    // this is expected
    xAssert(TRUE)
    end try        

    try
        f := 42
        xAssert(TRUE)
        
        TestFloat ( (BYTE) f ) 
    catch  
    // this is expected
    xAssert(FALSE)
    end try      
RETURN


FUNCTION TestFloat ( n AS WORD ) AS INT  
    ? n   // shows 193 and 73
RETURN 0

PROC xAssert(l AS LOGIC)
    IF .not. l
        THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
    END IF
    ? "Assertion passed"
RETURN    	
