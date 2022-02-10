#define VERSION_MIN 3
#define TARGET_VER  1
FUNCTION Start( ) AS VOID                
    #if 1 ^ 2
    xAssert(TRUE)
    #else
    xAssert(FALSE)    
    #endif
    #if TARGET_VER < VERSION_MIN  
        #stdout version is correct
    xAssert(TRUE)
    #else
    xAssert(FALSE)    

     #endif 

     #if UNDEFINED        // undefined tokens evaluate to false
     xAssert(FALSE)    
     #else                        
     #stdout UNDEFINED is (as the name indicates) undefined
     xAssert(TRUE)    
     #endif

     #if .F. 
        xAssert(FALSE)    
     #stdout This never shows up   
     #else
     xAssert(TRUE)    
     #stdout This is always shown because .F. = false

     #endif 

 

     #if .T. .AND. 1>0 .AND. "AA" < "BB" 

     #stdout This is always true 
     xAssert(TRUE)
     #else
     xAssert(FALSE)

     #endif 

 

     #if 1 < "2" 

     #stdout A numeric will be converted to string before the comparison. 
    xAssert(TRUE)      
    #else
     xAssert(FALSE)
     #endif 

 

     #if 1 == .T. 

     #stdout A logic literal will be converted to numeric before the comparison. 
     xAssert(TRUE) 
     #else
     xAssert(FALSE)
     #endif 

RETURN


PROC xAssert(l AS LOGIC) AS VOID
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
