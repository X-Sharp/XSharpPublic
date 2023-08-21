
FUNCTION Start( ) AS VOID
	LOCAL ARRAY arr(1,1), arr2[1,2]        	
    //DIMENSION arr(1,1), arr2(1,2)        	
    xAssert(1U== ALen(arr))
    xAssert(1U== ALen(arr,0))
    xAssert(1U== ALen(arr,1))
    xAssert(1U== ALen(arr,2))

    xAssert(2U== ALen(arr2))
    xAssert(2U== ALen(arr2,0))
    xAssert(1U== ALen(arr2,1))
    xAssert(2U== ALen(arr2,2))

    DIMENSION arr(3,1)         
    xAssert(3U== ALen(arr,0))
    xAssert(3U== ALen(arr,1))
    xAssert(1U== ALen(arr,2))
    DIMENSION arr(1,3)                                
    xAssert(3U== ALen(arr,0))
    xAssert(1U== ALen(arr,1))
    xAssert(3U== ALen(arr,2))
RETURN             


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
