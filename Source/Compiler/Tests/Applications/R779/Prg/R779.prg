// Allow enumeration on a USUAL that contains an array.
FUNCTION Start( ) AS VOID
     LOCAL aErrors := GetErrors() AS USUAL                 
	FOREACH oError AS Error IN aErrors
	    xAssert(oError:Description:StartsWith("Error"))
	NEXT     
	RETURN   
	
FUNCTION GetErrors() AS ARRAY	
	LOCAL aErrors AS ARRAY
	aErrors := {}
	AAdd(aErrors, Error{"Error 1"})
	AAdd(aErrors, Error{"Error 2"})
	AAdd(aErrors, Error{"Error 3"})
    RETURN aErrors    
    
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN   
