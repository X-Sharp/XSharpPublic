// R789 - FoxPro Public arrays
// https://github.com/X-Sharp/XSharpPublic/issues/662
//MEMVAR pubarray
FUNCTION Start( ) AS VOID 

	CreatePublicArray()
	
	IF IsArray ( pubarray )
        xAssert( pubarray IS __FOXARRAY )
 
		? "i am a public "
		IF pubarray IS __FOXARRAY 
			?? "fox array - "
		ELSE 
			?? "none fox array - "		
		ENDIF		             
		
		?? ClassName ( pubarray )
		
	ENDIF 

	RETURN 

FUNCTION CreatePublicArray() AS VOID 
    PUBLIC ARRAY pubarray [2,2] // AS __FOXARRAY 	//  creates always a none fox array
	RETURN 	
	
	
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN	
