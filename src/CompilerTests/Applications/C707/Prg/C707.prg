// 707. Name conflict between memvar and class
// slightly diffrnt case than #703
FUNCTION Start() AS VOID    
	MEMVAR PushButton
	PushButton := PushButton{}
	// error XS0120: An object reference is required for the non-static field, method, or property 'PushButton.HyperLabel'
	? PushButton:HyperLabel
	xAssert(PushButton:HyperLabel == 0)
	
RETURN

CLASS PushButton
	EXPORT HyperLabel AS INT
END CLASS


PROC xAssert(l AS LOGIC) 
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN
