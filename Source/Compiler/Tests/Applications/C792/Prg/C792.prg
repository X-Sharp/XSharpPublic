// 792. C792 - VFP dialect incorrect code generated to assign value to parameter

/* the generated code for the assignment of value 456 is:

XSharp.VFP.Functions.__FoxFillArray(iValue, 456)

when removing the IF block, then the generated code is correct
*/

FUNCTION TestNumeric ( iValue , lValue )
	IF lValue
		iValue := 1
	ENDIF
	iValue := 456  
RETURN iValue

FUNCTION Start( ) AS VOID
? TestNumeric(123 , TRUE)  // returns 1, wrong
? TestNumeric(123 , FALSE) // returns 123, wrong

xAssert( TestNumeric(123 , TRUE) == 456)
xAssert( TestNumeric(123 , FALSE) == 456)

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN NIL
