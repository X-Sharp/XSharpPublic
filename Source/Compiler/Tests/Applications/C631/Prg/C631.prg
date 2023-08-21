// 631. Incorrect results with iif() and USUAL arguments (sample version with /vo10 disabled)
FUNCTION Start(  ) AS VOID

	LOCAL ulFalse := FALSE AS USUAL
	LOCAL ulTrue := TRUE AS USUAL
	LOCAL lFalse := FALSE AS LOGIC
	LOCAL lTrue := TRUE AS LOGIC
	
	LOCAL l AS LOGIC
	l := iif(lFalse , !ulFalse , !ulFalse)
	? l
	xAssert(l == TRUE)
	
	l := iif(lTrue , .not. ulFalse , .not. ulFalse)
	? l
	xAssert(l == TRUE)

	l := iif(TRUE , .not. ulFalse , FALSE)
	? l
	xAssert(l == TRUE)
	
	l := iif(FALSE , FALSE  ,.not. ulFalse)
	? l
	xAssert(l == TRUE)
	


	LOCAL u AS USUAL
	u := iif(TRUE , !ulFalse , !ulFalse)
	? u
	xAssert(u == TRUE)

	u := iif(lFalse , FALSE , !ulFalse)
	? u
	xAssert(u == TRUE)

	u := iif(lTrue , !ulFalse , FALSE)
	? u
	xAssert(u == TRUE)
	
	
	u := iif(TRUE , !ulTrue , !ulTrue)
	? u
	xAssert(u == FALSE)

	u := iif(FALSE , !ulTrue , !ulTrue)
	? u
	xAssert(u == FALSE)
	
	u := iif(lTrue , !ulTrue , !ulTrue)
	? u
	xAssert(u == FALSE)
	
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

