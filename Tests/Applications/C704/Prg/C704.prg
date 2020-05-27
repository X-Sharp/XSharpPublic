// 704. Name conflicts between memvars and functions
FUNCTION Test()     
	? "Test is called"
RETURN TRUE

FUNCTION Start() AS VOID 
	// Not that if you do this in code then you will have to prefix test and another with m->
	// the MEMVAR statement now only allocates the memvar but it also tells the compiler how to resolve
	// the memvar.
	MEMVAR test, another
	test := "abc"
	another := "abc"
	var cb := {|| TRUE .and. Upper(test) = "ABC"} // error XS1503: Argument 1: cannot convert from 'method' to 'string'
	xAssert(Eval(cb))
	cb := {|| TRUE .and. test = "abc"} // error XS0019: Operator '==' cannot be applied to operands of type 'method' and 'string'
	xAssert(Eval(cb))
	// if only this version is included, then the "Failed to emit module" is reported:
	cb := {|| TRUE .and. test = another} // error XS7038: Failed to emit module 'C704'.
	xAssert(Eval(cb))   
	// the compiler now knows top distinguish between the variable and the method call
	xAssert(test()) 
	xAssert(Functions.test()) 
RETURN




PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
