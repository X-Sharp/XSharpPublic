// 704. Name conflicts between memvars and functions
FUNCTION Test()
RETURN NIL

FUNCTION Start() AS VOID
	__MemVarDecl("test" , FALSE)
	__MemVarPut("test" , "abc")
	__MemVarDecl("another" , FALSE)
	__MemVarPut("another" , "abc")
//	cb := {|| TRUE .and. Upper(test) = "ABC"} // error XS1503: Argument 1: cannot convert from 'method' to 'string'
	xAssert(Eval(cb))
//	cb := {|| TRUE .and. test = "abc"} // error XS0019: Operator '==' cannot be applied to operands of type 'method' and 'string'
	xAssert(Eval(cb))
	// if only this version is included, then the "Failed to emit module" is reported:
	cb := {|| TRUE .and. test = another} // error XS7038: Failed to emit module 'C704'.
	xAssert(Eval(cb))
RETURN




PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
