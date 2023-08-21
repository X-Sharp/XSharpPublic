// 705. Unclear error message when explicitly calling Axit() (ported from VO code)

// /vo1+
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	// error message is not descriptive of the problem in the line below:
	o:Axit() // error XS0201: Only assignment, call, increment, decrement, await, and new object expressions can be used as a statement
RETURN

CLASS TestClass
END CLASS
