// 714. Problem with "&cVarName" syntax

// Following compiles in VO and gives expected results
FUNCTION Start() AS VOID
	LOCAL cVarname AS USUAL
	cVarname := "abc"
	PRIVATE &cVarname // error XS9002: Parser: unexpected input '&'
	&cVarname := 123
	? &cVarname // 123
	? abc // 123
	
	xAssertTrue(&cVarname == 123)
	xAssertTrue(abc == 123)
RETURN
	
PROC xAssertTrue(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}     
END IF

