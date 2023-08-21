// 714. Problem with "&cVarName" syntax

// Following compiles in VO and gives expected results
FUNCTION Start() AS VOID
	LOCAL cVarname AS USUAL
	MEMVAR abc1
	cVarname := "abc"
	PRIVATE &cVarname // error XS9002: Parser: unexpected input '&'
	&cVarname.1 := 123
	? &cVarname.1 // 123
	? abc1 // 123
	? &cVarName.1
	xAssertTrue(&cVarname.1 == 123)
	xAssertTrue(abc1 == 123)
RETURN
	
PROC xAssertTrue(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}     
END IF

