// 673. Various issues with MEMVAR, PUBLIC etc
PUBLIC gPublic := "Global1"
PUBLIC gUndefined

PROCEDURE DoTest()
	PRIVATE SameName

	// there should be an error here that SameName is already declared
	MEMVAR SameName
	// Putting the MEMVAR before the PRIVATE causes the error message to display as it should
RETURN

FUNCTION Start( ) AS VOID

	undeclared := 123
	? undeclared
	xAssertTrue(undeclared == 123) // ok
	


	? gPublic
	xAssertTrue(gPublic == "Global1") // ok
	


	? gUndefined // "NIL", VO prints "FALSE"             
	xAssertTrue(gUndefined == FALSE)
	// this is what VO does, is it intentional???


	
	PUBLIC inline
	inline := 123 // exception here
	xAssertTrue(inline == 123)

	PUBLIC inline2 := 123
	xAssertTrue(inline2 == 123)
	


	PRIVATE pri1
	pri1 := TRUE
	? pri1
	xAssertTrue(pri1 == TRUE)
	


	PRIVATE pri2 := 123
	? pri2 // NIL, wrong
	xAssertTrue(pri2 == 123)
		
RETURN


PROC xAssertTrue(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

