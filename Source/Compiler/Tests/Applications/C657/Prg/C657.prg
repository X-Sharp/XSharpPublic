// 657. Problem with the <= operator comparing USUAL and 2 STRIGNS added in the same expression
FUNCTION Start( ) AS VOID
	LOCAL cAB,cC AS STRING
	LOCAL u AS USUAL
	u := "ABC"
	cAB := "AB"
	cC := "C"
	
	? u >= cAB + cC // TRUE, correct
	? u <= cAB + cC // FALSE, should be TRUE

	xAssert(u <= cAB + cC)
	xAssert(u <= cAB + "C")

	xAssert(u >= cAB + cC)
	xAssert(u >= cAB + "C")
	xAssert(u == cAB + cC)
	xAssert(u == cAB + "C")
	xAssert(u = cAB + cC)
	xAssert(u = cAB + "C")
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

