
FUNCTION Start() AS VOID
LOCAL s1,s2  AS STRING
s1 := "ABC"
s2 := "AB"

// comparison between strings, result is correctly TRUE for compatibility with VO
xAssert((s1 != s2) == TRUE) // TRUE, correct

// cast ignored, comparison is done again between STRINGs, while it should be done between USUALs
// result is still TRUE, while it should be FALSE for USUALs comparison
XAssert(((USUAL)s1 != (USUAL)s2) == FALSE) // TRUE, wrong

// When using intermediate USUAL vars (instead of cast to usual), then the comparison is now done between USUALs as expected
// and the result is now the correct FALSE
LOCAL u1,u2 AS USUAL
u1 := s1
u2 := s2
xAssert((u1 != u2) == FALSE) // FALSE, correct



PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "assertion passed"
	ELSE
		//? "Assertion failed"
		THROW Exception{"Incorrect result"}
	END IF   
RETURN

