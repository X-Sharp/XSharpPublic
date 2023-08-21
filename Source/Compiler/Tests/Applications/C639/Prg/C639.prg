// 639. Integer divisions with /vo12 option
FUNCTION Start() AS VOID
	LOCAL n,m AS INT
	LOCAL r AS REAL8
	
	? 2/3
	xAssertEquals( 2/3 , 2.0/3.0 )
	xAssertEquals( REAL8(2/3) , REAL8(2.0/3.0) )


	n := 2
	m := 3
	? n/m
	r := n/m
	? r
	xAssertEquals( r , REAL8(2.0/3.0) )
RETURN

PROC xAssertEquals(o1 AS USUAL, o2 AS USUAL)
? o1,o2
IF REAL8(o1) == REAL8(o2)
	? "Assertion passed"
ELSE
	THROW Exception{String.Format("Incorrect result, expected {0}, returned {1}" , o2 , o1)}
END IF


