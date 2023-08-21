// 698 USUAL IS Type check
FUNCTION Start AS VOID  
    LOCAL oTest as USUAL
	oTest := Error{}
	xAssert( oTest IS Error)
RETURN


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

