// 596. VO allows also LOCAL STATIC
FUNCTION Start( ) AS VOID
	Test()
	Test()
	IF .not. Test() == 6
		THROW Exception{"Incorrect result"}
	END IF
RETURN

FUNCTION Test() AS INT
	LOCAL STATIC n1 := 0 AS INT
	LOCAL STATIC n2 AS INT
	n1++
	n2++
RETURN n1 + n2

