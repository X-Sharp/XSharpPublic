FUNCTION Start() AS VOID STRICT

	LOCAL DIM st[3] IS TEST_STRUCT
	st[1].x := 1
	st[1].y := 2
	TestFunc(@st[1],1)

	st[2].x := 2
	st[2].y := 4
	TestFunc(@st[2],2)


	st[3].x := 3
	st[3].y := 6
	TestFunc(@st[3],3)

	RETURN

VOSTRUCT TEST_STRUCT
	MEMBER x AS INT
	MEMBER y AS INT
END VOSTRUCT

FUNCTION TestFunc(p AS TEST_STRUCT, nCheck as INT) AS VOID STRICT
	? p.x
	? p.y
	xAssert(p.x == nCheck)
	xAssert(p.y == nCheck*2)
	RETURN


	PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
