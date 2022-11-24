FUNCTION Start() AS VOID STRICT

	LOCAL DIM st[3] IS TEST_STRUCT
    LOCAL p as TEST_STRUCT
    p := @st
	st[1].x := 1
	st[1].y := 1
	TestFunc(@st[1])
	TestFunc(p)

	st[2].x := 2
	st[2].y := 2
	TestFunc(@st[2])
    p += 1
	TestFunc(p)


	st[3].x := 3
	st[3].y := 3
	TestFunc(@st[3])
	p += 1
	TestFunc(p)

	WAIT

	RETURN

VOSTRUCT TEST_STRUCT
	MEMBER x AS INT
	MEMBER y AS INT
END VOSTRUCT

FUNCTION TestFunc(p AS TEST_STRUCT) AS VOID STRICT
	? p.x
	? p.y
	RETURN
