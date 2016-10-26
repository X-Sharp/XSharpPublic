// 158. error XS0037: Cannot convert null to '__Usual' because it is a non-nullable value type
FUNCTION Start() AS VOID
LOCAL u AS USUAL
LOCAL l AS LOGIC

u := NULL

l := u == NULL
IF .not. l
	THROW Exception{"Incorrect comparison result"}
END IF

l := u != NULL
IF l
	THROW Exception{"Incorrect comparison result"}
END IF

