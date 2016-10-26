// 160. error XS0034: Operator '==' is ambiguous on operands of type '__Usual' and '__VODate'
FUNCTION Start() AS VOID
LOCAL u AS USUAL
LOCAL l AS LOGIC

u := NULL_DATE // this works ok, unlike NULL_PTR

l := u == NULL_DATE
IF .not. l
	THROW Exception{"Incorrect comparison result"}
END IF

l := u != NULL_DATE
IF l
	THROW Exception{"Incorrect comparison result"}
END IF

