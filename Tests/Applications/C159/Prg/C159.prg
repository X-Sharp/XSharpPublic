// 159. error XS0034: Operator '==' is ambiguous on operands of type '__Usual' and 'IntPtr'
FUNCTION Start() AS VOID
LOCAL u AS USUAL
LOCAL l AS LOGIC

u := NULL_PTR

l := u == NULL_PTR
IF .not. l
	THROW Exception{"Incorrect comparison result"}
END IF

l := u != NULL_PTR
IF l
	THROW Exception{"Incorrect comparison result"}
END IF


