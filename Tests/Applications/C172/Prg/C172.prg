// 172. error XS0218: In order for '__Usual.operator |(__Usual, __Usual)' to be applicable as a short circuit operator, its declaring type '__Usual' must define operator true and operator false
#pragma warnings(219, off) // assigned but never used
FUNCTION Start() AS VOID
LOCAL u AS USUAL
LOCAL l AS LOGIC
u := TRUE
l := u .OR. u

