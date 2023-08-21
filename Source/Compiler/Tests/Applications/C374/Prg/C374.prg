// 374. Unhandled Exception: System.InvalidCastException: Specified cast is not valid.
FUNCTION Start() AS VOID
LOCAL p AS PTR
LOCAL n AS INT
n := 1
p := @n
? p
LOCAL u AS USUAL
u := p
p := u
? u
? p

