#pragma warnings(165, off) // unassigned local
VOSTRUCT _VOSTRUCT
MEMBER n AS INT
MEMBER m AS INT

FUNCTION Start() AS VOID
LOCAL u AS USUAL
LOCAL vas AS _VOSTRUCT
LOCAL vis AS _VOSTRUCT

//v := (_VOSTRUCT)MemAlloc(SizeOf(_VOSTRUCT))
u := vas
vas := u

vis.n := 123
u := vis
vis := u
? u
? vis:n
