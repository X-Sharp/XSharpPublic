// 305. String2Psz() in Start() function
FUNCTION Start() AS VOID
LOCAL p AS PSZ
p := String2Psz("asd")
GLOBAL o,p,q AS error

PROCEDURE InitProc1 _INIT1
	o := Error{}
PROCEDURE InitProc2 _INIT2
	p := Error{}
PROCEDURE InitProc3 _INIT3
	q := Error{}
	
