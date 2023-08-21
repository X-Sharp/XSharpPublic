// 315. Compiler crash and incorrect warnings reported
// /vo4+, /warnaserror
FUNCTION Start() AS VOID
LOCAL dw AS DWORD
dw := 2
IF dw > 1
	? dw
END IF
LOCAL r AS REAL8
LOCAL f AS FLOAT
r := dw
f := dw
? r,f
LOCAL u AS USUAL
u := 1
? u
? Left("asd",dw-1)

