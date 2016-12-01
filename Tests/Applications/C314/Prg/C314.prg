// 314. Incorrect warnings reported
// warning XS9020: Narrowing conversion from 'int' to 'real8' may lead to loss of data or overflow errors
// warning XS9020: Narrowing conversion from 'dword' to '__VOFloat' may lead to loss of data or overflow errors
// etc
// /vo4+, /warnaserror
FUNCTION Start() AS VOID
LOCAL dw AS DWORD
dw := 2
IF dw > 1
	? dw
END IF
LOCAL u AS USUAL
u := 1
? u
? Left("asd",dw-1)

