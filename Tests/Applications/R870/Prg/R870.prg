using System.Diagnostics
FUNCTION Start() AS VOID
LOCAL c AS STRING
c := "a" +;
; // check1
; // check2
; // check3
"b"

? c
? StackFrame{true}:GetFileLineNumber()
xAssert(StackFrame{true}:GetFileLineNumber() == 12)
PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

