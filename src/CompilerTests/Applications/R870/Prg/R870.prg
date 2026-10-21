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
xAssert(StackFrame{true}:GetFileLineNumber() , 12)

PROC xAssert(nResult AS INT, nExpected AS INT)  AS VOID
	IF nExpected == nResult
		? "Assertion passed"
	ELSE
		THROW Exception{ System.String.Format("Incorrect result, expected {0}, returned {1}", nExpected, nResult)}
	END IF
RETURN

