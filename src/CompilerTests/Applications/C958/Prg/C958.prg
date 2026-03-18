// 958. Cast from usual nil to Nullable<int>
// https://github.com/X-Sharp/XSharpPublic/issues/1807

FUNCTION Start() AS VOID
	LOCAL i AS INT?
	LOCAL u := NIL AS USUAL
	i := u
	? i:HasValue
	? IsNil(i)
	xAssert(i:HasValue == FALSE)
	xAssert(IsNil(i) == TRUE)

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

