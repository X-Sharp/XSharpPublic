// https://github.com/X-Sharp/XSharpPublic/issues/857
FUNCTION Start( ) AS VOID
LOCAL u AS USUAL
LOCAL e AS Error
u := "abc"
e := u ASTYPE Error // ERROR: InvalidCastException
xAssert(e == NULL)
u := Error{}
e := u ASTYPE Error // OK: e is Error (no exception)
xAssert(e != NULL)
RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN 
