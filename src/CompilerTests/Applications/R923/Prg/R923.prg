// see https://github.com/X-Sharp/XSharpPublic/issues/1492
PROCEDURE main
    LOCAL a := {1, 2, 3}
    LOCAL nLen := Len(a)
    LOCAL i

    FOR i := 1 TO nLen
        _Increment(@a[i])
    NEXT

    FOR i := 1 TO nLen
        ? a[i]
        xAssert(a[i] == i+1)
    NEXT
RETURN


STATIC PROCEDURE _Increment(nNumber)
    nNumber++
RETURN


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN


