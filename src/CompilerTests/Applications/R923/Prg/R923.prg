// see https://github.com/X-Sharp/XSharpPublic/issues/1492
procedure main1
    local a := {1, 2, 3}
    local nLen := Len(a)
    local i

    for i := 1 to nLen
        _Increment(@a[i])
    next

    for i := 1 to nLen
        ? a[i]
        xAssert(a[i] == i+1)
    next
return


static procedure _Increment(nNumber)
    nNumber++
return


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN


