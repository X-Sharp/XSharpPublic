
FUNCTION Start( ) AS VOID
    local cCode := "123" as string
    var c := i"<a href=""{cCode}"">"

    ? c , cCode
    xAssert(c == "<a href=""123"">")
RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
