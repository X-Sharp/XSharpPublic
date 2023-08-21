// https://github.com/X-Sharp/XSharpPublic/issues/1029
function start as Void
    var text := "abc"
    text += "123"
    text += "123"
    text += "123"
    ? text
    xAssert(text == "abc123123123")


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

