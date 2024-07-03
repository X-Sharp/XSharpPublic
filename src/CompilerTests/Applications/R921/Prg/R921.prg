// see https://github.com/X-Sharp/XSharpPublic/issues/1487
// make sure no extra trailing white space is added for Extended Expression Match Marker
#translate PN(<file>, <(field)>) => GF(<file>, <(field)>)
FUNCTION Start( ) AS VOID
    local handle := 1 as long
	var f := PN(handle,F_NAME ) // one space before right paranthesis
	xAssert(f == 6)
	f := PN(handle,F_NAME)   // no space before right paranthesis
    xAssert(f == 6)
RETURN

function GF(handle as long, name as string) as long
    if name == null
        return 0
    endif
    return name:Length


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
