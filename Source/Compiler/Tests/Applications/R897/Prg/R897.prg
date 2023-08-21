// INIT and EXIT Procedure in Clipper/XBase++ style
// https://github.com/X-Sharp/XSharpPublic/issues/1290
static avalues[0] as array
init procedure Input()
    ? "Input"
    aadd(avalues, "Input")
return

procedure Main()
    aadd(avalues, "Main")
    xAssert(Alen(avalues) == 2)
    xAssert(avalues[1] == "Input")
    xAssert(avalues[2] == "Main")
    ? "Main"
    return

exit procedure Output()
    aadd(avalues, "Output")
    ? "Output"
    xAssert(Alen(avalues) == 3)
    xAssert(avalues[3] == "Output")
return


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
