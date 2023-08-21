// https://github.com/X-Sharp/XSharpPublic/issues/875
#pragma options("lb", on)
Function Start() as void strict
    local obj as object
    obj := MyClass{}
    // This throws a "No exported method" Exception
    xAssert(obj:Go() == "sub")
    obj := MyBaseClass{}
    // This throws a "No exported method" Exception
    xAssert(obj:Go() == "base")

    return

class MyBaseClass // This class we cannot touch (e.g., in .NET library)
    // Method NOT virtual, so cannot be overridden, only hidden.
    public method Go() as string strict
        ? "in base"
        return "base"
end class

class MyClass inherit MyBaseClass
    public new method Go() as string strict
        ? "in sub"
        return "sub"
end class


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
