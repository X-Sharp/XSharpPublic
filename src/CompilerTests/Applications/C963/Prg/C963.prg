// 963. Accessing parent class fields (XBase++) [#1836]
// https://github.com/X-Sharp/XSharpPublic/issues/1836

PROCEDURE main()
    Child():setter(1)
    ? Child():getter()
    xAssert( Child():getter() == 1)

    Child():exportedField := 2
    ? Child():exportedField
    xAssert( Child():exportedField == 2)
RETURN


CLASS Child FROM Parent
endclass


CLASS Parent
exported:
    inline CLASS METHOD initClass()
        ::classField := 0
        ::exportedField := 0
    RETURN

    inline CLASS METHOD getter()
    RETURN ::classField

    inline CLASS METHOD setter(n)
        ::classField := n
    RETURN

    CLASS VAR exportedField

HIDDEN:
    CLASS VAR classField
endclass


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
