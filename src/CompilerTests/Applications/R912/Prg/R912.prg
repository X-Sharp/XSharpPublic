// https://github.com/X-Sharp/XSharpPublic/issues/1357
procedure Main()
    local o := SomeClass():new()

    xAssert(o:Hello() == "Hello abc" )

return


class SomeClass from Base
exported:
    inline method Init()
        ::a := "abc"
    return super:Init()

    inline method Hello()
        local c
        c := "Hello "+::a
        ? c
    return c

    var a
endclass


class Base
endclass


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

