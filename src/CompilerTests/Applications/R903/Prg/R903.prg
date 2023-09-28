#pragma options("allowdot", on)
#pragma options("lb", on)
FUNCTION Start as Void
    local e as usual
    local fld as string
    e := Error{"Foo"}
    with e
        xAssert(:Description == "Foo")
        fld := "Description"
        xAssert(:&fld== "Foo")
        xAssert(.&fld== "Foo")
        fld := "Descript"
        xAssert(.&(fld+"ion")== "Foo")
        xAssert(  .&(fld+"ion") == "Foo")
    end with
    return
PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
