// Calling method from parent class through the generated access that returns a parent object
// https://github.com/X-Sharp/XSharpPublic/issues/1338
#pragma warnings(9201, off)   // SHARED clause
#pragma warnings(108, off)    // DerivedExample.a' hides inherited member 'Example.a'. Use the new keyword if hiding was intended.
procedure Main()
    local o

    o := DerivedExample906():new()
    xAssert(o:a == 4)
    xAssert(o:Example906:a == 5)
    xAssert(o:Name() == "DerivedExample:Name():Example Example")
    xAssert(o:Example906:Name()=="Example")
    xAssert(o:OldName() == "DerivedExample:OldName(): Example Example")


    return

class DerivedExample906 from Example906
exported:
    inline method Init()
        super:Init()
        ::a := 4
        ::Example906:a := 5
        ? "DerivedExample:Init(): ", ::a, super:a, ::Example906:a
        return self

    inline method Name()
        return "DerivedExample:Name():" + super:Name() + " " + ::Example906:Name()

    inline method OldName()
    return "DerivedExample:OldName(): " + super:Name() + " " + ::Example906:Name()

    var a
endclass


class Example906
exported:
    inline method Init()
        ::a := 3
        return self

    inline method Name()
        return "Example"

    var a
endclass

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
