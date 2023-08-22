// Test for https://github.com/X-Sharp/XSharpPublic/issues/1243
// Tests compiler generated Constructor (/vo16)
#pragma options("vo16", on)
#pragma options("lb", on)
#pragma warnings(9201, disable)
// XPP The 'FROM' clause is interpreted as SHARING because variables in parent classes are always shared in the .Net Runtime.
procedure main()
    local o

    o := DerivedClass():new(3, 4)

    ? o:GetA()
    xAssert(o:GetA()  == 3)
    ? o:GetB()
    xAssert(o:GetB()  == 4)
    xAssert(o:BaseClass:GetA() == 3)    // BaseClass returns Super
    xAssert(o:BaseClass:GetB() == 4)
    return

class DerivedClass from BaseClass
exported:
    inline method Init(a, b)
    ::BaseClass:Init(a,b)
    xAssert(::BaseClass:GetA() == a)
    xAssert(::BaseClass:GetB() == b)
    return self
endclass

class BaseClass
exported:
    inline method Init(a, b)
        ::_a := a
        ::_b := b
    return self

    inline method GetA(); return ::_a
    inline method GetB(); return ::_b

hidden:
    var _a
    var _b
endclass

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
