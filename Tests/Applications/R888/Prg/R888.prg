// Test for https://github.com/X-Sharp/XSharpPublic/issues/1243
// Tests compiler generated Constructor (/vo16)
#pragma options("vo16", on)
#pragma options("lb", on)
procedure main()
    local o

    o := DerivedClass():new(3, 4)

    ? o:GetA()
    ? o:GetB()
    return

class DerivedClass from BaseClass
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
