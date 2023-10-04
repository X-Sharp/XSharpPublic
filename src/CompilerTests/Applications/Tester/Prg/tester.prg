procedure Main()
    local o := SomeClass():new()

    o:Hello()
    _wait()
return


class SomeClass from Base
exported:
    inline method Init()
        ::a := "abc"
        return super:Init()

    inline method Hello()
        ? "Hello", ::a
    return

    var a
endclass


class Base
exported:
    inline method Init()
        ? "Base"
        return
endclass
