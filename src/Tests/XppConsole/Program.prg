// Calling method from parent class through the generated access that returns a parent object
// https://github.com/X-Sharp/XSharpPublic/issues/1338
#pragma warnings(9201, off)   // SHARED clause
#pragma warnings(108, off)    // DerivedExample.a' hides inherited member 'Example.a'. Use the new keyword if hiding was intended.
procedure Main()
    local o
    try
    o := DerivedExample():new()
    ? o:a
    ? o:Example:a
    ? o:Name()
    ? "Example:Name",o:Example:Name()
    ? o:OldName()
    catch e as exception
        ? e:ToString()
    end try
    _wait()
    return

class DerivedExample from Example
exported:
    inline method Init()
        super:Init()
        ::a := 4
        ::Example:a := 5
        ? "DerivedExample:Init(): ", ::a, super:a, ::Example:a
        return self

    inline method Name()
        return "DerivedExample:Name():" + super:Name() + " " + ::Example:Name()+ " "

    inline method OldName()
    return "DerivedExample:OldName(): " + super:Name() + " " + ::Example:Name()+ " "

    var a
endclass


class Example
exported:
    inline method Init()
        ::a := 3
        return self

    inline method Name()
        return "Example Name"

    var a
endclass
