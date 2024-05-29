
// https://github.com/X-Sharp/XSharpPublic/issues/1288
//#xcommand WP [/<x:S,F,SF,FS>] [/<y:CAPS,C>] [/<m:MUTE,M>] [/OBJ <obj> = <mes> [,<objn> = <mesn> ]] <list,...> [<file>] ;
//=> wpRouter({<list>},<(x)>,<file>,<(y)>,<(m)>,{[{<"obj">,{|o| <mes>}}] [,{<"objn">,{|o| <mesn>} }] })
//
//procedure Main()
//    local a, b, c
//
//    wp a,b,c
//    wp /sf /m a,b,c
//    return

procedure Main()
    local o

    o := DerivedExample896():new()
    ? o:a
    ? o:Example896:a // Runtime error in X# 2.14
    ? o:Name()
    ? o:Example896:Name() // Runtime error in X# 2.14

    return

class DerivedExample896 from Example896
exported:
    inline method Init()
        super:Init()
        ::a := 4
        super:a := 5
        return self

    inline method Name()
        return "DerivedExample"

    new var a
endclass


class Example896
exported:
    inline method Init()
        ::a := 3
        return self

    inline method Name()
        return "Example"

    var a
endclass

