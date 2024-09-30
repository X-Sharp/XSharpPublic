
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

PROCEDURE Main()
    LOCAL o

    o := DerivedExample896():new()
    ? o:a
    xAssert( o:a == 4 )
    ? o:Example896:a // Runtime error in X# 2.14
    xAssert( o:Example896:a == 5 )
    ? o:Name()
    xAssert( o:Name() == "DerivedExample" )
    ? o:Example896:Name() // Runtime error in X# 2.14
    xAssert( o:Example896:Name() == "Example" )

    RETURN

CLASS DerivedExample896 FROM Example896
exported:
    inline METHOD Init()
        SUPER:Init()
        ::a := 4
        SUPER:a := 5
        RETURN SELF

    inline METHOD Name()
        RETURN "DerivedExample"

    NEW VAR a
endclass


CLASS Example896
exported:
    inline METHOD Init()
        ::a := 3
        RETURN SELF

    inline METHOD Name()
        RETURN "Example"

    VAR a
endclass

PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "assertion passed"
	ELSE
		? "Assertion failed"
		THROW Exception{"Incorrect result"}
	END IF   
RETURN
