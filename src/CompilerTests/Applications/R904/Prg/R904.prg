#xcommand WP [/<x:S,F,SF,FS>] [/<y:CAPS,C>] [/<m:MUTE,M>] [/OBJ <obj> = <mes> [,<objn> = <mesn> ]] <list,...> [<file>] ;
=> wpRouter({<list>},<(x)>,<file>,<(y)>,<(m)>,{[{<"obj">,{|o| <mes>}}] [,{<"objn">,{|o| <mesn>} }] })
global result as string
procedure Main()
    local a, b, c
    a := "a"
    b := "b"
    c := "c"

    wp a,b,c
    XAssert(result=="{a,b,c},NIL,NIL,NIL,NIL,{}")
    wp /sf /m a,b,c
    XAssert(result=="{a,b,c},sf,NIL,NIL,m,{}")
    return


function WpRouter() as usual clipper
    var args := _Args()
    var sb := System.Text.StringBuilder{}
     local firstarg := true as logic
    foreach var arg in args
        if (!firstarg)
            sb:Append(",")
        endif
        if IsArray(arg)
            local firstelement := true as logic
            sb:Append("{")
            foreach var element in (Array) arg
                if (!firstelement)
                sb:Append(",")
                endif
                sb:Append(AsString(element))
                firstelement := false

            next
            sb:Append("}")
        else
            sb:Append(AsString(arg))
        endif
        firstarg := false
    next
    result := sb:ToString()
    ? result
    return nil

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

