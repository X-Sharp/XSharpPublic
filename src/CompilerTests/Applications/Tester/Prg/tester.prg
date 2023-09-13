#xcommand WP [/<x:S,F,SF,FS>] [/<y:CAPS,C>] [/<m:MUTE,M>] [/OBJ <obj> = <mes> [,<objn> = <mesn> ]] <list,...> [<file>] ;
=> wpRouter({<list>},<(x)>,<file>,<(y)>,<(m)>,{[{<"obj">,{|o| <mes>}}] [,{<"objn">,{|o| <mesn>} }] })

procedure Main()
    local a, b, c
    a := "a"
    b := "b"
    c := "c"

    wp a,b,c
    wp /sf /m a,b,c
    _wait()
    return


function WpRouter() as usual clipper
    var args := _Args()
    foreach var arg in args
        if IsArray(arg)
            showArray(arg)
        else
            ? arg
        endif
    next
    return nil
