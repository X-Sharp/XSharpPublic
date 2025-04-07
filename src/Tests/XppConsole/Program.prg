procedure main()
    try
    local o := Example():new()  as object


    ? Example():classField
    ? Example():classMethod()
    ? Example():classAccMethod
    ?
    ? o:classField
    ? o:classMethod()
    ? o:classAccMethod
    catch e as Exception
        ? e:ToString()
    end try

    wait
return


class Example
exported:
    inline access class method classAccMethod()
    return ::classField + 2

    inline class method initClass()
        ::classField := 4
    return

    inline class method classMethod()
    return ::classField + 1

    class var classField
endclass
