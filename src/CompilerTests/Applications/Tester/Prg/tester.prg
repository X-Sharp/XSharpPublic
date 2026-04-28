procedure main()
    Child():setter(1)
    ? Child():getter()

    Child():exportedField := 2
    ? Child():exportedField
    wait
return


class Child from Parent
endclass


class Parent
exported:
    inline class method initClass()
        ::classField := 0
        ::exportedField := 0
    return

    inline class method getter()
    return ::classField

    inline class method setter(n)
        ::classField := n
    return

    class var exportedField

hidden:
    class var classField
endclass
