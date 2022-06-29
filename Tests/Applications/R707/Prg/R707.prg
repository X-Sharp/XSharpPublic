#pragma warnings(9098, off) // late binding
FUNCTION Start as VOID
    local o as USUAL
    o := Foo{}
    o:Bar := "Test"
    ? o:Bar
    ? o:Hello()
    WAIT
    RETURN

CLASS Foo
    PROPERTY Bar as STRING AUTO
    METHOD Hello as STRING
        RETURN "Hello "+Bar
END CLASS
