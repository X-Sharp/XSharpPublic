function Start as void
    test (1,,2,,3)
    return


function test Clipper
    foreach var arg in _Args()
        ? arg
    next
    return nil
