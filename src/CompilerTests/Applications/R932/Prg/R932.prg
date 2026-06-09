// https://github.com/X-Sharp/XSharpPublic/issues/1806

RECORD CLASS Person
    PROPERTY Name as STRING AUTO
    PROPERTY Age as INT AUTO
END CLASS

CLASS Person2
    PROPERTY Name as STRING AUTO
    PROPERTY Age as INT AUTO
END CLASS



FUNCTION Start as void
    local oPerson1 as Person
    local oPerson2 as Person
    oPerson1 := Person{}{Name := "Robert", Age := 67}
    oPerson2 := Person{}{Name := "Chris", Age := 42}
    ? oPerson1 != oPerson2
    xAssert( oPerson1 != oPerson2)
    oPerson1 := Person{}{Name := "Chris", Age := 42}
    ? oPerson1 == oPerson2
    xAssert( oPerson1 == oPerson2)
    xAssert(! Object.ReferenceEquals(oPerson1, oPerson2))

    local oPerson3 as Person2
    local oPerson4 as Person2
    oPerson3 := Person2{}{Name := "Robert", Age := 67}
    oPerson4 := Person2{}{Name := "Chris", Age := 42}
    ? oPerson3 != oPerson4
    xAssert( oPerson3 != oPerson4)
    oPerson3 := Person2{}{Name := "Chris", Age := 42}
    ? oPerson3 == oPerson4
    xAssert( oPerson3 != oPerson4)
    xAssert(! Object.ReferenceEquals(oPerson3, oPerson4))




PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
