// 928. Problem defining properties when class inherits from system type
// https://github.com/X-Sharp/XSharpPublic/issues/1611
#pragma options("fox1", on)
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{42, date()}
	? o:testprop, o:testprop2, o:testprop3
    ? o:Exception:Message
// error XS0115: '_InitProperties': no suitable method found to override
DEFINE CLASS TestClass //AS System.Collections.ArrayList
	testprop := 123
	testprop2, testprop3
	add object Exception as Exception //Noinit With Message = "test"
	procedure init(a,b)   as usual
	    testprop2 := a
	    testprop3 := b
	    Exception := Exception{"Initialized"}
	    return 42

ENDDEFINE

