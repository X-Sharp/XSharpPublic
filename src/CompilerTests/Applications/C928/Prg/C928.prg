// 928. Problem defining properties when class inherits from system type
// https://github.com/X-Sharp/XSharpPublic/issues/1611
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o:testprop


// error XS0115: '_InitProperties': no suitable method found to override
DEFINE CLASS TestClass AS System.Collections.ArrayList
	testprop := 123 
ENDDEFINE

