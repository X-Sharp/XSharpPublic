// 142. Problem with virtual methods and /vo3 option

// /vo3
// without /vo3, ChildClass method gets executed as it should be
FUNCTION Start() AS VOID
LOCAL o AS BaseClass
o := ChildClass{}
o:Test()
RETURN

CLASS BaseClass
VIRTUAL METHOD Test() AS VOID
	THROW Exception{"Base method called instead of Child"}
END CLASS

CLASS ChildClass INHERIT BaseClass
VIRTUAL METHOD Test() AS VOID
	? "Correctly called child method"
END CLASS

