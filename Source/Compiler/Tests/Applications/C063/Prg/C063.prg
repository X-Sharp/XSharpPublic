// 63. error XS0246: The type or namespace name 'TestClass' could not be found
// I never liked this behavior, but in vulcan this works
BEGIN NAMESPACE ns.One
CLASS Foo
END CLASS

BEGIN NAMESPACE Two
CLASS Bar
END CLASS

CLASS TestClass
END CLASS
FUNCTION Test() AS VOID
LOCAL o AS testClass
o := testClass{}
? o


END NAMESPACE   
END NAMESPACE

FUNCTION Test2 AS VOID
	LOCAL f AS Foo
f := Foo{}
? f
LOCAL b AS Bar
b := Bar{}
? b
RETURN
