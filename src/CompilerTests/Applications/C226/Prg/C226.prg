// 226. Assertion failed and compiler crash with NULL in late bound code
FUNCTION Start( ) AS VOID
LOCAL o AS USUAL
o := TestClass{}
o:SomeMethod(NULL)
o:SomeAssign := NULL

CLASS TestClass
METHOD SomeMethod(u)
RETURN NIL
ASSIGN SomeAssign(u)
END CLASS

