// error XS0127: Since 'Test2.Test2(params __Usual[])' returns void, a return keyword must not be followed by an object expression
// vulcan does not allow this, but VO does
// /vo1+
#pragma warnings(9032, off) // return value
FUNCTION Start() AS VOID
? Test1{}
? Test2{}

CLASS Test1
CONSTRUCTOR()
? "constructor called"
RETURN SELF
DESTRUCTOR()
? "destructor called"
RETURN SELF
END CLASS

CLASS Test2
METHOD Init(a,b)
? "created"
RETURN SELF
METHOD Axit()
? "destroyed"
RETURN NIL // also vulcan allows this, with a warning as in the DESTRUCTOR case
END CLASS

