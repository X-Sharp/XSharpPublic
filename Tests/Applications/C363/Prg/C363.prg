// 363. compiler crash with Init() outside CLASS..END CLASS
// /vo1+
FUNCTION Start() AS VOID
? Test{}

PARTIAL CLASS Test
END CLASS

METHOD Init() CLASS Test
? "constructor called"
//RETURN SELF // should we also allow this, as VO does?

METHOD Axit() CLASS Test
? "destroyed"

