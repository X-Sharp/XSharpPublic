// 322. error XS0127: Since 'SQLConnection_SDK.~SQLConnection_SDK()' returns void, a return keyword must not be followed by an object expression
// This is code from the SDK!
// vulcan only reports a warning that the return value of the destructor is ignored
#pragma warnings(9032, off) // cannot return a value
FUNCTION Start() AS VOID
DoTest()
FUNCTION DoTest() AS VOID
LOCAL o AS SQLConnection_SDK
o := SQLConnection_SDK{}
? o
o := NULL
GC.Collect()
? "After collection"

CLASS SQLConnection_SDK
DESTRUCTOR()
? "destructor called"
RETURN SELF:Disconnect()
METHOD Disconnect() CLIPPER
	? "Disconnected"
RETURN NIL
END CLASS

