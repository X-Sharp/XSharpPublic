// 939. Conflict between method name and property setter
// https://github.com/X-Sharp/XSharpPublic/issues/1702
CLASS Parent
	ASSIGN Test(u)
		? "assign called"
END CLASS

CLASS Middle INHERIT Parent
END CLASS

CLASS Child INHERIT Middle
	METHOD Set_test() CLIPPER
		? "method called"
	RETURN "method called"
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS Child
	o := Child{}
	xAssert( o:Set_test() == "method called" ) // System.Reflection.AmbiguousMatchException

	LOCAL u AS USUAL
	u := Child{}
	xAssert( u:Set_test() == "method called" ) // System.Reflection.AmbiguousMatchException


PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

