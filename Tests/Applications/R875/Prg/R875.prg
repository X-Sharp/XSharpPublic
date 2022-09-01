// https://github.com/X-Sharp/XSharpPublic/issues/1101
// Bogus warning about ambiguous methods, with NEW ToString()
FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
? o:ToString()
xAssert(o:ToString() == "123")
CLASS TestClass
NEW METHOD ToString() AS STRING
RETURN "123"
END CLASS


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
