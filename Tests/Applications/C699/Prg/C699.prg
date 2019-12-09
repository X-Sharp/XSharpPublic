// 699. {SELF:Member} in interpolated string gets replaced with clas name, instead of member value
FUNCTION Start() AS VOID
	TestClass{}
RETURN

CLASS TestClass
	EXPORT MyField := - 123 AS INT
	PROPERTY MyProperty AS STRING GET "property"
	CONSTRUCTOR()
		LOCAL c AS STRING
		c := "oldvalue"
		c := i"SELECT {SELF:MyField} {c} FROM {SELF:MyProperty}"
		? c // incorrect: "SELECT TestClass oldvalue FROM TestClass"
		xAssert(c == "SELECT -123 oldvalue FROM property")
	RETURN
END CLASS

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
