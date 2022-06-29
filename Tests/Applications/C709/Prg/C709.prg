// 709. Calling NoMethod()/NoIva() through typed vars
#pragma warnings(9094, off) //   nomethod
#pragma warnings(9066, off) //   Call to function instead of method
CLASS TestClass1
	METHOD NoMethod(uParam)
		? NoMethod(), uParam
	RETURN "Called" + NoMethod() + AsString(uParam)
	METHOD NoIVarGet(cName)
		? cName
	RETURN upper(cName) + "VALUE"
END CLASS

CLASS TestClass2
	METHOD NoIvarPut(cName, uValue)
		? cName + AsString(uValue)
	RETURN upper(cName) + AsString(uValue)
END CLASS
CLASS TestClass3 INHERIT TestClass1
END CLASS

FUNCTION Start() AS VOID
	LOCAL u AS USUAL
	LOCAL o AS OBJECT
	LOCAL o1 AS testclass3
	u := TestClass3{}
	o := u
	o1 := u
	? u:Nothing(123)
	xAssert(u:Nothing(123) == "CalledNOTHING123")
	xAssert(o:Nothing(123) == "CalledNOTHING123")
	xAssert(o1:Nothing(123) == "CalledNOTHING123") // error XS1061: 'TestClass1' does not contain a definition for 'Nothing'

	? u:SomeField
	xAssert(u:SomeField == "SOMEFIELDVALUE")
	xAssert(o:SomeField == "SOMEFIELDVALUE")
	xAssert(o1:SomeField == "SOMEFIELDVALUE") // error XS1061: 'TestClass1' does not contain a definition for 'SomeField'


	u := TestClass2{}
	u:Noassign := -123
	LOCAL o2 AS TestClass2
	o2 := u
	o := u
	o:Noassign := -123
	o2:Noassign := -123 // error XS1061: 'TestClass2' does not contain a definition for 'Noassign'
RETURN


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
