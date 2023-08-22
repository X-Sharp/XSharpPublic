// 800. Null-conditional operator ?. on usual property throws System.AccessViolationException
FUNCTION Start() AS VOID STRICT
	LOCAL o1,o2 AS TestClass
	o1 := TestClass{}
	? o1:SomeString // test, OK

	// with the ":" operator, it works fine:
	o2 := o1:UsualAccess
	? o2:SomeString // test, OK
	xAssert(o2:SomeString == "test")

	// with the "?:" operator, there's a runtime exception:
	o2 := o1?:UsualAccess
	? o2:SomeString // runtime exception
	xAssert(o2:SomeString == "test")
RETURN

PUBLIC CLASS TestClass
	EXPORT SomeString := "test" AS STRING
	ACCESS UsualAccess AS USUAL
	RETURN TestClass{}
END CLASS


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN       
