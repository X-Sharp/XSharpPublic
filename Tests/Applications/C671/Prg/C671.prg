// 671. VAR and LOCAL IMPLIED
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	IF o IS TestClass VAR oo
		? oo:Test()
		o := NULL
	END IF
	xAssertTrue(o == NULL)

	IF o IS TestClass VAR oo
		THROW Exception{"Should not be here"}
	END IF
	
	VAR n := 1
	xAssertTrue(n:GetType() == TypeOf(INT))
	LOCAL IMPLIED nn := 1
	xAssertTrue(nn:GetType() == TypeOf(INT))
	
	FOREACH VAR c IN <STRING>{"a","b","C"}
		xAssertTrue(c:GetType() == TypeOf(STRING))
	NEXT
	FOREACH IMPLIED c IN <STRING>{"a","b","C"}
		xAssertTrue(c:GetType() == TypeOf(STRING))
	NEXT	
	
RETURN

CLASS TestClass
	METHOD Test() AS INT
	RETURN 123
END CLASS

PROC xAssertFalse(l AS LOGIC)
	xAssertTrue(.not. l)
PROC xAssertTrue(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

