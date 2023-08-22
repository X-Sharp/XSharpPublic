// 698. Various problems with passing params by reference in FoxPro dialect
FUNCTION Start() AS VOID
	LOCAL l AS LOGIC
	LOCAL x
	LOCAL i as LONG

	i := 42
	l := TestReference( i,123 )
	? i
	xAssert(i == 42)
	xAssert(l == FALSE)

	i := 42
	l := TestReference( @i,123 )


	? i
	xAssert(i == 123)
	xAssert(l == TRUE)
	x := "old"
	TestParameters( @x )
	? x
	xAssert(x == "PARAMETERS")
	x := 1
	TestParameters( REF x )
	? x
	xAssert(x == "PARAMETERS")

	x := "old"
	Test_L_Parameters( @x )
	? x
	xAssert(x == "LPARAMETERS")
	x := "1"
	Test_L_Parameters( REF x )
	? x
	xAssert(x == "LPARAMETERS")

	x:= "old"
	DO TestReference WITH x
	? x
	xAssert(x == 123)

	x := 1
	DO TestReference WITH REF x // works as well!
	? x
	xAssert(x == 123)

	x := 1
	DO TestReference WITH REF @x // work as well !
	? x
	xAssert(x == 123)

	x:= "old"
	DO TestParameters WITH x // does not pass by reference
	? x
	xAssert(x == "PARAMETERS")

	x:= "old"
	DO Test_L_Parameters WITH REF x // should that work as well?
	? x
	xAssert(x == "LPARAMETERS")
	*/
RETURN

FUNCTION TestReference ( a )
	LOCAL l AS LOGIC
	l := IsByref(a)
	a := 123
RETURN l

END FUNCTION

FUNCTION TestParameters()
	PARAMETERS a
	a := "PARAMETERS"
RETURN 0

PROCEDURE Test_L_Parameters()
	LPARAMETERS a AS STRING
	a := "LPARAMETERS"
RETURN 0


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN 0
