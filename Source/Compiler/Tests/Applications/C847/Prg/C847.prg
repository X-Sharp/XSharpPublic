// 847. Various inconsistencies with BEGIN SEQUENCE, BREAK etc
// https://github.com/X-Sharp/XSharpPublic/issues/883
#pragma warnings(165, off) // not assigned
#pragma warnings(162, off) // unreachanle
FUNCTION Start() AS VOID STRICT
	LOCAL xError AS USUAL

	LOCAL untyped AS USUAL
	LOCAL typed AS Tester

	typed := Tester{}
	untyped := typed

	BEGIN SEQUENCE
		untyped:Test()
	RECOVER USING xError
		? "Value returned back to late bound call:", xError
	END SEQUENCE
	xAssert(xError == 555)

	xError := 0

	BEGIN SEQUENCE
		typed:Test()
	RECOVER USING xError
		? "Value returned back to early bound call:", xError
	END SEQUENCE
	xAssert(xError == 555)

CLASS Tester
	METHOD Test()
		BREAK 555
	RETURN NIL
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

