// 948. "if x is not var y" not working
// https://github.com/X-Sharp/XSharpPublic/issues/1755

FUNCTION Start( ) AS VOID
	LOCAL uSwitch AS USUAL

	uSwitch := "abc"

	IF uSwitch IS not LOGIC
		xAssert(true)
	else
		xAssert(false)
	endif

	IF uSwitch IS not LOGIC VAR bSwitch1
		xAssert(true)
	ELSE
		xAssert(false)
	ENDIF

	uSwitch := true

	IF uSwitch IS not LOGIC VAR bSwitch2
		xAssert(false)
	ELSE
		xAssert(true)
		xAssert(bSwitch2)
	ENDIF
	
PROC xAssert(l AS LOGIC) AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

