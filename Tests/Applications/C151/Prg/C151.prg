// 151. error XS0030: Cannot convert type 'int' to 'bool'
// works in vulcan, maybe it should be supported only in the vulcan dialect?
FUNCTION Start() AS VOID
	LOCAL n := 0 AS INT
	LOCAL l AS LOGIC
	l := LOGIC(_CAST , n)
	IF l
		THROW Exception{"Result is TRUE"}
	END IF
	n := 1
	l := LOGIC(_CAST , n)
	IF .not. l
		THROW Exception{"Result is FALSE"}
	END IF

