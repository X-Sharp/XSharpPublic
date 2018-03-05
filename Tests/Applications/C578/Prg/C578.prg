// 578. error XS0221: Constant value '-1' cannot be converted to a 'word' (use 'unchecked' syntax to override)
// /ovf /fovf

// correct error XS0221: Constant value '-1' cannot be converted to a 'word' 
// (use 'unchecked' syntax to override)
//DEFINE d1 := (WORD) -1

// Still same error, despite using UNCKECKED, as suggested in the error message.
// Note that the compiler reports an error on this only in vulcan dialect.
// In Core, this does not report an error, which is as expected
DEFINE d2 := unchecked ((WORD) -1)

// This one does not cause an error to be reported
// But it is the same syntax as with the "d1" version, so it SHOULD report the same error, isn't that right?
DEFINE d3 := WORD(-1)

// This does not report an error and it is correct behavior.
// As we have decided, the _CAST syntax does not do any overflow checking
DEFINE d4 := WORD(_CAST,-1)

FUNCTION Start() AS VOID
? d2,d4
xAssert(d2 == 65535)
xAssert(d4 == 65535)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

