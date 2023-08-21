// 426. System.InvalidCastException
FUNCTION Start() AS VOID
	LOCAL s IS __struct
	LOCAL u AS USUAL
	
	u := TRUE
	s:mlogic := LOGIC(u)
    ? s:mlogic
	xAssert(s:mlogic)

	s:mlogic := FALSE
    ? s:mlogic
	xAssert(! s:mlogic)

//#error InvalidCastException here
	s:mlogic := u // InvalidCastException here
    ? s:mlogic
	xAssert(s:mlogic)
	
	
	// OK
	u := #MYSYMBOL
	s:msymbol := u
    ? s:msymbol
	xAssert(s:msymbol == #MYSYMBOL)
RETURN

VOSTRUCT __struct
MEMBER mlogic AS LOGIC
MEMBER msymbol AS SYMBOL

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

