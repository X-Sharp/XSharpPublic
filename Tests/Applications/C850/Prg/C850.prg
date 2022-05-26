// 850. Internal compiler error with Evaluate() in CODEBLOCK with /memvar+ in VFP dialect

// /memvar+ must be enabled
FUNCTION Start( ) AS VOID
	LOCAL cb AS USUAL
	cb := {|| Evaluate("1+2")}
	? Eval(cb)
	xAssert(Eval(cb) == 3)
RETURN

CLASS TestClass
	CONSTRUCTOR()
		LOCAL cb AS USUAL
		cb := {|| Evaluate("1+2")}
		? Eval(cb)
		xAssert(Eval(cb) == 3)
END CLASS

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN
