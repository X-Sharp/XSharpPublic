// 528. STATIC METHOD gets priority over FUNCTION with the same name
// This was in Bernhard's code, in vulcan the code is running as (he) expects,
// in X# he gets a Statck Overflow
#pragma warnings(9066, off) // ambiguous

FUNCTION Start() AS VOID
	LOCAL o AS AcAddOn3Commands
	o := AcAddOn3Commands{}
	o:DoRun()
	AcAddOn3Commands.DoRunStatic()
RETURN

CLASS AcAddOn3Commands
	METHOD DoRun() AS VOID STRICT
		STATIC LOCAL lAlreadyRun := FALSE
		IF lAlreadyRun
			THROW Exception{"Instance method call to itself instead of function"}
		END IF
		lAlreadyRun := TRUE
		DoRun()
	RETURN
	STATIC METHOD DoRunStatic() AS VOID STRICT
		STATIC LOCAL lAlreadyRun := FALSE
		IF lAlreadyRun
			THROW Exception{"Static method call to itself instead of function"}
		END IF
		lAlreadyRun := TRUE
		DoRunStatic()
	RETURN
END CLASS

FUNCTION DoRun() AS VOID STRICT
	? "function 1 called"

FUNCTION DoRunStatic() AS VOID STRICT
	? "function 2 called"

