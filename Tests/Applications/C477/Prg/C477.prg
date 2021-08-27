//  477. Unhandled Exception: System.TypeLoadException: Method 'WrOnGcAse' in type 'TEst' does not have an implementation.
/*
vulcan reports:
 'TEst' does not correctly implement interface member 'ITest.WrOnGcAse()'; implementation is static, not public, not virtual or abstract, has the wrong return type or different casing

I think x# must also report an error that the casing is not correct and do not allow it to compile
*/
INTERFACE ITest
	METHOD WrOnGcAse() AS VOID
END INTERFACE

CLASS TEst IMPLEMENTS ITest
	VIRTUAL METHOD WrongCase() AS VOID
		? "method called"
	RETURN
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS ITest
	o := Test{}
	o:WrongCase()
RETURN

