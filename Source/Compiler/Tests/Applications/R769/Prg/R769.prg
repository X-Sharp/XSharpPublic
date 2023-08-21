#pragma options("Allowdot", on)
FUNCTION Start( ) AS VOID
	LOCAL oError AS Error
	oError := Error{"this is a test"}
	? oError.Description
	? oError:Description
	Console.ReadLine()
RETURN
