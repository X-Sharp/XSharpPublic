// R770: Disallow dot as instance operator in Core dialect when compiler option /allowdot- is used.
FUNCTION Start( ) AS VOID
	LOCAL oError AS Error
	oError := Error{"this is a test"}
	? oError.Description
	Console.ReadLine()
RETURN
