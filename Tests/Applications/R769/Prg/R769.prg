
FUNCTION Start( ) AS VOID
	LOCAL oError AS Error
	oError := Error{"this is a test"}
	? oError.Description
	Console.ReadLine()
RETURN
