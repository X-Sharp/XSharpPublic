USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

BEGIN NAMESPACE ConsoleApplication1

	FUNCTION Start() AS VOID
		SetEpoch(1900)
		? ConDate(18,3,15)
		SetEpoch(2000)
		? ConDate(18,3,15)
		Console.ReadLine()

END NAMESPACE
